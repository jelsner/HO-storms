library(data.table)
library(dplyr)
library(fixest)
library(lubridate)
library(sf)
library(tidyr)
library(tigris)

storm_intensity <- 64
begin_year <- 1985
end_year <- 2022
begin_date <- paste0(begin_year, "-01-01")
end_date <- paste0(end_year, "-12-31")
min_lag <- -8L
max_lead <- 8L
rel_levels <- c("None", as.character(min_lag:max_lead))

dir.create("models", showWarnings = FALSE, recursive = TRUE)

build_rel_calendar <- function() {
  tracks <- st_read(dsn = "data", layer = "IBTrACS.NA.list.v04r00.lines", quiet = TRUE) |>
    st_transform(crs = 32616) |>
    filter(year >= begin_year & year <= end_year,
           USA_WIND >= storm_intensity) |>
    select(SID, SEASON, year, month, day, hour, min,
           NAME, SUBBASIN, ISO_TIME, USA_WIND, USA_PRES, USA_RMW,
           USA_EYE, USA_ROCI)

  tracks <- tracks |>
    group_by(SID) |>
    mutate(USA_RMW = ifelse(is.na(USA_RMW), mean(USA_RMW, na.rm = TRUE), USA_RMW)) |>
    ungroup() |>
    group_by(USA_PRES) |>
    mutate(USA_RMW = ifelse(is.na(USA_RMW), mean(USA_RMW, na.rm = TRUE), USA_RMW)) |>
    ungroup() |>
    group_by(SID) |>
    mutate(USA_RMW = ifelse(is.na(USA_RMW), mean(USA_RMW, na.rm = TRUE), USA_RMW)) |>
    ungroup()

  swaths <- tracks |>
    st_buffer(dist = tracks$USA_RMW * 1852)

  boundaries <- USAboundaries::us_states(resolution = "low", states = "FL") |>
    st_transform(crs = 32616)

  swaths <- swaths[st_intersects(swaths, boundaries, sparse = FALSE), ] |>
    mutate(Date = as_date(ISO_TIME)) |>
    group_by(SID) |>
    summarize(
      Date0 = first(Date),
      NAME = first(NAME),
      Wind = max(USA_WIND),
      geometry = st_union(geometry),
      .groups = "drop"
    ) |>
    mutate(Storm_Category = case_when(
      Wind >= 34 & Wind <= 63 ~ 0,
      Wind >= 64 & Wind <= 82 ~ 1,
      Wind >= 83 & Wind <= 95 ~ 2,
      Wind >= 96 & Wind <= 112 ~ 3,
      Wind >= 113 & Wind <= 136 ~ 4,
      Wind >= 137 ~ 5
    )) |>
    st_transform(crs = 4326)

  tic <- swaths |>
    rowwise() |>
    mutate(event_days = list(tibble(
      Date = Date0 + (min_lag:max_lead),
      rel_day = min_lag:max_lead
    ))) |>
    unnest(event_days) |>
    ungroup() |>
    select(Date, rel_day, Storm_Name = NAME, Storm_Category)

  st_geometry(tic) <- "geometry"

  options(tigris_use_cache = TRUE)
  zcta_us <- zctas(cb = TRUE, year = 2020) |>
    select(zip = ZCTA5CE20, geometry)
  fl <- states(cb = TRUE, year = 2020) |>
    filter(STUSPS == "FL") |>
    st_transform(st_crs(zcta_us))
  zctas_fl <- st_join(zcta_us, fl, join = st_intersects, left = FALSE) |>
    select(zip, geometry)

  tic_keep <- st_transform(tic, st_crs(zctas_fl)) |>
    select(Date, rel_day, geometry)

  zcta_date_rel <- st_join(
    zctas_fl,
    tic_keep,
    join = st_intersects,
    left = FALSE
  ) |>
    st_drop_geometry() |>
    as.data.table()

  setnames(zcta_date_rel, c("zip", "Date", "rel_day"), c("zip", "date", "rel_day"))
  zcta_date_rel[, `:=`(
    zip = as.character(zip),
    date = as.IDate(date),
    rel_day = as.integer(rel_day)
  )]

  zcta_date_rel <- zcta_date_rel[
    , .SD[order(abs(rel_day), rel_day)][1],
    by = .(zip, date)
  ]

  grid <- CJ(
    zip = as.character(sort(unique(zctas_fl$zip))),
    date = as.IDate(seq(as.Date(begin_date), as.Date(end_date), by = "day"))
  )
  rel_calendar <- zcta_date_rel[grid, on = .(zip, date)]
  rel_calendar[, rel_day_f := fifelse(is.na(rel_day), "None", as.character(rel_day))]
  rel_calendar[, rel_day_f := factor(rel_day_f, levels = rel_levels)]

  list(rel_calendar = rel_calendar, zctas_fl = zctas_fl)
}

make_daily_panel <- function(subset_path, object_name, rel_calendar, zctas_fl) {
  env <- new.env(parent = emptyenv())
  load(subset_path, envir = env)
  deaths <- env[[object_name]] |>
    mutate(date = as.Date(DATE_OF_DEATH)) |>
    filter(date >= as.Date(begin_date), date <= as.Date(end_date)) |>
    select(Death_ID = ID, date, final_lon, final_lat)

  deaths_sf <- deaths |>
    st_as_sf(coords = c("final_lon", "final_lat"), crs = 4326, remove = FALSE)

  target_crs <- 5070
  deaths_zip <- st_join(
    st_transform(deaths_sf, target_crs),
    st_transform(zctas_fl, target_crs),
    join = st_intersects,
    left = TRUE,
    largest = TRUE
  ) |>
    st_drop_geometry() |>
    filter(!is.na(zip)) |>
    transmute(
      zip = as.character(zip),
      date = as.IDate(date)
    ) |>
    as.data.table()

  daily_deaths <- deaths_zip[, .(deaths = .N), by = .(zip, date)]

  panel <- daily_deaths[as.data.table(rel_calendar), on = .(zip, date)]
  panel[is.na(deaths), deaths := 0L]
  panel[, rel_day_f := factor(as.character(rel_day_f), levels = rel_levels)]
  panel
}

fit_event_model <- function(panel) {
  dat <- panel |>
    as.data.frame() |>
    filter(month(as.Date(date)) %in% 5:11) |>
    transmute(
      zip = as.character(zip),
      date = as.IDate(date),
      deaths = as.integer(deaths),
      rel_day_f = factor(as.character(rel_day_f), levels = rel_levels),
      dow = factor(
        strftime(as.Date(date), "%u"),
        levels = as.character(1:7),
        labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
      ),
      yw = interaction(year(as.Date(date)), isoweek(as.Date(date)), drop = TRUE),
      covid = as.integer(date >= as.IDate("2020-01-01") & date <= as.IDate("2022-12-31"))
    ) |>
    mutate(
      zip_covid = interaction(zip, covid, drop = TRUE),
      zip = factor(zip)
    )

  fepois(
    deaths ~ i(rel_day_f, ref = "None") | zip + yw + dow + zip_covid,
    data = dat,
    cluster = ~ zip + yw,
    notes = TRUE
  )
}

calendar <- build_rel_calendar()

cause_specs <- list(
  injury = list(
    subset_path = "data/injury_subset_all.Rdata",
    object_name = "injury_subset_all",
    model_path = "models/m_event_64kt_injury.rds"
  )
)

if (identical(Sys.getenv("FIT_CARDIO"), "1")) {
  cause_specs$cardio <- list(
    subset_path = "data/cardio_subset_all.Rdata",
    object_name = "cardio_subset_all",
    model_path = "models/m_event_64kt_cardio.rds"
  )
}

for (label in names(cause_specs)) {
  spec <- cause_specs[[label]]
  if (file.exists(spec$model_path)) {
    message("Skipping existing model: ", spec$model_path)
    next
  }
  message("Building panel for ", label)
  panel <- make_daily_panel(
    spec$subset_path,
    spec$object_name,
    calendar$rel_calendar,
    calendar$zctas_fl
  )
  message("Fitting model for ", label)
  model <- fit_event_model(panel)
  saveRDS(model, spec$model_path)
  rm(panel, model)
  gc()
}
