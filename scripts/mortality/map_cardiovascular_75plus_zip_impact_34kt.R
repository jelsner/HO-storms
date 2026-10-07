#!/usr/bin/env Rscript

# Estimate ZIP-specific cardiovascular mortality rate ratios for adults age 75+
# during days 0 to +6 after >=34 kt tropical-cyclone impact. A pooled daily
# fixed-effects Poisson model supplies each ZIP's counterfactual expected count.
# ZIP-level observed counts are modeled as Poisson(expected * rate ratio), then
# stabilized with a gamma-Poisson empirical-Bayes model.

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
  library(lubridate)
  library(scales)
  library(sf)
  library(tigris)
})

source("scripts/mortality/cause_age_count_helpers.R")

wind_threshold <- 34L
target_period <- "Days 0 to +6"
period_levels <- c(
  "None",
  "Days -14 to -8",
  "Days -7 to -1",
  target_period,
  "Days +7 to +13"
)

daily_panel_path <- sprintf(
  "data/mortality/processed/death-panels/daily_panel_%dkt.csv",
  wind_threshold
)
deaths_path <- sprintf(
  "data/mortality/processed/spatial-results/deaths_with_zip_%dkt.rds",
  wind_threshold
)
cause_lookup_path <-
  "data/mortality/derived/mortality_underlying_cause_lookup_1985_2022.rds"

output_dir <- "outputs/mortality/spatial_cardiovascular_75plus_34kt"
figure_dir <- "figs/mortality"
raw_output_path <- file.path(
  output_dir,
  "cardiovascular_75plus_impact_zip_raw_34kt.csv"
)
estimate_output_path <- file.path(
  output_dir,
  "cardiovascular_75plus_impact_zip_eb_34kt.csv"
)
metadata_output_path <- file.path(
  output_dir,
  "cardiovascular_75plus_impact_zip_eb_metadata_34kt.csv"
)
spatial_output_path <- file.path(
  output_dir,
  "cardiovascular_75plus_impact_zip_eb_34kt.gpkg"
)
figure_output_path <- file.path(
  figure_dir,
  "cardiovascular_75plus_impact_zip_eb_34kt.png"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

required_paths <- c(daily_panel_path, deaths_path, cause_lookup_path)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths)) {
  stop("Missing required input(s): ", paste(missing_paths, collapse = ", "))
}

message("Analysis progress")
progress <- txtProgressBar(min = 0L, max = 5L, style = 3L)

period_from_rel_day <- function(rel_day) {
  fcase(
    rel_day >= -14L & rel_day <= -8L, "Days -14 to -8",
    rel_day >= -7L & rel_day <= -1L, "Days -7 to -1",
    rel_day >= 0L & rel_day <= 6L, target_period,
    rel_day >= 7L & rel_day <= 13L, "Days +7 to +13",
    default = NA_character_
  )
}

message("Building the ZIP-date event calendar")
calendar_source <- fread(
  daily_panel_path,
  select = c("zip", "date", "rel_day")
)
calendar_source[, `:=`(
  zip = as.character(zip),
  date = as.IDate(date),
  rel_day = suppressWarnings(as.integer(rel_day))
)]

zip_events <- unique(calendar_source[!is.na(rel_day), .(
  zip,
  impact_date = date - rel_day
)])

event_calendar <- zip_events[, .(rel_day = -14L:13L), by = .(zip, impact_date)]
event_calendar[, `:=`(
  date = impact_date + rel_day,
  event_period = period_from_rel_day(rel_day)
)]

# Resolve overlapping event windows exactly as in the pooled models: choose the
# closest impact date, breaking equal-distance ties in favor of the pre-period.
event_calendar <- event_calendar[
  order(zip, date, abs(rel_day), rel_day, impact_date)
][, .SD[1L], by = .(zip, date)]

complete_window_dates <- unique(event_calendar$date)
calendar <- calendar_source[, .(zip, date)]
calendar[, month := month(as.Date(date))]
calendar <- calendar[month %in% 5:11 | date %in% complete_window_dates]
calendar[, month := NULL]

calendar <- event_calendar[
  calendar,
  on = .(zip, date),
  .(zip = i.zip, date = i.date, event_period = x.event_period)
]
calendar[is.na(event_period), event_period := "None"]
calendar[, event_period := factor(event_period, levels = period_levels)]
all_panel_zips <- sort(unique(as.character(calendar$zip)))
calendar[, `:=`(
  zip = factor(zip),
  yw = year(as.Date(date)) * 100L + isoweek(as.Date(date)),
  dow = as.integer(strftime(as.Date(date), "%u")),
  covid = as.integer(
    date >= as.IDate("2020-01-01") & date <= as.IDate("2022-12-31")
  )
)]
calendar[, zip_covid := interaction(zip, covid, drop = TRUE)]

stopifnot(!anyDuplicated(calendar[, .(zip, date)]))

rm(calendar_source, event_calendar)
gc()
setTxtProgressBar(progress, 1L)

message("Building cardiovascular death counts for adults age 75+")
deaths_sf <- readRDS(deaths_path)
deaths <- as.data.table(deaths_sf)[, .(
  Death_ID,
  zip = as.character(zip),
  date = as.IDate(Death_Date),
  age = suppressWarnings(as.integer(age))
)]
rm(deaths_sf)
gc()

deaths <- deaths[
  age >= 75L & age <= 120L &
    (month(as.Date(date)) %in% 5:11 | date %in% complete_window_dates) &
    !is.na(zip)
]
deaths[, join_year := as.character(year(as.Date(date)))]

cause_lookup <- as.data.table(readRDS(cause_lookup_path))
cause_lookup <- cause_lookup[, .(
  Death_ID,
  join_year = as.character(join_year),
  code = as.character(code)
)]
if (anyDuplicated(cause_lookup[, .(Death_ID, join_year)])) {
  stop("Underlying-cause lookup is not unique by death ID and year.")
}

deaths <- merge(
  deaths,
  cause_lookup,
  by = c("Death_ID", "join_year"),
  all.x = TRUE,
  sort = FALSE
)
missing_cause_codes <- sum(is.na(deaths$code) | deaths$code == "")
if (missing_cause_codes) {
  warning(
    missing_cause_codes,
    " candidate deaths lack an underlying-cause code and will be excluded."
  )
}

is_cardiovascular <- build_cause_masks(
  year = as.integer(deaths$join_year),
  code = deaths$code
)[["Cardiovascular"]]
deaths <- deaths[!is.na(is_cardiovascular) & is_cardiovascular]
daily_counts <- deaths[, .(deaths = .N), by = .(zip, date)]

rm(deaths, cause_lookup, is_cardiovascular)
gc()
setTxtProgressBar(progress, 2L)

daily_counts[, zip := factor(zip, levels = levels(calendar$zip))]
model_data <- daily_counts[calendar, on = .(zip, date)]
model_data[is.na(deaths), deaths := 0L]

rm(calendar, daily_counts)
gc()

target_term <- paste0("event_period::", target_period)

# This is the same pooled specification used for the reported cause/age model.
# Its fitted fixed effects provide the expected death count for each target day
# after the event indicator is set to the reference (non-event) level.
message("Fitting the pooled counterfactual Poisson model")
fixest::setFixest_notes(FALSE)
pooled_model <- fepois(
  deaths ~ i(event_period, ref = "None") |
    zip + yw + dow + zip_covid,
  data = model_data,
  notes = FALSE
)
if (!target_term %in% names(coef(pooled_model))) {
  stop("The pooled impact-period coefficient was not estimable.")
}
pooled_beta <- unname(coef(pooled_model)[[target_term]])

target_data <- model_data[as.character(event_period) == target_period]
target_counterfactual <- copy(target_data)
target_counterfactual[, event_period := factor("None", levels = period_levels)]
target_data[, expected_deaths := predict(
  pooled_model,
  newdata = target_counterfactual,
  type = "response"
)]
setTxtProgressBar(progress, 3L)

# At each ZIP, Y_z ~ Poisson(E_z * theta_z), where E_z is the sum of daily
# counterfactual expected counts and theta_z is the ZIP's impact-period RR.
zip_counts <- target_data[
  is.finite(expected_deaths) & expected_deaths > 0,
  .(
    observed_deaths = sum(deaths),
    expected_deaths = sum(expected_deaths),
    target_days = .N
  ),
  by = .(zip = as.character(zip))
]
zip_counts[, `:=`(
  rate_ratio_raw = observed_deaths / expected_deaths,
  status = "estimated"
)]

raw_results <- merge(
  data.table(zip = all_panel_zips),
  zip_counts,
  by = "zip",
  all.x = TRUE,
  sort = TRUE
)
raw_results[is.na(status), status := "counterfactual_not_estimable"]
fwrite(raw_results, raw_output_path)

estimated <- raw_results[status == "estimated" & expected_deaths > 0]
if (nrow(estimated) < 10L) {
  stop("Fewer than 10 ZIP Poisson counts were estimable; EB smoothing stopped.")
}

# Estimate a Gamma(shape, rate) prior by maximizing the marginal likelihood of
# the Poisson counts. The posterior mean is (observed + shape)/(expected + rate),
# which handles zero counts and shrinks sparse ZIPs more strongly.
fit_gamma_prior <- function(observed, expected) {
  marginal_nll <- function(log_parameters) {
    shape <- exp(log_parameters[1L])
    rate <- exp(log_parameters[2L])
    -sum(
      lgamma(observed + shape) - lgamma(shape) - lgamma(observed + 1) +
        shape * log(rate) + observed * log(expected) -
        (observed + shape) * log(expected + rate)
    )
  }

  mean_ratio <- sum(observed) / sum(expected)
  raw_ratio <- observed / expected
  between_variance <- max(
    stats::var(raw_ratio) - mean(mean_ratio / expected),
    0.01^2
  )
  initial_shape <- mean_ratio^2 / between_variance
  initial_rate <- mean_ratio / between_variance

  fit <- optim(
    par = log(c(initial_shape, initial_rate)),
    fn = marginal_nll,
    method = "L-BFGS-B",
    lower = log(c(1e-6, 1e-6)),
    upper = log(c(1e8, 1e8))
  )
  if (fit$convergence != 0L || !is.finite(fit$value)) {
    stop("Gamma-Poisson EB prior estimation failed: ", fit$message)
  }
  c(shape = exp(fit$par[1L]), rate = exp(fit$par[2L]))
}

prior <- fit_gamma_prior(
  estimated$observed_deaths,
  estimated$expected_deaths
)
prior_shape <- unname(prior[["shape"]])
prior_rate <- unname(prior[["rate"]])
prior_mean <- prior_shape / prior_rate
prior_sd <- sqrt(prior_shape / prior_rate^2)

estimated[, `:=`(
  posterior_shape = prior_shape + observed_deaths,
  posterior_rate = prior_rate + expected_deaths,
  reliability = expected_deaths / (prior_rate + expected_deaths)
)]
estimated[, `:=`(
  rate_ratio_eb = posterior_shape / posterior_rate,
  ci_lower_eb = qgamma(0.025, posterior_shape, rate = posterior_rate),
  ci_upper_eb = qgamma(0.975, posterior_shape, rate = posterior_rate),
  probability_rr_gt_1 = 1 - pgamma(
    1,
    posterior_shape,
    rate = posterior_rate
  )
)]
estimated[, beta_eb := log(rate_ratio_eb)]
setorder(estimated, zip)
fwrite(estimated, estimate_output_path)
setTxtProgressBar(progress, 4L)

metadata <- data.table(
  wind_threshold_kt = wind_threshold,
  age_group = "75+",
  cause = "Cardiovascular",
  target_period = target_period,
  total_panel_zips = length(all_panel_zips),
  estimable_zips = nrow(estimated),
  pooled_log_rate_ratio = pooled_beta,
  pooled_rate_ratio = exp(pooled_beta),
  aggregate_observed_deaths = sum(estimated$observed_deaths),
  aggregate_expected_deaths = sum(estimated$expected_deaths),
  aggregate_standardized_rate_ratio =
    sum(estimated$observed_deaths) / sum(estimated$expected_deaths),
  eb_prior_shape = prior_shape,
  eb_prior_rate = prior_rate,
  eb_prior_mean_rr = prior_mean,
  eb_prior_sd_rr = prior_sd
)
fwrite(metadata, metadata_output_path)

message("Joining estimates to 2020 ZCTA polygons")
options(tigris_use_cache = TRUE)
zcta_us <- tigris::zctas(cb = TRUE, year = 2020, progress_bar = FALSE)
zip_column <- if ("ZCTA5CE20" %in% names(zcta_us)) {
  "ZCTA5CE20"
} else if ("GEOID20" %in% names(zcta_us)) {
  "GEOID20"
} else {
  stop("Could not identify the ZIP-code column in the 2020 ZCTA data.")
}
zcta_us$zip <- as.character(zcta_us[[zip_column]])
zcta_fl <- zcta_us[zcta_us$zip %in% all_panel_zips, c("zip", "geometry")]

map_data <- merge(
  zcta_fl,
  as.data.frame(estimated),
  by = "zip",
  all.x = TRUE,
  sort = FALSE
)
map_data <- st_as_sf(map_data)

if (file.exists(spatial_output_path)) {
  unlink(spatial_output_path)
}
st_write(map_data, spatial_output_path, quiet = TRUE)

# Center the palette on the statewide EB mean. Centering on RR = 1 would make
# nearly every ZIP red when the statewide storm effect is elevated and would
# conceal the small spatial differences the map is intended to display.
map_center <- log(prior_mean)
map_delta <- max(abs(quantile(
  estimated$beta_eb - map_center,
  probs = c(0.02, 0.98),
  na.rm = TRUE
)))
if (!is.finite(map_delta) || map_delta <= 0) {
  map_delta <- 0.01
}
map_limits <- map_center + c(-map_delta, map_delta)

map_plot <- ggplot(map_data) +
  geom_sf(aes(fill = beta_eb), color = "white", linewidth = 0.08) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "#F7F7F7",
    high = "#B2182B",
    midpoint = map_center,
    limits = map_limits,
    oob = scales::squish,
    na.value = "grey85",
    breaks = pretty(map_limits, n = 5),
    labels = function(value) sprintf("%.3f", exp(value)),
    name = sprintf(
      "EB-smoothed\nrate ratio\n(state mean %.3f)",
      prior_mean
    )
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Cardiovascular mortality around tropical-cyclone impact",
    subtitle = sprintf(
      paste0(
        "Adults age 75+, days 0 to +6, winds >=%d kt | ",
        "%s of %s ZIPs estimable"
      ),
      wind_threshold,
      comma(nrow(estimated)),
      comma(length(all_panel_zips))
    ),
    caption = paste0(
      "Expected counts come from the pooled ZIP, year-week, day-of-week, and ",
      "ZIP-by-COVID fixed-effects Poisson model with exposure set to None. ",
      "ZIP observed/expected RRs are gamma-Poisson EB smoothed; gray = no estimate."
    )
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, margin = margin(b = 8)),
    plot.caption = element_text(size = 8, color = "grey35", hjust = 0),
    legend.position = "right"
  )

ggsave(
  figure_output_path,
  map_plot,
  width = 8.5,
  height = 7.5,
  dpi = 300,
  bg = "white"
)
setTxtProgressBar(progress, 5L)
close(progress)

cat("\nCompleted cardiovascular ZIP analysis\n")
cat("ZIPs in panel:", length(all_panel_zips), "\n")
cat("ZIPs with estimable observed/expected counts:", nrow(estimated), "\n")
cat("Pooled impact-period RR:", sprintf("%.4f", exp(pooled_beta)), "\n")
cat(
  "Aggregate observed/expected RR:",
  sprintf("%.4f", metadata$aggregate_standardized_rate_ratio),
  "\n"
)
cat("EB prior mean RR:", sprintf("%.4f", prior_mean), "\n")
cat("EB prior SD:", sprintf("%.4f", prior_sd), "\n")
cat("Raw estimates:", raw_output_path, "\n")
cat("EB estimates:", estimate_output_path, "\n")
cat("Metadata:", metadata_output_path, "\n")
cat("Spatial output:", spatial_output_path, "\n")
cat("Map:", figure_output_path, "\n")
