suppressPackageStartupMessages(library(data.table))
setDTthreads(2)
out <- 'outputs/births/logistic-window1-storm-inventory'
dir.create(out, recursive=TRUE, showWarnings=FALSE)
b <- as.data.table(readRDS('data/births/processed/All_Births_Corrected_Storm_Exposure-trimester-eligible-v2.rds'))
# Match the union cohort in HO-storms-models.Rmd before outcome-specific filters.
b <- b[exposure_eligible %in% TRUE & suppressWarnings(as.integer(PLURALITY_CODE)) == 1L &
 !is.na(suppressWarnings(as.numeric(GESTATION_WEEKS))) &
 !is.na(suppressWarnings(as.numeric(BIRTH_WEIGHT_GRAMS))) &
 !is.na(conception_year) & conception_month %in% 1:12 & !is.na(county_geoid),
 .(ID, exposure_eligible_T1, exposure_eligible_T2, exposure_eligible_T3)]
d <- fread('data/births/raw/all_births_hurricane.csv',select=c('ID','DATE_OF_BIRTH','trimester1_s_date','trimester1_e_date','trimester2_s_date','trimester2_e_date','trimester3_s_date'))
d <- merge(b,d,by='ID',sort=FALSE)
cols <- c('DATE_OF_BIRTH','trimester1_s_date','trimester1_e_date','trimester2_s_date','trimester2_e_date','trimester3_s_date')
d[, (cols):=lapply(.SD,as.IDate),.SDcols=cols]
d[, `:=`(T1_start=trimester1_s_date,T1_end=pmin(trimester1_e_date,DATE_OF_BIRTH),T2_start=trimester2_s_date,T2_end=pmin(trimester2_e_date,DATE_OF_BIRTH),T3_start=trimester3_s_date,T3_end=DATE_OF_BIRTH)]
e <- fread('data/births/derived/birth-storm-events-time-specific-trimester-eligible-v2.csv',select=c('ID','SID','NAME','threshold_kt','local_impact_date','USA_WIND','observed_quadrants'))[threshold_kt==34L]
e[, local_impact_date:=as.IDate(local_impact_date)]
e <- merge(e,d,by='ID',sort=FALSE)
h <- rbindlist(lapply(1:3,function(k){
 start <- paste0('T',k,'_start'); end <- paste0('T',k,'_end'); eligible <- paste0('exposure_eligible_T',k)
 e[get(eligible)%in%TRUE & !is.na(get(start)) & !is.na(get(end)) & local_impact_date-1L<=get(end) & local_impact_date+1L>=get(start),
 .(ID,SID,NAME,local_impact_date,USA_WIND,observed_quadrants,trimester=paste0('T',k))]
}))
h <- unique(h)
# Match archived prevalence before accepting the inventory.
qa <- h[,.(exposed=uniqueN(ID)),by=trimester]
expected <- fread('models/births/conventional-window1/corrected_exposure_prevalence.csv')
expected[,trimester:=sub('exposed_window_(T[123])_34','\\1',exposure)]
qa <- merge(qa,expected[,.(trimester,eligible=n_eligible,expected=n_exposed)],by='trimester')
stopifnot(all(qa$exposed==qa$expected))
fwrite(qa,file.path(out,'verification.csv'))
storms <- h[,.(first_local_impact=min(local_impact_date),last_local_impact=max(local_impact_date),exposed_births_any_trimester=uniqueN(ID),min_impact_wind_kt=min(USA_WIND),max_impact_wind_kt=max(USA_WIND)),by=.(SID,NAME)]
wide <- dcast(h[,.(exposed_births=uniqueN(ID)),by=.(SID,trimester)],SID~trimester,value.var='exposed_births',fill=0L)
storms <- merge(storms,wide,by='SID');setorder(storms,first_local_impact,SID)
fwrite(storms,file.path(out,'storms.csv'))
h[,impact_year:=as.integer(format(local_impact_date,'%Y'))]
annual <- h[,.(distinct_storms=uniqueN(SID),exposed_births_any_trimester=uniqueN(ID)),by=impact_year]
annual <- merge(data.table(impact_year=2000:2022),annual,by='impact_year',all.x=TRUE)
annual[is.na(distinct_storms),`:=`(distinct_storms=0L,exposed_births_any_trimester=0L)]
fwrite(annual,file.path(out,'annual.csv'))
print(qa);print(annual);cat('Distinct storms:',nrow(storms),'\nUnique exposed births:',uniqueN(h$ID),'\n')
