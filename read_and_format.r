library(lubridate)
library(dplyr)
library(ggplot2)
library(doMC)
library(fastmatch)
registerDoMC(cores=2)
weather <- read.csv("bordeaux_weather.csv", sep=";")
weather$tms_gmt <- parse_date_time(weather$tms_gmt, "Ymd HMS", tz="GMT", truncated = 3)
names(weather)[1]="w_tms_gmt"

stations <- read.csv("bordeaux_bikeshare_stations.csv", sep=";")

occupations <- read.csv("bordeaux_bikeshare_occupations.csv", sep=";")
saveRDS(occupations, file="occupations.RDS")
occupations <- readRDS("occupations.RDS")
stations <- read.csv("bordeaux_bikeshare_stations.csv", sep=";")
l_sid <-  unique(stations$sid)
dir.create("occupations_stations")

split_by_station <- function(sid){
  socc <- occupations[occupations$sid == sid, ]
  socc$tms_gmt <- parse_date_time(socc$tms_gmt, "Ymd HMS", tz="GMT", truncated = 3)
  saveRDS(socc, file=paste("occupations_stations/", sid, ".RDS", sep=""))
}

split_by_station(l_sid[1])
lapply(l_sid, split_by_station)

dir.create("occupations_stations_info")
merge_station_info <- function(sid){
  socc <-  readRDS(paste("occupations_stations/", sid, ".RDS", sep=""))
  socc <- merge(socc, stations, by="sid")
  saveRDS(socc, file=paste("occupations_stations_info/", sid, ".RDS", sep=""))
}
merge_station_info(l_sid[1])
lapply(l_sid, merge_station_info)

dir.create("occupations_stations_hplus1")
merge_hplus1 <- function(sid){
  require(foreach)
  socc <-  readRDS(paste("occupations_stations_info/", sid, ".RDS", sep=""))
  i_hplus1 <- foreach(tms=socc$tms_gmt, .combine=c) %dopar%{
    fmatch(tms+3600,socc$tms_gmt)
  }
  hplus1 <- socc[i_hplus1, c("bikes", "free_slots")]
  names(hplus1) <- c("bikes_hplus1", "free_slots_hplus1")
  socc <- cbind(socc, hplus1)
  socc$occ <- socc$bikes / (socc$bikes + socc$free_slots)
  socc$occ_hplus1 <- socc$bikes_hplus1 / (socc$bikes_hplus1 + socc$free_slots_hplus1)
  socc$inc_bikes <- socc$bikes_hplus1 - socc$bikes
  socc$inc_occ <- socc$occ_hplus1 - socc$occ
  saveRDS(socc, file=paste("occupations_stations_hplus1/", sid, ".RDS", sep=""))
}
merge_hplus1(l_sid[1])
lapply(l_sid, merge_hplus1)

dir.create("occupations_time_info")
merge_time_info <- function(sid){
  require(foreach)
  socc <-  readRDS(paste("occupations_stations_hplus1/", sid, ".RDS", sep=""))
  socc$year <- year(socc$tms_gmt)
  socc$month <- month(socc$tms_gmt)
  socc$wday <- wday(socc$tms_gmt)
  socc$hour <- hour(socc$tms_gmt)
  socc$minute <- minute(socc$tms_gmt)
  socc$year_hplus1 <- year(socc$tms_gmt+3600)
  socc$month_hplus1 <- month(socc$tms_gmt+3600)
  socc$wday_hplus1 <- wday(socc$tms_gmt+3600)
  socc$hour_hplus1 <- hour(socc$tms_gmt+3600)
  socc$minute_hplus1 <- minute(socc$tms_gmt+3600)
  saveRDS(socc, file=paste("occupations_time_info/", sid, ".RDS", sep=""))
}
merge_time_info(l_sid[1])
lapply(l_sid, merge_time_info)

dir.create("occupations_stations_weather")
merge_weather_info <- function(sid){
  require(foreach)
  socc <-  readRDS(paste("occupations_time_info/", sid, ".RDS", sep=""))
  weather <- read.csv("bordeaux_weather.csv", sep=";")
  weather$tms_gmt <- parse_date_time(weather$tms_gmt, "Ymd HMS", tz="GMT", truncated = 3)
  w_hour <- as.integer(as.numeric(weather$tms_gmt) %/% 3600)
  s_hour <- as.integer(as.numeric(socc$tms_gmt) %/% 3600)
  names(weather)[1]="w_tms_gmt"
  #i_sweath <- numeric(nrow(socc))
  i_sweath <- foreach(h = s_hour, .combine=c) %dopar% {
    #print(h)
    ans <- fmatch(h, w_hour)
    #print(weather$w_tms_gmt[ans])
    ans
  }
  sweath <- weather[i_sweath, ]
  socc <- cbind(socc, sweath)
  saveRDS(socc, file=paste("occupations_stations_weather/", sid, ".RDS", sep=""))
}
merge_weather_info(l_sid[1])
lapply(l_sid, merge_weather_info)
# library(parallel)
# mclapply(l_sid, merge_weather_info)


load_sid <- function(sid){
  socc <-  readRDS(paste("occupations_stations_weather/", sid, ".RDS", sep=""))
  i_na <- which(is.na(socc$bikes_hplus))
  if(length(i_na) == 0) i_na=FALSE
  i_col <- which(names(socc) %in% c("name", "last_update", "address", "total_slots", "banking", "movable", "extra", "w_tms_gmt"))
  socc[-i_na,-i_col]
}
st1 <- load_sid(1)
# pairs(select(st1,tms_gmt, windchill, wind, windir, pressure, gust, precipitation, bikes))
# pairs(select(st1, occ, occ_hplus1, temperature, precipitation, wind))
# pairs(select(st1, occ, occ_hplus1))
# cor(select(st1, occ, occ_hplus1), use="pairwise.complete.obs")
# pairs(select(st1, occ, inc_occ))
# cor(select(st1, occ, inc_occ), use="pairwise.complete.obs")
# pairs(select(st1, occ, inc_occ, temperature))
# cor(select(st1, occ, inc_occ, temperature, precipitation, wind), use="pairwise.complete.obs")
# acf(as.ts(select(filter(st1, minute==0),inc_occ)), na.action=na.pass)
# pacf(as.ts(select(filter(st1, minute==0),inc_occ)), na.action=na.pass)
# plot(diff(st1$inc_occ), st1$inc_occ[-1])
# cor(diff(st1$inc_occ), st1$inc_occ[-1], use="pairwise.complete.obs")


#TODO Proporly rename hplus 1 column
#TODO Add percentage of bikes available

