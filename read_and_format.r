library(lubridate)
weather <- read.csv("bordeaux_weather.csv", sep=";")
weather$tms_gmt <- parse_date_time(weather$tms_gmt, "Ymd HMS", tz="GMT", truncated = 3)
names(weather)[1]="w_tms_gmt"

stations <- read.csv("bordeaux_bikeshare_stations.csv", sep=";")

# occupations <- read.csv("bordeaux_bikeshare_occupations.csv", sep=";")
# saveRDS(occupations, file="occupations.RDS")
occupations <- readRDS("occupations.RDS")

l_sid <-  unique(stations$sid)
# dir.create("occupations_stations")

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

find_hour_line <- function(tms_gmt, l_tms_gmt){
  d_tms <- difftime(l_tms_gmt, tms_gmt, units="hours")
  ans <- which(d_tms >= 0 & d_tms < 1)
  if(length(ans) == 0){
    print(tms_gmt)
    ans <- NA
  }
  ans
}

dir.create("occupations_stations_weather")
merge_weather_info <- function(sid){
  socc <-  readRDS(paste("occupations_stations_info/", sid, ".RDS", sep=""))
  i_sweath <- numeric(nrow(socc))
  for( i in 1:nrow(socc)){
    print(i)
    i_sweath[i] <- find_hour_line(socc$tms_gmt[i], weather$w_tms_gmt)
  }
  sweath <- weather[i_sweath, ]
  socc <- cbind(socc, sweath)
  saveRDS(socc, file=paste("occupations_stations_weather/", sid, ".RDS", sep=""))
}
merge_weather_info(l_sid[1])
lapply(l_sid, merge_weather_info)

dir.create("occupations_stations_hplus1")
merge_hplus1 <- function(sid){
  socc <-  readRDS(paste("occupations_stations_weather/", sid, ".RDS", sep=""))
  i_hplus1 <- numeric(nrow(socc))
  for( i in 1:nrow(socc)){
    print(i)
    d_tms <- difftime(socc$tms_gmt[i+(1:4)], socc$tms_gmt[i], units="hours")
    d_tms <- which(d_tms == 1)
    if(length(d_tms) == 0){
      print(socc$tms_gmt[i])
      d_tms <- NA
      browser()
    } else{
      d_tms + i
    }
    i_hplus1[i] <- d_tms
  }
  hplus1 <- socc[i_sweath, c("bikes", "free_slots")]
  names(hplus1) <- c("bikes_hplus1", "free_slots_hplus1")
  socc <- cbind(socc, sweath)
  socc$occ <- socc$bikes / socc$free_slots
  socc$occ_hplus1 <- socc$bikes_hplus1 / socc$free_slots_hplus1
  saveRDS(socc, file=paste("occupations_stations_hplus1/", sid, ".RDS", sep=""))
}
merge_hplus1(l_sid[1])
lapply(l_sid, merge_hplus1)

#TODO Proporly rename hplus 1 column
#TODO Add percentage of bikes available

compute_mean_increment <- function(sid, tms_gmt, by_hour=TRUE, by_wday=TRUE, by_month=FALSE, period=NULL){
    socc
    compute_hour <- (tms_gmt){
        hour <- hour(tms_gmt) + minute(tms_gmt)/60
    }
    hour <- compute_hour(tms_gmt)
    wday <- wday(tms_gmt)
    month <- month(tms_gmt)
    if(by_wdat) wday_cond <- wday(socc$tms_gmt) == wday else wday <- TRUE
    if(by_month) month_cond <- month(socc$tms_gmt) == month else month <- TRUE
    if(by_hour) hour_cond <- compute_hour(socc$tms_gmt) == hour else hout <- TRUE
    # add period condition
    all_cond <-  wday_cond * month_cond * hour_cond
    subset <- tms_gmt[all_cond, c("bike_hplus1", "bikes")]
    ans <- mean(apply(subset, 1, "-"))
    ans
}



