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
pairs(select(st1,tms_gmt, windchill, wind, windir, pressure, gust, precipitation, bikes))
pairs(select(st1, occ, occ_hplus1, temperature, precipitation, wind))
pairs(select(st1, occ, occ_hplus1))
cor(select(st1, occ, occ_hplus1), use="pairwise.complete.obs")
pairs(select(st1, occ, inc_occ))
cor(select(st1, occ, inc_occ), use="pairwise.complete.obs")
pairs(select(st1, occ, inc_occ, temperature))
cor(select(st1, occ, inc_occ, temperature, precipitation, wind), use="pairwise.complete.obs")
acf(as.ts(select(filter(st1, minute==0),inc_occ)), na.action=na.pass)
pacf(as.ts(select(filter(st1, minute==0),inc_occ)), na.action=na.pass)
plot(diff(st1$inc_occ), st1$inc_occ[-1])
cor(diff(st1$inc_occ), st1$inc_occ[-1], use="pairwise.complete.obs")


#TODO Proporly rename hplus 1 column
#TODO Add percentage of bikes available

compute_mean_increment_tab <- function(socc, max_tms, variable, 
                                       grouping=c("year",
                                                  "month",
                                                  "wday",
                                                  "hour",
                                                  "minute")){
    require(dplyr)
    i_ant <- which(difftime(max_tms, socc$tms_gmt) > 0)
    socc <- socc[i_ant, c(grouping, variable)]
    by_cycle <- group_by_(socc, .dots=grouping)
    # ddply(data,.(year, month, wday, hour, minute), mean, .parallel=TRUE)
    summarise_each_(by_cycle, funs(mean), vars=variable)
}
st1_it <- compute_mean_increment_tab(st1, max(st1$tms_gmt, na.rm=TRUE), variable="inc_bikes", grouping=c("wday", "hour"))
ggplot(data=st1_it, aes(x=hour, y=inc_bikes, group=wday, color=as.factor(wday)))+ geom_line()

st1_it <- compute_mean_increment_tab(st1, max(st1$tms_gmt, na.rm=TRUE), variable="bikes", grouping=c("month", "wday"))
ggplot(data=st1_it, aes(x=month, y=bikes, group=wday, color=as.factor(wday)))+ geom_line()


get_increment <- function(inc_table, tms_gmt, variable="inc_bikes", grouping=c("wday", "hour")){
  line_to_find <- unlist(lapply(grouping, function(x) do.call(x,args=list(x=tms_gmt))))
  i_line <- which(apply(inc_table[,grouping], 1, function(x) all(x == line_to_find)))
  #print(inc_table[i_line,])
  ans <- inc_table[i_line, variable]
  ans
}
st1_inc <- get_increment(st1_it, max(st1$tms_gmt, na.rm=TRUE))

correct_nb_bikes <- function(nb_bikes, total_slots){
  nb_bikes <- ifelse(nb_bikes > total_slots, total_slots, nb_bikes)
  nb_bikes <- ifelse(nb_bikes < 0, 0, nb_bikes)
  nb_bikes
}

correct_occ <- function(occ){
  occ <- ifelse(occ > 1, 1, occ)
  occ <- ifelse(occ < 0, 0, occ)
  occ
}

increment_model <- function(socc, tms_gmt, variable="bikes", grouping=c("wday", "hour")){
  inc_var <- paste("inc_", variable, sep="")
  inc_table <- compute_mean_increment_tab(socc, tms_gmt, inc_var, grouping)
  inc <- get_increment(inc_table, tms_gmt+3600, inc_var, grouping)
  i_ans <-  fmatch(tms_gmt, socc$tms_gmt)
  socc <- socc[i_ans, ]
  ans <- socc[, variable] + inc
  ans
}
st1_incmod <- increment_model(st1, max(st1$tms_gmt, na.rm=TRUE))

rmse <- function(predicted, truth) sqrt(mean((predicted - truth)^2))

predict.increment_model <- function(socc, variable="bikes", grouping=c("wday", "hour")){
  ans <- foreach(tms=socc$tms_gmt, .combine=c) %do% {
    ans <- increment_model(socc, tms, variable, grouping)
    print(ans)
    ans
  }
  # browser()
  ans <- as.numeric(ans)
  ans[is.na(ans)] <- socc[is.na(ans), variable]
  browser()
  if(variable == "bikes") ans <- correct_nb_bikes(round(ans), socc$bikes+socc$free_slots)
  if(variable == "occ") ans <- correct_occ(ans)
  ans
}
print(system.time({p_incmod <- predict.increment_model(head(st1))}))
print(rmse(p_incmod, head(st1$bikes_hplus1)))
print(system.time({p_incmod <- predict.increment_model(st1)}))
print(rmse(p_incmod, st1$bikes_hplus1))
saveRDS(p_incmod, file="p_incmod.RDS")
print(system.time({p_incmod_month <- predict.increment_model(st1, grouping=c("month", "wday", "hour"))}))
print(rmse(p_incmod_month, st1$bikes_hplus1))
saveRDS(p_incmod_month, file="p_incmod_month.RDS")

predict.static_model <- function(socc, variable="bikes", grouping=c("wday", "hour")){
  ans <- socc[, variable]
  ans
}
p_stamod <- predict.static_model(st1)
print(rmse(p_stamod, st1$bikes_hplus1))


occ_to_bikes <- function(occ, total_slots) {
  ans <- round(occ * total_slots)
  ans <- correct_nb_bikes(ans, total_slots)
  ans
}

library(ggmap)
bordeaux_map <- get_googlemap("bordeaux", scale=1, zoom = 12)
saveRDS(bordeaux_map, file="bordeaux_map.RDS")
bordeaux_map <- readRDS(file="bordeaux_map.RDS")
plot_station_map <- function(data, variable){
  require(ggmap)
  bmap <- ggmap(bordeaux_map, legend="topleft")
  bmap + 
    geom_point(aes_string(x="longitude", y="latitude", colour=variable, size=variable), data=data)
}
bdata <- read.csv("bordeaux_bikeshare_stations.csv", sep=";")
bdata <- arrange(cbind(bdata, color=(bdata$sid==1)), desc(sid))
plot_station_map(bdata, "color")

plot_sid_ts <- function(l_sid, variable="bikes"){
  require(ggplot2)
  data <- foreach(cur_sid = l_sid, .combine=rbind) %dopar% {
    cur_socc <- load_sid(cur_sid)  
    cur_socc <- cur_socc[,c("sid", "tms_gmt", variable)]
    cur_socc <- cbind(cur_socc, col=(cur_socc$sid == l_sid[1]))
  }
  ggpot(data=data, aes_strings(x="tms_gmt", y=substitute(variable), group="sid", color="col"))
}
 
compute_sid_corr <- function(sid, l_sid, variable="bikes"){
  require(geosphere)
  socc <- load_sid(sid)
  coord <- socc[1, c("latitude", "longitude")]
  socc <- socc[,c("tms_gmt", variable)]
  names(socc)[2] <- paste(variable,"_", sid, sep="")
  v_corr <- foreach(cur_sid = l_sid, .combine=rbind) %dopar% {
    cur_socc <- load_sid(cur_sid)  
    cur_coord <- cur_socc[1, c("latitude", "longitude")]
    cur_socc <- cur_socc[,c("tms_gmt", variable)]
    names(cur_socc)[2] <- paste(variable,"_", sid, sep="")
    cur_socc <- merged(socc, cur_socc, by="tms_gmt")
    corr <- cor(cur_socc[,-1], socc[, -1], use="pairwise.complete.obs")
    dist <- distm(coord, cur_coord)
    data.frame(corr=corr, dist=dist)
  }
  v_corr
}

compute_sid_corr <- function(sid, l_sid, variable="bikes"){
  require(geosphere)
  socc <- load_sid(sid)
  coord <- socc[1, c("latitude", "longitude")]
  socc <- socc[,c("tms_gmt", variable)]
  names(socc)[2] <- paste(variable,"_", sid, sep="")
  v_corr <- foreach(cur_sid = l_sid, .combine=rbind) %dopar% {
    cur_socc <- load_sid(cur_sid)  
    cur_coord <- cur_socc[1, c("latitude", "longitude")]
    cur_socc <- cur_socc[,c("tms_gmt", variable)]
    names(cur_socc)[2] <- paste(variable,"_", sid, sep="")
    cur_socc <- merged(socc, cur_socc, by="tms_gmt")
    corr <- cor(cur_socc[,-1], socc[, -1], use="pairwise.complete.obs")
    dist <- distm(coord, cur_coord)
    data.frame(corr=corr, dist=dist)
  }
  v_corr
}

compute_sid_trend <- function(sid, l_sid, variable="bikes"){
  trends <- foreach(cur_sid = l_sid, .combine=c) %dopar% {
    cur_socc <- load_sid(cur_sid)  
    cur_socc <- cur_socc[,c("tms_gmt", variable)]
    tms_gmt <- cur_socc$tms_gmt
    y <- cur_socc[,variable]
    lm_fit <- lm(y~tms_gmt)
    trend <- coefficients(lm_fit)[2]
  }
  trends
}
 
  
  
  


