load_sid <- function(sid){
  socc <-  readRDS(paste("occupations_stations_weather/", sid, ".RDS", sep=""))
  i_na <- which(is.na(socc$bikes_hplus))
  if(length(i_na) == 0) i_na=FALSE
  i_col <- which(names(socc) %in% c("name", "last_update", "address", "total_slots", "banking", "movable", "extra", "w_tms_gmt"))
  socc[-i_na,-i_col]
}

correct_nb_bikes <- function(nb_bikes, total_slots){
  nb_bikes <- round(nb_bikes)
  nb_bikes <- ifelse(nb_bikes > total_slots, total_slots, nb_bikes)
  nb_bikes <- ifelse(nb_bikes < 0, 0, nb_bikes)
  nb_bikes
}

rmse <- function(predicted, truth) sqrt(mean((predicted - truth)^2))

correct_occ <- function(occ){
  occ <- ifelse(occ > 1, 1, occ)
  occ <- ifelse(occ < 0, 0, occ)
  occ
}

occ_to_bikes <- function(occ, total_slots) {
  ans <- round(occ * total_slots)
  ans <- correct_nb_bikes(ans, total_slots)
  ans
}

# library(ggmap)
# bordeaux_map <- get_googlemap("bordeaux", scale=1, zoom = 12)
# saveRDS(bordeaux_map, file="bordeaux_map.RDS")
plot_station_map <- function(data, variable){
  require(ggmap)
  bordeaux_map <- readRDS(file="bordeaux_map.RDS")
  bmap <- ggmap(bordeaux_map, legend="topleft")
  bmap + 
    geom_point(aes_string(x="longitude", y="latitude", colour=variable, size=variable), data=data)
}
# bdata <- read.csv("bordeaux_bikeshare_stations.csv", sep=";")
# bdata <- arrange(cbind(bdata, color=(bdata$sid==1)), desc(sid))
# plot_station_map(bdata, "color")

gather_sid_ts <- function(l_sid){
  require(geosphere)
  require(foreach)
  for(i in seq_along(l_sid)){
    # v_corr <- foreach(cur_sid = l_sid, .combine=rbind) %do% {
    cur_sid <- l_sid[i]
    cur_socc <- load_sid(cur_sid)  
    if( i== 1){
      ans <- cur_socc[,c("tms_gmt", "bikes_hplus1","bikes")]
      next
    }
    cur_socc <- cur_socc[,c("tms_gmt", "bikes")]
    names(cur_socc)[2] <- paste("bikes_", l_sid[i], sep="")
    ans <- merge(ans, cur_socc, by="tms_gmt", all=TRUE)
  }
  names(ans)[-c(1,2)] <- paste("bikes_",l_sid,sep="")
  ans
}
# l_sid <-  unique(stations$sid)
# s_ts <- gather_sid_ts(head(l_sid))
# s_ts <- gather_sid_ts(l_sid)
# s_ts <- readRDS("s_ts.RDS")
# s_ts <- readRDS("../../../Downloads/s_ts.RDS")
# s_corr <- cor(s_ts[,-(1:2)])[,1]
# i_station <- 2+which(abs(s_corr) < 0.5)
# lm_alls <- lm(data=s_ts[,-c(1, i_station)], bikes_hplus1 ~ .)
# stp_lm <- step(lm_alls)
# var2include <- names(stp_lm$coefficients)
# st1_m <- gather_sid_ts(c(128, 131, 19, 21, 39, 6, 60))
# st1_m <- merge(st1, st1_m[,-2], by="tms_gmt", all.x=TRUE)
# # st1_m <- merge(st1, s_ts[, c("tms_gmt", var2include[-c(1:2)])], by="tms_gmt")
# saveRDS(st1_m, file="st1_m.RDS")

compute_sid_corr <- function(sid, l_sid, variable="bikes"){
  require(geosphere)
  require(foreach)
  socc <- load_sid(sid)
  coord <- socc[1, c("longitude", "latitude")]
  socc <- socc[,c("tms_gmt", variable)]
  names(socc)[2] <- paste(variable,"_", sid, sep="")
  v_corr <- matrix(NA, nrow=length(l_sid), ncol=2)
  for(i in seq_along(l_sid)){
    # v_corr <- foreach(cur_sid = l_sid, .combine=rbind) %do% {
    cur_sid <- l_sid[i]
    cur_socc <- load_sid(cur_sid)  
    cur_coord <- cur_socc[1, c("longitude", "latitude")]
    cur_socc <- cur_socc[,c("tms_gmt", variable)]
    names(cur_socc)[2] <- paste(variable,"_", sid, sep="")
    cur_socc <- merge(socc, cur_socc, by="tms_gmt")
    corr <- cor(cur_socc[,2], cur_socc[, 3], use="pairwise.complete.obs")
    dist <- distm(coord, cur_coord)
    v_corr[i,] <- c(corr, dist)
  }
  v_corr
}
# stations <- read.csv("bordeaux_bikeshare_stations.csv", sep=";")
# l_sid <-  unique(stations$sid)
# v_corr <- compute_sid_corr(1, l_sid, variable="bikes")
# s_corr <- cbind(stations,v_corr)
# names(s_corr)[10:11] <- c("corr", "dist")
# plot_station_map(s_corr, "corr")

plot_sid_ts <- function(l_sid, variable="bikes"){
  require(ggplot2)
  data <- foreach(cur_sid = l_sid, .combine=rbind) %dopar% {
    cur_socc <- load_sid(cur_sid)  
    cur_socc <- cur_socc[,c("sid", "tms_gmt", variable)]
    cur_socc <- cbind(cur_socc, col=(cur_socc$sid == l_sid[1]))
  }
  ggpot(data=data, aes_strings(x="tms_gmt", y=substitute(variable), group="sid", color="col"))
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

