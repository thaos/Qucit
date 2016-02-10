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