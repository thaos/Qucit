library(lubridate)
library(foreach)
library(dplyr)
source("tools.r")
# st1 <- load_sid(1)
# lm_fit <- lm(data=st1, bikes_hplus1 ~ month+wday*hour+as.factor(bikes)) 
# lm_fit <- lm(data=st1, bikes_hplus1 ~ month_hplus1+wday_hplus1*hour_hplus1+as.factor(bikes)) 
# lm_fit <- lm(data=head(st1), bikes_hplus1 ~ as.factor(month_hplus1) +
#                as.factor(wday_hplus1)*as.factor(hour_hplus1) +
#                as.factor(bikes)) 
# summary(lm_fit)
# sqrt(mean(residuals(lm_fit)^2))
# length(lm_fit$coefficients)

predict.lmmod <- function(st, formule="bikes_hplus1~ bikes "){
  require(qmap)
  # foreach(i=1:nrow(st), .combine=c) %do% {
  st$month_hplus1 <- as.factor(st$month_hplus1)
  st$wday_hplus1 <- as.factor(st$wday_hplus1)
  st$hour_hplus1 <- as.factor(st$hour_hplus1)
  st$weather <- as.factor(ifelse(!(st$weather_type %in% c("Clear", "Sunny")), "Other", st$weather_type))
  # st$weather <- as.factor(st$weather_type)
  ans <- st$bikes
  week_number <- (as.numeric(st$tms_gmt)-as.numeric(st$tms_gmt[1]))%/%(3600*24*7)
  for( w in unique(week_number)){
    print(w)
    st_sb <- subset(st, week_number <= w, select=c("bikes_hplus1", "bikes", "month_hplus1", "wday_hplus1", "hour_hplus1", "weather", "bikes_128", "bikes_131", "bikes_19", "bikes_21", "bikes_39", "bikes_6", "bikes_60"))
    i_curw <- which(week_number == w)
    i_prew <- which(week_number < w)
    # st_sb <- mutate(st_sb, bikes=as.factor(bikes),
    #                 month_hplus1=as.factor(month_hplus1),
    #                 wday_hplus1=as.factor(month_hplus1),
    #                 hour_hplus1=as.factor(hour_hplus1))
    #+ bikes_128 + bikes_131 + bikes_19 + bikes_21 + bikes_39 + bikes_6 + bikes_60
    if( w <= 2){ 
      formule <- paste(formule, "+ hour_hplus1")
    }
    else{
      if(w <= 2*4){
        formule <- paste(formule, "+ wday_hplus1 * hour_hplus1 ")
      }else{
        # formule <- paste(formule, "+ month_hplus1 + wday_hplus1 * hour_hplus1")
        # formule <- paste(formule, "+ bikes:hour_hplus1 + month_hplus1 + wday_hplus1 * hour_hplus1")
        formule <- paste(formule, "+ bikes:hour_hplus1 + wday_hplus1 * hour_hplus1")
      }
    }
    if(length(unique(st_sb$weather)) >= 2)
      formule <- paste(formule, "+ weather")
#     if(length(unique(st_sb$bikes)) >= 2)
#       st_sb$bikes <- as.factor(st_sb$bikes)
#     else
#       st_sb$bikes <- as.integer(st_sb$bikes)
    pred <- try({
      lm_fit <- lm(formula(formule), data=st_sb[-i_curw, ])      
      print(length(predict(lm_fit)))
      print(length(st$free_slots[i_prew]+st$bikes[i_prew]))
      print(table(
        correct_nb_bikes(predict(lm_fit), st$free_slots[i_prew]+st$bikes[i_prew])
        , st_sb$bikes_hplus[i_prew]))
      predict(lm_fit, newdata = st_sb[i_curw,])
    })
    print(class(pred))
    if(class(pred) == "try-error")
      next
    else
      ans[i_curw] <- ifelse(is.na(pred), st$bikes[i_curw], pred)
  }
  ans
}


# install.packages("mgcv_1.8-11.zip")
# library(mgcv)
# st1 <- load_sid(1)
# st1 <- head(mutate(st1, handm= hour_hplus1 + minute_hplus1/60), n=200)
# gam_fit <- gam(data=st1, bikes_hplus1~ te(wday_hplus1, handm)+ s(bikes) + s(month))
# gam_fit <- gam(data=st1, bikes_hplus1~ s(handm)+ s(bikes) + s(month))
# gam_fit <- gam(data=st1, bikes_hplus1~ s(wday_hplus1, k=2)+ s(handm)+ s(bikes) + s(month))
# gam_fit <- gam(data=st1, bikes_hplus1~ te(wday_hplus1, handm, k=3))
# summary(gam_fit)
# sqrt(mean(residuals(gam_fit)^2))
# fv <- fitted(gam_fit)
# rv <- residuals(gam_fit)
# plot(hexbin(x=fitted(gam_fit), y=residuals(gam_fit)))
# res_fit <- lm(rv~ fv)
# abline(b=coefficients(res_fit)[2], a=coefficients(res_fit)[1])

ok_var <- function(st){
  k <- apply(st, 2, function(x) length(unique(x)))
  var_names <- names(k)
  var_names <- var_names[which(k>=3)]
  k <- k[k>=3]
  data.frame(v=var_names, k=k)
}

paste_s_formula <- function(var, k){
  paste("s(", var, ", k=",k,")", sep="")
}

paste_te_formula <- function(l_var, k){
  var <- paste(l_var, collapse = ", ")
  paste("s(", var, ", k=",k, ")", sep="")
}

create_gam_formula <- function(v, k){
  i_wday <- which(v=="wdays_hplus1")
  i_handm <- which(v=="handm")
  formule=""
  if(length(i_wday) * length(i_handm) == 1){
    formule <- paste_te_formula(v[c(i_wday, i_handm)], min(k[c(i_wday, i_handm)],5))
    v <- v[-c(i_wday, i_handm)]
    k <- k[-c(i_wday, i_handm)]
  }
  formule <- paste(formule, mapply(paste_s_formula, v, min(k,5)), collapse = " + ")
  formule <- paste("bikes_hplus1 ~", formule)
}
    
predict.gammod <- function(st){
  require(mgcv)
  # foreach(i=1:nrow(st), .combine=c) %do% {
  require(splines)
  require(dyn)
  require(zoo)
  ans <- st$bikes
  st$month_hplus1 <- as.factor(st$month_hplus1)
  st$wday_hplus1 <- as.factor(st$wday_hplus1)
  st$hour_hplus1 <- as.factor(st$hour_hplus1)
  st$weather <- as.factor(ifelse(!(st$weather_type %in% c("Clear", "Sunny")), "Other", st$weather_type))
  st$dh_hplus1=st$wday_hplus1+st$hour_hplus1/24+st$minute_hplus1/2
  week_number <- (as.numeric(st$tms_gmt)-as.numeric(st$tms_gmt[1]))%/%(3600*24*7)
  for( w in unique(week_number)){
    print(w)
    i_curw <- which(week_number == w)
    i_prew <- which(week_number < w)
    #formule <- "bikes_hplus1 ~ sbikes + lag(inc_bikes, -4) + lag(bikes, -1) + wdh_plus1"
    #formule_gam <- "bikes_hplus1 ~ s(bikes) + s(`lag(inc_bikes, -4)`) + s(`lag(bikes, -1)`) + s(wdh_hplus)"
    formule <- "bikes_hplus1 ~ bikes + dh_hplus1"
    formule_gam <- "bikes_hplus1 ~ s(bikes) + s(dh_hplus1)"
    if(w >= 2){
      formule <- "bikes_hplus1 ~ bikes + dh_hplus1 + wday_hplus1"
      formule_gam <- "bikes_hplus1 ~ s(bikes) + s(dh_hplus1) + wday_hplus1"
    }
    if(w >= 2*4){
      formule <- "bikes_hplus1 ~ bikes + dh_hplus1 + wday_hplus1" #+ month_hplus1"
      formule_gam <- "bikes_hplus1 ~ s(bikes) + s(dh_hplus1) + wday_hplus1" # + month_hplus1"
    }
    pred <- try({ 
      gam_fit <- gam(data=st[i_prew,], formula(formule_gam))
      #print(summary(gam_fit))
      # print(st_sb[i,])
      print(table(
        correct_nb_bikes(predict(lm_fit), st$free_slots[i_prew]+st$bikes[i_prew])
        , st_sb$bikes_hplus[i_prew]))
      predict(lm_fit, newdata = st_sb[i_curw,])
      predict(gam_fit, newdata = st[i_curw,])
    })
    #print(pred)
    if(class(pred) == "try-error") next 
    else ans[i_curw] <- ifelse(is.na(pred), st$bikes[i_curw], pred)
  }
  ans
}


# install.packages("dyn_0.2-9.zip")
# install.packages("zoo_1.7-12.zip")
# library(dyn)
# st1 <- load_sid(1)
# dyn_fit <- dyn$lm(data=st1, bikes_hplus1 ~ month_hplus1+wday_hplus1*hour_hplus1+bikes+lag(inc_bikes)+lag(inc_bikes, 2)+lag(inc_bikes, 3)+lag(inc_bikes, 4))
# dyn_fit <- dyn$lm(data=st1, bikes_hplus1 ~ month_hplus1+wday_hplus1*hour_hplus1+bikes+lag(inc_bikes))
# plot(hexbin(x=fitted(dyn_fit), y=residuals(gam_fit)))
# head(st1$inc_bikes)
# lag(head(st1$inc_bikes))
# z_inc_bikes <- zoo(st1$inc_bikes, order.by = st1$tms_gmt)
# ts(z_inc_bikes)


predict.dynmod <- function(st){
  # foreach(i=1:nrow(st), .combine=c) %do% {
  require(splines)
  require(dyn)
  require(zoo)
  z_st <- zoo(cbind(tms_gmt=st$tms_gmt,
                    bikes_hplus1=st$bikes_hplus1,
                    bikes=st$bikes, 
                    inc_bikes=st$inc_bikes, 
                    month_hplus1=st$month_hplus1,
                    wday_hplus1=st$wday_hplus1,
                    hour_hplus1=st$hour_hplus1,
                    weather=as.factor(ifelse(!(st$weather_type %in% c("Clear", "Sunny")), "Other", st$weather_type))),
              order.by = st$tms_gmt) 
  print(names(z_st)) 
  week_number <- (as.numeric(st$tms_gmt)-as.numeric(st$tms_gmt[1]))%/%(3600*24*7)
  for( w in unique(week_number)){
    print(w)
    i_curw <- which(week_number == w)
    i_prew <- which(week_number < w)
    # st_sb <- mutate(st_sb, bikes=as.factor(bikes),
    #                 month_hplus1=as.factor(month_hplus1),
    #                 wday_hplus1=as.factor(month_hplus1),
    #                 hour_hplus1=as.factor(hour_hplus1))
    if( w <= 2){
      formule <- "bikes_hplus1 ~ bikes + lag(inc_bikes, -4) + lag(bikes, -1) + hour_hplus1"
      formule_lm <- "bikes_hplus1 ~ bikes + `lag(inc_bikes, -4)` + `lag(bikes, -1)`+ hour_hplus1"
    }
    else{
      if(w <= 2*4){
        formule <- "bikes_hplus1 ~ bikes + lag(inc_bikes, -4) + lag(bikes, -1) + wday_hplus1 * hour_hplus1"
        formule_lm <- "bikes_hplus1 ~ bikes + `lag(inc_bikes, -4)` + `lag(bikes, -1)` + wday_hplus1 * hour_hplus1"
      }else{
        formule <- "bikes_hplus1 ~ bikes*hour_hplus1 + lag(inc_bikes, -4) + lag(bikes, -1)+  wday_hplus1 * hour_hplus1"
        formule_lm <- "bikes_hplus1 ~ bikes*hour_hplus1 + `lag(inc_bikes, -4)` + `lag(bikes, -1)`+  wday_hplus1 * hour_hplus1"
      }
    }
    if(length(unique(z_st$weather)) >= 2){
      formule <- paste(formule, "+ weather")
      formule_lm <- paste(formule_lm, "+ weather")
    }
    ans <- st$bikes
    print(formule)
    # print(summary(lm_fit))
    # print(st_sb[i,])
    pred <- try({
      mf <- dyn$model.frame(formula(formule), data = z_st[week_number <= w,])
      for( ic in 5 : ncol(mf)) mf[, ic] <- as.factor(mf[, ic])
      dyn_fit <- lm(data=mf, formula(formule_lm), subset=-length(i_curw))
      # print(summary(dyn_fit))
      tail(predict(dyn_fit, newdata = tail(mf, n=length(i_curw))), n=length(i_curw))
    })
    print(pred)
    # print(class(pred))
    # browser()
    if(any(class(pred) == "try-error"))
      next
    else{
      print(length(i_curw))
      print(length(pred))
      print(length(ans[i_curw]))
      ans[i_curw] <- ifelse(is.na(pred), st$bikes[i_curw], pred)
    }
  }
  ans
}

predict.nnmod <- function(st, formule="bikes_hplus1~ bikes "){
  require(nnet)
  # foreach(i=1:nrow(st), .combine=c) %do% {
  st$month_hplus1 <- as.factor(st$month_hplus1)
  st$wday_hplus1 <- as.factor(st$wday_hplus1)
  st$bikes <- scale(st$bikes)
  st$bikes_hplus1 <- scale(st$bikes_hplus1)
  st$hm <- scale(st$hour_hplus1 + st$minute_hplus1/60)
  st$weather <- as.factor(ifelse(!(st$weather_type %in% c("Clear", "Sunny")), "Other", st$weather_type))
  # st$weather <- as.factor(st$weather_type)
  ans <- st$bikes
  week_number <- (as.numeric(st$tms_gmt)-as.numeric(st$tms_gmt[1]))%/%(3600*24*7)
  for( w in unique(week_number)){
    print(w)
    st_sb <- subset(st, week_number <= w, select=c("bikes_hplus1", "bikes", "month_hplus1", "wday_hplus1", "hour_hplus1", "hm", "weather"))
    i_curw <- which(week_number == w)
    i_prew <- which(week_number < w)
    if( w <= 2){ 
      formule <- paste(formule, "+ hour_hm")
    }
    else{
        # formule <- paste(formule, "+ month_hplus1 + wday_hplus1 * hour_hplus1")
        # formule <- paste(formule, "+ bikes:hour_hplus1 + month_hplus1 + wday_hplus1 * hour_hplus1")
        formule <- paste(formule, "+ wday_hplus1 + hmlus1")
    }
    if(length(unique(st_sb$weather)) >= 2)
      formule <- paste(formule, "+ weather")
    #     if(length(unique(st_sb$bikes)) >= 2)
    #       st_sb$bikes <- as.factor(st_sb$bikes)
    #     else
    #       st_sb$bikes <- as.integer(st_sb$bikes)
    pred <- try({
      nn_fit <- nnet(bikes_hplus1~bikes + hm + wday_hplus1 + weather, data=st_sb[-i_curw, ],size= 12, linout=TRUE, maxit=200) 
      predict(nn_fit, newdata=st_sb[i_curw,])*attr(st$bikes_hplus1, "scaled:scale") + attr(st$bikes_hplus1, "scaled:center")
    })
    print(class(pred))
    if(class(pred) == "try-error")
      next
    else
      ans[i_curw] <- ifelse(is.na(pred), st$bikes[i_curw], pred)
  }
  ans
}


# 
# plot(st$bikes_hplus1, residuals(nn_fit))
# 



# library(nnet)
# st1 <- load_sid(1)
# st <- st1
# st$month_hplus1 <- as.factor(st$month_hplus1)
# st$wday_hplus1 <- as.factor(st$wday_hplus1)
# # st$hour_hplus1 <- as.factor(st$hour_hplus1)
# st$hm <- scale(st$hour_hplus1 + st$minute_hplus1/60)
# st$bikes <- scale(st$bikes)
# st$bikes_hplus1 <- scale(st$bikes_hplus1)
# st$weather <- as.factor(ifelse(!(st$weather_type %in% c("Clear", "Sunny")), "Other", st$weather_type))
# nn_fit <- nnet(bikes_hplus1~bikes + hm + wday_hplus1 + weather, data=st,
#      size= 12, linout=TRUE, maxit=200) 
# plot(st$bikes_hplus1, residuals(nn_fit))
# nn_pre <- predict(nn_fit)*attr(st$bikes_hplus1, "scaled:scale") + attr(st$bikes_hplus1, "scaled:center")
# plot(st1$bikes_hplus1,correct_nb_bikes(nn_pre, st1$free_slots+st1$bikes))
# plot(st1$bikes_hplus1,correct_nb_bikes(nn_pre, st1$free_slots+st1$bikes))
# rmse(st1$bikes_hplus1,correct_nb_bikes(nn_pre, st1$free_slots+st1$bikes))
# 
# install.packages("randomForest")
# library(randomForest)
# rm_var <-c("sid", "tms_gmt", "status", "latitude", "longitude", "free_slots_hplus1",
#            "occ_hplus1", "inc_bikes", "inc_occ", "year", "month", "wday", "hour", "minute")
# var_names <- names(st1)
# var_names <- var_names[!(var_names %in% rm_var)]
# st <- st1[, var_names]
# rf_fit <- randomForest(bikes_hplus ~ ., data=st, mtry=3, importance=TRUE, na.action=na.omit)
# 
# 
# install.packages("forecast")
# library(forecast)
# y <- msts(x, seasonal.periods=c(7,365.25))
# fit <- tbats(y)
# fc <- forecast(fit)
# plot(fc)