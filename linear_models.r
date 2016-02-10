library(lubridate)
library(foreach)
library(dplyr)

st1 <- load_sid(1)
st1$month <- as.factor(st1$month)
st1$wday <- as.factor(st1$wday)
st1$hour <- as.factor(st1$hour)
st1$month_hplus1 <- as.factor(st1$month_hplus1)
st1$wday_hplus1 <- as.factor(st1$wday_hplus1)
st1$hour_hplus1 <- as.factor(st1$hour_hplus1)
lm_fit <- lm(data=st1, inc_bikes~ bikes+month+wday*hour) 
lm_fit <- lm(data=st1, bikes_hplus1 ~ bikes+month+wday*hour) 
lm_fit <- lm(data=st1, bikes_hplus1 ~ month+wday*hour+as.factor(bikes)) 
lm_fit <- lm(data=st1, bikes_hplus1 ~ month_hplus1+wday_hplus1*hour_hplus1+as.factor(bikes)) 
lm_fit <- lm(data=head(st1), bikes_hplus1 ~ as.factor(month_hplus1) +
               as.factor(wday_hplus1)*as.factor(hour_hplus1) +
               as.factor(bikes)) 
summary(lm_fit)
sqrt(mean(residuals(lm_fit)^2))
length(lm_fit$coefficients)

predict.lmmod <- function(st){
  # foreach(i=1:nrow(st), .combine=c) %do% {
  st$month <- as.factor(st$month)
  st$wday <- as.factor(st$wday)
  st$hour <- as.factor(st$hour)
  st$month_hplus1 <- as.factor(st$month_hplus1)
  st$wday_hplus1 <- as.factor(st$wday_hplus1)
  st$hour_hplus1 <- as.factor(st$hour_hplus1)
  ans <- st$bikes
  for( i in 2:nrow(st)){
    print(i)
    st_sb <- select(st[1:i, ], bikes_hplus1, bikes, month_hplus1, wday_hplus1, hour_hplus1)
    # st_sb <- mutate(st_sb, bikes=as.factor(bikes),
    #                 month_hplus1=as.factor(month_hplus1),
    #                 wday_hplus1=as.factor(month_hplus1),
    #                 hour_hplus1=as.factor(hour_hplus1))
    nb_lvl <- apply(st_sb[-i, -1], 2, function(x) length(unique(x)))
    # print(nb_lvl)
    var_names <- names(nb_lvl)
    var_names <- var_names[which(nb_lvl >1)]
    if(length(var_names)>=1){
      formule <- paste(var_names,collapse = "+")
      if("wday_hplus" %in% var_names & "hour_hplus1" %in% var_names)
        formule <- paste(formule, "+wdays_hplus1:hour_hplus1", sep="")
      formule <- paste("bikes_hplus1~", formule, sep="")
      # print(formule)
      lm_fit <- lm(formula(formule), data=st_sb[-i, ])
      # print(summary(lm_fit))
      # print(st_sb[i,])
      pred <- try(
        predict(lm_fit, newdata = st_sb[i,])
      )
      print(class(pred))
      if(class(pred) == "try-error")
        next
      else
        ans[i] <- pred
    }else{
      next
    }
  }
  ans
}
st <- head(st1, n=1000)
p_lmmod <- predict.lmmod(st)
p_lmmod <- correct_nb_bikes(round(p_lmmod), st$bikes+st$free_slots)
rmse(p_lmmod, st$bikes_hplus1)
                          
  
install.packages("mgcv_1.8-11.zip")
library(mgcv)
st1 <- load_sid(1)
st1 <- head(mutate(st1, handm= hour_hplus1 + minute_hplus1/60), n=200)
gam_fit <- gam(data=st1, bikes_hplus1~ te(wday_hplus1, handm)+ s(bikes) + s(month))
gam_fit <- gam(data=st1, bikes_hplus1~ s(handm)+ s(bikes) + s(month))
gam_fit <- gam(data=st1, bikes_hplus1~ s(wday_hplus1, k=2)+ s(handm)+ s(bikes) + s(month))
gam_fit <- gam(data=st1, bikes_hplus1~ te(wday_hplus1, handm, k=3))
summary(gam_fit)
sqrt(mean(residuals(gam_fit)^2))
fv <- fitted(gam_fit)
rv <- residuals(gam_fit)
plot(hexbin(x=fitted(gam_fit), y=residuals(gam_fit)))
res_fit <- lm(rv~ fv)
abline(b=coefficients(res_fit)[2], a=coefficients(res_fit)[1])

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
  ans <- st$bikes
  st <- mutate(st, handm= hour_hplus1 + minute_hplus1/60)
  for( i in 2:nrow(st)){
    print(i)
    st_sb <- select(st[1:i, ], bikes_hplus1, bikes, month_hplus1, wday_hplus1, handm)
    in_mod <- ok_var(select(st_sb, -bikes_hplus1))
    if(nrow(in_mod) == 0) next
    formule <- create_gam_formula(in_mod$v, in_mod$k)
    print(formule)
    pred <- try({
      gam_fit <- gam(data=st_sb[-i, ], formula(formule))
      # print(summary(lm_fit))
      # print(st_sb[i,])
      predict(gam_fit, newdata = st_sb[i,])
    })
    print(pred)
    if(class(pred) == "try-error") next else ans[i] <- pred
  }
  ans
}
st1 <- load_sid(1)
st <- head(st1, n=100)
p_gammod <- predict.gammod(st)
p_gammod <- correct_nb_bikes(round(p_gammod), st$bikes+st$free_slots)
rmse(p_gammod, st$bikes_hplus1)
                          


install.packages("dyn_0.2-9.zip")
install.packages("zoo_1.7-12.zip")
library(dyn)
st1 <- load_sid(1)
dyn_fit <- dyn$lm(data=st1, bikes_hplus1 ~ month_hplus1+wday_hplus1*hour_hplus1+bikes+lag(inc_bikes)+lag(inc_bikes, 2)+lag(inc_bikes, 3)+lag(inc_bikes, 4))
dyn_fit <- dyn$lm(data=st1, bikes_hplus1 ~ month_hplus1+wday_hplus1*hour_hplus1+bikes+lag(inc_bikes))
plot(hexbin(x=fitted(dyn_fit), y=residuals(gam_fit)))
head(st1$inc_bikes)
lag(head(st1$inc_bikes))
z_inc_bikes <- zoo(st1$inc_bikes, order.by = st1$tms_gmt)
ts(z_inc_bikes)


predict.dynmod <- function(st){
  # foreach(i=1:nrow(st), .combine=c) %do% {
  st$month <- as.factor(st$month)
  st$wday <- as.factor(st$wday)
  st$hour <- as.factor(st$hour)
  st$month_hplus1 <- as.factor(st$month_hplus1)
  st$wday_hplus1 <- as.factor(st$wday_hplus1)
  st$hour_hplus1 <- as.factor(st$hour_hplus1)
  ans <- st$bikes
  for( i in 2:nrow(st)){
    print(i)
    st_sb <- select(st[1:i, ], bikes_hplus1, bikes, month_hplus1, wday_hplus1, hour_hplus1)
    # st_sb <- mutate(st_sb, bikes=as.factor(bikes),
    #                 month_hplus1=as.factor(month_hplus1),
    #                 wday_hplus1=as.factor(month_hplus1),
    #                 hour_hplus1=as.factor(hour_hplus1))
    nb_lvl <- apply(st_sb[-i, -1], 2, function(x) length(unique(x)))
    # print(nb_lvl)
    var_names <- names(nb_lvl)
    var_names <- var_names[which(nb_lvl >1)]
    if(length(var_names)>=1){
      formule <- paste(var_names,collapse = "+")
      if("wday_hplus" %in% var_names & "hour_hplus1" %in% var_names)
        formule <- paste(formule, "+wdays_hplus1:hour_hplus1", sep="")
      formule <- paste("bikes_hplus1~", formule, sep="")
      # print(formule)
      lm_fit <- lm(formula(formule), data=st_sb[-i, ])
      # print(summary(lm_fit))
      # print(st_sb[i,])
      pred <- try(
        predict(lm_fit, newdata = st_sb[i,])
      )
      print(class(pred))
      if(class(pred) == "try-error")
        next
      else
        ans[i] <- pred
    }else{
      next
    }
  }
  ans
}

install.packages("forecast")
library(forecast)
y <- msts(x, seasonal.periods=c(7,365.25))
fit <- tbats(y)
fc <- forecast(fit)
plot(fc)