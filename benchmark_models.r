compute_mean_increment_tab <- function(socc, max_tms, variable, 
                                       grouping=c("year",
                                                  "month",
                                                  "wday",
                                                  "hour",
                                                  "minute")){
  require(dplyr)
  require(lubridate)
  i_ant <- which(difftime(max_tms, socc$tms_gmt) >= 0)
  socc <- socc[i_ant, c(grouping, variable)]
  by_cycle <- group_by_(socc, .dots=grouping)
  # ddply(data,.(year, month, wday, hour, minute), mean, .parallel=TRUE)
  summarise_each_(by_cycle, funs(mean), vars=variable)
}
# st1_it <- compute_mean_increment_tab(st1, max(st1$tms_gmt, na.rm=TRUE), variable="inc_bikes", grouping=c("wday", "hour"))
# ggplot(data=st1_it, aes(x=hour, y=inc_bikes, group=wday, color=as.factor(wday)))+ geom_line()
# 
# st1_it <- compute_mean_increment_tab(st1, max(st1$tms_gmt, na.rm=TRUE), variable="bikes", grouping=c("month", "wday"))
# ggplot(data=st1_it, aes(x=month, y=bikes, group=wday, color=as.factor(wday)))+ geom_line()

get_increment <- function(inc_table, tms_gmt, variable="inc_bikes", grouping=c("wday", "hour")){
  line_to_find <- unlist(lapply(grouping, function(x) do.call(x,args=list(x=tms_gmt))))
  i_line <- which(apply(inc_table[,grouping], 1, function(x) all(x == line_to_find)))
  #print(inc_table[i_line,])
  ans <- inc_table[i_line, variable]
  ans
}
# st1_inc <- get_increment(st1_it, max(st1$tms_gmt, na.rm=TRUE))

increment_model <- function(socc, tms_gmt, variable="bikes", grouping=c("wday", "hour")){
  require(fastmatch)
  inc_var <- paste("inc_", variable, sep="")
  inc_table <- compute_mean_increment_tab(socc, tms_gmt-3600, inc_var, grouping)
  inc <- get_increment(inc_table, tms_gmt+3600, inc_var, grouping)
  i_ans <-  fmatch(tms_gmt, socc$tms_gmt)
  socc <- socc[i_ans, ]
  ans <- socc[, variable] + inc
  ans
}
# st1_incmod <- increment_model(st1, max(st1$tms_gmt, na.rm=TRUE))
# 
predict.increment_model <- function(socc, variable="bikes", grouping=c("wday", "hour")){
  ans <- foreach(tms=socc$tms_gmt, .combine=c) %dopar% {
    ans <- increment_model(socc, tms, variable, grouping)
    print(ans)
    ans
  }
  # browser()
  ans <- as.numeric(ans)
  ans[is.na(ans)] <- socc[is.na(ans), variable]
  #browser()
  if(variable == "bikes") ans <- correct_nb_bikes(round(ans), socc$bikes+socc$free_slots)
  if(variable == "occ") ans <- correct_occ(ans)
  ans
}
# print(system.time({p_incmod <- predict.increment_model(head(st1))}))
# print(rmse(p_incmod, head(st1$bikes_hplus1)))

predict.static_model <- function(socc, variable="bikes", grouping=c("wday", "hour")){
ans <- socc[, variable]
ans
}
# p_stamod <- predict.static_model(st1)
# print(rmse(p_stamod, st1$bikes_hplus1))