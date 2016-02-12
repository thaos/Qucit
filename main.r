source("tools.r")
library(doMC)
library(fastmatch)
registerDoMC(cores=2)


################################################################################
# Benchmarks
source("benchmark_models.r")
st1 <- load_sid(1)

#static
p_stamod <- predict.static_model(st1)
print(rmse(p_stamod, st1$bikes_hplus1))

# increment
# wday + hour
print(system.time({p_incmod <- predict.increment_model(st1)}))
print(rmse(p_incmod, st1$bikes_hplus1))
saveRDS(p_incmod, file="p_incmod.RDS")
# wday + hour + month
print(system.time({p_incmod_month <- predict.increment_model(st1, grouping=c("month", "wday", "hour"))}))
print(rmse(p_incmod_month, st1$bikes_hplus1))
saveRDS(p_incmod_month, file="p_incmod_month.RDS")


################################################################################
# Linear Models
source("linear_models.r")

#lm
st1 <- load_sid(1)
st <- head(st1, n=50000)
p_lmmod <- predict.lmmod(st)
p_lmmod <- correct_nb_bikes(round(p_lmmod), st$bikes+st$free_slots)
rmse(p_lmmod, st$bikes_hplus1)

print(system.time(p_lmmod <- predict.lmmod(st1)))
p_lmmod <- correct_nb_bikes(round(p_lmmod), st1$bikes+st1$free_slots)
rmse(p_lmmod, st1$bikes_hplus1)            
# rmse 2.94 without additionnal stations

st1_m <- readRDS("st1_m.RDS")
print(system.time(p_lmmod <- predict.lmmod(st1_m)))
p_lmmod <- correct_nb_bikes(round(p_lmmod), st1_m$bikes+st1_m$free_slots)
rmse(p_lmmod, st1_m$bikes_hplus1)            
# rmse 2.97


# GAM
st <- head(st1, n=50000)
p_gammod <- predict.gammod(st)
p_gammod <- correct_nb_bikes(round(p_gammod), st$bikes+st$free_slots)
rmse(p_gammod, st$bikes_hplus1)

print(system.time(p_gammod <- predict.gammod(st1)))
p_gammod <- correct_nb_bikes(round(p_gammod), st1$bikes+st1$free_slots)
rmse(p_gammod, st1$bikes_hplus1)    

# dyn
st <- head(st1, n=50000)
p_dynmod <- predict.dynmod(st)
p_dynmod <- correct_nb_bikes(round(p_dynmod), st$bikes+st$free_slots)
rmse(p_dynmod, st$bikes_hplus1)

print(system.time(p_dynmod <- predict.dynmod(st1)))
p_dynmod <- correct_nb_bikes(round(p_dynmod), st1$bikes+st1$free_slots)
rmse(p_dynmod, st1$bikes_hplus1)        

