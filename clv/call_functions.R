# build CLV model
# clear environment
rm(list = ls())
gc()
cat("\014")  


# source functions and load packages
setwd("~/c_a")
source("data_prep.R")
.clv_packages()


# define dates
ind_start <- as.Date("2012-09-27")
ind_end <- as.Date("2013-06-01")
dep_start <- as.Date("2013-06-04")
dep_end <- as.Date("2014-02-01")
date_dump <- as.Date("2014-02-02")


# where is the data
setwd("~/c_a_data")


# train model
clv_mod <- clv_model(start.ind = ind_start, 
                     end.ind = ind_end, 
                     start.dep = dep_start, 
                     end.dep = dep_end,
                     disc_rate = 0.04,
                     active_window = 90)


# deploy model
clv_pred <- predict(clv_mod, dump.date = date_dump)
