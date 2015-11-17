rm(list=ls())
# define dates --------------------------------------------------------------
ind_start <- as.Date("2006-01-01")
ind_end <- as.Date("2009-12-31")
dep_start <- as.Date("2010-01-01")
dep_end <- as.Date("2010-12-31")

# load the required packages ------------------------------------------------
if(require(plyr) == F) {install.packages("plyr")
  library(plyr)}


if(require(dplyr) == F) {install.packages("dplyr")
  library(dplyr)}


if(require(stringr) == F) {install.packages("stringr")
  library(stringr)}


if(require(lubridate) == F) {install.packages("lubridate")
  library(lubridate)}


if(require(dummy) == F) {install.packages("dummy")
  library(dummy)}

# set working directory -----------------------------------------------------
# Erwin wd
setwd("C:/Users/Ryan/Google Drive/MSBA/Fall 2015/customer_analytics/2nd project")
source("data_prep_2.R")


# test <- .read.and.prepare.data(train = T)
# 
# 
# #Temporary read and prepare testing
# testTrain <- .read.and.prepare.data()
# testDeploy <- .read.and.prepare.data(train = F, cats = testTrain$categories)


#Build Model-----------------------------------------------------------------
object <- defectionModel(start.ind = ind_start, 
                         end.ind = ind_end, 
                         start.dep = dep_start, 
                         end.dep = dep_end)


#Predict From Dump.Date------------------------------------------------------
dump.date <- as.Date("2010-12-31")
pred <- predict(object = object, dump.date = dump.date)
