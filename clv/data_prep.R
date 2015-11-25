# packages ------------------------------------------------------------------
# get which packages do I need
.load_packages <- function(){
  
  # not sure if I need it yet, but if I do, plyr has to be loaded first
  if(require(plyr) == F){install.packages("plyr", quiet = T)
    library(plyr, quietly = T)}
  
  
  # for data frame joining and aggregation
  if(require(dplyr) == F){install.packages("dplyr", quiet = T)
    library(dplyr, quietly = T)}
  
  
  # string processing
  if(require(stringr) == F){install.packages("stringr", quiet = T)
    library(stringr, quietly = T)}
  
  
  # date processing
  if(require(lubridate) == F){install.packages("lubridate", quiet = T)
    library(lubridate, quietly = T)}
  
  
  # create dummies
  if(require(dummy) == F){install.packages("dummy", quiet = T)
    library(dummy, quietly = T)}
  
  
  # model Evaluation
  if(require(AUC) == F){install.packages("AUC", quiet = T)
    library(AUC, quietly = T)}
  
  
  # random forest package
  if(require(randomForest) == F){install.packages("randomForest", quiet = T)
    library(randomForest, quietly = T)}
}


# quitely load packages
.clv_packages <- function(){
  suppressMessages(
    suppressWarnings(
      .load_packages()
    )
  )
}


# data processing helper functions ------------------------------------------
# get active customers
.get_active_cust <- function(trans, ...){
  
  # get optional arguments
  if(length(list(...)$end.ind) != 0) {end.ind <- list(...)$end.ind}
  
  
  # convert transaction date to date/time object
  trans$date <- ymd(trans$date)
  
  
  # filter down to customers that have had a purchase in the last 90 days
  active_cust <- trans %>% 
    filter(as.Date(date) >= (end.ind - 90),
           as.Date(date) <= end.ind) %>% 
    select(custid) %>% 
    distinct() %>% 
    .[[1]]
  
  
  # return the active customers and subscriptions
  return(active_cust)
}


# join store and transactions
.join_trans_store <- function(trans, stores){
  
  # join the to tables together
  trans_store <- left_join(trans, stores, "storeid")
  
  
  # return the joined tables
  return(trans_store)
}