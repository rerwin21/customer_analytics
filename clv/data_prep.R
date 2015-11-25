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
.get_active_cust <- function(trans, train = T, ...){
  
  # get optional arguments
  if(length(list(...)$length.dep) != 0) {length.dep <- list(...)$length.dep}
  if(length(list(...)$start.ind) != 0) {start.ind <- list(...)$start.ind}
  if(length(list(...)$end.ind) != 0) {end.ind <- list(...)$end.ind}
  if(length(list(...)$start.dep) != 0) {start.dep <- list(...)$start.dep}
  if(length(list(...)$end.dep) != 0) {end.dep <- list(...)$end.dep}
  
  
  # convert transaction date to date/time object
  trans$date <- ymd(trans$date)
   
  
  if(train == T) {
    
    # filter down to customers that have had a purchase in the last 90 days
    trans <- trans %>% 
      filter(as.Date(date) >= (end.ind - 90),
             as.Date(date) <= end.ind) %>% 
      select(custid) %>% 
      distinct()
    
  } else {
    
    #Filter down to subscriptions that begin in independent pd. and end in dependent period
    subscriptions <- subscriptions %>%
      mutate(StartDate = as.Date(StartDate, format = "%d/%m/%Y"),
             EndDate = as.Date(EndDate, format = "%d/%m/%Y")) %>% 
      filter(StartDate <= end.ind,
             EndDate <= length.dep + end.ind,
             EndDate >= end.ind)
    
  }
  
  customers <- subscriptions %>% 
    select(CustomerID, SubscriptionID)
  
  # return the active customers and subscriptions
  return(customers)
}