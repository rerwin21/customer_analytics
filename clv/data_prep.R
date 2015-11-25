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


# join products and trans_details
.join_td_products <- function(trans_details, products){
  
  # join the tables
  td_products <- left_join(trans_details, products, "SKU")
  
  
  # create total price and cost
  td_products <- td_products %>% 
    mutate(
      price = price * quantity,
      cost = cost * quantity,
      profit = price - cost
    )
  
  
  # return the td's with products
  return(td_products)
}


# summarize and compute td_products
.agg_td_products <- function(td_products, train = T, ...){
  
  # get optional arguments
  if(length(list(...)$cats) != 0) {cats <- list(...)$cats}
  
  
  # create cats based 
  if(train == T){
    cats <- categories(td_products)
  }
  
  
  # create dummies
  td_prods_dummy <- dummy(td_products,
                          int = T,
                          object = cats)
  
  
  # bind the dummies with original table
  td_products <- bind_cols(td_products, td_prods_dummy)
  
  
  # summarize the columns and dummies
  td_products <- td_products %>% 
    select(-c(SKU, family)) %>% 
    group_by(receiptnbr) %>% 
    summarise_each(funs(sum))
}


# summarize and aggregate the active customers in trans_store
.agg_trans_store <- function(trans_store, active_cust, agg_td_products, 
                             start.ind, end.ind, train = T, ...){
  
  # get optional arguments
  if(length(list(...)$cats) != 0) {cats <- list(...)$cats}
  
  
  # convert factors and date then filter
  trans_store <- trans_store %>% 
    mutate(
      date = as.Date(ymd(date)),
      storeid = as.factor(storeid),
      ZIP = as.factor(ZIP)
    ) %>% 
    filter(
      custid %in% active_cust,
      date >= start.ind,
      date <= end.ind
    )
  
  
  # training categories or deployment?
  if(train == T){cats <- categories(trans_store)}
  
  
  # create dummies
  trans_store_dummy <- dummy(trans_store, 
                             int = T,
                             object = cats)
  
  
  # bind dummies with original table
  trans_store <- bind_cols(trans_store, trans_store_dummy)
  
  
  # join product details from aggregated transactions details
  trans_store <- left_join(trans_store, agg_td_products, "receiptnbr")
  
  
  # get the count of stores
  trans_store_count <- trans_store %>% 
    group_by(custid) %>% 
    summarise(
      n_trips = n()
    )
  
  
  # aggregate sums
  trans_store_sum <- trans_store %>% 
    select(-c(receiptnbr, date, storeid, ZIP)) %>% 
    group_by(custid) %>% 
    summarise_each(funs(sum))
  
  
  # join count and sums
  agg_trans_store <- left_join(trans_store_count, 
                               trans_store_sum,
                               "custid")
  
  
  # return the aggregated transactions and details
  return(agg_trans_store)
}


# update active customers and join aggregated trans_store information
.create_basetable <- function(customers, active_cust, agg_trans_store,
                              train = T, end.ind, ...){
  
  # get optional arguments
  if(length(list(...)$cats) != 0) {cats <- list(...)$cats}
  
  
  # filter down to active customers and convert date to create age variable
  act_cust <- customers %>% 
    filter(custid %in% active_cust) %>% 
    mutate(
      dob = as.Date(ymd(dob)),
      age = (end.ind - dob)/365.25,
      age = ceiling(age)
    )
  
  
  # training categories or deployment
  if(train == T) {cats <- categories(act_cust)}
  
  
  # create dummies for customer (just in case)
  cust_dummy <- dummy(act_cust, int = T, object = cats)
  
  
  # bind the dummies with original table of active customers
  act_cust <- bind_cols(act_cust, cust_dummy)
  
  
  # join active customers with purchasing history
  basetable <- left_join(act_cust, 
                         agg_trans_store,
                         "custid")
  
  
  # remove unnecessary columns
  basetable <- basetable %>% 
    select(-c(rent, garden, gender, dob)) %>% 
    arrange(custid)
  
 
  # return the basetable 
  return(basetable)
}


# create dependent variable: clv
.create_clv <- function(trans, active_cust, disc_rate = 0.04,
                       trans_details, products, 
                       start.dep, end.dep,
                       train = T){
  
  # convert date
  trans$date <- as.Date(ymd(trans$date))
  
  
  # filter trans for active customers only
  trans <- trans %>% 
    filter(custid %in% active_cust,
           date >= start.dep,
           date <= end.dep)
  
  
  # get profit per item
  profit_item <- .join_td_products(trans_details, products)
  
  
  # summarize profit per item to transaction level
  profit_trans <- profit_item %>% 
    group_by(receiptnbr) %>% 
    summarise(
      profit = sum(profit)
    )
  
  
  # join profit with transactions
  trans <- left_join(trans, profit_trans, "receiptnbr")
  
  
  # function for clv for each purchase
  clv <- function(disc_rate, profit, start.dep, date) {
    
    # get discount factor then calculate clv contribution for that purchase
    dp_do <- as.numeric(date - start.dep)
    disc_factor <- (1 + disc_rate)^(dp_do/365)
    clv_p <- profit/disc_factor
    
    # clv contribution for this customer
    return(clv_p)
  }
  
  
  # calculate the clv for each purchase customer
  trans_clv <- trans %>% 
    mutate(
      clv = clv(disc_rate, profit, start.dep, date),
      clv = round(clv, 2)
    ) %>% 
    group_by(custid) %>% 
    summarise(
      clv = sum(clv)
    ) %>% 
    ungroup() %>% 
    arrange(custid)
  
  
  # get full list (those w/ and w/o purchases in dep)
  response <- data.frame(custid = active_cust) %>% 
    left_join(trans_clv, "custid") %>% 
    mutate(
      clv = ifelse(!is.na(clv), clv, 0)
    ) %>% 
    arrange(custid)
  
  
  # return the response variable
  return(response)  
}