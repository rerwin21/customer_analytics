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
  
  
  # lift chart
  if(require(lift) == F){install.packages("lift",repos='http://cran.rstudio.com',quiet=TRUE) 
    library(lift)}
  
  
  # miscTools for rSquared function
  if(require(miscTools) == F){install.packages("miscTools", quiet = T)
    library(miscTools, quietly = T)}
  
  
  # ggplot for some plotting
  if(require(ggplot2) == F){install.packages("ggplot2", quiet = T)
    library(ggplot2, quietly = T)}
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
.get_active_cust <- function(trans, active_window, ...){
  
  # get optional arguments
  if(length(list(...)$end.ind) != 0) {end.ind <- list(...)$end.ind}
  
  
  # convert transaction date to date/time object
  trans$date <- ymd(trans$date)
  
  
  # filter down to customers that have had a purchase in the last 90 days
  active_cust <- trans %>% 
    filter(as.Date(date) >= (end.ind - active_window),
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
.join_td_products <- function(trans_details, products, 
                              trans, independent = T,
                              ...){
  
  # get optional arguments
  if(length(list(...)$start.ind) != 0) {start.ind <- list(...)$start.ind}
  if(length(list(...)$end.ind) != 0) {end.ind <- list(...)$end.ind}
  if(length(list(...)$active_cust) != 0) {active_cust <- list(...)$active_cust}
  
  # filter transactions to get the active customers
  if(independent) {
    
    # for the independent period
    receipts <- trans %>% 
      mutate(date = as.Date(ymd(date))) %>% 
      filter(custid %in% active_cust,
             date >= start.ind,
             date <= end.ind) %>% 
      select(receiptnbr) %>% 
      .[[1]]
  } else {
    
    # for the CLV calculation
    receipts <- trans %>% 
      select(receiptnbr) %>% 
      .[[1]]
  }
  
  
  # join the tables
  td_products <- left_join(trans_details, products, "SKU")
  
  
  # create total price and cost
  td_products <- td_products %>% 
    mutate(
      price = price * quantity,
      cost = cost * quantity,
      profit = price - cost,
      prod_family = str_extract(family, "\\d{1,2}"),
      prod_category = str_extract(family, "\\d{2}$")
    ) %>% 
    select(-family) %>% 
    filter(receiptnbr %in% receipts)
  
  
  # return the td's with products
  return(td_products)
}


# summarize and compute td_products
.agg_td_products <- function(td_products, train = T, ...){
  
  # get optional arguments
  if(length(list(...)$cats) != 0) {cats <- list(...)$cats}
  
  
  # remove SKU, don't need it here. Only use was to join with...
  # ... with the products table
  td_products <- td_products %>% 
    select(-SKU)
  
  
  # remove na's
  td_products[is.na(td_products)] <- 0
  
  
  # create cats based 
  if(train == T){cats <- categories(td_products)}
  
  
  # create dummies
  td_prods_dummy <- dummy(td_products,
                          int = T, 
                          object = cats)
  
  
  # bind the dummies with original table
  td_products <- bind_cols(td_products, td_prods_dummy)
  
  
  # summarize the columns and dummies
  td_products <- td_products %>% 
    select(-c(prod_family, prod_category)) %>% 
    group_by(receiptnbr) %>% 
    summarise_each(funs(sum))
  
  
  # return the aggregated td_products
  if(train) {
    return(list(table = td_products, cats = cats))
  } else {
    return(td_products)
  }
}


# summarize and aggregate the active customers in trans_store
.agg_trans_store <- function(trans_store, active_cust, agg_td_products, 
                             start.ind, end.ind, train = T, ...){
  
  # get optional arguments
  if(length(list(...)$cats) != 0) {cats <- list(...)$cats}
  
  
  # quick function to test whether a purhcase has taken place ...
  # ... with a give window: e.g. last 30 days, 60 days, etc.
  time_window <- function(date, end.ind, days_since){
    
    # create the interval object
    interval_window <- interval(end.ind - days_since, end.ind)
    within_window <- date %within% interval_window # is date in the interval
    purchase_within <- ifelse(within_window, 1, 0) # if so, give it a 1
    return(purchase_within) # return yes(1) or no(0)
  }
  
  
  # similar function but for the purchase amount not binary
  dollar_window <- function(date, end.ind, days_since, total){
    
    # create the interval object
    interval_window <- interval(end.ind - days_since, end.ind)
    within_window <- date %within% interval_window # is date in the interval
    total_within <- ifelse(within_window, total, 0) # T, return dollar amt
    return(total_within) 
  }
  
  
  # convert factors and date then filter, then create trips per window
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
    ) %>% 
    mutate(
      last_7_trip = time_window(date, end.ind, 7),
      last_30_trip = time_window(date, end.ind, 30),
      last_60_trip = time_window(date, end.ind, 60),
      last_90_trip = time_window(date, end.ind, 90),
      last_7_total = dollar_window(date, end.ind, 7, total),
      last_30_total = dollar_window(date, end.ind, 30, total),
      last_60_total = dollar_window(date, end.ind, 60, total),
      last_90_total = dollar_window(date, end.ind, 90, total),
      month = month.abb[month(date)]
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
  
  
  # get amount of profit in recent windows
  trans_store <- trans_store %>% 
    mutate(
      last_7_profit = dollar_window(date, end.ind, 7, profit),
      last_30_profit = dollar_window(date, end.ind, 30, profit),
      last_60_profit = dollar_window(date, end.ind, 60, profit),
      last_90_profit = dollar_window(date, end.ind, 90, profit)
    )
  
  
  # get the count of trips and store, LOR, and recency
  trans_store_count <- trans_store %>% 
    group_by(custid) %>% 
    summarise(
      n_trips = n(),
      n_stores = n_distinct(storeid),
      LOR = max(date) - min(date),
      recency = end.ind - max(date)
    )
  
  
  # aggregate sums
  trans_store_sum <- trans_store %>% 
    select(-c(receiptnbr, date, storeid, ZIP, month)) %>% 
    group_by(custid) %>% 
    summarise_each(funs(sum))
  
  
  # join count and sums
  agg_trans_store <- left_join(trans_store_count, 
                               trans_store_sum,
                               "custid")
  
  
  # return the aggregated transactions and details
  if(train) {
    return(list(table = agg_trans_store, cats = cats))
  } else {
    return(agg_trans_store)
  }
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
  
  
  # remove NA's
  basetable[is.na(basetable)] <- 0
  
  
  # return the basetable 
  if(train) {
    return(list(table = basetable, cats = cats))
  } else {
    return(basetable)
  }
}


# create dependent variable: clv
.create_clv <- function(trans, active_cust, disc_rate,
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
  profit_item <- .join_td_products(trans_details, 
                                   products,
                                   trans,
                                   independent = F)
  
  
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


# wrapper function for helpers: read and prepare data -----------------------
.read.and.prepare.data <- function(train = T, disc_rate, active_window, ...){
  
  # get optional arguments: - - - - - - - - - - - - - - - - - - - - - - - - -
  # dates and categories
  if(length(list(...)$start.ind) != 0) {start.ind <- list(...)$start.ind}
  if(length(list(...)$end.ind) != 0) {end.ind <- list(...)$end.ind}
  if(length(list(...)$start.dep) != 0) {start.dep <- list(...)$start.dep}
  if(length(list(...)$end.dep) != 0) {end.dep <- list(...)$end.dep}
  if(length(list(...)$cats) != 0) {cats <- list(...)$cats}
  
  
  # read in data  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  customers <- read.csv("customers.csv", 
                        stringsAsFactors = F)
  
  
  stores <- read.csv("stores.csv",
                     stringsAsFactors = F)
  
  
  trans <- read.csv("transactions.csv",
                    stringsAsFactors = F)
  
  
  # products and trans_details have problems with ...
  # .. leading zeros (in SKU) and need to be correctly loaded
  # need to see how many columns
  products <- read.csv("products.csv", 
                       stringsAsFactors = F,
                       nrows = 10)
  
  
  products <- read.csv("products.csv", 
                       stringsAsFactors = F,
                       colClasses = rep("character", 
                                        ncol(products)))
  
  
  # columns to change
  num_cols <- c("family", "price", "cost")
  
  
  # change the columns to numeric for product
  products[ , num_cols] <- suppressWarnings(
    sapply(products [ , num_cols], as.numeric)
  ) 
  
  
  trans_details <- read.csv("transactiondetails.csv",
                            stringsAsFactors = F,
                            nrows = 10)
  
  
  # td defaults and change SKU
  td_class <- sapply(trans_details, class)
  td_class["SKU"] <- "character"
  
  
  trans_details <- read.csv("transactiondetails.csv",
                            stringsAsFactors = F,
                            colClasses = td_class)
  
  
  # summarize and compute tables then join where each row ...  - - - - - - - 
  # ... is a customer
  
  
  # get the active customers 
  active_cust <- .get_active_cust(trans = trans,
                                  active_window = active_window,
                                  end.ind = end.ind)
  
  
  # join transactions with store information
  trans_store <- .join_trans_store(trans = trans, 
                                   stores = stores)
  
  
  # join td's with products
  td_products <- .join_td_products(trans_details = trans_details,
                                   products = products,
                                   trans = trans,
                                   active_cust = active_cust,
                                   start.ind = start.ind,
                                   end.ind = end.ind)
  
  
  # the following tables depend on whether we're building training or not ...
  # ... (active_cust does too, but I deal with that by passing the correct date ...
  # ... ahead of time)
  if(train) {
    
    # Make list of categories
    all_categories <- list()
    
    
    # compute and summmarize td's & product information: return ...
    # ... table and categories for dummies
    agg_td_prods <- .agg_td_products(td_products = td_products)
    
    
    # get categories and table for aggregated transactions...
    # ...details
    all_categories$agg_td_prods <- agg_td_prods$cats
    agg_td_prods <- agg_td_prods$table
    
    
    # compute and summarize transaction & store information: return ...
    # ... table and categories for dummies
    agg_trans_store <- .agg_trans_store(trans_store = trans_store,
                                        active_cust = active_cust,
                                        agg_td_products = agg_td_prods,
                                        start.ind = start.ind,
                                        end.ind = end.ind)
    
    
    # get table and categories for aggregated transactions
    all_categories$agg_trans_store <- agg_trans_store$cats
    agg_trans_store <- agg_trans_store$table
    
    
    # join customer info with aggregated purchase history ...
    # ... at the customer level: return table and dummy categories
    basetable <- .create_basetable(customers = customers,
                                   active_cust = active_cust,
                                   agg_trans_store = agg_trans_store,
                                   end.ind = end.ind)
    
    
    # get table and categories for basetable
    all_categories$basetable <- basetable$cats
    basetable <- basetable$table
    
    
    # create
    clv <- .create_clv(trans = trans, 
                       active_cust = active_cust, 
                       disc_rate = disc_rate,
                       trans_details = trans_details, 
                       products = products,
                       start.dep = start.dep,
                       end.dep = end.dep)
    
    
  } else {
    
    # pull out the correct categories from the training data
    cat_td_product <- cats$agg_td_prods
    cat_agg_trans_store <- cats$agg_trans_store
    cat_basetable <- cats$basetable
    
    
    # compute and summmarize td's & product information
    agg_td_prods <- .agg_td_products(td_products = td_products,
                                     train = train,
                                     cats = cat_td_product)
    
    
    # compute and summarize transaction & store information
    agg_trans_store <- .agg_trans_store(trans_store = trans_store,
                                        active_cust = active_cust,
                                        agg_td_products = agg_td_prods,
                                        start.ind = start.ind,
                                        end.ind = end.ind,
                                        train = train,
                                        cats = cat_agg_trans_store)
    
    
    # join customer info with aggregated purchase history ...
    # ... at the customer level
    basetable <- .create_basetable(customers = customers,
                                   active_cust = active_cust,
                                   agg_trans_store = agg_trans_store,
                                   end.ind = end.ind,
                                   train = train,
                                   cats = cat_basetable)
  }
  
  
  # if training, return the basetable, categories for dummies, and ...
  # ... the response variable
  if(train) {
    return(list(basetable = basetable, 
                categories = all_categories, 
                response = clv$clv,
                active_window = active_window,
                clv = clv))
  } else {
    return(list(basetable = basetable))
  }
}


# build model ---------------------------------------------------------------
clv_model <- function(start.ind, end.ind, start.dep, end.dep, 
                      evaluate = T, verbose = T, ...) {
  
  # optional arguments - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # discount rate for clv calculation
  if(length(list(...)$disc_rate) != 0) {
    disc_rate <- list(...)$disc_rate
  } else {
    disc_rate <- 0.04
  }
  
  # window for active customers (days): default value of 90 means ...
  # ... must have purchase within last 90 days to be an active customer
  if(length(list(...)$active_window) != 0) {
    active_window <- list(...)$active_window
  } else {
    active_window <- 90
  }
  
  
  # call read and prepare data to get basetable - - - - - - - - - - - - - - -
  dataprep <- .read.and.prepare.data(train = T, 
                                     start.ind = start.ind, 
                                     end.ind = end.ind, 
                                     start.dep = start.dep, 
                                     end.dep = end.dep,
                                     disc_rate = disc_rate,
                                     active_window = active_window)
  
  
  # pull basetable from dataprep
  basetable <- dataprep$basetable
  
  
  # pull out the response variable from dataprep
  response <- dataprep$response
  
  
  # evaluate the performance of random forest model
  if(evaluate) {
    
    #get training indices
    train <- sample(nrow(basetable), .7*nrow(basetable))
    test <- c(1:nrow(basetable))[-train]
    
    
    # split basetable into train and test portions
    X_Train <- basetable[train, -which(names(basetable) %in% c("custid"))]
    Y_Train <- response[train]
    X_Test <- basetable[test, -which(names(basetable) %in% c("custid"))]
    Y_Test <- response[test]
    
    
    # train random forest model
    RFmodel <- randomForest(X_Train, Y_Train, ntree = 1000, importance = TRUE)
    
    
    # plot the learning curve
    plot(RFmodel)
    
    
    # variable importance plots
    varImpPlot(RFmodel, n = 10)
    
    
    # get predictions on test set & training set
    pred <- predict(RFmodel, X_Test)
    pred_train <- predict(RFmodel, X_Train)
    
    
    # actual vs predicted
    a_p <- data.frame(actual = Y_Test, pred = pred, n_trips = X_Test$n_trips)
    
    
    # evaluation of regression
    r2 <- rSquared(Y_Test, Y_Test - pred)
    r2_check <- sum((Y_Test - pred)^2)/sum((Y_Test - mean(Y_Test))^2)
    r2_check <- 1 - r2_check
    cat("R-squared for test set is: ", r2, "\n")
    cat("R-squared test set check: ", r2_check, "\n")
    
    
    # get R2 for training set as well
    r2_train <- rSquared(Y_Train, Y_Train - pred_train)
    cat("R-squared for training set is: ", r2_train)
    
    
    # plot actual versus predicted
    p <- ggplot(a_p, aes(x = actual, y = pred)) + 
      geom_point(aes(size = n_trips), alpha = 0.6) +
      geom_abline(color = "darkred") + 
      ggtitle(paste("RandomForest Regression in R r^2=", round(r2, 2), sep="")) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "bisque"),
            axis.line = element_blank())
    
    
    # print plot
    print(p)
    
    
    # plot the partial dependence plot: pick a few
    # number of newspapers
    partialPlot(x = RFmodel,
                x.var = "LOR",
                pred.data = as.data.frame(X_Test))
    
    
    # Gross Formula Price
    partialPlot(x = RFmodel,
                x.var = "last_90_total",
                pred.data = as.data.frame(X_Test))
  }
  
  
  #   # train big model
  #   X <- basetable[ , -which(names(basetable) %in% c("custid"))]
  #   Y <- response
  #   RFmodel <- randomForest(X, Y, ntree = 500, importance = TRUE)
  
  
  # dispatch output as 'defection' for predict.defection()
  out <- list(length = end.ind - start.ind,
              length.dep = end.dep - start.dep,
              categories = dataprep$categories, 
              model = RFmodel,
              basetable = basetable,
              active_window = dataprep$active_window,
              act_pred = a_p,
              r2 = r2,
              clv = dataprep$clv)
  
  
  # change class for method dispatching
  class(out) <- "clv"
  
  
  # return object
  return(out)
}


# Predict function ----------------------------------------------------------
predict.clv <- function(object, dump.date) {
  
  # Call read and prepare data to get basetable 
  dataprep <- .read.and.prepare.data(train = F,
                                     start.ind = dump.date-object$length,
                                     end.ind = dump.date,
                                     cats = object$categories,
                                     active_window = object$active_window)
  
  
  # create predictors and predictions
  X <- dataprep$basetable[, -which(names(dataprep$basetable) %in% c("custid"))]
  predictions <- predict(object$model, newdata = X)
  
  
  # return the predictions
  return(data.frame(CustomerID=dataprep$basetable$custid, Prediction=predictions))
}