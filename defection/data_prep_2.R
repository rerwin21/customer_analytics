# get customers needed for modeling -----------------------------------------
.getIndependentCustomers <- function(subscriptions, train = T, ...) {
  
  
  #get optional arguments
  if(length(list(...)$length.dep) != 0) {length.dep <- list(...)$length.dep}
  if(length(list(...)$start.ind) != 0) {start.ind <- list(...)$start.ind} # start.ind
  if(length(list(...)$end.ind) != 0) {end.ind <- list(...)$end.ind} # end.ind
  if(length(list(...)$start.dep) != 0) {start.dep <- list(...)$start.dep} # start.dep
  if(length(list(...)$end.dep) != 0) {end.dep <- list(...)$end.dep} # end.dep
  
  
  if(train == T) {
    
    #Filter down to subscriptions that begin in independent pd. and end in dependent period
    subscriptions <- subscriptions %>%
      mutate(StartDate = as.Date(StartDate, format = "%d/%m/%Y"),
             EndDate = as.Date(EndDate, format = "%d/%m/%Y")) %>% 
      filter(StartDate <= end.ind,
             EndDate <= end.dep,
             EndDate >= start.dep)
    
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


# credit aggregation --------------------------------------------------------
.aggregateCredit <- function(credit, activeCustomers, subscriptions, end.ind, Train, ...) {
  
  #get optional arguments
  if(length(list(...)$cats) != 0) {cats <- list(...)$cats}
  
  #Add CustomerID to table 
  credit <- inner_join(credit, subscriptions %>% select(SubscriptionID, CustomerID), "SubscriptionID")
  
  #Filter credit table
  credit <- credit %>% 
    mutate(ProcessingDate = as.Date(ProcessingDate, format = "%d/%m/%Y")) %>%
    filter(ProcessingDate <= end.ind,
           CustomerID %in% activeCustomers$CustomerID)
  
  #Make credit dummies according to categories
  if(Train == T) {
    cats <- categories(credit)
  } 
  
  credit.dummies <- cbind(SubscriptionID = credit$SubscriptionID, 
                            dummy(credit,
                                  object = cats, 
                                  int = T)
    )
  
  credit <- inner_join(credit %>% select(SubscriptionID, ProcessingDate, Amount, NbrNewspapers),
                       credit.dummies,
                       by = "SubscriptionID")
  names(credit)[which(names(credit) == "NbrNewspapers")] <- "Newspapers_Credited"
  
  #Note: dropping processing date in aggregation
  
  #Calcuate totals of credit table
  numCredits <- aggregate(credit %>% select(numCredits = SubscriptionID), 
                          by = list(credit$SubscriptionID), length)
  credit <- aggregate(credit %>% select(-SubscriptionID, -ProcessingDate), 
                      by = list(credit$SubscriptionID), sum)
  
  #Combine all calculations and clean table
  credit <- inner_join(credit, numCredits, "Group.1")
  names(credit)[1] <- "SubscriptionID"; rm(numCredits)
  credit$SubscriptionID <- as.factor(credit$SubscriptionID)
  
  if(Train == T) {
    return(list(table = credit, cats = cats))
  } else if (Train == F) {
    return(credit)
  }
}


# delivery aggregation ------------------------------------------------------
.aggregateDelivery <- function(delivery, activeCustomers, subscriptions, end.ind, Train, ...) {

  #get optional arguments
  if(length(list(...)$cats) != 0) {cats <- list(...)$cats}
  
  #Add CustomerID to table 
  delivery <- inner_join(delivery, subscriptions %>% select(SubscriptionID, CustomerID), "SubscriptionID")
  
  #Filter delivery table
  delivery <- delivery %>% 
    mutate(StartDate = as.Date(StartDate, format = "%d/%m/%Y"),
      EndDate = as.Date(EndDate, format = "%d/%m/%Y")) %>%
    filter(EndDate <= end.ind,
           CustomerID %in% activeCustomers$CustomerID)
  
  if(Train == T) {
    cats <- categories(delivery)
  } 
  
  delivery.dummies <- cbind(SubscriptionID = delivery$SubscriptionID, 
                              dummy(delivery,
                                    object = cats,
                                    int = T)
    )
  
  delivery <- inner_join(delivery %>% select(SubscriptionID, StartDate, EndDate),
                         delivery.dummies,
                         by = "SubscriptionID")
  
  #Change date types
  delivery[,2:3] <- sapply(delivery[,2:3], as.Date, format="%d/%m/%Y")
  
  #Calculate delivery values
  delivery$DeliveryDuration <- delivery$EndDate - delivery$StartDate
  numDeliveries <- aggregate(delivery %>% select(numDeliveries = SubscriptionID), 
                             by = list(delivery$SubscriptionID), length)
  
  #Mean duration
  delivery.means <- aggregate(delivery %>% select(DeliveryDuration),
                              list(delivery$SubscriptionID),
                              mean)
  names(delivery.means)[-1] <- paste("Avg", names(delivery.means)[-1], sep = "_")
  
  #Sum of dummies and duration
  delivery.sums <- aggregate(delivery %>% select(-SubscriptionID, -StartDate, -EndDate),
                             list(delivery$SubscriptionID),
                             sum)
  names(delivery.sums)[-1] <- paste("Total", names(delivery.sums)[-1], sep = "_")
  
  #Combine all calculations and clean table
  delivery.list <- list(delivery.means, delivery.sums, numDeliveries)
  delivery <-  Reduce(function(x,y) merge(x,y, by = "Group.1"), delivery.list)
  names(delivery)[1] <- "SubscriptionID" 
  delivery$SubscriptionID <- as.factor(delivery$SubscriptionID)
  
  if(Train == T) {
    return(list(table = delivery, cats = cats))
  } else if (Train == F) {
    return(delivery)
  }
}


# complaints aggregation -----------------------------------------------------
.comps_agg <- function(t1, t2, comps, act_cust, train = T, ...) {
  
  
  #get optional arguments
  if(length(list(...)$cats) != 0) {cats <- list(...)$cats}
  
  
  # replace na's
  comps[is.na(comps)] <- ""
  
  
  # convert date
  comps <- comps %>% 
    mutate(
      ComplaintDate = dmy(ComplaintDate),
      ProductID = as.factor(ProductID), 
      ComplaintType = as.factor(ComplaintType),
      SolutionType = as.factor(SolutionType),
      FeedbackType = as.factor(FeedbackType)
    )
  
  
  # leave customers we're interested in and complaints that take place ...
  # ... within the ind_period
  comps <- comps %>% 
    filter(
      CustomerID %in% act_cust$CustomerID,
      as.Date(ComplaintDate) >= t1,
      as.Date(ComplaintDate) <= t2
    )
  
  
  # rename ambiguous column
  comps <- comps %>% 
    rename(complaint_product = ProductID)
  
  
  # create the categories depending on training or deployment
  if(train == T){
    comp_categories <- categories(comps)
  } else {
    comp_categories <- cats
  }
  
  
  # create dummies
  comp_dummy <- dummy(comps,
                      int = T,
                      object = comp_categories)
  
  
  # bind the dummies with the original table
  comps <- bind_cols(comps, comp_dummy)
  
  
  # aggregate complaints
  comps_agg <- comps %>% 
    select(-c(ComplaintID,
              complaint_product,
              ComplaintDate,
              ComplaintType,
              SolutionType,
              FeedbackType)) %>% 
    group_by(CustomerID) %>% 
    summarise_each(funs(sum))
  
  
  # return the aggregated complaints
  if(train == T) {
    return(list(table = comps_agg, cats = comp_categories))
  } else if (train == F) {
    return(comps_agg)
  }
}


# prepare customer table ----------------------------------------------------
.cust_prep <- function(cust, act_cust, t2, train = T, ...){
  
  
  # prepar for optional arguments
  if(length(list(...)$cats) != 0) {cats <- list(...)$cats}
  
  
  # filter only active customers
  cust <- cust %>% 
    filter(CustomerID %in% act_cust$CustomerID)
  
  
  # convert date and factors
  cust <- cust %>% 
    mutate(
      DOB = dmy(DOB),
      Gender = as.factor(Gender),
      District = as.factor(District)
    )
  
  
  # Compute Age
  cust <- cust %>% 
    mutate(
      cust_age = difftime(t2, DOB, units = "days"),
      cust_age = as.numeric(cust_age)/365.25,
      cust_age = ceiling(cust_age)
    )
  
  
  # prepare categories
  if(train == T){
    cust_categories <- categories(cust)
  } else {
    cust_categories <- cats
  }
  
  
  # create dummies
  cust_dummy <- dummy(cust,
                      int = T,
                      object = cust_categories)
  
  
  # bind dummies with customers
  cust <- bind_cols(cust, cust_dummy)
  
  
  # remove the uneccessary columns
  cust <- cust %>% 
    select(-c(Gender,
              DOB,
              District,
              ZIP,
              StreetID))
  
  
  # return the prepared customers
  if(train == T) {
    return(list(table = cust, cats = cust_categories))
  } else if (train == F) {
    return(cust)
  }
}


# create dependent variable ------------------------------------------------
.dep_variable <- function(t2, t3, t4, subs, act_cust){
  
  
  # convert date
  subs <- subs %>% 
    mutate(
      StartDate = dmy(StartDate)
    )
  
  
  # filter out the uneccessary customers
  subs <- subs %>% 
    filter(CustomerID %in% act_cust$CustomerID)
  
  
  # create interval using dependent dates
  dep_interval <- interval(t3, t4)
  
  
  # compute interim response
  subs$churn <- ifelse(subs$StartDate %within% dep_interval, 1, 0)
  
  
  # convert na's to zero
  subs <- subs %>% 
    mutate(
      churn = ifelse(is.na(churn), 0, churn)
    ) %>% 
    arrange(CustomerID)
  
  
  
  
  # summarise to get final response
  subs <- subs %>% 
    group_by(CustomerID) %>% 
    summarise(
      churn = sum(churn),
      churn = ifelse(churn == 0, 1, 0)
    )
  
  
  # return the response variable
  return(subs)
}


# append subscriptions with aggregated credit and deliver, and ... ----------
# ... the forumla table
.append_subs <- function(subs, agg_credit, agg_deliver, formula, active_subs, end.ind) {
  
  
  # filter subs
  subs <- subs %>% 
    mutate(StartDate = as.Date(dmy(StartDate))) %>% 
    filter(CustomerID %in% active_subs$CustomerID,
           StartDate <= end.ind)
  
  
  # convert ids for joining
  # subs
  subs$SubscriptionID <- as.factor(subs$SubscriptionID)
  subs$SubscriptionID <- as.character(subs$SubscriptionID)
  
  
  # agg_credit
  agg_credit$SubscriptionID <- as.character(agg_credit$SubscriptionID)
  
  
  # agg_delivery
  agg_deliver$SubscriptionID <- as.character(agg_deliver$SubscriptionID)
  
  
  # create list of data frames
  dfs <- list(subs, agg_credit, agg_deliver)
  
  
  # join agg_credit and agg_deliver with subscriptions
  sub <- Reduce(function(...) {
    left_join(..., by = "SubscriptionID")
  },
  dfs)
  
  
  # join subscriptions with formula (no need to aggregate formula table)
  sub <- left_join(sub, formula, by = c("FormulaID" = "FormulaID"))
  
  
  # replace na's
  sub[is.na(sub)] <- 0
  
  
  # convert to numeric
  sub[ , which(names(sub) %in% c(names(agg_credit), names(agg_deliver)))] <- 
    sapply(sub[,which(names(sub) %in% c(names(agg_credit), names(agg_deliver)))],
           function(x) as.numeric(x))
  
  
  # return the appended subscriptions table
  return(sub)
}


# subscription aggregation --------------------------------------------------
.subs_agg <- function(subs_appended, train = T, ...) {
  
  
  #get optional arguments
  if(length(list(...)$cats) != 0) {cats <- list(...)$cats}
  
  
  # convert the dates
  subs_appended <- subs_appended %>% 
    mutate(
      EndDate = dmy(EndDate),
      RenewalDate = dmy(RenewalDate),
      PaymentDate = dmy(PaymentDate),
      start_month = as.factor(month(StartDate)),
      end_month = as.factor(month(EndDate)),
      Pattern = as.factor(Pattern),
      ProductID = as.factor(ProductID),
      FormulaID = as.factor(FormulaID)
    )
  
  
  # create the categories depending on training or deployment
  if(train == T){
    sub_categories <- categories(subs_appended)
  } else {
    sub_categories <- cats
  }
  
  
  # create dummies for subscriptions
  sub_dummy <- dummy(subs_appended, 
                     int = T, 
                     object = sub_categories)
  
  
  # bind dummies to subscriptions
  subs_appended <- bind_cols(subs_appended, sub_dummy)
  
  
  # remove subscriptionID
  subs_appended <- subs_appended %>% 
    select(-c(SubscriptionID,
              ProductID,
              Pattern,
              StartDate,
              EndDate,
              RenewalDate,
              PaymentDate,
              PaymentType,
              PaymentStatus,
              FormulaID,
              FormulaCode,
              FormulaType))
  
  # replace na's
  subs_appended[is.na(subs_appended)] <- 0
  
  
  # counts
  subs_count <- subs_appended %>% 
    group_by(CustomerID) %>% 
    summarise(subs_count = n())
  
  
  # averages
  subs_avg <- subs_appended %>% 
    select(-c(start_month, end_month)) %>% 
    group_by(CustomerID) %>% 
    summarise_each(funs(mean))
  
  
  
  # rename
  names(subs_avg)[2:length(names(subs_avg))] <- paste(names(subs_avg)[2:length(names(subs_avg))], 
                                                      "_avg", 
                                                      sep = "")
  
  
  # sums
  subs_sum <- subs_appended %>% 
    select(-c(start_month, end_month)) %>% 
    group_by(CustomerID) %>% 
    summarize_each(funs(sum))
  
  # rename
  names(subs_sum)[2:length(names(subs_sum))] <- paste(names(subs_sum)[2:length(names(subs_sum))], 
                                                      "_sum", 
                                                      sep = "")
  
  
  # create list of aggregated subscriptions
  dfs <- list(subs_count, subs_avg, subs_sum)
  
  
  # join the aggregated data frames together
  subs_agg <- Reduce(function(...) {
    left_join(..., by = "CustomerID")
  },
  dfs)
  
  
  # return the aggregated subscriptions
  if(train == T) {
    return(list(table = subs_agg, cats = sub_categories))
  } else if (train == F) {
    return(subs_agg)
  }
}


# join, aggregated subscriptions and aggregated complaints ... --------------
# ... with the customer table
.basetable_join <- function(cust, sub, comp) {
  
  # form list as an input to the Reduce function below
  dfs <- list(cust, sub, comp)
  
  
  
  # join them together
  basetable <- Reduce(function(...) {
    left_join(..., by = "CustomerID")
  },
  dfs)
  
  
  # remove na's
  basetable[is.na(basetable)] <- 0
  
  
  # sort
  basetable <- basetable %>% 
    arrange(CustomerID)
  
  
  # return the basetable
  return(basetable)
}


# read and prepare data -----------------------------------------------------
.read.and.prepare.data <- function(train = T, ...) {
  
  # read in data 
  # get optional arguments
  if(length(list(...)$start.ind) != 0) {start.ind <- list(...)$start.ind}
  if(length(list(...)$end.ind) != 0) {end.ind <- list(...)$end.ind}
  if(length(list(...)$start.dep) != 0) {start.dep <- list(...)$start.dep}
  if(length(list(...)$end.dep) != 0) {end.dep <- list(...)$end.dep}
  if(length(list(...)$cats) != 0) {cats <- list(...)$cats}
  if(length(list(...)$length.dep) != 0) {length.dep <- list(...)$length.dep}
  
  
  # Subcription credits
  credit <- read.table("credit.txt", 
                       header=T, 
                       sep=";")
  
  
  # customers
  customers <- read.table("customers.txt", 
                          header=T, 
                          sep=";")
  
  
  # delivery records
  delivery <- read.table("delivery.txt", 
                         header=T, 
                         sep=";")
  
  
  # customer complaints
  complaints <- read.table("complaints.txt", 
                           header=T, 
                           sep=";")
  
  
  
  # product formulas (packages)
  formula <- read.table("formula.txt", 
                        header=T, 
                        sep=";")
  
  
  # subscription records
  subscriptions <- read.table("subscriptions.txt", 
                              header=T, 
                              sep=";")
  

  # aggregate data into subscription level  
  if(train == T) {
    
    
    # Make list of categories
    all_categories <- list()
    
    
    # get customers to be modeled 
    active_subs <- .getIndependentCustomers(subscriptions,
                                            start.ind = start.ind,
                                            end.ind = end.ind,
                                            start.dep = start.dep,
                                            end.dep = end.dep)
    
    
    # summarize credit
    agg_credit <- .aggregateCredit(credit,
                                   active_subs,
                                   subscriptions,
                                   end.ind,
                                   Train = train)
    
    
    all_categories$credit <- agg_credit$cats
    agg_credit <- agg_credit$table
    
    
    # summarize deliveries
    agg_deliver <- .aggregateDelivery(delivery,
                                      active_subs,
                                      subscriptions,
                                      end.ind, 
                                      Train = train)
    
    
    all_categories$deliver <- agg_deliver$cats
    agg_deliver <- agg_deliver$table
    
    
    # append the summarized credits and deliveries ...
    # ... and formulas with subscriptions, using active subs ...
    # ... to filter
    append_subs <- .append_subs(subscriptions,
                                agg_credit,
                                agg_deliver,
                                formula,
                                active_subs,
                                end.ind)
    
    
    # summarize appended subs
    agg_subs <- .subs_agg(append_subs, 
                          train = train)
    
    
    all_categories$subs <- agg_subs$cats
    agg_subs <- agg_subs$table
    
    
    # summarize complaints
    agg_comp <- .comps_agg(start.ind,
                           end.ind,
                           complaints,
                           active_subs,
                           train = train)
    
    
    all_categories$comp <- agg_comp$cats
    agg_comp <- agg_comp$table
    
    
    # preparte the customer table 
    cust_prep <- .cust_prep(customers,
                            active_subs,
                            end.ind,
                            train = train)
    
    
    all_categories$cust <- cust_prep$cats
    cust_prep <- cust_prep$table
 
  } else if (train == F) {
    
    
    # pull out the correct categories from the training data
    credit_cat <- cats$credit
    deliver_cat <- cats$deliver
    subs_cat <- cats$subs
    comp_cat <- cats$comp
    cust_cat <- cats$cust
    
    
    # get customers to be modeled 
    active_subs <- .getIndependentCustomers(subscriptions,
                                            end.ind = end.ind,
                                            length.dep = length.dep,
                                            train = train)
    
    
    
    # summarize credit
    agg_credit <- .aggregateCredit(credit,
                                   active_subs,
                                   subscriptions,
                                   end.ind,
                                   Train = train,
                                   cats = credit_cat)
    
    
    # summarize deliveries
    agg_deliver <- .aggregateDelivery(delivery,
                                      active_subs,
                                      subscriptions,
                                      end.ind, 
                                      Train = train,
                                      cats = deliver_cat)
    
    
    # append the summarized credits and deliveries ...
    # ... and formulas with subscriptions, using active subs ...
    # ... to filter
    append_subs <- .append_subs(subscriptions,
                                agg_credit,
                                agg_deliver,
                                formula,
                                active_subs,
                                end.ind)
    
    
    # summarize appended subs
    agg_subs <- .subs_agg(append_subs, 
                          train = train,
                          cats = subs_cat)
    
    
    # summarize complaints
    agg_comp <- .comps_agg(start.ind,
                           end.ind,
                           complaints,
                           active_subs,
                           train = train,
                           cats = comp_cat)
    
    
    # preparte the customer table 
    cust_prep <- .cust_prep(customers,
                            active_subs,
                            end.ind,
                            train = train,
                            cats = cust_cat)
  }
  

  # compute dependent variable
  if(train==T) {
    
    # Dependent Var calculation
    Churn <- .dep_variable(end.ind,
                           start.dep,
                           end.dep,
                           subscriptions,
                           active_subs)
  }
  
  
  # aggregate data into customer level  
  Basetable <- .basetable_join(cust_prep,
                               agg_subs, 
                               agg_comp)
  
  
  # return statements  
  if(train ==T ) {
    return(list(basetable = Basetable, 
                categories = all_categories, 
                response = Churn$churn))
  } else {
    return(list(basetable = Basetable))
  }
}


# Build classification model  --------------------------------------------------------------
defectionModel <- function(start.ind, end.ind, start.dep, end.dep, 
                           evaluate = T, verbose = T) {
  
  # Load packages
  if (require('randomForest')==FALSE) install.packages('randomForest');require('randomForest')
  if (require('AUC')==FALSE) install.packages('AUC');require('AUC')
  
  # Call read and prepare data to get basetable 
  dataprep <- .read.and.prepare.data(train = T, 
                                     start.ind = start.ind, 
                                     end.ind = end.ind, 
                                     start.dep = start.dep, 
                                     end.dep = end.dep)
  
  
  # pull basetable from dataprep
  Basetable <- dataprep$basetable
  
  
  # pull out the response variable from dataprep
  response <- dataprep$response
  
  
  # Evaluate Performace of a Random Forest Model 
  if(evaluate == TRUE) {
    #get training indices
    train <- sample(nrow(Basetable), .7*nrow(Basetable))
    test <- c(1:nrow(Basetable))[-train]
    
    # split Basetable into Train and Test portions
    X_Train <- Basetable[train, -which(names(Basetable) %in% c("CustomerID"))]
    Y_Train <- as.factor(response[train])
    X_Test <- Basetable[test, -which(names(Basetable) %in% c("CustomerID"))]
    Y_Test <- as.factor(response[test])
    
    # Train randomForest model
    RFmodel <- randomForest(X_Train, Y_Train, ntree = 500, importance = TRUE)
    
    
    # Plot Learning Curve
    plot(RFmodel)
    
    
    # Plot Variable Importances
    varImpPlot(RFmodel, n = 10)
    
    
    # Print AUC
    # must predict with type='prob' (want scores)
    pred <- predict(RFmodel, newdata = X_Test, type = 'prob')[,2]
    auc <- AUC::auc(roc(pred, Y_Test))
    cat('AUC is: ', auc, '\n')
    
    
    # Plot ROC Curve
    plot(roc(pred, Y_Test), main = "Random Forest ROC")
    #print confusion matrix (only if predictions are labels)
    # table(Y_Test,pred,dnn = list("Actual","Predicted"))
    
    
    # Plot Lift Curve
    if (require('lift')==FALSE) install.packages('lift',repos='http://cran.rstudio.com',quiet=TRUE); require('lift')
    plotLift(pred,Y_Test)
    
    
    # Print Top Decile Lift
    cat('top decile lift is: ', TopDecileLift(pred, Y_Test), '\n')
    
  }
  
  X <- Basetable[ , -which(names(Basetable) %in% c("CustomerID"))]
  Y <- as.factor(response)
  RFmodel <- randomForest(X, Y, ntree = 500, importance = TRUE)
  
  # dispatch output as 'acquisition' for predict.acquisition()
  out <- list(length = end.ind - start.ind,
              length.dep = end.dep - start.dep,
              categories = dataprep[[2]], 
              model=RFmodel,
              Basetable = Basetable)
  
  
  # change class for method dispatching
  class(out) <- 'defection'
  
  
  # return object
  return(out)
}


# Predict function ----------------------------------------------------------
predict.defection <- function(object, dump.date) {
  # Load packages 
  if (require('randomForest')==FALSE) install.packages('randomForest');require('randomForest')
  if (require('AUC')==FALSE) install.packages('AUC');require('AUC')
  
  # Call read and prepare data to get basetable 
  dataprep <- .read.and.prepare.data(train = F,
                                     start.ind = dump.date-object$length,
                                     end.ind = dump.date,
                                     cats = object$categories,
                                     length.dep = object$length.dep)
  
  X <- dataprep$basetable[, -which(names(dataprep$basetable) %in% c("CustomerID"))]
  predictions <- predict(object$model, newdata = X, type = "prob")
  
  return(data.frame(CustomerID=dataprep$basetable$CustomerID, Prediction=predictions))
}