
# load packages -------------------------------------------------------------
library(plyr)
library(dplyr)
library(stringr)
library(lubridate)
library(dummy)



basetable_deploy <- function(start_ind, end_ind, start_dep, end_dep) {
  
  # convert dates
  start_dep <- as.POSIXct(start_dep)
  end_dep <- as.POSIXct(end_dep)
  

  # Read in data --------------------------------------------------------------
  # setwd: where is the data?
  setwd("C:/Users/Ryan/Google Drive/MSBA/Fall 2015/customer_analytics/1st_project")
  
  
  # read in customers
  customers <- read.csv("customers.csv", stringsAsFactors = F)
  
  
  # read in the purchases table
  purchases <- read.csv("purchases.csv", stringsAsFactors = F)
  
  
  # read in registrations
  registrations <- read.csv("registrations.csv", stringsAsFactors = F)
  
  
  # Convert date columns ------------------------------------------------------
  # purchase date
  purchases$PurchaseDate <- ymd(purchases$PurchaseDate)
  
  
  # registration date
  registrations$RegistrationDate <- ymd(registrations$RegistrationDate)
  
  
  # filter dates
  registrations <- registrations %>% 
    filter(RegistrationDate >= start_ind, RegistrationDate <= end_ind)
  
  
  # create quick function
  date_parse <- function(x, type){
    
    funs <- list(
      year,
      month,
      day,
      quarter
    )
    
    names(funs) <- paste(type, c("_year", "_month", "_day", "_quarter"), sep = "")
    
    df <- sapply(funs, function(f, y) f(y), y = x) %>% 
      t() %>% 
      as.data.frame()
    
    return(df)
  }
  
  
  # pull out year, month, day, and quarter for purchases table
  purchases <- ldply(purchases$PurchaseDate, 
                     function(x, type) date_parse(x, type), type = "pur") %>% 
    bind_cols(purchases, .)
  
  
  # pull out year, month, day, and quarter for registrations table
  registrations <- ldply(registrations$RegistrationDate, 
                         function(x, type) date_parse(x, type), type = "reg") %>% 
    bind_cols(registrations, .)
  
  
  # pull out the state, city and zip:  ----------------------------------------
  registrations$state <- registrations$CompanyAddress %>% 
    str_extract_all("(?<=\\,\\s?)[[:ALPHA:]]{2}(?=\\s?\\d+)") %>% 
    unlist()
  
  
  # get city state combo
  registrations$city_state <- registrations$CompanyAddress %>%
    str_extract_all("(?<=\\,\\s?).*(?=\\s?\\d{5})") %>% 
    unlist()
  
  
  # don't need address, contact name, or phone number for modeliing
  registrations <- registrations %>% 
    select(-c(ContactName, CompanyAddress, PhoneNumber))
  
  
  # merge customers and purchases ---------------------------------------------
  customers <- left_join(customers, purchases, by = c("CustomerID" = "CustomerID"))
  
  
  # create dependent variable
  dep_var <- customers %>% 
    select(CompanyName, PurchaseDate) %>% 
    mutate(Response = as.factor(ifelse(PurchaseDate >= start_dep & PurchaseDate <= end_dep, 
                             1,
                             0)))
  
  
  # remove date, don't need
  dep_var <- dep_var %>% select(CompanyName, Response)
  
  
  # don't need purchases anymore
  rm(purchases)
  
  
  # who is in the customers table but not the registrations table...
  # ... the idea is if they became a customer first, then we cannot model them
  
  # Distinct company names in the registrations table
  reg_cust <- registrations %>% 
    distinct(CompanyName)
  
  
  # get the companies in the customers table that did not appear...
  # ... in the registrations table
  cust_no_reg <- dplyr::setdiff(customers["CompanyName"], reg_cust["CompanyName"])
  
  
  # now let's remove these customers from since we don't have historicals
  customers <- customers %>% 
    filter(!(CompanyName %in% cust_no_reg[[1]])) %>% 
    select(CustomerID, CompanyName, PurchaseDate:pur_quarter)
  
  
  # remove, don't need anymore
  rm(cust_no_reg)
  
  
  # need to join them all together
  basetable_pre_agg <- left_join(registrations, customers, by = c("CompanyName" = "CompanyName"))
  
  
  # get rid of customers that have a purchase date before their first registration date...
  # ... same principle as removing customers that became customers in the customers table ...
  # ... that became customers without registering first
  
  
  # get minimumn and maximum dates
  base_table <- basetable_pre_agg %>% 
    filter(RegistrationDate <= PurchaseDate | is.na(PurchaseDate)) %>% 
    group_by(CompanyName) %>% 
    summarise(first_reg = min(RegistrationDate),
              last_reg = max(RegistrationDate))
  
  
  # join with basetable
  basetable_pre_agg <- left_join(basetable_pre_agg, base_table, by = c("CompanyName" = "CompanyName"))
  
  
  # don't need anymore
  rm(base_table)
  
  
  # filter out people that had a purchase before their first registration
  basetable_pre_agg <- basetable_pre_agg %>% 
    filter(PurchaseDate >= first_reg | is.na(PurchaseDate)) %>% 
    select(CompanyName, 
           reg_year, 
           reg_month, 
           reg_day, 
           reg_quarter, 
           state, 
           city_state, 
           first_reg, 
           last_reg,
           PurchaseDate)
  
  
  # remove unecessary tables
  rm(customers, reg_cust, registrations, date_parse)
  
  
  # create dummies ------------------------------------------------------------
  # state, city-state, quarter, quarter-year, first & last reg year
  ## recency
  # end of independent period
  
  
  
  # begin dummy and summarization
  basetable_agg <- basetable_pre_agg %>% 
    select(reg_month, 
           reg_day, 
           reg_quarter, 
           state) %>% 
    sapply(as.character) %>% 
    as.data.frame(stringsAsFactors = F)
  
  
  # define categories
  categ <- categories(basetable_agg)
  
  
  # pass the categories to the dummy function
  basetable_agg <- basetable_agg %>% 
    dummy(int = T, object = categ) %>% 
    bind_cols(select(basetable_pre_agg, CompanyName), .) %>% 
    group_by(CompanyName) %>% 
    summarise_each(funs(sum))
  
  
  # aggregate and summarize
  basetable_agg_total <- basetable_pre_agg %>% 
    group_by(CompanyName, first_reg, last_reg) %>% 
    summarise(total_regs = n()) %>% 
    ungroup() %>% 
    mutate(days_in_sys = difftime(last_reg, first_reg, units = "days"),
           days_since_last = difftime(end_ind, last_reg, units = "days")) %>% 
    left_join(basetable_agg, by = c("CompanyName" = "CompanyName")) %>% 
    select(-c(first_reg, last_reg))
  
  
  # filter dependent variable
  dep_var <- dep_var %>% 
    filter(CompanyName %in% basetable_agg_total[["CompanyName"]])
  
  
  # join the response to the dependent period
  basetable_agg_total <- basetable_agg_total %>% 
    left_join(dep_var, by = c("CompanyName" = "CompanyName"))
  
  
  # if na then not a customer and response should be a zero
  basetable_agg_total[is.na(basetable_agg_total)] <- 0
  
  
  # don't need anymore
  rm(basetable_agg)
  
  return(list(basetable = basetable_agg_total,
              category = categ))
}