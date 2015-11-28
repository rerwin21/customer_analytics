# load in the first 10 rows and see which columns to test and how many ------
customers <- read.csv("customers.csv", 
                      stringsAsFactors = F,
                      nrows = 10)


products <- read.csv("products.csv", 
                     stringsAsFactors = F,
                     nrows = 10)


stores <- read.csv("stores.csv",
                   stringsAsFactors = F,
                   nrows = 10)


trans <- read.csv("transactions.csv",
                  stringsAsFactors = F,
                  nrows = 10)


trans_details <- read.csv("transactiondetails.csv",
                          stringsAsFactors = F,
                          nrows = 10)


# read in the data again, as characters and  test the id's in question ------
customers <- read.csv("customers.csv", 
                      stringsAsFactors = F,
                      colClasses = rep("character", ncol(customers)))


products <- read.csv("products.csv", 
                     stringsAsFactors = F,
                     colClasses = rep("character", ncol(products)))


stores <- read.csv("stores.csv",
                   stringsAsFactors = F,
                   colClasses = rep("character", ncol(stores)))


trans <- read.csv("transactions.csv",
                  stringsAsFactors = F,
                  colClasses = rep("character", ncol(trans)))


trans_details <- read.csv("transactiondetails.csv",
                          stringsAsFactors = F,
                          colClasses = rep("character", ncol(trans_details)))


# test for leading zeros ----------------------------------------------------
.leading_zero <- function(table, cols){
  table <- table[cols] # subset the columns specified, while maintaining a df
  lz <- function(x) any(str_detect(x, "^0")) # any zero as first char
  lzs <- sapply(table, lz) # apply function over each specified col
  return(lzs) # return the each col and the T/F
} 


# list of tables
tables <- list(cust = customers,
               prod = products,
               store = stores,
               trans = trans,
               td = trans_details)


# list of columns to inspect for each
cols <- list("custid",
             c("SKU", "family"),
             c("ZIP", "storeid"),
             c("custid", "receiptnbr", "storeid"),
             c("receiptnbr", "SKU"))


# now test each table
table_lz <- Map(.leading_zero, tables, cols)


# do any of the tables have leading zeroes?
lzs <- rapply(table_lz, any)


# look at the list of tables that do
table_lz[lzs]


# SKU seems to be the only problem, so read that in as a character...
# ... all other can be read in as the defaults, so I need to get those ...
# ... and store them for later use


# reload and get the faults -------------------------------------------------
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