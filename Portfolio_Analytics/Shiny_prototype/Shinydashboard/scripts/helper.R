
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(openxlsx)

# Read & Manipulate Data -------------------------------------------------------
#read.data <- function() {
  
  

  products <- read.xlsx("data/bikes.xlsx",detectDates = TRUE)
  str(products)
  
  
  customers <- read.xlsx("./data/bikeshops.xlsx",detectDates = TRUE  )
  str(customers)
  orders <- read.xlsx("./data/orders.xlsx" ,detectDates = TRUE )
  str(orders)
  orders.extended <- merge(orders, customers, 
                           by.x = "customer.id", by.y="bikeshop.id")
  orders.extended <- merge(orders.extended, products, 
                           by.x = "product.id", by.y = "bike.id")
  
  orders.extended <- orders.extended %>%
    mutate(price.extended = price * quantity,
           month = lubridate::month(order.date, label = TRUE),
           year = year(order.date),
           category1 = as.character(category1),
           category2 = as.character(category2)) %>%
    select(order.id, order.line, order.date, month, year,
           bikeshop.name, bikeshop.state, latitude, longitude,
           model, category1, category2, frame, 
           quantity, price, price.extended) %>%
    arrange(order.id, order.line) 



#}


