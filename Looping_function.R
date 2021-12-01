#Function that takes unknown number of arguments and loops through arguments to create new column, called "quarter"
date_quarter <- function(...) {
  library(tidyverse)
  library(lubridate)
  library(nycflights13)
  #pass unknown variables vector to one name
  dates_months <- (...)
  
  #create column for eval of quarter of year
  quarter_flights <- mutate(flights, quarter = sapply(dates_months, 
  (function(dates_months) if (dates_months <=3) {
    "Q1"
  }else if(dates_months>3 & dates_months <=6) {
    "Q2"
  }else if (dates_months>6 & dates_months <=9) {
    "Q3"
  } else {
    "Q4"
  } ))) 
                                                                          
  return(quarter_flights)
}

#set flights to vector of months
date_flights <- c(flights$month)

#test
date_quarter(date_flights)
