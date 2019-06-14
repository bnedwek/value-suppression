library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)

rbool <- function(n) {
  bools <- sample(c(TRUE,FALSE), n, replace=T)
  return(bools)
}

make_data <- function(nobs=10) {
  
  start_dt <- ymd('2009-01-01')
  dates <- start_dt + months(seq(nobs) - 1)
  
  x <- runif(nobs)
  x.suppress <- rbool(nobs)
  
  y <- runif(nobs)
  y.suppress <- rbool(nobs)
  
  z <- runif(nobs)
  z.suppress <- rbool(nobs)
  
  data <- data.frame(
    date=dates,
    x, x.suppress,
    y, y.suppress,
    z, z.suppress
  )
  
  return(data)

}

data <- make_data()

data.publish <- data %>%
  gather(sloosvar, measure, -date) %>%
  mutate(
    measure_type = if_else(str_detect(sloosvar, '.suppress$'), 'suppress', 'val'),
    sloos_question = str_remove(sloosvar, '.suppress')
  ) %>%
  select(-sloosvar) %>%
  spread(measure_type, measure) %>%
  mutate(
    suppress = as.logical(suppress),
    val.publish = ifelse(suppress, NA, val)
  )

data.suppressed <- data.publish %>%
  gather(measure_type, measure, -date, -sloos_question) %>%
  unite("colname", sloos_question, measure_type) %>%
  spread(colname, measure) %>%
  mutate_at(vars(contains('suppress')), as.logical)
