# Loads tidyquant, lubridate, xts, quantmod, TTR 
library(tidyverse)
library(tidyquant)

tq_index_options()

acciones_sp500 = tq_index("SP500")

acciones_nyse = tq_exchange("NYSE")

filter_sqm = acciones_nyse %>% filter(symbol == 'SQM')

filter_ltm = acciones_nyse %>% filter(symbol == 'LTM')


ltm_prices  <- tq_get("LTM", get = "stock.prices", from = " 1990-01-01")
ltm_prices 

stocks <- c("LTM", "SQM") %>%
  tq_get(from = "2013-01-01")
stocks

###

library(purrr)

tickers <- c("LTM", "SQM")

from <- "2013-01-01"

batch_size <- 50

stocks <- tickers %>%
  split(ceiling(seq_along(.) / batch_size)) %>%   # crea batches
  map_dfr(~ tq_get(.x, from = from))     # une resultados

stocks
