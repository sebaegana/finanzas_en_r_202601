library(tidyverse)
library(tidyquant)
library(quantmod)

getSymbols("AAPL",
           src = "yahoo",
           from = "2010-01-01",
           to = "2010-07-30",
           periodicity = "daily")

chartSeries(AAPL, TA=NULL)

chartSeries(AAPL)

chartSeries(AAPL, subset = "last 3 months")

df_aapl_dates <- as_tibble(index(AAPL))

df_aapl_data <- as_tibble(AAPL)

df_aapl <- df_aapl_dates %>%
  bind_cols(df_aapl_data)

g1 <- ggplot(df_aapl) + geom_line(mapping = aes(value, AAPL.Close))

g1 <- g1 + labs(title = "AAPLE", subtitle = "Año 2010") +
  xlab("Fecha") + ylab("")

g1 <- g1 + theme_bw()

g1

##############################################################

tickers <- c("ORCL","AMD","IBM","NVDA")

getSymbols(tickers, src = "yahoo",
           from = "2010-01-01",
           to = "2018-07-30", periodicity = "daily")

list <- lapply(tickers, function(x) Cl(get(x)))

precio.cierre <- do.call(merge,list)

df_all_dates <- as_tibble(index(precio.cierre))

df_all_data <- as_tibble(precio.cierre)

df_all <- df_all_dates %>%
  bind_cols(df_all_data)

g1 <- ggplot(df_all) +
  geom_line(mapping = aes(value, ORCL.Close), color = 'blue') +
  geom_line(mapping = aes(value, AMD.Close), color = 'red') +
  geom_line(mapping = aes(value, IBM.Close), color = 'green') +
  geom_line(mapping = aes(value, NVDA.Close), color = 'yellow')
g1 <- g1 + labs(title = "ARCL", subtitle = "2010 al 2018") + xlab("Fecha") + ylab("")
g1 <- g1 + theme_bw()
g1





