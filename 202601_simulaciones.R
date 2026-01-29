library(tidyverse)

So = 100
volatilidad = 0.15
mu = 0.50
delta_t = 1 / 365
iteraciones = 1000
periodos = 365

matriz_precios = matrix(, periodos + 1, iteraciones)

matriz_precios[1, ] = So

for (i in 1:iteraciones)
  for (j in 2:nrow(matriz_precios))
  {matriz_precios[j, i] =
    matriz_precios[j - 1, i] *
    exp((mu - volatilidad ^ 2 / 2) *
          delta_t +
          volatilidad *
          rnorm(1) *
          sqrt(delta_t))
  }

drift = as_tibble(apply(matriz_precios, 1, mean))

cuantiles = as_tibble(t(apply(matriz_precios, 1, quantile,
                              probs = c(0.01, 0.05, 0.95, 0.99))))


data <- drift %>%
  bind_cols(cuantiles) %>%
  mutate(Trend = seq(1,366))

ggplot(data=data, aes(x=Trend)) +
  geom_line(aes(y=value), color = "green") +
  geom_line(aes(y=`1%`), color = "blue") +
  geom_line(aes(y=`5%`), color = "darkred") +
  geom_line(aes(y=`95%`), color = "darkred") +
  geom_line(aes(y=`99%`), color = "blue")


