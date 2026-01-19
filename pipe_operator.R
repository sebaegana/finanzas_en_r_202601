
###

x <- c(1, 2, 3, 4, 5)

resultado <- sqrt(mean(log(x)))

resultado_2 <- x %>%
  log() %>%
  mean() %>%
  sqrt()


###



set.seed(123)

ventas_df <- data.frame(
  id_venta = 1:12,
  fecha = as.Date("2024-01-01") + sample(0:60, 12),
  region = sample(c("Norte", "Centro", "Sur"), 12, replace = TRUE),
  vendedor = sample(c("Ana", "Juan", "Pedro", "María"), 12, replace = TRUE),
  producto = sample(c("Laptop", "Tablet", "Celular"), 12, replace = TRUE),
  unidades = sample(1:5, 12, replace = TRUE),
  monto = sample(c(200000, 350000, 500000), 12, replace = TRUE)
)

ventas_df

library(dplyr)

resultado <- summarise(
  group_by(ventas_df, region),
  ventas_promedio = mean(monto),
  total_ventas = sum(monto)
)

resultado

### Utilización de pipe operator

ventas_df %>%
  group_by(region) %>%
  summarise(
    ventas_promedio = mean(monto),
    total_ventas = sum(monto)
  )