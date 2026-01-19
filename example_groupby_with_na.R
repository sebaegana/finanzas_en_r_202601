df <- data.frame(
  region = c("Norte", "Centro", NA, "Sur", "", "Norte"),
  producto = c("A", "B", "A", "B", "A", NA),
  monto = c(100, 150, 200, NA, 120, 180)
)


df %>%
  group_by(region) %>%
  summarise(total = sum(monto))
