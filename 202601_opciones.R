library(tidyverse)

# Set parameters
strike_price <- 50
stock_prices <- seq(from = 30, to = 70, by = 1)
# Calculate the payoff
payoff <- pmax(stock_prices - strike_price, 0)
# Create a dataframe

option_data <- data.frame(StockPrice = stock_prices, Payoff = payoff)
# Plotting the call option payoff
option_plot <- ggplot(option_data, aes(x = StockPrice, y = Payoff)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(title = "Call Option Payoff", x = "Stock Price at Expiration", y = "Payoff") +
  theme_minimal()

print(option_plot)

# Parameters
strike_price <- 50
stock_prices <- seq(from = 30, to = 70, by = 1)

payoff <- pmax(strike_price - stock_prices, 0)
# Create a dataframe
option_data <- data.frame(StockPrice = stock_prices, Payoff = payoff)
# Plotting the put option payoff
option_plot <- ggplot(option_data, aes(x = StockPrice, y = Payoff)) +
  geom_line(color = "red", size = 1.5) + # Red line for visibility
  labs(title = "Put Option Payoff",
       x = "Stock Price at Expiration",
       y = "Payoff",
       subtitle = "Strike Price: 50") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), # Centering title
        plot.subtitle = element_text(hjust = 0.5)) # Centering subtitle
# Display the plot
print(option_plot)


p2 <- ggplot(data.frame(x = c(0, 100)), aes(x = x))
fun.2 <- function(x) x - 40
p2 <- p2 + stat_function(fun = fun.2, colour = "red", linewidth =1.5)
p2 <- p2 + scale_x_continuous("Precio del activo") + scale_y_continuous("Payoff") +
  geom_hline(yintercept = 0, colour = "black") +
  geom_vline(xintercept = 0, colour = "black") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  ggtitle("Posición larga en un activo")
p2


p3 <- ggplot(data.frame(x = c(0, 100)), aes(x = x))
fun.3 <- function(x) x - 40
p3 <- p3 + stat_function(fun = fun.3, colour = "red", size=1.5)
p3 <- p3 + scale_x_continuous("Precio del activo") + scale_y_continuous("Payoff") +
  geom_hline(yintercept = 0, colour = "black") +
  geom_vline(xintercept = 0, colour = "black") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  ggtitle("Posición larga en un activo") +
  geom_point(aes(x=40, y=0), colour="blue", shape="|", size=10)
p3


p4 <- ggplot(data.frame(x = c(0, 100)), aes(x = x))
fun.4 <- function(x) -x + 40
p4 <- p4 + stat_function(fun = fun.4, colour = "blue", size=1.5)
p4 <- p4 + scale_x_continuous("Precio del activo") + 
  scale_y_continuous("Payoff") +
  geom_hline(yintercept = 0, colour = "black") +
  geom_vline(xintercept = 0, colour = "black") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  ggtitle("Posición corta en un contrato de venta a futuro") +
  geom_point(aes(x=40, y=0), colour="red", shape="|", size=10)
p4


p5 <- ggplot(data.frame(x = c(0, 100)), aes(x = x))
fun.5 <- function(x) x - 40
fun.6 <- function(x) -x + 40
p5 <- p5 + stat_function(fun = fun.5, colour = "red", size=1.5) +
  stat_function(fun = fun.6, colour = "blue", size=1.5)
p5 <- p5 + scale_x_continuous("Precio del activo") + scale_y_continuous("Payoff") +
  geom_hline(yintercept = 0, colour = "black") +
  geom_vline(xintercept = 0, colour = "black") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  ggtitle("Cobertura 1")

p5
