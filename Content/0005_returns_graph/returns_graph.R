library(tidyverse)
library(tidyquant)
library(lubridate)
library(forcats)
library(gridExtra)
library(viridis)
library(wesanderson)
library(RColorBrewer)

# Large Cap Index Returns - VV
# Mid Cap Index Returns - VO
# Small Cap Index Returns - VB
# Real Estate Index Returns - VNQ
# Emerging Markets Index Returns - VWO
# Developing Markets Index Returns - VEA

#Get S&P prices from Yahoo Finance

prices <- c("VV", 'VO', 'VB', 'VNQ', 'VWO', 'VEA') %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2021-01-01")


graph <- function(year) {
  returns_yearly <- prices %>%
    group_by(symbol) %>%
    tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn, 
                 period     = "yearly", 
                 col_rename = "Returns") %>% 
    mutate(Year = year(date)) %>% 
    group_by(Year, symbol) %>% 
    arrange(Year, Returns) %>% 
    filter(Year == year) %>% 
    mutate(Year = as.factor(Year), symbol = as.factor(symbol), Returns = scales::percent(Returns, accuracy = .11))
  
  returns_yearly$symbol <- factor(returns_yearly$symbol, levels = returns_yearly$symbol)
  
  ggplot(returns_yearly, aes(x = Year, y = symbol, fill = symbol)) +
    geom_tile() + 
    geom_text(data = returns_yearly, aes(x = Year, y = symbol, label = symbol, vjust = -.2), size =5) +
    geom_text(aes(fill = Returns, label = returns_yearly$Returns, vjust = 2), size =3) +
    facet_wrap(~Year, scales = 'free') +
    scale_fill_brewer(palette="Paired") +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0)) +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none",
          plot.margin = rep(unit(0,"null"),4),
          panel.margin = unit(0,"null"))
}


grid.arrange(graph(2010), graph(2011), graph(2012), graph(2013), graph(2014), graph(2015), 
             graph(2016), graph(2017), graph(2018), graph(2019), graph(2020), ncol=12)


##########################################################################
p <- ggplot() +
  geom_point(data = returns_yearly, aes(x = Year, y = symbol, color = symbol, shape = 15, fill = Returns), size = 28, alpha = .5) +
  geom_text(data = returns_yearly, aes(x = Year, y = symbol, label = Returns)) +
  geom_text(data = returns_yearly, aes(x = Year, y = symbol, label = symbol, vjust = 3)) +
  facet_wrap(~Year, scales = 'free') +
  scale_shape_identity() +
  theme(axis.text.y = element_blank(),
        legend.position = "none") 

p1 <- ggplot() +
  geom_point(data = returns_yearly2011, aes(x = Year, y = symbol, color = symbol, shape = 15, fill = Returns), size = 28, alpha = .5) +
  geom_text(data = returns_yearly2011, aes(x = Year, y = symbol, label = Returns)) +
  geom_text(data = returns_yearly2011, aes(x = Year, y = symbol, label = symbol, vjust = 3)) +
  scale_shape_identity() +
  facet_wrap(~Year) +
  theme(axis.text.y = element_blank(),
        legend.position = "none") 

grid.arrange(p, p1, ncol=2)

