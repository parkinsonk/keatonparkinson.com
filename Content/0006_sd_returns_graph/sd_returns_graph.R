library(tidyverse) #Used for data wrangling and visualization
library(tidyquant) #Get Stock data
library(lubridate) #Deals with dates
library(viridis)   #For coloring the plot


# Large Cap Index Returns - VV
# Mid Cap Index Returns - VO
# Small Cap Index Returns - VB
# Real Estate Index Returns - VNQ
# Emerging Markets Index Returns - VWO
# Developing Markets Index Returns - VEA
# Gold Returns - GLD


#Get Stock prices from Yahoo Finance

prices <- c("VV", 'VO', 'VB', 'VNQ', 'VWO', 'VEA', 'GLD') %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2021-01-01")

annualized_sd <- prices %>% 
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "weekly", 
               col_rename = "Returns") %>% 
  summarize(annualized_sd = (sd(Returns) * sqrt(52)) * 100)

returns_weekly <- prices %>% 
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "weekly", 
               col_rename = "Returns") %>% 
  filter(Returns != max(Returns), Returns != min(Returns))

returns_weekly_max <- prices %>% 
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "weekly", 
               col_rename = "Returns") %>% 
  summarize(max = max(Returns))

returns_weekly_min <- prices %>% 
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "weekly", 
               col_rename = "Returns") %>% 
  summarize(min = min(Returns))

returns_weekly_median <- prices %>% 
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "weekly", 
               col_rename = "Returns") %>% 
  summarize(median = median(Returns))



ggplot(returns_weekly, aes(x = Returns)) +
  geom_histogram(bins = 50) +
  facet_wrap(~symbol, scales = 'free')

ggplot(returns_weekly, mapping = aes(x = '', y = Returns)) +
  geom_point(returns_weekly, mapping = aes(x = '', y = Returns)) +
  geom_point(returns_weekly_max, mapping = aes(x = '', y = max, color = 'red'), size = 3) +
  geom_point(returns_weekly_min, mapping = aes(x = '', y = min, color = 'red'), size = 3) +
  geom_point(returns_weekly_median, mapping = aes(x = '', y = median, color = 'red'), size = 3) +
  geom_jitter() +
  coord_flip() +
  facet_grid(rows = vars(symbol)) +
  theme(strip.text.y.left = element_text(angle = 0))

returns_daily %>% 
  group_by(symbol) %>% 
  tq_performance(perfomance_fun = sd.annualized())
