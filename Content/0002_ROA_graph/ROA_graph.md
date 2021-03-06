---
title: "Return on Assets graph"
author: "Keaton Parkinson"
date: "April 30, 2021"
output:
  html_document:  
    keep_md: true
    toc: true
    toc_float: true
    code_folding: hide
    fig_height: 6
    fig_width: 12
    fig_align: 'center'
---


```r
library(tidyverse)
library(readxl)
library(gganimate) #This turns the plot into a GIF so that it is animated
library(lubridate)
library(ggthemes)
```


```r
# key_ratios.xlsx came from the key ratios at Morninstar for Delta, American, United, and Southwest
data_ROA <- read_excel('key_ratios.xlsx', sheet = 'ROA') %>% 
  mutate(Year = paste(Year, '-01', sep = ''), Year = as.Date(Year), Year = year(Year))

# Ranking who has the highest return on assets ratio for each year. This allows me
# to let change the order of the bars each year easily with gganimate
plotdata <- data_ROA %>%
  group_by(Year) %>%
  mutate(ordering = rank(`Return On Assets`)) %>%
  ungroup() 

#Colors for the bar charts for the airline companies
colorsp <- c('gray60', 'darkblue', 'red', 'royalblue')
```


```r
# Return on assets plot
ggplot(plotdata, aes(ordering, group = Airline, color = Airline, fill = Airline)) + 
  geom_tile(aes(y = `Return On Assets`/2,
                height = `Return On Assets`,
                width = 0.9), alpha = 0.9) +
  geom_text(aes(y = `Return On Assets`, label = Airline), hjust = -0.4) +
  geom_text(aes(y = -10, label = Airline), hjust = .4) +
  coord_flip() +
  scale_color_manual(values = colorsp) +
  scale_fill_manual(values = colorsp) +
  theme_tufte(14,"Avenir") +
  guides(color=F,fill=F) +
  geom_hline(yintercept=c(-5, 5, 10, 15), linetype="dotted") +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-5, 0, 5, 10, 15)) +
  transition_time(as.integer(Year)) +
  labs(title = "Return on Assets Ratio: {frame_time} ", y = 'Return on Assets Ratio', x = '',
       subtitle = 'The return on assets ratio is calculated at the end of each year except for the year 2020.\nThe 2020 ratio is calcuated using the past 12 consecutive months.',
       caption = 'Source: Morningstar (keatonparkinson.com)') +
  ease_aes('cubic-in-out') +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(size = 11),
        plot.title = element_text(size = 18))
```

![](ROA_graph_files/figure-html/unnamed-chunk-3-1.gif)<!-- -->

