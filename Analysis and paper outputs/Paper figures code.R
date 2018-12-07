# 

library(tidyverse)
library(gridExtra)

##  Load in data
inequal.tab <-
  read.csv('Working analysis files/Duncan index results table.csv')

inequal.tab %>% head

##  Graphs
geo1 <- 
  ggplot(data = inequal.tab, aes(x = geo04, y = geo10, colour = type)) + geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('2011') + xlab('2001') +
  ggtitle('RCI in 2001 and 2011')

rci2
geo2 <- 
  ggplot(data = inequal.tab, aes(x = log(total.pop), y = geodiff, colour = type)) + 
  geom_point() + geom_smooth(alpha = 0.2) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Change in RCI') + xlab('Population (log)')+
  ggtitle('RCI change by population (loess curve)') +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed')

rci.toptemp <- rci.res[order(-1 * rci.res$pop)[1:10], ]
rci.topten <- data.frame(ttwa = factor(rci.toptemp$ttwa, 
                                       levels = rci.toptemp$ttwa),
                         rci = c(rci.toptemp$rci01.50., rci.toptemp$rci11.50.),
                         lo.rci = c(rci.toptemp$rci01.2.5., rci.toptemp$rci11.2.5.),
                         hi.rci = c(rci.toptemp$rci01.97.5., rci.toptemp$rci11.97.5.),
                         year = c(rep('2001', 10), rep('2011', 10)))

rci3 <- ggplot(data = rci.topten, aes(x = ttwa, y = rci, fill = year)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(data = rci.topten, 
                aes(x = ttwa, ymin = lo.rci, ymax = hi.rci),
                alpha = 0.5,
                position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1, size = 8),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.1, 0.9),
        legend.background = element_rect(fill="transparent"),
        legend.title = element_blank(),
        legend.direction = 'horizontal') +
  ylab('RCI') + 
  xlab('')+
  ggtitle('RCI for most populated TTWAs with 95% credible intervals') 

grid.arrange(rci3, rci1, rci2, 
             layout_matrix = rbind(c(1, 1),
                                   c(2, 3))
)

##  Graph 3

rci1 <- 
  ggplot(data = inequal.tab, aes(x = rci01, y = rci11, colour = type)) + geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('2011') + xlab('2001') +
  ggtitle('RCI in 2001 and 2011')

rci2
rci2 <- 
  ggplot(data = rci.gg, aes(x = log(total.pop), y = rcidiff, colour = type)) + 
  geom_point() + geom_smooth(alpha = 0.2) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Change in RCI') + xlab('Population (log)')+
  ggtitle('RCI change by population (loess curve)') +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed')
