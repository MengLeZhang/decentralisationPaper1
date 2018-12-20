# This is the graphics used for the revised Urban studies paper
##  First we will recreate the graphs we first used



# Preamble: Loading files and common datasets -----------------------------
source('RCI functions.R')
library(tidyverse)
library(gridExtra)

##  Creating common datasets 
##  Load in data
inequal.tab <-
  read.csv('Working analysis files/Duncan index results table.csv')

##  Creating a long form version; it's already in pop size order 
topten <- inequal.tab$TTWA11NM[1:10]
topten_long <- 
  inequal.tab %>%
  filter(TTWA11NM %in% topten) %>% 
  gather(key = stat, value = value, -TTWA11NM, - type) %>%
  mutate(type_stat = paste(type, stat, sep = '_'))

##  Non-default colours for 2 colour palette
contrast <- c("#E69F00", "#56B4E9")

##  Graphs
##  Example graph for RCI -----
gg.tab <- 'Extra data/Example RCI table.csv' %>% read.csv
green <- rgb(0.4, 1, 0.4, alpha=0.25)

duncan <- 
  ggplot(gg.tab, aes(x = c.propB, y = c.propA)) + geom_line() + 
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  geom_ribbon(aes(ymin = c.propB, ymax = c.propA, fill = green, alpha = 0.5)) +
  scale_fill_manual(values=c(green)) + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + 
#  ggtitle('Relative centralisation index') +
  theme(text = element_text(size = 11)) + 
  ylab('Cumulative prop. of poor ranked by distance to centre (low to high)') +
  xlab('Cumulative prop. of non-poor ranked by distance to centre (low to high)')


print(duncan)

##  RCI -----

rci1 <- 
  ggplot(data = inequal.tab, aes(x = rci01, y = rci11, group = type)) + 
  geom_point(alpha = 0.8, aes(colour = type)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('2011') + xlab('2001') +
  ggtitle('RCI in 2001 and 2011/12') +
  scale_color_discrete(name = 'Type', labels = c('Income', 'JSA')) 


rci2 <- 
  ggplot(data = inequal.tab, aes(x = log(total.pop), y = rcidiff, colour = type)) + 
  geom_point(alpha = 0.8) + 
  geom_smooth(alpha = 0.2) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Change in index') + xlab('Population (log)')+
  ggtitle('Index change by population (loess curve)') +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  guides(colour = F)

rci3 <- 
  ggplot(data = 
           topten_long %>%
           filter(stat %in% c('rci01', 'rci11') & type == 'jsa'), 
         aes(x = TTWA11NM, y = value, fill = type_stat)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  scale_fill_manual(values = contrast, 
                    labels = c('2001', '2011')) +
  #  scale_fill_discrete(labels = c('2001', '2011')) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1, size = 8),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'top',
        legend.background = element_rect(fill="transparent"),
        legend.title = element_blank(),
        legend.direction = 'horizontal') +
  ylab('Relative centralisation') + 
  xlab('')+
  ggtitle('RCI for most populated TTWAs (JSA)') 

grid.arrange(rci3, rci1, rci2, 
             layout_matrix = rbind(c(1, 1, 1, 1, 1, 1, 1, 1, 1),
                                   c(2, 2, 2, 2, 2, 3, 3, 3, 3))
)

##  Access to work -----
work1 <- 
  ggplot(data = inequal.tab, aes(x = work01_sq, y = work11_sq, group = type)) + 
  geom_point(alpha = 0.8, aes(colour = type)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('2011') + xlab('2001') +
  ggtitle('Access to employment in 2001 and 2011/12') +
  scale_color_discrete(name = 'Type', labels = c('Income', 'JSA')) 


work2 <- 
  ggplot(data = inequal.tab, aes(x = log(total.pop), y = workdiff_sq, colour = type)) + 
  geom_point(alpha = 0.8) + 
  geom_smooth(alpha = 0.2) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Change in index') + xlab('Population (log)')+
  ggtitle('Index change by population (loess curve)') +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  guides(colour = F)

work3 <- 
  ggplot(data = 
           topten_long %>%
           filter(stat %in% c('work01_sq', 'work11_sq') & type == 'jsa'), 
         aes(x = TTWA11NM, y = value, fill = type_stat)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  scale_fill_manual(values = contrast, 
                    labels = c('2001', '2011')) +
  #  scale_fill_discrete(labels = c('2001', '2011')) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1, size = 8),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'top',
        legend.background = element_rect(fill="transparent"),
        legend.title = element_blank(),
        legend.direction = 'horizontal') +
  ylab('Relative access to employment index') + 
  xlab('')+
  ggtitle('Access to employment for most populated TTWAs (JSA)') 

grid.arrange(work3, work1, work2, 
             layout_matrix = rbind(c(1, 1, 1, 1, 1, 1, 1, 1, 1),
                                   c(2, 2, 2, 2, 2, 3, 3, 3, 3))
)




##  Geo ----
geo1 <- 
  ggplot(data = inequal.tab, aes(x = geo04, y = geo10, group = type)) + 
  geom_point(alpha = 0.8, aes(colour = type)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('2011') + xlab('2001') +
  ggtitle('Proximity to amenities index in 2001 and 2011/12') +
  scale_color_discrete(name = 'Type', labels = c('Income', 'JSA')) 


geo2 <- 
  ggplot(data = inequal.tab, aes(x = log(total.pop), y = geodiff, colour = type)) + 
  geom_point(alpha = 0.8) + geom_smooth(alpha = 0.2) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Change in index') + xlab('Population (log)')+
  ggtitle('Index change by population (loess curve)') +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  guides(colour = F)

geo3 <- 
  ggplot(data = 
           topten_long %>%
           filter(stat %in% c('geo04', 'geo10') & type == 'jsa'), 
         aes(x = TTWA11NM, y = value, fill = type_stat)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  scale_fill_manual(values = contrast, 
                    labels = c('2001', '2011')) +
#  scale_fill_discrete(labels = c('2001', '2011')) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1, size = 8),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'top',
        legend.background = element_rect(fill="transparent"),
        legend.title = element_blank(),
        legend.direction = 'horizontal') +
  ylab('Amenities index') + 
  xlab('')+
  ggtitle('Proximity to amenities index for most populated TTWAs (JSA: England)') 

grid.arrange(geo3, geo1, geo2, 
             layout_matrix = rbind(c(1, 1, 1, 1, 1, 1, 1, 1, 1),
                                   c(2, 2, 2, 2, 2, 3, 3, 3, 3))
)


