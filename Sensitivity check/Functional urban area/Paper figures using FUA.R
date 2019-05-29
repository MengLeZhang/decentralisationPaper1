## This is the graphics used for the revised Urban studies paper BUT using FUA
##  First we will recreate the graphs we first used

# Preamble: Loading files and common datasets -----------------------------
source('RCI functions.R')
library(tidyverse)
library(gridExtra)
library(tmap)
library(tmaptools)

##  Common graph elements
##  Size of font
font1 <- element_text(size = 14)

##  Creating common datasets 
##  Load in data
inequal.tab <-
  read.csv('Working analysis files/Duncan index results table for FUA (optional).csv')

##  For maps : need CRS
ukgrid = "+init=epsg:27700" ## always remember the CRS

##  Creating a long form version; it's already in pop size order 
topten <- inequal.tab$fua[1:10]
topten_long <- 
  inequal.tab %>%
  filter(fua %in% topten) %>% 
  gather(key = stat, value = value, -fua, - type) %>%
  mutate(type_stat = paste(type, stat, sep = '_')) # Stat plus type of claimant


##  Non-default colours for 2 colour palette
contrast <- c("#E69F00", "#56B4E9")

##  Graphs
##  Change by population -------------
##  Basically neeed a new variable saying what type of stat it is
facet.gg <- rbind(inequal.tab %>% 
                    mutate(stat_diff = rcidiff,
                           index = '(a) RCI'),
                  inequal.tab %>% 
                    mutate(stat_diff = workdiff_exp,
                           index = '(b) RAE'),
                  inequal.tab %>% 
                    mutate(stat_diff = geodiff,
                           index = '(c) RPA'))


fig_popdiff <- 
  ggplot(data = facet.gg, 
         aes(x = log(total.pop), 
             y = stat_diff, 
             colour = type)) + 
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(alpha = 0.4) + 
  geom_smooth(alpha = 0.8, se = F) + 
  ylab(NULL) + 
  xlab('Population (log)')+
  facet_grid(. ~ index) +
  scale_colour_discrete(labels = c('Low Income', 'Jobseekers'),
                        name = 'Claimant type') +
  theme(legend.position = 'bottom',
        title = NULL,
        text = font1
  )

### Change for top ten TTWAs ----
facet.gg_topten <- 
  topten_long %>%
  filter(
    stat %in% c(
      'rci01', 'rci11',
      'work01_exp', 'work11_exp',
      'geo04', 'geo10'
    ) & 
      type == 'inc') %>%
  mutate(year = ifelse(stat %in% c('rci01', 'work01_exp', 'geo04'),
                       '2001', 
                       '2011'),
         index =stat %>% 
           plyr::revalue(c(
             rci01 = '(a) RCI',
             rci11 = '(a) RCI',
             work01_exp = '(b) RAE',
             work11_exp = '(b) RAE',
             geo04 = '(c) RPA',
             geo10 = '(c) RPA'
           )
           )
  )



fig_topten <- 
  ggplot(data = facet.gg_topten, 
         aes(x = fua, y = value, fill = year)) + 
  geom_hline(yintercept=0) +
  geom_bar(stat='identity', position=position_dodge()) +
  ylab(NULL) + 
  xlab(NULL) +
  facet_grid(index ~ .) +
  scale_fill_manual(values = contrast, 
                    labels = c('2001', '2011'),
                    name = 'Year') +
  theme(legend.position = 'bottom',
        title = NULL,
        text = font1,
        axis.text.x = element_text(angle = 15, hjust = 1, size = 8),
        plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill="transparent"),
        legend.title = element_blank(),
        legend.direction = 'horizontal'
  )

fig_topten
