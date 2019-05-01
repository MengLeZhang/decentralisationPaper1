##  Paper stats using FUA instead of ttwa
##  This is a worksheet that basically generate the stats that we used in the 
##  paper from the saved results that we have

library(tidyverse)

##  Results summary
inequal.tab <-
  read.csv('Working analysis files/Duncan index results table for FUA (optional).csv')

stargazer::stargazer(inequal.tab, type = 'text')

##  create quantile variable?
inequal.tab <- 
  inequal.tab %>%
  mutate(third = ntile(total.pop, 3),
         decile = ntile(total.pop, 10))

inequal.tab$third %>% table #okay so euql ns good 
inequal.tab$decile %>% table


inequal.tab %>% 
  filter(type == 'inc') %>% 
  summary

inequal.tab %>% filter(is.na(rci01))
##  All the Welsh ones for income basically 
inequal.tab %>% filter(is.na(geo04)) #again welsh due to no geo??

inequal.tab %>% 
  filter(type == 'inc') %>%
  group_by(third) %>%
  summarise_all(mean, na.rm = T) %>% 
  print.data.frame ## to print all

inequal.tab %>% 
  filter(type == 'inc') %>%
  group_by(decile) %>%
  summarise_all(mean, na.rm = T) %>% 
  print.data.frame ## to print all

inequal.tab %>% 
  filter(type == 'jsa') %>% 
  summary

##  England only?
wales.df <- 
  inequal.tab %>% 
  filter(is.na(rci01))
wales.df
##4 fua
(inequal.tab %>% nrow) /2
41 - 4 # 37 english

inequal.tab %>% 
  filter(type == 'jsa') %>% 
  summary

inequal.tab %>% 
  filter(type == 'jsa' & !is.na(geo04)) %>% 
  summary

##  RCI difference
inequal.tab <-
  read.csv('Working analysis files/Duncan index results table for FUA (optional).csv')

inequal.tab %>% head
inequal.tab <- inequal.tab %>% 
  mutate(rci01_diff_method = abs(rci01 - rci01_main),
         rci11_diff_method = abs(rci11 - rci11_main)) 

inequal.tab$rci01_diff_method %>% quantile(na.rm = T, 0.9)

inequal.tab %>% 
  filter(type == 'inc') %>% 
  dplyr::select(total.pop, 
                rci01_diff_method, 
                rci11_diff_method) %>%
  cor(use = 'pairwise.complete.obs',
      method = 'spearman')

##  Correlations matrix
inequal.tab <-
  read.csv('Working analysis files/Duncan index results table for FUA (optional).csv')

inequal.tab %>% 
  filter(type == 'inc') %>% 
  dplyr::select(total.pop, 
                rcidiff,
                workdiff_exp,
                geodiff) %>%
  cor(use = 'pairwise.complete.obs',
      method = 'spearman')



inequal.tab %>% 
  filter(type == 'jsa') %>% 
  dplyr::select(total.pop, 
                rcidiff,
                workdiff_exp,
                geodiff,
                work01_exp,
                work11_exp) %>%
  cor(use = 'pairwise.complete.obs',
      method = 'spearman')
##  Cor between

##  test for cor
cor.test(formula =  ~ rcidiff + total.pop, 
         data = inequal.tab,
         subset = (type == 'jsa'),
         method = 'spearman') 
## No longer significant

cor.test(formula =  ~ workdiff_exp + total.pop, 
         data = inequal.tab,
         subset = (type == 'jsa'),
         method = 'spearman') 
##  sig
cor.test(formula =  ~ geodiff + total.pop, 
         data = inequal.tab,
         subset = (type == 'jsa'),
         method = 'spearman') 
#sig

cor.test(formula =  ~ rcidiff + total.pop, 
         data = inequal.tab,
         subset = (type == 'inc'),
         method = 'spearman') 
## Not sig (but lower power and variance)

cor.test(formula =  ~ workdiff_exp + total.pop, 
         data = inequal.tab,
         subset = (type == 'inc'),
         method = 'spearman') 
# sig

cor.test(formula =  ~ geodiff + total.pop, 
         data = inequal.tab,
         subset = (type == 'inc'),
         method = 'spearman') 
# N.S

