### Table code

library(stargazer)
library(tidyverse)

##  journal styles


##  table 1: example
tab1.df <- 'Extra data/Example RCI table shortened.csv' %>% read.csv
stargazer(tab1.df, 
          type = 'html',
          summary = F,
          rownames = F,
          covariate.labels = c('Zone ranked by distance to centre (low to high)',
                       'Cumulative prop. of poor',
                       'Cumulative prop. of non-poor'),
          column.labels = 1:3 %>% as.character,
          out = 'Table 1.html'
          )

##  Appendix table
res.tab <- 'Working analysis files/Duncan index results table.csv' %>% read.csv
res.tab_out <- 
  res.tab %>% 
  mutate(TTWA = TTWA11NM,
         'Total pop (18 - 65, 2011)' = total.pop,
         'RCI (2001)' = rci01 %>% round(3),
         'RCI (2011)' = rci11 %>% round(3),
         'Change in RCI' = rcidiff %>% round(3),
         'RAE (2001)' = work01_exp %>% round(3),
         'RAE (2011)' = work11_exp %>% round(3),
         'Change in RAE' = workdiff_exp %>% round(3),
         'Geo. (2001)' = geo04 %>% round(3),
         'Geo. (2011)' = geo10 %>% round(3),
         'Change in Geo.' = geodiff %>% round(3),
         'Poverty indicator' = type     
         ) %>%
  dplyr::select(TTWA, 
                'Total pop (18 - 65, 2011)',
                'RCI (2001)',
                'RCI (2011)',
                'Change in RCI',
                'RAE (2001)',
                'RAE (2011)',
                'Change in RAE',
                'Geo. (2001)',
                'Geo. (2011)',
                'Change in Geo.',
                'Poverty indicator')

res.tab_out %>% write.csv('Saved paper outputs/All results.csv', 
                          row.names = F)
