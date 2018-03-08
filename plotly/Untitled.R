library(tidyverse)
library(plotly)
library(ggplot2)
library(lubridate)
library(rmarkdown)
library(knitr)
library(tidyverse)
library(vegan)
library(stringr)
library(RJDBC)
library(RColorBrewer)
library(doBy)
library(sqldf)
library(gridExtra)
library(reshape)
library(stringi)
library(clValid)
library(jsonlite)
library(cluster)
library(factoextra)
library(magrittr)
library(broom)
library(ggplot2)
library(ggdendro)
library(cluster)
library(psych)
source('/Users/mebner/src/product_analytics/R_Common/query_atl_db.R')
source('/Users/mebner/src/product_analytics/R_Common/basic_functions.R')
source('/Users/mebner/src/product_analytics/R_Common/evaluate_statistical_test.R')
source('/Users/mebner/src/product_analytics/R_Common/atlassian_theme.R')


setwd("/Users/mebner/Documents/for_me/R_coursera/GitHub/data-products/plotly")
df <- read.csv("2017_season.sql")



tab <- df %>% filter(FGA < 99)
xvar = tab$Age
xtitle = "Age in Years"
yvar = tab$eFG.
col = tab$Pos
ytitle = "Effective Field Goal Percentage"
title = paste0(xtitle," VS ",ytitle)

ylim1 = boxplot.stats(yvar)$stats[c(1, 5)]
xlim1 = boxplot.stats(xvar)$stats[c(1, 5)]
p<- ggplot(data=tab, aes(x=xvar,y=yvar)) +
  geom_smooth()+
  geom_jitter(size=1.0,height = 0.1,width = 0.25,aes(colour = col,alpha = G),size = 1)+
  labs(title = title,
       y = ytitle,
       x = xtitle)+
  coord_cartesian(ylim = ylim1*1.05,
                  xlim = xlim1*1.05,
                  caption = paset0("n=",nrow(tab),",\n2017-2018 season days\nIncludes only Players at least 100 Field Goal Attempts"))+
  scale_color_manual(values=c("#8777D9","#FF5630","#36B37E","#FFAB00","#00B8D9","#0065FF","#403294","#BF2600","#006644","#FF8B00",
                              "#008DA6","#0049B0","#C0B6F2","#FF8F73","#79F2C0","#FFE380","#79E2F2","#4C9AFF"))+
  atlassian_theme

ggplotly(p)

