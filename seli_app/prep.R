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
library("Hmisc")
library(clValid)
library(jsonlite)
library(cluster)
library(factoextra)
library(magrittr)
library(broom)
library(ggplot2)
library(ggdendro)
library(corrplot)
library(cluster)
library(psych)
library(ggfortify)
library(caret)
library(zoo)
library(plyr)
library(xlsx)
require("pacman")
library(lubridate)
library(stringdist)
library(wordnet)
library(koRpus)

pacman::p_load(sentimentr, dplyr, magrittr)


#Set Working Directore
setwd("/Users/mebner/Documents/for_me/R_coursera/GitHub/data-products/seli_app")
#get data
df <- read.csv("20180830003631-SurveyExport - 20180830003631-SurveyExport.csv")

raw <- df


#reshape data
subdf <- raw %>%
  subset(!is.na(For.a.chance.to.win..200.voucher..please.tell.us.in.100.words.or.less..what.type.of.advertising.you.d.like.to.see.from.us.and.how.we.can.improve.Â.)) %>%
  select(Response.ID,Time.Started,City,For.a.chance.to.win..200.voucher..please.tell.us.in.100.words.or.less..what.type.of.advertising.you.d.like.to.see.from.us.and.how.we.can.improve.Â.) %>%
  mutate(month = round_date(as.Date(Time.Started),"month")) %>%
  select(month,Response.ID,City,For.a.chance.to.win..200.voucher..please.tell.us.in.100.words.or.less..what.type.of.advertising.you.d.like.to.see.from.us.and.how.we.can.improve.Â.) %>%
  dplyr::rename(feedback = For.a.chance.to.win..200.voucher..please.tell.us.in.100.words.or.less..what.type.of.advertising.you.d.like.to.see.from.us.and.how.we.can.improve.Â.) %>%
  mutate(feedback = as.character(feedback),month = as.character(month))

#keywords
word_list <- data.frame(variable = subdf$feedback,stringsAsFactors =F ) %>%
  mutate(variable = str_replace_all(variable, "[[:punct:]]", " ")) %>%
  mutate(variable = str_split(variable,' ')) %>%
  unnest() %>% subset(grepl("[A-Za-z]", variable)) %>%
  mutate(variable = tolower(variable)) %>%
  mutate(var_length = nchar(variable)) %>%
  subset(var_length > 2)

word_list <- as.data.frame(table(word_list$variable))
word_list$perc <- round(word_list$Freq / sum(word_list$Freq),4)

#attribution

subdf$social <- ifelse(grepl("soci|facebook|facbook|faceb|influenc|instagram|pinterest|snapc|qzone|weibo|twitter|reddit|flickr|linkedin|whtasapp|vlogger|tweet|tumblr|post|blog", subdf$feedback),1,0)
subdf$sea <- ifelse(grepl("adwo|search add|organic|yahoo", subdf$feedback),1,0)
subdf$seo <- ifelse(grepl("seo|google|organic|yahoo", subdf$feedback),1,0)
subdf$crm <- ifelse(grepl("newsletter|mail|notifi", subdf$feedback),1,0)
subdf$display <- ifelse(grepl("banner|display", subdf$feedback),1,0)
subdf$iconic <- ifelse(grepl("iconic", subdf$feedback),1,0)
subdf$pricing <- ifelse(grepl("price|bargain|cheap|expensive|deal|sale|discount|vouch", subdf$feedback),1,0)
subdf$shipping <- ifelse(grepl("ship", subdf$feedback),1,0)
subdf$presentation <- ifelse(grepl("pictur|photo|photo|models|video|graph|virtual|visual|diversit",subdf$feedback),1,0)
subdf$features <- ifelse(grepl("watchlist|web|page|recommend|perso.nal|shop|wishlist|inspir|interact|scroll|lookbook",subdf$feedback),1,0)
subdf$brands <- ifelse(grepl("adidas|nike|billabong|birkenstock|birdsnest|tommi|tomboy|reebok|ralph|lauren|oxygen",subdf$feedback),1,0)
subdf$assortment <- ifelse(grepl("assort|baby|babie|basics|basketball|baskets|beach|black|blazer|waterproof|uniform|trainer|tracksuit|toddler|theiconicmen|theiconicsports|shirt|jeans|swim|sunglass|summer|racewear|race|petit|outerwear|outdoor|styles|outfit|tailored|variety|accessor|colour|categories|festival|everywhere",subdf$feedback),1,0)

sent <- with(subdf,sentiment_by(get_sentences(feedback),list(Response.ID)))

out <- merge(subdf,sent,by = "Response.ID")


social <- out %>% subset(social == 1) %>% mutate(topic = paste0('social (',nrow(out %>% subset(social == 1)),')'))
sea <- out %>% subset(sea == 1) %>% mutate(topic = paste0('sea (',nrow(out %>% subset(sea == 1)),')'))
seo <- out %>% subset(seo == 1) %>% mutate(topic = paste0('seo (',nrow(out %>% subset(seo == 1)),')'))
crm <- out %>% subset(crm == 1) %>% mutate(topic = paste0('crm (',nrow(out %>% subset(crm == 1)),')'))
display  <- out %>% subset(display == 1) %>% mutate(topic = paste0('display (',nrow(out %>% subset(display == 1)),')'))
iconic <- out %>% subset(iconic == 1) %>% mutate(topic = paste0('iconic (',nrow(out %>% subset(iconic == 1)),')'))
pricing  <- out %>% subset(pricing == 1) %>% mutate(topic = paste0('pricing (',nrow(out %>% subset(pricing == 1)),')'))
shipping  <- out %>% subset(shipping == 1) %>% mutate(topic = paste0('shipping (',nrow(out %>% subset(shipping == 1)),')'))
presentation  <- out %>% subset(presentation == 1) %>% mutate(topic = paste0('presentation (',nrow(out %>% subset(presentation == 1)),')'))
features  <- out %>% subset(features == 1) %>% mutate(topic = paste0('features (',nrow(out %>% subset(features == 1)),')'))
brands  <- out %>% subset(brands == 1) %>% mutate(topic = paste0('brands (',nrow(out %>% subset(brands == 1)),')'))
assortment  <- out %>% subset(assortment == 1) %>% mutate(topic = paste0('assortment (',nrow(out %>% subset(assortment == 1)),')'))

final <- rbind(social,sea,seo,crm,display,iconic,pricing,shipping,presentation,features,brands,assortment)

ggplot(final, aes(x=reorder(topic,ave_sentiment,FUN= median),y=ave_sentiment,color=topic)) +
  geom_boxplot()+
  #facet_grid(connie_applink_active~.,scales = "free")+
  scale_y_continuous(breaks = seq(-2,2, by=.25), limits = c(-1.25,2.25))+
  #atlassian_theme +
  coord_flip() +
  labs(title = 'Sentiment by Topic',
       y = 'sentiment score',
       x = ' ')+
  theme_light()











