#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(plotly)
library(stringr)
library(ggplot2)
library(plyr)
library(lubridate)
library(sentimentr)
library(dplyr)

pacman::p_load(sentimentr, dplyr, magrittr)

c25 <- c("dodgerblue2","#E31A1C", # red
         "green4",
         "#6A3D9A", # purple
         "#FF7F00", # orange
         "black","gold1",
         "skyblue2","#FB9A99", # lt pink
         "palegreen2",
         "#CAB2D6", # lt purple
         "#FDBF6F", # lt orange
         "gray70", "khaki2",
         "maroon","orchid1","deeppink1","blue1","steelblue4",
         "darkturquoise","green1","yellow4","yellow3",
         "darkorange4","brown")


#Set Working Directore
#setwd("/Users/mebner/Documents/for_me/R_coursera/GitHub/data-products/seli_app/sentiment")

#get data
df_list <- lapply(Sys.glob("*.csv"), read.csv)
df <- ldply(df_list, data.frame)



raw <- df



#segments
raw$customer_type <- ifelse(str_replace_all(raw$How.often.do.you.purchase.from.THE.anonymized., "[[:punct:]]", " ") == 'I haven t made a purchase yet','new',
                            ifelse(raw$How.often.do.you.purchase.from.THE.anonymized. == 'This is my first purchase','new',
                                   'existing'))


questions <- colnames(df %>% select(-Response.ID,-Time.Started,-Date.Submitted,-Comments,-Country,-City,-State.Region,-Postal))
questions <- as.data.frame(questions) %>% mutate(clean = str_replace_all(questions, "\\.+", " ")) %>% filter(row_number(questions) %in$ c(2,3,23,25,59,61,62)) 


#reshape data
subdf <- raw %>%
  subset(!is.na(For.a.chance.to.win..200.voucher..please.tell.us.in.100.words.or.less..what.type.of.advertising.you.d.like.to.see.from.us.and.how.we.can.improve.Â.)) %>%
  select(Response.ID,customer_type,Time.Started,City,For.a.chance.to.win..200.voucher..please.tell.us.in.100.words.or.less..what.type.of.advertising.you.d.like.to.see.from.us.and.how.we.can.improve.Â.) %>%
  mutate(month = round_date(as.Date(Time.Started),"month")) %>%
  select(month,customer_type,Response.ID,City,For.a.chance.to.win..200.voucher..please.tell.us.in.100.words.or.less..what.type.of.advertising.you.d.like.to.see.from.us.and.how.we.can.improve.Â.) %>%
  dplyr::rename(feedback = For.a.chance.to.win..200.voucher..please.tell.us.in.100.words.or.less..what.type.of.advertising.you.d.like.to.see.from.us.and.how.we.can.improve.Â.) %>%
  mutate(feedback = as.character(feedback),month = as.character(month))

#keywords
#word_list <- data.frame(variable = subdf$feedback,stringsAsFactors =F ) %>%
#  mutate(variable = str_replace_all(variable, "[[:punct:]]", " ")) %>%
#  mutate(variable = str_split(variable,' ')) %>%
#  unnest() %>% subset(grepl("[A-Za-z]", variable)) %>%
#  mutate(variable = tolower(variable)) %>%
#  mutate(var_length = nchar(variable)) %>%
#  subset(var_length > 2)
#
#word_list <- as.data.frame(table(word_list$variable))
#word_list$perc <- round(word_list$Freq / sum(word_list$Freq),4)

#attribution

subdf$social <- ifelse(grepl("soci|facebook|facbook|faceb|influenc|instagram|pinterest|snapc|qzone|weibo|twitter|reddit|flickr|linkedin|whtasapp|vlogger|tweet|tumblr|post|blog", subdf$feedback),1,0)
subdf$search <- ifelse(grepl("adwo|search|organic|yahoo|seo|google|yahoo|link", subdf$feedback),1,0)
subdf$crm <- ifelse(grepl("mail|notifi|mial|news.letter", subdf$feedback),1,0)
subdf$display <- ifelse(grepl("banner|display|add", subdf$feedback),1,0)
subdf$promo <- ifelse(grepl("price|bargain|cheap|expensive|deal|sale|discount|vouch|promo|offer", subdf$feedback),1,0)
subdf$shipping <- ifelse(grepl("ship|deliv", subdf$feedback),1,0)
subdf$presentation <- ifelse(grepl("pictur|photo|photo|models|video|graph|virtual|visual|diversit",subdf$feedback),1,0)
subdf$features <- ifelse(grepl("watchlist|web|page|recommend|perso.nal|wishlist|inspir|interact|scroll|lookbook",subdf$feedback),1,0)
subdf$loyalty <- ifelse(grepl("vip|loya|rewar|progra",subdf$feedback),1,0)
subdf$assortment <- ifelse(grepl("brand|adidas|nike|billabong|birkenstock|birdsnest|tommi|tomboy|reebok|ralph|lauren|oxygen|assort|baby|babie|basics|basketball|baskets|beach|black|blazer|waterproof|uniform|trainer|tracksuit|toddler|anynomizedmen|anynomizedsports|shirt|jeans|swim|sunglass|summer|racewear|race|petit|outerwear|outdoor|styles|outfit|tailored|variety|accessor|colour|categories|festival|everywhere",subdf$feedback),1,0)

sent <- with(subdf,sentiment_by(get_sentences(feedback),list(Response.ID)))

out <- merge(subdf,sent,by = "Response.ID")


social <- out %>% subset(social == 1) %>% mutate(topic = 'social', topic_n = paste0('social (',nrow(out %>% subset(social == 1)),')'))
search <- out %>% subset(search == 1) %>% mutate(topic = 'search', topic_n = paste0('search (',nrow(out %>% subset(search == 1)),')'))
crm <- out %>% subset(crm == 1) %>% mutate(topic = 'crm', topic_n = paste0('crm (',nrow(out %>% subset(crm == 1)),')'))
display  <- out %>% subset(display == 1) %>% mutate(topic = 'display', topic_n = paste0('display (',nrow(out %>% subset(display == 1)),')'))
promo  <- out %>% subset(promo == 1) %>% mutate(topic = 'promo', topic_n = paste0('promo (',nrow(out %>% subset(promo == 1)),')'))
shipping  <- out %>% subset(shipping == 1) %>% mutate(topic = 'shipping', topic_n = paste0('shipping (',nrow(out %>% subset(shipping == 1)),')'))
presentation  <- out %>% subset(presentation == 1) %>% mutate(topic = 'presentation', topic_n = paste0('presentation (',nrow(out %>% subset(presentation == 1)),')'))
features  <- out %>% subset(features == 1) %>% mutate(topic = 'features', topic_n = paste0('features (',nrow(out %>% subset(features == 1)),')'))
assortment  <- out %>% subset(assortment == 1) %>% mutate(topic = 'assortment', topic_n = paste0('assortment (',nrow(out %>% subset(assortment == 1)),')'))
loyalty  <- out %>% subset(loyalty == 1) %>% mutate(topic = 'loyalty', topic_n = paste0('loyalty (',nrow(out %>% subset(loyalty == 1)),')'))


final <- rbind(social,search,crm,display,promo,shipping,presentation,features,loyalty,assortment)



freq <- as.data.frame(table(final$topic)) %>% dplyr::rename(topic = Var1)
freq$prop <- freq$Freq / nrow(out) * 100

freq_c <- as.data.frame(table(final$customer_type,final$topic)) %>% dplyr::rename(customer_type = Var1,topic = Var2)
total_c <- as.data.frame(table(out$customer_type)) %>% dplyr::rename(customer_type = Var1,total = Freq)
freq_c <- merge(freq_c,total_c,by = c("customer_type"))
freq_c$prop <- freq_c$Freq / freq_c$total * 100

freq_m <- as.data.frame(table(final$month,final$topic)) %>% dplyr::rename(month = Var1,topic = Var2)
total_m <- as.data.frame(table(out$month)) %>% dplyr::rename(month = Var1,total = Freq)
freq_m <- merge(freq_m,total_m,by = c("month"))
freq_m$prop <- freq_m$Freq / freq_m$total * 100

freq_m_c <- as.data.frame(table(final$month,final$customer_type,final$topic)) %>% dplyr::rename(month = Var1,topic = Var3,customer_type = Var2)
total_m_c <- as.data.frame(table(out$customer_type,out$month)) %>% dplyr::rename(month = Var2,total = Freq,customer_type = Var1)
freq_m_c <- merge(freq_m_c,total_m_c,by = c("month","customer_type"))
freq_m_c$prop <- freq_m_c$Freq / freq_m_c$total * 100



# Define UI for application that draws a histogram
ui <-
  navbarPage("Survey Overview",
             tabPanel("Frequencies",
                    fluidPage(    
                      
                      # Give the page a title
                      titlePanel("Frequencies"),
                      
                      # Generate a row with a sidebar
                      sidebarLayout(      
                        
                        # Define the sidebar with one input
                        sidebarPanel(
                          selectInput("view",
                                      "Totals or Percentages",
                                      c("Percentages",
                                        "Totals")),
                          selectInput("monthly_view",
                                            "Time Series:",
                                            c("off",
                                              "on")),
                          selectInput("customer_type_split",
                                            "Split data into customer types:",
                                            c("off",
                                              "on"))
          
                                                    ),
        
                                      # Create a spot for the barplot
                                      mainPanel(
                                        plotOutput("freq_plot", width = "100%", height = "400px")
                                              )
                                          )
                                      )
                    ),
                    tabPanel("Sentiment",
                             fluidPage(    
                               
                               # Give the page a title
                               titlePanel("Sentiment"),
                               
                               # Generate a row with a sidebar
                               sidebarLayout(      
                                 
                                 # Define the sidebar with one input
                                 sidebarPanel(
                                   selectInput('topics_sent',
                                               'Select a topic',
                                               c('overall', 'all topics', unique(final$topic)), selectize=FALSE),
                                   selectInput("monthly_view_sent",
                                               "Time Series:",
                                               c("off",
                                                 "on")),
                                   selectInput("customer_type_split_sent",
                                               "Split data into customer types:",
                                               c("off",
                                                 "on"))
                                   
                                 ),
                                 
                                 # Create a spot for the barplot
                                 mainPanel(
                                   plotOutput("sent_plot", width = "100%", height = "400px")
                                 )
                               )
                             )
                    ),
             tabPanel("Documentation",
                fluidPage(
                  h1("Documentation"),
                  h3("About this version of the dashboard"),
                  HTML(
                      "<p> This dashboards presents the results of a online survye conducted by an US-american fashion online store.</p>
                       <p> The survey is conducted continously and new data will be added over time.</p>
                       <p> This version of the dashboard is specially build for the peer-graded Coursera assignment and does not allow users to upload new data. The final version of the dashboard, however, will allow users to upload new data sets.</p>
                       <p> The data has been anynomized in order to protect the client and the customers. </p>
                       <p></p>"
                       ),
                  h3("How to use this dashboard"),
                  HTML(
                      "<p> Users may select 2 different views on the data: 'Frequencies' and 'Sentiment'</p>
                       <p> The 'Frequencies' answers the question how often have certain topics been mentioned.</p>
                       <p> The 'Sentiment' gives you an idea of the sentiment per topic or overall. The higher the sentiment score the better the sentiment.</p>
                       <p> Simply switch between the views using the tabs on top.</p>
                       <p></p>
                       <p> Within the views you may pick the topic of choice and split it into customer types and/or add a time dimension (month) by using the drop down menues</p>
                      "
                      )
                )
             )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$freq_plot <- renderPlot({
      # generate bins based on input$bins from ui.R
     view <- input$view
     customer_type_split <- input$customer_type_split
     monthly_view <- input$monthly_view
     
     
     if (view == 'Totals' & customer_type_split == 'on' & monthly_view == 'on') {
       ggplot(freq_m_c, aes(x = month, y = Freq,group = topic, color = topic)) + 
         geom_line()+
         geom_point() +
         #scale_y_continuous(breaks = seq(0,9999999, by=250))+
         facet_grid(customer_type~.,scales = "free")+
         scale_color_manual(values=c25)+
         labs(title = 'Number of Mentions by Topic',
              y = '# mentions',
              x = ' ')+
         theme_classic(base_size = 15)
     } else if (view == 'Percentages' & customer_type_split == 'on' & monthly_view == 'on') {
       ggplot(freq_m_c, aes(x = month, y = prop,group = topic, color = topic)) + 
         geom_line()+
         geom_point() +
         scale_y_continuous(breaks = seq(0,100, by=10), limits = c(-0,100))+
         facet_grid(customer_type~.,scales = "free")+
         scale_color_manual(values=c25)+
         labs(title = 'Proportino of Mentions by Topic',
              y = '% mentions',
              x = ' ')+
         theme_classic(base_size = 15)
     } else if (view == 'Totals' & customer_type_split == 'off' & monthly_view == 'on') {
       ggplot(freq_m, aes(x = month, y = Freq,group = topic, color = topic)) + 
         geom_line()+
         geom_point() +
         #scale_y_continuous(breaks = seq(0,9999999, by=250))+
         #scale_y_continuous(breaks = seq(0,100, by=10), limits = c(-0,100))+
         #facet_grid(customer_type~.,scales = "free")+
         scale_color_manual(values=c25)+
         labs(title = 'Proportino of Mentions by Topic',
              y = '% mentions',
              x = ' ')+
         theme_classic(base_size = 15)
     } else if (view == 'Percentages' & customer_type_split == 'off' & monthly_view == 'on') {
       ggplot(freq_m, aes(x = month, y = prop,group = topic, color = topic)) + 
         geom_line()+
         geom_point() +
         scale_y_continuous(breaks = seq(0,100, by=10), limits = c(-0,100))+
         #facet_grid(customer_type~.,scales = "free")+
         scale_color_manual(values=c25)+
         labs(title = 'Proportino of Mentions by Topic',
              y = '% mentions',
              x = ' ')+
         theme_classic(base_size = 15)
     } else if (view == 'Totals' & customer_type_split == 'on' & monthly_view == 'off') {
       ggplot(freq_c, aes(x = reorder(topic,-Freq,FUN=sum), weight = Freq, fill = topic)) + 
         geom_bar()+
         geom_text(aes(label = Freq,y = Freq), size = 4,position=position_dodge(width=0.9), vjust=-0.25)+
         #scale_y_continuous(breaks = seq(-2,2, by=.10), limits = c(-1.25,2.25))+
         #scale_y_continuous(breaks = seq(0,9999999, by=250))+
         facet_grid(customer_type~.,scales = "free")+
         scale_fill_manual(values=c25)+
         labs(title = 'Number of Mentions by Topic',
              y = '# mentions',
              x = ' ')+
         theme_classic(base_size = 15)
     } else if (view == 'Totals' & customer_type_split == 'off' & monthly_view == 'off') {
       ggplot(freq, aes(x = reorder(topic,-Freq,FUN=sum), weight = Freq, fill = topic)) + 
         geom_bar()+
         geom_text(aes(label = Freq,y = Freq), size = 4,position=position_dodge(width=0.9), vjust=-0.25)+
         #scale_y_continuous(breaks = seq(0,9999999, by=250))+
         #scale_y_continuous(breaks = seq(-2,2, by=.10), limits = c(-1.25,2.25))+
         #facet_grid(customer_type~.,scales = "free")+
         scale_fill_manual(values=c25)+
         labs(title = 'Number of Mentions by Topic',
              y = '# mentions',
              x = ' ')+
         theme_classic(base_size = 15)
     } else if (view == 'Percentages' & customer_type_split == 'off' & monthly_view == 'off') {
       ggplot(freq, aes(x = reorder(topic,-prop,FUN=sum), weight = prop, fill = topic)) + 
         geom_bar()+
         geom_text(aes(label = paste0(round(prop,1),'%'),y = prop), size = 4,position=position_dodge(width=0.9), vjust=-0.25)+
         #scale_y_continuous(breaks = seq(-2,2, by=.10), limits = c(-1.25,2.25))+
         #facet_grid(customer_type~.,scales = "free")+
         scale_y_continuous(breaks = seq(0,100, by=10), limits = c(-0,100))+
         scale_fill_manual(values=c25)+
         labs(title = 'Number of Mentions by Topic',
              y = '% mentions',
              x = ' ')+
         theme_classic(base_size = 15)
     } else if (view == 'Percentages' & customer_type_split == 'on' & monthly_view == 'off') {
       ggplot(freq_c, aes(x = reorder(topic,prop,FUN=sum), weight = prop, fill = topic)) + 
         geom_bar()+
         geom_text(aes(label = paste0(round(prop,1),'%'),y = prop), size = 4,position=position_dodge(width=0.9), vjust=-0.25)+
         #scale_y_continuous(breaks = seq(-2,2, by=.10), limits = c(-1.25,2.25))+
         scale_y_continuous(breaks = seq(0,100, by=10), limits = c(-0,100))+
         facet_grid(customer_type~.,scales = "free")+
         scale_fill_manual(values=c25)+
         labs(title = 'Proportion of Mentions by Topic',
              y = '% mentions',
              x = ' ')+
         theme_classic(base_size = 15)
     }
   })
   
   output$sent_plot <- renderPlot({
     # generate bins based on input$bins from ui.R
     topic_selection <- input$topics_sent
     customer_type_split <- input$customer_type_split_sent
     monthly_view <- input$monthly_view_sent
     
     if (topic_selection != 'overall' & topic_selection != 'all topics' & customer_type_split == 'on' & monthly_view == 'on') {
       tab <- final %>% subset(topic == topic_selection)
       ylim <- boxplot.stats(final$ave_sentiment)$stats[c(1,5)]
       ggplot(tab, aes(x=month,y=ave_sentiment,color=topic)) +
         geom_jitter(position=position_jitter(width=.1, height=0),size = .1, color = "grey")+
         geom_boxplot(alpha = .01,show.legend=F)+
         facet_grid(customer_type~.,scales = "free")+
         scale_y_continuous(breaks = seq(-2,2, by=.10), limits = ylim)+
         scale_color_manual(values=c25)+
         labs(title = paste0('sentiment on ',topic_selection),
              y = 'sentiment score',
              x = ' ',
              caption = 'outliers removed')+
         theme_classic(base_size = 15)
     } else if(topic_selection != 'overall' & topic_selection != 'all topics' & customer_type_split == 'on' & monthly_view == 'off') {
       tab <- final %>% subset(topic == topic_selection)
       ylim <- boxplot.stats(final$ave_sentiment)$stats[c(1,5)]
       ggplot(tab, aes(x='total',y=ave_sentiment,color=topic)) +
         geom_jitter(position=position_jitter(width=.1, height=0),size = .1, color = "grey")+
         geom_boxplot(alpha = .01,show.legend=F)+
         facet_grid(.~customer_type,scales = "free")+
         scale_y_continuous(breaks = seq(-2,2, by=.10), limits = ylim)+
         scale_color_manual(values=c25)+
         labs(title = paste0('sentiment on ',topic_selection),
              y = 'sentiment score',
              x = ' ',
              caption = 'outliers removed')+
         theme_classic(base_size = 15)
     } else if(topic_selection != 'overall' & topic_selection != 'all topics' & customer_type_split == 'off' & monthly_view == 'on') {
       tab <- final %>% subset(topic == topic_selection)
       ylim <- boxplot.stats(final$ave_sentiment)$stats[c(1,5)]
       ggplot(tab, aes(x=month,y=ave_sentiment,color=topic)) +
         geom_jitter(position=position_jitter(width=.1, height=0),size = .1, color = "grey")+
         geom_boxplot(alpha = .01,show.legend=F)+
         #facet_grid(customer_type~.,scales = "free")+
         scale_y_continuous(breaks = seq(-2,2, by=.10), limits = ylim)+
         scale_color_manual(values=c25)+
         labs(title = paste0('sentiment on ',topic_selection),
              y = 'sentiment score',
              x = ' ',
              caption = 'outliers removed')+
         theme_classic(base_size = 15)
     } else if(topic_selection != 'overall' & topic_selection != 'all topics' & customer_type_split == 'off' & monthly_view == 'off') {
       tab <- final %>% subset(topic == topic_selection)
       ylim <- boxplot.stats(final$ave_sentiment)$stats[c(1,5)]
       ggplot(tab, aes(x='total',y=ave_sentiment,color=topic)) +
         geom_jitter(position=position_jitter(width=.1, height=0),size = .1, color = "grey")+
         geom_boxplot(alpha = .01,show.legend=F)+
         #facet_grid(customer_type~.,scales = "free")+
         scale_y_continuous(breaks = seq(-2,2, by=.10), limits = ylim)+
         scale_color_manual(values=c25)+
         labs(title = paste0('sentiment on ',topic_selection),
              y = 'sentiment score',
              x = ' ',
              caption = 'outliers removed')+
         theme_classic(base_size = 15)
     } else if (topic_selection == 'overall' & customer_type_split == 'on' & monthly_view == 'on') {
       tab <- out
       ylim <- boxplot.stats(final$ave_sentiment)$stats[c(1,5)]
       ggplot(tab, aes(x=month,y=ave_sentiment,color=c25[1])) +
         geom_jitter(position=position_jitter(width=.1, height=0),size = .1, color = "grey")+
         geom_boxplot(alpha = .01,show.legend=F)+
         facet_grid(customer_type~.,scales = "free")+
         scale_y_continuous(breaks = seq(-2,2, by=.10), limits = ylim)+
         scale_color_manual(values=c25)+
         labs(title = 'overall sentiment',
              y = 'sentiment score',
              x = ' ',
              caption = 'outliers removed')+
         theme_classic(base_size = 15)
     } else if(topic_selection == 'overall' & customer_type_split == 'on' & monthly_view == 'off') {
       tab <- out
       ylim <- boxplot.stats(final$ave_sentiment)$stats[c(1,5)]
       ggplot(tab, aes(x='total',y=ave_sentiment,color=c25[1])) +
         geom_jitter(position=position_jitter(width=.1, height=0),size = .1, color = "grey")+
         geom_boxplot(alpha = .01,show.legend=F)+
         facet_grid(.~customer_type,scales = "free")+
         scale_y_continuous(breaks = seq(-2,2, by=.10), limits = ylim)+
         scale_color_manual(values=c25)+
         labs(title = 'overall sentiment',
              y = 'sentiment score',
              x = ' ',
              caption = 'outliers removed')+
         theme_classic(base_size = 15)
     } else if(topic_selection == 'overall' & customer_type_split == 'off' & monthly_view == 'on') {
       tab <- out
       ylim <- boxplot.stats(final$ave_sentiment)$stats[c(1,5)]
       ggplot(tab, aes(x=month,y=ave_sentiment,color=c25[1])) +
         geom_jitter(position=position_jitter(width=.1, height=0),size = .1, color = "grey")+
         geom_boxplot(alpha = .01,show.legend=F)+
         #facet_grid(customer_type~.,scales = "free")+
         scale_y_continuous(breaks = seq(-2,2, by=.10), limits = ylim)+
         scale_color_manual(values=c25)+
         labs(title = 'overall sentiment',
              y = 'sentiment score',
              x = ' ',
              caption = 'outliers removed')+
         theme_classic(base_size = 15)
     } else if(topic_selection == 'overall' & customer_type_split == 'off' & monthly_view == 'off') {
       tab <- final
       ylim <- boxplot.stats(final$ave_sentiment)$stats[c(1,5)]
       ggplot(tab, aes(x='total',y=ave_sentiment,color=c25[1])) +
         geom_jitter(position=position_jitter(width=.1, height=0),size = .1, color = "grey")+
         geom_boxplot(alpha = .01,show.legend=F)+
         #facet_grid(customer_type~.,scales = "free")+
         scale_y_continuous(breaks = seq(-2,2, by=.10), limits = ylim)+
         scale_color_manual(values=c25)+
         labs(title = 'overall sentiment',
              y = 'sentiment score',
              x = ' ',
              caption = 'outliers removed')+
         theme_classic(base_size = 15)
     } else if (topic_selection == 'all topics' & customer_type_split == 'on' & monthly_view == 'on') {
       tab <- final
       ylim <- boxplot.stats(final$ave_sentiment)$stats[c(1,5)]
       ggplot(tab, aes(x=month,y=ave_sentiment,color=topic)) +
         geom_boxplot(alpha = .01)+
         facet_grid(customer_type~.,scales = "free")+
         scale_y_continuous(breaks = seq(-2,2, by=.10), limits = ylim)+
         scale_color_manual(values=c25)+
         labs(title = paste0('sentiment on ',topic_selection),
              y = 'sentiment score',
              x = ' ',
              caption = 'outliers removed')+
         theme_classic(base_size = 15)
     } else if(topic_selection == 'all topics' & customer_type_split == 'on' & monthly_view == 'off') {
       tab <- final
       ylim <- boxplot.stats(final$ave_sentiment)$stats[c(1,5)]
       ggplot(tab, aes(x=topic,y=ave_sentiment,color=topic)) +
         geom_jitter(position=position_jitter(width=.1, height=0),size = .1, color = 'grey')+
         geom_boxplot(alpha = .01)+
         facet_grid(.~customer_type,scales = "free")+
         scale_y_continuous(breaks = seq(-2,2, by=.10), limits = ylim)+
         scale_color_manual(values=c25)+
         labs(title = paste0('sentiment on ',topic_selection),
              y = 'sentiment score',
              x = ' ',
              caption = 'outliers removed')+
         theme_classic(base_size = 15)
     } else if(topic_selection == 'all topics'& customer_type_split == 'off' & monthly_view == 'on') {
       tab <- final
       ylim <- boxplot.stats(final$ave_sentiment)$stats[c(1,5)]
       ggplot(tab, aes(x=month,y=ave_sentiment,color=topic)) +
         geom_boxplot(alpha = .01)+
         #facet_grid(customer_type~.,scales = "free")+
         scale_y_continuous(breaks = seq(-2,2, by=.10), limits = ylim)+
         scale_color_manual(values=c25)+
         labs(title = paste0('sentiment on ',topic_selection),
              y = 'sentiment score',
              x = ' ',
              caption = 'outliers removed')+
         theme_classic(base_size = 15)
     } else if(topic_selection == 'all topics' & customer_type_split == 'off' & monthly_view == 'off') {
       tab <- final
       ylim <- boxplot.stats(final$ave_sentiment)$stats[c(1,5)]
       ggplot(tab, aes(x=topic,y=ave_sentiment,color=topic)) +
         geom_jitter(position=position_jitter(width=.1, height=0),size = .1,color = 'grey')+
         geom_boxplot(alpha = .01)+
         #facet_grid(customer_type~.,scales = "free")+
         scale_y_continuous(breaks = seq(-2,2, by=.10), limits = ylim)+
         scale_color_manual(values=c25)+
         labs(title = paste0('sentiment on ',topic_selection),
              y = 'sentiment score',
              x = ' ',
              caption = 'outliers removed')+
         theme_classic(base_size = 15)
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

