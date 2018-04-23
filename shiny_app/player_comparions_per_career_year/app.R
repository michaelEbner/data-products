#rm(list = ls())
library(ggplot2)
library(dplyr)
library(bbr)
library(htmltab)
library(stringr)
library(shiny)

setwd("/Users/mebner/Documents/for_me/R_coursera/GitHub/shiny_app/player_comparions_per_career_year/")

df_players = data.frame()
for (i in  letters){
  
  # vector output
  #players <- try(get_players(i))
  players <-  htmltab(doc = paste0("https://www.basketball-reference.com/players/",i), which = 1, header = 1,rm_nodata_cols = F)
  df_players <- rbind(df_players,players)
}

players <- df_players %>% mutate(from = as.numeric(from), to = as.numeric(to)) %>% mutate(years_active = to - from) %>% filter(!is.na(from) & from >= 1980 & years_active >= 5)
players$name <- paste0(sub("[a-zA-Z\\.\\'\\-]+ ", "", players$player)," ",sub(" [a-zA-Z\\.\\'\\-]+", "", players$player))

slugs <- unlist(as.list(players$slug))

df = data.frame()
for (i in  slugs){
  
  # vector output
  initials <- paste0(substr(i,1,1),"/")
  player_stats <- htmltab(doc = paste0("https://www.basketball-reference.com/players/",initials,i,".html"), which = 1, header = 1,rm_nodata_cols = F)
  df_players <- rbind(df,player_stats)
}

runApp(list(
  ui = fluidPage(
    selectInput("player1", "Select a player",
              list(`A` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^A.*")))),use.names = F),
                   `B` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^B.*")))),use.names = F))
    ),
    verbatimTextOutput('bar')
  ),
server = function(input, output) {
  df_player1 <- reactive({
    slug <- subset(players,name == "Bryant Kobe") %>% select(slug)
    initial <- paste0(substr(as.character(slug[1,1]), 1, 1),"/")
    df <- htmltab(doc = paste0("https://www.basketball-reference.com/players/",initial,as.character(slug[1,1]),".html"), which = 1, header = 1,rm_nodata_cols = F)
    return(df)
    })
  
  output$table1 <- renderTable(df_player1())
                           
  }
))



