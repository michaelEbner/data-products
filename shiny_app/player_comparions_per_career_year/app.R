#rm(list = ls())
library(ggplot2)
library(dplyr)
library(bbr)
library(htmltab)
library(stringr)
library(shiny)

setwd("/Users/mebner/Documents/for_me/R_coursera/GitHub/data-products/shiny_app/player_comparions_per_career_year/")

#df_players = data.frame()
#for (i in  letters){

 #vector output
  #players <- try(get_players(i))
  #df_players <- rbind(df_players,players)
#}

#players <- df_players %>% mutate(from = as.numeric(from), to = as.numeric(to)) %>% mutate(years_active = to - from) %>% filter(!is.na(from) & from >= 1980 & years_active >= 5)
#players$name <- paste0(sub("[a-zA-Z\\.\\'\\-]+ ", "", players$player)," ",sub(" [a-zA-Z\\.\\'\\-]+", "", players$player))
players <- read.csv('df_players.csv')
slugs <- unlist(as.list(players$slug))

#player_stats = data.frame()
#for (i in  slugs){
# vector output
#  initials <- paste0(substr(i,1,1),"/")
#  selection <- htmltab(doc = paste0("https://www.basketball-reference.com/players/",initials,i,".html"), which = 1, header = 1,rm_nodata_cols = F)
#  selection$slug <- paste0(i)
#  player_stats <- rbind(player_stats,selection)
#}

# "armstbj01"
# "Abdul-Rauf Mahmoud"

runApp(list(
  ui = fluidPage(
    selectInput("player1", "Select a player",
                #list(`Players` = c('John Doe','Peter Gynn','Jolie Hope'))
                list(`Players` = unlist(as.list(players %>% select(player)),use.names = F))
                #    `A` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^A.*")))),use.names = F),
                #    `B` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^B.*")))),use.names = F),
                #    `C` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^C.*")))),use.names = F),
                #    `D` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^D.*")))),use.names = F),
                #    `E` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^E.*")))),use.names = F),
                #    `F` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^F.*")))),use.names = F),
                #    `G` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^G.*")))),use.names = F),
                #    `H` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^H.*")))),use.names = F),
                #    `I` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^I.*")))),use.names = F),
                #    `J` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^J.*")))),use.names = F),
                #    `K` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^K.*")))),use.names = F),
                #    `L` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^L.*")))),use.names = F),
                #    `M` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^M.*")))),use.names = F),
                #    `N` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^N.*")))),use.names = F),
                #    `O` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^O.*")))),use.names = F),
                #    `P` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^P.*")))),use.names = F),
                #    `Q` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^Q.*")))),use.names = F),
                #    `R` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^R.*")))),use.names = F),
                #    `S` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^S.*")))),use.names = F),
                #    `T` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^T.*")))),use.names = F),
                #    `U` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^U.*")))),use.names = F),
                #    `V` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^V.*")))),use.names = F),
                #    `W` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^W.*")))),use.names = F),
                #    `X` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^X.*")))),use.names = F),
                #    `Y` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^Y.*")))),use.names = F),
                #    `Z` = unlist(as.list(players %>% select(player) %>% filter(!is.na(str_match(as.character(players$player),"^Z.*")))),use.names = F))
    ),
    tableOutput('table1')
  ),
  server = function(input, output) {

    df_player1 <- reactive({
      selection <- players %>% subset(player == input$player1) %>% select(slug)
      initial <- paste0(substr(as.character(selection), 1, 1),"/")
      df <- htmltab(doc = paste0("https://www.basketball-reference.com/players/",initial,as.character(selection),".html"), which = 1, header = 1,rm_nodata_cols = F)
      df <- df %>% mutate()
      return(df)
    })
    
    output$table1 <- renderTable(df_player1())
    
  }
))


