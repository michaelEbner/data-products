#rm(list = ls())
library(ggplot2)
library(dplyr)
library(bbr)
library(htmltab)
library(stringr)
library(shiny)

setwd("/Users/mebner/Documents/for_me/R_coursera/GitHub/data-products/shiny_app/player_comparions_per_career_year/")

df_players = data.frame()
for (i in  letters){
  
  # vector output
  players <- try(get_players(i))
  df_players <- rbind(df_players,players)
}

players <- df_players %>% mutate(from = as.numeric(from), to = as.numeric(to)) %>% mutate(years_active = to - from) %>% filter(!is.na(from) & from >= 1980 & years_active >= 5)
players$name <- paste0(sub("[a-zA-Z\\.\\'\\-]+ ", "", players$player)," ",sub(" [a-zA-Z\\.\\'\\-]+", "", players$player))

slugs <- unlist(as.list(players$slug))

player_stats = data.frame()
for (i in  slugs){
  # vector output
  initials <- paste0(substr(i,1,1),"/")
  selection <- htmltab(doc = paste0("https://www.basketball-reference.com/players/",initials,i,".html"), which = 1, header = 1,rm_nodata_cols = F)
  selection$slug <- paste0(i)
  player_stats <- rbind(player_stats,selection)
}

# "armstbj01"
# "Abdul-Rauf Mahmoud"

runApp(list(
  ui = fluidPage(
    selectInput("player1", "Select a player",
                list(`A` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^A.*")))),use.names = F),
                     `B` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^B.*")))),use.names = F),
                     `C` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^C.*")))),use.names = F),
                     `D` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^D.*")))),use.names = F),
                     `E` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^E.*")))),use.names = F),
                     `F` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^F.*")))),use.names = F),
                     `G` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^G.*")))),use.names = F),
                     `H` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^H.*")))),use.names = F),
                     `I` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^I.*")))),use.names = F),
                     `J` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^J.*")))),use.names = F),
                     `K` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^K.*")))),use.names = F),
                     `L` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^L.*")))),use.names = F),
                     `M` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^M.*")))),use.names = F),
                     `N` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^N.*")))),use.names = F),
                     `O` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^O.*")))),use.names = F),
                     `P` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^P.*")))),use.names = F),
                     `Q` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^Q.*")))),use.names = F),
                     `R` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^R.*")))),use.names = F),
                     `S` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^S.*")))),use.names = F),
                     `T` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^T.*")))),use.names = F),
                     `U` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^U.*")))),use.names = F),
                     `V` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^V.*")))),use.names = F),
                     `W` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^W.*")))),use.names = F),
                     `X` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^X.*")))),use.names = F),
                     `Y` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^Y.*")))),use.names = F),
                     `Z` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^Z.*")))),use.names = F))
    ),
    tableOutput('table1')
  ),
  server = function(input, output) {
    df_player1 <- reactive({
      selection <- players %>% subset(name == input$player1) %>% select(slug)
      initial <- paste0(substr(as.character(selection), 1, 1),"/")
      df <- htmltab(doc = paste0("https://www.basketball-reference.com/players/",initial,as.character(selection),".html"), which = 1, header = 1,rm_nodata_cols = F)
      df <- df %>% mutate()
      return(df)
    })
    
    output$table1 <- renderTable(df_player1())
    
  }
))


runApp(list(
  ui = fluidPage(
    selectInput("player1", "Select a player",
                list(`A` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^A.*")))),use.names = F),
                     `C` = unlist(as.list(players %>% select(name) %>% filter(!is.na(str_match(as.character(players$name),"^C.*")))),use.names = F))
    ),
    tableOutput('table1')
  ),
  server = function(input, output) {
    df_player1 <- reactive({
      selection <- players %>% subset(name == input$player1) %>% select(slug)
      df <- player_stats %>% subset(slug == as.character(selection)) %>% select(-slug)
      return(df)
    })
    
    output$table1 <- renderTable(df_player1())
    
  }
))
