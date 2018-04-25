#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  titlePanel("Player Comparison by Carreer Year"),
  fluidRow(
    column(3, wellPanel(
      selectizeInput("player1","Select a Player",unlist(as.list(players %>% select(player)),use.names=F),multiple=F,options=NULL),
      selectizeInput("player2","Select a Player",unlist(as.list(players %>% select(player)),use.names=F),multiple=F,options=NULL),
      submitButton("Submit")
    ))),
  column(6,
         tableOutput("table1")
  )
)