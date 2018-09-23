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

pageWithSidebar(
  headerPanel("Player Comparison by Carreer Year and Age"),
  sidebarPanel(
    selectizeInput("player1","Select a Player",unlist(as.list(players %>% select(player)),use.names=F),multiple=F,options=NULL),
    selectizeInput("player2","Select another Player",unlist(as.list(players %>% select(player)),use.names=F),multiple=F,options=NULL),
    actionButton("goButton", "Go!")
  ),
  mainPanel(
    plotOutput("plot", width = 400, height = 300)
  )
)

