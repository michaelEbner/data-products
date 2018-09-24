#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- 
  fluidPage(    
    
    # Give the page a title
    titlePanel("Telephones by region"),
    
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
        plotOutput("freq_plot", width = "100%", height = "400px",)
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
         #scale_y_continuous(breaks = seq(-2,2, by=.25), limits = c(-1.25,2.25))+
         facet_grid(customer_type~.,scales = "free")+
         scale_fill_manual(values=c25)+
         labs(title = 'Number of Mentions by Topic',
              y = '# mentions',
              x = ' ')+
         theme_dark(base_size = 20)
     } else if (view == 'Percentages' & customer_type_split == 'on' & monthly_view == 'on') {
       ggplot(freq_m_c, aes(x = month, y = prop,group = topic, color = topic)) + 
         geom_line()+
         geom_point() +
         scale_y_continuous(breaks = seq(0,100, by=10), limits = c(-0,100))+
         facet_grid(customer_type~.,scales = "free")+
         scale_fill_manual(values=c25)+
         labs(title = 'Proportino of Mentions by Topic',
              y = '% mentions',
              x = ' ')+
         theme_dark(base_size = 20)
     } else if (view == 'Totals' & customer_type_split == 'off' & monthly_view == 'on') {
       ggplot(freq_m, aes(x = month, y = Freq,group = topic, color = topic)) + 
         geom_line()+
         geom_point() +
         #scale_y_continuous(breaks = seq(0,100, by=10), limits = c(-0,100))+
         #facet_grid(customer_type~.,scales = "free")+
         scale_fill_manual(values=c25)+
         labs(title = 'Proportino of Mentions by Topic',
              y = '% mentions',
              x = ' ')+
         theme_dark(base_size = 20)
     } else if (view == 'Percentages' & customer_type_split == 'off' & monthly_view == 'on') {
       ggplot(freq_m, aes(x = month, y = prop,group = topic, color = topic)) + 
         geom_line()+
         geom_point() +
         scale_y_continuous(breaks = seq(0,100, by=10), limits = c(-0,100))+
         #facet_grid(customer_type~.,scales = "free")+
         scale_fill_manual(values=c25)+
         labs(title = 'Proportino of Mentions by Topic',
              y = '% mentions',
              x = ' ')+
         theme_dark(base_size = 20)
     } else if (view == 'Totals' & customer_type_split == 'on' & monthly_view == 'off') {
       ggplot(freq_c, aes(x = reorder(topic,-Freq,FUN=sum), weight = Freq, fill = topic)) + 
         geom_bar()+
         geom_text(aes(label = Freq,y = Freq), size = 4,position=position_dodge(width=0.9), vjust=-0.25)+
         #scale_y_continuous(breaks = seq(-2,2, by=.25), limits = c(-1.25,2.25))+
         facet_grid(customer_type~.,scales = "free")+
         scale_fill_manual(values=c25)+
         labs(title = 'Number of Mentions by Topic',
              y = '# mentions',
              x = ' ')+
         theme_dark(base_size = 20)
     } else if (view == 'Totals' & customer_type_split == 'off' & monthly_view == 'off') {
       ggplot(freq, aes(x = reorder(topic,-Freq,FUN=sum), weight = Freq, fill = topic)) + 
         geom_bar()+
         geom_text(aes(label = Freq,y = Freq), size = 4,position=position_dodge(width=0.9), vjust=-0.25)+
         #scale_y_continuous(breaks = seq(-2,2, by=.25), limits = c(-1.25,2.25))+
         #facet_grid(customer_type~.,scales = "free")+
         scale_fill_manual(values=c25)+
         labs(title = 'Number of Mentions by Topic',
              y = '# mentions',
              x = ' ')+
         theme_dark(base_size = 20)
     } else if (view == 'Percentages' & customer_type_split == 'off' & monthly_view == 'off') {
       ggplot(freq, aes(x = reorder(topic,-prop,FUN=sum), weight = prop, fill = topic)) + 
         geom_bar()+
         geom_text(aes(label = paste0(round(prop,1),'%'),y = prop), size = 4,position=position_dodge(width=0.9), vjust=-0.25)+
         #scale_y_continuous(breaks = seq(-2,2, by=.25), limits = c(-1.25,2.25))+
         #facet_grid(customer_type~.,scales = "free")+
         scale_y_continuous(breaks = seq(0,100, by=10), limits = c(-0,100))+
         scale_fill_manual(values=c25)+
         labs(title = 'Number of Mentions by Topic',
              y = '% mentions',
              x = ' ')+
         theme_dark(base_size = 20)
     } else if (view == 'Percentages' & customer_type_split == 'on' & monthly_view == 'off') {
       ggplot(freq_c, aes(x = reorder(topic,prop,FUN=sum), weight = prop, fill = topic)) + 
         geom_bar()+
         coord_flip()+
         geom_text(aes(label = paste0(round(prop,1),'%'),y = prop), size = 4,position=position_dodge(width=0.9), vjust=-0.25)+
         #scale_y_continuous(breaks = seq(-2,2, by=.25), limits = c(-1.25,2.25))+
         scale_y_continuous(breaks = seq(0,100, by=10), limits = c(-0,100))+
         facet_grid(customer_type~.,scales = "free")+
         scale_fill_manual(values=c25)+
         labs(title = 'Proportion of Mentions by Topic',
              y = '% mentions',
              x = ' ')+
         theme_dark(base_size = 20)
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

