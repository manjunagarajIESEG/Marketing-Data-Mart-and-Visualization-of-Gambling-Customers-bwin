#---------------------------------------------------------------------------------------#
# Install Shiny Package
#---------------------------------------------------------------------------------------#
if(!require("shiny")) install.packages("shiny"); library("shiny")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")

ui <- fluidPage(titlePanel
          ("Poker Chip bought Vs Sold"),
          sidebarLayout(
            sidebarPanel(
              selectInput("country_type","select a country",choices = unique(final_table$CountryName)),
              selectInput("gender_type","select a Gender",choices = unique(final_table$Gender))),
              
            mainPanel(plotOutput("barchart"))))

server <- function(input,output){
  
  temp <- reactive ({
    req(input$gender_type)
    req(input$continent_type)
    final_table %>% 
    filter(Gender %in% input$gender_type & CountryName %in% input$country_type) 
  
  })
  
  output$barchart <- renderPlot({
    
      ggplot(data=temp(), aes(x=temp$Loyalty)) +
      geom_bar(stat="identity", fill="steelblue")+
      theme_minimal()
  })
  
  
}

shinyApp(ui = ui, server = server)
