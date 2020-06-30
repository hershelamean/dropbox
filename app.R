#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Load necessary packages
library(shiny)
library(tidyverse)
library(RCurl)
library(ggimage)
library(Lahman)
library(usmap)

#Lahman Data binding
Lahman::People -> Players
Lahman::Schools -> Schools
Lahman::CollegePlaying -> College
Lahman::AllstarFull -> Allstars
Lahman::Batting -> Batting
Lahman::AwardsPlayers -> Awards

Awards %>%
    left_join(College, by = c("playerID" = "playerID")) %>%
    left_join(Schools) %>%
    left_join(Players, by = c("playerID" = "playerID")) %>%
    filter(birthCountry == "USA", !is.na(birthState)) -> Awards_College_Schools

# Define UI ----
ui <- fluidPage(
    titlePanel("Major League Baseball App"),
    
    sidebarPanel(
        selectInput('award', h3("Choose an Award"), choices = unique(Awards_College_Schools$awardID)),
        sliderInput('year', h3("What years?"), min = 1877, max = 2017, value = 2017 )),
    
    mainPanel(
        textOutput('plot1'),
        plotOutput('plotState')
    )
)

# Define server logic ----
server <- function(input, output, session) {
    
    output$plot1 <- renderText({
        paste("You have selected", input$award, "from", input$year) 
    })
    
    output$plotState <- renderPlot({
        
        Awards_College_Schools_subset <- filter(Awards_College_Schools, awardID == input$award & 
                                                    yearID.x  <= input$year) 
        
        ggplot(Awards_College_Schools_subset, mapping = aes(x = birthState, fill = birthState)) +
            geom_bar() +
            coord_flip() 
        
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)
