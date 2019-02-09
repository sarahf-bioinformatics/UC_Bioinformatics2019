#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Weights Data Graph"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("yvalues",
                     "Y-axis Choice",
                     choices = c(
                       "Percent body weight lost daily"="avg.daily",
                       "Percent body weight lost"="avgdailypercent",
                       "Days Survived at 0% RH"="days.survived"
                    ))
                     
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("waterPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$waterPlot <- renderPlot({
     water <-read.csv("water.csv",header=TRUE)
     watersum<-summary(water)
       
     
      # generate bins based on input$bins from ui.R
      if (input$yvalues=="avg.daily"){
        y=water$avg.daily
        ylabs="Percent body weight lost daily"
      }
     
     else if (input$yvalues=="avgdailypercent"){
       y=water$avgdailypercent
       ylabs="Percent body weight lost"
     }
     
     else {
       y=water$days.survived
       ylabs="Days Survived at 0% RH"
     }

     colors = rep("purple",42)
     colors[0:12] = "red"
     colors[13:22] = "green"
     colors[23:32] = "lightblue"
    
     #make ggplot2 object
     ggplot(water,aes(factor(status),y))+
       geom_bar(stat="identity", fill= colors)+
       geom_errorbar(aes(ymin=max(y)-sd(y), ymax=max(y)+sd(y))) +
       ylab(ylabs)+
       xlab("Reproductive Status")
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

