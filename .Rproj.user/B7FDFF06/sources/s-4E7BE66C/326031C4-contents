#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
cspec_pal<-c("#1c133c", "#0089b8", "#be1622", "#ffffff")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Social Distancing Cycle Explorer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("weeks_lock",
                     "Number of Weeks in High Social Distancing:",
                     min = 1,
                     max = 20,
                     value =4),
      # Sidebar with a slider input for number of bins 
          sliderInput("weeks_free",
                      "Number of Weeks in Low Social Distancing:",
                      min = 1,
                      max = 20,
                      value = 8),
      sliderInput("num_locks",
                  "Number of Lockdowns:",
                  min = 1,
                  max =10,
                  value = 5),
        # Sidebar with a slider input for number of bins 
            sliderInput("R0",
                        "Doubling Time:",
                        min = 0,
                        max = 10,
                        value = 1), 
      sliderInput("initial_infected",
                  "How Many Individuals Infected at t=0",
                  min = 1,
                  max = 100000,
                  value = 1000)
  ),
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("cyclePlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$cyclePlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     infection_trend<-lockdown_cycle(
       initial_infect = input$initial_infected, 
       weeks_lock = input$weeks_lock, 
       weeks_free = input$weeks_free,
       first_lock_weeks = input$weeks_lock,
       total_population=1e6, 
       doubling_time = input$R0, 
       nlocks = input$num_locks
     )
     
      # draw the histogram with the specified number of bins
     ggplot(infection_trend, aes(x=week, y=infected))+geom_line(size=1.5, col=cspec_pal[2]) +
       ggtitle("lockdown cycle")+
       labs(y="Number of Infected Individuals", x="Number of Weeks",
            title="Time Trend of Infected Individuals Under Lockdowns") +cspec_theme_shiny()     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

