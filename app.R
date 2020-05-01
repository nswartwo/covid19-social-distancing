
library(shiny)
library(shinythemes)
library(shinyjs)
library(markdown)
library(gridExtra)

source("functions.R")
source("theme.R")
# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("simplex"),
                useShinyjs(),
   
   # Application title
   titlePanel("Design Your Own Intermittent Lockdown Cycle"),
   includeMarkdown("content/intro.md"), 
   hr(),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        tabsetPanel(
        tabPanel("Social Distancing Characteristics", fluid=TRUE,
         sliderInput("weeks_lock",
                     "Number of Weeks in Strict Social Distancing:",
                     min = 1,
                     max = 20,
                     value =6),
      # Sidebar with a slider input for number of bins 
          sliderInput("weeks_free",
                      "Number of Weeks in Low Social Distancing:",
                      min = 1,
                      max = 20,
                      value = 6),
      sliderInput("num_locks",
                  "Number of Lockdown Periods:",
                  min = 1,
                  max =20,
                  value = 6)
          ), 
      tabPanel("Infection Characteristics", fluid=TRUE,
        # Sidebar with a slider input for number of bins 
            sliderInput("R0",
                        "R0 during strict social distancing:",
                        min = 0,
                        max = 10,
                        value = 2), 
            sliderInput("Reff",
                        "R0 during low social distancing:",
                        min = 0,
                        max = 10,
                        value =.32), 
            # sliderInput("inc.days",
            #             "Incubation Days:",
            #             min = 0,
            #             max = 10,
            #             value = 1), 
            # sliderInput("inf.days",
            #             "R0 during free periods:",
            #             min = 0,
            #             max = 10,
            #             value = 1), 
            sliderInput("initial_pop",
                        "Population Size",
                        min = 1,
                        max = 1000000000,
                        value = 1000000),
            sliderInput("initial_infected",
                        "How Many Individuals Infected at t=0",
                        min = 1,
                        max = 100000,
                        value = 1000)
      )),
      width=4),
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("cyclePlots"),
         br(),
         br(),
         plotOutput("costPlot")
      )
   ),
   hr(),
   includeMarkdown("content/footer.md")

 )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$cyclePlots <- renderPlot({
      # generate bins based on input$bins from ui.R
     results<-cycle_function(
       weeksdown = input$weeks_lock, 
       weeksup = input$weeks_free,
       Rup = input$R0, 
       Rdown = input$Reff, 
       periods = input$num_locks,
       initialinfrate =input$initial_infected/input$initial_pop
     )
     alpha<-c(0,1,2)
     #create our utility df
     
    utility_df<-data.frame("alpha"=alpha, U=results[[2]]^alpha)
      
      cspec_pal<-c("#1c133c", "#0089b8", "#be1622", "#ffffff")
     
      # draw the histogram with the specified number of bins
     plot1<-ggplot(results[[1]], aes(x=week, y=infected))+geom_line(size=1.5, col=cspec_pal[2]) +
       ggtitle("lockdown cycle")+ scale_y_log10()+ geom_hline(yintercept=input$initial_infected/input$initial_pop,  color = "gold", size=1.5)+
       labs(y="Number of Infected Individuals", x="Number of Weeks",
            title="Time Trend of Infected Individuals Under Lockdowns") +cspec_theme_shiny() 
     print(plot1)
   })
   
   output$costPlot <- renderPlot({
     results<-cycle_function(
       weeksdown = input$weeks_lock,
       weeksup = input$weeks_free,
       Rup = input$R0,
       Rdown = input$Reff,
       periods = input$num_locks,
       initialinfrate =input$initial_infected/input$initial_pop
     )

     cspec_pal<-c("#1c133c", "#0089b8", "#be1622", "#ffffff")

     plot2<-ggplot(results[[2]])+geom_line(mapping= aes(x=alpha, y=U),size=1.5, col=cspec_pal[2]) +
       ggtitle("lockdown cycle")+geom_line(mapping=aes(x=alpha, y=steadyU),size=1.5, color="gold") +
       labs(y="Utility U(c)=c^alpha", x="alpha",
            title="Utility of Designed Lockdown for a Range of Alphas") +cspec_theme_shiny()
      
     print(plot2)

      })
     
}

# Run the application 
shinyApp(ui = ui, server = server)

