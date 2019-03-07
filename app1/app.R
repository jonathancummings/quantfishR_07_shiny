library(shiny)
library(tidyverse)

phd_field <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")

glimpse(phd_field)

# Define UI for application that draws a histogram
ui <- fluidPage(


   # Application title
   titlePanel("NSF - PhDs Completed from 2008 to 2017"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         selectInput("major","Major",choices = phd_field$major_field)
      ),

      # Show a plot of the generated distribution
      mainPanel(
        fluidRow(column(width=4,   p("Hello World")),
                 column(width=6, p("Text2"))
        ),
        fluidRow(   p("Hello World1"),
                    br(),
                    hr(),
                    p("Text3"),
         plotOutput("barPlot")
      )
    ) # end mainPanel
  ) # end sidebarPanel
) # end fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$barPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x<-phd_field %>% 
        filter(major_field == input$major)

      # draw the histogram with the specified number of bins
      ggplot(x,aes(x=year,y=n_phds))+geom_col()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

