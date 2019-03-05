# libraries
library(shiny)
library(ggplot2)
library(Cairo)
library(tidyverse)

# Global variables can go here
n <- 200

phd_field <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")


# Define UI ----
ui <- fluidPage(
  navlistPanel(
    tabPanel("tab 1: Links and Shiny Template",
             h1("QuantfishR Shiny Workshop"),
             p(strong("RStudio Shiny Cheatsheet:"), 
               a(href="https://shiny.rstudio.com/articles/cheatsheet.html",
                 "Cheatsheet link")),
             p("RStudio Shiny app webpage:", 
               a(href="https://shiny.rstudio.com/",
                 "https://shiny.rstudio.com/")),
             hr(),
             p("Data source, R4DS Tidy Tuesday:",
               a(href="https://github.com/rfordatascience/tidytuesday",
                 "Tidy Tuesday link")),
             hr(),
             p("Gallery of example Shiny applications. They come with code!",
               a(href="https://shiny.rstudio.com/gallery/",
                 "Shiny App Gallery")),
             p("Simple Shiny histogram example:",
               a(href="https://shiny.rstudio.com/gallery/single-file-shiny-app.html",
                 "histogram link"),
               "Tab 1"),
             p("Shiny Interactive Plot example:",
               a(href="https://shiny.rstudio.com/gallery/plot-interaction-basic.html",
                 "Interactive Plot link"),
               "Tab 2"),
             hr(),
             p("Shiny Template:"),
             p(code("library(shiny)")),
             p(code("ui <- fluidPage() # end fluidPage")),
             p(code("server <- function(input, output) {} # end server")),
             p(code("shinyApp(ui = ui, server = server)")),
             hr(),
             h4("Example Interactives"),
             numericInput('n', 'Number of obs', n),
             plotOutput('plot')
             ), # end tabPanel
    tabPanel("tab 2",
             # Some custom CSS for a smaller font for preformatted text
             tags$head(
               tags$style(HTML("
                               pre, table.table {
                               font-size: smaller;
                               }
                               "))
               ),
             
             fluidRow(
               column(width = 4, wellPanel(
                 radioButtons("plot_type", "Plot type",
                              c("base", "ggplot2")
                 )
               )),
               column(width = 4,
                      # In a plotOutput, passing values for click, dblclick, hover, or brush
                      # will enable those interactions.
                      plotOutput("plot1", height = 350,
                                 # Equivalent to: click = clickOpts(id = "plot_click")
                                 click = "plot_click",
                                 dblclick = dblclickOpts(
                                   id = "plot_dblclick"
                                 ),
                                 hover = hoverOpts(
                                   id = "plot_hover"
                                 ),
                                 brush = brushOpts(
                                   id = "plot_brush"
                                 )
                      )
               )
             ),
             fluidRow(
               column(width = 3,
                      verbatimTextOutput("click_info")
               ),
               column(width = 3,
                      verbatimTextOutput("dblclick_info")
               ),
               column(width = 3,
                      verbatimTextOutput("hover_info")
               ),
               column(width = 3,
                      verbatimTextOutput("brush_info")
               )
             )), # end tabPanel
    tabPanel("tab 3",
             p(code("phd_field <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv')"))
             )
  )# end navlistPanel
)# end fluidpage UI

# Define server logic ----
server <- function(input, output) {
  output$plot <- renderPlot({
    hist(runif(input$n))
  })
  output$plot1 <- renderPlot({
    if (input$plot_type == "base") {
      plot(mtcars$wt, mtcars$mpg)
    } else if (input$plot_type == "ggplot2") {
      ggplot(mtcars, aes(wt, mpg)) + geom_point()
    }
  })
  
  output$click_info <- renderPrint({
    cat("input$plot_click:\n")
    str(input$plot_click)
  })
  output$hover_info <- renderPrint({
    cat("input$plot_hover:\n")
    str(input$plot_hover)
  })
  output$dblclick_info <- renderPrint({
    cat("input$plot_dblclick:\n")
    str(input$plot_dblclick)
  })
  output$brush_info <- renderPrint({
    cat("input$plot_brush:\n")
    str(input$plot_brush)
  })
  
  } # end server


shinyApp(ui = ui, server = server)

# ##### - Advanced Template - #####
# # import libraries
# library(shiny)
# 
# ##################
# # User Interface #
# ##################
# 
# #generic line initiating the UI
# ui <- shinyUI(fluidPage(
#   
#   #Add a title
#   titlePanel("Add Title Here"),
#   
#   #This creates a layout with a left sidebar and main section
#   sidebarLayout(
#     
#     #beginning of sidebar section
#     #usually includes inputs
#     sidebarPanel(),
#     
#     #beginning of main section
#     mainPanel()
#   )
#   
#   #Close the UI definition
# ))
# 
# ##########
# # SERVER #
# ##########
# 
# #generic line initiating the SERVER 
# 
# server <- shinyServer(function(input, output) {
#   
#   #########################
#   # Data load and cleanup #
#   #########################
#   
#   #Import data
#   
#   #Clean data
#   
#   #############
#   # Reactives #
#   #############
#   
#   # define any reactive elements of the app
#   
#   #Close the server definition
# })
# 
# 
# ##############
# # Launch App #
# ##############
# 
# #generic line that launches the app
# shinyApp(ui = ui, server = server)