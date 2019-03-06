# libraries
library(shiny)
library(ggplot2)
library(Cairo)
library(tidyverse)

# Global variables can go here
n <- 200

phd_field <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv") %>% 
  mutate(year = as.integer(year))

# summarized by broad field
phd2 <- phd_field %>%
  group_by(broad_field, year) %>% 
  summarize(n = sum(n_phds, na.rm = TRUE))

# summarized by broad and major field for second plot
phd3 <- phd_field %>%
  group_by(broad_field, major_field, year) %>% 
  summarize(n = sum(n_phds, na.rm = TRUE))

# Define UI ----
ui <- fluidPage(
  navlistPanel(
    tabPanel("tab 1: Links and Shiny Template",
             h1("QuantfishR Shiny Workshop"),
             h4("How To:"),
             p(strong("RStudio Shiny Cheatsheet:"), 
               a(href="https://shiny.rstudio.com/articles/cheatsheet.html",
                 "Cheatsheet link")),
             p("RStudio Shiny app webpage:", 
               a(href="https://shiny.rstudio.com/",
                 "https://shiny.rstudio.com/")),
             hr(),
             h4("Data Sets:"),
             p("Data source, R4DS Tidy Tuesday:",
               a(href="https://github.com/rfordatascience/tidytuesday",
                 "Tidy Tuesday link")),
             hr(),
             h4("Example Shiny Apps:"),
             p("Gallery of example Shiny applications. They come with code!",
               a(href="https://shiny.rstudio.com/gallery/",
                 "Shiny App Gallery")),
             p("Simple Shiny histogram example:",
               a(href="https://shiny.rstudio.com/gallery/single-file-shiny-app.html",
                 "histogram link"),
               "Tab 2"),
             p("Shiny Interactive Plot example:",
               a(href="https://shiny.rstudio.com/gallery/plot-interaction-basic.html",
                 "Interactive Plot link"),
               "Tab 3"),
             p("Jonathan's Town Meeting Tax App:",
               a(href="https://jonathancummings.shinyapps.io/NottinghamTaxCalculator/",
                 "Link")),
             p("Jonathan's Management Strategy Evaluation Review App:",
               a(href="https://jonathancummings.shinyapps.io/MSEreview/",
                 "Link")),
             hr(),
             p("Shiny Template:"),
             p(code("library(shiny)")),
             p(code("ui <- fluidPage() # end fluidPage")),
             p(code("server <- function(input, output) {} # end server")),
             p(code("shinyApp(ui = ui, server = server)"))
             ), # end tabPanel
    tabPanel("tab 2: Histogram Example",
             h4("Example Interactives"),
             numericInput('n', 'Number of obs', n),
             plotOutput('plot')
             ), # end tabPanel
    tabPanel("tab 3: Plot Interactives Example",
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
    tabPanel("tab 4: Hart Fay Example",
             # Application title
             titlePanel("Awarded PhDs by Field"),
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 sliderInput("startyr",
                             "Starting year:",
                             min = min(phd2$year),
                             max = max(phd2$year),
                             value = mean(phd2$year),
                             step = 1,           # make step size of 1 year
                             sep = ""),          # get rid of thousand "," separator
                 selectInput("bfield",
                             "Select broad field",
                             choices = unique(phd3$broad_field))
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("distPlot"),
                 plotOutput("majorPlot")
               )
             )) # End TabPanel
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
  
  output$distPlot <- renderPlot({ # curly brackets make this 1 expression
    # plot we want
    ggplot(data=filter(phd2,year >= input$startyr),
           aes(x=year, y=n, color=broad_field, group=broad_field)) +
      geom_line() +
      ggtitle("Number of PhDs Awarded by Broad Field")
  })
  
  output$majorPlot <- renderPlot({
    ggplot(data=filter(phd3, broad_field == input$bfield),
           aes(x=year, y=n, color=major_field, group=major_field)) +
      geom_line() +
      ggtitle(paste("PhDs Awarded Within", input$bfield))
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