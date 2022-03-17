# Attempt to to make my histogram of citations from the ground up

library(shiny)
require(dplyr)
require(ggplot2)

#loading the data that is needed for the shiny app
data <- read.csv("data.csv")

#giving the shiny app the same theme as the rest of my visualizations
#theme_thesis <- theme_classic()

# 
ui <- fluidPage(
    #title
    titlePanel("Histogram of citation numbers"),
    
    #adding blank space for aesthetics
    br(),
    
    fluidRow(
        column(5,
               align = 'center',
               selectInput(inputId = 'year_start',
                           label = NULL,
                           choices = seq(1970, 2022, 1), 
                           selected = 1970)),
        column(2, style = 'margin-top: 7px', align = 'center', p("to")),
        column(5, align = 'center',
               selectInput(inputId = 'year_end',
                           label = NULL,
                           choices = seq(1934, 2022, 1), 
                           selected = 2022))
    ),
    
    #adding a break between date range and subtitle
    br(),
    
    #adding description to the 
    h4("Histogram of the number of citations recieved by EV items"),
    
    br(),

    #without placing it in the "main panel" it automatically centers better
    plotOutput(outputId = "histogram.citations", height = "400px")
)


# 
server <- function(input, output, session) ({
    
    #making a reactive database that will select citation information only for the years selected in the iu
    filtered.data <- reactive({
    data %>% select(year, cited.by) %>% filter(year >= input$year_start & year <= input$year_end)
    })
    
    #making the histogram
    output$histogram.citations <- renderPlot({
        filtered.data() %>%
            ggplot(aes(x=cited.by)) +
            geom_histogram() +
            labs(x = "Number of citations", 
                 y = "Number of articles") +
            theme_classic() +
            theme(axis.title=element_text(size=20), axis.text=element_text(size=12))
    })
})

# Run the application 
shinyApp(ui = ui, server = server)
