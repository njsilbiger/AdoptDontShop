
library(shiny)
library(shinythemes)


# Load stuff here ---------------------------------------------------------




# ui ----------------------------------------------------------------------
ui <- fluidPage(theme = shinytheme("superhero"),

    # Application title
    titlePanel("Adopt, Don't Shop"),

       sidebarLayout(
           sidebarPanel(
               # Button to generate new kitkats
               actionButton("go", "Refresh tha kitties!", icon("heart", lib = "glyphicon","fa-2x"),
                            style="color: #fff; background-color: #f46d43; border-color: #f46d43")
           ),
           
       mainPanel(
            tabsetPanel(
                tabPanel("Home", 
                         plotOutput("plot1")
                         ), 
                tabPanel("Names",
                         plotOutput("plot2")
                         ), 
                tabPanel("Anything else?",
                         plotOutput("plot3"))
                )
)))


# server logic ------------------------------------------------------------

server <- function(input, output) {

    output$plot1 <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = 10 + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$plot2 <- renderPlot({
        plot(1:10,1:10)
    })
    
    output$plot3 <- renderPlot({
        plot(1:10,1:10)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
