#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("gdata")
library(shinydashboard)

# Read csv for
mydata = read.csv(paste(gsub("Lab9","",getwd()),"/importacionesVehiculosSAT.csv", sep=""))

ui <- dashboardPage(
    dashboardHeader(title = "Importaciones Vehiculos SAT"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Valor de Carros", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Carros por año", tabName = "widgets1", icon = icon("th")),
            menuItem("Valor e Impuestos", tabName = "widgets2", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                        box(plotOutput("plot1", height = 250)),
                        sidebarPanel(
                            sliderInput("bins",
                                        "Number of bins:",
                                        min = 1,
                                        max = 50,
                                        value = 30)
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "widgets1",
                    fluidRow(
                        box(plotOutput("plot2", height = 250)),
                        
                        box(plotOutput("plot3", height = 250))
                    )
            ),
            
            # Third tab content
            tabItem(tabName = "widgets2",
                    fluidRow(
                        box(plotOutput("plot4", height = 250)),
                        
                        box(plotOutput("plot5", height = 250))
                    )
            )
        )
    )
)

server <- function(input, output) {
    output$plot1 <- renderPlot({
        x <- mydata["Valor.CIF"][,1]
        x <- x[x > 100000 & x < 500000]
        bins <- seq(100000, 500000, length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'lightsteelblue', border = 'white', 
             main = "Valor de carros", xlab = "Valor $", ylab = "Cantidad")
    })
    
    output$plot2 <- renderPlot({
        x    <- mydata["Anio"][,1]
        bins <- seq(2011, 2019, length.out = 10)
        
        hist(x, breaks = bins, col = 'lightsteelblue', border = 'white',
             main = "Cantidad de carros por año", 
             xlab = "Año", ylab = "Cantidad")
    })
    
    output$plot3 <- renderPlot({
        x    <- mydata["Mes"][,1]
        bins <- seq(1, 12, length.out = 12)
        
        hist(x, breaks = bins, col = 'lightsteelblue', border = 'white',
             main = "Cantidad de carros al mes", 
             xlab = "Mes", ylab = "Cantidad")
    })
    
    output$plot4 <- renderPlot({
        x <- mydata["Valor.CIF"][,1]
        x <- x[x > 100000 & x < 500000]
        bins <- seq(100000, 500000, length.out = 10)
        
        hist(x, breaks = bins, col = 'lightsteelblue', border = 'white',
             main = "Valor CIF vs Impuesto", 
             xlab = "Valor CIF", ylab = "Cantidad")
    })
    
    output$plot5 <- renderPlot({
        x    <- mydata["Impuesto"][,1]
        x <- x[x > 2874 & x < 70625]
        bins <- seq(2874, 70625, length.out = 10)
        hist(x, breaks = bins, col = 'lightsteelblue', border = 'white',
             main = "Impuesto pagado", 
             xlab = "Impuesto", ylab = "Cantidad")
    })
}

shinyApp(ui, server)
# Run the application 
shinyApp(ui = ui, server = server)
