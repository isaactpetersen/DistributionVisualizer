#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library("shiny")
library("PearsonDS")

# Define UI for application
ui <- fluidPage(
  titlePanel("Distribution Visualizer"),
  sidebarLayout(
    sidebarPanel(
      
      numericInput(
        "mean",
        "Mean:",
        value = 100,
        width = "100px"),
      
      numericInput(
        "sd",
        "Standard Deviation:",
        value = 15,
        min = 1,
        width = "80px"),
      
      numericInput(
        "skew",
        "Skewness (approx.):",
        value = 0,
        step = 0.1,
        width = "80px"),
      
      numericInput(
        "kurt", # excess kurtosis
        "Kurtosis (approx.):",
        value = 0,
        step = 0.1,
        width = "80px"),
      
      width = 2
    ),
    mainPanel(
      plotOutput("distPlot"),
      verbatimTextOutput("cutoffs")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  rawKurtosis <- input$kurt + 3 # rawKurtosis = excessKurtosis + 3
  
  output$distPlot <- renderPlot({
    moments <- c(
      mean = input$mean,
      variance = input$sd^2,
      skewness = input$skew,
      kurtosis = input$kurt)
    
    # Try to simulate data; catch errors if moments are invalid
    x <- tryCatch({
      PearsonDS::rpearson(
        n = 1000000,
        moments = moments)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(x)) {
      plot.new()
      title(
        main = "Invalid combination of variance, skewness, and kurtosis.
        Please try a different combination.")
      return()
    }
    
    dens <- density(x)
    
    # Fixed x-range centered around mean, scaled by SD
    xlim <- c(
      input$mean - 4 * input$sd,
      input$mean + 4 * input$sd)
    
    # Fixed y-axis height to show shape changes clearly
    ylim <- c(0, 0.03)
    
    plot(
      dens,
      main = "Approximate Distribution (Density)",
      xlab = "Value",
      col = "steelblue",
      lwd = 2,
      xlim = xlim,
      ylim = ylim)
    
    # Add ±1, 2, 3 SD lines
    for (i in -3:3) {
      abline(
        v = input$mean + i * input$sd,
        col = ifelse(i == 0, "red", "gray"),
        lty = ifelse(i == 0, 1, 2))
    }
  })
  
  output$cutoffs <- renderPrint({
    cutoffs <- input$mean + input$sd * (-3:3)
    names(cutoffs) <- paste0(c(-3:3), " SD")
    cutoffs
  })
}

# Run the application 
shinyApp(
  ui = ui,
  server = server)
