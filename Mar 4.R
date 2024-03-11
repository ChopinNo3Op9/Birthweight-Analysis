library(shiny)

name = c("intercept", "boy", "married", "mother's age", "mother's age^2", "high school", "some college", "college", "no prenatal", "prenatal second", "prenatal third", "smoker", "cigarettes / day", "mother's weight gain", "mother's weight gain^2", "black")

plotcoef = function(plotthis) {
  plot(rnorm(30), rnorm(30))
  # replace this plot with the coefficient plot for the different variables
  # x-axis: quantile level (0.05, 0.01,... 0.95)
  # y-axis: quantile regression coefficient for the variable selected.
}

ui <- navbarPage("Visual exploration of the birthweight dataset",
                 tabPanel("Coefficients", 
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(
                                inputId = 'thisvar',
                                label = 'Plot coefficients for:',
                                choices = name
                              )
                            ),
                            mainPanel(
                              plotOutput('coef')
                            )
                          )
                 ),
                 fluid = T)

server <- function(input, output) {
  output$coef <- renderPlot(
    plotcoef(input$thisvar)
  )
}

shinyApp(ui = ui, server = server)

