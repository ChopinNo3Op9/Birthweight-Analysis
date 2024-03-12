library(shiny)
library(quantreg)
library(shinydashboard)

load('natality_processed.RData')

lm1 <- lm(weight ~ ., data = d)
summary(lm1)

Y <- d$weight
X <- as.matrix(d[,-1])  # assigns all columns of dataset d except the first one

tau <- seq(0.05, 0.95, 0.05)

# The i'th column of a coefficient matrix is
# the quantile coefficients obtained at
# tau[i] quantile.
rqcoef <- function(X, Y, tau) {
  n <- nrow(X); p <- ncol(X)
  res <- matrix(NA, p+1, length(tau))
  for (i in 1:length(tau)) {
    temp <- try(rq(Y ~ as.matrix(X), tau = tau[i], 
                   method = 'fn')$coefficients, T)
    if (length(temp) == 1) {
      res[,i] <- rq(Y ~ as.matrix(X), tau = tau[i])$coefficients
    } else {
      res[,i] <- temp
    }
  }
  return(res)
}


# coef<- rqcoef(d[,-1], d[,1], tau)

## Task for today
# plot the quantile regression coeffcients
# for the two regressions: one with weight~everything, the other with weight~everthing except race

# 1st model
lm1 <- lm(weight ~ ., data = d)
summary(lm1)

# Fit quantile regression models
fit <- rq(
    weight ~ ., 
    tau = seq(0.05, 0.95, by = 0.05), 
    data = d
)
# Extract coefficients
coef_values <- coef(fit)
write.csv(coef_values,"rq_coef1.csv")

# 2nd model
fit_except_race <- rq(
    weight ~ . - black, 
    tau = seq(0.05, 0.95, by = 0.05), 
    data = d
)
coef2 <- coef(fit_except_race)
write.csv(coef2,"rq_coef2.csv")

# Extract fitted values
# fitted_all <- fitted(model_all)
# fitted_no_race <- fitted(model_no_race)

# incorporate into shiny app
# build a second tab for the shiny app, comparing the fitted values (predicted values) from the quantile regression, 
# e.g. x-axis: fitted values from one prediction, y-axis fitted values from the other prediction
# plot for a certain tau, say tau=0.5
# include a slider for all tau from 0.05,...,0.95
# Define UI for Shiny app
name = c(
    "intercept",
    "boy",
    "married",
    "mother's age",
    "mother's age^2",
    "high school",
    "some college",
    "college",
    "no prenatal",
    "prenatal second",
    "prenatal third",
    "smoker",
    "cigarettes / day",
    "mother's weight gain",
    "mother's weight gain^2",
    "black"
)

coef_values <- read.csv("rq_coef1.csv")
coef_values <- coef_values[,-1]

coef_values_except_race <- read.csv("rq_coef2.csv")
coef_values_except_race <- coef_values_except_race[,-1]

plotcoef <- function(plotthis, name, coef_values) {
    i <- which(name == plotthis)
    if(length(i) == 0) return()  # In case of no match
    a <- coef_values[i,]

    # Plot coefficient values
    plot(
        seq(0.05, 0.95, length.out = ncol(coef_values)), 
        a, 
        type = "l", 
        xlab = "Quantile level",
        ylab = "Quantile regression coefficient",
        main = paste("Coefficient plot for", plotthis)
    )
}

ui <- dashboardPage(
  dashboardHeader(title = "Quantile Regression Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Coefficient1", tabName = "coefficient1", icon = icon("chart-line")),
      menuItem("Coefficient2", tabName = "coefficient2", icon = icon("chart-line")),
      menuItem("Comparison fitted values", tabName = "comparison", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "coefficient1",
              fluidRow(
                box(selectInput('id1', 'Quantile Coefficients weight~everything', choices = name)),
                box(plotOutput('coef1'), width = 12)
              )
      ),
      tabItem(tabName = "coefficient2",
              fluidRow(
                box(selectInput('id2', 'Quantile Coefficients weight~everthing except race', choices = name)),
                box(plotOutput('coef2'), width = 12)
              )
      ),
      tabItem(tabName = "comparison",
        fluidRow(
          box(title = "Compare Fitted Values", status = "primary", solidHeader = TRUE, 
              sliderInput("id3", "Select τ (tau):", min = 0.05, max = 0.95, value = 0.5, step = 0.5),
              plotOutput("comparePlot"))
              )
      )
  )
)
)

new_data1 <- as.data.frame(X)
new_data2 <- new_data1[, -which(names(new_data1) == "black")]

server <- function(input, output) {
  output$coef1 <- renderPlot({
    plotcoef(input$id1, name, coef_values)
    # print('testing')
  })
  output$coef2 <- renderPlot({
    plotcoef(input$id2, name, coef_values_except_race)
    # print('testing')
  })
  output$comparePlot <- renderPlot({
    # Predictions for each model
    # Calculate the first row separately and then apply the rest
    coeff1 <- coef_values[, which(colnames(coef_values) == paste0("tau..", input$id3))]
    coeff2 <- coef_values_except_race[, which(colnames(coef_values_except_race) == paste0("tau..", input$id3))]
    
    fitted1 <- apply(coef_values, 1, function(coeff1) coeff1[1] + sum(coeff1[-1] * t(new_data1)))  # 1: row-wise, 2: column-wise
    fitted2 <- apply(coef_values_except_race, 1, function(coeff2) coeff2[1] + sum(coeff2[-1] * t(new_data2)))
    print(fitted1)
    print(length(fitted1))
    print(fitted2)
    print(length(fitted2))

    # Plotting the comparison
    plot(fitted1, fitted2, xlab = "Fitted Values from Model 1", ylab = "Fitted Values from Model 2",
         main = paste("Comparison of Fitted Values at τ =", input$tau))
    abline(0, 1)  # Adds a 45-degree line for reference
  })
}

shinyApp(ui, server)

# Look at the prediction, think about
# 1. why are the coefficients for the "married" covariate be lower if race is included in the quantile regression?
# 2. in the predicted values, why are the predictions for # black mothers higher?
# 3. why are the scatter points divided into 2 clusters? 

