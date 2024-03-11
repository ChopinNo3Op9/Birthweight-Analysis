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
  dashboardHeader(title = "Visual exploration of the birthweight dataset"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Coefficients", tabName = "coefficients", icon = icon("chart-line")),
      menuItem("Coefficients2", tabName = "coefficients2", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "coefficients",
              fluidRow(
                box(selectInput('id1', 'Plot coefficients for:', choices = name)),
                box(plotOutput('coef1'), width = 12)
              )
      ),
      tabItem(tabName = "coefficients2",
              fluidRow(
                box(selectInput('id2', 'Plot coefficients for:', choices = name)),
                box(plotOutput('coef2'), width = 12)
              )
      )
    )
  )
)

server <- function(input, output) {
  output$coef1 <- renderPlot({
    plotcoef(input$id1, name, coef_values)
  })
  output$coef2 <- renderPlot({
    plotcoef(input$id2, name, coef_values_except_race)
  })
}

shinyApp(ui = ui, server = server)


# Look at the prediction, think about
# 1. why are the coefficients for the "married" covariate be lower if race is included in the quantile regression?
# 2. in the predicted values, why are the predictions for # black mothers higher?
# 3. why are the scatter points divided into 2 clusters? 


