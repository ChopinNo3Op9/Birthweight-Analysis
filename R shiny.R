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
                box(selectInput('id1', 'Quantile Coefficients weight ~ everything', choices = name)),
                box(plotOutput('coef1'), width = 12)
              )
      ),
      tabItem(tabName = "coefficient2",
              fluidRow(
                box(selectInput('id2', 'Quantile Coefficients weight ~ everthing except race', choices = name)),
                box(plotOutput('coef2'), width = 12)
              )
      ),
      tabItem(tabName = "comparison",
        fluidRow(
          box(title = "Compare Fitted Values", status = "primary", solidHeader = TRUE, 
              sliderInput("id3", "Select τ (tau):", min = 0.05, max = 0.95, value = 0.5, step = 0.05),
              plotOutput("comparePlot"))
              )
      )
  )
)
)

new_data1 <- as.data.frame(X)
new_data2 <- new_data1[, -which(names(new_data1) == "black")]
black_1_indices <- which(new_data1$black == 1)
black_0_indices <- which(new_data1$black == 0)


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
    # print(paste0("tau..",  format(input$id3, nsmall = 2)))
    coeff1 <- coef_values[, which(colnames(coef_values) == paste0("tau..", format(input$id3, nsmall = 2)))]
    coeff2 <- coef_values_except_race[, which(colnames(coef_values_except_race) == paste0("tau..",  format(input$id3, nsmall = 2)))]
    # print(coeff1)
    # print(coeff2)
    
    fitted1 <- apply(new_data1, 1, function(new_data1) coeff1[1] + sum(coeff1[-1] * new_data1))  # 1: row-wise, 2: column-wise, each row in new_data1
    fitted2 <- apply(new_data2, 1, function(new_data2) coeff2[1] + sum(coeff2[-1] * new_data2))
    # print(fitted1)
    # print(length(fitted1))
    # print(fitted2)
    # print(length(fitted2))

    # Plotting the comparison
    plot(fitted1, fitted2, xlab = "Fitted Values from Model 1", ylab = "Fitted Values from Model 2",
         main = paste("Comparison of Fitted Values at τ =", format(input$id3, nsmall = 2)), col = "blue", pch = 19)
    
    # points(fitted1, col = "blue", pch = 19)
    # points(fitted2, col = "red", pch = 19)

    # Color points based on black_1_indices and black_0_indices
    points(fitted1[black_1_indices], fitted2[black_1_indices], col = "black", pch = 19)
    points(fitted1[black_0_indices], fitted2[black_0_indices], col = "grey", pch = 19)

    legend("topright", legend = c("Fitted Values (Black == 1)", "Fitted Values (Black == 0)"),
           col = c("black", "grey"), pch = 19)

    abline(0, 1)
  })
}

shinyApp(ui, server)

# Look at the prediction, think about
# 1. why are the coefficients for the "married" covariate be lower if race is included in the quantile regression?
# Collinearity: Race might be correlated with the "married" covariate. When both variables are included in the model, their effects may become confounded, leading to changes in the estimated coefficients. This could result in coefficients for "married" being attenuated or suppressed when race is included.
# Mediating Effect: Race could act as a mediator between "married" and the response variable. In this case, part of the effect of being married on the response variable might be captured by race. When both variables are included in the model, the direct effect of "married" may appear smaller.

# 2. in the predicted values, why are the predictions for # black mothers higher?
# Unmeasured Variables: The model may not fully capture all relevant factors that influence outcomes for black mothers. There could be unmeasured variables that are correlated with both race and the outcome, leading to higher predicted values for black mothers.
# Maybe due to racial disparities, black mothers may have different health conditions, physiological, socioeconomic status, or other factors that are not captured by the model. These unmeasured variables could lead to differences in predicted values for black mothers.

# 3. why are the scatter points divided into 2 clusters? 
# Categorical Variables: There are categorical variables in the dataset, particularly those with two levels (binary variables), it's common to observe clusters in scatter plots. Each cluster may correspond to one level of the categorical variable.
# Nonlinear Relationships: Nonlinear relationships between variables can manifest as clusters in scatter plots. If the relationship between two variables is not linear but follows a distinct pattern (such as quadratic or exponential), the scatter plot may exhibit clusters corresponding to different regions of the nonlinear relationship.
# Different races may have different characteristics when giving a birth. For example, black mothers may have different health conditions, physiological, socioeconomic status, or other factors that are not captured by the model. These unmeasured variables could lead to differences in predicted values for black mothers.