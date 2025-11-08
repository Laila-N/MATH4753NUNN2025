# Load packages
library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Maximum Likelihood Estimation Demonstrator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Choose a distribution:",
                  choices = c("Normal", "Exponential", "Poisson", "Uniform", "Binomial")),
      numericInput("n", "Sample size:", value = 30, min = 5),
      actionButton("simulate", "Generate sample"),
      hr(),
      uiOutput("param_inputs")
    ),
    mainPanel(
      plotOutput("likelihoodPlot"),
      verbatimTextOutput("mleOutput")
    )
  )
)

server <- function(input, output, session) {

  # Generate random data
  data <- eventReactive(input$simulate, {
    switch(input$dist,
           "Normal" = rnorm(input$n, mean = 2, sd = 1),
           "Exponential" = rexp(input$n, rate = 1),
           "Poisson" = rpois(input$n, lambda = 3),
           "Uniform" = runif(input$n, min = 0, max = 5),
           "Binomial" = rbinom(input$n, size = 10, prob = 0.5))
  })

  # Generate parameter inputs based on distribution
  output$param_inputs <- renderUI({
    switch(input$dist,
           "Normal" = tagList(
             sliderInput("mean_range", "Mean range:", min = -2, max = 6, value = c(0, 4)),
             sliderInput("sd_range", "SD range:", min = 0.1, max = 3, value = c(0.5, 2))
           ),
           "Exponential" = sliderInput("rate_range", "Rate range:", min = 0.1, max = 3, value = c(0.1, 2)),
           "Poisson" = sliderInput("lambda_range", "Lambda range:", min = 0.1, max = 10, value = c(0.1, 6)),
           "Uniform" = tagList(
             sliderInput("min_range", "Min range:", min = -2, max = 2, value = c(0, 1)),
             sliderInput("max_range", "Max range:", min = 2, max = 10, value = c(4, 6))
           ),
           "Binomial" = sliderInput("prob_range", "Probability range:", min = 0, max = 1, value = c(0.1, 0.9))
    )
  })

  # Compute log-likelihood and plot
  output$likelihoodPlot <- renderPlot({
    x <- data()
    dist <- input$dist

    if (dist == "Normal") {
      means <- seq(input$mean_range[1], input$mean_range[2], length.out = 100)
      sds <- seq(input$sd_range[1], input$sd_range[2], length.out = 100)
      grid <- expand.grid(mean = means, sd = sds)
      grid$logLik <- sapply(1:nrow(grid), function(i)
        sum(dnorm(x, mean = grid$mean[i], sd = grid$sd[i], log = TRUE)))
      mle <- grid[which.max(grid$logLik), ]
      ggplot(grid, aes(mean, sd, fill = logLik)) +
        geom_tile() + labs(title = "Normal Distribution Log-Likelihood") +
        geom_point(aes(x = mle$mean, y = mle$sd), color = "red", size = 3) +
        scale_fill_viridis_c()

    } else if (dist == "Exponential") {
      rates <- seq(input$rate_range[1], input$rate_range[2], length.out = 200)
      logLik <- sapply(rates, function(r) sum(dexp(x, rate = r, log = TRUE)))
      mle <- rates[which.max(logLik)]
      ggplot(data.frame(rate = rates, logLik = logLik),
             aes(rate, logLik)) +
        geom_line() + geom_vline(xintercept = mle, color = "red") +
        labs(title = "Exponential Log-Likelihood", y = "Log-Likelihood")

    } else if (dist == "Poisson") {
      lambdas <- seq(input$lambda_range[1], input$lambda_range[2], length.out = 200)
      logLik <- sapply(lambdas, function(l) sum(dpois(x, lambda = l, log = TRUE)))
      mle <- lambdas[which.max(logLik)]
      ggplot(data.frame(lambda = lambdas, logLik = logLik),
             aes(lambda, logLik)) +
        geom_line() + geom_vline(xintercept = mle, color = "red") +
        labs(title = "Poisson Log-Likelihood", y = "Log-Likelihood")

    } else if (dist == "Uniform") {
      mins <- seq(input$min_range[1], input$min_range[2], length.out = 50)
      maxs <- seq(input$max_range[1], input$max_range[2], length.out = 50)
      grid <- expand.grid(min = mins, max = maxs)
      grid <- grid[grid$max > grid$min, ]
      grid$logLik <- sapply(1:nrow(grid), function(i)
        sum(dunif(x, min = grid$min[i], max = grid$max[i], log = TRUE)))
      mle <- grid[which.max(grid$logLik), ]
      ggplot(grid, aes(min, max, fill = logLik)) +
        geom_tile() + geom_point(aes(x = mle$min, y = mle$max), color = "red", size = 3) +
        labs(title = "Uniform Distribution Log-Likelihood") +
        scale_fill_viridis_c()

    } else if (dist == "Binomial") {
      probs <- seq(input$prob_range[1], input$prob_range[2], length.out = 200)
      logLik <- sapply(probs, function(p)
        sum(dbinom(x, size = 10, prob = p, log = TRUE)))
      mle <- probs[which.max(logLik)]
      ggplot(data.frame(prob = probs, logLik = logLik),
             aes(prob, logLik)) +
        geom_line() + geom_vline(xintercept = mle, color = "red") +
        labs(title = "Binomial Log-Likelihood", y = "Log-Likelihood")
    }
  })

  # Display MLE estimates
  output$mleOutput <- renderPrint({
    x <- data()
    dist <- input$dist
    mle <- switch(dist,
                  "Normal" = c(mean = mean(x), sd = sd(x)),
                  "Exponential" = c(rate = 1 / mean(x)),
                  "Poisson" = c(lambda = mean(x)),
                  "Uniform" = c(min = min(x), max = max(x)),
                  "Binomial" = c(prob = mean(x) / 10)
    )
    cat("Sample Size:", length(x), "\nMLE Estimates:\n")
    print(mle)
  })
}

shinyApp(ui, server)
