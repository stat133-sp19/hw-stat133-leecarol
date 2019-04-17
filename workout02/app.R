#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Modes of Saving"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(4,
            sliderInput("amount",
                        "Initial Amount",
                        min = 0,
                        max = 100000,
                        value = 1000,
                        step = 500),
            fluidRow(
              column(12,
                     sliderInput("contrib",
                                "Annual Contribution",
                                min = 0,
                                max = 50000,
                                value = 2000,
                                step = 500)))),
     column(4,
            sliderInput("rate",
                        "Return Rate (in %)",
                        min = 0,
                        max = 20,
                        value = 5,
                        step = 0.1),
            fluidRow(
              column(12,
                     sliderInput("growth",
                                 "Growth Rate (in %)",
                                 min = 0,
                                 max = 20,
                                 value = 2,
                                 step = 0.1)))),
     column(4,
            sliderInput("years",
                        "Years",
                        min = 0,
                        max = 50,
                        value = 20,
                        step = 1),
            fluidRow(
              column(12,
                     selectInput("facet",
                                 "Facet?",
                                 c("No" = FALSE,
                                   "Yes" = TRUE)))))
      ),
   
   hr(),
   
      # Show a plot of the generated distribution
      mainPanel(
        h4('Timeline'),
        plotOutput("distPlot"),
        h4('Balances'),
        verbatimTextOutput("table")
      )
   )


server <- function(input, output) {
  output$distPlot <- renderPlot({
    # Future Value Function
    #' @description computes the future value of an investment
    #' @param amount initial invested amount
    #' @param rate annual rate of return
    #' @param years number of years
    #' @return the computed future value
    future_value <- function(amount, rate, years){
      return(amount * ((1 + rate) ^ years))
    }
    
    # Future Value of Annuity
    #' @title future value of annuity function
    #' @description computes the future value of an annuity
    #' @param contrib contributed amount
    #' @param rate annual rate of return
    #' @param years number of years
    #' @return the computed future value of annuity
    annuity <- function(contrib, rate, years){
      return(contrib * (((1 + rate) ^ years - 1) / rate))
    }
    
    # Future Value of Growing Annuity
    #' @title future value of growing annuity function
    #' @description computes the future value of a growing annuity
    #' @param contrib contributed amount
    #' @param rate annual rate of return
    #' @param growth annual growth rate
    #' @param years number of years
    #' @return the computed future value of growing annuity
    growing_annuity <- function(contrib, rate, growth, years){
      return(contrib * ((((1 + rate) ^ years - (1 + growth) ^ years)/(rate - growth))))
    }
    
    # Modalities
    fv <- c()
    fva <- c()
    fvga <- c()
    for(i in seq(0, input$years)){
      x1 <- future_value(amount = input$amount, rate = input$rate * 0.01, years = i)
      x2 <- x1 + annuity(contrib = input$contrib, rate = input$rate * 0.01, years = i)
      x3 <- x1 + growing_annuity(contrib = input$contrib, rate = input$rate * 0.01, growth = input$growth * 0.01, years = i)
      fv <- append(fv, x1)
      fva <- append(fva, x2)
      fvga <- append(fvga, x3)
    }
    modalities <- data.frame(year = rep(seq(0, input$years), 3), balance = c(fv, fva, fvga), variable = c(rep(c('no_contrib'), input$years + 1), rep(c('fixed_contrib'), input$years + 1), rep(c('growing_contrib'), input$years + 1)))
    modalities$variable <- factor(modalities$variable, levels = c('no_contrib', 'fixed_contrib', 'growing_contrib'))
     if(input$facet){
       ggplot(modalities, aes(color = variable, fill = variable)) + geom_point(aes(x = year, y = balance)) + geom_area(aes(x = year, y = balance), alpha = 0.4) + xlab('year') + ylab('value') + ggtitle('Three modes of investing') + scale_colour_manual(values = c('red', 'green', 'blue')) + facet_grid(.~variable)
     }
     else{
       ggplot(modalities, aes(color = variable)) + geom_point(aes(x = year, y = balance)) + geom_line(aes(x = year, y = balance)) + xlab('year') + ylab('value') + ggtitle('Three modes of investing') + scale_colour_manual(name = 'variable', values = c('red', 'green', 'blue'))
     }
   })
  
  data <- reactive({
    # Future Value Function
    #' @description computes the future value of an investment
    #' @param amount initial invested amount
    #' @param rate annual rate of return
    #' @param years number of years
    #' @return the computed future value
    future_value <- function(amount, rate, years){
      return(amount * ((1 + rate) ^ years))
    }
    
    # Future Value of Annuity
    #' @title future value of annuity function
    #' @description computes the future value of an annuity
    #' @param contrib contributed amount
    #' @param rate annual rate of return
    #' @param years number of years
    #' @return the computed future value of annuity
    annuity <- function(contrib, rate, years){
      return(contrib * (((1 + rate) ^ years - 1) / rate))
    }
    
    # Future Value of Growing Annuity
    #' @title future value of growing annuity function
    #' @description computes the future value of a growing annuity
    #' @param contrib contributed amount
    #' @param rate annual rate of return
    #' @param growth annual growth rate
    #' @param years number of years
    #' @return the computed future value of growing annuity
    growing_annuity <- function(contrib, rate, growth, years){
      return(contrib * ((((1 + rate) ^ years - (1 + growth) ^ years)/(rate - growth))))
    }
    
    # Modalities
    fv <- c()
    fva <- c()
    fvga <- c()
    for(i in seq(0, input$years)){
      x1 <- future_value(amount = input$amount, rate = input$rate * 0.01, years = i)
      x2 <- x1 + annuity(contrib = input$contrib, rate = input$rate * 0.01, years = i)
      x3 <- x1 + growing_annuity(contrib = input$contrib, rate = input$rate * 0.01, growth = input$growth * 0.01, years = i)
      fv <- append(fv, x1)
      fva <- append(fva, x2)
      fvga <- append(fvga, x3)
    }
    data <- data.frame(years = seq(0, input$years), no_contrib = fv, fixed_contrib = fva, growing_contrib = fvga)
    return(data)})

  output$table <- renderPrint({
   data()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

