library(shiny)
library(ggplot2)
library(reshape2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Savings + Investing"),
  
  fluidRow(
    
    column(3,
           wellPanel(
             sliderInput("init",
                         "Initial Amount",
                         min = 0,
                         max = 100000,
                         value = 1000,
                         step = 500),
             sliderInput("annual",
                         "Annual Contribution:",
                         min = 0,
                         max = 50000,
                         value = 2000,
                         step = 500)
           )
           
    ),
    column(3,
           wellPanel(
             sliderInput("return",
                         "Return Rate (in %):",
                         min = 0,
                         max = 20,
                         value = 5,
                         step = 0.1),
             sliderInput("growth",
                         "Growth Rate (in %):",
                         min = 0,
                         max = 20,
                         value = 2,
                         step = 0.1)
           
    )
  ),
  column(3,
         wellPanel(
           sliderInput("years",
                       "Years:",
                       min = 0,
                       max = 50,
                       value = 20,
                       step = 1),
           selectInput("select", label = h3("Facet?"), 
                       choices = list("Yes" = TRUE, "No" = FALSE), 
                       selected = FALSE)
         )
  ),
  mainPanel(
      plotOutput("timeline"),
      tableOutput("balances")
  )
)
)

server <- function(input, output) {
  future_value = function(amount = 0, rate = 0, years = 0){
    return(amount*(1+rate)^years)
  }
  
  annuity = function(contrib = 0, rate = 0, years = 0){
    return(contrib*(((1+rate)^years)-1)/rate)
  }
  
  growing_annuity = function(contrib = 0, rate = 0, growth = 0, years = 0){
    return(contrib*(((1+rate)^years)-(1+growth)^years)/(rate-growth))
  }
  
  output$balances <- renderTable({
    no_contrib <- rep(0, input$years)
    fixed_contrib <- rep(0, input$years)
    growing_contrib <- rep(0, input$years)
    year <- seq(0,input$years,1)
    for (i in 0:input$years+1) {
      no_contrib[i] = future_value(amount = input$init, rate = input$return/100, years = i-1)
      fixed_contrib[i] = no_contrib[i] + annuity(contrib = input$annual, rate = input$return/100, years = i-1)
      growing_contrib[i] = no_contrib[i] + growing_annuity(contrib = input$annual, rate = input$return/100, growth = input$growth/100, years = i-1)
    }
    modalities <- data.frame(year, no_contrib, fixed_contrib, growing_contrib)
    modalities
  })
  output$timeline <- renderPlot({
    no_contrib <- rep(0, input$years)
    fixed_contrib <- rep(0, input$years)
    growing_contrib <- rep(0, input$years)
    year <- seq(0,input$years,1)
    for (i in 0:input$years+1) {
      no_contrib[i] = future_value(amount = input$init, rate = input$return/100, years = i-1)
      fixed_contrib[i] = no_contrib[i] + annuity(contrib = input$annual, rate = input$return/100, years = i-1)
      growing_contrib[i] = no_contrib[i] + growing_annuity(contrib = input$annual, rate = input$return/100, growth = input$growth/100, years = i-1)
    }
    modalities <- data.frame(year, no_contrib, fixed_contrib, growing_contrib)
    modalities <- na.omit(modalities)
    modalities2 <- melt(modalities, id.vars = "year")
    if(input$select == FALSE){
      ggplot(data = modalities2, aes(x = year, y = value, group = variable, colour = variable)) + 
        geom_point() + 
        geom_line() + 
        ggtitle("Three Modes of Investing") + 
        theme_bw() + 
        ylab("Value") + 
        labs(color = "modality")
    }
    else{
      ggplot(data = modalities2, aes(x = year, y = value, group = variable, colour = variable)) + 
        geom_area(aes(fill = variable), alpha = 0.5) +
        geom_point() + 
        geom_line() + 
        ggtitle("Three Modes of Investing") + 
        theme_bw() + 
        ylab("Value") + 
        labs(color = "modality") +
        facet_wrap(~ variable)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

