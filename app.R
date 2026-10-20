library(shiny)
library(plotrix)
#l
# 1. UI Section (Paste content from your old ui.R here)
ui <- fluidPage( # Application title
  titlePanel("Simulation: Confidence Intervals"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput("cl", 
                   label = "Confidence Level (1 - alpha)", 
                   value = 0.90,
                   min = 0,
                   max = 1,
                   step = 0.05),
      sliderInput("n",
                  "Sample Size:",
                  min = 10,
                  max = 50,
                  value = 10,
                  step = 1),
      sliderInput("r",
                  "Number of Samples:",
                  min = 15,
                  max = 100,
                  value = 15,
                  step = 5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))

  
  # 2. Server Section (Paste content from your old server.R here)
  server <- function(input, output){
    
    output$distPlot <- renderPlot({
      
      myci <- function(t) {
        se <- sd(t)/sqrt(input$n); # Find the standard error of the sample
        m <- mean(t); # Find the sample mean
        cv <- qnorm((1-input$cl)/2,lower.tail = FALSE) # cv is a critical value for the t distribution. P( t > cv ) = 0.025 = P( t < -cv )
        c(m-cv*se,m+cv*se) # Return the 95% confidence interval
      }
      mycolor <- function(endpoints, mn) {
        if (mn < endpoints[1]) 
          "Red" # if the mean is below the left endpoint of the confidence interval
        else if (mn > endpoints[2]) 
          "Red" # if the mean is above the right endpoint of the confidence interval
        else 
          "Black" # if the mean lies between the endpoints
      }
      w <- replicate(input$r,rnorm(input$n,mean=0,sd=1)) # w is a matrix, where each column is one random sample
      y <- apply(w,2,mean) # apply the mean function to each column of w
      ci <- apply(w,2,myci) # apply the myci function to each column of w. Each column of ci has the endpoints of a conficence interval. The first row has the left end points, the second row has the right end points.
      z <- apply(ci,2,mycolor,0) # apply the mycolor function to each column of ci. Note: 3 is the true mean.
      plotCI(x=1:input$r,y=y,li=ci[1,],ui=ci[2,],col=z,lwd=3,ylim=c(-2,2), xaxt="n", xlab = "", ylab = "", main=paste0(input$cl*100, "% Confidence Interval for the Mean"), sub = paste0(round(100*prop.table(table(z)),2)[1], "% of the intervals include the population mean, mu = 0"))
      abline(h = 0, lty = 2)
    })
    
  }

    
    # 3. App Launch (This binds the UI and Server together)
    shinyApp(ui = ui, server = server)
