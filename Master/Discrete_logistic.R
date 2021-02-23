library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)

inline = function (x) {
  tags$div(style = "display:inline-block;", x)
}

ui <-
  fluidPage(titlePanel("Simulating the discrete logistic growth map, x(t+1)=rx(t)(1-x(t))"),
            
            box(width = 12, title = "Input parameters",
                column(12, align="center", style = "background-color:linen;",
                       
                       splitLayout(
                         sliderInput(inputId = "R",
                                     label = p(HTML(paste0("Choose the growth rate, r"))),
                                     min = 0,max = 4,value = 1,step = 0.01),
                         sliderInput(inputId = "Iterations",
                                     label = p(HTML(paste0("Number of iterations"))),
                                     min = 10,max = 100,value = 50,step = 10)),
                       
                       splitLayout(
                         numericInput(inputId = "IC1",
                                      label = p(HTML(paste0("Black initial condition, x",tags$sub("0")," (between 0 and 1)"))),
                                      value = 0.6,min = 0,max = 1,step = 0.1),
                         numericInput(inputId = "IC2",
                                      label = p(HTML(paste0("Blue initial condition, x",tags$sub("0")," (between 0 and 1)"))),
                                      value = 0.61,min = 0,max = 1,step = 0.1)),
                       
                       
                       
                )
            ),  
            box(width = 12, title = "Simulation results",
                column(12, align="center", style = "background-color:lightcyan;",
                       fluidRow(
                         
                         column(3,plotOutput(outputId = "Spider")),
                         column(4,plotOutput(outputId = "Bifurcation")),
                         column(5,plotOutput(outputId = "TimeSeries")),
                       )
                )
            )
            
  )






server <- function(input, output) {
  y1<-reactive({
    x <- c(input$Iterations,1)
    x[1] <- input$IC1
    for (i in 2:input$Iterations){
      x[i]<-input$R*x[i-1]*(1-x[i-1])
    }
    x
  })
  y2<-reactive({
    x <- c(input$Iterations,1)
    x[1] <- input$IC2
    for (i in 2:input$Iterations){
      x[i]<-input$R*x[i-1]*(1-x[i-1])
    }
    x
  })
  
  xTimeSeries<-reactive(1:input$Iterations);
  xSpider<-seq(0,1,0.01);
  
  output$Spider <- renderPlot({
    par(bg = "lightcyan")
    plot(xSpider,input$R*xSpider*(1-xSpider),"l",xlab="x(t)",ylab="x(t+1)",col="cyan3",ylim=c(0,1),xlim=c(0,1), lwd = 3,asp=1)
    lines(xSpider,xSpider,"l",col="cyan4",ylim=c(0,1),xlim=c(0,1), lwd = 3,asp=1)
    for (i in 2:input$Iterations){
      lines(c(y1()[i-1],y1()[i]),c(y1()[i],y1()[i]),"l",col="black", lwd = 3)
      lines(c(y1()[i-1],y1()[i-1]),c(y1()[i-1],y1()[i]),"l",col="black", lwd = 3)
      lines(c(y2()[i-1],y2()[i]),c(y2()[i],y2()[i]),"l",col="blue", lwd = 3)
      lines(c(y2()[i-1],y2()[i-1]),c(y2()[i-1],y2()[i]),"l",col="blue", lwd = 3)
    }
    
  })
  
  
  output$TimeSeries <- renderPlot({
    par(bg = "lightcyan")
    plot(xTimeSeries(),y1(),"b",xlab="Iterations",ylab="x",col="black",ylim=c(0,1), lwd = 3)
    lines(xTimeSeries(),y2(),"b",col="blue", lwd = 3)
  })
  
  
  xyLogisticsPoints <- read.csv(file = "Logistic_points.csv",header = F)
  
  output$Bifurcation <- renderPlot({
    par(bg = "lightcyan")
    plot(xyLogisticsPoints,lwd=1,pch=20,xlab="r",ylab="Steady states",ylim=c(0,1))
    lines(c(input$R,input$R),c(0,1),col="red", lwd = 3)
  })
  
}



shinyApp(ui = ui, server = server)
