library(shiny)
library(ggplot2)

node_weight = function(l=1:8, N = 2^9, lambda = 8, verbose=0){
  N_l_1 = N/2^(l-1)
  N_l   = N/2^(l)
  w1 = N_l_1/(N_l_1 + lambda) 
  w2 =  N_l/(N_l + lambda)
  w = w1-w2
  L = max(l)+1
  N_L_1 = N/2^(L-1)
  N_0 = N
  w = c(1 - N_0/(N_0 + lambda),w,N_L_1/(N_L_1 + lambda))
  if (verbose) {
    wm = as.numeric(rbind(w1,-w2))
    print(wm)
    print(round(c(w[1], N_L_1/(N_L_1 + lambda)),4))
  }
  return(w)
}

ui <- fluidPage(
  numericInput(inputId = "lambda",
               "lambda", value = 10),
  plotOutput(outputId = "LinePlot")
)

server <- function(input, output) {
  
  output$LinePlot <- renderPlot({
    # x=seq(0, input$Num, 0.1)
    # y=sin(x)
    # df <- data.frame(x, y)
    # 
    # ggplot(data = df, aes(x, y)) +
    #   geom_line() +
    #   labs(x = "Time", y = "Sine Wave")
    #print(input$lambda)
    plot(0:9, node_weight(1:8, lambda=input$lambda), type="l", lwd = 2, xlab = "depth", ylab = "weight");grid()
  })
}
shinyApp(ui = ui, server = server)

