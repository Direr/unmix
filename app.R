# install.packages("shiny")

library(shiny)
library(shinythemes)
rm(list=ls())

ui <- fluidPage(
    
    theme = shinytheme("superhero"),
    # theme = shinytheme("cerulean"),
    # img(src = "image.jpg"),
    h1("How your investment varies with ETH/DAI price"),
    # h3(""),
    span(" ", style="size-font:2.4em;"),
    # span(a("texte", href="#"), style="size:2.4em;"),
    br(),
    span("Auteur: Alexis Direr", style="size-font:2.4em;"),
    br(),
    span("R code: ", style="size-font:2.4em;"),
    span(a("https://github.com/Direr/blog_01", href="https://github.com/Direr/blog_01"), style="size:2.4em;"),
    br(),
    br(),
    
    sidebarLayout(
        
        sidebarPanel(
            
            sliderInput(inputId ="w0", 
                        label = "Your investment ($):",
                        min = 0, max = 10000, value = 1000),
            
            sliderInput("ho", 
                        label = "Your investment horizon (in months):",
                        min = 0, max = 24, value = 12),
            
            sliderInput(inputId ="part_uX", 
                        label = "Your exposition to ETH price (%):",
                        min = 0, max = 100, value = 100),
            
            sliderInput(inputId ="part_IL", 
                        label = "Your share of impermanent loss (%):",
                        min = 0, max = 100, value = 0),
            br(),                                                                                                                                                                                                                                                                                                                                                                                         
            "Assumptions:",
            br(),
            " yearly interest rate paid to Unmix uETH-IL LPs is 5%",
            br(),
            " yearly interest rate paid to Unmix uDAI-IL LPs is 10%",
            br(),
            " yearly fee rate earned by Uniswap LPs is 20%."
        ),
        
        mainPanel(
            
            plotOutput("pattern"),
            br(),
            div("Return decomposition (in percentage, over the selected horizon)", style="font-size:1.1em;"),
            div("Unmix interest rate:"),
            strong(textOutput("rendement"), style="color:lightblue; font-size:1.05em;"),
            div("Unmix insurance premium earned for bearing IL risk:"),
            strong(textOutput("r_IL"), style="color:lightblue; font-size:1.05em;"),
            div("Uniswap fee rate:"),
            strong(textOutput("fee_rate"), style="color:lightblue; font-size:1.05em;"),
            div("Overall interest rate: "),
            strong(textOutput("r_total"), style="color:lightblue; font-size:1.05em;")
        ),
        
    ),
    
)

server <- function(input, output) {
    
  reactive_results <- eventReactive(
    c(input$ho),
    {
      rr1 = 1.1^(input$ho/12)-1       # uX et uY-IL
      rr2 = 1.05^(input$ho/12)-1      # uY et uX-IL
      fr  = 0.2*(input$ho/12)
      
      c(rr1, rr2, fr)   # output dans r
      
    }
  )
  
  output$pattern <- renderPlot({
        
    r <- reactive_results()
    
    gamm = 0.5
    p = seq(from=0.01, to=3, by=0.01)
    v_uni = input$w0*(1+r[3])*sqrt(p)
    transfert1 = (p^gamm)*(1-r[2]) - 1
    uX = (1+transfert1)*v_uni
    uY_IL = (1-transfert1)*v_uni
    transfert2 = (p^(-gamm))*(1-r[3]) - 1
    uY = (1+transfert2)*v_uni
    uX_IL = (1-transfert2)*v_uni
    PX = input$w0*p
    PY = input$w0*rep(1,length(p))
    
    suX = (input$part_uX/100)*(1-input$part_IL/100)
    suY = (1-input$part_uX/100)*(1-input$part_IL/100)
    suX_IL = (input$part_uX/100)*input$part_IL/100
    suY_IL = (1-(input$part_uX/100))*input$part_IL/100
    
    PV = suX*uX + suY_IL*uY_IL + suY*uY + suX_IL*uX_IL
    
    plot(p,PV, type = "l", col="darkred", lwd = 3,
         xlab = "price", ylab = "",
         ylim = c(0,4*input$w0), yaxs="i", xaxs="i")
    lines(p,PX, col="blue", lwd = 2)
    lines(p,PY, col="darkgreen", lwd = 2)
    grid(0, 4, col = "gray", lwd = 2)
    legend("topleft", c("Your investment","ETH portfolio", "DAI portfolio"), 
           fill=c("darkred","blue", "darkgreen"))
   
    })
    
    output$rendement <- renderText({
      
      # paste("texte ", round(results[5], digits=2))
      
      r <- reactive_results()
      rend = -r[1]+(1-input$part_uX/100)*(r[1]-r[2])
      print(round(100*rend, digits=2))
    
    })
    
    output$r_IL <- renderText({
    
      r <- reactive_results()
      rr_IL = 100*((input$part_IL)*(1/100)*(r[1]+r[2]))
      print(round(rr_IL, digits=2))
      
    })
    
    output$fee_rate <- renderText({
      
      r <- reactive_results()
      print(100*round(r[3], digits=2))
      
    })
    
    output$r_total <- renderText({
      
      r <- reactive_results()
      rend = -r[1]+(1-input$part_uX/100)*(r[1]-r[2])
      r_IL = (input$part_IL)*(1/100)*(r[1]+r[2])
      print(round(100*(r[3]+rend+r_IL), digits=2))
      
    })
    
}
shinyApp(ui = ui, server = server)

