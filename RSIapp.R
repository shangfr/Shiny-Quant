library("shiny")
library("highcharter")
library("quantmod")
library("stringr")
# Define UI for application that draws a histogram

ui <- shinyUI(fluidPage(
  HTML('<nav class="navbar navbar-inverse">
       <a class="navbar-brand" href="#">商丰瑞のshiny</a>
       </nav>'),
  
  headerPanel("RSI策略分析工具",windowTitle = "MAstock-app"),
  
  sidebarPanel(
    textInput("text1", "股票代码", value = "603198"), 
    fluidRow(verbatimTextOutput("value")),
    dateRangeInput("dates", label = "观察时间",start ="2018-01-01"),
    selectInput("ttr", label = "TTR",  width = "100%",choices = c("EMA", "SMA", "VWAP")),
    sliderInput("ina", h5("短期均线参数值"), min = 1, max = 9, value = 5,step=1),
    sliderInput("inb", h5("长期均线参数值"), min = 10, max = 20, value = 10,step=1),
    submitButton("提交")
  ),
  column(width = 8,
         highchartOutput("hcontainer",height = "500px")
  )
  
  
  ))
server = function(input, output) {
  
  dataInput1 <- reactive({
    stockcode=input$text1
    firstcode=substr(stockcode,1,1)
    if((nchar(stockcode) != 6) | (length(str_extract_all(stockcode,"[0-9]")[[1]])!= 6)) {
      output$value <- renderPrint({"股票代码错误"}) 
    } else if (firstcode %in% c('0','2','3')) {
      ticker1 = paste(stockcode,"sz", sep=".") 
    } else if (firstcode  %in% c('6')) {
      ticker1 = paste(stockcode,"ss", sep=".") 
    } else {
      output$value <- renderPrint({ "只支持上海、深圳证券交易所股票" }) 
    }
    date=input$dates
    tSSE <- getSymbols(ticker1, from = date[1],to = date[2], auto.assign = FALSE)
    output$value <- renderPrint({invisible(NULL)})
    return(tSSE)
  })
  
  
  output$hcontainer <- renderHighchart({
    
    #SSE <- getSymbols("002612.sz", from = Sys.Date() - lubridate::years(1), auto.assign = FALSE)
    SSE = dataInput1()
    sma=input$ina
    lma=input$inb
    #移动平均SMA
    if( input$ttr =="SMA" ){
      Sma <- SMA(Cl(SSE), n = sma)
      Lma <- SMA(Cl(SSE), n = lma)
    }
    else if( input$ttr =="EMA" ){
      Sma <- EMA(Cl(SSE), n = sma)
      Lma <- EMA(Cl(SSE), n = lma)   
        
    }
    else{
      Sma <- VWAP(Cl(SSE),Vo(SSE), n = sma)
      Lma <- VWAP(Cl(SSE),Vo(SSE), n = lma)   
      
    }
    
    RSI <- RSI(Cl(SSE))
    RSI.SellLevel <- xts(rep(80, NROW(SSE)), index(SSE))
    RSI.Mid <- xts(rep(50, NROW(SSE)), index(SSE))
    RSI.BuyLevel <- xts(rep(20, NROW(SSE)), index(SSE))

    hc <-  highchart(type = "stock") %>% 
      hc_yAxis_multiples(create_yaxis(3, height = c(2, 1, 1), turnopposite = TRUE)) %>% 
      hc_add_series(SSE, color = '#1d953f',upColor = "#ed1941", yAxis = 0, name = input$text1) %>% 
      hc_add_series(Sma, color = "#ffd400",yAxis = 0, name = paste(input$ttr,input$ina, sep="-")) %>% 
      hc_add_series(Lma, color = "#33a3dc",yAxis = 0, name = paste(input$ttr,input$inb, sep="-")) %>% 
      hc_add_series(Vo(SSE), color = "#B0B0B0", yAxis = 1, name = "交易量", type = "column") %>% 
      hc_add_series(RSI, yAxis = 2, name = "RSI-14", color = hex_to_rgba("#43CD80", 0.7)) %>%
      hc_add_series(RSI.SellLevel, color = hex_to_rgba("#7CFC00", 0.7),yAxis = 2, name = "卖点") %>% 
      hc_add_series(RSI.Mid,color = hex_to_rgba("#d9d6c3", 0.7),yAxis = 2, name = "中线") %>% 
      hc_add_series(RSI.BuyLevel, color = hex_to_rgba("#FF3030", 0.7),yAxis = 2, name = "买点") 
    
    hc
    
  })
  
}

shinyApp(ui = ui, server = server)

