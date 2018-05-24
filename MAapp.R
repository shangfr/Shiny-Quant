library(stringr)
library(tidyquant)
library(dygraphs)
library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
                        HTML('<nav class="navbar navbar-inverse">
                             <a class="navbar-brand" href="#">商丰瑞のshiny</a>
                             </nav>'),
                        
                        headerPanel("均线策略分析工具",windowTitle = "MAstock-app"),
                        
                        sidebarPanel(
                          textInput("text1", "股票代码", value = "603198"), 
                          fluidRow(verbatimTextOutput("value")),
                          dateRangeInput("dates", label = "观察时间",start ="2018-01-01"),
                          selectInput("ttr", label = "TTR",  width = "100%",choices = c("SMA", "EMA", "VWAP")),
                          br(),
                          sliderInput("ina", h5("短期均线参数值"), min = 1, max = 9, value = 5,step=1),
                          sliderInput("inb", h5("长期均线参数值"), min = 10, max = 20, value = 10,step=1),
                          submitButton("提交")
                        ),
                        mainPanel(
                          tabsetPanel(type = "tabs", 
                                      tabPanel("买入窗口", dygraphOutput("portfolioPlot")), 
                                      tabPanel("资本损益", dygraphOutput("trend")), 
                                      tabPanel("买卖详情",DT::dataTableOutput("sheet"))
                          )
                          
                        )
                        ))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
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
    tSSE <- tq_get(ticker1, get = "stock.prices", from = date[1],to = date[2])
    output$value <- renderPrint({invisible(NULL)})
    return(tSSE)
  })
  
  
  
  dataInput2 <- reactive({
    tSSE=dataInput1()
    sma=input$ina
    lma=input$inb
    #移动平均SMA
    
    if( input$ttr =="SMA" ){
      SSE <- tSSE %>%
        tq_mutate(select = close, mutate_fun = SMA, n = sma,col_rename = "sma") %>%
        tq_mutate(select = close, mutate_fun = SMA, n = lma,col_rename = "lma") %>%
        mutate(nsig = sma - lma) %>% mutate(bs = nsig>0)
    }
    else if( input$ttr =="EMA" ){
      SSE <- tSSE %>%
        tq_mutate(select = close, mutate_fun = EMA, n = sma,col_rename = "sma") %>%
        tq_mutate(select = close, mutate_fun = EMA, n = lma,col_rename = "lma") %>%
        mutate(nsig = sma - lma) %>% mutate(bs = nsig>0)
      
    }
    else{
      SSE <- tSSE %>%
        tq_mutate_xy(x = close, y = volume, mutate_fun = VWMA, n = sma,col_rename = "sma") %>%
        tq_mutate_xy(x = close, y = volume, mutate_fun = VWMA, n = lma,col_rename = "lma") %>%
        mutate(nsig = sma - lma) %>% mutate(bs = nsig>0)
      
    }
    
    SSE$close = round(SSE$close,2)
    return(SSE)
  })
  
  dataInput3 <- reactive({
    tSSE=dataInput1()
    SSE=dataInput2()
    stock_p = SSE %>%
      select(date,close,bs) %>%
      filter(!is.na(bs)) %>%
      mutate(bs = gsub(pattern = "TRUE", replacement = 1, bs)) %>%
      mutate(bs = gsub(pattern = "FALSE", replacement = -1, bs))  %>%
      mutate(bs = as.numeric(bs))  %>%
      mutate(po = bs+c(-1,head(bs,-1)))  %>%
      filter(po == 0) %>%
      select(date,close,bs)
    
    #初始化
    #佣金千分之三
    deal_fees = 0.003
    
    t= length(stock_p$close)
    if(t%%2 != 0) {
      t= t-1
    }else{
      t= t
    }
    
    buysell = stock_p$close[1:t]
    balance = 100000
    account = data.frame()
    for(i in seq(1,t,2)){
      #买入
      volume = floor(balance/(buysell[i]*(1+deal_fees)))
      balance = balance-buysell[i]*volume*(1+deal_fees)
      account =rbind(account, data.frame(volume,balance))
      #卖出
      balance = balance + buysell[i+1]*volume*(1+deal_fees)
      account =rbind(account, data.frame(volume,balance))
    }
    account$date = stock_p$date[1:t]
    account$balance = round(account$balance,2)
    #账户记录
    aa = tSSE %>% select(date,close) 
    c = tibble()
    i <- 1
    j = length(account$date)
    while( i <= j)
    {
      a= aa %>%
        filter(date>=account$date[i] & date<account$date[i+1]) %>%  
        mutate(volu=account$volume[i]) %>%  
        mutate(yue=close*volu+account$balance[i])
      if(i+2<j){
        b= aa %>%
          filter(date>=account$date[i+1] & date<account$date[i+2]) %>%  
          mutate(volu=0) %>%  
          mutate(yue=account$balance[i+1]) 
        
      }
      
      else{
        b= aa %>%
          filter(date==account$date[i+1]) %>%  
          mutate(volu=0) %>%  
          mutate(yue=account$balance[i+1]) 
        
      }  
      
      c = bind_rows(c,a,b)                                                              
      
      i <- i + 2
    }
    
    #交易记录
    record= list()
    record$c = c
    record$stock_p = stock_p
    record$account = account
    
    ass = account$balance[seq(2,t,2)]
    
    #总收益
    record$tot <- round(tail(ass,1)-100000,2)
    #年化收益
    days=as.numeric(date(range(account$date)[2])-date(range(account$date)[1]))
    record$an_return=round(record$tot/days*0.0365,2)
    
    #夏普比率
    
    # max drawdown
    cum  <- cummax(ass)
    cmaxx <- cum - ass
    mdd <- max(cmaxx)
    dx <- cum[which(cmaxx==mdd)]
    record$maxdrawdown <- round(100*mdd/dx,2)
    
    return(record)
  })
  
  output$portfolioPlot <- renderDygraph({
    SSE= dataInput2() %>% filter(!is.na(bs))
    record=dataInput3()
    #dygraph,xts data
    dSSE1 = as.data.frame(SSE)
    dSSE = as.xts(dSSE1,order.by = dSSE1$date, descr='my new xts object')[,c(2:5,8:9)]
    
    dateindex=index(dSSE)
    dateWindow <- range(dateindex)
    buydate = dateindex[which(dSSE1$nsig>0)]
    selldate = dateindex[which(dSSE1$nsig<0)]
    dygraph = dygraph(dSSE,main = paste("年化收益率：", record$an_return,"%  最大回撤：",record$maxdrawdown,"%", sep = "")) %>%
      dyCandlestick() %>%
      dyRangeSelector(dateWindow = dateWindow) 
    

    t= length(record$account$date)
    i=1 
    
    
    repeat{
      dygraph = dyShading(dygraph,from = record$account$date[i], to = record$account$date[i+1], color = "#FFE6E6")
      i <- i+2
      if(i>t)break
    }   
    
    dygraph    
  })
 
  output$trend <- renderDygraph({
    record = dataInput3()
    dyaccount = record$c
    dyaccount = as.xts(dyaccount,order.by = dyaccount$date, descr='my new xts object')[,"yue"]
    
    dygraph(dyaccount, main = paste("总收益：",record$tot,"  年化收益率：", record$an_return,"%  最大回撤：",record$maxdrawdown,"%", sep = "")) %>%
      dyAxis("y", label = "账户", valueRange =c(0,(as.numeric(max(dyaccount))+10000) )) %>%
      dyOptions(axisLineWidth = 3, fillGraph = TRUE, drawGrid = FALSE)
    
    
  })
  
  output$sheet <- DT::renderDataTable({
    record=dataInput3() 
    finalrecord = left_join(record$stock_p,record$account,by="date")
    
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

