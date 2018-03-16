#
#

library(shiny)
library(tidyquant)
library(stringr)
library(dygraphs)
library(grid)
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(theme = "bootstrap.css",
                        HTML('<nav class="navbar navbar-inverse">
                             <div class="container-fluid">
                             
                             <div class="navbar-header">
                             <button type="button" class="navbar-toggle collapsed" data-toggle="collapse">
                             <span class="sr-only">Toggle navigation</span>
                             <span class="icon-bar"></span>
                             </button>
                             <a class="navbar-brand" href="#">商丰瑞のshiny</a>
                             </div>
                             
                             <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-2">
                             <ul class="nav navbar-nav">
                             <li class="active"><a href="http://www.cnblogs.com/shangfr/">我的博客 <span class="sr-only">(current)</span></a></li>
                             </ul>
                             </div>
                             
                             </div>
                             </nav>'),
                        
                        headerPanel("投资组合分析工具",windowTitle = "portfolio-app"),
                        
                        sidebarPanel(
                          textInput("text1", "股票代码", value = "000001"), 
                          fluidRow(verbatimTextOutput("value")),
                          dateRangeInput("dates", label = "观察时间",start ="2017-01-01"),
                          br(),
                          sliderInput("ina", h5("短期均线参数值"), min = 1, max = 9, value = 5,step=1),
                          sliderInput("inb", h5("长期均线参数值"), min = 10, max = 20, value = 10,step=1),
                          submitButton("提交")
                        ),
                        mainPanel(
                          tabsetPanel(type = "tabs", 
                                      tabPanel("买入窗口", dygraphOutput("portfolioPlot")), 
                                      tabPanel("资本损益", plotOutput("trend")), 
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
    SSE=dataInput1()
    sma=input$ina
    lma=input$inb
    SSE <- SSE %>%
      tq_mutate(select = close, mutate_fun = SMA, n = sma) %>%
      rename(sma = SMA) %>%
      tq_mutate(select = close, mutate_fun = SMA, n = lma) %>%
      rename(lma = SMA) %>%
      mutate(nsig = sma - lma) %>%
      mutate(bs = nsig>0)  %>%
      mutate(mbs = gsub(pattern = "TRUE", replacement = "BUY", bs))
    return(SSE)
  })
  
  dataInput3 <- reactive({
    SSE=dataInput2()
    stock_p = SSE %>%
      select(date,close,bs) %>%
      filter(!is.na(bs)) %>%
      mutate(bs = gsub(pattern = "TRUE", replacement = -1, bs)) %>%
      mutate(bs = gsub(pattern = "FALSE", replacement = 1, bs))  %>%
      mutate(bs = as.numeric(bs))  %>%
      mutate(po = bs*c(bs[-1],0))  %>%
      filter(po == -1) %>%
      mutate(stock_price = close*100*bs)
    
    #初始化
    my_investment = 100000
    market_value = 0
    deal_fees = 0.003
    account_balances = my_investment
    df = data.frame()
    tt= length(stock_p$stock_price)
    ss= stock_p$stock_price[1]
    
    if(tt%%2 != 0 & ss<0) {
      stock_p = slice(stock_p,-1)
    } else if (tt%%2 != 0 & ss>0) {
      stock_p = slice(stock_p,1:(tt-1))
    }else if (tt%%2 == 0 & ss<0) {
      stock_p = slice(stock_p,2:(tt-1))
    }else{
      stock_p = stock_p
    }
    
    t= length(stock_p$stock_price)
    for(i in seq(1,t,2)){
      #买入
      stock_price = stock_p$stock_price[i]
      n = floor(account_balances/(stock_price*(1+deal_fees)))
      account_balances =account_balances- stock_price*n*(1+deal_fees)
      market_value = market_value + stock_price*n
      total_assets = account_balances+market_value
      df =rbind(df, data.frame(n,market_value,account_balances,total_assets))
      #卖出
      stock_price = stock_p$stock_price[i+1]
      account_balances =account_balances- stock_price*n+stock_price*n*deal_fees
      n=0
      market_value = 0
      total_assets = account_balances
      df =rbind(df, c(n,market_value,account_balances,total_assets))
    }
    stock_p$n=df$n
    stock_p$total_assets=round(df$total_assets,2)
    return(stock_p)
  })
  
  output$portfolioPlot <- renderDygraph({
    SSE= dataInput2() %>% filter(!is.na(bs))
      
    #dygraph,xts data
    dSSE1 = as.data.frame(SSE)
    dSSE = as.xts(dSSE1,order.by = dSSE1$date, descr='my new xts object')[,c(2:5,8:9)]
    
    dateindex=index(dSSE)
    dateWindow <- range(dateindex)
    buydate = dateindex[which(dSSE1$nsig>0)]
    selldate = dateindex[which(dSSE1$nsig<0)]
    dygraph = dygraph(dSSE) %>%
      dyCandlestick() %>%
      dyRangeSelector(dateWindow = dateWindow) 
    
    stock_p=dataInput3()
    t= length(stock_p$date)
    i=1 
    
    
    repeat{
      dygraph = dyShading(dygraph,from = stock_p$date[i], to = stock_p$date[i+1], color = "#FFE6E6")
      i <- i+2
      if(i>t)break
    }   
    
    dygraph    
  })
 
  output$trend <- renderPlot({
    stock_p=dataInput3()
    ass <- stock_p$total_assets
    tot <- tail(ass,1)-100000
    
    days=as.numeric(date(range(stock_p$date)[2])-date(range(stock_p$date)[1]))
    an_return=round(tot/days*0.0365,2)
    
    cum  <- cummax(ass) # max drawdown
    cmaxx <- cum - ass
    mdd <- max(cmaxx)
    dx <- cum[which(cmaxx==mdd)]
    rmdd <- round(100*mdd/dx,2)
    
    na.omit(stock_p) %>%
      select(date,total_assets) %>%
      ggplot(aes(x=date,y=total_assets))+
      geom_bar(stat="identity",size=1, colour="#20B2AA")+ 
      geom_line(linetype = 6,size=2,colour="#FF6A6A")+ 
      geom_area(alpha = 0.3)+
      labs(title = paste("总收益：",tot,"  年化收益率：", an_return,"  最大回撤：",rmdd, sep = "") )
    
   
  })
  
  output$sheet <- DT::renderDataTable({
    d=dataInput3() %>%
      select(date,close,bs,n,total_assets)
    
  })
})
# Run the application 
shinyApp(ui = ui, server = server)

