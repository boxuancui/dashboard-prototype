library(shiny)
library(data.table)
library(reshape2)
library(scales)
library(rCharts)

shinyServer(function(input, output) {
  raw_data <- reactive({
    raw <- fread("data/reporting_data.csv", sep=",")
    raw[, PlacementKey:=substr(Placement, 1, 6)]
    raw.agg <- raw[,
                   list(
                     Impressions = sum(Impressions),
                     Clicks = sum(Clicks),
                     ViewThroughs = ConversionTracking_LandingPages_ViewthroughConversions +
                       Remarketing_ContactUsUnique_ViewthroughConversions +
                       Remarketing_HomepageUnique_ViewthroughConversions +
                       Remarketing_LocationsPageUnique_ViewthroughConversions +
                       Remarketing_ScheduleCallUnique_ViewthroughConversions,
                     DBMCostUSD = sum(DBMCostUSD)
                   ),
                   by=list(PlacementKey, Date)]
    raw.agg[Date<="2015-01-18", DateKey:="B"]
    raw.agg[Date>"2015-01-18", DateKey:="A"]
    raw.agg[, CTR:=Clicks/Impressions]
    raw.agg[, VTR:=ViewThroughs/Impressions]
    raw.agg[, CPM:=1000*DBMCostUSD/Impressions]
    raw.agg[, CPC:=DBMCostUSD/Clicks]
    raw.agg[, CPV:=DBMCostUSD/ViewThroughs]
    setnames(raw.agg, c("DBMCostUSD", "PlacementKey"), c("Spend", "Placement"))
    setkeyv(raw.agg, c("DateKey", "Placement"))
    output <- merge(raw.agg, bridge)
    output
  })
  
  campaign_data <- reactive({
    input.date <- input$date
    
    data <- raw_data()[Date==input.date,
                       list(
                         Impressions = as.double(sum(Impressions)),
                         Clicks = as.double(sum(Clicks)),
                         ViewThroughs = as.double(sum(ViewThroughs)),
                         Spend = as.double(round(sum(Spend)))
                       ),
                       by=Group]
    data[, CTR:=as.double(Clicks/Impressions)]
    data[, VTR:=as.double(ViewThroughs/Impressions)]
    data[, CPM:=as.double(round(1000*Spend/Impressions, 2))]
    data[, CPC:=as.double(round(Spend/Clicks, 2))]
    data[, CPV:=as.double(round(Spend/ViewThroughs, 2))]
    data
  })
  
  output$campaign_stats <- renderTable({
    data <- campaign_data()
    dcast(melt(data, id.vars="Group"), variable ~ Group)
  })
  
  output$campaign_ctr <- renderChart2({
    input.data <- campaign_data()
    data <- data.frame(input.data[, list(CTR)])
    rownames(data) <- c("Control", "Test")
    
    a <- Highcharts$new()
    a$chart(height=380, width=380, type="column")
    a$title(text="CTR")
    a$xAxis(categories=rownames(data))
    a$legend(enabled=FALSE)
    a$data(data, dataLabels=list(enabled=TRUE))
    a
  })
  
  output$campaign_vtr <- renderChart2({
    input.data <- campaign_data()
    data <- data.frame(input.data[, list(VTR)])
    rownames(data) <- c("Control", "Test")
    
    a <- Highcharts$new()
    a$chart(height=380, width=380, type="column")
    a$title(text="VTR")
    a$xAxis(categories=rownames(data))
    a$legend(enabled=FALSE)
    a$data(data, dataLabels=list(enabled=TRUE))
    a
  })
  
  output$campaign_cpm <- renderChart2({
    input.data <- campaign_data()
    data <- data.frame(input.data[, list(CPM)])
    rownames(data) <- c("Control", "Test")
    
    a <- Highcharts$new()
    a$chart(height=380, width=380, type="column")
    a$title(text="CPM")
    a$xAxis(categories=rownames(data))
    a$legend(enabled=FALSE)
    a$data(data, dataLabels=list(enabled=TRUE))
    a
  })
  
  output$campaign_cpc <- renderChart2({
    input.data <- campaign_data()
    data <- data.frame(input.data[, list(CPC)])
    rownames(data) <- c("Control", "Test")
    
    a <- Highcharts$new()
    a$chart(height=380, width=380, type="column")
    a$title(text="CPC")
    a$xAxis(categories=rownames(data))
    a$legend(enabled=FALSE)
    a$data(data, dataLabels=list(enabled=TRUE))
    a
  })
  
  output$campaign_cpv <- renderChart2({
    input.data <- campaign_data()
    data <- data.frame(input.data[, list(CPV)])
    rownames(data) <- c("Control", "Test")
    
    a <- Highcharts$new()
    a$chart(height=380, width=380, type="column")
    a$title(text="CPV")
    a$xAxis(categories=rownames(data))
    a$legend(enabled=FALSE)
    a$data(data, dataLabels=list(enabled=TRUE))
    a
  })
  
  output$item_stats <- renderDataTable({
    input.date <- input$date
    input.data <- raw_data()
    
    data <- input.data[Date==input.date,
                       list(
                         Impressions = sum(Impressions),
                         Clicks = sum(Clicks),
                         ViewThroughs = sum(ViewThroughs),
                         Spend = round(sum(Spend))
                       ),
                       by=list(Group, LineItem, TechniqueBucket)]
    data[, CTR:=percent(Clicks/Impressions)]
    data[, VTR:=percent(ViewThroughs/Impressions)]
    data[, CPM:=round(1000*Spend/Impressions, 2)]
    data[, CPC:=round(Spend/Clicks, 2)]
    data[, CPV:=round(Spend/ViewThroughs, 2)]
    data$Impressions <- format(data$Impressions, big.mark=",")
    data$Clicks <- format(data$Clicks, big.mark=",")
    data$ViewThroughs <- format(data$ViewThroughs, big.mark=",")
    data$Spend <- format(data$Spend, big.mark=",")
    data
  })
  
  output$item_ctr_cpc <- renderChart2({
    input.date <- input$date
    input.data <- raw_data()
    
    data <- input.data[Date==input.date & Clicks>0,
                       list(
                         Impressions = sum(Impressions),
                         Clicks = sum(Clicks),
                         Spend = round(sum(Spend))
                       ),
                       by=list(LineItem)]
    data[, CTR:=Clicks/Impressions]
    data[, CPC:=round(Spend/Clicks, 2)]
    plot.data <- data.frame(data[, list(LineItem, Impressions, CTR, CPC)])
    
    a <- Highcharts$new()
    a$chart(type="bubble", height=768, width=1024, zoomType="xy")
    a$title(text="Line Item Performance")
    a$xAxis(title=list(text="CPC"))
    a$yAxis(title=list(text="CTR"))
    for (item in plot.data$LineItem) {
      tmp <- subset(plot.data, plot.data$LineItem==item)
      a$series(name=item, data=lapply(1:nrow(tmp), function(i) {list(tmp[i, "CPC"], tmp[i, "CTR"], tmp[i, "Impressions"])}))
    }
    return(a)
  })
  
  output$time_dim <- renderChart2({
    time_p <- input$time_p
    input.data <- raw_data()
    
    data <- input.data[,
                       list(
                         Impressions = sum(Impressions),
                         Clicks = sum(Clicks),
                         ViewThroughs = sum(ViewThroughs),
                         Spend = sum(Spend)
                       ),
                       by=list(Group, Date)]
    data[, CTR:=Clicks/Impressions]
    data[, VTR:=ViewThroughs/Impressions]
    data[, CPM:=round(1000*Spend/Impressions, 2)]
    data[, CPC:=round(Spend/Clicks, 2)]
    data[, CPV:=round(Spend/ViewThroughs, 2)]
    data <- data[order(Date)]
    plot.data <- data.frame(dcast(melt(data, id.vars=c("Group", "Date"), measure.vars=time_p), Date~Group))
    rownames(plot.data) <- plot.data$Date
    plot.data <- subset(plot.data, select=-Date)
    print(plot.data)
    
    a <- Highcharts$new()
    a$chart(height=768, width=1024, zoomType="xy")
    a$title(text="Daily Test Line Items Performance")
    a$xAxis(categories=data$Date[test.ind], labels=list(rotation=-45))
    a$yAxis(title=list(text=time_p))
    a$data(plot.data)
    return(a)
  })
})