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
    start.date <- input$date[1]
    end.date <- input$date[2]
    
    data <- raw_data()[Date>=start.date & Date<=end.date,
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
    output <- dcast.data.table(melt(data, id.vars="Group"), variable ~ Group)
    setnames(output, "variable", "Metrics")
    output <- data.frame(output)
    format.bign <- which(output$Metrics %in% c("Impressions", "Clicks", "ViewThroughs", "Spend"))
    format.smalln <- which(output$Metrics %in% c("CTR", "VTR"))
    output[format.bign, -1] <- format(output[format.bign, -1], big.mark=",")
    for (group in colnames(output)[-1]) {
      output[format.smalln, group] <- percent(as.numeric(output[format.smalln, group]))
    }
    output
  }, include.rownames=FALSE)
  
  output$campaign_ctr <- renderChart2({
    input.data <- campaign_data()
    data <- data.frame(input.data[, list(CTR=round(CTR*100,4))])
    rownames(data) <- c("Control", "Test")
    
    a <- Highcharts$new()
    a$chart(height=380, width=380, type="column")
    a$title(text="CTR (%)")
    a$xAxis(categories=rownames(data))
    a$legend(enabled=FALSE)
    a$data(data, dataLabels=list(enabled=TRUE, format="{y} %"))
    a
  })
  
  output$campaign_vtr <- renderChart2({
    input.data <- campaign_data()
    data <- data.frame(input.data[, list(round(VTR*100,4))])
    rownames(data) <- c("Control", "Test")
    
    a <- Highcharts$new()
    a$chart(height=380, width=380, type="column")
    a$title(text="VTR (%)")
    a$xAxis(categories=rownames(data))
    a$legend(enabled=FALSE)
    a$data(data, dataLabels=list(enabled=TRUE, format="{y} %"))
    a
  })
  
  output$campaign_cpm <- renderChart2({
    input.data <- campaign_data()
    data <- data.frame(input.data[, list(CPM)])
    rownames(data) <- c("Control", "Test")
    
    a <- Highcharts$new()
    a$chart(height=380, width=380, type="column")
    a$title(text="CPM ($)")
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
    a$title(text="CPC ($)")
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
    a$title(text="CPV ($)")
    a$xAxis(categories=rownames(data))
    a$legend(enabled=FALSE)
    a$data(data, dataLabels=list(enabled=TRUE))
    a
  })
  
  output$item_stats <- renderDataTable({
    start.date <- input$date[1]
    end.date <- input$date[2]
    input.data <- raw_data()
    
    data <- input.data[Date>=start.date & Date<=end.date,
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
  
  output$item_bubble <- renderChart2({
    start.date <- input$date[1]
    end.date <- input$date[2]
    x_axis <- input$item_bubble_x
    y_axis <- input$item_bubble_y
    size <- input$item_bubble_size
    input.data <- raw_data()
    
    data <- input.data[Date>=start.date & Date<=end.date & Clicks>0,
                       list(
                         Impressions = sum(Impressions),
                         Clicks = sum(Clicks),
                         Spend = round(sum(Spend))
                       ),
                       by=list(LineItem)]
    data[, CTR:=round(100*Clicks/Impressions, 4)]
    data[, CPC:=round(Spend/Clicks, 2)]
    data[, CPM:=round(1000*Spend/Impressions, 2)]
    plot.data <- data.frame(data[, c("LineItem", x_axis, y_axis, size), with=FALSE])
    
    if (x_axis=="CTR" & y_axis=="CTR") {
      tooltip_text <- paste0(x_axis, ": {point.x}%, ", y_axis, ": {point.y}%, ", size, ": {point.z}")
    } else if (x_axis!="CTR" & y_axis!="CTR") {
      tooltip_text <- paste0(x_axis, ": ${point.x}, ", y_axis, ": ${point.y}, ", size, ": {point.z}")
    } else if (x_axis=="CTR" & y_axis!="CTR") {
      tooltip_text <- paste0(x_axis, ": {point.x}%, ", y_axis, ": ${point.y}, ", size, ": {point.z}")
    } else {
      tooltip_text <- paste0(x_axis, ": ${point.x}, ", y_axis, ": {point.y}%, ", size, ": {point.z}")
    }
    
    a <- Highcharts$new()
    a$chart(type="bubble", height=768, width=1024, zoomType="xy")
    a$plotOptions(bubble=list(tooltip=list(pointFormat=tooltip_text)))
    a$title(text="Line Item Performance")
    a$xAxis(title=list(text=ifelse(x_axis=="CTR", "CTR (%)", paste0(x_axis, " ($)"))))
    a$yAxis(title=list(text=ifelse(y_axis=="CTR", "CTR (%)", paste0(y_axis, " ($)"))))
    for (item in plot.data$LineItem) {
      tmp <- subset(plot.data, plot.data$LineItem==item)
      a$series(name=item, data=lapply(1:nrow(tmp), function(i) {list(tmp[i, x_axis], tmp[i, y_axis], tmp[i, size])}))
    }
    return(a)
  })
  
  output$time_dim <- renderChart2({
    time_p <- input$time_p
    input.data <- raw_data()
    
    data <- input.data[Date>="2015-01-05" & Date<="2015-02-01",
                       list(
                         Impressions = sum(Impressions),
                         Clicks = sum(Clicks),
                         ViewThroughs = sum(ViewThroughs),
                         Spend = sum(Spend)
                       ),
                       by=list(Group, Date)]
    data[, CTR:=round(100*Clicks/Impressions, 4)]
    data[, VTR:=round(100*ViewThroughs/Impressions, 4)]
    data[, CPM:=round(1000*Spend/Impressions, 2)]
    data[, CPC:=round(Spend/Clicks, 2)]
    data[, CPV:=round(Spend/ViewThroughs, 2)]
    data <- data[order(Date)]
    plot.data <- data.frame(dcast(melt(data, id.vars=c("Group", "Date"), measure.vars=time_p), Date~Group))
    rownames(plot.data) <- plot.data$Date
    plot.data <- subset(plot.data, select=-Date)
    
    a <- Highcharts$new()
    a$chart(height=768, width=1024, zoomType="xy")
    a$title(text="Daily Test Line Items Performance")
    a$xAxis(categories=rownames(plot.data), labels=list(rotation=-45))
    a$yAxis(title=list(text=ifelse(time_p %in% c("CTR", "VTR"), paste0(time_p, " (%)"), time_p)))
    a$data(plot.data)
    return(a)
  })
  
  output$item_playback <- renderChart2({
    x_axis <- input$item_playback_x
    y_axis <- input$item_playback_y
    size <- input$item_playback_size
    slider_day <- input$item_playback_day
    input.data <- raw_data()
    
    days <- data.table(Date=seq.Date(as.Date("2015-01-05"), as.Date("2015-02-01"), by=1), id=1:difftime(as.Date("2015-02-02"), as.Date("2015-01-05")))
    setkey(days, Date)
    data <- input.data[Clicks>0,
                       list(
                         Impressions = sum(Impressions),
                         Clicks = sum(Clicks),
                         Spend = round(sum(Spend))
                       ),
                       by=list(Date, LineItem)]
    data[, CTR:=round(100*Clicks/Impressions, 4)]
    data[, CPC:=round(Spend/Clicks, 2)]
    data[, CPM:=round(1000*Spend/Impressions, 2)]
    data[, Date:=as.Date(Date)]
    setkey(data, Date)
    merge.data <- days[data]
    plot.data <- data.frame(merge.data[id==slider_day, c("LineItem", x_axis, y_axis, size), with=FALSE])
    
    if (x_axis=="CTR" & y_axis=="CTR") {
      tooltip_text <- paste0(x_axis, ": {point.x}%, ", y_axis, ": {point.y}%, ", size, ": {point.z}")
    } else if (x_axis!="CTR" & y_axis!="CTR") {
      tooltip_text <- paste0(x_axis, ": ${point.x}, ", y_axis, ": ${point.y}, ", size, ": {point.z}")
    } else if (x_axis=="CTR" & y_axis!="CTR") {
      tooltip_text <- paste0(x_axis, ": {point.x}%, ", y_axis, ": ${point.y}, ", size, ": {point.z}")
    } else {
      tooltip_text <- paste0(x_axis, ": ${point.x}, ", y_axis, ": {point.y}%, ", size, ": {point.z}")
    }
    
    a <- Highcharts$new()
    a$chart(type="bubble", height=768, width=1024, zoomType="xy")
    a$plotOptions(bubble=list(tooltip=list(pointFormat=tooltip_text), animation=list(duration=200)))
    a$title(text="Line Item Performance")
    a$xAxis(title=list(text=ifelse(x_axis=="CTR", "CTR (%)", paste0(x_axis, " ($)"))))
    a$yAxis(title=list(text=ifelse(y_axis=="CTR", "CTR (%)", paste0(y_axis, " ($)"))))
    for (item in plot.data$LineItem) {
      tmp <- subset(plot.data, plot.data$LineItem==item)
      a$series(name=item, data=lapply(1:nrow(tmp), function(i) {list(tmp[i, x_axis], tmp[i, y_axis], tmp[i, size])}))
    }
    return(a)
  })
  
})