library(shiny)
library(shinythemes)
library(rCharts)

shinyUI(
  navbarPage(
    title="Performance Dashboard",
    theme=shinytheme("flatly"),
    tabPanel(
      "Ads Level",
      sidebarLayout(
        sidebarPanel(
          width=3,
          dateInput("date", label="Enter Date: ", value=(Sys.Date()-1), min="2014-12-27", max=(Sys.Date()-1))
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Campaign",
              fluidPage(
                fluidRow(
                  column(4, br(), br(), tableOutput("campaign_stats")),
                  column(4, showOutput("campaign_ctr", "highcharts")),
                  column(4, showOutput("campaign_vtr", "highcharts"))
                ),
                fluidRow(
                  column(4, showOutput("campaign_cpm", "highcharts")),
                  column(4, showOutput("campaign_cpc", "highcharts")),
                  column(4, showOutput("campaign_cpv", "highcharts"))
                )
              )
            ),
            tabPanel("Line Item", showOutput("item_ctr_cpc", "highcharts")),
            tabPanel("Performance Table", dataTableOutput("item_stats"))
          )
        )
      )
    ),
    tabPanel(
      "Time Dimension",
      sidebarLayout(
        sidebarPanel(
          width=3,
          selectInput("time_p", label="Primary Axis", choices=c("Impressions", "Clicks", "ViewThroughs", "Spend", "CTR", "CPM", "CPC"), selected = "CPC")
        ),
        mainPanel(showOutput("time_dim", "highcharts"))
      )
    )
  )
)

