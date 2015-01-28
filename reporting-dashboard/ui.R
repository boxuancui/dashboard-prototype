library(shiny)
library(shinythemes)
library(rHighcharts)

shinyUI(
  navbarPage(
    title="Performance Dashboard",
    theme=shinytheme("flatly"),
    tabPanel(
      "Ads Level",
      sidebarLayout(
        sidebarPanel(
          width=3,
          dateInput("date", label="Enter Date: ", value=(Sys.Date()-2), min="2014-12-27", max=(Sys.Date()-1))
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Campaign",
              fluidPage(
                fluidRow(
                  column(4, br(), br(), tableOutput("campaign_stats")),
                  column(4, chartOutput("campaign_ctr")),
                  column(4, chartOutput("campaign_vtr"))
                ),
                fluidRow(
                  column(4, chartOutput("campaign_cpm")),
                  column(4, chartOutput("campaign_cpc")),
                  column(4, chartOutput("campaign_cpv"))
                )
              )
            ),
            tabPanel("Line Item", chartOutput("item_ctr_cpc")),
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
          selectInput("time_p", label="Primary Axis", choices=c("Impressions", "Clicks", "ViewThroughs", "Spend", "CTR", "CPM", "CPC"), selected = "CTR")
        ),
        mainPanel(chartOutput("time_dim"))
      )
    )
  )
)

