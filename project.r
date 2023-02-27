# Halemba Mikolaj

options(width=250)
rm(list=ls())

library(eurostat)
library(dplyr)
library(tidyverse)
library(shiny)
library(plotly)
library(googleVis)
library(data.table)

# Download EU countries 
filtered_eu_countries <- eu_countries %>% filter(code != "UK")

#Create USER INTERFACE 
ui <- fluidPage(
  sidebarPanel(
    actionButton("data_download", "Download data"),
    radioButtons("sex", "Sex", c("male", "female")),
    uiOutput("render_slider"),
    checkboxGroupInput("countries", "Choose country", filtered_eu_countries$name),
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Tabel", tableOutput('table')), 
      tabPanel("Maps", htmlOutput("map")),
      tabPanel("Visualization", plotlyOutput("plot"))
    )
  )
)

# Function to load data from location
load_data <- function() {
  csv_data <- read.csv(file = file.path("demo_r_mwk_ts.tsv.gz"), sep = "\t", dec = ".", header = T)
  df_download <- as.data.frame(rbindlist(lapply(eu_countries$code, function(country){
    x <- t(csv_data[grep(country,csv_data[,1]),])
    x <- x[-1,]
    options(warn=-1)
    x <- data.frame(week = gsub("X","",rownames(x)), 
                    female = as.integer(gsub(" p","",x[,1])),
                    male = as.integer(gsub(" p","",x[,2])),
                    total = as.integer(gsub(" p","",x[,3])),
                    country = country
    )
    options(warn=0)
    rownames(x) <- NULL
    x <- x[order(x$week),]
    
    return(x)
  })))
}

# BACKEND SIDE
server <- function(input, output) {

  observeEvent(input$data_download, {
    download.file(
      url = "https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/demo_r_mwk_ts.tsv.gz",
      destfile = file.path(".", "demo_r_mwk_ts.tsv.gz"), 
      method = "curl"
    )
  })
  
  # Load data
  data <- load_data
  slider_values <- reactive({
    min_year<- as.integer(input$date_range[1])
    max_year <- as.integer(input$date_range[2])
    c(min_year, max_year)
  })
  
  data_processed <- reactive({
    min_year<- slider_values()[1]
    max_year <- slider_values()[2]
    
    data() %>%
    mutate(year = as.numeric(str_sub(week,1,4))) %>%
    filter(between(year, min_year, max_year)) %>%
    transmute(country = eu_countries$name[match(country, eu_countries$code)],
              sex = input$sex,week = week,number = case_when(input$sex == "female" ~ female,
                                                             input$sex == "male" ~ male
                                                             )
            )
  })
  
  processed_data <- reactive({
    min_year<- slider_values()[1]
    max_year <- slider_values()[2]
    
    countries_code <- eu_countries %>% filter(name %in% input$countries)
    
    data() %>% filter(country %in% countries_code$code) %>%
      mutate(year = substr(week,1,4),week_short = substr(week,6,7)) %>%
      filter(between(year, min_year, max_year)) %>%
      transmute(country = eu_countries$name[match(country, eu_countries$code)],
        sex = input$sex,week = week,number = case_when(input$sex == "male" ~ male,
                                                       input$sex == "female" ~ female
                                                       )
        )
  })
  
  output$table = renderTable({
    processed_data()
  })
  
  output$render_slider <- renderUI({
    years_range <- regmatches(data()$week, regexpr("^[0-9]{4}", data()$week))
    min <- as.integer(min(years_range))
    max <- as.integer(max(years_range))
    
    sliderInput("date_range","Choose date",min = min,max = max,
      value = c(min, max),
      sep = "",
      step = 1
    )
  })
  
  output$map <- renderGvis({
    map_data <- data_processed() %>%
      replace(is.na(.), 0) %>%
      group_by(country) %>%
      summarise(number = sum(number)) 
    gvisGeoChart(map_data, "country", "number",options=list(region="150",width=850,height=850))
  })
  
  output$plot <- renderPlotly({
    plot_data <- processed_data() %>%
      replace(is.na(.), 0) %>%
      mutate(year = substr(week,1,4)) %>%
      group_by(country, year) %>%
      summarise(number = sum(number))
    
    plot_ly(data = plot_data, x=~year, y=~number,type="scatter",mode='lines',color = ~country) %>%
      layout(title = "Time series",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Number", tickformat = ",d"))
  })
}

app <- shinyApp(ui = ui, server = server)
runApp(app)
