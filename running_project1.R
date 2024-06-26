rm(list=ls())
library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)

load(file="/Users/katherine/Documents/r_projects/ha_state_forShny.rData")
load(file="/Users/katherine/Documents/r_projects/crop_forShny.rData")
load(file="/Users/katherine/Documents/r_projects/conUS_forShny.rData")

source("/Users/katherine/Documents/r_projects/map_utils.R")
source("/Users/katherine/Documents/r_projects/ts_utils.R")
source("/Users/katherine/Documents/r_projects/bar_utils.R")

fao_group <- levels(ha_state$crop_fao)
all_years <- sort(unique(ha_state$year))
all_states <- conUS$NAME

param_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel(
    "map",
    selectInput("Crop_map","Choose Crop",fao_group,fao_group[1]),
    sliderInput("Year_map", "Choose Year", value=all_years[2], 
                min=all_years[1],max=all_years[length(all_years)],
                step=1)
  ),
  tabPanel(
    "time",
    selectInput("Crop_ts","Choose Crop",fao_group,fao_group[1]),
    selectInput("State_ts","Choose State",all_states,"Maryland")
  ),
  tabPanel(
    "crop",
    selectInput("State_cr","Choose State",all_states,"Maryland"),
    sliderInput("Year_cr", "Choose Year", value=all_years[2], 
                min=all_years[1],max=all_years[length(all_years)])
  )
)
ui <-fluidPage(
  
  titlePanel(""),
  flowLayout(
    tabsetPanel(
      tabPanel("Metadata", "tab2 panel contents"),
      tabPanel("About", "tab3 panel contents")
    )
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      #User input CSV file, lat and longitude needed
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
                
      ),
      
      #Side panel, user controlled markers, size and color
      textAreaInput("color", "Choose marker color in hex code", "#1ff903"),
      radioButtons("view", label = h5("Choose View"),
                   choices = list("map"= "map", "time" = "time",
                                  "crop"= "crop"),
                   inline = TRUE,
                   selected = "map"),
      param_tabs,
      uiOutput("column"),
      # selectInput("Dataset", "Choose Dataset",
      #             c("NUGIS" = "nugis",
      #               "FAO" = "fao",
      #               "P.Cao" = "p.cao")),
      #selectInput("Crop","Choose Crop",fao_group,fao_group[1]),
      #selectInput("State","Choose State",all_states,"Maryland"),
      #sliderInput("Year", "Choose Year", value=all_years[2], 
      #            min=all_years[1],max=all_years[length(all_years)]),
      selectInput("CropDef", "Choose Crop Definitions", 
                  c("Crop Definitions" = "cropdef",
                    "No Crop Definitions" = "nocropdef")),
      hr()
      
      
      
      
    ),
    #Map Display, and data uploaded display
    mainPanel(
      plotOutput("stateMap"),
      
      h4("Data & Tables", align = "center"),
      tableOutput("contents"),
      
    )
    
  )
  
)

server <- function(input, output, session) {
  
  ## update user interface
  observeEvent(input$view,{
    updateTabsetPanel(inputId = "params",selected = input$view)
  })
  
  newData <- reactive({
    infile <- input$file1
    if (is.null(infile)){
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  output$column <- renderUI({
    df <- newData()
    if (is.null(df)) return(NULL)
    numc <- sapply(df, is.numeric)
    items=names(numc[numc])
    selectInput("bubble", items)
  })
  
  #Returning all objects as numeric datatypes 
  
  output$color <- renderUI({
    df <- newData()
    if (is.null(df)) return(NULL)
    numc <-sapply(df, is.numeric)
    items = names(numc[numc])
    names(items) = items
    selectInput("color", items)
  })
  selected_data <- reactive({
    if(input$Dataset %in% Dataset) {
      state %>% 
        filter(Dataset == input$Dataset)
    } else {
      state
    }
  })
  
 
  mapobj <- reactive({
    switch(input$params,
           map=map.helper(input$Year_map,input$Crop_map,
                          data=ha_state,
                          meta=crop,
                          shape=conUS),
           time=ts.helper(input$State_ts,input$Crop_ts,
                          data = ha_state,
                          meta=crop,
                          shape=conUS,
                          all_years = all_years),
           crop=bar.helper(input$State_cr,input$Year_cr,
                           data = ha_state,
                           meta=crop,
                           shape=conUS))
  })
  selectInput$CropDef <- renderUI()
  input$CropDef <- read_file("/Users/katherine/Documents/Meta.png")
})
    
  
  output$stateMap <- renderPlot({
    mapobj()$value
  })
  
  output$contents = renderTable({
    mapobj()$meta
  })
  
  
  output$downloadPlot <- downloadHandler(
    filename = function() {pastel('map-', Sys.Date(), 'png', sep= '')},
    content = function(file) {
      png(file, type = 'cairo')
    },
    contentType = 'image/png'
  )
  
}
shinyApp(ui = ui, server = server)
