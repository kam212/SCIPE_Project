library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)

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
      radioButtons("radio", label = h5("Choose Marker Shape"),
                   choices = list("Circle"= 1, "Triangle" = 2),
                   selected = 1),
      uiOutput("column"),
      selectInput("Dataset", "Choose Dataset", "NUGIS"),
      selectInput("Year", "Choose Year", "1990", "1987"),
      sliderInput("LegendSize", "Title Size", value = 12, min = 12, max = 50),
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
  output$contents = renderTable({
    df <- newData()
    return(df)
  })
 
  output$stateMap <- renderPlot({
    df <- newData
    
    #Preset map of United States
    
  
    #Downloader, png type file
    
    rm(list=ls())
   
    #Presenting Bar Graph Data 
    
    ## load cropland area data
    load(file="/Users/katherine/Documents/ha_state.rData")
    
    ## load cross-walk data
    library(readxl)
    crop0 <- as.data.frame(read_xlsx("/Users/katherine/Documents/us_cropgrids.xlsx",
                                     sheet = "us_cropgrids"))
    
    ## remove duplicates
    crop1 <- subset(crop0,TOTAL==2&DUPLICATE==2)
    
    ## remove undefined FAO group
    crop <- subset(crop1,!is.na(FAO_GROUP))
    
    ha1 <- merge(
      ha_state_df,
      crop[,c("crop_id","FAO_GROUP","Temporrary/Permanent")],
      by="crop_id")
    
    
    load(file="/Users/katherine/Documents/conUS.rData")
    library(ggplot2)
    ## map for a given year and crop
    df_ <- subset(ha1,year==2000&crop_id==1)
    
    df2_ <- merge(df_,crop1[,1:4],by="crop_id")
    title_ <- with(df2_,paste(commodity_desc,class_desc,util_practice_desc,FAO_GROUP))
    shp_ <- merge(conUS,df_,by.x="STATEFP",by.y="state_fips_code")
    range(df_$kHA)
    ggplot(data=shp_)+geom_sf(aes(fill=kHA))+
      ggtitle(title_)+
      guides(fill=guide_legend(title="HA(x1,000)"))
    
    ## time series
    ## for a given state and crop
    df_ <- subset(ha1,state_fips_code=="01"&crop_id==3)
    df2_ <- merge(df_,crop1[,1:4],by="crop_id")
    title_ <- with(df2_,paste(commodity_desc,class_desc,util_practice_desc,FAO_GROUP))
    ggplot(data=df2_,aes(x=year,y=kHA))+geom_line()+
      ggtitle(title_)+ylab("HA (x1,000)")
    
    ## bar plots
    ## for a given state and year, over crop groups
    df_ <- subset(ha1,state_fips_code=="01"&year==2000)
    df2_ <- aggregate(kHA~FAO_GROUP,data=df_,FUN=sum)
    ggplot(data=df2_,
           aes(x=reorder(FAO_GROUP,-kHA),y=kHA))+
      geom_bar(stat="identity")+
      coord_flip()+xlab("FAO Group")+ylab("HA(x1,000)")

    
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
