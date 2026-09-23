#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#



library(shiny)
library(shinydashboard)
library(readxl)
library(DT)
library(leaflet)
library(dplyr)
library(rlang)
library(readr)


collars <- read.csv2("Deployment_ Collar Tracking(2023-2026)(Collar Tracking).csv",stringsAsFactors = FALSE )

# Check that the data loaded correctly
head(collars)
str(collars)
names(collars)

# User Interface


ui <- dashboardPage(
  
  
  # Dashboard Header
  
  
  dashboardHeader(
    
    title = "Wildlife Collar Monitoring Portal"
    mainPanel(
      # 1. Create a placeholder slot for the table
      table = "Collar Inventory"
    )
    
  ),
  
  
  # Sidebar
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Dashboard",
        tabName="dashboard",
        icon=icon("dashboard")
      ),
      
      menuItem(
        "Collar Inventory",
        tabName="inventory",
        icon=icon("table")
      ),
      
      menuItem(
        "Interactive Map",
        tabName="map",
        icon=icon("globe")
      )
      
    )
    
  ),
  
  
  # Dashboard Body
  
  
  dashboardBody(
    
    tabItems(
      
      
      # Dashboard Page
      
      
      tabItem(
        
        tabName="dashboard",
        
        fluidRow(
          
          valueBoxOutput("total"),
          
          valueBoxOutput("active"),
          
          valueBoxOutput("inactive"),
          
          valueBoxOutput("redeployed")
          
        )
        
      ),
      
      
      # Inventory Page
      
      
      tabItem(
        
        tabName="inventory",
        
        DTOutput("table")
        
      ),
      
      
      # Map Page
      
      
      tabItem(
        
        tabName="map",
        
        leafletOutput("map",height=700)
        
      )
      
    )
    
  )
  
)


# Server


server <- function(input, output){
  
  
  # Dashboard Statistics
  
  output$total <- renderValueBox({
    
    valueBox(
      
      value=nrow(collars),
      
      subtitle="Total Collars",
      
      icon=icon("paw"),
      
      color="green"
      
    )
    
  })
  
  
  # Active collars
  
  
  output$active <- renderValueBox({
    
    valueBox(
      
      value=sum(collars$State=="Active"),
      
      subtitle="Active",
      
      icon=icon("check"),
      
      color="blue"
      
    )
    
  })
  
  
  # Deactivated collars
  
  
  output$inactive <- renderValueBox({
    
    valueBox(
      
      value=sum(collars$State=="Deactivated"),
      
      subtitle="Deactivated",
      
      icon=icon("times"),
      
      color="red"
      
    )
    
  })
  
  # Redeployed collars
  
  
  output$redeployed <- renderValueBox({
    
    valueBox(
      
      value=sum(collars$State=="Redeployed"),
      
      subtitle="Redeployed",
      
      icon=icon("star"),
      
      color="yellow"
      
    )
    
  })
  
  
  # Interactive Table
  
  
  output$table <- renderDT({
    
    datatable(
      
      collars,
      
      filter="top",
      
      options=list(
        
        pageLength=15,
        
        scrollX=TRUE
        
      )
      
    )
    
  })
  
  
  # Interactive Map
  
  
  output$map <- renderLeaflet({
    
    leaflet(collars) %>%
      
      addTiles() %>%
      
      addCircleMarkers(
        
        lng=~"GPS X",
        
        lat=~"GPS Y",
        
        radius=7,
        
        popup=~paste(
          
          "<b>Collar ID:</b>","SAT ID",
          
          "<br><b>Individual:</b>",Individual,
          
          "<br><b>Species:</b>",Species,
          
          "<br><b>Sex:</b>",Sex,
          
          "<br><b>Brand:</b>",Brand,
          
          "<br><b>State:</b>","State",
          
          "<br><b>Last Fix:</b>","LastFixDate"
          
        )
        
      )
    
  })
  
}

# Run Shiny App
shinyApp(ui = ui, server = server)

