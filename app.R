rm(list=ls())
library(tidyverse)
library(maps)
library(shiny)
library(sf)
library(leaflet)

total_library_budgets <- read_csv("derived_data//total_library_budgets.csv")
total_library_expenditures <- read_csv("derived_data//total_library_expenditures.csv")
budget_data <- read_csv("derived_data//budget_data.csv")
expense_data <- read_csv("derived_data//expense_data.csv")
  
getHist <- function(selected_county){
  budget_hist <- budget_data %>%
    filter(County == str_replace(selected_county, " County", ""))
  
  create_hist <- budget_hist %>%
    full_join(total_library_budgets, names(budget_hist)) %>%
    pivot_longer(cols=Municipal_Appropriation:Contract_Income, names_to="Category", values_to="Percent") %>%
    ggplot() +
    geom_bar(aes(x=Percent, y = Category, fill = County), stat="identity", position = position_dodge())+ 
    labs(title = paste(selected_county, "Library Income Breakdown"))+ 
    scale_y_discrete(expand = c(0,0)) + 
    theme(panel.grid = element_blank())
  return(create_hist)
}

getHistEx <- function(selected_county){
  expense_hist <- expense_data %>%
    filter(County == str_replace(selected_county, " County", ""))
  
  create_hist <- total_library_expenditures %>%
    full_join(expense_hist, names(expense_hist)) %>%
    pivot_longer(cols=Salaries_Wages:Other_Operating_Expenditures, names_to="Category", values_to="Percent") %>%
    ggplot() +
    geom_bar(aes(x=Percent, y = Category, fill = County), stat="identity", position = position_dodge()) + 
    labs(title = paste(selected_county, "Library Expenses Breakdown"))+ 
    scale_y_discrete(expand = c(0,0)) + 
    theme(panel.grid = element_blank())
  return(create_hist)
}


shape <- tigris::counties(state = "WI", class = "sf")

# Shiny App
ui <- fluidPage(
  titlePanel("Library Income and Expense Breakdown"),
  
  fluidRow(
    column(1,),
    
    column(10, 
           leafletOutput("map")
    ), 
    column(1,)
  ),
  
  hr(), 
  
  fluidRow(
    column(6, 
           plotOutput("budgets")
    ),
    column(6, 
           plotOutput("expenses")
    )
  ), 
  hr()
)


# Define server logic       
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Stamen.Toner") %>% 
      addPolygons(data = shape, 
                  fillColor = "aliceblue", 
                  color = "grey",
                  weight = 1,
                  layerId = ~COUNTYNS)
  })
  
  observe({ 
    event <- input$map_shape_click
    output$cnty <- renderText(shape$NAMELSAD[shape$COUNTYNS == event$id])
    
    output$budgets <- renderPlot({
      getHist(shape$NAMELSAD[shape$COUNTYNS == event$id])
    })
    
    output$expenses <- renderPlot({
      getHistEx(shape$NAMELSAD[shape$COUNTYNS == event$id])
    })
  })
}

shinyApp(ui, server)