rm(list=ls())
library(readr)
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(maps)
library(shiny)
library(sf)
library(leaflet)

library_2019 <- readxl::read_excel("PRELIMINARY_2019_public_library_service_data.xlsx", sheet = "2019 PRELIMINARY DATA", skip = 1) %>%
  slice(-1) %>%
  rename_all(funs(str_replace_all(., "[^a-zA-Z0-9]+", "_"))) %>%
  mutate(across(.cols=-c(County, Public_Library), as.numeric), 
         year = 2019)

library_2018 <- readxl::read_excel("PRELIMINARY_2018_public_library_service_data.xlsx", sheet = "2018 WI Public Library Data", skip = 1) %>%
  slice(-1) %>%
  rename_all(funs(str_replace_all(., "[^a-zA-Z0-9]+", "_"))) %>%
  mutate(across(.cols=-c(County, Public_Library), as.numeric), 
         year = 2018)

library_2017 <- readxl::read_excel("2017_public_library_service_data_by_library.xlsx", sheet = "2017 WI Public Library Data", skip = 1) %>%
  slice(-1) %>%
  rename_all(funs(str_replace_all(., "[^a-zA-Z0-9]+", "_"))) %>%
  mutate(across(.cols=-c(County, Public_Library), as.numeric), 
         year = 2017)

library_2016 <- readxl::read_excel("2016_public_library_service_data_by_library.xlsx", sheet = "2016 WI Public Library Data", skip = 1) %>%
  slice(-1) %>%
  rename_all(funs(str_replace_all(., "[^a-zA-Z0-9]+", "_")))


# Tidy-ing Data 
identification_columns <- c('Public_Library', 'County')

income_columns <- c('Municipal_Appropriation', 'Home_County_Appropriation', 'Other_County_Payments_Adjacent_Counties', 'State_Funds', 'Federal_Funds', 'Contract_Income', 'Total_Income')

expenditure_columns <- c('Salaries_Wages', 'Employee_Benefits', 'Print_Materials', 'Electronic_format', 'Audiovisual_Materials', 'All_Other_Materials', 'Contracted_Services', 'Other_Operating_Expenditures', 'Total_Operating_Expenditures')

library_2016 <- library_2016 %>%
  select(all_of(identification_columns), all_of(income_columns), all_of(expenditure_columns)) %>%
  mutate(across(.cols=-c(County, Public_Library), as.numeric), year = 2016)


library_2017 <- library_2017 %>%
  select(all_of(identification_columns), all_of(income_columns), all_of(expenditure_columns)) %>%
  mutate(across(.cols=-c(County, Public_Library), as.numeric), year = 2017) %>%
  drop_na()

library_2018 <- library_2018 %>%
  select(all_of(identification_columns), all_of(income_columns), all_of(expenditure_columns)) %>%
  mutate(across(.cols=-c(County, Public_Library), as.numeric), year = 2018)

library_2019 <- library_2019 %>%
  select(all_of(identification_columns), all_of(income_columns), all_of(expenditure_columns)) %>%
  mutate(across(.cols=-c(County, Public_Library), as.numeric), year = 2019)

library_budgets <- library_2016 %>%
  full_join(library_2017, by=names(library_2016)) %>%
  full_join(library_2018, by=names(library_2016)) %>%
  full_join(library_2019, by=names(library_2016))

# Shiny Preparation 
total_library_budgets <- library_budgets %>%
  select(all_of(identification_columns), all_of(income_columns)) %>%
  summarise(across(.cols=-c(County, Public_Library), sum)) %>%
  mutate(across(.cols=all_of(income_columns), ~ round((.x/Total_Income)*100, 1)),
         County = "All")

total_library_expenditures <- library_budgets %>%
  select(all_of(identification_columns), all_of(expenditure_columns)) %>%
  summarise(across(.cols=-c(County, Public_Library), sum)) %>%
  mutate(across(.cols=all_of(expenditure_columns), ~ round((.x/Total_Operating_Expenditures)*100, 1)),
         County = "All")

budget_data <- library_budgets %>%
  group_by(County) %>%
  select(all_of(identification_columns), all_of(income_columns)) %>%
  summarise(across(.cols=all_of(income_columns), sum)) %>%
  mutate(across(.cols=all_of(income_columns), ~ round((.x/Total_Income)*100, 1))) 

expense_data <- library_budgets %>%
  group_by(County) %>%
  select(all_of(identification_columns), all_of(expenditure_columns)) %>%
  summarise(across(.cols=all_of(expenditure_columns), sum)) %>%
  mutate(across(.cols=all_of(expenditure_columns), ~ round((.x/Total_Operating_Expenditures)*100, 1)))
  
  
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