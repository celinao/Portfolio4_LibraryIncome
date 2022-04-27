rm(list=ls())
library(tidyverse)
library(maps)
library(shiny)
library(sf)
library(leaflet)
library(scales)
library(patchwork)

total_library_budgets <- read_csv("derived_data//total_library_budgets.csv")
total_library_expenditures <- read_csv("derived_data//total_library_expenditures.csv")
budget_data <- read_csv("derived_data//budget_data.csv")
expense_data <- read_csv("derived_data//expense_data.csv")
library_expense <- read_csv("derived_data//library_expense.csv")

# Leaflet Color Palette 
color_palette = c("#02818A", "#014636")
pal <- colorQuantile("PuBuGn", domain = library_expense$Total_Operating_Expenditures, n = 10)

# Leaflet detailed labels 
labels <- sprintf(
  "<strong>%s</strong><br/>Average Operating Income: $%s",
  library_expense$County, comma(library_expense$Total_Operating_Expenditures)
) %>% lapply(htmltools::HTML)
  
getHist <- function(selected_county){
  budget <- budget_data %>%
    filter(County %in% c(str_replace(selected_county, " County", ""), "!"))
  
  expense <- expense_data %>%
    filter(County %in% c(str_replace(selected_county, " County", ""), "All")) 
  
  # Reformat labels and pivot data 
  budget_hist <- budget %>%
    full_join(total_library_budgets, names(budget)) %>%
    pivot_longer(cols=Municipal_Appropriation:Contract_Income, names_to="Category", values_to="Percent") %>%
    mutate(Category = str_replace_all(Category, "_", " \n")) %>%
    # Create barplot 
    ggplot() +
    geom_bar(aes(x=Percent, y = Category, fill = County), stat="identity", position = position_dodge())+ 
    labs(title = paste(selected_county, "Library Income Breakdown"))+ 
    scale_fill_manual(values=color_palette) + 
    scale_x_continuous(expand = c(0,0)) +
    theme(panel.grid = element_blank())
  
  # Reformat labels and pivot data 
  expense_hist <- total_library_expenditures %>%
    full_join(expense, names(expense)) %>%
    pivot_longer(cols=Salaries_Wages:Other_Operating_Expenditures, names_to="Category", values_to="Percent") %>%
    mutate(Category = str_replace_all(Category, "_", " \n")) %>%
    # Create barplot 
    ggplot() +
    geom_bar(aes(x=Percent, y = Category, fill = County), stat="identity", position = position_dodge()) + 
    labs(title = paste(selected_county, "Library Expenses Breakdown"))+ 
    scale_fill_manual(values=color_palette) + 
    scale_x_continuous(expand = c(0,0)) +
    theme(panel.grid = element_blank(), 
          axis.title.y = element_blank())
  
  return(budget_hist + expense_hist + plot_layout(guides = "collect") & theme(legend.position = "bottom"))
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
    plotOutput("budgets")
  ), 
  hr()
)


# Define server logic       
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Stamen.Toner") %>% 
      addPolygons(data = shape, 
                  fillColor = ~pal(library_expense$Total_Operating_Expenditures), 
                  color = "grey",
                  fillOpacity = 0.7, 
                  weight = 1,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666"), 
                  layerId = ~COUNTYNS, 
                  label = labels, 
                  ) %>%
      addLegend(pal = pal, values = library_expense$Total_Operating_Expenditures, opacity = 0.7, title = NULL,
                position = "bottomright")
      
  })
  
  observe({ 
    event <- input$map_shape_click
    output$cnty <- renderText(shape$NAMELSAD[shape$COUNTYNS == event$id])
    
    output$budgets <- renderPlot({
      getHist(shape$NAMELSAD[shape$COUNTYNS == event$id])
    })
  })
}

shinyApp(ui, server)