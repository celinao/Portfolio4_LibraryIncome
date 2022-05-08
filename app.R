rm(list=ls())
library(tidyverse)
library(maps)
library(shiny)
library(sf)
library(leaflet)
library(scales)
library(patchwork)
library(RColorBrewer)

total_library_budgets <- read_csv("derived_data//total_library_budgets.csv")
total_library_expenditures <- read_csv("derived_data//total_library_expenditures.csv")
budget_data <- read_csv("derived_data//budget_data.csv")
expense_data <- read_csv("derived_data//expense_data.csv")
library_expense <- read_csv("derived_data//library_expense.csv")

# Shape Data 
shape <- tigris::counties(state = "WI", class = "sf")  %>%
  select(NAME, COUNTYNS) %>%
  left_join(library_expense, by = c("NAME" = "County" )) %>%
  mutate(color = "light grey")

# Leaflet Color Palette 
pal <- colorQuantile("PuBuGn", domain = shape$Total_Operating_Expenditures, n = 9)

# Leaflet detailed labels 
labels <- sprintf(
  "<strong>%s</strong><br/>Total Operating Expenditures: $%s",
  shape$NAME, comma(shape$Total_Operating_Expenditures)
) %>% lapply(htmltools::HTML)
  
getHist <- function(selected_county){
  budget <- budget_data %>%
    group_by(County) %>%
    filter(sum(selected_county == County)%%2 == 1)
  
  expense <- expense_data %>%
    group_by(County) %>%
    filter(sum(selected_county == County)%%2 == 1)
  
  # Reformat labels and pivot data 
  budget_hist <- budget %>%
    full_join(total_library_budgets, names(budget)) %>%
    pivot_longer(cols=Municipal_Appropriation:Contract_Income, names_to="Category", values_to="Percent") %>%
    mutate(Category = str_replace_all(Category, "_", "\n")) %>%
    # Create barplot 
    ggplot() +
    geom_bar(aes(x=Percent, y = Category, fill = County), stat="identity", position = position_dodge())+ 
    labs(title = "Library Income Breakdown")+ 
    scale_fill_brewer(palette = "Paired") + 
    scale_x_continuous(expand = c(0,0)) +
    theme(panel.grid = element_blank())
  
  # Reformat labels and pivot data 
  expense_hist <- total_library_expenditures %>%
    full_join(expense, names(expense)) %>%
    pivot_longer(cols=Salaries_Wages:Other_Operating_Expenditures, names_to="Category", values_to="Percent") %>%
    mutate(Category = str_replace_all(Category, "_", "\n")) %>%
    # Create barplot 
    ggplot() +
    geom_bar(aes(x=Percent, y = Category, fill = County), stat="identity", position = position_dodge()) + 
    labs(title = "Library Expenses Breakdown")+
    scale_fill_brewer(palette = "Paired") + 
    scale_x_continuous(expand = c(0,0)) +
    theme(panel.grid = element_blank(), 
          axis.title.y = element_blank())
  
  return(budget_hist + expense_hist + plot_layout(guides = "collect") & theme(legend.position = "top"))
}

color_shape <- function(selected_counties){
  shape <- shape %>%
    group_by(NAME) %>%
    mutate(color = ifelse((sum(selected_counties == NAME)%%2 == 1), "red" ,"light grey"))
  return(shape)
}

# Shiny App
ui <- fluidPage(
  titlePanel("Library Income and Expense Breakdown"),
  
  fluidRow(
    # column(1,),
    
    column(2, 
           h3("Overview"), 
           p("This interface gathers the total expenditures and income sources of public libraries across Wisconsin."),
           br(), 
           p("You can use it to see which countries are in the highest expenditures and select counties to examine how their 
             broken down budgets relate to other counties in Wisconsins. ")
           ), 
    
    column(9, 
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
   
server <- function(input, output) {
  
  clicked_county <- reactive({
    shape$NAME[shape$COUNTYNS == input$map_shape_click$id]
    })
  
  selected_counties <- reactiveValues()

  output$map <- renderLeaflet({
    # Add outline 
    shape <- color_shape(selected_counties$counties)
    
    # Create leaflet map 
    leaflet() %>% 
      addProviderTiles("Stamen.Toner") %>% 
      addPolygons(data = shape, 
                  fillColor = ~pal(shape$Total_Operating_Expenditures), 
                  color = shape$color,
                  fillOpacity = 0.7, 
                  weight = 3,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666"), 
                  layerId = ~COUNTYNS, 
                  label = labels, 
                  ) %>%
      addLegend(pal = pal, values = shape$Total_Operating_Expenditures, opacity = 0.7, title = "Total Expenditure", position = "bottomright")
  })
  
  observe({ 
    # Add clicked county to list 
    selected_counties$counties <- c(isolate(selected_counties$counties), clicked_county())
    
    # Get histograms 
    output$budgets <- renderPlot({
      getHist(selected_counties$counties)
    })
  })
}

shinyApp(ui, server)