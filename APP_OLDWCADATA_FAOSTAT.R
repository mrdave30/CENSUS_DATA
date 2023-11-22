# Load necessary libraries
library(plotly)
library(dplyr)
library(readxl)
library(shiny)
require(DT)

# Load the data
data <- read_excel("Area.xlsx")
data2 <- read_excel("holdings.xlsx")

# Filter and Calculate Percentage Difference
filtered_data <- data %>%
  arrange(Area) %>%
  mutate(Percentage_Difference = ((Value_olddata - Value_FAOSTAT) / Value_FAOSTAT) * 100)
filtered_data2 <- data2 %>%
  arrange(Area) %>%
  mutate(Percentage_Difference = ((Value_olddata - Value_FAOSTAT) / Value_FAOSTAT) * 100)

# UI for the Shiny app
ui <- fluidPage(
  titlePanel("Historical Trend Comparison of Old WCA data and FAOSTAT data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("chart_type", "Select item (Area or holding):", 
                  choices = c("Area", "Holdings")),
      selectInput("country", "Select Country:", 
                  choices = NULL),  # Initially empty
      sliderInput("percentage_range", "Percentage Difference Range:", 
                  min = 0, max = 700, 
                  value = c(1, 700))
    ),
    mainPanel(
      plotlyOutput("line_chart"),
      h3("Table of census rounds for selected country"),
      DTOutput("countries_table")
    )
  )
)

# Server logic for the Shiny app
server <- function(input, output, session) {
  # Calculate the maximum absolute percentage difference
  max_abs_percentage <- max(abs(filtered_data$Percentage_Difference), abs(filtered_data2$Percentage_Difference))
  
  # Update available country choices based on selected percentage range and chart type
  observe({
    if (input$chart_type == "Area") {
      updated_filtered_data <- filtered_data %>%
        filter(abs(Percentage_Difference) >= input$percentage_range[1] & abs(Percentage_Difference) <= input$percentage_range[2])
    } else if (input$chart_type == "Holdings") {
      updated_filtered_data <- filtered_data2 %>%
        filter(abs(Percentage_Difference) >= input$percentage_range[1] & abs(Percentage_Difference) <= input$percentage_range[2])
    }
    
    updateSelectInput(session, "country", choices = unique(updated_filtered_data$Area))
  })
  
  # Filter data based on selected country
  selected_data <- reactive({
    if (!is.null(input$country)) {
      if (input$chart_type == "Area") {
        filtered_data %>%
          filter(Area == input$country)
      } else if (input$chart_type == "Holdings") {
        filtered_data2 %>%
          filter(Area == input$country)
      }
    }
  })
  
  # Create the line chart and table
  output$line_chart <- renderPlotly({
    if (!is.null(input$country)) {
      fig <- plot_ly(data = selected_data(), x = ~as.factor(Censusround)) %>%
        add_trace(y = ~Value_olddata, name = "OldData", type = "scatter", mode = "lines+markers") %>%
        add_trace(y = ~Value_FAOSTAT, name = "FAOSTAT", type = "scatter", mode = "lines+markers") %>%
        layout(title = paste("Historical Trend Comparison for", input$country),
               xaxis = list(title = "Census Round"),
               yaxis = list(title = "Value"))
      
      fig
    }
  })
  
  # Create a table with selected data
  output$countries_table <- renderDT({
    if (!is.null(input$country)) {
      selected_data() %>%
        mutate(Percentage_Difference_Abs = abs(Percentage_Difference)) %>%
        select(Area, Censusround, Value_olddata, Value_FAOSTAT, Percentage_Difference_Abs) %>%
        datatable(
          options = list(columnDefs = list(list(targets = c(3:5), 
                                                render = JS("function(data, type, full, meta) { return parseFloat(data).toFixed(1); }"))))
        )
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)
