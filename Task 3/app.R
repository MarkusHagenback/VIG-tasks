# Task 3 - Designing a Dashboard
rm(list=ls())

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(leaflet)
library(readxl)
library(DT)  
library(writexl)  
library(rsconnect)

# Define the URL to the raw Excel file on GitHub
file_url <- "https://raw.githubusercontent.com/MarkusHagenback/VIG-tasks/main/003%20-%20Visualization/Dry_Run_Data_anonymised_VIG_ESRS.xlsx"

# Define a temporary file path to save the downloaded Excel file
temp_file <- tempfile(fileext = ".xlsx")

# Download the Excel file from GitHub to the temporary file
download.file(file_url, temp_file, mode = "wb")

# Read the downloaded Excel file
data <- read_excel(temp_file, sheet = "ALL_V.35.01.01 (by dtp)")  # Adjust the sheet name if needed

# Renaming columns
data <- data %>%
  rename(
    `ESRS code` = 3,
    `ESRS full name` = 4,
    Entity = 7,
    `Entity full name` = 8,
    Consumption = 11,
    `Involved Entity` = 9,
    `Reporting year` = 10
  )

  # Add 'Entity country' based on the entity full name
data <- data %>%
  mutate(
    `Entity country` = case_when(
      `Entity full name` == "BTA Baltic Insurance Company AAS" ~ "Latvia",
      `Entity full name` == "Compensa Vienna Insurance Group, akcine draudimo bendrove" ~ "Lithuania",
      `Entity full name` == "Ceská podnikatelská pojist'ovna, a.s., Vienna Insurance Group" ~ "Czechia",
      `Entity full name` == "DONAU Versicherung AG Vienna Insurance Group" ~ "Austria",
      `Entity full name` == "InterRisk Towarzystwo Ubezpieczen Spolka Akcyjna Vienna Insurance Group" ~ "Poland",
      `Entity full name` == "Kooperativa, pojist'ovna, a.s. Vienna Insurance Group" ~ "Czechia",
      `Entity full name` == "WIENER RE akcionarsko društvo za reosiguranje, Beograd" ~ "Serbia",
      `Entity full name` == "VIG RE zajist'ovna, a.s." ~ "Czechia",
      `Entity full name` == "VIENNA INSURANCE GROUP AG Wiener Versicherung Gruppe" ~ "Austria",
      `Entity full name` == "WIENER STÄDTISCHE Versicherung AG Vienna Insurance Group" ~ "Austria",
      TRUE ~ NA_character_
    ),
    
    # Add 'Involved Entity country' based on the involved entity full name
    `Involved Entity country` = case_when(
      `Involved Entity` == "BTA Baltic Insurance Company AAS" ~ "Latvia",
      `Involved Entity` == "OÜ LiveOn Paevalille" ~ "Estonia",
      `Involved Entity` == "Compensa Vienna Insurance Group, akcine draudimo bendrove" ~ "Lithuania",
      `Involved Entity` == "Ceská podnikatelská pojist'ovna, a.s., Vienna Insurance Group" ~ "Czechia",
      `Involved Entity` == "DONAU Versicherung AG Vienna Insurance Group" ~ "Austria",
      `Involved Entity` == "InterRisk Towarzystwo Ubezpieczen Spolka Akcyjna Vienna Insurance Group" ~ "Poland",
      `Involved Entity` == "Global Expert, s.r.o." ~ "Czechia",
      `Involved Entity` == "Kooperativa, pojist'ovna, a.s. Vienna Insurance Group" ~ "Czechia",
      `Involved Entity` == "WIENER RE akcionarsko društvo za reosiguranje, Beograd" ~ "Serbia",
      `Involved Entity` == "VIG RE zajist'ovna, a.s." ~ "Czechia",
      `Involved Entity` == "VIENNA INSURANCE GROUP AG Wiener Versicherung Gruppe" ~ "Austria",
      `Involved Entity` == "DV Immoholding GmbH" ~ "Austria",
      `Involved Entity` == "WIENER STÄDTISCHE Versicherung AG Vienna Insurance Group" ~ "Austria",
      `Involved Entity` == "Österreichisches Verkehrsbüro Aktiengesellschaft" ~ "Austria",
      TRUE ~ NA_character_
    )
  )

# Fixing columns corretly
# Fix Reporting year by extracting year part (remove "012." or similar prefixes)
data <- data %>%
  mutate(
    `ESRS full name` = gsub("\u00A0", " ", `ESRS full name`),  # Replace non-breaking spaces
    `ESRS full name` = trimws(`ESRS full name`),  # Trim whitespace
    `Reporting year` = as.numeric(sub(".*\\.", "", `Reporting year`)),
    Consumption = as.numeric(Consumption)  # Ensure Consumption is numeric
  )

# Add country coordinates for mapping
country_coordinates <- data.frame(
  `Entity country` = c("Latvia", "Lithuania", "Czechia", "Austria", "Poland", "Serbia", "Estonia"),
  lat = c(56.8796, 54.6872, 49.8175, 47.5162, 51.9194, 44.0165, 58.5953),
  lon = c(24.6032, 25.2797, 15.4729, 14.5501, 19.1451, 21.0059, 25.0136)
)
country_coordinates <- country_coordinates %>%
  rename(
    `Entity country` = 1
  )

# Join data with coordinates
data <- data %>%
  left_join(country_coordinates, by = "Entity country") %>%
  left_join(country_coordinates, by = c("Involved Entity country" = "Entity country"), suffix = c(".entity", ".involved"))
################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "ESRS Reporting Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      selectInput("entity", "Select Entities:", 
                  choices = c("All", unique(data$Entity)), 
                  selected = NULL, 
                  multiple = TRUE),  # Allow multiple selections
      uiOutput("involved_entity_ui"),  # Dynamic Involved Entity input
      sliderInput("year", "Select Reporting Years:", 
                  min = min(data$`Reporting year`, na.rm = TRUE),  # Ensure NA values are handled
                  max = max(data$`Reporting year`, na.rm = TRUE), 
                  value = c(min(data$`Reporting year`, na.rm = TRUE), max(data$`Reporting year`, na.rm = TRUE)),
                  step = 1),
      selectInput("esrs_code", "Select ESRS Codes:", 
                  choices = c("All", unique(data$`ESRS code`)), 
                  selected = "All"),
      
      # New selectInputs for filtering by Entity country and Involved Entity country
      
      # uiOutput("entity_country_ui"),  # Dynamic Involved Entity input
      
      # selectInput("entity_country", "Select Entity Country:",
      #             choices = c("All", unique(data$`Entity country`)),
      #             selected = "All"),
      
      # uiOutput("involved_entity_country_ui"),  # Dynamic Involved Entity input
      
      # selectInput("involved_entity_country", "Select Involved Entity Country:",
      #             choices = c("All", unique(data$`Involved Entity country`)),
      #             selected = "All"),
      
      # Numeric Inputs for Consumption Range
      fluidRow(
        column(6,
               numericInput("consumption_min", "Minimum value:", 
                            value = min(data$Consumption), min = 0, step = 1000)
        ),
        column(6,
               numericInput("consumption_max", "Maximum value:", 
                            value = max(data$Consumption), min = 1, step = 1000)
        )
      ),
      checkboxInput("filter_zero", "Remove zero values", value = FALSE),  # Checkbox for zero values
      menuItem("Data Overview", tabName = "overview", icon = icon("table")),
      menuItem("Visualizations", tabName = "visuals", icon = icon("chart-bar")),
      menuItem("Graphs", tabName = "graphs", icon = icon("chart-bar"))    
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .btn-download {
          background-color: #28a745; /* Green background */
          color: white; /* White text */
          padding: 10px 20px; /* Padding */
          font-size: 16px; /* Font size */
          border: none; /* No border */
          border-radius: 5px; /* Rounded corners */
          cursor: pointer; /* Pointer cursor on hover */
          transition: background-color 0.3s ease; /* Smooth transition */
        }
        .btn-download:hover {
          background-color: #218838; /* Darker green on hover */
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(title = NULL, width = 8,
                    valueBoxOutput("totalConsumption"),
                    valueBoxOutput("entityCount"),
                    valueBoxOutput("CountryCount"),
                    valueBoxOutput("CountryInvolvedCount"),
                    valueBoxOutput("ESRSCount")
                ),
                box(title = NULL, width = 12,
                    DTOutput("dataTable")),  # Use DTOutput instead of dataTableOutput
                downloadButton("downloadData", "Download Data")
              )),
      tabItem(tabName = "visuals",
              fluidRow(
                box(title = "Consumption Overview", width = 8,
                    plotlyOutput("esrsConsumptionChart")),
                box(title = "Geographic Distribution of Entities", width = 12,
                    leafletOutput("entityMap"))
              )
      ),
      tabItem(tabName = "graphs",
              fluidRow(
                plotlyOutput("esrsComparisonChart", height = "800px")
                )  
                )
              )
              
      )
    )

################################################################################
server <- function(input, output, session) {
  
  # # Reactive UI for Involved Entity Selection based on selected Entity
  # output$involved_entity_ui <- renderUI({
  #   involved_entities <- if ("All" %in% input$entity) {
  #     unique(data$`Involved Entity`)
  #   } else {
  #     # Filter to show only involved entities related to selected entities
  #     filtered_entities <- data %>%
  #       filter(Entity %in% input$entity) %>%
  #       pull(`Involved Entity`) %>%
  #       unique()
  #     # Combine with "All" option
  #     c("All", filtered_entities)
  #   }
  #   
  #   selectInput("involved_entity", "Select Involved Entities:",
  #               choices = involved_entities,
  #               selected = "All",
  #               multiple = TRUE)
  # })
  # 
  # 
  # # Reactive UI for Involved Entity Selection based on selected Entity
  # output$entity_country_ui <- renderUI({
  #   entity_countries <- if ("All" %in% input$entity) {
  #     unique(data$`Entity country`)
  #     
  #   } else {
  #     # Filter to show only countries related to selected entities
  #     filtered_countries <- filtered_entities %>%
  #       filter(Entity %in% input$entity) %>%
  #       pull(`Entity country`) %>%
  #       unique()
  #     # Combine with "All" option
  #     c("All", filtered_countries)
  #   }
  #   
  #   selectInput("entity_country", "Select Entity Country:",
  #               choices = entity_countries,
  #               selected = "All")
  # }) 
  
  # Reactive expression for filtered involved entities based on selected entity
  filtered_involved_entities <- reactive({
    if ("All" %in% input$entity) {
      unique(data$`Involved Entity`)
    } else {
      data %>%
        filter(Entity %in% input$entity) %>%
        pull(`Involved Entity`) %>%
        unique()
    }
  })
  
  # Reactive UI for Involved Entity Selection based on selected Entity
  output$involved_entity_ui <- renderUI({
    involved_entities <- filtered_involved_entities()
    # Combine with "All" option
    selectInput("involved_entity", "Select Involved Entities:",
                choices = c(involved_entities),
                selected = NULL,
                multiple = TRUE)
  })
  
  # Reactive UI for Entity Country Selection based on selected Entity
  # output$entity_country_ui <- renderUI({
  #   entity_countries <- if ("All" %in% input$entity) {
  #     unique(data$`Entity country`)
  #   } else {
  #     # Filter to show countries related to the selected entities
  #     filtered_countries <- data %>%
  #       filter(Entity %in% input$entity) %>%
  #       pull(`Entity country`) %>%
  #       unique()
  #     # Combine with "All" option
  #     c("All", filtered_countries)
  #   }
  # 
  #   selectInput("entity_country", "Select Entity Country:",
  #               choices = entity_countries,
  #               selected = "All")
  # })
  
  ####
  
  # # Reactive UI for Involved Entity Country Selection based on selected Involved Entities
  # output$involved_entity_country_ui <- renderUI({
  #   involved_entities <- input$involved_entity
  #   
  #   if (is.null(involved_entities) || length(involved_entities) == 0) {
  #     involved_entities <- c("All")  # Default to "All" if nothing selected
  #   }
  #   
  #   involved_countries <- if ("All" %in% involved_entities) {
  #     unique(data$`Involved Entity country`)
  #   } else {
  #     # Filter countries related to selected involved entities and the current entities
  #     filtered_countries <- data %>%
  #       filter(`Involved Entity` %in% involved_entities & Entity %in% input$entity) %>%
  #       pull(`Involved Entity country`) %>%
  #       unique()
  #     # Combine with "All" option
  #     c("All", filtered_countries)
  #   }
  #   
  #   selectInput("involved_entity_country", "Select Involved Entity Country:",
  #               choices = involved_countries,
  #               selected = "All")
  # })
  
  ################################
  
  # Filter the data based on user input
  filtered_data <- reactive({
    data_filtered <- data
    
    # If "All" is selected for Entity, do not filter on Entity
    if (!("All" %in% input$entity)) {
      data_filtered <- data_filtered[data_filtered$Entity %in% input$entity, ]
    }
    
    # Check if "All" is in involved entities
    if (!("All" %in% input$involved_entity) && length(input$involved_entity) > 0) {
      # Filter data if specific involved entities are selected
      data_filtered <- data_filtered[data_filtered$`Involved Entity` %in% input$involved_entity, ]
    }
    
    # # Apply country filters
    # if (input$entity_country != "All") {
    #   data_filtered <- data_filtered[data_filtered$`Entity country` == input$entity_country, ]
    # }
    # 
    # if (input$involved_entity_country != "All") {
    #   data_filtered <- data_filtered[data_filtered$`Involved Entity country` == input$involved_entity_country, ]
    # }
    
    # Apply year filtering
    data_filtered <- data_filtered[data_filtered$`Reporting year` >= input$year[1] & 
                                     data_filtered$`Reporting year` <= input$year[2], ]
    
    # Filter by ESRS Code
    if (input$esrs_code != "All") {
      data_filtered <- data_filtered[data_filtered$`ESRS code` == input$esrs_code, ]
    }
    
    # Apply consumption filtering using numeric inputs
    data_filtered <- data_filtered[data_filtered$Consumption >= input$consumption_min &
                                     data_filtered$Consumption <= input$consumption_max, ]
    
    # Filter out zero consumption if checkbox is selected
    if (input$filter_zero) {
      data_filtered <- data_filtered[data_filtered$Consumption > 0, ]
    }
    
    return(data_filtered)
  })
  
  # Render DataTable
  output$dataTable <- renderDT({
    # Specify the desired columns
    desired_columns <- c("ESRS code", "ESRS full name", 
                         "Entity", "Entity full name", 
                         "Reporting year", "Consumption", 
                         "Entity country", "Involved Entity", 
                         "Involved Entity country")
    
    # Filter the data to only include the desired columns
    filtered_data_subset <- filtered_data()[, desired_columns, drop = FALSE]
    
    # Render the DataTable with the filtered data
    datatable(filtered_data_subset, options = list(pageLength = 10)) %>%
      formatRound(columns = "Consumption", digits = 3)  # Format the Consumption column
  })
  
  # Download filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(filtered_data(), file)
    }
  )
  
  # Plot for ESRS Consumption Chart
  output$esrsConsumptionChart <- renderPlotly({
    filtered <- filtered_data() %>%
      group_by(`ESRS full name`) %>%
      summarise(total_consumption = sum(Consumption, na.rm = TRUE))
    
    plot_ly(
      filtered,
      x = ~`ESRS full name`,
      y = ~total_consumption,
      type = "bar"
    ) %>%
      layout(title = "Total Consumption by ESRS Code",
             xaxis = list(title = "ESRS Code"),
             yaxis = list(title = "Total Consumption"))
  })
  
  output$esrsComparisonChart <- renderPlotly({
    # Filter the data based on user input and group by ESRS full name and Entity Name
    filtered <- filtered_data() %>%
      group_by(`ESRS code`, `ESRS full name`, `Entity full name`, `Reporting year`) %>%
      summarise(total_consumption = sum(Consumption, na.rm = TRUE), .groups = 'drop')  # Summarise to get total consumption
    
    # Get unique values for the ESRS code, full name, and reporting year
    unique_values <- filtered %>%
      distinct(`ESRS code`, `ESRS full name`, `Reporting year`, .keep_all = TRUE) %>%  # Keep only unique observations
      summarise(
        Series_Name = paste(`ESRS full name`, "(", `ESRS code`, ")", sep = " "),  # Merge ESRS full name and code
        Reporting_Year = unique(`Reporting year`)  # Get unique Reporting year
      )
    
    # Create the final title string
    final_title <- paste("Series: ", unique_values$Series_Name, " | Year: ", unique_values$Reporting_Year)
    
    plot_ly(
      data = filtered,
      x = ~`Entity full name`,
      y = ~total_consumption,
      type = "bar",
      color = ~`ESRS full name`,  # Use ESRS full name to differentiate the bars
      text = ~sprintf("%.2f", total_consumption),  # Format total consumption to 2 decimal places
      hoverinfo = "text",  # Show only the text on hover
      marker = list(
        opacity = 0.85,  # Adjust opacity for better visibility
        line = list(width = 1, color = 'rgba(0, 0, 0, 0.5)')  # Add borders to bars
      )
    ) %>%
      layout(
        title = list(text = final_title, font = list(size = 18)),  # Increase title font size
        xaxis = list(title = "", tickangle = -45, automargin = TRUE, tickfont = list(size = 12)),  # Adjust font size and enable automargin
        yaxis = list(title = ""),
        barmode = "group",  # Group bars together for each entity
        margin = list(l = 50, r = 50, t = 50, b = 100),  # Increase bottom margin for x-axis labels
        showlegend = FALSE,  # Hide legend
        paper_bgcolor = 'rgba(245, 245, 245, 1)',  # Light gray background
        plot_bgcolor = 'rgba(255, 255, 255, 1)'  # White plot background
      ) %>%
      config(displayModeBar = TRUE, responsive = TRUE)  # Make plot responsive

  })
  
  ##
  
  # Render Leaflet Map
  output$entityMap <- renderLeaflet({
    # Group data by entity country and calculate total consumption
    filtered <- filtered_data() %>%
      group_by(`Entity country`) %>%
      summarise(total_consumption = sum(Consumption, na.rm = TRUE)) %>%
      left_join(country_coordinates, by = "Entity country")
    
    # If involved entities are in a different data frame, prepare it
    involved_entities <- filtered_data() %>%
      filter(!is.na(`Involved Entity`)) %>%
      group_by(`Involved Entity country`) %>%
      summarise(total_involved_consumption = sum(Consumption, na.rm = TRUE)) %>%
      left_join(country_coordinates, by = c("Involved Entity country" = "Entity country"))
    
    # Create the leaflet map
    leaflet() %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%  # Default tiles
      addProviderTiles("CartoDB.Positron") %>%  # Gray tile layer
      
      # Add markers for main entities
      addCircleMarkers(
        data = filtered,
        lng = ~lon, 
        lat = ~lat, 
        weight = 1,
        radius = ~sqrt(total_consumption) / 100,  # Scale size based on total consumption
        color = "blue",  # Main entity color
        stroke = TRUE,
        fillOpacity = 0.7,  # Set opacity for better visibility
        popup = ~paste0("<strong>Country:</strong> ", `Entity country`, 
                        "<br><strong>Total Consumption:</strong> ", total_consumption, " MWh")
      ) %>%
      
      # Add markers for involved entities
      addCircleMarkers(
        data = involved_entities,
        lng = ~lon, 
        lat = ~lat, 
        weight = 1,
        radius = ~sqrt(total_involved_consumption) / 100,  # Scale size based on involved consumption
        color = "red",  # Involved entity color
        stroke = TRUE,
        fillOpacity = 0.5,  # Set opacity for better visibility
        popup = ~paste0("<strong>Involved Entity Country:</strong> ", `Involved Entity country`, 
                        "<br><strong>Total Involved Consumption:</strong> ", total_involved_consumption, " MWh")
      )
  })
  
  # ValueBox Outputs for Summary Info
  output$totalConsumption <- renderValueBox({
    total <- sum(filtered_data()$Consumption, na.rm = TRUE)
    valueBox(
      paste(round(total / 1e6, 2), "MWh"), "Total Consumption",
      icon = icon("bolt"),
      color = "yellow"
    )
  })
  
  output$entityCount <- renderValueBox({
    count <- n_distinct(filtered_data()$Entity)
    valueBox(
      count, "Number of Entities",
      icon = icon("building"),
      color = "blue"
    )
  })
  
  output$CountryCount <- renderValueBox({
    count <- n_distinct(filtered_data()$`Entity country`)
    valueBox(
      count, "Countries Represented (Entities)",
      icon = icon("globe"),
      color = "green"
    )
  })
  
  output$CountryInvolvedCount <- renderValueBox({
    count <- n_distinct(filtered_data()$`Involved Entity country`)
    valueBox(
      count, "Countries Represented (Involved Entities)",
      icon = icon("globe"),
      color = "green"
    )
  })
  
  output$ESRSCount <- renderValueBox({
    count <- n_distinct(filtered_data()$`ESRS code`)
    valueBox(
      count, "Number of ESRS Codes",
      icon = icon("list"),
      color = "purple"
    )
  })
}

################################################################################
# Run the application 
shinyApp(ui = ui, server = server)