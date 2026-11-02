
# Load packages
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

# Load accident data
accidents <- read.csv("accident.csv", stringsAsFactors = FALSE,
                      check.names = FALSE)

# Load VMT data
VMT <- read.csv("VMT_2024.csv", stringsAsFactors = FALSE,
                check.names = FALSE)

# Clean state names
accidents$STATENAME <- trimws(as.character(accidents$STATENAME))
VMT$State <- trimws(as.character(VMT$State))

# Convert VMT values to numeric
VMT$VMT <- as.numeric(gsub(",", "", as.character(VMT$VMT)))

# Keep only 2024 data if multiple years exist
if ("Year" %in% names(VMT)) {
  VMT <- VMT %>%
    filter(as.numeric(as.character(Year)) == 2024)
}

# Combine rural and urban VMT and all road classifications
VMT_by_state <- VMT %>%
  group_by(State) %>%
  summarise(
    VMT = sum(VMT, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate fatal crash rates by state
crash_rates <- accidents %>%
  count(STATENAME, name = "Crashes") %>%
  left_join(VMT_by_state, by = c("STATENAME" = "State")) %>%
  mutate(
    Rate = (Crashes / VMT) * 100000000
  )

# Factors that can be compared
factor_choices <- c(
  "Overall Crashes" = "Overall",
  "Weather" = "WEATHERNAME",
  "Harmful Event" = "HARM_EVNAME",
  "Rural/Urban Location" = "RUR_URBNAME",
  "Hour of Day" = "HOURNAME",
  "Day of Week" = "DAY_WEEKNAME"
)

# Make categories easier to work with
for (column in factor_choices[-1]) {
  accidents[[column]] <- as.character(accidents[[column]])
}

accidents$MONTHNAME <- as.character(accidents$MONTHNAME)

# Put months in calendar order
available_months <- unique(na.omit(accidents$MONTHNAME))

month_order <- c(
  month.name[month.name %in% available_months],
  sort(setdiff(available_months, month.name))
)

# Group less common harmful events together
limit_harmful_events <- function(data, column) {
  
  if (column == "HARM_EVNAME") {
    
    event_counts <- sort(
      table(data[[column]]),
      decreasing = TRUE
    )
    
    top_events <- names(event_counts)[
      seq_len(min(12, length(event_counts)))
    ]
    
    data[[column]] <- ifelse(
      is.na(data[[column]]) | data[[column]] == "",
      "Unknown",
      ifelse(
        data[[column]] %in% top_events,
        data[[column]],
        "Other harmful events"
      )
    )
  }
  
  data
}

# User interface
ui <- page_navbar(
  
  title = "2024 Fatal Vehicle Accident Data",
  
  # TAB 1: CUSTOM CRASH ANALYSIS
  nav_panel(
    "Custom Crash Analysis",
    
    page_sidebar(
      
      sidebar = sidebar(
        title = "Customize Your Analysis",
        
        selectInput(
          "state",
          "Select State:",
          choices = c("All states", sort(unique(accidents$STATENAME))),
          selected = "All states"
        ),
        
        selectInput(
          "month",
          "Select Month:",
          choices = c("All Months", month_order),
          selected = "All Months"
        ),
        
        tags$hr(),
        
        selectInput(
          "factor1",
          "Select First Factor:",
          choices = factor_choices,
          selected = "Overall"
        ),
        
        selectInput(
          "factor2",
          "Select Second Factor:",
          choices = c(
            "None" = "Na",
            factor_choices[-1]
          ),
          selected = "Na"
        ),
        
        tags$hr(),
        
        p("Harmful Event charts show the 12 most common categories. The remaining categories are grouped as Other.")
      ),
      
      card(
        card_header("Graph Recommendation"),
        textOutput("graphRecommendation")
      ),
      
      card(
        card_header("Fatal Crash Analysis"),
        plotOutput("accidentPlot", height = "650px")
      )
    )
  ),
  
  # TAB 2: VMT ANALYSIS
  nav_panel(
    "VMT Analysis",
    
    page_sidebar(
      
      sidebar = sidebar(
        title = "VMT Filters",
        
        selectInput(
          "vmt_state",
          "Select State:",
          choices = c("All states", sort(unique(accidents$STATENAME))),
          selected = "All states"
        ),
        
        tags$hr(),
        
        p("This graph compares fatal crash rates using annual vehicle miles traveled. Select an individual state or compare all states.")
      ),
      
      card(
        card_header("VMT-Adjusted Fatal Crash Rate"),
        
        p("Fatal crashes per 100 million vehicle miles traveled. Rural and urban VMT, along with all road classifications, are combined for each state."),
        
        plotOutput("vmtPlot", height = "750px")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Filter data by state and month
  filtered_data <- reactive({
    
    data <- accidents
    
    if (input$state != "All states") {
      data <- data %>%
        filter(STATENAME == input$state)
    }
    
    if (input$month != "All Months") {
      data <- data %>%
        filter(MONTHNAME == input$month)
    }
    
    data
  })
  
  # Explain which graph is being displayed
  output$graphRecommendation <- renderText({
    
    if (input$factor1 == "Overall" && input$factor2 == "Na") {
      
      if (input$month == "All Months") {
        return("A monthly line chart shows how total fatal crashes change throughout the year.")
      } else {
        return("A total crash count is displayed for the selected month.")
      }
    }
    
    if (input$factor1 == "Overall" && input$factor2 != "Na") {
      
      if (input$month == "All Months") {
        return("A multi-line chart compares monthly crash trends across categories.")
      } else {
        return("A horizontal bar chart compares crash counts across categories.")
      }
    }
    
    if (input$factor2 == "Na") {
      return("A horizontal bar chart compares fatal crashes across categories.")
    }
    
    if (input$factor1 == input$factor2) {
      return("Please select two different factors.")
    }
    
    return("A heatmap compares the two selected factors. Lighter/brighter cells represent MORE fatal crashes.")
  })
  
  # Create the original crash analysis graph
  output$accidentPlot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0, "No accident records match these filters.")
    )
    
    factor1 <- input$factor1
    factor2 <- input$factor2
    
    # Overall crashes
    if (factor1 == "Overall" && factor2 == "Na") {
      
      if (input$month == "All Months") {
        
        monthly_data <- data %>%
          count(MONTHNAME, name = "Crashes") %>%
          mutate(Month = factor(MONTHNAME, levels = month_order)) %>%
          arrange(Month)
        
        return(
          ggplot(monthly_data,
                 aes(x = Month, y = Crashes, group = 1)) +
            geom_line(linewidth = 1.2, color = "steelblue") +
            geom_point(size = 3, color = "steelblue") +
            labs(
              title = "Monthly Fatal Crashes",
              x = "Month",
              y = "Number of Fatal Crashes"
            ) +
            theme_minimal(base_size = 13) +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1)
            )
        )
        
      } else {
        
        total <- nrow(data)
        
        return(
          ggplot(
            data.frame(
              Category = "Fatal Crashes",
              Crashes = total
            ),
            aes(x = Category, y = Crashes)
          ) +
            geom_col(fill = "steelblue", width = 0.5) +
            geom_text(aes(label = Crashes), vjust = -0.5, size = 5) +
            labs(
              title = paste("Total Fatal Crashes -", input$month),
              x = "",
              y = "Number of Fatal Crashes"
            ) +
            theme_minimal(base_size = 13) +
            expand_limits(y = total * 1.15)
        )
      }
    }
    
    # Overall crashes compared with one factor
    if (factor1 == "Overall" && factor2 != "Na") {
      
      data <- limit_harmful_events(data, factor2)
      
      data <- data %>%
        filter(
          !is.na(.data[[factor2]]),
          .data[[factor2]] != ""
        )
      
      if (input$month == "All Months") {
        
        monthly_data <- data %>%
          transmute(
            Month = MONTHNAME,
            Group = .data[[factor2]]
          ) %>%
          count(Month, Group, name = "Crashes") %>%
          mutate(Month = factor(Month, levels = month_order))
        
        return(
          ggplot(
            monthly_data,
            aes(x = Month, y = Crashes,
                color = Group, group = Group)
          ) +
            geom_line(linewidth = 1) +
            geom_point(size = 2) +
            labs(
              title = "Monthly Fatal Crashes by Category",
              x = "Month",
              y = "Number of Fatal Crashes",
              color = "Category"
            ) +
            theme_minimal(base_size = 12) +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "right"
            )
        )
        
      } else {
        
        category_data <- data %>%
          count(Category = .data[[factor2]], name = "Crashes") %>%
          arrange(Crashes)
        
        return(
          ggplot(
            category_data,
            aes(x = reorder(Category, Crashes), y = Crashes)
          ) +
            geom_col(fill = "steelblue") +
            coord_flip() +
            labs(
              title = paste("Fatal Crashes by", names(factor_choices)[
                match(factor2, factor_choices)
              ]),
              x = "",
              y = "Number of Fatal Crashes"
            ) +
            theme_minimal(base_size = 12)
        )
      }
    }
    
    # One factor selected
    if (factor2 == "Na") {
      
      data <- limit_harmful_events(data, factor1)
      
      category_data <- data %>%
        filter(
          !is.na(.data[[factor1]]),
          .data[[factor1]] != ""
        ) %>%
        count(Category = .data[[factor1]], name = "Crashes") %>%
        arrange(Crashes)
      
      return(
        ggplot(
          category_data,
          aes(x = reorder(Category, Crashes), y = Crashes)
        ) +
          geom_col(fill = "steelblue") +
          coord_flip() +
          labs(
            title = paste("Fatal Crashes by", names(factor_choices)[
              match(factor1, factor_choices)
            ]),
            x = "",
            y = "Number of Fatal Crashes"
          ) +
          theme_minimal(base_size = 12)
      )
    }
    
    # Two factors selected: heatmap
    validate(
      need(factor1 != factor2, "Please select two different factors.")
    )
    
    data <- limit_harmful_events(data, factor1)
    data <- limit_harmful_events(data, factor2)
    
    data <- data %>%
      filter(
        !is.na(.data[[factor1]]),
        !is.na(.data[[factor2]]),
        .data[[factor1]] != "",
        .data[[factor2]] != ""
      )
    
    validate(
      need(nrow(data) > 0, "No data available for these factors.")
    )
    
    # Count combinations of the two factors
    heatmap_data <- data %>%
      transmute(
        Factor1 = as.character(.data[[factor1]]),
        Factor2 = as.character(.data[[factor2]])
      ) %>%
      count(Factor1, Factor2, name = "Crashes")
    
    # Order categories by their total crash counts
    factor1_order <- heatmap_data %>%
      group_by(Factor1) %>%
      summarise(Total = sum(Crashes), .groups = "drop") %>%
      arrange(Total) %>%
      pull(Factor1)
    
    factor2_order <- heatmap_data %>%
      group_by(Factor2) %>%
      summarise(Total = sum(Crashes), .groups = "drop") %>%
      arrange(Total) %>%
      pull(Factor2)
    
    heatmap_data <- heatmap_data %>%
      mutate(
        Factor1 = factor(Factor1, levels = factor1_order),
        Factor2 = factor(Factor2, levels = factor2_order)
      )
    
    # Create heatmap
    ggplot(
      heatmap_data,
      aes(x = Factor2, y = Factor1, fill = Crashes)
    ) +
      geom_tile(color = "white", linewidth = 0.4) +
      scale_fill_viridis_c(
        option = "C",
        direction = 1,
        trans = "sqrt",
        name = "Fatal Crashes"
      ) +
      labs(
        title = paste(
          names(factor_choices)[match(factor1, factor_choices)],
          "vs.",
          names(factor_choices)[match(factor2, factor_choices)]
        ),
        subtitle = "Lighter/brighter colors represent MORE fatal crashes",
        x = names(factor_choices)[match(factor2, factor_choices)],
        y = names(factor_choices)[match(factor1, factor_choices)]
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(
          angle = 45, hjust = 1, vjust = 1
        ),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "right"
      )
    
  }, res = 100)
  
  # VMT-adjusted fatal crash rate by state
  output$vmtPlot <- renderPlot({
    
    data <- crash_rates
    
    # Apply the VMT tab's independent state filter
    if (input$vmt_state != "All states") {
      data <- data %>%
        filter(STATENAME == input$vmt_state)
    }
    
    # Check that VMT data matches accident data
    validate(
      need(nrow(data) > 0, "No state data available."),
      need(
        !any(is.na(data$VMT)),
        "Some states did not match between the accident and VMT datasets. Check state names."
      ),
      need(
        all(data$VMT > 0),
        "VMT must be greater than zero to calculate crash rates."
      )
    )
    
    # Create graph
    ggplot(
      data,
      aes(x = reorder(STATENAME, Rate), y = Rate)
    ) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Fatal Crash Rate by State",
        subtitle = "2024 fatal crashes per 100 million vehicle miles traveled",
        x = "State",
        y = "Fatal Crashes per 100 Million VMT"
      ) +
      theme_minimal(base_size = 12)
    
  }, res = 100)
}

# Run app
shinyApp(ui = ui, server = server)
