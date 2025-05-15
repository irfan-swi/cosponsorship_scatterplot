# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(gdtools)
library(data.table)
library(arrow)
library(fst)

# Register font
register_gfont("Roboto") # If using ggiraph

# Read data once at startup using the fastest method
# Using data.table with key columns for faster filtering
df <- fread('df2.csv', key = c("congress", "bill_chamber", "issue_name"))

# Pre-compute bill_search_item once at startup
df[, bill_search_item := bill_number]

# Pre-compute unique values for dropdowns to avoid repeated calculations
unique_congresses <- sort(unique(df$congress), decreasing = TRUE)
unique_issues <- sort(unique(df$issue_name))
unique_chambers <- c("House", "Senate")

# Pre-aggregate data for the plot to reduce computation during rendering
precompute_plot_data <- function(data) {
  data[cosponsor_party %in% c("Democrat", "Republican"),
       .(num_dem_cosponsors = uniqueN(cosponsor_person_id[cosponsor_party == "Democrat"]),
         num_rep_cosponsors = uniqueN(cosponsor_person_id[cosponsor_party == "Republican"])),
       by = .(bill_url, bill_name, bill_number, sponsor_name, sponsor_party, issue_name)]
}

# Define UI for the application
ui <- fluidPage(
  # Link to external CSS file
  includeCSS("styles.css"),
  
  # Title section
  fluidRow(
    # Selection Panel
    column(
      width = 12,  # Initial width
      addGFontHtmlDependency(family = c("Roboto")),
      id = "sidebar",
      tags$div(
        id = "sidebar-content",
        tags$div(
          class = "grid-container",
          
          # Filter by Congress Version
          selectInput("congress_version",
                      "Congress",
                      choices = unique_congresses,
                      selected = max(unique_congresses)),
          
          # Filter by Chamber
          selectInput("selected_chamber",
                      "Chamber",
                      choices = unique_chambers,
                      selected = "House"),
          
          # Filter by Issue
          selectInput("selected_issue",
                      "Issue Area",
                      choices = c("All", unique_issues),
                      selected = "All"),
          
          # Search for specific bills
          selectizeInput("search_bills",
                         "Search Bills",
                         choices = NULL,
                         selected = "All",
                         options = list(maxOptions = 1000)),
          
          selectizeInput("search_sponsor",
                         "Search Sponsor",
                         choices = NULL,
                         selected = "All",
                         options = list(maxOptions = 500)),
          
          selectizeInput("search_cosponsor",
                         "Search Cosponsor",
                         choices = NULL,
                         selected = "All",
                         options = list(maxOptions = 500))
        )
      )
    ),
    
    # Main Panel
    column(
      width = 12,  # Initial width
      id = "main",
      titlePanel(textOutput("main_title")),
      tags$h3(id = "subtitle", textOutput("subtitle")),
      girafeOutput("plot", width = "1000px"),
      tags$p(HTML("<b>Methodology</b><br>
                   Data is provided by <a href='https://www.congress.gov' target='_blank'>congress.gov</a>. 
                   Cosponsor data is derived from each bill's Republican and Democratic cosponsors.")),
      tags$script(
        "Shiny.addCustomMessageHandler('openURL', function(url) { window.open(url); });"
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Use reactive values to store filtered data
  values <- reactiveValues(
    current_filtered_data = NULL,
    current_plot_data = NULL
  )
  
  # Debounced reactive for filtered data to reduce flickering
  filtered_data <- reactive({
    # Use data.table's fast filtering
    result <- df[congress == input$congress_version & 
                   bill_chamber == input$selected_chamber]
    
    if (input$selected_issue != "All") {
      result <- result[issue_name == input$selected_issue]
    }
    
    if (input$search_sponsor != "All") {
      result <- result[sponsor_name == input$search_sponsor]
    }
    
    if (input$search_cosponsor != "All") {
      result <- result[cosponsor_name == input$search_cosponsor]
    }
    
    if (input$search_bills != "All") {
      result <- result[bill_search_item == input$search_bills]
    }
    
    result
  }) %>% debounce(300)  # Add debounce to reduce reactivity frequency
  
  # Pre-compute plot data reactively
  plot_data <- reactive({
    data <- filtered_data()
    if (nrow(data) > 0) {
      precompute_plot_data(data)
    } else {
      data.table()
    }
  }) %>% debounce(300)
  
  # Update dropdowns based on filtered data
  observe({
    data <- filtered_data()
    
    # Update only if data has changed significantly
    isolate({
      if (!identical(values$current_filtered_data, data)) {
        values$current_filtered_data <- data
        
        # Update dropdowns with unique values
        updateSelectizeInput(session, "search_bills",
                             choices = c("All", sort(unique(data$bill_search_item))),
                             selected = input$search_bills,
                             server = FALSE)
        
        updateSelectizeInput(session, "search_sponsor",
                             choices = c("All", sort(unique(data$sponsor_name))),
                             selected = input$search_sponsor,
                             server = FALSE)
        
        updateSelectizeInput(session, "search_cosponsor",
                             choices = c("All", sort(unique(data$cosponsor_name))),
                             selected = input$search_cosponsor,
                             server = FALSE)
      }
    })
  })
  
  # Initial load of dropdowns
  observe({
    # Load initial data for default selections
    initial_data <- df[congress == max(unique_congresses) & bill_chamber == "House"]
    
    updateSelectizeInput(session, "search_bills",
                         choices = c("All", sort(unique(initial_data$bill_search_item))),
                         selected = "All",
                         server = FALSE)
    
    updateSelectizeInput(session, "search_sponsor",
                         choices = c("All", sort(unique(initial_data$sponsor_name))),
                         selected = "All",
                         server = FALSE)
    
    updateSelectizeInput(session, "search_cosponsor",
                         choices = c("All", sort(unique(initial_data$cosponsor_name))),
                         selected = "All",
                         server = FALSE)
  })
  
  # Main plot logic
  output$plot <- renderGirafe({
    plot_df <- plot_data()
    
    # Only update if data has changed
    isolate({
      if (!identical(values$current_plot_data, plot_df)) {
        values$current_plot_data <- plot_df
      }
    })
    
    if (nrow(plot_df) == 0) {
      # Return empty plot if no data
      p <- ggplot() + 
        geom_blank() + 
        theme_classic() +
        labs(x = "Democratic Cosponsors", y = "Republican Cosponsors")
    } else {
      # Add tooltip to the data
      plot_df[, tooltip := paste(
        "Bill:", bill_number, "<br>",
        "Title:", bill_name, "<br>",
        "Sponsor:", sponsor_name, "<br>",
        "Issue:", issue_name, "<br>",
        "Democratic Cosponsors:", num_dem_cosponsors, "<br>",
        "Republican Cosponsors:", num_rep_cosponsors
      )]
      
      # Set axis limits
      axis_limit <- if (input$selected_chamber == "Senate") 50 else 220
      
      # Create the plot
      p <- ggplot(plot_df, aes(x = num_dem_cosponsors, y = num_rep_cosponsors)) +
        geom_point_interactive(
          aes(tooltip = tooltip, data_id = bill_url),
          color = ifelse(plot_df$sponsor_party == 'Republican', '#810000',
                         ifelse(plot_df$sponsor_party == 'Democrat', '#2E598E', '#6B56AA')),
          size = 3.5, 
          alpha = 0.5
        ) +
        geom_abline(intercept = 0, slope = 1, color = "gray", linewidth = 0.5, linetype = "dashed") +
        coord_cartesian(
          xlim = c(0, axis_limit),
          ylim = c(0, axis_limit)
        ) +
        labs(x = "Democratic Cosponsors",
             y = "Republican Cosponsors") +
        theme_classic(base_family = "Roboto")
    }
    
    girafe(
      ggobj = p, 
      width_svg = 10,
      height_svg = 8,
      options = list(
        opts_hover(css = "cursor:pointer;fill:yellow;stroke:gray;"),
        opts_selection(type = "single", css = "fill:yellow;stroke:gray;"),
        opts_sizing(rescale = TRUE, width = 1)
      )
    )
  })
  
  # Handle click events
  observeEvent(input$plot_selected, {
    selected_id <- input$plot_selected
    if (!is.null(selected_id)) {
      url <- paste0("https://app.legis1.com/bill/detail?id=", selected_id, "#summary")
      session$sendCustomMessage(type = 'openURL', message = url)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)