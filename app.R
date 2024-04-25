# Load necessary libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, shiny, shinythemes, readr)

# Set global options and themes
theme_set(theme_minimal(base_size = 14))
options(width = 65)

# Load and prepare data
global_data <- read_csv("./data/countries.csv") %>% 
  mutate(Date = as.Date(Date)) %>%
  pivot_longer(cols = c(Confirmed, Recovered, Deaths),
               names_to = "Case_Type",
               values_to = "Cases") %>%
  mutate(Case_Type = tolower(gsub(" ", "_", Case_Type))) %>% 
  na.omit()

us_confirmed <- read_csv("./data/us_confirmed.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  na.omit()

us_deaths <- read_csv("./data/us_deaths.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  na.omit()

# Define UI
ui <- fluidPage(
    theme = shinytheme("united"),
    navbarPage(
        theme = "cerulean",
        title = "COVID-19 Analysis",
        id = "main_navbar",
        tabPanel("Time Series",
            sidebarLayout(
                sidebarPanel(
                    uiOutput("dynamicInput"),
                    dateRangeInput("dateRange", "Select Date Range:", 
                                   start = min(global_data$Date, na.rm = TRUE), 
                                   end = max(global_data$Date, na.rm = TRUE))
                ),
                mainPanel(
                    tabsetPanel(id = "tabs",
                        tabPanel("Global Trends", plotOutput("globalPlot")),
                        tabPanel("US Confirmed Cases", plotOutput("usConfirmedPlot")),
                        tabPanel("US Deaths", plotOutput("usDeathsPlot")),
                        tabPanel("Combined Data", plotOutput("usCombinedPlot"))
                    )
                )
            )
        ),
        tabPanel("Maps",
            "Maps content will go here. This can include interactive maps or other geographical visualizations."
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    output$dynamicInput <- renderUI({
        if (input$tabs == "Global Trends") {
            selectInput("country", "Select Country:", choices = unique(global_data$Country))
        } else if (input$tabs %in% c("US Confirmed Cases", "US Deaths", "Combined Data")) {
            selectInput("county", "Select County:", choices = unique(us_confirmed$`Province/State`))
        }
    })

    filtered_data <- reactive({
        if (!is.null(input$country) && !is.null(input$dateRange)) {
            global_data %>%
                filter(Country == input$country, Date >= input$dateRange[1], Date <= input$dateRange[2])
        } else {
            data.frame()  # Return an empty data frame if inputs are not ready
        }
    })

    # Define colors for case types
    case_colors <- c("confirmed" = "blue", "recovered" = "green", "deaths" = "red")


   output$globalPlot <- renderPlot({
        data_to_plot <- filtered_data()
        if (nrow(data_to_plot) > 0) {
            # Creating a plot
            p <- ggplot(data_to_plot, aes(x = Date, y = Cases, color = Case_Type)) +
                geom_line(size = 0.5) +
                scale_color_manual(values = case_colors) +
                theme_minimal() +
                facet_wrap(~Case_Type, scales = "free_y", nrow = 3) +
                theme(
                    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                    axis.title.x = element_text(size = 14, face = "bold"),
                    axis.title.y = element_text(size = 14, face = "bold"),
                    axis.text.x = element_text(size = 12),
                    axis.text.y = element_text(size = 12),
                    strip.text = element_blank(),
                    panel.border = element_rect(colour = "black", fill = NA, size = 1),
                    panel.spacing = unit(1, "lines"),
                    legend.position = "none"  # Remove traditional legend
                )

            # Add labels for each case type at the top left
           
            labels_to_add <- data_to_plot %>%
                group_by(Case_Type) %>%
                summarize(Cases = max(Cases), Date = first(Date), .groups = 'drop') 


            # Using geom_text to add labels directly on the graph
            for (i in 1:nrow(labels_to_add)) {
                label_color <- case_colors[labels_to_add$Case_Type[i]]
                p <- p + geom_text(data = labels_to_add[i, ], aes(label = paste(Case_Type)),
                                   hjust = 0, vjust = 1.5, size = 5, fontface = "bold", color = label_color)
            }

            p
        } else {
            plot.new()
            text(0.5, 0.5, "No data available", cex = 1.5)
        }
    })


 # Plots for US data

    output$usConfirmedPlot <- renderPlot({
        if (!is.null(input$county) && !is.null(input$dateRange)) {
            us_data <- us_confirmed %>%
                filter(`Province/State` == input$county, Date >= input$dateRange[1], Date <= input$dateRange[2])
            ggplot(data = us_data, aes(x = Date, y = Case, color = `Province/State`)) +
                geom_line() +
                labs(title = "Daily Confirmed Cases in the US", x = "Date", y = "Confirmed Cases") +
                theme(legend.position = "none")
        } else {
            plot.new()
            text(0.5, 0.5, "Select a county and date range", cex = 1.5)
        }
    })

    output$usDeathsPlot <- renderPlot({
        if (!is.null(input$county) && !is.null(input$dateRange)) {
            us_data <- us_deaths %>%
                filter(`Province/State` == input$county, Date >= input$dateRange[1], Date <= input$dateRange[2])
            ggplot(data = us_data, aes(x = Date, y = Case, color = `Province/State`)) +
                geom_line() +
                labs(title = "Daily Deaths in the US", x = "Date", y = "Deaths") +
                theme(legend.position = "none")
        } else {
            plot.new()
            text(0.5, 0.5, "Select a county and date range", cex = 1.5)
        }
    })



    # combined data

    output$usCombinedPlot <- renderPlot({
        if (!is.null(input$county) && !is.null(input$dateRange)) {
            # Filtering and merging the confirmed and death data
            filtered_confirmed <- us_confirmed %>%
                filter(`Province/State` == input$county, Date >= input$dateRange[1], Date <= input$dateRange[2])
            
            filtered_deaths <- us_deaths %>%
                filter(`Province/State` == input$county, Date >= input$dateRange[1], Date <= input$dateRange[2])
            
            # Combining the data
            combined_data <- merge(filtered_confirmed, filtered_deaths, by = c("Date", "Province/State"), suffixes = c("_confirmed", "_deaths"))
            
            # Plotting the data
            ggplot() +
                geom_line(data = combined_data, aes(x = Date, y = Case_confirmed, color = "Confirmed Cases"), size = 1) +
                geom_line(data = combined_data, aes(x = Date, y = Case_deaths, color = "Deaths"), size = 1) +
                labs(title = "Daily COVID-19 Statistics in the US (Confirmed Cases and Deaths)",
                     x = "Date", y = "Number of Cases",
                     color = "Statistic") +
                scale_color_manual(values = c("Confirmed Cases" = "yellow", "Deaths" = "red")) +
                theme_minimal() +
                theme(
                    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                    legend.title = element_text(size = 12),
                    legend.text = element_text(size = 12),
                    axis.title.x = element_text(size = 14, face = "bold"),
                    axis.title.y = element_text(size = 14, face = "bold"),
                    axis.text.x = element_text(size = 12),
                    axis.text.y = element_text(size = 12),
                    legend.position = "bottom"
                )
        } else {
            plot.new()
            text(0.5, 0.5, "Select a county and date range", cex = 1.5)
        }
    })}

# Run the application
shinyApp(ui = ui, server = server)
