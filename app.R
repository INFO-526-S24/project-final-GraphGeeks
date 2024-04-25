# Load necessary libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, shiny, shinythemes, readr, leaflet, rnaturalearth, rnaturalearthdata)

# Set global options and themes
theme_set(theme_minimal(base_size = 14))
options(width = 65)

# Load and prepare data
global_data <- read_csv("./data/countries.csv") %>%
    mutate(Date = as.Date(Date)) %>%
    pivot_longer(
        cols = c(Confirmed, Recovered, Deaths),
        names_to = "Case_Type",
        values_to = "Cases"
    ) %>%
    mutate(Case_Type = tolower(gsub(" ", "_", Case_Type))) %>%
    na.omit()

us_confirmed <- read_csv("./data/us_confirmed.csv") %>%
    mutate(Date = as.Date(Date)) %>%
    na.omit()

us_deaths <- read_csv("./data/us_deaths.csv") %>%
    mutate(Date = as.Date(Date)) %>%
    na.omit()

vaccination_data <- read_csv("./data/country_vaccinations_by_manufacturer.csv") %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    group_by(location) %>%
    # Summarize to find the maximum total vaccinations and get the associated date
    summarize(
        Total_Vaccinations = max(total_vaccinations, na.rm = TRUE),
        date = date[which.max(total_vaccinations)],
        .groups = "drop"
    ) %>%
    arrange(desc(Total_Vaccinations))

print(vaccination_data)


# Define UI
ui <- fluidPage(
    theme = shinytheme("united"),
    navbarPage(
        theme = "cerulean",
        title = "COVID-19 Analysis",
        id = "main_navbar",
        tabPanel(
            "Time Series",
            sidebarLayout(
                sidebarPanel(
                    uiOutput("dynamicInput"),
                    dateRangeInput("dateRange", "Select Date Range:",
                        start = min(global_data$Date, na.rm = TRUE),
                        end = max(global_data$Date, na.rm = TRUE)
                    )
                ),
                mainPanel(
                    tabsetPanel(
                        id = "tabs",
                        tabPanel("Global Trends", plotOutput("globalPlot")),
                        tabPanel("US Confirmed Cases", plotOutput("usConfirmedPlot")),
                        tabPanel("US Deaths", plotOutput("usDeathsPlot")),
                        tabPanel("Combined Data", plotOutput("usCombinedPlot"))
                    )
                )
            )
        ),
        tabPanel(
            "Maps",
            sidebarLayout(
                sidebarPanel(
                    uiOutput("mapInput"),
                    dateRangeInput("mapDateRange", "Select Date Range:",
                        start = min(global_data$Date, na.rm = TRUE),
                        end = max(global_data$Date, na.rm = TRUE),
                        min = min(global_data$Date, na.rm = TRUE),
                        max = max(global_data$Date, na.rm = TRUE)
                    ),
                    selectInput("caseType", "Select Case Type",
                        choices = c("Confirmed" = "confirmed", "Recovered" = "recovered", "Deaths" = "deaths")
                    )
                ),
                mainPanel(
                    leafletOutput("mapPlot")
                )
            )
        ),
        tabPanel(
            "Vaccination",
            sidebarLayout(
                sidebarPanel(
                    dateRangeInput("vaccineDateRange", "Select Date Range for Vaccination Data:",
                        start = as.Date("2020-12-29"),
                        end = as.Date("2022-03-29"),
                        min = as.Date("2020-12-29"),
                        max = as.Date("2022-03-29")
                    )
                ),
                mainPanel(
                    plotOutput("vaccinePlot")
                )
            )
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
            data.frame() # Return an empty data frame if inputs are not ready
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
                    legend.position = "none" # Remove traditional legend
                )

            # Add labels for each case type at the top left
            labels_to_add <- data_to_plot %>%
                group_by(Case_Type) %>%
                summarize(Cases = max(Cases), Date = first(Date), .groups = "drop")

            # Using geom_text to add labels directly on the graph
            for (i in 1:nrow(labels_to_add)) {
                label_color <- case_colors[labels_to_add$Case_Type[i]]
                p <- p + geom_text(
                    data = labels_to_add[i, ], aes(label = paste(Case_Type)),
                    hjust = 0, vjust = 1.5, size = 5, fontface = "bold", color = label_color
                )
            }

            return(p)
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

    # Combined plot logic
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
                labs(
                    title = "Daily COVID-19 Statistics in the US (Confirmed Cases and Deaths)",
                    x = "Date", y = "Number of Cases",
                    color = "Statistic"
                ) +
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
    })

    # Map logic for displaying data
    output$mapPlot <- renderLeaflet({
        req(input$mapDateRange)  # Ensure date range is selected

        # Filter data for the selected date range and case type
        selected_data <- global_data %>%
            filter(Date >= input$mapDateRange[1], Date <= input$mapDateRange[2], 
                   Case_Type == input$caseType) %>%
            group_by(Country) %>%
            summarize(Cases = sum(Cases), .groups = "drop")  # Sum cases over the period for each country

        # Prepare world map data
        world <- ne_countries(scale = "medium", returnclass = "sf")

        # Join world map data with the filtered COVID data
        world_data <- left_join(world, selected_data, by = c("name" = "Country"))

        # Create the map only if there are cases to display
        if(any(!is.na(world_data$Cases))) {
            pal <- colorBin("YlOrRd", domain = world_data$Cases, bins = 10, na.color = "#808080")

            leaflet(world_data) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(
                    fillColor = ~pal(Cases),
                    fillOpacity = 0.7,
                    color = "#BDBDC3",
                    weight = 0.5,
                    popup = ~paste(name, "<br>", "Cases: ", Cases)
                ) %>%
                addLegend(
                    "bottomright",
                    pal = pal,
                    values = ~Cases,
                    title = "Number of Cases",
                    opacity = 0.7
                )
        } else {
            # Handle case where no data is available
            leaflet() %>%
                addTiles() %>%
                addMarkers(lng = 0, lat = 0, popup = "No data available for the selected criteria.")
        }
    })

    # Vaccination tab logic
    # Vaccination tab logic
output$vaccinePlot <- renderPlot({
    req(input$vaccineDateRange)  # Ensure date range is selected




    # Print to console for debugging
    print(str(vaccination_data$date))
    print(input$vaccineDateRange)




    # Filter vaccination data within the selected date range


        vaccine_data_filtered <- vaccination_data %>%
        filter(date >= input$vaccineDateRange[1], date <= input$vaccineDateRange[2]) %>%
        group_by(location) %>%
        summarize(Total_Vaccinations = max(total_vaccinations, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(Total_Vaccinations)) %>%
        slice_max(order_by = Total_Vaccinations, n = 10)  



    # Join with global data to get cases for these top 10 vaccinated countries within the date range
    global_data_filtered <- global_data %>%
        filter(Country %in% vaccine_data_filtered$location & Date >= input$vaccineDateRange[1] & Date <= input$vaccineDateRange[2]) %>%
        group_by(Country, Case_Type) %>%
        summarize(Total_Cases = sum(Cases, na.rm = TRUE), .groups = "drop")

    # Create a bar plot to show cases for the top 10 vaccinated countries
    ggplot(global_data_filtered, aes(x = Country, y = Total_Cases, fill = Case_Type)) +
        geom_bar(stat = "identity", position = "dodge") +
        theme_minimal() +
        labs(title = "COVID-19 Case Types in Top 10 Vaccinated Countries",
             x = "Country", y = "Total Cases",
             fill = "Case Type")
})


}

# Run the application
shinyApp(ui = ui, server = server)
