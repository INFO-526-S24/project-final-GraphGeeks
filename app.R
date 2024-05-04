# nolint start

if (!require("pacman")) {
    install.packages("pacman")
}

pacman::p_load(ggplot2, dplyr, colorspace,tidyr, shiny, shinythemes, rnaturalearth, rnaturalearthdata, BiocManager, readr, leaflet, ggiraph, RColorBrewer, sf, zoo, shinyWidgets, plotly)

# set theme for ggplot2
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 14))

# set width of code output
options(width = 65)

# set figure parameters for knitr
knitr::opts_chunk$set(
    fig.width = 7, # 7" width
    fig.asp = 0.618, # the golden ratio
    fig.retina = 3, # dpi multiplier for displaying HTML output on retina
    fig.align = "center", # center align figures
    dpi = 300 # higher dpi, sharper image
)

# Importing the datasets
data <- read_csv("./data/countries.csv")

pops <- read_csv("./data/populations.csv")

us_confirmed <- read_csv("./data/us_confirmed.csv") %>%
    rename(Confirmed = Case, County = Admin2, State = `Province/State`)

us_deaths <- read_csv("./data/us_deaths.csv") %>%
    rename(Death = Case, County = Admin2, State = `Province/State`)

us_ <- us_deaths %>%
    inner_join(us_confirmed, by = c("County", "Date", "State")) %>%
    select(Date, County, State, Confirmed, Death) %>%
    rename(Deaths = Death)

# Global Preprocessing ---------------------------------------------------------

# Removing some irrelevant rows
data <- data %>%
    filter(!Country %in% c(
        "Winter Olympics 2022",
        "Summer Olympics 2020",
        "Diamond Princess",
        "MS Zaandam",
        "Antarctica"
    ))

# Correcting the name of the countries
data <- data %>%
    mutate(Country = case_when(
        Country == "US" ~ "United States of America",
        Country == "West Bank and Gaza" ~ "Palestine",
        Country == "Congo (Brazzaville)" ~ "Dem. Rep. Congo",
        Country == "Congo (Kinshasa)" ~ "Congo",
        Country == "Central African Republic" ~ "Central African Rep.",
        Country == "South Sudan" ~ "S. Sudan",
        Country == "Cote d'Ivoire" ~ "Côte d'Ivoire",
        Country == "Taiwan*" ~ "Taiwan",
        Country == "Korea, South" ~ "South Korea",
        Country == "Bosnia and Herzegovina" ~ "Bosnia and Herz.",
        Country == "Dominican Republic" ~ "Dominican Rep.",
        TRUE ~ Country # Default case to keep original names for other countries
    ))

# Adding population of each country
data <- left_join(data, pops, by = "Country")

# Geting the countries choices
countries_choices <- data |>
    distinct(Country) |>
    arrange(Country) |>
    pull(Country)

# Calculate the daily confirmed cases, recovered cases, and deaths over the whole world.
data <- data %>%
    arrange(Date) %>%
    group_by(Country) %>%
    mutate(
        daily_confirmed = ifelse(Confirmed - lag(Confirmed, default = first(Confirmed)) < 0,
            0,
            Confirmed - lag(Confirmed, default = first(Confirmed))
        ),
        daily_recovered = ifelse(Recovered - lag(Recovered, default = first(Recovered)) < 0,
            0,
            Recovered - lag(Recovered, default = first(Recovered))
        ),
        daily_deaths = ifelse(Deaths - lag(Deaths, default = first(Deaths)) < 0,
            0,
            Deaths - lag(Deaths, default = first(Deaths))
        )
    ) %>%
    ungroup()

# Pivoting the dataset
long_data <- data %>%
    pivot_longer(
        cols = c(daily_confirmed, daily_recovered, daily_deaths),
        names_to = "Case_Type",
        values_to = "Cases",
        names_prefix = "daily_"
    ) %>%
    select(-Confirmed, -Recovered, -Deaths)

# Calculating normalized daily confirmed cases, recovered cases, and deaths per 100000
data <- data %>%
    mutate(
        normalized_daily_confirmed = (daily_confirmed / Population) * 100000,
        normalized_daily_recovered = (daily_recovered / Population) * 100000,
        normalized_daily_deaths = (daily_deaths / Population) * 100000
    )


# US preprocessing -------------------------------------------------------------

# Calculate the total cumulative cases and deaths for each state on each day, converting them to daily by taking the difference from the previous day
us_state_daily_cumulate <- us_ %>%
    group_by(Date, State) %>%
    summarize(
        Confirmed = sum(Confirmed, na.rm = TRUE),
        Deaths = sum(Deaths, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    group_by(State) %>%
    arrange(Date, .by_group = TRUE) %>%
    mutate(
        Daily_Confirmed = ifelse(Confirmed - lag(Confirmed, default = first(Confirmed)) < 0,
            0,
            Confirmed - lag(Confirmed, default = first(Confirmed))
        ),
        Daily_Deaths = ifelse(Deaths - lag(Deaths, default = first(Deaths)) < 0,
            0,
            Deaths - lag(Deaths, default = first(Deaths))
        )
    )

# Pivoting the dataset
us_long_data <- us_state_daily_cumulate %>%
    pivot_longer(
        cols = c(Daily_Confirmed, Daily_Deaths),
        names_to = "Case_Type",
        values_to = "Cases",
        names_prefix = "daily_"
    ) %>%
    select(-Confirmed, -Deaths)

# Geting the US state choices
states_choices <- us_state_daily_cumulate |>
    distinct(State) |>
    arrange(State) |>
    pull(State)

# Choosing the start and end date for input dates
start_date <- "2020-01-23"
end_date <- "2022-04-16"



# vacination data
vaccination_data <- read_csv("./data/country_vaccinations_by_manufacturer.csv") %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    group_by(location) %>%
    # Summarize to find the maximum total vaccinations and get the associated date
    summarize(
        Total_Vaccinations = max(total_vaccinations, na.rm = TRUE),
        date = date[which.max(total_vaccinations)],
        vaccine = vaccine,
        .groups = "drop"
    ) %>%
    arrange(desc(Total_Vaccinations))

# without preprossed data
manufacture_data <- read_csv("./data/country_vaccinations_by_manufacturer.csv")


# propessed global data
global_data <- read_csv("./data/countries.csv") %>%
    mutate(Date = as.Date(Date)) %>%
    pivot_longer(
        cols = c(Confirmed, Recovered, Deaths),
        names_to = "Case_Type",
        values_to = "Cases"
    ) %>%
    mutate(Case_Type = tolower(gsub(" ", "_", Case_Type))) %>%
    na.omit()


ui <- fluidPage(
    theme = shinytheme("united"),
    tags$head(

        # Custom CSS  
        tags$style(HTML("

            body {
                background-image: url('https://img.freepik.com/free-vector/3d-virus-bacteria-infection-banner-with-text-space_1017-24372.jpg?w=1380&t=st=1714364581~exp=1714365181~hmac=7c887513452dbf7f262c3697db5058d26004d19c04e730a3af65be4ed6c04071');
                background-size: cover;
                background-repeat: no-repeat;
                background-attachment: fixed;
                position: relative;
            }
            body::after {
                content: '';
                position: absolute;
                top: 0;
                left: 0;
                right: 0;
                bottom: 0;
                background-color: rgba(255, 255, 255, 0.7); /* white with 50% opacity */
                z-index: -1;
            }
               
            .navbar {
                background-color: #3768abb5 !important; /* Dark gray background */
                color: #FFFFFF !important; /* White text color */
            }
            .navbar-default .navbar-nav>li>a {
                color: #FFFFFF; /* White text color for links */
            }
            .navbar-default .navbar-nav>li>a:hover,
            .navbar-default .navbar-nav>li>a:focus {
                background-color: #555555; /* Slightly lighter gray for hover */
                color: #FFFFFF; 
            }
            .well {
                background-color: #f5f5f5f2;
            }
            .nav-tabs>li>a {
                color: #000000; /* White text for inactive tabs */
                font-weight: bold;
            }
            .nav-tabs>li.active>a,
            .nav-tabs>li.active>a:hover,
            .nav-tabs>li.active>a:focus {
                color: #000000; /* Black text for active tab */
                background-color: #FFFFFF; /* White background for active tab */
            }
            h1 {
                color: #c34113;
                font-family: serif;
            }
        "))
    ),
   

# Navabar
    navbarPage(
        theme = "cerulean",
        "COVID-19 Data Analysis",
        # 1st tabpanel
        tabPanel(
            "Time Series",
            # subtabs
            tabsetPanel(
                tabPanel(
                    "Global Timeseries",
                    headerPanel("Country's COVID-19 Time Series"),
                    sidebarLayout(
                        sidebarPanel(
                            HTML("<h1>Input Country</h1>"),
                            selectInput("Country",
                                label = "Country:",
                                choices = countries_choices,
                                selected = "Afghanistan"
                            ),
                            dateRangeInput("date", strong("Date range"),
                                start = start_date, end = end_date,
                                min = start_date, max = end_date
                            ),
                            selectInput("smoothing_window",
                                label = "Smoothing Window Size:",
                                choices = 1:20,
                                selected = 1
                            )
                        ),
                        mainPanel(tabPanel("COVID-19 Time Series", plotOutput(outputId = "timeseries")))
                    )
                ),
                tabPanel(
                    "US Timeseries",
                    headerPanel("US COVID-19 Time Series"),
                    sidebarLayout(
                        sidebarPanel(
                            HTML("<h1>Input State</h1>"),
                            selectInput("State",
                                label = "State:",
                                choices = states_choices,
                                selected = "Arizona"
                            ),
                            dateRangeInput("us_date", strong("Date range"),
                                start = start_date, end = end_date,
                                min = start_date, max = end_date
                            ),
                            selectInput("us_smoothing_window",
                                label = "Smoothing Window Size:",
                                choices = 1:20,
                                selected = 5
                            )
                        ),
                        mainPanel(tabPanel("COVID-19 Time Series", plotOutput(outputId = "us_timeseries")))
                    )
                )
            )
        ),
        # second tab
        tabPanel(
            "Maps",
            tabsetPanel(
                tabPanel(
                    "Spatial Distribution",
                    headerPanel("Each Day Spatial Distribution of Confirmed COVID-19 Cases"),
                    sidebarPanel(
                        dateInput(
                            inputId = "selectedDate",
                            label = "Choose a date:",
                            min = as.Date(start_date),
                            max = as.Date(end_date),
                            value = as.Date("2020-01-23"),
                            format = "yyyy-mm-dd"
                        ),
                        selectInput("case_type",
                            label = "Case:",
                            choices = c(
                                "Confirmed" = "daily_confirmed",
                                "Recovered" = "daily_recovered",
                                "Deaths" = "daily_deaths"
                            )
                        )
                    ),
                    mainPanel(leafletOutput("distributionMap"))
                ),
                tabPanel(
                    "Normalized Spatial Distribution",
                    headerPanel("Each Day Spatial Distribution of COVID-19 Different Cases per 100,000 People"),
                    sidebarPanel(
                        dateInput(
                            inputId = "normalizedSelectedDate",
                            label = "Choose a date:",
                            min = as.Date(start_date),
                            max = as.Date(end_date),
                            value = as.Date("2020-01-23"),
                            format = "yyyy-mm-dd"
                        ),
                        selectInput("normalized_case_type",
                            label = "Case:",
                            choices = c(
                                "Confirmed" = "normalized_daily_confirmed",
                                "Recovered" = "normalized_daily_recovered",
                                "Deaths" = "normalized_daily_deaths"
                            )
                        )
                    ),
                    mainPanel(leafletOutput("normalizedDistributionMap"))
                ),
            )
        ),
        # Third tab

        tabPanel(
            "Vaccination",
            tabsetPanel(
                tabPanel(
                    "Vaccination Data",
                    headerPanel("COVID-19 Case Types in Top 10 Vaccinated Countries"),
                    sidebarPanel(
                        dateRangeInput("vaccineDateRange", "Select Date Range for Vaccination Data:",
                            start = as.Date("2020-12-29"),
                            end = as.Date("2022-03-29"),
                            min = as.Date("2020-12-29"),
                            max = as.Date("2022-03-29")
                        )
                    ),
                    mainPanel(plotlyOutput("vaccinePlot"))
                ),
                tabPanel(
                    "Vaccination Manufacture Data",
                    headerPanel("Vaccination Data by Manufacturer"),
                    sidebarPanel(
                        selectInput("country", "Choose a country:",
                            choices = unique(manufacture_data$location)
                        ),
                        selectInput("vaccine", "Choose a vaccine:",
                            choices = unique(manufacture_data$vaccine)
                        ),
                        radioButtons("plot_type", "Plot type:",
                            choices = c(
                                "Line" = "line",
                                "Scatter" = "scatter",
                                "Both" = "both"
                            ),
                            selected = "both"
                        ),
                    ),
                    mainPanel(
                        plotOutput("vaccination_plot")
                    )
                )
            )
        )
    )
)

server <- function(input, output) {
    output$timeseries <- renderPlot({
        # Filtering the country
        X <- data %>%
            filter(Country == input$Country)

        # Smoothing the daily timeseries
        data_smoothed <- X %>%
            mutate(across(
                .cols = c(daily_confirmed, daily_recovered, daily_deaths),
                .fns = ~ rollapply(
                    data = .x,
                    width = as.integer(input$smoothing_window),
                    FUN = mean,
                    fill = .x[c(1, 2, length(.x) - 1, length(.x))],
                    align = "center"
                )
            ))

        # Pivoting the dataset
        long_data <- data_smoothed %>%
            pivot_longer(
                cols = c(daily_confirmed, daily_recovered, daily_deaths),
                names_to = "Case_Type",
                values_to = "Cases",
                names_prefix = "daily_"
            ) %>%
            select(-Confirmed, -Recovered, -Deaths)


        X <- long_data %>%
            filter(Date >= input$date[1] & Date <= input$date[2])

        ggplot(X, aes(x = Date, y = Cases, color = Case_Type)) +
            geom_line(size = 0.5) +
            labs(
                title = "COVID-19 Time Series for: Confirmed, Recovered, and Deaths Cases",
                x = "Date", y = "Number of Cases",
                color = "Case Type"
            ) +
            theme_minimal() +
            scale_color_manual(values = c("confirmed" = "blue", "recovered" = "green", "deaths" = "red")) +
            facet_wrap(~Case_Type, scales = "free_y", nrow = 3) +
            theme(
                plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                legend.title = element_text(size = 14),
                legend.text = element_text(size = 12),
                axis.title.x = element_text(size = 14, face = "bold"),
                axis.title.y = element_text(size = 14, face = "bold"),
                axis.text.x = element_text(size = 12),
                axis.text.y = element_text(size = 12),
                strip.text = element_blank(),
                panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
                panel.spacing = unit(1, "lines"),
            )
    })

    output$us_timeseries <- renderPlot({
        # Filtering the state
        X <- us_state_daily_cumulate %>%
            filter(State == input$State)


        print(as.integer(input$us_smoothing_window))
        # Smoothing the dataset
        us_data_smoothed <- X %>%
            mutate(across(
                .cols = c(Daily_Confirmed, Daily_Deaths),
                .fns = ~ rollapply(
                    data = .x,
                    width = as.integer(input$us_smoothing_window),
                    FUN = mean,
                    fill = .x[c(1, 2, length(.x) - 1, length(.x))],
                    align = "center"
                )
            ))

        # Pivoting the dataset
        us_long_data <- us_data_smoothed %>%
            pivot_longer(
                cols = c(Daily_Confirmed, Daily_Deaths),
                names_to = "Case_Type",
                values_to = "Cases",
                names_prefix = "daily_"
            ) %>%
            select(-Confirmed, -Deaths)

        X <- us_long_data %>%
            filter(Date >= input$us_date[1] & Date <= input$us_date[2])

        ggplot(X, aes(x = Date, y = Cases, color = Case_Type)) +
            geom_line(size = 0.5) +
            labs(
                title = "COVID-19 Time Series for: Confirmed and Deaths Cases",
                x = "Date", y = "Number of Cases",
                color = "Case Type"
            ) +
            theme_minimal() +
            scale_color_manual(values = c("Daily_Confirmed" = "blue", "Daily_Deaths" = "red")) +
            facet_wrap(~Case_Type, scales = "free_y", nrow = 3) +
            theme(
                plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                legend.title = element_text(size = 14),
                legend.text = element_text(size = 12),
                axis.title.x = element_text(size = 14, face = "bold"),
                axis.title.y = element_text(size = 14, face = "bold"),
                axis.text.x = element_text(size = 12),
                axis.text.y = element_text(size = 12),
                strip.text = element_blank(),
                panel.border = element_rect(colour = "black", fill = NA, size = 1),
                panel.spacing = unit(1, "lines")
            )
    })

    output$distributionMap <- renderLeaflet({
        # Filter data for the selected date
        date_data <- data %>%
            filter(Date == input$selectedDate)

        # Get the world map data
        world <- ne_countries(scale = "medium", returnclass = "sf")
        world_data <- merge(world, date_data, by.x = "name", by.y = "Country", all.x = TRUE)

        # Getting selected case type dynamically
        selected_case_type <- input$case_type

        num_colors <- 10
        pal <- colorBin("YlOrRd", domain = world_data[[selected_case_type]], bins = num_colors, na.color = "#808080")

        # creating maps
        map <- leaflet(world_data) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = 0, lat = 20, zoom = 1.5) %>%
            addPolygons(
                fillColor = ~ pal(eval(as.name(selected_case_type))),
                fillOpacity = 0.7,
                color = "#BDBDC3",
                weight = 0.5,
                popup = ~ paste(name, "<br>", selected_case_type, ": ", eval(as.name(selected_case_type)))
            ) %>%
            addLegend(
                "bottomright",
                pal = pal,
                values = ~ eval(as.name(selected_case_type)),
                title = "Cases",
                opacity = 0.7
            )

        return(map)
    })

    output$normalizedDistributionMap <- renderLeaflet({
        date_data <- data %>%
            filter(Date == input$normalizedSelectedDate)

        world <- ne_countries(scale = "medium", returnclass = "sf")
        world_data <- merge(world, date_data, by.x = "name", by.y = "Country", all.x = TRUE)

        selected_case_type <- input$normalized_case_type

        # Define a color palette
        num_colors <- 10
        pal <- colorBin("YlOrRd", domain = world_data[[selected_case_type]], bins = num_colors, na.color = "#808080")

        map <- leaflet(world_data) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = 0, lat = 20, zoom = 1.5) %>%
            addPolygons(
                fillColor = ~ pal(eval(as.name(selected_case_type))),
                fillOpacity = 0.7,
                color = "#BDBDC3",
                weight = 0.5,
                popup = ~ paste(name, "<br>", "Normalized Cases: ", eval(as.name(selected_case_type)))
            ) %>%
            addLegend(
                "bottomright",
                pal = pal,
                values = ~ eval(as.name(selected_case_type)),
                title = "Cases per 100,000",
                opacity = 0.7
            )
    })

    output$vaccinePlot <- renderPlotly({
        # checking whether data is having or not
        req(input$vaccineDateRange)
        # Filtering vaccination data within the selected date range
        vaccine_data_filtered <- vaccination_data %>%
            filter(date >= input$vaccineDateRange[1], date <= input$vaccineDateRange[2]) %>%
            group_by(location) %>%
            summarize(Total_Vaccinations = max(Total_Vaccinations, na.rm = TRUE), .groups = "drop") %>%
            arrange(desc(Total_Vaccinations)) %>%
            slice_max(order_by = Total_Vaccinations, n = 10)

        global_data_filtered <- global_data %>%
            filter(Country %in% vaccine_data_filtered$location & Date >= input$vaccineDateRange[1] & Date <= input$vaccineDateRange[2]) %>%
            group_by(Country, Case_Type) %>%
            summarize(Total_Cases = sum(Cases, na.rm = TRUE), .groups = "drop")

        p <- ggplot(global_data_filtered, aes(x = Country, y = Total_Cases, fill = Case_Type)) +
            geom_bar(stat = "identity", position = "stack") +
            theme_minimal() +
            labs(
                title = "COVID-19 Case Types in Top 10 Vaccinated Countries",
                x = "Country", y = "Total Cases",
                fill = "Case Type"
            )
        p <- ggplotly(p, tooltip = "y")
        p
    })



    # manufacture
    manufacture_data$date <- as.Date(manufacture_data$date)
    selected_data <- reactive({
        manufacture_data[manufacture_data$location == input$country & manufacture_data$vaccine == input$vaccine, ]
    })
    # Render plot
    output$vaccination_plot <- renderPlot({
        plot_type <- switch(input$plot_type,
            "line" = geom_line(),
            "scatter" = geom_point(),
            "both" = list(geom_line(), geom_point())
        )
        ggplot(selected_data(), aes(x = date, y = total_vaccinations, color = location)) +
            plot_type +
            labs(
                title = paste("Total Vaccinations Over Time in", input$country, "for", input$vaccine),
                x = "Date",
                y = "Total Vaccinations",
                color = "Country"
            ) +
            theme_minimal()+
            theme(
                legend.position = 'none'
            )
    })
}

# Creating shiny app
shinyApp(ui = ui, server = server)

# nolint end