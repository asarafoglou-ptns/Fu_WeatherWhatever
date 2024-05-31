# Shiny app for plotting weather data
devtools::install_github("asarafoglou-ptns/Fu_WeatherWhatever/WeatherWhatever")
library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)
library(leaflet)
library(stringr)
library(WeatherWhatever)

ui <- fluidPage(
  titlePanel("Weather Data Plotter"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "city",
        "City",
        choices = NULL,
        options = list(create = TRUE, placeholder = "Type a city name")
      ),
      dateRangeInput("dates", "Date Range", start = Sys.Date() - 30, end = Sys.Date()),
      actionButton("weather", "Switch Weather/Temperature"),
      actionButton("clear", "Clear Plot"),
      h3("Weather Statistics"),
      textOutput("max_temp"),
      textOutput("min_temp"),
      textOutput("total_precip"),
      textOutput("total_snow"),
      textOutput("total_sunny"),
      textOutput("max_wind")
    ),
    mainPanel(leafletOutput("map"), plotOutput("tempPlot"))
  )
)

flag <- 1 # flag for the switch weather/temperature button

server <- function(input, output, session) {
  selected_coords <- reactiveVal(NULL)
  
  # Map output
  output$map <- renderLeaflet({
    map <- leaflet()
    map <- addProviderTiles(map, providers$OpenStreetMap)
    map <- setView(map, lng = 0, lat = 0, zoom = 2)
  })
  
  # Check date validity
  observeEvent(input$dates, {
    start_date <- input$dates[1]
    end_date <- input$dates[2]
    if(end_date < start_date)
      showNotification("End date should be after start date.", type = "error")
    if(end_date > Sys.Date())
      showNotification("End date should be no later than today.", type = "error")
    if (end_date - start_date > 30) {
      showNotification("Date range cannot exceed 30 days.", type = "error")
    }
    if (start_date < '2023-01-01') {
      showNotification("Only dates from 2023-01-01 are supported.", type = "error")
    }
  })
  
  # Plot by text input (city)
  observeEvent(input$city, {
    if (!is.null(input$city) && input$city != " ") {
      start_date <- input$dates[1]
      end_date <- input$dates[2]
      coords <- selected_coords()
      if (end_date - start_date > 30) {
        showNotification("Date range cannot exceed 30 days.", type = "error")
      }
      if (start_date < '2023-01-01') {
        showNotification("Only dates from 2023-01-01 are supported.", type = "error")
      }
      city_nospace <- str_replace_all(input$city, " ", "_")
      res <- httr::GET(
        paste0(
          "http://api.positionstack.com/v1/forward?access_key=1d997e60a263a01bd94067619f5e61f3&query=",
          city_nospace
        )
      )
      loc_data <- jsonlite::fromJSON(rawToChar(res$content))
      lat <- loc_data$data$latitude[1]
      lng <- loc_data$data$longitude[1]
      if (!is.null(lng) && !is.null(lat)) {
        selected_coords(c(lat, lng))
        setView(
          clearMarkers(leafletProxy("map")),
          lng = lng,
          lat = lat,
          zoom = 10
        ) %>%
          addMarkers(lng = lng, lat = lat, popup = input$city)
        if (flag == 0) {
          output$tempPlot <- renderPlot({plot_weather(start_date, end_date, input$city)})
        } else {
          output$tempPlot <- renderPlot({plot_temperature(start_date, end_date, input$city)})
        }
        stats <- wstats(start_date, end_date, city = input$city)
        output$max_temp <- renderText({ paste("Max Temperature:", stats$max_temp, "°C") })
        output$min_temp <- renderText({ paste("Min Temperature:", stats$min_temp, "°C") })
        output$total_precip <- renderText({ paste("Total Precipitation:", stats$total_precip, "mm") })
        output$total_snow <- renderText({ paste("Total Snow:", stats$total_snow, "cm") })
        output$total_sunny <- renderText({ paste("Total Sunny Days:", stats$total_sunny) })
        output$max_wind <- renderText({ paste("Max Wind Speed:", stats$max_wind, "kph") })
      }
    }
  })
  
  # Plot by map click
  observeEvent(input$map_click, {
    click <- input$map_click
    lat <- click$lat
    lng <- click$lng
    start_date <- input$dates[1]
    end_date <- input$dates[2]
    coords <- selected_coords()
    res <- httr::GET(
      paste0(
        "http://api.positionstack.com/v1/reverse?access_key=1d997e60a263a01bd94067619f5e61f3&query=",
        lat,
        ",",
        lng
      )
    )
    loc_data <- jsonlite::fromJSON(rawToChar(res$content))
    
    if (!is.null(loc_data$data)) {
      city_name <- loc_data$data$name[1]
      updateSelectizeInput(session, "city", selected = city_name)
      selected_coords(c(lat, lng))
      addMarkers(
        clearMarkers(leafletProxy("map")),
        lng = lng,
        lat = lat,
        popup = city_name
      ) %>%
        leaflet::addPopups(lng = lng, lat = lat, popup = city_name)
      
      if (flag == 0) {
        output$tempPlot <- renderPlot({plot_weather(start_date, end_date, lat = lat, lng = lng)})
      } else {
        output$tempPlot <- renderPlot({plot_temperature(start_date, end_date, lat = lat, lng = lng)})
      }
      
      stats <- wstats(start_date, end_date, lat = lat, lng = lng)
      output$max_temp <- renderText({ paste("Max Temperature:", stats$max_temp, "°C") })
      output$min_temp <- renderText({ paste("Min Temperature:", stats$min_temp, "°C") })
      output$total_precip <- renderText({ paste("Total Precipitation:", stats$total_precip, "mm") })
      output$total_snow <- renderText({ paste("Total Snow:", stats$total_snow, "cm") })
      output$total_sunny <- renderText({ paste("Total Sunny Days:", stats$total_sunny) })
      output$max_wind <- renderText({ paste("Max Wind Speed:", stats$max_wind, "kph") })
    }
  })
  
  # Switch between weather conditions and temperature
  observeEvent(input$weather, {
    flag <<- !flag
    click <- input$map_click
    lat <- click$lat
    lng <- click$lng
    city <- input$city
    start_date <- input$dates[1]
    end_date <- input$dates[2]
    if (flag == 0) {
      output$tempPlot <- renderPlot({plot_weather(start_date, end_date, city, lat, lng)})
    } else {
      output$tempPlot <- renderPlot({plot_temperature(start_date, end_date, city, lat, lng)})
    }
  })
  
  # Refresh plot when new dates are entered
  observeEvent(input$dates, {
    start_date <- input$dates[1]
    end_date <- input$dates[2]
      coords <- selected_coords()
      if (!is.null(coords)) {
        lat <- coords[1]
        lng <- coords[2]
        if (flag == 0) {
          output$tempPlot <- renderPlot({plot_weather(start_date, end_date, lat = lat, lng = lng)})
        } else {
          output$tempPlot <- renderPlot({plot_temperature(start_date, end_date, lat = lat, lng = lng)})
        }
        stats <- wstats(start_date, end_date, lat = lat, lng = lng)
        output$max_temp <- renderText({ paste("Max Temperature:", stats$max_temp, "°C") })
        output$min_temp <- renderText({ paste("Min Temperature:", stats$min_temp, "°C") })
        output$total_precip <- renderText({ paste("Total Precipitation:", stats$total_precip, "mm") })
        output$total_snow <- renderText({ paste("Total Snow:", stats$total_snow, "cm") })
        output$total_sunny <- renderText({ paste("Total Sunny Days:", stats$total_sunny) })
        output$max_wind <- renderText({ paste("Max Wind Speed:", stats$max_wind, "kph") })
      } else if (!is.null(input$city) && input$city != " ") {
        if (flag == 0) {
      output$tempPlot <- renderPlot({plot_weather(start_date, end_date, input$city)})
    } else {
      output$tempPlot <- renderPlot({plot_temperature(start_date, end_date, input$city)})
    }
        stats <- wstats(start_date, end_date, city = input$city)
        output$max_temp <- renderText({ paste("Max Temperature:", stats$max_temp, "°C") })
        output$min_temp <- renderText({ paste("Min Temperature:", stats$min_temp, "°C") })
        output$total_precip <- renderText({ paste("Total Precipitation:", stats$total_precip, "mm") })
        output$total_snow <- renderText({ paste("Total Snow:", stats$total_snow, "cm") })
        output$total_sunny <- renderText({ paste("Total Sunny Days:", stats$total_sunny) })
        output$max_wind <- renderText({ paste("Max Wind Speed:", stats$max_wind, "kph") })
      }
    
  })
  
  # Clear plots
  observeEvent(input$clear, {
    updateSelectizeInput(session, "city", selected = "")
    updateDateRangeInput(session, "dates", start = Sys.Date() - 30, end = Sys.Date())
    selected_coords(NULL)
    setView(
      clearMarkers(leafletProxy("map")),
      lng = 0,
      lat = 0,
      zoom = 2
    )
    output$tempPlot <- renderPlot(NULL)
    output$max_temp <- renderText("")
    output$min_temp <- renderText("")
    output$total_precip <- renderText("")
    output$total_snow <- renderText("")
    output$total_sunny <- renderText("")
    output$max_wind <- renderText("")
  })
}

shinyApp(ui = ui, server = server)
