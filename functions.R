# Keeping libraries here in case I write something wrong, to be deleted
library(shiny)
library(bslib)
library(httr)
library(jsonlite)
library(ggplot2)

#' @export
get_weather <- function (start_date, end_date = start_date){
  start_date_d <- as.Date(start_date)
  end_date_d <- as.Date(end_date)
  if (end_date_d - start_date_d > 30) #
    error("Time intervals longer than 30 days are not allowed")
  if (start_date_d < as.Date("2023-01-01"))
    errorCondition("Only dates since 2023 are supported")
  # not finished
}

#' @export
plot_temperature <- function(start_date, end_date = start_date, city){
  res_js = httr::GET(paste0("http://api.weatherapi.com/v1/history.json?key=205ae0f94f0d4e9cb1875731242105&q=",city,"&dt=",start_date,"&end_dt=",end_date))
  data <- jsonlite::fromJSON(rawToChar(res_js$content))
  #Errors
  if (httr::status_code(res_js) != 200) {
    stop("Error: Failed to retrieve data from API.")
  }
  data$forecast$forecastday$date <- as.Date(data$forecast$forecastday$date)
  df <- cbind(data$forecast$forecastday$date,as.data.frame(data$forecast$forecastday$day), as.data.frame(data$forecast$forecastday$astro))
  df <- as.data.frame(df)
  #Plotting temperature and precipitation
  colnames(df)[1] <- "date"
  ggplot2::ggplot(data = df, ggplot2::aes(x = date)) + 
    ggplot2::geom_bar(ggplot2::aes(y = totalprecip_mm), stat = 'identity', alpha = 0.2, fill = "#16BCD0")+
    ggplot2::geom_line(color="#FF7400", ggplot2::aes(y = maxtemp_c), linewidth=0.5, group = "High") + 
    ggplot2::geom_line(color = "#2283FF", ggplot2::aes(y = mintemp_c), linewidth = 0.5, group = "Low") +
    ggplot2::xlab("Date")+
    ggplot2::ylab("Temperature (Celcius)")+
    ggplot2::scale_y_continuous(limits = c(-30, 50),sec.axis = sec_axis(~ . * 1, name = "Precipitation(mm)"))+
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%Y-%m-%d")
}

ui <- fluidPage(
  titlePanel("Weather Data Plotter"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("city", "City", choices = NULL, options = list(create = TRUE, placeholder = "Type a city name")),
      dateRangeInput("dates", "Date Range", start = Sys.Date() - 30, end = Sys.Date()),
      actionButton("plot", "Plot Weather")
    ),
    mainPanel(
      plotOutput("tempPlot")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$plot, {
    start_date <- input$dates[1]
    end_date <- input$dates[2]
    if (as.numeric(difftime(end_date, start_date, units = "days")) > 30) {
      warning("Date range cannot exceed 30 days.")
    } else {
      output$tempPlot <- renderPlot({
        plot_temperature(start_date, end_date, input$city)
      })
    }
  })
}
shinyApp(ui = ui, server = server)
