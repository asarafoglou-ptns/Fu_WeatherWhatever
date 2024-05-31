#' Retrieve weather data from WeatherAPI
#' @description
#' This function retrieves historical weather data from WeatherAPI for a given location and time period.  
#' This API key is valid until Jun 14, 2024.  
#' Supports start dates since 2023-01-01 and maximum time interval is 30 days (for example, from May 1 to May 31).
#' @param start_date Start date of the data retrieval (in "YYYY-MM-DD" format).
#' @param end_date End date of the data retrieval (in "YYYY-MM-DD" format). Defaults to start_date.
#' @param city Name of the city for which weather data is to be retrieved. Either city name or latitude and longitude should be provided.
#' @param lat Latitude of the location for which weather data is to be retrieved. Either city name or latitude and longitude should be provided.
#' @param lng Longitude of the location for which weather data is to be retrieved. Either city name or latitude and longitude should be provided.
#' @return A data frame containing the retrieved weather data.
#' @examples
#' get_wdata('2024-05-01','2024-05-31','Amsterdam')
#' get_wdata('2024-05-01','2024-05-31', 52.3676, 4.9041)
#' @export
get_wdata <- function(start_date, end_date = start_date, city = NULL, lat = NULL, lng = NULL) {
  city <- str_replace_all(city, " ", "_")
  if (!is.null(lat) & !is.null(lng)) {
    res_js <- httr::GET(
      paste0(
        "http://api.weatherapi.com/v1/history.json?key=205ae0f94f0d4e9cb1875731242105&q=",
        lat,
        ",",
        lng,
        "&dt=",
        start_date,
        "&end_dt=",
        end_date
      )
    )
  } else if (!is.null(city)) {
    res_js <- httr::GET(
      paste0(
        "http://api.weatherapi.com/v1/history.json?key=205ae0f94f0d4e9cb1875731242105&q=",
        city,
        "&dt=",
        start_date,
        "&end_dt=",
        end_date
      )
    )
  }
  
  if (httr::status_code(res_js) != 200) {
    warning("Failed to retrive API data.")
    return(NULL) # Returns NULL to avoid errors in Shiny
  }
  
  data <- jsonlite::fromJSON(rawToChar(res_js$content))
  data$forecast$forecastday$date <- as.Date(data$forecast$forecastday$date)
  df <- cbind(
    data$forecast$forecastday$date,
    as.data.frame(data$forecast$forecastday$day),
    as.data.frame(data$forecast$forecastday$astro)
  )
  df <- as.data.frame(df)
  colnames(df)[1] <- "date"
  return(df)
}
#' Plot temperature data
#' @description
#' This function plots the daily maximum and minimum temperature and precipitation retrieved from WeatherAPI for a given location and time period.
#' @seealso [get_wdata()] for retrieving data from WeatherAPI
#' @param start_date Start date of the data retrieval (in "YYYY-MM-DD" format).
#' @param end_date End date of the data retrieval (in "YYYY-MM-DD" format). Defaults to start_date.
#' @param city Name of the city for which weather data is to be retrieved. Either city name or latitude and longitude should be provided.
#' @param lat Latitude of the location for which weather data is to be retrieved. Either city name or latitude and longitude should be provided.
#' @param lng Longitude of the location for which weather data is to be retrieved. Either city name or latitude and longitude should be provided.
#' @examples
#' plot_temperature('2024-05-01','2024-05-31','Amsterdam')
#' plot_temperature('2024-05-01','2024-05-31', 52.3676, 4.9041)
#' @export
plot_temperature <- function(start_date, end_date = start_date, city = NULL, lat = NULL, lng = NULL) {
  df <- get_wdata(start_date, end_date, city, lat, lng)
  if (is.null(df)) # Checks if API is correctly retrieved
    return();
  max_temp <- max(df$maxtemp_c, na.rm = TRUE)
  min_temp <- min(df$mintemp_c, na.rm = TRUE)
  max_precip <- max(df$totalprecip_mm, na.rm = TRUE)
  
  ggplot2::ggplot(data = df, ggplot2::aes(x = date)) +
    ggplot2::geom_bar(
      ggplot2::aes(y = totalprecip_mm),
      stat = 'identity',
      alpha = 0.2,
      fill = "#16BCD0"
    ) +
    ggplot2::geom_line(
      color = "#FF7400",
      ggplot2::aes(y = maxtemp_c),
      linewidth = 0.5,
      group = 1
    ) +
    ggplot2::geom_line(
      color = "#2283FF",
      ggplot2::aes(y = mintemp_c),
      linewidth = 0.5,
      group = 1
    ) +
    ggplot2::xlab("Date") +
    ggplot2::ylab("Temperature (Celsius)") +
    ggplot2::scale_y_continuous(
      limits = c(min(min_temp - 5, 0), max(max_temp + 5, max_precip + 5)),
      sec.axis = sec_axis(~ . * 1, name = "Precipitation(mm)")
    ) +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%Y-%m-%d") + 
    ggplot2::theme_bw()
}
#' Summarizing Weather Data
#' @description
#' This function summarizes the maximum and minimum temperature, total precipitation, total snow, number of sunny days, and maximum wind speed
#' retrieved from WeatherAPI for a given location and time period.
#' @seealso [get_wdata()] for retrieving data from WeatherAPI
#' @param start_date Start date of the data retrieval (in "YYYY-MM-DD" format).
#' @param end_date End date of the data retrieval (in "YYYY-MM-DD" format). Defaults to start_date.
#' @param city Name of the city for which weather data is to be retrieved. Either city name or latitude and longitude should be provided.
#' @param lat Latitude of the location for which weather data is to be retrieved. Either city name or latitude and longitude should be provided.
#' @param lng Longitude of the location for which weather data is to be retrieved. Either city name or latitude and longitude should be provided.
#' @return a list of weather summary: maximum and minimum temperature, total precipitation, total snow, number of sunny days, and maximum wind speed
#' @examples
#' weather_stats <- wstats('2024-05-01','2024-05-31','Amsterdam')
#' weather_stats <- wstats('2024-05-01','2024-05-31', 52.3676, 4.9041)
#' @export
wstats <- function(start_date, end_date = start_date, city = NULL, lat = NULL, lng = NULL) {
  df <- get_wdata(start_date, end_date, city, lat, lng)
  if (is.null(df)) # Checks if API is correctly retrieved
    return();
  max_temp <- max(df$maxtemp_c, na.rm = TRUE)
  min_temp <- min(df$mintemp_c, na.rm = TRUE)
  total_precip <- sum(df$totalprecip_mm, na.rm = TRUE)
  total_snow <- sum(df$totalsnow_cm, na.rm = TRUE)
  total_sunny <- sum(df$condition$text == "Sunny", na.rm = TRUE)
  max_wind <- max(df$maxwind_kph, na.rm = TRUE)
  list(
    max_temp = max_temp,
    min_temp = min_temp,
    total_precip = total_precip,
    total_snow = total_snow,
    total_sunny = total_sunny,
    max_wind = max_wind
  )
}
#' Summing up Weather Conditions
#' @description
#' This function plots the number of days of different weather conditions retrieved from WeatherAPI for a given location and time period.
#' @seealso [get_wdata()] for retrieving data from WeatherAPI
#' 
#' @param start_date Start date of the data retrieval (in "YYYY-MM-DD" format).
#' @param end_date End date of the data retrieval (in "YYYY-MM-DD" format). Defaults to start_date.
#' @param city Name of the city for which weather data is to be retrieved. Either city name or latitude and longitude should be provided.
#' @param lat Latitude of the location for which weather data is to be retrieved. Either city name or latitude and longitude should be provided.
#' @param lng Longitude of the location for which weather data is to be retrieved. Either city name or latitude and longitude should be provided.
#' @examples
#' plot_weather('2024-05-01','2024-05-31','Amsterdam')
#' plot_weather('2024-05-01','2024-05-31', 52.3676, 4.9041)
#' @export
plot_weather <- function(start_date, end_date = start_date, city = NULL, lat = NULL, lng = NULL) {
  df <- get_wdata(start_date, end_date, city, lat, lng)
  if (is.null(df)) # Checks if API is correctly retrieved
    return();
  ggplot2::ggplot(data = df, ggplot2::aes(x = condition$text)) + 
    ggplot2::geom_bar(stat = 'count', fill = '#16BCD0', alpha = 0.5) + 
    ggplot2::ylab("Days") + 
    ggplot2::xlab("Weather") + 
    ggplot2::theme_bw() + 
    ggplot2::theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1, vjust = 1))
}

