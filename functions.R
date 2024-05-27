data <- data.frame()
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
  # not finished
  res_js = httr::GET(paste0("http://api.weatherapi.com/v1/history.json?key=205ae0f94f0d4e9cb1875731242105&q=",city,"&dt=",start_date,"&end_dt=",end_date))
  data <- jsonlite::fromJSON(rawToChar(res_js$content))
  df <- cbind(data$forecast$forecastday$date,as.data.frame(data$forecast$forecastday$day))
  df <- as.data.frame(df)
  colnames(df)[1] <- "date"
  ggplot2::ggplot(data = df, ggplot2::aes(x = date)) + 
    ggplot2::geom_line(color="orange", ggplot2::aes(y = maxtemp_c), linewidth=0.5, group = "High") + 
    ggplot2::geom_line(color = "blue", ggplot2::aes(y = mintemp_c), linewidth = 0.5, group = "Low") +
    ggplot2::xlab("Date")+
    ggplot2::ylab("Temperature (Celcius)")
}


# not finished
start_date <- "2024-02-01"
end_date <- "2024-02-22"
city = "Tokyo"
res_js = httr::GET(paste0("http://api.weatherapi.com/v1/history.json?key=205ae0f94f0d4e9cb1875731242105&q=",city,"&dt=",start_date,"&end_dt=",end_date))
data <- jsonlite::fromJSON(rawToChar(res_js$content))
df <- cbind(data$forecast$forecastday$date,as.data.frame(data$forecast$forecastday$day))
df <- as.data.frame(df)
colnames(df)[1] <- "date"



