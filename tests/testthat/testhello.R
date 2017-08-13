#load_all("RCPTEC")
#test("RCPTEC") function

library(testthat)
test_check("RCPTEC")

hello()
getWeatherData(lon = -75.05, lat = -35.05, weatherData = 'TP')
getWeatherData(lon = -75.05, lat = -35.05, weatherData = 'OCIS')
getWeatherData(lon = -75.05, lat = -35.05, weatherData = 'PREC')
getWeatherData(lon = -75.05, lat = -35.05, weatherData = 'UR')
getWeatherData(lon = -75.05, lat = -35.05, weatherData = 'V10M')

