#setwd("")

getWeatherData <- function(lon, lat, iTime, fTime="", weatherData= "all") {

  if(weatherData == "all"){
    getWeatherHour(lon1 = lon ,lat1 = lat, iTime1 = iTime, fTime1 = fTime)
  }
  else{
    d <- loadRda(weatherData)
    verifyLonLat(d, lon, lat)
    lonlat = ajuste_ponto(d[1:2],lon,lat)

    if(fTime=="")
      temp = subset(d, LONGITUDE == lonlat[1] & LATITUDE == lonlat[2], select=iTime)
    else
      temp = subset(d, LONGITUDE == lonlat[1] & LATITUDE == lonlat[2], select = match(iTime, names(d)):match(fTime, names(d)))

    row.names(temp) <- weatherData
    return(temp)
  }
}
getWeatherData(lon = -50.88, lat = -28.51, iTime = '2017080700', fTime = '2017080710')
getWeatherData(lon = -50.88, lat = -28.51, iTime = '2017080700')
getWeatherData(lon = -50.88, lat = -28.51, iTime = '2017080700', weatherData = 'OCIS')

#getWeatherData(-50.88,-28.51, 'OCIS')
#getWeatherData(-50.88,-28.51, 'PREC')
#getWeatherData(-50.88,-28.51, 'UR2M')
#getWeatherData(-50.88,-28.51, 'V10M')

print.RCPTEC <- function(df){

}

getWeatherHour<- function(lon1, lat1, iTime1, fTime1){
  temp = getWeatherData(lon = lon1, lat = lat1, iTime = iTime1, fTime = fTime1, weatherData = 'TP2M')
  oc = getWeatherData(lon = lon1, lat = lat1, iTime = iTime1, fTime = fTime1, weatherData = 'OCIS')
  precip = getWeatherData(lon = lon1, lat = lat1, iTime = iTime1, fTime = fTime1, weatherData = 'PREC')
  humi = getWeatherData(lon = lon1, lat = lat1, iTime = iTime1, fTime = fTime1, weatherData = 'UR2M')
  wind = getWeatherData(lon = lon1, lat = lat1, iTime = iTime1, fTime = fTime1, weatherData = 'V10M')

  # df <- data.frame(Temperature = temp, Radiation = oc, Precipitation = precip, Humidity = humi, Wind = wind)

  w = rbind(Temperature = temp, Radiation = c, Precipitation = precip, Humidity = humi, Wind = wind)
  return(w)
}
w = getWeatherHour(lon = -50.88,lat = -28.51, iTime = '2017080700')
print(w)

verifyLonLat <- function(dado, lon, lat){
  limite_lon = range(dado$LONGITUDE)
  limite_lat = range(dado$LATITUDE)

  if(lon < limite_lon[1] || lon > limite_lon[2])
    stop("### Coordenada da longitude fora da faixa de dados da previsão ###")
  if(lat < limite_lat[1] || lat > limite_lat[2])
    stop("### Coordenada da latitude fora da faixa de dados da previsão ###")

  # lonlat = ajuste_ponto(dado[1:2],lon,lat)
}

ajuste_ponto <- function(dados,longitude,latitude){

  ponto_lon = dados$LONGITUDE[which.min(dados$LONGITUDE < longitude)-1]
  ponto_lat = dados$LATITUDE[which.min(dados$LATITUDE < latitude)]

  return(c(ponto_lon,ponto_lat))
}

library(jsonlite)

getClimateData<- function(modelID, modelFrequency, modelVar, lon, lat, iMonth = 1, iYear, fMonth = 1, fYear) {

  modelOption <- switch(modelFrequency,
                        HOURLY = '/1',
                        DAILY = '/2',
                        MONTHLY = '/3',
                        YEARLY = '/4')

  api <- paste("https://projeta.cptec.inpe.br/api/v1/public/ETA/", modelID, "/", modelFrequency, modelOption, "/", iMonth, "/", iYear, "/", fMonth, "/", fYear, "/", modelVar, "/", lon, '/',lat,'/', sep="")
  model_data <- fromJSON(api)
  model_output <- list(Model = modelID, Frequency = modelFrequency, Value = model_data$value,
                       Year = model_data$date)
  model_output

}

teste <- getClimateData('1', 'YEARLY','TP2M', '-12', '-49', 1, 2006, 12, 2010)
teste$Year = c(2006:2010)
getClimateData('1', 'YEARLY','TP2M', '-12', '-49', iYear=2006, fYear=2010)

loadRda <- function(fileName){
  load(paste("~/RCPTEC/R/data/", fileName, ".Rdata", sep = ""))
  get(ls()[ls() != "fileName"])
}

info.RCPTEC <- function(){

  cat(paste("CPTEC/INPE",
            "Eta Model 15km Brazil",

            "AREA COVERED IN THE MODEL",
            "LONGITUDE:   -75.05 to -33.95,",
            "LATITUDE:   -35.05 to 5.90",

            "Variables:",
            "- V10M - wind up 10m (m/s)",
            "- TP2M - temperature up 2m",
            "- PREC - precipitation",
            "- UR2M - humidity",
            "- OCIS - solar radiation",

            "FORECAST PERIOD (11 days)",
            "YYYYMMDDHH to YYYYMMDDHH (Initial YearMonthDayHour to Final YearMonthDayHour)",  sep='\n'))
}

info.RCPTEC()

library(jsonlite)
api <- fromJSON("https://projeta.cptec.inpe.br/api/v1/public/ETA/1/YEARLY/4/1/2006/12/2010/TP2M/-12/-49/")
api <- unname(api)
a <- data.table(LONGITUDE=api[6])

