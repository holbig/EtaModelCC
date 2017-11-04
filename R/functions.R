# Verifica a instalação dos pacotes necessários para o programa
list.of.packages <- c("jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

suppressMessages(library(jsonlite))

source(paste(getwd(),"/R/global.R", sep=""))


### Functions for weather models

getWeatherData(lon = -50.88, lat = -28.51,
               iTime = '2017110400', fTime = '2017110423',
               weatherData = 'OCIS')

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
    model_output <- list(Model = "Eta 15km",
                         Frequency = "Hourly",
                         Variable_name = row.names(temp),
                         Variable_description = "TESTE",
                         Dados = temp)
    model_output
  }
}


print.RCPTEC <- function(df){

}

getWeatherHour<- function(lon1, lat1, iTime1, fTime1){
  temp = getWeatherData(lon = lon1, lat = lat1, iTime = iTime1, fTime = fTime1, weatherData = 'TP2M')
  oc = getWeatherData(lon = lon1, lat = lat1, iTime = iTime1, fTime = fTime1, weatherData = 'OCIS')
  precip = getWeatherData(lon = lon1, lat = lat1, iTime = iTime1, fTime = fTime1, weatherData = 'PREC')
  humi = getWeatherData(lon = lon1, lat = lat1, iTime = iTime1, fTime = fTime1, weatherData = 'UR2M')
  wind = getWeatherData(lon = lon1, lat = lat1, iTime = iTime1, fTime = fTime1, weatherData = 'V10M')

  # df <- data.frame(Temperature = temp, Radiation = oc, Precipitation = precip, Humidity = humi, Wind = wind)

  w = rbind(Temperature = temp, Radiation = oc, Precipitation = precip, Humidity = humi, Wind = wind)
  return(w)
}

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

loadRda <- function(fileName){
  load(paste(getwd(),"/R/data/", fileName, ".Rda", sep = ""))
  get(ls()[ls() != "fileName"])
}

info.RCPTEC.weather <- function(){

  cat(paste("CPTEC/INPE",
            "Eta Model 15km Brazil",

            "AREA COVERED IN THE MODEL",
            "LONGITUDE:   -75.05 to -33.95,",
            "LATITUDE:   -35.05 to 5.90",

            "Variables:",
            "V10M - wind up 10m (m/s)",
            "TP2M - temperature up 2m",
            "PREC - precipitation",
            "UR2M - humidity",
            "OCIS - solar radiation",

            "FORECAST PERIOD (11 days)",
            "YYYYMMDDHH to YYYYMMDDHH (Initial YearMonthDayHour to Final YearMonthDayHour)",  sep='\n'))
}


#### Funtions for climate models ###

getClimateData<- function(modelID, modelFrequency, modelVar, lon, lat, iMonth = 1, iYear, fMonth = 1, fYear) {

  modelOption <- switch(modelFrequency,
                        HOURLY = '/1',
                        DAILY = '/2',
                        MONTHLY = '/3',
                        YEARLY = '/4')

  api <- paste("https://projeta.cptec.inpe.br/api/v1/public/ETA/", modelID, "/", modelFrequency, modelOption, "/", iMonth, "/", iYear, "/", fMonth, "/", fYear, "/", modelVar, "/", lon, '/',lat,'/', sep="")

  model_data <- fromJSON(api)

  model_output <- switch(modelFrequency,
                         HOURLY = list(Model = "Eta", Couple = models$couple[as.numeric(modelID)],
                                       Frequency = modelFrequency,
                                       Variable_name = modelVar,
                                       Variable_description = variables$description[which(variables$variable==modelVar)],
                                       Longitude = lon, Latitude = lat,
                                       Data = as.data.frame(cbind(Date = model_data$date,
                                                                  Hour = substr(model_data$time, 4, 8),
                                                                  Value = model_data$value))),
                         DAILY = list(Model = "Eta", Couple = models$couple[as.numeric(modelID)],
                                      Frequency = modelFrequency,
                                      Variable_name = modelVar,
                                      Variable_description = variables$description[which(variables$variable==modelVar)],
                                      Longitude = lon, Latitude = lat,
                                      Data = as.data.frame(cbind(Date = model_data$date,
                                                                 Value = model_data$value))),
                         MONTHLY = list(Model = "Eta", Couple = models$couple[as.numeric(modelID)],
                                        Frequency = modelFrequency,
                                        Variable_name = modelVar,
                                        Variable_description = variables$description[which(variables$variable==modelVar)],
                                        Longitude = lon, Latitude = lat,
                                        Data = as.data.frame(cbind(Year = substr(model_data$date, 1, 4),
                                                                   Month = substr(model_data$date, 6, 7),
                                                                   Value = model_data$value))),
                         YEARLY = list(Model = "Eta", Couple = models$couple[as.numeric(modelID)],
                                       Frequency = modelFrequency,
                                       Variable_name = modelVar,
                                       Variable_description = variables$description[which(variables$variable==modelVar)],
                                       Longitude = lon, Latitude = lat,
                                       Data = as.data.frame(cbind(Year = substr(model_data$date, 1, 4),
                                                                  Value = model_data$value))))
  model_output
}

info.RCPTEC.climate <- function(){
  cat("Avaiable Models: o modelo (modelID) deve ser acessado pelo valor",
      "do campo <id>. Verifique o respectivo período de abrangência do modelo:",
      "data inicial (iMonth, iYear) e data final (fMonth, fYear)", "", sep = '\n')
  print(models, row.names=FALSE, right=FALSE)

  cat (paste("", "modelFrequency - Avaiable frequencies:",
             "HOURLY : horária de 3 em 3 horas ",
             "DAILY  : diária",
             "MONTHLY: mensal",
             "YEARLY : anual", "\n", sep='\n'))

  cat("Avaiable Variables - a variável (modelVar) deve ser acessada",
      "pelo seu short name <variable>", sep = '\n')
  print(variables[,c("variable","description")], row.names=FALSE, right=FALSE)
}

