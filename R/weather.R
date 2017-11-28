# Verifica a instalação dos pacotes necessários para o programa
list.of.packages <- c("jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

suppressMessages(library(jsonlite))

source("R/global.R")


### Functions for weather models

getWeatherData <- function(lon, lat, weatherData= "all", iTime=NULL, fTime=NULL) {

  if(weatherData == "all"){
    #getWeatherHour(lon1 = lon ,lat1 = lat, iTime1 = iTime, fTime1 = fTime)
    TP2M = getWeatherData(lon, lat, weatherData = 'TP2M', iTime, fTime)
    OCIS = getWeatherData(lon, lat, weatherData = 'OCIS', iTime, fTime)
    PREC = getWeatherData(lon, lat, weatherData = 'PREC', iTime, fTime)
    UR2M = getWeatherData(lon, lat, weatherData = 'UR2M', iTime, fTime)
    V10M = getWeatherData(lon, lat, weatherData = 'V10M', iTime, fTime)

    v_all <- as.data.frame(rbind(TP2M = TP2M$Dados, OCIS = OCIS$Dados,
                                 PREC = PREC$Dados, UR2M = UR2M$Dados,
                                 V10M = V10M$Dados))

    model_output <- list(Model = "Eta 15km",
                         Frequency = "Hourly",
                         Longitude = TP2M$Longitude,
                         Latitude = TP2M$Latitude,
                         Variable_name = var_eta15km$variable,
                         Variable_description = var_eta15km$description,
                         Dados = v_all)
  }
  else{
    d <- loadRda(weatherData)
    verifyLonLat(d, lon, lat)
    lonlat = ajuste_ponto(d[1:2],lon,lat)
    if(is.null(iTime) & is.null(fTime)){
      temp = subset(d, LONGITUDE == lonlat[1] & LATITUDE == lonlat[2])
      temp = temp[,-c(1:2)]
    }
    else
      if(is.null(fTime))
        temp = subset(d, LONGITUDE == lonlat[1] & LATITUDE == lonlat[2], select=iTime)
    else
      temp = subset(d, LONGITUDE == lonlat[1] & LATITUDE == lonlat[2],
                    select = match(iTime, names(d)):match(fTime, names(d)))
    row.names(temp) <- weatherData
    model_output <- list(Model = "Eta 15km",
                         Frequency = "Hourly",
                         Longitude = lonlat[1],
                         Latitude = lonlat[2],
                         Variable_name = row.names(temp),
                         Variable_description =
                           var_eta15km$description[which(var_eta15km$variable == row.names(temp))],
                         Dados = temp)
  }

  model_output
}

info.RCPTEC.weather <- function(){
  cat(paste("CPTEC/INPE",
            "Eta Model 15km Brazil\n",
            "AREA COVERED IN THE MODEL:",
            "LONGITUDE:   -75.05 to -33.95",
            "LATITUDE :   -35.05 to   5.90",
            "\nAvailable variables:\n",sep ="\n"))
      print(var_eta15km, row.names=FALSE, right=FALSE)
      cat(paste("\nFORECAST PERIOD (11 days)",
                "YYYYMMDDHH to YYYYMMDDHH (Initial YearMonthDayHour to Final YearMonthDayHour)",  sep='\n'))
}

