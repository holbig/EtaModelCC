# Verifica a instalação dos pacotes necessários para o programa
list.of.packages <- c("jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

suppressMessages(library(jsonlite))

source(paste(getwd(),"/R/global.R", sep=""))


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
            "Eta Model 15km Brazil\n",
            "AREA COVERED IN THE MODEL:",
            "LONGITUDE:   -75.05 to -33.95",
            "LATITUDE :   -35.05 to   5.90",
            "\nAvailable variables:\n",sep ="\n"))
      print(var_eta15km, row.names=FALSE, right=FALSE)
      cat(paste("\nFORECAST PERIOD (11 days)",
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

