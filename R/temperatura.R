#setwd("")

getWeatherData <- function(lon, lat, weatherData) {
  if(weatherData == 'TP') {
    getTemperatura(lon, lat)
  }else  if(weatherData == 'OCIS') {
    getOCIS(lon, lat)
  }else  if(weatherData == 'PREC') {
    getPrecipitation(lon, lat)
  }else  if(weatherData == 'UR') {
    getHumidiity(lon, lat)
  }else  if(weatherData == 'V10M') {
    getWind(lon, lat)
  }

}

getTemperature <- function(lon, lat) {
  #carregando as informações
  #load("~/RCPTEC/R/data/temperatura.Rda") # temperatura

  arq = url("https://github.com/marinadezordi/RCPTEC/raw/master/R/data/TP2M.Rda")
  load(arq)
  #dados
  temp = subset(temperatura, LONGITUDE == lon & LATITUDE == lat)
  print(temp)
}

getOCIS <- function(lon, lat) {

  arq = url("https://github.com/marinadezordi/RCPTEC/raw/master/R/data/OCIS.Rdata")
  load(arq)
  #dados
  oc = subset(OCIS, LONGITUDE == lon & LATITUDE == lat)
  print(oc)
}

getPrecipitation <- function(lon, lat) {
  arq = url("https://github.com/marinadezordi/RCPTEC/raw/master/R/data/PREC.Rdata")
  load(arq)
  #dados
  pr = subset(PREC, LONGITUDE == lon & LATITUDE == lat)
  print(pr)
}

getHumidiity <- function(lon, lat){
  arq = url("https://github.com/marinadezordi/RCPTEC/raw/master/R/data/UR2M.Rdata")
  load(arq)
  #dados
  humi = subset(UR2M, LONGITUDE == lon & LATITUDE == lat)
  print(humi)
}

getWind <- function(lon, lat){
  arq = url("https://github.com/marinadezordi/RCPTEC/raw/master/R/data/V10M.Rdata")
  load(arq)
  #dados
  v = subset(V10M, LONGITUDE == lon & LATITUDE == lat)
  print(v)
}

