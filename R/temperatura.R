#setwd("")

getWeatherData <- function(lon, lat, weatherData) {
  if(weatherData == 'TP2M') {
    getTemperature(lon, lat)
  }else  if(weatherData == 'OCIS') {
    getOCIS(lon, lat)
  }else  if(weatherData == 'PREC') {
    getPrecipitation(lon, lat)
  }else  if(weatherData == 'UR2M') {
    getHumidiity(lon, lat)
  }else  if(weatherData == 'V10M') {
    getWind(lon, lat)
  }
}

#getWeatherData(-50.88,-28.51, 'TP2M')
#getWeatherData(-50.88,-28.51, 'OCIS')
#getWeatherData(-50.88,-28.51, 'PREC')
#getWeatherData(-50.88,-28.51, 'UR2M')
#getWeatherData(-50.88,-28.51, 'V10M')


getTemperature <- function(lon, lat) {
  #arq = url("https://github.com/marinadezordi/RCPTEC/raw/master/R/data/TP2M.Rdata")
  #load(arq)

  load("~/RCPTEC/R/data/TP2M.Rdata")

  verifyLonLat(TP2M, lon, lat)

  lonlat = ajuste_ponto(TP2M[1:2],lon,lat)

  temp = subset(TP2M, LONGITUDE == lonlat[1:1] & LATITUDE == lonlat[2:2])

  print(temp)
}

#getTemperature(-50.88,-28.51)

getOCIS <- function(lon, lat) {

  #arq = url("https://github.com/marinadezordi/RCPTEC/raw/master/R/data/OCIS.Rdata")
  #load(arq)
  load("~/RCPTEC/R/data/OCIS.Rdata")

  verifyLonLat(OCIS, lon, lat)

  lonlat = ajuste_ponto(OCIS[1:2],lon,lat)

  oc = subset(OCIS, LONGITUDE == lonlat[1:1] & LATITUDE == lonlat[2:2])

  print(oc)
}

#getOCIS(-50.88,-28.51)

getPrecipitation <- function(lon, lat) {
  #arq = url("https://github.com/marinadezordi/RCPTEC/raw/master/R/data/PREC.Rdata")
  #load(arq)

  load("~/RCPTEC/R/data/PREC.Rdata")

  verifyLonLat(PREC, lon, lat)

  lonlat = ajuste_ponto(PREC[1:2],lon,lat)

  precip = subset(PREC, LONGITUDE == lonlat[1:1] & LATITUDE == lonlat[2:2])

  print(precip)
}

#getPrecipitation(-50.88,-28.51)

getHumidiity <- function(lon, lat){
  #arq = url("https://github.com/marinadezordi/RCPTEC/raw/master/R/data/UR2M.Rdata")
  #load(arq)

  load("~/RCPTEC/R/data/UR2M.Rdata")

  verifyLonLat(UR2M, lon, lat)

  lonlat = ajuste_ponto(UR2M[1:2],lon,lat)

  humi = subset(UR2M, LONGITUDE == lonlat[1:1] & LATITUDE == lonlat[2:2])

  print(humi)
}

#getHumidiity(-50.88,-28.51)

getWind <- function(lon, lat){
  #arq = url("https://github.com/marinadezordi/RCPTEC/raw/master/R/data/V10M.Rdata")
  #load(arq)

  load("~/RCPTEC/R/data/V10M.Rdata")

  verifyLonLat(V10M, lon, lat)

  lonlat = ajuste_ponto(V10M[1:2],lon,lat)

  wind = subset(V10M, LONGITUDE == lonlat[1:1] & LATITUDE == lonlat[2:2])

  print(wind)
}

#getWind(-50.88,-28.51)

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
