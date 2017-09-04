#setwd("")

getWeatherData <- function(lon, lat, iTime, weatherData) {
  if(weatherData == 'TP2M') {
    getTemperature(lon, lat, iTime)
  }else  if(weatherData == 'OCIS') {
    getOCIS(lon, lat, iTime)
  }else  if(weatherData == 'PREC') {
    getPrecipitation(lon, lat, iTime)
  }else  if(weatherData == 'UR2M') {
    getHumidiity(lon, lat, iTime)
  }else  if(weatherData == 'V10M') {
    getWind(lon, lat, iTime)
  }
}

getWeatherData(lon = -50.88, lat = -28.51, iTime = '2017080700', weatherData = 'TP2M')
#getWeatherData(-50.88,-28.51, 'OCIS')
#getWeatherData(-50.88,-28.51, 'PREC')
#getWeatherData(-50.88,-28.51, 'UR2M')
#getWeatherData(-50.88,-28.51, 'V10M')

getWeatherHour<- function(lon, lat, iTime, fTime){
  temp = getTemperature(lon, lat, iTime)
  oc = getOCIS(lon, lat, iTime)
  precip = getPrecipitation(lon, lat, iTime)
  humi= getHumidiity(lon, lat, iTime)
  wind = getWind(lon, lat, iTime)

 # df <- data.frame(Temperature = temp, Radiation = oc, Precipitation = precip, Humidity = humi, Wind = wind)

  w = rbind(Temperature = temp, Radiation = oc, Precipitation = precip, Humidity = humi, Wind = wind)
  return(w)
}
w = getWeatherHour(lon = -50.88,lat = -28.51, iTime = '2017080700')
print(w)

getWeatherHour<- function(lon, lat, iTime, fTime){
  temp = getTemperature(lon=lon, lat=lat, iTime=iTime)
  oc = getOCIS(lon=lon, lat=lat, iTime=iTime)
  precip = getPrecipitation(lon=lon, lat=lat, iTime=iTime)
  humi= getHumidiity(lon=lon, lat=lat, iTime=iTime)
  wind = getWind(lon=lon, lat=lat, iTime=iTime)

  # df <- data.frame(Temperature = temp, Radiation = oc, Precipitation = precip, Humidity = humi, Wind = wind)

  w = rbind(Temperature = temp, Radiation = oc, Precipitation = precip, Humidity = humi, Wind = wind)
  return(w)
}
#w = getWeatherHour(lon = -50.88,lat = -28.51, iTime = '2017080700')
#print(w)

getTemperature <- function(lon, lat, iTime, fTime) {
  #arq = url("https://github.com/marinadezordi/RCPTEC/raw/master/R/data/TP2M.Rdata")
  #load(arq)

  load("~/RCPTEC/R/data/TP2M.Rdata")
  verifyLonLat(TP2M, lon, lat)
  lonlat = ajuste_ponto(TP2M[1:2],lon,lat)
  if(fTime = NULL)
    temp = subset(TP2M, LONGITUDE == lonlat[1:1] & LATITUDE == lonlat[2:2], select=iTime)
  else
    temp = subset(TP2M, LONGITUDE == lonlat[1:1] & LATITUDE == lonlat[2:2], select=iTime)

  return(temp)
}

getPeriodo <- function(iTime, fTime){
  #t = getTemperature(lon = -50.88,lat = -28.51, iTime = '2017080700')
  #print(t)
  iTime = 2017080702
  tempo = paste(substr(iTime, 1, 4), substr(iTime, 5, 6), substr(iTime, 7, 8), sep="-")
  hour = paste(substr(iTime, 9, 10), "00", "00", sep = ":")
  t = as.POSIXlt(paste(tempo, hour, sep = " ") )

  fTime = 2017080708
  tempo = paste(substr(fTime, 1, 4), substr(fTime, 5, 6), substr(fTime, 7, 8), sep="-")
  hour = paste(substr(fTime, 9, 10), "00", "00", sep = ":")
  t2 = as.POSIXlt(paste(tempo, hour, sep = " ") )

  x = t
  while(x < t2)
  {
    x = x + 1*60*60

  }
}



getOCIS <- function(lon, lat, iTime, fTime) {
  #arq = url("https://github.com/marinadezordi/RCPTEC/raw/master/R/data/OCIS.Rdata")
  #load(arq)
  load("~/RCPTEC/R/data/OCIS.Rdata")
  verifyLonLat(OCIS, lon, lat)
  lonlat = ajuste_ponto(OCIS[1:2],lon,lat)
  oc = subset(OCIS, LONGITUDE == lonlat[1:1] & LATITUDE == lonlat[2:2], select=iTime)
  return(oc)
}
#oc = getOCIS(lon = -50.88,lat = -28.51, iTime = '2017080700')
#print(oc)

getPrecipitation <- function(lon, lat, iTime, fTime) {
  #arq = url("https://github.com/marinadezordi/RCPTEC/raw/master/R/data/PREC.Rdata")
  #load(arq)
  load("~/RCPTEC/R/data/PREC.Rdata")
  verifyLonLat(PREC, lon, lat)
  lonlat = ajuste_ponto(PREC[1:2],lon,lat)
  precip = subset(PREC, LONGITUDE == lonlat[1:1] & LATITUDE == lonlat[2:2], select=iTime)
  return(precip)
}
#precip = getPrecipitation(-50.88,-28.51, '2017080700')
#print(precip)

getHumidiity <- function(lon, lat, iTime, fTime){
  #arq = url("https://github.com/marinadezordi/RCPTEC/raw/master/R/data/UR2M.Rdata")
  #load(arq)
  load("~/RCPTEC/R/data/UR2M.Rdata")
  verifyLonLat(UR2M, lon, lat)
  lonlat = ajuste_ponto(UR2M[1:2],lon,lat)
  humi = subset(UR2M, LONGITUDE == lonlat[1:1] & LATITUDE == lonlat[2:2], select=iTime)
  return(humi)
}
#humi = getHumidiity(-50.88,-28.51, '2017080700')
#print(humi)

getWind <- function(lon, lat, iTime, fTime){
  #arq = url("https://github.com/marinadezordi/RCPTEC/raw/master/R/data/V10M.Rdata")
  #load(arq)
  load("~/RCPTEC/R/data/V10M.Rdata")
  verifyLonLat(V10M, lon, lat)
  lonlat = ajuste_ponto(V10M[1:2],lon,lat)
  wind = subset(V10M, LONGITUDE == lonlat[1:1] & LATITUDE == lonlat[2:2], select=iTime)
  return(wind)
}
#wind = getWind(-50.88,-28.51, '2017080700')
#print(wind)

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
