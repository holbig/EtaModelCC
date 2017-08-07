#setwd("")
getTemperatura <- function(lon, lat = "") {
  #carregando as informações
  #load("~/RCPTEC/R/data/temperatura.Rda") # temperatura

  arq = url("https://github.com/marinadezordi/RCPTEC/raw/master/R/data/temperatura.Rda")
  load(arq)
  #dados
  temp = subset(temperatura, LONGITUDE == lon & LATITUDE == lat)
  print(temp)
}
