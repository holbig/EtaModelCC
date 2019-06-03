# #' @importFrom "jsonlite" "fromJSON"
# #' @importFrom "ggplot2" "aes" "ggplot"
# #' @importFrom "raster" "rasterFromXYZ"
# #' @importFrom "RCurl" "url.exists"
#' @importFrom "magrittr" "%>%"
# #' @importFrom "methods" "as"
# #' @import "leaflet"
# #' @import "leafem"
# #' @import "rgdal"
# #' @import "sp"

# PROJETA API URL
projeta_api <- "https://projeta.cptec.inpe.br/api/v1/public/ETA/"

# ERRO API URL
erro_api <- "Ocorreu um erro ao tentar se conectar com a API."

# Data-frame com os dados dos modelos de mudanças climaticas
url <- "https://projeta.cptec.inpe.br/api/v1/public/models"
models <- jsonlite::fromJSON(url)
models <- as.data.frame(models$data)

# Data-frame com variaveis geradas pelos modelos de mudancas climaticas
url <- "https://projeta.cptec.inpe.br/api/v1/public/variables"
variables <- jsonlite::fromJSON(url)
variables <- as.data.frame(variables$data[,c(1,3,2,4)])
names(variables)[2] <- c("variable")

#LON LAT
latMin <- -35.05 #-35.1
latMax <- 5.90 #7.1
lonMin <- -75.05 #-76.7
lonMax <- -33.95 #-32.01

checkCoordinates <- function(lat, lon){

  lat <- as.numeric(lat)
  lon <- as.numeric(lon)

  if(lat < latMin || lat > latMax){
    if(lon < lonMin || lon > lonMax){
      stop(paste("Coordenadas de LATITUDE(",lat,") e LONGITUDE(",lon,") fora da faixa de dados da previsao \n",
                 "AREA COVERED IN THE MODEL:\n",
                 "--> LONGITUDE:   ", lonMin ,"to ",lonMax,"\n",
                 "--> LATITUDE :   ",latMin," to   ",latMax,"\n"))
    }
    stop(paste("Coordenada de LATITUDE(",lat,") fora da faixa de dados da previsao \n",
               "AREA COVERED IN THE MODEL:\n",
               "--> LONGITUDE:   ", lonMin ,"to ",lonMax,"\n",
               "--> LATITUDE :   ",latMin," to   ",latMax,"\n"))
  }
  else{
    if(lon < lonMin || lon > lonMax)
      stop(paste("Coordenada de LONGITUDE(",lon,") fora da faixa de dados da previsao \n",
                 "AREA COVERED IN THE MODEL:\n",
                 "--> LONGITUDE:   ", lonMin ,"to ",lonMax,"\n",
                 "--> LATITUDE :   ",latMin," to   ",latMax,"\n"))
  }
}


