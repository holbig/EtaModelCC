#' @import "raster"
#' @importFrom "magrittr" "%>%"

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

load(paste0(system.file("extdata", package = "EtaModelCC"),"/units.Rda"))
variables <- merge(variables,units[,c("variable","unit")], by="variable", all.x = TRUE)

#LON LAT
latMin <- -35.1  # -35.05
latMax <-   5.9  #   5.90
lonMin <- -76.7  # -75.05
lonMax <- -32.01 # -33.95

checkCoordinates <- function(lat, lon){

  lat <- as.numeric(lat)
  lon <- as.numeric(lon)

  if((lat < latMin || lat > latMax) && (lon < lonMin || lon > lonMax)){
    #if(lon < lonMin || lon > lonMax){
      stop(paste("Coordenadas de LATITUDE(",lat,") e LONGITUDE(",lon,") fora da faixa de dados da previsao \n",
                 "AREA COVERED IN THE MODEL:\n",
                 "--> LONGITUDE:   ", lonMin ,"to ",lonMax,"\n",
                 "--> LATITUDE :   ",latMin," to   ",latMax,"\n"))
    }
#else{
      if(lat < latMin || lat > latMax){
        stop(paste("Coordenada de LATITUDE(",lat,") fora da faixa de dados da previsao \n",
                   "AREA COVERED IN THE MODEL:\n",
                   "--> LONGITUDE:   ", lonMin ,"to ",lonMax,"\n",
                   "--> LATITUDE :   ",latMin," to   ",latMax,"\n"))
      }
    #}
    #else{
      if(lon < lonMin || lon > lonMax){
        stop(paste("Coordenada de LONGITUDE(",lon,") fora da faixa de dados da previsao \n",
                   "AREA COVERED IN THE MODEL:\n",
                   "--> LONGITUDE:   ", lonMin ,"to ",lonMax,"\n",
                   "--> LATITUDE :   ",latMin," to   ",latMax,"\n"))
      }
    #}
  #}
}


