#devtools::use_package("jsonlite") # Defaults to imports

#' @importFrom "ggplot2" "aes" "ggplot"
#' @importFrom "magrittr" "%>%"

# Data-frame com os dados dos modelos de mudanças climaticas
url <- "https://projeta.cptec.inpe.br/api/v1/public/models"
models <- jsonlite::fromJSON(url)
models <- as.data.frame(models$data)

# Data-frame com variaveis geradas pelos modelos de mudancas climaticas
url <- "https://projeta.cptec.inpe.br/api/v1/public/variables"
variables <- jsonlite::fromJSON(url)
variables <- as.data.frame(variables$data[,c(1,3,2,4)])
names(variables)[2] <- c("variable")

verifyLonLat <- function(dado, lon, lat){
  limite_lon = range(dado$LONGITUDE)
  limite_lat = range(dado$LATITUDE)

  if(lon < limite_lon[1] || lon > limite_lon[2])
    stop("### Coordenada da longitude fora da faixa de dados da previsao ###")
  if(lat < limite_lat[1] || lat > limite_lat[2])
    stop("### Coordenada da latitude fora da faixa de dados da previsao ###")
}


ajuste_ponto <- function(dados,longitude,latitude){

  ponto_lon = dados$LONGITUDE[which.min(dados$LONGITUDE < longitude)-1]
  ponto_lat = dados$LATITUDE[which.min(dados$LATITUDE < latitude)]

  return(c(ponto_lon,ponto_lat))
}
