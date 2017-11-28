# Verifica a instalação dos pacotes necessários para o programa
list.of.packages <- c("jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

suppressMessages(library(jsonlite))

# Definição de variáveis globais para o pacote RCPTEC

# Data-frame com os dados dos modelos de mudanças climáticas
url <- "https://projeta.cptec.inpe.br/api/v1/public/models"
models <- fromJSON(url)
models <- as.data.frame(models$data)

# Data-frame com variáveis geradas pelos modelos de mudanças climáticas
url <- "https://projeta.cptec.inpe.br/api/v1/public/variables"
variables <- fromJSON(url)
variables <- as.data.frame(variables$data[,c(1,3,2,4)])
names(variables)[2] <- c("variable")

# Data-frame com variáveis geradas pelo modelo Eta 15km
var_eta15km <- data.frame(variable = c("V10M","TP2M","PREC","UR2M","OCIS"),
                          description = c("Wind up 10m (m/s)","Temperature up 2m","Precipitation",
                                          "Relative humidity up 2m","Solar radiation"),
                          unit.measure = c("m/s","degrees Celsius","mm","%","W/m^2"),
                          stringsAsFactors = FALSE)

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
  load(paste("data/", fileName, ".Rda", sep = ""))
  get(ls()[ls() != "fileName"])
}
