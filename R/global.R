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
