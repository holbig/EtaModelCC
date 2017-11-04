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
