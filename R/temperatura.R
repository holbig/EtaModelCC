#setwd("")
getTemperatura <- function() {
  #carregando as informações
  load("~/RCPTEC/R/temperatura.Rda") # temperatura
  #dados
  temp <- temperatura[50:70, 17]
  print(temp)
}
