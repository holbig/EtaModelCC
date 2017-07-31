#setwd("")
getTemperatura <- function() {
  #carregando as informações
  #load("~/RCPTEC/R/data/temperatura.Rda") # temperatura

  arq = url("https://github.com/marinadezordi/RCPTEC/raw/master/R/data/temperatura.Rda")
  load(arq)
  #dados
  temp <- temperatura[50:70, 17]
  print(temp)
}
