# Verifica a instalação dos pacotes necessários para o programa
list.of.packages <- c("jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

suppressMessages(library(jsonlite))

source("R/global.R")


#### Funtions for climate models ###

getClimateData<- function(modelID, modelFrequency, modelVar, lon, lat, iMonth = 1, iYear, fMonth = 1, fYear) {

  modelOption <- switch(modelFrequency,
                        HOURLY = '/1',
                        DAILY = '/2',
                        MONTHLY = '/3',
                        YEARLY = '/4')

  api <- paste("https://projeta.cptec.inpe.br/api/v1/public/ETA/", modelID, "/", modelFrequency, modelOption, "/", iMonth, "/", iYear, "/", fMonth, "/", fYear, "/", modelVar, "/", lon, '/',lat,'/', sep="")

  model_data <- fromJSON(api)

  model_output <- switch(modelFrequency,
                         HOURLY = list(Model = "Eta", Couple = models$couple[as.numeric(modelID)],
                                       Scenario = models$scenario[as.numeric(modelID)],
                                       Resolution = models$resolution[as.numeric(modelID)],
                                       Frequency = modelFrequency,
                                       Variable_name = modelVar,
                                       Variable_description = variables$description[which(variables$variable==modelVar)],
                                       Longitude = lon, Latitude = lat,
                                       Data = as.data.frame(cbind(Date = model_data$date,
                                                                  Hour = substr(model_data$time, 4, 8),
                                                                  Value = model_data$value))),
                         DAILY = list(Model = "Eta", Couple = models$couple[as.numeric(modelID)],
                                      Scenario = models$scenario[as.numeric(modelID)],
                                      Resolution = models$resolution[as.numeric(modelID)],
                                      Frequency = modelFrequency,
                                      Variable_name = modelVar,
                                      Variable_description = variables$description[which(variables$variable==modelVar)],
                                      Longitude = lon, Latitude = lat,
                                      Data = as.data.frame(cbind(Date = model_data$date,
                                                                 Value = model_data$value))),
                         MONTHLY = list(Model = "Eta", Couple = models$couple[as.numeric(modelID)],
                                        Scenario = models$scenario[as.numeric(modelID)],
                                        Resolution = models$resolution[as.numeric(modelID)],
                                        Frequency = modelFrequency,
                                        Variable_name = modelVar,
                                        Variable_description = variables$description[which(variables$variable==modelVar)],
                                        Longitude = lon, Latitude = lat,
                                        Data = as.data.frame(cbind(Year = substr(model_data$date, 1, 4),
                                                                   Month = substr(model_data$date, 6, 7),
                                                                   Value = model_data$value))),
                         YEARLY = list(Model = "Eta", Couple = models$couple[as.numeric(modelID)],
                                       Scenario = models$scenario[as.numeric(modelID)],
                                       Resolution = models$resolution[as.numeric(modelID)],
                                       Frequency = modelFrequency,
                                       Variable_name = modelVar,
                                       Variable_description = variables$description[which(variables$variable==modelVar)],
                                       Longitude = lon, Latitude = lat,
                                       Data = as.data.frame(cbind(Year = substr(model_data$date, 1, 4),
                                                                  Value = model_data$value))))
  model_output
}

info.RCPTEC.climate <- function(){
  cat("Avaiable Models: o modelo (modelID) deve ser acessado pelo valor",
      "do campo <id>. Verifique o respectivo período de abrangência do modelo:",
      "data inicial (iMonth, iYear) e data final (fMonth, fYear)", "", sep = '\n')
  print(models, row.names=FALSE, right=FALSE)

  cat (paste("", "modelFrequency - Avaiable frequencies:",
             "HOURLY : horária de 3 em 3 horas ",
             "DAILY  : diária",
             "MONTHLY: mensal",
             "YEARLY : anual", "\n", sep='\n'))

  cat("Avaiable Variables - a variável (modelVar) deve ser acessada",
      "pelo seu short name <variable>", sep = '\n')
  print(variables[,c("variable","description")], row.names=FALSE, right=FALSE)
}

