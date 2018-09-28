#### Funtions for climate models ###

#' Access the climate change data.
#'
#' \code{getClimateData} access the climate change data from CPTEC/INPE.
#'
#' @param modelID numeric (model ID).
#' @param modelFrequency string (data frequency).
#' @param modelVar string (model variable short name).
#' @param lat numeric (latitude coordenate).
#' @param lon numeric (longitude coordenate).
#' @param iYear numeric (initial year).
#' @param fYear numeric (final year).
#'
#' @return list (list with climate change data)
#' @examples getClimateData('1', 'YEARLY', 'TP2M', '-28.35', '-52.34', iYear = 2006, fYear = 2010)
#' @export
getClimateData<- function(modelID, modelFrequency, modelVar, lat, lon, iYear, fYear) {

  modelOption <- switch(modelFrequency,
                        HOURLY = '/1',
                        DAILY = '/2',
                        MONTHLY = '/3',
                        YEARLY = '/4')

  api <- paste("https://projeta.cptec.inpe.br/api/v1/public/ETA/", modelID, "/", modelFrequency, modelOption, "/1/", iYear, "/12/", fYear, "/", modelVar, "/", lat, '/',lon,'/', sep="")

  model_data <- jsonlite::fromJSON(api)

  model_output <- switch(modelFrequency,
                         HOURLY = list(Model = "Eta", Couple = models$couple[as.numeric(modelID)],
                                       Scenario = models$scenario[as.numeric(modelID)],
                                       Resolution = models$resolution[as.numeric(modelID)],
                                       Frequency = modelFrequency,
                                       Variable_name = modelVar,
                                       Variable_description = variables$description[which(variables$variable==modelVar)],
                                       Latitude = lat, Longitude = lon,
                                       Data = as.data.frame(cbind(Date = model_data$date,
                                                                  Hour = substr(model_data$time, 4, 8),
                                                                  Value = model_data$value))),
                         DAILY = list(Model = "Eta", Couple = models$couple[as.numeric(modelID)],
                                      Scenario = models$scenario[as.numeric(modelID)],
                                      Resolution = models$resolution[as.numeric(modelID)],
                                      Frequency = modelFrequency,
                                      Variable_name = modelVar,
                                      Variable_description = variables$description[which(variables$variable==modelVar)],
                                      Latitude = lat, Longitude = lon,
                                      Data = as.data.frame(cbind(Date = model_data$date,
                                                                 Value = model_data$value))),
                         MONTHLY = list(Model = "Eta", Couple = models$couple[as.numeric(modelID)],
                                        Scenario = models$scenario[as.numeric(modelID)],
                                        Resolution = models$resolution[as.numeric(modelID)],
                                        Frequency = modelFrequency,
                                        Variable_name = modelVar,
                                        Variable_description = variables$description[which(variables$variable==modelVar)],
                                        Latitude = lat, Longitude = lon,
                                        Data = as.data.frame(cbind(Year = substr(model_data$date, 1, 4),
                                                                   Month = substr(model_data$date, 6, 7),
                                                                   Value = model_data$value))),
                         YEARLY = list(Model = "Eta", Couple = models$couple[as.numeric(modelID)],
                                       Scenario = models$scenario[as.numeric(modelID)],
                                       Resolution = models$resolution[as.numeric(modelID)],
                                       Frequency = modelFrequency,
                                       Variable_name = modelVar,
                                       Variable_description = variables$description[which(variables$variable==modelVar)],
                                       Latitude = lat, Longitude = lon,
                                       Data = as.data.frame(cbind(Year = substr(model_data$date, 1, 4),
                                                                  Value = model_data$value))))
  model_output
}

#' Access the Brazil climate change data.
#'
#' \code{getClimateDataBR} access the Brazil climate change data from CPTEC/INPE.
#'
#' @param modelID numeric (model ID).
#' @param modelFrequency string (data frequency).
#' @param modelVar string (model variable short name).
#' @param iYear numeric (initial year).
#' @param fYear numeric (final year).
#'
#' @return list (list with climate change data)
#' @examples getClimateDataBR('1', 'YEARLY', 'TP2M', iYear = 2006, fYear = 2010)
#' @export
getClimateDataBR<- function(modelID, modelFrequency, modelVar, iYear, fYear) {

  modelOption <- switch(modelFrequency,
                        HOURLY = '/1',
                        DAILY = '/2',
                        MONTHLY = '/3',
                        YEARLY = '/4')

  api <- paste("https://projeta.cptec.inpe.br/api/v1/public/ETA/", modelID, "/", modelFrequency, modelOption, "/1/", iYear, "/12/", fYear, "/", modelVar,'/', sep="")

  model_data <- jsonlite::fromJSON(api)

  model_output <- switch(modelFrequency,
                         HOURLY = list(Model = "Eta", Couple = models$couple[as.numeric(modelID)],
                                       Scenario = models$scenario[as.numeric(modelID)],
                                       Resolution = models$resolution[as.numeric(modelID)],
                                       Frequency = modelFrequency,
                                       Variable_name = modelVar,
                                       Variable_description = variables$description[which(variables$variable==modelVar)],
                                       Data = as.data.frame(cbind(Latitude = model_data$latitude,
                                                                  Longitude = model_data$longitude,
                                                                  Date = model_data$date,
                                                                  Hour = substr(model_data$time, 4, 8),
                                                                  Value = model_data$val))),
                         DAILY = list(Model = "Eta", Couple = models$couple[as.numeric(modelID)],
                                      Scenario = models$scenario[as.numeric(modelID)],
                                      Resolution = models$resolution[as.numeric(modelID)],
                                      Frequency = modelFrequency,
                                      Variable_name = modelVar,
                                      Variable_description = variables$description[which(variables$variable==modelVar)],
                                      Data = as.data.frame(cbind(Latitude = model_data$latitude,
                                                                 Longitude = model_data$longitude,
                                                                 Date = model_data$date,
                                                                 Value = model_data$val))),
                         MONTHLY = list(Model = "Eta", Couple = models$couple[as.numeric(modelID)],
                                        Scenario = models$scenario[as.numeric(modelID)],
                                        Resolution = models$resolution[as.numeric(modelID)],
                                        Frequency = modelFrequency,
                                        Variable_name = modelVar,
                                        Variable_description = variables$description[which(variables$variable==modelVar)],
                                        Data = as.data.frame(cbind(Latitude = model_data$latitude,
                                                                   Longitude = model_data$longitude,
                                                                   Year = substr(model_data$date, 1, 4),
                                                                   Month = substr(model_data$date, 6, 7),
                                                                   Value = model_data$val))),
                         YEARLY = list(Model = "Eta", Couple = models$couple[as.numeric(modelID)],
                                       Scenario = models$scenario[as.numeric(modelID)],
                                       Resolution = models$resolution[as.numeric(modelID)],
                                       Frequency = modelFrequency,
                                       Variable_name = modelVar,
                                       Variable_description = variables$description[which(variables$variable==modelVar)],
                                       Data = as.data.frame(cbind(Latitude = model_data$latitude,
                                                                  Longitude = model_data$longitude,
                                                                  Year = substr(model_data$date, 1, 4),
                                                                  Value = model_data$val))))
  model_output
}

#' Information about the climate change data.
#'
#' \code{getInfoClimate} returns information about the climate change data accessed from CPTEC/INPE.
#'
#' @return Model driven, frequencies and variables.
#' @examples
#' getInfoClimate()
#' @export
getInfoClimate <- function(){
  cat("Avaiable Models: o modelo (modelID) deve ser acessado pelo valor",
      "do campo <id>. Verifique o respectivo periodo de abrangencia do modelo:",
      "data inicial (iMonth, iYear) e data final (fMonth, fYear)", "", sep = '\n')
  print(models, row.names=FALSE, right=FALSE)

  cat (paste("", "modelFrequency - Avaiable frequencies:",
             "HOURLY : horaria de 3 em 3 horas ",
             "DAILY  : diaria",
             "MONTHLY: mensal",
             "YEARLY : anual", "\n", sep='\n'))

  cat("Avaiable Variables - a variavel (modelVar) deve ser acessada",
      "pelo seu short name <variable>", sep = '\n')
  print(variables[,c("variable","description")], row.names=FALSE, right=FALSE)
}

