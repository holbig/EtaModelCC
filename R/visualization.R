#### Funtions for climate models ###

#' Create a graph with climate change data.
#'
#' \code{plotGrafData} create a graph with climate change data from CPTEC/INPE.
#'
#' @param modelID numeric (model ID).
#' @param modelFrequency string (data frequency).
#' @param modelVar string (model variable short name).
#' @param lat numeric (latitude coordenate).
#' @param lon numeric (longitude coordenate).
#' @param iYear numeric (initial year).
#' @param fYear numeric (final year).
#'
#' @return graph (graph with climate change data)
#' @examples
#' \dontrun{
#'  plotGrafData('1', 'MONTHLY', 'TP2M', '-28.35', '-52.34', iYear = 2006, fYear = 2010)
#' }
#' @export
plotGrafData<- function(modelID, modelFrequency, modelVar, lat, lon, iYear, fYear) {
  climate <- getClimateData(modelID, modelFrequency, modelVar, lat, lon, iYear, fYear )

  valores = as.numeric(levels(climate$Data$Value)[climate$Data$Value])

  data_graf <- switch(modelFrequency,
                      YEARLY = levels(climate$Data$Year),
                      MONTHLY = as.Date(paste(climate$Data$Year,"-",
                                              levels(climate$Data$Month),
                                              "-01",sep = "")),
                      DAILY = climate$Data$Date,
                      HOURLY = as.Date(strptime(paste(climate$Data$Date," ",
                                                      climate$Data$Hour,sep = ""),
                                                "%Y-%m-%d %H:%M")) )

  ggplot2::ggplot()+
    ggplot2::geom_line(ggplot2::aes(data_graf, valores, group = 1), color = "red") +
    ggplot2::geom_point(ggplot2::aes(data_graf, valores), color="blue") +
    ggplot2::labs(title = paste("Forecast for longitute: ",lon," / latitude: ",lat, sep=""),
                  subtitle = paste("Forecast by ", models$model[which(models$id == modelID)]," Model ",
                                   models$resolution[which(models$id == modelID)],"km -- ",
                                   variables$name[which(variables$variable == modelVar)]),
                  caption = "Source: CPTEC/INPE, Brazil",
                  x = "Date", y = variables$description[which(variables$variable == modelVar)])


}

#' Create a map with Brazil climate change data.
#'
#' \code{plotMapBR} create a map with Brazil climate change data from CPTEC/INPE.
#'
#' @param modelID numeric (model ID).
#' @param modelVar string (model variable short name).
#' @param year numeric (year).
#'
#' @return map (map with climate change data)
#' @examples
#' \dontrun{
#'  plotMapBR('1', 'PREC', 2006)
#'  plotMapBR('1', 'TP2M', 2006)
#' }
#' @export
plotMapBR<- function(modelID, modelVar, year){

  climate <- getClimateDataBR(modelID, 'YEARLY', modelVar, year, year)

  arquivo <- system.file("extdata", package = "EtaModelCC")
  shape_br <- rgdal::readOGR(arquivo, "estados", GDAL1_integer64_policy = TRUE)

  pontos <- data.frame(Longitude = as.numeric(gsub(",", ".", as.character(climate$Data$Longitude))),
                       Latitude = as.numeric(gsub(",", ".", as.character(climate$Data$Latitude))),
                       Value = as.numeric(gsub(",", ".", as.character(climate$Data$Value)))
                       )

  sp::coordinates(pontos) <- ~ Latitude + Longitude ### arrumar aqui PROJETA
  sp::proj4string(pontos) <- sp::proj4string(shape_br)
  new_pontos <- climate$Data[!is.na(sp::over(pontos, as(shape_br, "SpatialPolygons"))),]
  r <- raster::rasterFromXYZ(new_pontos[c(1,2,4)])
  raster::crs(r) <- sp::CRS("+init=epsg:4326")

  paleta = "Blues"
  pal <- leaflet::colorNumeric(palette = paleta, raster::values(r),
                               na.color = "transparent", reverse = FALSE)
  pal1 <- leaflet::colorNumeric(palette = paleta, raster::values(r),
                                na.color = "transparent", reverse = TRUE)

  ## custom label format function
  myLabelFormat = function(..., reverse_order = FALSE){
    if(reverse_order){
      function(type = "numeric", cuts){
        cuts <- sort(cuts, decreasing = T)
      }
    }else{
      labelFormat(...)
    }
  }

  leaflet::leaflet() %>%
    leaflet::addTiles(attribution = 'Data source: <a href="http://cptec.inpe.br">CPTEC/INPE</a>') %>%
    leaflet::addPolygons(data=shape_br, color = "black", weight = 1, fillOpacity = 0) %>%
    leaflet::addRasterImage(r, colors = pal, layerId =  "values",opacity = 0.8) %>%
    leafem::addMouseCoordinates() %>%
    leafem::addImageQuery(r, type="mousemove", layerId = "values", position = "topright", digits = 3, prefix = climate$Variable_name)%>%
    leaflet::addLegend(pal = pal1, values = raster::values(r),
                       title = climate$Variable_description,
                       labFormat = myLabelFormat(reverse_order = T))
}


#' Create a map with the climate change data from the rectangular area between 2 points.
#'
#' \code{plotMapPontos} create a map with the climate change data from the rectangular area between 2 points.
#'
#' @param modelID numeric (model ID).
#' @param modelVar string (model variable short name).
#' @param lat1 numeric (latitude coordenate).
#' @param lon1 numeric (longitude coordenate).
#' @param lat2 numeric (latitude coordenate).
#' @param lon2 numeric (longitude coordenate).
#' @param year numeric (year).
#'
#' @return map (map with climate change data from the rectangular area between 2 points)
#' @examples
#' \dontrun{
#'  plotMapPontos('2', 'PREC','-27.26', '-57.10', '-33.67', '-48.85', year = 2006)
#'  plotMapPontos('2', 'PREC', '-35.05','-23.95','5.9','-75.05', year = 2006)
#' }
#' @export
plotMapPontos<- function(modelID, modelVar, lat1, lon1, lat2, lon2, year) {

  climate <- getClimateDataPontos(modelID, modelVar, lat1, lon1, lat2, lon2, year)

  arquivo <- system.file("extdata", package = "EtaModelCC")
  shape_br <- rgdal::readOGR(arquivo, "estados", GDAL1_integer64_policy = TRUE)

  r <- raster::rasterFromXYZ(climate$Data[c(2,1,4)])
  raster::crs(r) <- sp::CRS("+init=epsg:4326")

  paleta = "Blues"
  pal <- leaflet::colorNumeric(palette = paleta, raster::values(r),
                               na.color = "transparent", reverse = FALSE)
  pal1 <- leaflet::colorNumeric(palette = paleta, raster::values(r),
                                na.color = "transparent", reverse = TRUE)

  ## custom label format function
  myLabelFormat = function(..., reverse_order = FALSE){
    if(reverse_order){
      function(type = "numeric", cuts){
        cuts <- sort(cuts, decreasing = T)
      }
    }else{
      labelFormat(...)
    }
  }

  leaflet::leaflet() %>%
    leaflet::addTiles(attribution = 'Data source: <a href="http://cptec.inpe.br">CPTEC/INPE</a>') %>%
    leaflet::addPolygons(data=shape_br, color = "black", weight = 1, fillOpacity = 0) %>%
    leaflet::addRasterImage(r, colors = pal, layerId =  "values",opacity = 0.8) %>%
    leafem::addMouseCoordinates() %>%
    leafem::addImageQuery(r, type="mousemove", layerId = "values", position = "topright", digits = 3, prefix = climate$Variable_name)%>%
    leaflet::fitBounds(lon1, lat1, lon2, lat2) %>%
    leaflet::addLegend(pal = pal1, values = raster::values(r),
                       title = climate$Variable_description,
                       labFormat = myLabelFormat(reverse_order = T))

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
  cat("Available Models: o modelo (modelID) deve ser acessado pelo valor",
      "do campo <id>. Verifique o respectivo periodo de abrangencia do modelo:",
      "data inicial (iMonth, iYear) e data final (fMonth, fYear)", "", sep = '\n')
  print(models, row.names=FALSE, right=FALSE)

  cat (paste("", "modelFrequency - Available frequencies:",
             "HOURLY : horaria de 3 em 3 horas ",
             "DAILY  : diaria",
             "MONTHLY: mensal",
             "YEARLY : anual", "\n", sep='\n'))

  cat("Available Variables - a variavel (modelVar) deve ser acessada",
      "pelo seu short name <variable>", sep = '\n')
  print(variables[,c("variable","description")], row.names=FALSE, right=FALSE)
}

#####################################################################################################
#' Create a map with the climate data from the shapefile area.
#'
#' \code{plotMapShape} create a map with the climate data from the shapefile area.
#'
#' @param modelID numeric (model ID).
#' @param modelVar string (model variable short name).
#' @param year numeric (year).
#' @param folderPath xxx
#' @param fileName xxx
#' @param subName xx
#' @param subNameValue xxx
#'
#' @return map (map with climate change data from the rectangular area between 2 points)
#' @examples
#' \dontrun{
#'   plotMapShape('2', 'PREC', 2006, "C:/path/to/your/folder", "FileNameWithoutExtension")
#'   plotMapShape('1', 'PREC', 2006, "C:/Users/Lemon/Desktop/Shapefile", "teste", "sigla", "RS")
#'   plotMapShape('1', 'PREC', 2006, "C:/Users/Lemon/Desktop/Shapefile", "teste", "regiao_id", "3")
#' }
#' @export
plotMapShape<- function(modelID, modelVar, year, folderPath, fileName, subName= NULL, subNameValue = NULL){

  climate <- getClimateDataBR(modelID, 'YEARLY', modelVar, year, year)

  arquivo <- folderPath
  shape <- rgdal::readOGR(arquivo, fileName, GDAL1_integer64_policy = TRUE)

  # if(!is.null(subName) & !is.null(subName)){
  #   command <- paste("shape[shape$",subName," %in% \"",subNameValue,"\",]", sep = "")
  #
  #   shape <- eval(parse(text=command))
  # }
  if(!is.null(subName) & !is.null(subNameValue)){
    shape <- shape[shape@data[,subName] %in% subNameValue,]
  }

  pontos <- data.frame(Longitude = as.numeric(gsub(",", ".", as.character(climate$Data$Longitude))),
                       Latitude = as.numeric(gsub(",", ".", as.character(climate$Data$Latitude))),
                       Value = as.numeric(gsub(",", ".", as.character(climate$Data$Value)))
  )

  sp::coordinates(pontos) <- ~ Latitude + Longitude
  sp::proj4string(pontos) <- sp::proj4string(shape)
  new_pontos <- climate$Data[!is.na(sp::over(pontos, as(shape, "SpatialPolygons"))),]
  r <- raster::rasterFromXYZ(new_pontos[c(1,2,4)])
  raster::crs(r) <- sp::CRS("+init=epsg:4326")

  paleta = "Blues"
  pal <- leaflet::colorNumeric(palette = paleta, raster::values(r),
                               na.color = "transparent", reverse = FALSE)
  pal1 <- leaflet::colorNumeric(palette = paleta, raster::values(r),
                                na.color = "transparent", reverse = TRUE)

  ## custom label format function
  myLabelFormat = function(..., reverse_order = FALSE){
    if(reverse_order){
      function(type = "numeric", cuts){
        cuts <- sort(cuts, decreasing = T)
      }
    }else{
      labelFormat(...)
    }
  }

  leaflet::leaflet() %>%
    leaflet::addTiles(attribution = 'Data source: <a href="http://cptec.inpe.br">CPTEC/INPE</a>') %>%
    leaflet::addPolygons(data=shape, color = "black", weight = 1, fillOpacity = 0) %>%
    leaflet::addRasterImage(r, colors = pal, layerId =  "values",opacity = 0.8) %>%
    leafem::addMouseCoordinates() %>%
    leafem::addImageQuery(r, type="mousemove", layerId = "values", position = "topright", digits = 3, prefix = climate$Variable_name)%>%
    leaflet::addLegend(pal = pal1, values = raster::values(r),
                       title = climate$Variable_description,
                       labFormat = myLabelFormat(reverse_order = T))
}
