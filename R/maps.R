# Verifica a instalação dos pacotes necessários para o programa
list.of.packages <- c("jsonlite","rgdal","leaflet","raster")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

suppressMessages(library(jsonlite))
suppressMessages(library(rgdal))
library(leaflet)
#library(RColorBrewer)
library(raster)

source("R/global.R")

RCPTEC_map("OCIS", "2017112321", c("RS","PR","MG"))
RCPTEC_map("UR2M", "2017112313", c("BR"))
RCPTEC_map("TP2M", "2017112313", c("RS","PR","MG"))

weatherData="PREC"
iTime="2017110400"
estado="AM"

### Functions for weather models
RCPTEC_map <- function(weatherData, iTime, estado) {
  tabela <- loadRda(weatherData)

  arquivo = "C:/Users/Carlos/Documents/UPF/Pesquisa/Git Projects/RCPTEC/data"
  shape_br <- readOGR(arquivo, "estados", GDAL1_integer64_policy = TRUE)
#  invisible(capture.output(shape_br <- readOGR(arquivo, "estados", GDAL1_integer64_policy = TRUE)))
  if((length(estado)==1) & (estado=="BR"))
    shape_estado <- shape_br
  else
    shape_estado <- shape_br[shape_br$sigla %in% estado,]

  pontos <- data.frame(tabela$LONGITUDE,tabela$LATITUDE,tabela[,iTime])
  colnames(pontos) = c("LONGITUDE","LATITUDE",iTime)
  coordinates(pontos) <- c("LONGITUDE", "LATITUDE")
  proj4string(pontos) <- proj4string(shape_estado)
  new_pontos <- tabela[!is.na(over(pontos, as(shape_estado, "SpatialPolygons"))),]

  tab_raster = rasterFromXYZ(new_pontos)
  r <- raster(tab_raster,layer = grep(iTime, colnames(new_pontos)))
  crs(r) <- CRS("+init=epsg:4326")

  if(weatherData=="PREC")
    r@data@values[which(values(r)==0)] <- NA

  graf_options <- switch(weatherData,
                         OCIS = list(paleta = c("white","yellow","orange","red")),
                         TP2M = list(paleta = c("blue","green","yellow","orange","red")),
                         PREC = list(paleta = "Blues"),
                         V10M = list(paleta = "Blues"),
                         UR2M = list(paleta = c("red","orange","yellow","green")))


  #paleta = "Blues" #"RdYlBu"
  pal <- colorNumeric(palette = graf_options$paleta, values(r),
                      na.color = "transparent", reverse = FALSE)
  pal1 <- colorNumeric(palette = graf_options$paleta, values(r),
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

  leaflet() %>% addTiles(attribution = 'Data source: <a href="http://cptec.inpe.br">CPTEC/INPE</a>') %>%
    addPolygons(data=shape_estado, color = "black", weight = 1, fillOpacity = 0) %>%
    addRasterImage(r, colors = pal, opacity = 0.8) %>%
    addLegend(pal = pal1, values = values(r),
              title = var_eta15km$description[which(var_eta15km$variable == weatherData)],
              labFormat = myLabelFormat(reverse_order = T))
}


