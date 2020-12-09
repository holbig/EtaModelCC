arquivo <- folderPath
shape <- rgdal::readOGR(arquivo, fileName, GDAL1_integer64_policy = TRUE)

(shape@bbox)
lon1 <- shape@bbox[[1]]  # -73.99024
lat1 <- shape@bbox[[2]]  # -33.75136
lon2 <- shape@bbox[[3]]  # -32.39088
lat2 <- shape@bbox[[4]]  #   5.270972
clima <- getClimateDataPontos('2', 'PREC', lat1, lon1, lat2, "-33.95", year = 2006)

clima <- getClimateDataPontos('2', 'PREC', shape@bbox[[2]], shape@bbox[[1]], shape@bbox[[4]], shape@bbox[[3]], year = 2006)
plotMapShape(modelID='2', modelVar='PREC', year=2019, folderPath="../test_Eta_pack/shapes", 
             fileName="estados", subName="sigla", subNameValue=c("PA"))

leaflet() %>% addTiles() %>%
  addRectangles(
    shape@bbox[[1]], shape@bbox[[2]],
    shape@bbox[[3]], shape@bbox[[4]],
    fillColor = "transparent"
  )