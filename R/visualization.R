#### Funtions for plot data of weather forecast ###

plot.RCPTEC.meteogram <- function(lon, lat, iTime = NULL, fTime = NULL){
  info <- getWeatherData(lon, lat, "all", iTime, fTime)
  x = strptime(colnames(info$Dados),'%Y%m%d%H')
  if (dev.interactive()) dev.new()
  par(mfrow=c(5,1))
  for (i in 1:5){
    y = as.numeric(info$Dados[i,])
    plot(x,y, type="l", main = paste("Forecast for lonigute: ",lon," / latitude: ",lat, sep=""),
     sub = "Forecast by Model Eta 15km (CPTEC/INPE, Brazil)", xlab = "Date",
     ylab = var_eta15km$description[i],
     ylim = range(y)+5)
  }
}

plot.RCPTEC.var <- function(lon, lat, var, iTime = NULL, fTime = NULL){
  info <- getWeatherData(lon, lat, var, iTime, fTime)
  y = as.numeric(info$Dados)
  x = strptime(colnames(info$Dados),'%Y%m%d%H')
  plot(x,y, type="l", main = paste("Forecast for longitude: ",lon," / latitude: ",lat, sep=""),
       sub = "Forecast by Model Eta 15km (CPTEC/INPE, Brazil)", xlab = "Date",
       ylab = var_eta15km$description[which(var_eta15km$variable == var)])
}


plot.RCPTEC.THP <- function(lon, lat, iTime = NULL, fTime = NULL){
  info <- getWeatherData(lon, lat, "all", iTime, fTime)
  x = strptime(colnames(info$Dados),'%Y%m%d%H')

  y = as.numeric(info$Dados["TP2M",])
  plot(x,y, type="l", main = paste("Forecast for longitude: ",lon," / latitude: ",lat, sep=""),
       sub = "Forecast by Model Eta 15km (CPTEC/INPE, Brazil)", xlab = "Date",
       ylab = var_eta15km$description[which(var_eta15km$variable == "TP2M")],
       ylim = c(-5,100), col= "blue")

  y = as.numeric(info$Dados["UR2M",])
  lines(x,y,col="red")
  y = as.numeric(info$Dados["PREC",])
  lines(x,y,col="green")
}
