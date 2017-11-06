#### Funtions for plot data of weather forecast ###

add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0),
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

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

  if(var!="PREC"){
    plot(x,y, type="l", main = paste("Forecast for longitude: ",lon," / latitude: ",lat, sep=""),
         sub = "Forecast by Model Eta 15km (CPTEC/INPE, Brazil)", xlab = "Date", col = "blue", lwd =2,
         ylab = var_eta15km$unit.measure[which(var_eta15km$variable == var)],
         ylim = c(range(y)[1]-2, range(y)[2]+2), cex.lab = 1.5, cex.main=1.5)
    mtext(var_eta15km$description[which(var_eta15km$variable == var)], cex=1.5, col="blue")
    points(x,y, pch=16, col="blue")
  } else{
    barplot(y, main = paste("Forecast for longitude: ",lon," / latitude: ",lat, sep=""),
         sub = "Forecast by Model Eta 15km (CPTEC/INPE, Brazil)", xlab = "Date", col = "blue", lwd =2,
         ylab = var_eta15km$unit.measure[which(var_eta15km$variable == var)],
         ylim = c(range(y)[1], range(y)[2]+1), cex.lab = 1.5, cex.main=1.5)
    mtext(var_eta15km$description[which(var_eta15km$variable == var)], cex=1.5, col="blue")
    lab<-x[seq(1, length(x), by=24)]
    axis(1, at=seq(1, length(x), by=24),labels=lab)
  }

  linha = ceiling((range(y)[2]-range(y)[1])/10)
  abline(h=seq(range(y)[1],range(y)[2],by=linha), col="black")
}


plot.RCPTEC.THP <- function(lon, lat, iTime = NULL, fTime = NULL){
  info <- getWeatherData(lon, lat, "all", iTime, fTime)

  # Add extra space to right of plot area; change clipping to figure
  par(mar = c(5, 4, 1.4, 0.2))

  x = strptime(colnames(info$Dados),'%Y%m%d%H')

  y = as.numeric(info$Dados["TP2M",])
  plot(x,y, type="l", main = paste("Forecast for longitude: ",lon," / latitude: ",lat, sep=""),
       sub = "Forecast by Model Eta 15km (CPTEC/INPE, Brazil)", xlab = "Date",
       ylab = expression(paste("o"^"C"," / % / mm")),
       ylim = c(-5,110), col= "blue",lwd=2)
  abline(h=seq(0,100,by=10), col="black")

  y = as.numeric(info$Dados["UR2M",])
  lines(x,y,col="green",lwd=2)

  y = as.numeric(info$Dados["PREC",])
  y[which(y==0)] = NA
  lines(x,y,col="red",lwd=3, type = "h")

  add_legend("topright", legend = c("Temperature","Relative Humidity","Precipitation"), horiz=TRUE,
             bty='n', cex=0.8, col=c("blue","green","red"), lty = 1, lwd=2)
}
