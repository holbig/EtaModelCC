# Verifica a instalação dos pacotes necessários para o programa
list.of.packages <- c("jsonlite","ggplot2","magrittr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

suppressMessages(library(jsonlite))
suppressMessages(library(ggplot2))
suppressMessages(library(magrittr))

source(paste(getwd(),"/R/global.R", sep=""))

#### Funtions for plot data of weather forecast ###

add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0),
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
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


plot.RCPTEC.meteogram <- function(lon, lat, iTime = NULL, fTime = NULL){
  info <- getWeatherData(lon, lat, "all", iTime, fTime)
  datas = as.POSIXct(strptime(colnames(info$Dados),'%Y%m%d%H'))
  new.data = data.frame(time=datas,TP2M=as.numeric(info$Dados['TP2M',]),
                        OCIS=as.numeric(info$Dados['OCIS',]),
                        V10M=as.numeric(info$Dados['V10M',]),
                        UR2M=as.numeric(info$Dados['UR2M',]),
                        PREC=as.numeric(info$Dados['PREC',]))
  new.data$PREC[which(new.data$PREC==0)] = NA
  dat.long<-melt(new.data,id.vars="time")

  varNames = c(
    TP2M = var_eta15km$description[which(var_eta15km$variable == "TP2M")],
    PREC = var_eta15km$description[which(var_eta15km$variable == "PREC")],
    UR2M = var_eta15km$description[which(var_eta15km$variable == "UR2M")],
    V10M = var_eta15km$description[which(var_eta15km$variable == "V10M")],
    OCIS = var_eta15km$description[which(var_eta15km$variable == "OCIS")]
  )

  ggplot() +
    geom_line(data=subset(dat.long,variable=="TP2M"),aes(time,value)) +
    geom_line(data=subset(dat.long,variable=="UR2M"),aes(time,value)) +
    geom_line(data=subset(dat.long,variable=="V10M"),aes(time,value)) +
    geom_line(data=subset(dat.long,variable=="OCIS"),aes(time,value)) +
    geom_bar(data=subset(dat.long,variable=="PREC"),aes(time,value),stat="identity", color="blue") +
    ggtitle(paste("Forecast for longitute: ",lon," / latitude: ",lat, sep="")) +
    labs(title = paste("Forecast for longitute: ",lon," / latitude: ",lat, sep=""),
         subtitle = "Forecast by Eta Model 15km",
         caption = "Source: CPTEC/INPE, Brazil") +
    theme_bw() + xlab("Date") + ylab("") +
    facet_grid(variable~.,scales="free_y",
               labeller = labeller( variable = varNames, .multi_line = TRUE ))
}

plot.RCPTEC.THP <- function(lon, lat, iTime = NULL, fTime = NULL){
  info <- getWeatherData(lon, lat, "all", iTime, fTime)
  datas = as.POSIXct(strptime(colnames(info$Dados),'%Y%m%d%H'))

  new.data = data.frame(time=datas,
                        Temperature=as.numeric(info$Dados['TP2M',]),
                        Relative_Humidity=as.numeric(info$Dados['UR2M',]),
                        Precipitation=as.numeric(info$Dados['PREC',]))
  new.data$Precipitation[which(new.data$Precipitation==0)] = NA
  dat.long<-melt(new.data,id.vars="time")

  ggplot(dat.long) + geom_line(aes(x=time, y=value, colour=variable)) +
    geom_density(data=subset(dat.long,variable=="Precipitation"),aes(time,value),stat="identity",
                 color="orange", fill="orange") +
    scale_colour_manual(values=c("red","blue","orange")) +
   labs(title = paste("Forecast for longitute: ",lon," / latitude: ",lat, sep=""),
         subtitle = "Forecast by Eta Model 15km",
         caption = "Source: CPTEC/INPE, Brazil",
         x = "Date", y = expression(paste(" "^"o","C"," / % / mm"))) +
    theme(legend.position="top",
          legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
          legend.title=element_blank(),
          legend.text = element_text(size = 10, face = "bold"))
}
