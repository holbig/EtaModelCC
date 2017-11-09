# Verifica a instalação dos pacotes necessários para o programa
list.of.packages <- c("jsonlite","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

suppressMessages(library(jsonlite))
suppressMessages(library(ggplot2))

source(paste(getwd(),"/R/global.R", sep=""))

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
    geom_bar(data=subset(dat.long,variable=="PREC"),aes(time,value),
             stat="identity", color="blue", fill="white") +
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
    geom_bar(data=subset(dat.long,variable=="Precipitation"),aes(time,value),stat="identity",
                 color="green", fill="white") +
    scale_colour_manual(values=c("blue","red","green")) +
   labs(title = paste("Forecast for longitute: ",lon," / latitude: ",lat, sep=""),
         subtitle = "Forecast by Eta Model 15km",
         caption = "Source: CPTEC/INPE, Brazil",
         x = "Date", y = expression(paste(" "^"o","C"," / % / mm"))) +
    theme(legend.position="top",
          legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
          legend.title=element_blank(),
          legend.text = element_text(size = 10, face = "bold"))
}

plot.RCPTEC.var <- function(lon, lat, var, iTime = NULL, fTime = NULL){
  info <- getWeatherData(lon, lat, var, iTime, fTime)
  datas = as.POSIXct(strptime(colnames(info$Dados),'%Y%m%d%H'))

  new.data = data.frame(time=datas,
                        Variavel = as.numeric(info$Dados[var,]))
  if (var == "PREC")
    new.data$Variavel[which(new.data$Variavel==0)] = NA
  dat.long<-melt(new.data,id.vars="time")

  if(var!="PREC"){
    ggplot(dat.long) +
      geom_line(aes(time, value), color="blue") +
      geom_point(aes(time, value), color="blue") +
      labs(title = paste("Forecast for longitute: ",lon," / latitude: ",lat, sep=""),
           subtitle = paste("Forecast by Eta Model 15km --",
                            var_eta15km$description[which(var_eta15km$variable == var)]),
           caption = "Source: CPTEC/INPE, Brazil",
           x = "Date", y = var_eta15km$unit.measure[which(var_eta15km$variable == var)])
  } else{
    ggplot(dat.long) +
      geom_bar(aes(time,value),stat="identity", color="blue", fill="white") +
      labs(title = paste("Forecast for longitute: ",lon," / latitude: ",lat, sep=""),
           subtitle = paste("Forecast by Eta Model 15km --",
                            var_eta15km$description[which(var_eta15km$variable == var)]),
           caption = "Source: CPTEC/INPE, Brazil",
           x = "Date", y = var_eta15km$unit.measure[which(var_eta15km$variable == var)])

  }
}
