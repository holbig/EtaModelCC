shinyServer( 
  function(input, output, session){
    

    volumes = getVolumes()
    shapeInfo <- reactiveValues()
    shapeCoords <- reactiveValues()
    
    #Upload Shapefile
    #shape@bbox[[2]] = lat1
    #shape@bbox[[1]] = lon1
    #shape@bbox[[3]] = lon2
    #shape@bbox[[4]] = lat2
    
    observe({  
      shinyFileChoose(input, "Btn_GetFile", roots = volumes, session = session)
      
      if(!is.null(input$Btn_GetFile)){
        file_selected<-parseFilePaths(volumes, input$Btn_GetFile)
        output$txt_file <- renderText(as.character(file_selected$datapath))
        Name <- substr(file_selected$name, 1, nchar(file_selected$name)-4)
        Path <- substr(file_selected$datapath, 1, nchar(file_selected$datapath)-(nchar(file_selected$name)+1))
        shapeInfo$Path <- Path
        shapeInfo$Name <- Name
        
        if(!identical(shapeInfo$Path, character(0))){
            arquivo <- shapeInfo$Path
            shape <- rgdal::readOGR(arquivo, shapeInfo$Name, GDAL1_integer64_policy = TRUE)
        
            shapeCoords$lat1 <- shape@bbox[4]
            shapeCoords$lat2 <- shape@bbox[2]
            shapeCoords$lon1 <- shape@bbox[1]
            shapeCoords$lon2 <- shape@bbox[3]
       }
        
        output$shape_var_lat_n <- renderUI({
          lat_norte <- shapeCoords$lat1
          numericInput('lat_n', value = lat_norte, label = "North Latitude")
        })
        output$shape_var_lat_s <- renderUI({
          lat_sul <- shapeCoords$lat2
          numericInput('lat_s', value = lat_sul, label = "South Latitude")
        })
        output$shape_var_lon_l <- renderUI({
          lon_leste <- shapeCoords$lon2
          numericInput('lon_l', value = lon_leste, label = "East Longitude")
        })
        output$shape_var_lon_o <- renderUI({
          lon_oeste <- shapeCoords$lon1
          numericInput('lon_o', value = lon_oeste, label = "West Longitude")
        })
        
      }
      
    })
    
    observeEvent(input$show, {
      showModal(
        modalDialog(
          uiOutput("mapModal"),
          leafletOutput("polygon_map", height = 800),
          footer = modalButton("Ok"), 
          size="l", easyClose=TRUE
        ),
      )
    })
    
    globals <- eventReactive(input$load, {
      if(input$var_search == "Coordenate"){
        c(y1=input$coord_iyear,y2=input$coord_fyear,
          g_model=input$var_model, g_freq=input$var_freq, g_var=input$var_Eta, 
          g_lat=input$var_lat, g_lon=input$var_lon)
      }else if(input$var_search == "City"){
        c(y1=input$city_iyear,y2=input$city_fyear,
          g_model=input$var_model, g_freq=input$var_freq, g_var=input$var_Eta, 
          g_lat=cities$latitude[which(cities$nome_municipio==input$var_city)],
          g_lon=cities$longitude[which(cities$nome_municipio==input$var_city)])
      }else if(input$var_search == "Polygon"){
        c(y=input$polygon_year, g_model=input$var_model, g_var=input$var_Eta,
          g_lat1=polygonCoords$lat1, g_lon1=polygonCoords$lon1, 
          g_lat2=polygonCoords$lat2, g_lon2=polygonCoords$lon2)
      }else{
        c(y=input$shape_year, g_model=input$var_model, g_var=input$var_Eta,
          g_lat1=shapeCoords$lat1, g_lon1 = shapeCoords$lon1, 
          g_lat2=shapeCoords$lat2, g_lon2 = shapeCoords$lon2)
      }
    })
    
    clima <- eventReactive(input$load, {
      if(input$var_search == "Polygon"){
        getClimateDataPontos(modelID = globals()['g_model'], modelVar = globals()['g_var'],
                             lat1 = globals()['g_lat1'], lon1 = globals()['g_lon1'],
                             lat2 = globals()['g_lat2'], lon2 = globals()['g_lon2'],
                             year =input$polygon_year)
        
      }else if(input$var_search == "Shapefile"){
        getClimateDataPontos(modelID = globals()["g_model"], modelVar = globals()["g_var"],
                             lat1 = globals()['g_lat1'], lon1 = globals()['g_lon1'],
                             lat2 = globals()['g_lat2'], lon2 = globals()['g_lon2'],
                             year = input$shape_year)
        
        
      }else{
        getClimateData(globals()['g_model'], globals()['g_freq'], 
                       globals()['g_var'],
                       lat = globals()['g_lat'], lon = globals()['g_lon'], 
                       iYear = globals()['y1'],
                       fYear = globals()['y2'])
      }
      
    })
    
    Dados <- eventReactive(input$load, {
      dados <- as.data.frame(clima()["Data"])
      if(input$var_search != "Polygon" && input$var_search != "Shapefile"){
        if(input$var_freq == "MONTHLY"){
          colnames(dados) <- c("Year", "Month", "Value")
          dados <- tidyr::unite(dados, col="Date", Year, Month, sep="-01-", remove = FALSE)
          dados$Date = lubridate::ydm(dados$Date)
        }else if(input$var_freq == "DAILY"){
          colnames(dados) <- c("Date", "Value")
          dados$Date = lubridate::ymd(dados$Date)
        }else if(input$var_freq == "HOURLY"){
          colnames(dados) <- c("Time", "Hour", "Value")
          dados <- tidyr::unite(dados, col="Date", Time, Hour, sep="-", remove = FALSE)
          dados$Date = lubridate::ymd_hm(dados$Date)
        }else {
          colnames(dados) <- c("Date", "Value")
        }
      }
      return(dados)
    })
    
    cols_tabela <- eventReactive(input$load, {
      if(input$var_search == "Polygon" || input$var_search == "Shapefile"){
        c('Latitude','Longitude', 'Year', 'Value')
      } else {
        switch(input$var_freq,
               "YEARLY" = c("Date","Value"),
               "MONTHLY" = c("Year","Month","Value"),
               "DAILY" = c("Date","Value"),
               "HOURLY" = c("Date","Hour","Value"))
      }
    })
    
    
    plot_object <- reactive({
      dados <- as.data.frame(Dados())
      
      valores = as.numeric(dados$Value)
      
      ggplot() +
        ggplot2::geom_line(aes(dados$Date, valores, group = 1), color="red") +
        ggplot2::geom_point(aes(dados$Date, valores), color="blue") +
        ggplot2::labs(title = paste("Forecast for longitute: ",globals()['g_lon'],
                                    " / latitude: ",globals()['g_lat'], sep=""),
                      subtitle = paste("Forecast by Eta Model 20km --",
                                       units$name[which(units$variable == input$var_Eta)]),
                      caption = "Source: CPTEC/INPE, Brazil",
                      x = "Date", y = units$unit[which(units$variable == input$var_Eta)])
    })
    
    output$plot <- renderPlot({
      shiny::validate(
        need(input$var_search != "Polygon", "Unavailable")
      )
      plot_object()
    })
    
    output$tabela <- renderDataTable({
      dados <- as.data.frame(clima()["Data"])
      colnames(dados) <- cols_tabela()
      dados},options = list(pageLength = 10)
    )
    
    output$tabelaPolygon <- renderDataTable({
      dados <- as.data.frame(clima()["Data"])
      colnames(dados) <- cols_tabela()
      dados},options = list(pageLength = 10)
    )
    
    output$tabelaShape <- renderDataTable({
      dados <- as.data.frame(clima()["Data"])
      colnames(dados) <- cols_tabela()
      dados},options = list(pageLength = 10)
    )
    
    output$yearselect <- renderUI({
      selected_value <- input$var_model
      ano1 <- models$start_year[which(models$id == selected_value)]
      ano2 <- models$end_year[which(models$id == selected_value)]
      numericInput("polygon_year", label = "Year", 
                   value = ano1, min = ano1, max = ano2)
    })
    
    output$coord_yearselect1 <- renderUI({
      selected_value <- input$var_model
      ano1 <- models$start_year[which(models$id == selected_value)]
      ano2 <- models$end_year[which(models$id == selected_value)]
      numericInput("coord_iyear", label = "Initial year",
                   value = ano1, min = ano1, max = ano2)
    })
    
    output$coord_yearselect2 <- renderUI({
      selected_value <- input$var_model
      ano1 <- models$start_year[which(models$id == selected_value)]
      ano2 <- models$end_year[which(models$id == selected_value)]
      numericInput("coord_fyear", label = "Final year", 
                   value = ano1, min = ano1, max = ano2)
    })
    
    output$city_yearselect1 <- renderUI({
      selected_value <- input$var_model
      ano1 <- models$start_year[which(models$id == selected_value)]
      ano2 <- models$end_year[which(models$id == selected_value)]
      numericInput("city_iyear", label = "Initial year", 
                   value = ano1, min = ano1, max = ano2)
    })
    
    output$city_yearselect2 <- renderUI({
      selected_value <- input$var_model
      ano1 <- models$start_year[which(models$id == selected_value)]
      ano2 <- models$end_year[which(models$id == selected_value)]
      numericInput("city_fyear", label = "Final year", 
                   value = ano1, min = ano1, max = ano2)
    })
    
    
    output$shape_yearselect <- renderUI({
      selected_value <- input$var_model
      ano1 <- models$start_year[which(models$id == selected_value)]
      ano2 <- models$end_year[which(models$id == selected_value)]
      numericInput("shape_year", label = "Year", 
                   value = ano1, min = ano1, max = ano2)
    })
    
    output$figura <- renderImage({
      
      return(list(
        src = paste("~/UPF/Pesquisa/Git Projects/ProjetaVisual/maps/",input$var_Eta,".png",sep=""),
        contentType = "image/png",
        alt = "Face"
      ))
      
    }, deleteFile = FALSE)
    
    
    map_object <- reactive({
      if(input$var_search != "Polygon"){
        lat <- as.numeric(globals()['g_lat'])
        lon <- as.numeric(globals()['g_lon'])
        leaflet() %>% addTiles %>% setView(lng=lon, lat=lat, zoom=5) %>%
          addMarkers(lng=lon,lat=lat)
      }
    })
    
    output$mapa <- renderLeaflet(
      if(input$var_search != "Polygon"){
        map_object()
      }
    )
    
    #Download Print
    output$printMap <- downloadHandler(
      filename = function() {
        paste("customleaflet", Sys.Date(), ".png", sep="")
      },
      
      content = function(file) {
        mapshot( x = map_object()
                 , file = file
                 , cliprect = "viewport"
                 , selfcontained = FALSE 
        )
      }
    )
    
    shape_map_object <- reactive(
      if(input$var_search == "Shapefile"){
        lat = (as.numeric(globals()['g_lat1']) + as.numeric(globals()['g_lat2'])) /2
        lon = (as.numeric(globals()['g_lon1']) + as.numeric(globals()['g_lon2'])) /2
        leaflet() %>% addTiles() %>% setView(lng=lon,lat=lat, zoom = 5) %>%
          addRectangles(
            lng1 =  as.numeric(globals()['g_lon1']), lat1 =  as.numeric(globals()['g_lat1']),
            lng2 = as.numeric(globals()['g_lon2']), lat2 = as.numeric(globals()['g_lat2']),
            color = "blue", fillColor = "transparent"
          )
      }
    )
    
    output$shape_map <- renderLeaflet(
      if(input$var_search == "Shapefile"){
        shape_map_object()
      }
    )
    
    output$polygon_map <- renderLeaflet(
      leaflet() %>%
        addTiles %>%
        setView(lng = -60, lat = -20, zoom = 3) %>%
        addDrawToolbar(
          targetGroup='draw',
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
          circleOptions = FALSE,
          circleMarkerOptions = FALSE,
          markerOptions = FALSE,
          polygonOptions = FALSE,
          polylineOptions = FALSE)  %>%
        addLayersControl(overlayGroups = c('draw'), options =
                           layersControlOptions(collapsed=FALSE)) %>%
        addRectangles(
          lng1 =  -75.05, lat1 = -35.05,
          lng2 = -33.95, lat2 = 5.9,
          color = "red", fillColor = "transparent"
        )
    )
    
    
    
    polygonCoords <- reactiveValues()
    observeEvent(input$polygon_map_draw_new_feature,{
      coords <- input$polygon_map_draw_new_feature
      
      output$var_lat_n <- renderUI({
        lat_norte <- coords$geometry$coordinates[[1]][[2]][[2]]
        polygonCoords$lat1 <- lat_norte
        numericInput('lat_n', value = lat_norte, label = "North Latitude")
      })
      output$var_lat_s <- renderUI({
        lat_sul <- coords$geometry$coordinates[[1]][[1]][[2]]
        polygonCoords$lat2 <- lat_sul
        numericInput('lat_s', value = lat_sul, label = "South Latitude")
      })
      output$var_lon_l <- renderUI({
        lon_leste <- coords$geometry$coordinates[[1]][[1]][[1]]
        polygonCoords$lon2 <- lon_leste
        numericInput('lon_l', value = lon_leste, label = "East Longitude")
      })
      output$var_lon_o <- renderUI({
        lon_oeste <- coords$geometry$coordinates[[1]][[3]][[1]] #coords[[1]][[3]]
        polygonCoords$lon1 <- lon_oeste 
        numericInput('lon_o', value = lon_oeste, label = "West Longitude")
      })
      
      leafletProxy("polygon_map") %>% removeDrawToolbar(clearFeatures = TRUE)
      
    }, ignoreNULL = FALSE)
    
        
    polygon_map_object <- reactive({
      if(input$var_search == "Polygon"){
        lat = (as.numeric(globals()['g_lat1']) + as.numeric(globals()['g_lat2'])) /2
        lon = (as.numeric(globals()['g_lon1']) + as.numeric(globals()['g_lon2'])) /2
        leaflet() %>% addTiles() %>% setView(lng=lon,lat=lat, zoom = 5) %>%
          addRectangles(lng1 = as.numeric(globals()['g_lon1']), #as.numeric(input$lon_o), 
                        lat1 = as.numeric(globals()['g_lat1']), #as.numeric(input$lat_n), 
                        lng2 = as.numeric(globals()['g_lon2']), #as.numeric(input$lon_l), 
                        lat2 = as.numeric(globals()['g_lat2']), #as.numeric(input$lat_s), 
                        color = "blue", fillColor = "transparent")
      }
    })
      
    output$polygon_map_render <- renderLeaflet(
      if(input$var_search == "Polygon"){
        polygon_map_object()
      }
    )
    
    #Download map image
    output$printPolygon <- downloadHandler(
      filename = function() {
        paste("customleaflet", Sys.Date(), ".png", sep="")
      },
      
       content = function(file) {
        mapshot( x = polygon_map_object()
                 , file = file
                 , cliprect = "viewport"
                 , selfcontained = FALSE 
        )
      }
    )
    
    output$printShape <- downloadHandler(
      filename = function() {
        paste("customleaflet", Sys.Date(), ".png", sep="")
      },
      
      content = function(file) {
        mapshot( x = shape_map_object()
                 , file = file
                 , cliprect = "viewport"
                 , selfcontained = FALSE 
        )
      }
    )
    
    #Download plot image
    output$downloadPlot <- downloadHandler(
      filename = function(){paste("dataset-", Sys.Date(), ".png", sep="")},
      content = function(file){
        ggsave(file,plot=plot_object())
      }
    )
    
    
    
    # Downloadable csv of selected dataset ----
    output$download_csv <- downloadHandler(
      
      filename = function() {
        paste("dataset-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(clima()['Data'], file, row.names = FALSE)
      }
    )
    
    output$download_csvPolygon <- downloadHandler(
      
      filename = function() {
        paste("dataset-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(clima()['Data'], file, row.names = FALSE)
      }
    )
    
    output$download_csvShape <- downloadHandler(
      
      filename = function() {
        paste("dataset-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(clima()['Data'], file, row.names = FALSE)
      }
    )
    
    
    
    observe({
      if(input$navbar1 == "stop") 
        stopApp()
    })
  }
)
