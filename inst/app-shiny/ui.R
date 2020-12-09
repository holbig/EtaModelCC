ui <- fluidPage(theme= "cerulean_opt.css",
                
    navbarPage(title=HTML('<div><a href="https://projeta.cptec.inpe.br" target="_blank">
	                         <img src="./img/projeta_logo.png" width="180px" height="40px" style="margin-top: -4px"></a></div>'),
                           windowTitle="PROJETA",collapsible=TRUE,id="navbar1",
    
        tabPanel("Welcome",
             source("welcome.R")$value,
    ), 
    
    tabPanel("Explore Data",
        sidebarPanel(
            
            selectInput("var_model","Climate Scenary:",
                choices=setNames(models$id, models$name),
                selected=models$id[1]
            ),
                                                 
            selectInput("var_Eta","Variable:",
                choices=setNames(variables$nickname, variables$name),
                selected="TP2M"
            ),
            
            selectInput("var_freq","Frequency:",
                choices=frequency$nickname,
                selected="YEARLY"
            ),
                                                 
            radioGroupButtons(
                inputId = "var_search", label = "Make a choice :", 
                justified = TRUE,
                choices = c("Coordenate", "City", "Polygon", "Shapefile"), 
                status = "primary",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), 
                                 no = icon("remove", lib = "glyphicon")),
            ),
                                                 
            #Coordenate Panel
            conditionalPanel("input.var_search=='Coordenate'",	
                column(6, uiOutput('coord_yearselect1'), style = "min-width: auto"),
                column(6, uiOutput('coord_yearselect2'), style = "min-width: auto"),
                column(6, textInput("var_lat", label = "Latitude", 
                                    value = -35.05)),
                bsTooltip("var_lat", "5.9 to -35.05",
                          "right", options = list(container = "body")),
                column(6, textInput("var_lon", label = "Longitude",
                                    value = -75.05)),
                bsTooltip("var_lon", "-75.05 to -33.95",
                          "right", options = list(container = "body")),                    
            ),
                                                 
            #City Panel
            conditionalPanel("input.var_search=='City'",
                column(6, uiOutput('city_yearselect1'), style = "min-width: auto"),
                column(6, uiOutput('city_yearselect2'), style = "min-width: auto"),
                column(12, align = "center",
                    selectInput("var_city","City:",width="100%",
                                choices=cities$nome_municipio),
                    ),
                bsTooltip("var_city", "Choice in list or write it´s name",
                          "right", options = list(container = "body"))
                ),
                                                 
            #Polygon Panel
            conditionalPanel("input.var_search=='Polygon'",
                fluidRow(
                    column(6, align="center", offset = 3,
                        actionButton("show","Map", icon("map"),
                                     style="color: black; border-color: black; width: 100%"),
                    )
                ),
                column(12, uiOutput('yearselect'), style = "min-width: auto"),
                column(6, uiOutput("var_lat_n")),
                column(6, uiOutput("var_lat_s")),
                column(6, uiOutput("var_lon_l")),
                column(6, uiOutput("var_lon_o")),
            ),
            
            #Shapefile Panel
            conditionalPanel("input.var_search=='Shapefile'",
                             column(12, uiOutput('shape_yearselect'), style = "min-width: auto"),
                             fluidRow(
                                 column(6, align="center", offset = 3,
                                        shinyFilesButton("Btn_GetFile", "Choose a file" ,
                                                         title = "Please select a file:", multiple = FALSE,
                                                         buttonType = "default", class = NULL,
                                                         style="color: Black; background-color: #337ab7;
	                                                     border-color: black; min-width: 120px"),
                                        HTML('<br>'),
                                 ),
                             ),
                             column(6, uiOutput("shape_var_lat_n")),
                             column(6, uiOutput("shape_var_lat_s")),
                             column(6, uiOutput("shape_var_lon_l")),
                             column(6, uiOutput("shape_var_lon_o")),
            ),
           
           fluidRow(
              column(6, align="center", offset = 3,
                  actionButton("load", "Send request", icon("upload"),
                               class="btn-block", width = '40%',
                               style="color: Black; background-color: #337ab7;
	                              border-color: black; min-width: 120px"),
                   ),
              ),
           HTML('<br>'),
           h5(HTML(paste('<p style="text-align:justify">',caption, 
                         '<a href="https://projeta.cptec.inpe.br" target="_blank">projeta.cptec.inpe.br</a></p>'))),
           ),
            
            mainPanel(
                conditionalPanel("input.var_search=='Coordenate' || input.var_search=='City'",

                    tabsetPanel(
                  # Dataset tab panel
                        tabPanel(p(icon("table"), "Dataset"),
                          dataTableOutput(outputId="tabela") %>% withSpinner(),
                          HTML('<br>'),
                          downloadButton("download_csv","Download csv")),
                                            
                  # Plot tab panel
                      tabPanel(p(icon("line-chart"), "Plot"),
                          plotOutput("plot") %>% withSpinner(),
                          HTML('<style>.rChart {width: "auto"; height: "auto"}</style>'),
                          downloadButton("downloadPlot","Download Plot")
                          ),
                                            
                  # Map tab panel
                      tabPanel(p(icon("globe"), "Map"),
                          leafletOutput("mapa") %>% withSpinner(),
                           HTML('<style>.rChart {width: "auto"; height: "auto"}</style>'),
                          downloadButton( outputId = "printMap", "Download Map"),
                          )
                 ),
              ),
              
              conditionalPanel("input.var_search=='Polygon'",
                 # Dataset tab panel
                    tabsetPanel(
                        tabPanel(p(icon("table"), "Dataset"),
                                 dataTableOutput(outputId="tabelaPolygon") %>% withSpinner(),
                                 HTML('<br>'),
                                 downloadButton("download_csvPolygon","Download csv")
                                ),
                        tabPanel(p(icon("globe"), "Map"),
                                 leafletOutput("polygon_map_render") %>% withSpinner(),
                                 HTML('<style>.rChart {width: "auto"; height: "auto"}</style>'),
                                 downloadButton( outputId = "printPolygon","Download Map"),
                                 )
                    ),
             ),
             
             conditionalPanel("input.var_search=='Shapefile'",
                              # Dataset tab panel
                              tabsetPanel(
                                  tabPanel(p(icon("table"), "Dataset"),
                                           dataTableOutput(outputId="tabelaShape") %>% withSpinner(),
                                           HTML('<br>'),
                                           downloadButton("download_csvShape","Download csv"),
                                  ),
                                  tabPanel(p(icon("globe"), "Map"),
                                    leafletOutput("shape_map")%>% withSpinner(),
                                    downloadButton( outputId = "printShape","Download Map"),
                                  )
                              ),
                              
             ),
           ),
    ),
                           
    #About
    navbarMenu(title = "About",
        tabPanel(title = "About Projeta", source("about.R", local=T)$value),
        tabPanel("About Models", source("aboutModels.R")$value),
        tabPanel("About RCPs", source("aboutRCPs.R")$value)
    ),
    #Quit button
    tabPanel(title = "Quit", value="stop", icon = icon("circle-o-notch"))
                           
    ),
    
)