list.of.packages <- c("shiny", "shinydashboard","ggplot2","DT","leaflet","leaflet.extras",
                      "jsonlite","shinyWidgets","shinyBS","tidyverse","lubridate","shinycssloaders")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

list.of.packages <- c("EtaModelCC")
new.packages <- list.of.packages[!("EtaModelCC" %in% installed.packages()[,"Package"])]
if(length(new.packages)) remotes::install_github("holbig/EtaModelCC")

library(shiny)
library(shinydashboard)
library(shinyBS)
library(DT)
library(ggplot2)
library(jsonlite)
library(EtaModelCC)
library(leaflet)
library(shinyWidgets)
library(shinyFiles)
library(mapview)

library(tidyverse)
library(lubridate)
library(shinycssloaders)
library(leaflet.extras)

# Data-frame com variaveis geradas pelos modelos de mudancas climaticas
url <- "https://projeta.cptec.inpe.br/api/v1/public/variables"
variables <- jsonlite::fromJSON(url)
variables <- as.data.frame(variables$data[,c(1,3,4)])

# Data-frame com frequencias geradas pelos modelos de mudancas climaticas
url <- "https://projeta.cptec.inpe.br/api/v1/public/intervals"
frequency <- jsonlite::fromJSON(url)
frequency <- as.data.frame(frequency$data[,c(1,3)])

# Data-frame com os dados dos modelos de mudanÃ§as climaticas
url <- "https://projeta.cptec.inpe.br/api/v1/public/models"
models <- jsonlite::fromJSON(url)
models <- as.data.frame(models$data)

caption <- "The data provided by this platform cannot be used for commercial purposes. In no case can CPTEC/INPE be held liable for damages arising from the use of this data."

# Data-frame com os dados das cidades brasileiras
load("./data/cities_BR.RDA")

# Data-frame com os dados das cidades brasileiras
load("./data/units.Rda")
