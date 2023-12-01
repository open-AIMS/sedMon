library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinyTree)
library(fansi)
library(DT)
library(reactable)
library(leaflet)


unloadNamespace("sedMod")
#detach(package:sedMod)
library(sedMod)

source("../shiny/ui.R")
source("../shiny/server.R")

shinyApp(ui, server)
