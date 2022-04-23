

##Mapas interactivos##
##13-08-2020##

setwd("~/MAPAS_R")

library(shiny)
library(leaflet)
library(sf)
library(tmap)

censo <- st_read("argcenso.shp")

varlist <- setdiff(names(censo), "geometry")

runApp(list(
  ui = fluidPage(
    titlePanel("Shiny tmap!"),
    sidebarLayout(
      sidebarPanel(
        selectInput("var", label = "Variable", choices = varlist, selected = "Fem")  
      ),
      mainPanel(
        leafletOutput("map")
      )
    )
  ),
  server = function(input, output) {
    output$map = renderLeaflet({
      if (packageVersion("tmap") >= 2.0) {
        tm <- tm_basemap(leaflet::providers$Stamen.TerrainBackground) +
          tm_shape(censo) +
          tm_polygons(input$var) +
          tm_tiles(leaflet::providers$Stamen.TonerLabels, group = "Labels")  
      } else {
        tm <- tm_shape(censo) +
          tm_polygons(input$var) + 
          tm_view(basemaps = "Stamen.TerrainBackground")
      }
      
      tmap_leaflet(tm)
    })
  }
))
