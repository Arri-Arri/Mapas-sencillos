
rm(list=ls())
gc()
setwd("~/MAPAS_R")
install.packages("sf")
install.packages("raster")
install.packages("sp")

library(shiny)
library(leaflet)
library(sf)
library(tmap)
library(raster)

censo <- st_read("argcenso.shp")

varlist <- setdiff(names(censo), "geometry")



qtm(censo, fill =  NULL)
qtm(censo, fill = "Dens_h")

#Leer datos temperatura media MODis 2010

temp <- raster("Tmed2010e.tif")
#class(temp)
#(temp)

qtm(temp)
qtm(temp, legend.outside = TRUE)

#Agregando capas

mapatemp <- qtm(temp,legend.outside = TRUE)
mapatemp + qtm(censo, fill =  NULL)

qtm(temp, legend.outside = TRUE)
#la funciÃ³n last_map() estÃ¡ obsoleta, en las nuevas versiones es tmap_last()
#last_map() + qtm(censo, fill =  NULL) 
tmap_last() + qtm(censo, fill =  NULL)
#Mapas tematicos rapidos con qtm() en modo View

tmap_mode(mode = "view")
qtm("Argentina")
qtm(censo)
qtm(censo, fill =  NULL)
qtm(censo, fill =  NULL, borders = "black")
qtm(temp)
tmap_last() + qtm(censo, fill =  NULL)
#Exportar desde panel del Viewer directamente con herramienta Export y ver en un browser

#para alternar entre modos plot y view sin tener que escribir todo
ttm()

# mapa rapido vs por elementos

#qtm(censo)
#qtm(censo, fill =  NULL)




##Otra opcion##

##Creacion de mapas en R##
##Agosto 06, 2020##

rm(list=ls())
gc()
setwd("~/MAPAS_R")

Comorbilidad <- read.csv("Base_comorbilidad.csv", header=TRUE)

library(tidyverse)
library(car) #Para recodificar#

Entidades <- Comorbilidad  %>%
  group_by(Entidad.comor,Entidad) %>%
  summarize(mean_comor=(mean(Indice.comor)*100)) 

Entidades <- as.data.frame(Entidades)
str(Entidades)
table(Entidades$Entidad)

#install.packages("forcats")
library(forcats)
Entidades$Entidad <- fct_recode(Entidades$Entidad,
                                "Distrito Federal"= "Ciudad de México")

#install.packages("tmap", dependencies = T)
#install.packages("sf", dependencies = T)
#install.packages("extrafont")

library(tmap)      # ElaboraciÃ³n de mapas temÃ¡ticos <3!
library(sf)        # Manejo de datos vectoriales
library(extrafont) # Diferentes tipos de letras

Shape_entidades<- st_read("México_Estados.shp")

Shape_entidades <- merge(Shape_entidades, Entidades, by.x="ESTADO", by.y= "Entidad", all.x=TRUE)

##Construcción del mapa##

tm_shape(Shape_entidades) + tm_polygons()+

tm1 <- tm_shape(Shape_entidades) + 
       tm_polygons(col = 'mean_comor',
              title = 'Prevalencia(%)',
              title.size=2,
              palette = 'viridis',
              style = "quantile", )

tm2 <- tmap_mode(mode = "view") 

tm1+ tm_view( view.legend.position=c(0.074,0.04))
  
tm1+ tm_view( view.legend.position=c(0.074,0.04))+
     tm_layout(bg.color = "white", frame = F,
            main.title = 'Prevalencia de comorbilidad, México',
            main.title.size = 0.8,
            main.title.position = c(0.3,0.5),
            legend.hist.size = 0.5,
            legend.hist.width = 0.9,
            fontface = 1,
            fontfamily = 'Tw Cen MT Condensed',
            legend.position=c(0.074,0.04),
            legend.format  =  list (text.separator='-'),
            legend.outside = F,
            legend.height = 0.45,
            legend.bg.color = "white",
            legend.title.size = 0.9,
            legend.text.size = 0.6)

+
tm1+  tm_text('ESTADO',
          size = 0.5,
          fontface = 2,
          fontfamily = 'Tw Cen MT Condensed')+
  tm_grid(lwd=0.5, col = 'black',
          alpha = 0.15)+
  tm_scale_bar(size = 0.4,
               width = 0.21,
               color.dark = 'White',
               color.light = 'black',
               position = c(0.45,0.03))+
  tm_compass(position = c(0.85,0.85), size = 2)


tmap_save( filename  =  './mapa4.png',
           width = 5,height = 8, dpi = 300)

##Otra opcion

# Load libraries ----------------------------------------------------------
require(pacman)
install.packages("pacman")
library(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, RColorBrewer, cowplot, ggpubr, 
               ggspatial, rnaturalearth, rnaturalearthdata)
rm(list = ls())
setwd("~/MAPAS_R")
# Load data ---------------------------------------------------------------
shp <- shapefile('./shp/cacao_prd.shp')

vls <- read_csv('./tbl/data_crop.csv') %>% 
  mutate(NOMBRE_DPT = iconv(NOMBRE_DPT, to = 'latin1'),
         NOMBRE_DPT = str_to_sentence(NOMBRE_DPT))

dpt <- st_read('./shp/dptos_col.shp') %>% 
  mutate(NOMBRE_DPT = str_to_sentence(NOMBRE_DPT)) %>% 
  inner_join(., vls, by = c('NOMBRE_DPT' = 'NOMBRE_DPT'))
crd <- as_tibble(cbind(as.data.frame(st_coordinates(st_centroid(dpt))), dpto = dpt$NOMBRE_DPT)) 


wrl <- ne_countries(scale = "medium", returnclass = "sf")

#help(ne_countries)
#spdf_world@data[["continent"]]
#spdf_world <- ne_countries()
#spdf_africa <- ne_countries(continent = 'africa')
#spdf_france <- ne_countries(country = 'france')

#if (require(sp)) {
#  plot(spdf_world)
#  plot(spdf_africa)
#  plot(spdf_france)
#}


# BBOX --------------------------------------------------------------------
col_bb = st_as_sfc(st_bbox(dpt))
ext <- wrl %>% 
  filter(region_wb == 'Latin America & Caribbean') %>% 
  as(., 'Spatial') %>% 
  extent() 

# Extracting San Andres y Providencia
dp1 <- dpt %>% filter(NOMBRE_DPT != 'Archipiélago de san andrés, providencia y santa catalina')
cr1 <- crd %>% filter(dpto != 'Archipiélago de san andrés, providencia y santa catalina')

dp2 <- dpt %>% filter(NOMBRE_DPT == 'Archipiélago de san andrés, providencia y santa catalina')
cr1 <- crd %>% filter(dpto != 'Archipiélago de san andrés, providencia y santa catalina')

# To make the map
g1 <- ggplot() +
  geom_sf(data = wrl, fill = "white") +
  geom_sf(data = col_bb, fill = NA, color = 'red', size = 1.2) +
  ggtitle(label = 'Macrolocalización') +
  theme_bw() + 
  coord_sf(xlim = ext[1:2], ylim = ext[3:4]) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 7, face = "bold")) 

g2 <- ggplot() +
  geom_sf(data = wrl, fill = 'white') +
  geom_sf(data = dp1, aes(fill = crp)) +
  ggtitle(label = 'Área sembrada de Cacao por departamento en Colombia') +
  scale_fill_gradientn(name = 'Área sembrada (ha)', 
                       colours = RColorBrewer::brewer.pal(n = 8, name = 'YlOrBr'), 
                       na.value = 'white') +
  # scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), # 0.2 # 0.3
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = extent(dpt)[1:2], ylim = extent(dpt)[3:4]) +
  geom_text(data= cr1, aes(x = X, y = Y, label = dpto),
            color = "darkblue", fontface = "bold", check_overlap = FALSE, size = 3.3) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        panel.grid.major = element_blank(),
        # legend.key.width = unit(5, 'line'),
        panel.grid.minor = element_blank(),
        legend.justification = c(0,0),
        legend.position = c(0.005, 0.005),
        legend.key.size = unit(0.9, "cm"),
        legend.background = element_rect(fill = alpha('white', 1), colour = alpha('white', 0.4))) +
  labs(x = 'Longitud',
       y = 'Latitud',
       caption = "Fuente: Min. de Agricultura\n y desarrollor rural (2017)") +
  annotate(geom = 'text', x = -68, y = -3, label = 'Autor: Fabio A. \nCastro', 
           fontface = 'italic', color = 'grey22', size = 4) 

g3 <- ggplot() +
  geom_sf(data = dp2, fill = "white") +
  ggtitle(label = 'San Andres y Providencia') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size = 7, face = "bold")) 

gg_inset <- ggdraw() +
  draw_plot(g2) +
  draw_plot(g1, x = 0.72, y = 0.76, width = 0.28, height = 0.19) +
  draw_plot(g3, x = 0.03, y = 0.77, width = 0.25, height = 0.17)

ggsave(plot = gg_inset,
       filename = './myMap_inset.png', units = 'in', width = 8, height = 10, dpi = 300)





data(World, metro)

metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100

map1 <- tm_shape(metro) +
        tm_bubbles("pop2010", col = "growth", 
             border.col = "black", border.alpha = .5, 
             style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf),
             palette="-RdYlBu", contrast=1, 
             title.size="Metro population", 
             title.col="Growth rate (%)", id="name", 
             popup.vars=c("pop2010", "pop2020", "growth"))+  
         tm_legend(outside=TRUE)

current.mode <- tmap_mode("plot")

# plot map
map1

# view map with default view options
tmap_mode("view")
map1

map1 + tm_view(alpha = 1, 
               view.legend.position=c(0.074,0.04),
               basemaps = "Stamen.Watercolor")

tm_shape(World) +tm_polygons("HPI") + tm_view(projection = 0) + tm_basemap(NULL)

help(rnaturalearth)

rivers50 <- ne_download(scale = 50, type = 'rivers_lake_centerlines', category = 'physical')
sp::plot(rivers50)

ocean50 <- ne_download(scale = 50, type = 'ocean', category = 'physical')
mapx <- tm_shape(ocean50)
sp::plot(ocean50)

bari <- ne_download(scale = 50, type = 'bathymetry', category = 'raster')


https://www.naturalearthdata.com/downloads/50m-raster-data/50m-bathymetry/


tm_shape(ocean50) + tm_polygons()



Batrimetria0<- st_read("~/MAPAS_R/naturalearth/Batimetria/ne_10m_bathymetry_L_0.shp")
Batrimetria200<- st_read("~/MAPAS_R/naturalearth/Batimetria/ne_10m_bathymetry_L_200.shp")
Batrimetria1000<- st_read("~/MAPAS_R/naturalearth/Batimetria/ne_10m_bathymetry_L_1000.shp")
Batrimetria2000<- st_read("~/MAPAS_R/naturalearth/Batimetria/ne_10m_bathymetry_L_2000.shp")
Batrimetria3000<- st_read("~/MAPAS_R/naturalearth/Batimetria/ne_10m_bathymetry_L_3000.shp")

tm_shape(Batrimetria0) + tm_polygons(col="gray16") + geom_sf(alpha=1) +
tm_shape(Batrimetria200) + tm_polygons(col="darkblue") + geom_sf(alpha=0.04)+
tm_shape(Batrimetria1000) + tm_polygons(col="green") + geom_sf(alpha=0.5) + 
tm_shape(Batrimetria2000) + tm_polygons(col="darkgreen") + geom_sf(alpha=0.5) +
tm_shape(Batrimetria3000) + tm_polygons(col="blue") + geom_sf(alpha=0.1) 

marina<- st_read("~/MAPAS_R/naturalearth/ne_50m_geography_marine_polys.shp")
marina1<- st_read("~/MAPAS_R/naturalearth/ne_50m_geography_regions_elevation_points.shp")
marina2<- st_read("~/MAPAS_R/naturalearth/ne_50m_geography_regions_points.shp")
marina3<- st_read("~/MAPAS_R/naturalearth/ne_50m_geography_regions_polys.shp")


tm_shape(marina) + tm_polygons() +
tm_shape(marina2) + tm_polygons() +
  tm_shape(marina3) + tm_polygons() 
  tm_shape(marina3) + tm_polygons() 
  
  
p1 + p2


tm_polygons(col = 'mean_comor',
            title = 'Prevalencia(%)',
            title.size=2,
            palette = 'viridis',
            style = "quantile", )



oceano50<- st_read("~/MAPAS_R/naturalearth/ne_50m_ocean.shp")
oceano110<- st_read("~/MAPAS_R/naturalearth/ne_110m_ocean.shp")


 tm_shape(Batrimetria1) + tm_polygons(palette = "Blues")
tm_shape(oceano50) + tm_polygons()

azul1 <- tm_shape(temp) + tm_raster(palette = "Blues") + tm_legend(position = c("right", "bottom"))



