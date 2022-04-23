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

install.packages("forcats")
library(forcats)
Entidades$Entidad <- fct_recode(Entidades$Entidad,
                                "Distrito Federal"= "Ciudad de México")
                                
install.packages("tmap", dependencies = T)
install.packages("sf", dependencies = T)
install.packages("extrafont")

library(tmap)      # ElaboraciÃ³n de mapas temÃ¡ticos <3!
library(sf)        # Manejo de datos vectoriales
library(extrafont) # Diferentes tipos de letras

Shape_entidades<- st_read("México_Estados.shp")

Shape_entidades <- merge(Shape_entidades, Entidades, by.x="ESTADO", by.y= "Entidad", all.x=TRUE)

##Construcción del mapa##
tm_shape(Shape_entidades) + tm_polygons()

tm_shape(Shape_entidades) + 
  tm_polygons(col = 'mean_comor',
              title = 'Prevalencia(%)',
              title.size=2,
              palette = 'viridis',
              style = "quantile",
              )+
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
            legend.text.size = 0.6)+
  tm_text('ESTADO',
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


##https://www.youtube.com/watch?v=NQZNpyEgVss
##https://www.youtube.com/watch?v=wc9lKDStnmE##









