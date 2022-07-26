
##############################################################
#                 Clase 02: Introduccion a datos Raster en R #
#                     Año: 2022                              #
#                       Gorky Florez Castillo                #
#                  Parte 2: Datos raster                     #
##############################################################

#Librerias----------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(raster)
library(ggspatial)
library(cptcity)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(grid)
library(RStoolbox)


SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Peru          <- getData('GADM', country='Peru', level=0) %>% st_as_sf()
PeRR           <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
Per           <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
SMT           <- subset(Per, NAME_1  == "San Martín")
Amaz          <- subset(Per, NAME_1  == "Amazonas")
Cajamarca     <- subset(Per, NAME_1  == "Cajamarca")

Plot_merge =st_union(SMT, Amaz)
Total_merge =st_union(Plot_merge,Cajamarca )

library(elevatr)
elev = get_elev_raster(Total_merge, z=9)
Poligo_alt    <- crop(elev, Total_merge)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Total_merge)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)
colores = c( 
            "#8e9aaf",#celeste
            "#dda15e", # maroon 
            "#faedcd")#amarillo pastel


Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")

SurA= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Peru , fill="gray", color="black")+
  geom_sf(data = SMT, fill="black", color="black")+
  geom_sf(data = Amaz, fill="black", color="black")+
  geom_sf(data = Cajamarca, fill="black", color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 10, hjust = 0, vjust = 1, 
           label = "a) Sur America",size = 3, family="serif", color = 
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1, 
           label = "Pacific ocean",size = 3, family="serif", color = 
             "black",  fontface="italic", angle=90)+
  annotate(geom = "text", x = -55, y = -50, hjust = 0, vjust = 1, 
           label = "Atlantic ocean",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -10, hjust = 0, vjust = 1, 
           label = "Peru",size = 3, family="serif", color = 
             "black",  fontface="italic")
SurA

library(ggnewscale) 
ElevGG=ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6, 
              show.legend = T)+
  scale_fill_gradientn(colours = colores, 
                       name='Elevacion \n(msnm)') +
  geom_sf(data = SMT , fill=NA, color="black")+
  geom_sf(data = Amaz, fill=NA, color="black")+
  geom_sf(data = Cajamarca, fill=NA, color="black")+
  geom_sf_text(data = SMT, aes(label=NAME_1),fontface="italic", 
               family="serif",size = 3)+
  geom_sf_text(data = Amaz, aes(label=NAME_1),fontface="italic", 
               family="serif",size = 3)+
  geom_sf_text(data = Cajamarca, aes(label=NAME_1),fontface="italic", 
               family="serif",size = 3)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))


Elev_Caja=ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6, 
              show.legend = T)+
  scale_fill_gradientn(colours = colores, 
                       name='Elevacion \n(msnm)',
                       breaks = c(0,500,1000,2000,3000,4000,5000),
                       labels = c("[0 - 499] ","[500 - 999]",
                                  "[1000 - 1999]", "[2000 - 2999]", 
                                  "[3000 - 3999]", "[4000 - 4999]",
                                  "[5000 - 5999]")) +
  geom_sf(data = SMT , fill=NA, color="black")+
  geom_sf(data = Amaz, fill=NA, color="black")+
  geom_sf(data = Cajamarca, fill=NA, color="black")+
  geom_sf_text(data = SMT, aes(label=NAME_1),fontface="italic", 
               family="serif",size = 3)+
  geom_sf_text(data = Amaz, aes(label=NAME_1),fontface="italic", 
               family="serif",size = 3)+
  geom_sf_text(data = Cajamarca, aes(label=NAME_1),fontface="italic", 
               family="serif",size = 3)+
  theme(legend.position = c(0.8, 0.8),
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(title = '', fill = '',  x = 'Longitud', y = 'Latitud') +
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))
  
 
Local= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6, 
              show.legend = T)+
  scale_fill_gradientn(colours = colores, 
                       name='Elevacion \n(msnm)',
                       breaks = c(0,500,1000,2000,3000,4000,5000),
                       labels = c("[0 - 499] ","[500 - 999]",
                                  "[1000 - 1999]", "[2000 - 2999]", 
                                  "[3000 - 3999]", "[4000 - 4999]",
                                  "[5000 - 5999]")) +
  geom_sf(data = SMT , fill=NA, color="black")+
  geom_sf(data = Amaz, fill=NA, color="black")+
  geom_sf(data = Cajamarca, fill=NA, color="black")+
  geom_sf_text(data = SMT, aes(label=NAME_1),fontface="italic", 
               family="serif",size = 3)+
  geom_sf_text(data = Amaz, aes(label=NAME_1),fontface="italic", 
               family="serif",size = 3)+
  geom_sf_text(data = Cajamarca, aes(label=NAME_1),fontface="italic", 
               family="serif",size = 3)+
  theme(legend.position = "none",
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(title = '', fill = '',  x = 'Longitud', y = 'Latitud') +
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  coord_sf(xlim = c(-79, -76), ylim = c(-8,-5)) 

  
# Mapa final
library(cowplot)
Final=ggdraw() +
  coord_equal(xlim = c(0, 22.86), ylim = c(0, 17.78), expand = FALSE) +
  draw_plot(SurA, width = 7, height = 7,x = -1.3, y = 10.8)+
  draw_plot(ElevGG, width = 7.5, height = 7,x = 3, y = 10.8)+
  draw_plot(Elev_Caja, width = 18.5, height = 18.5,x = 6.8, y = 0.01)+
  draw_plot(Local, width = 10, height = 10,x = -0.5, y = 0.001)+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "white", fill = NA, size = 1))+
  annotate(geom = "text", x = 8, y = 3, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo                       Gorky Florez Castillo                        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)

Final

ggsave(plot=Final,"Mapa/Mapa de Elevacion1.png",units = "cm",width = 22.86, #alto
       height = 17.78, #ancho
       dpi=1200)



























