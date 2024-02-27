library(readxl)
library(data.table)
library(sf)
library(tmap)
library(stringdist)

# Datos de cortas
datos <- read_excel("Data/Aproveitamentos_xestion_privada_por_concello_2019.xlsx")
datos <- as.data.table(datos)

datos[CONCELLO == "Cesuras" | 
        CONCELLO == "Oza dos Ríos", "CONCELLO"] <- "Oza-Cesuras"
datos[CONCELLO == "Cerdedo" | 
        CONCELLO == "Cotobade", "CONCELLO"] <- "Cerdedo-Cotobade"

datos.concellos <- datos[,.(volume = sum(VOLUME_M3)), .(CONCELLO)]

# Capas auxiliares
muni <- st_read("../LimitesAdmin/IGN/SHP_ETRS89/recintos_municipales_inspire_peninbal_etrs89/")
muni <- muni[muni$CODNUT2 == "ES11",]
muni <- st_simplify(muni, preserveTopology = TRUE, dTolerance = 200)

ccaa <- st_read("/home/edujose/Traballo/Recursos/Datos_Cartografia/LimitesAdmin/IGN/SHP_ETRS89/recintos_autonomicas_inspire_peninbal_etrs89/") 
pt <- st_read("~/Traballo/Recursos/Datos_Cartografia/LimitesAdmin/Portugal/PRT_adm0.shp")
ccaa <- st_simplify(ccaa, preserveTopology = TRUE, dTolerance = 200)
pt <- st_simplify(pt, preserveTopology = TRUE, dTolerance = 200)

muni.arb <- st_read("Data/sup_arb_concellos.gpkg")
muni.arb$prop.arb <- 100*(muni.arb$p_arb_sum/16e2)/muni.arb$area_ha

# Busca de correspondencias entre nomes de municipios
# 1. Simplificamos os nomes
muni$concello2 <- gsub("(A )|(As )|(O )", "", muni$NAMEUNIT)
datos.concellos$concello2 <- gsub(" \\(.\\)", "", datos.concellos$CONCELLO)

corresp <- amatch(muni$concello2,
                  datos.concellos$concello2,
                  method = "jw",
                  maxDist = 4)

# (Comprobación)
# corresp2 <- data.frame(shape = muni$NAMEUNIT,
#                        datos = datos.concellos$CONCELLO[corresp])
# View(corresp2)

muni$volume.cortas <- datos.concellos$volume[corresp]



# Representación
mapa1 <- tm_shape(muni) + 
  tm_polygons(col = "volume.cortas",
              lwd = .5,
              style = "cont",
              convert2density = TRUE,
              palette = "viridis",
              title = "Densidade de cortas (m³/km²)",
              legend.hist = TRUE) +
  tm_shape(ccaa[ccaa$CODNUT2 != "ES11",]) + tm_polygons(lwd = .5, col = "lightgrey") +
  tm_shape(pt)   + tm_polygons(lwd = .5, col = "lightgrey") +
  tm_layout(legend.format = list(text.separator = "a",
                                 fun=function(x) formatC(x, digits=0, format="d")),
            legend.outside = TRUE,
            main.title = "Aproveitamentos en montes de xestión privada (2019)",
            main.title.size = 1.2) 


mapa2 <- tm_shape(muni) + 
  tm_polygons(col = "volume.cortas",
              lwd = .5,
              style = "fixed",
              breaks = c(0,250,500,1000,2000),
              convert2density = TRUE,
              palette = "viridis",
              title = "Densidade de cortas (m³/km²)",
              legend.hist = TRUE) +
  tm_shape(ccaa[ccaa$CODNUT2 != "ES11",]) + tm_polygons(lwd = .5, col = "lightgrey") +
  tm_shape(pt)   + tm_polygons(lwd = .5, col = "lightgrey") +
  tm_layout(legend.format = list(text.separator = "a",
                                 fun=function(x) formatC(x, digits=0, format="d")),
            legend.outside = TRUE,
            main.title = "Aproveitamentos en montes de xestión privada (2019)",
            main.title.size = 1.2) 



## adenda: volume de cortas por hectárea arborada
muni2 <- merge(muni, st_drop_geometry(muni.arb), by = "codigoine", all = TRUE)

muni2$cortas_m3ha <- muni2$volume.cortas / (0.01* muni2$prop.arb * muni2$area_ha)

mapa3 <- tm_shape(muni2) + 
  tm_polygons(col = "cortas_m3ha",
              lwd = .5,
              style = "cont",
              convert2density = FALSE,
              palette = "viridis",
              title = "Densidade de cortas (m³/ha arborada)",
              legend.hist = TRUE) +
  tm_shape(ccaa[ccaa$CODNUT2 != "ES11",]) + tm_polygons(lwd = .5, col = "lightgrey") +
  tm_shape(pt)   + tm_polygons(lwd = .5, col = "lightgrey") +
  tm_layout(legend.format = list(text.separator = "a",
                                 fun=function(x) formatC(x, digits=0, format="d")),
            legend.outside = TRUE,
            main.title = "Aproveitamentos en montes de xestión privada (2019)",
            main.title.size = 1.2) 


mapa4 <- tm_shape(muni2) + 
  tm_polygons(col = "cortas_m3ha",
              lwd = .5,
              style = "fixed",
              breaks = c(0,6,12,24,36),
              convert2density = FALSE,
              palette = "viridis",
              title = "Densidade de cortas (m³/ha arborada)",
              legend.hist = TRUE) +
  tm_shape(ccaa[ccaa$CODNUT2 != "ES11",]) + tm_polygons(lwd = .5, col = "lightgrey") +
  tm_shape(pt)   + tm_polygons(lwd = .5, col = "lightgrey") +
  tm_layout(legend.format = list(text.separator = "a",
                                 fun=function(x) formatC(x, digits=0, format="d")),
            legend.outside = TRUE,
            main.title = "Aproveitamentos en montes de xestión privada (2019)",
            main.title.size = 1.2) 


## Saída ----

pdf("Cortas_concellos_2019.pdf", width = 15/2.54, height = 10/2.54)
print(mapa1)
print(mapa2)
print(mapa3)
print(mapa4)
dev.off()

svg("Cortas_concellos_2019_1.svg", width = 15/2.54, height = 10/2.54)
print(mapa1)
dev.off()

svg("Cortas_concellos_2019_2.svg", width = 15/2.54, height = 10/2.54)
print(mapa2)
dev.off()

svg("Cortas_concellos_2019_3.svg", width = 15/2.54, height = 10/2.54)
print(mapa3)
dev.off()

svg("Cortas_concellos_2019_4.svg", width = 15/2.54, height = 10/2.54)
print(mapa4)
dev.off()
