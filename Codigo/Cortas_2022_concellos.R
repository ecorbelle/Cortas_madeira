# Cortas por grupos de especies a nivel municipal, Galicia, 2022
# Datos: Observatorio Forestal de Galicia (https://ovmediorural.xunta.gal/gl/consultas-publicas/observatorio-forestal )
# Eduardo Corbelle, 27 febreiro 2024

library(readODS)
library(data.table)
library(sf)
library(stringdist)
library(tmap)

# Datos de cortas ----
datos <- read_ods("Datos/Municipios/IOBSFOR_BI_072_INT_2022.ods",
                  sheet = "3 Por concello",
                  range = "A3:J318",
                  na = "---") |> 
  as.data.table() |> 
  setnafill(type = "const", fill = 0, cols = 2:10)
datos[, ':='(`Coníferas` = `Pinus pinaster` + `Pinus radiata` + 
               `Pinus sylvestris` + `Outras / Mistura Coníferas`,
             `Eucalipto` = `Eucalyptus spp`,
             `Outras`= `Frondosas caducifolias` + `Outras frondosas non caducifolias` + `Outras`)]

# Capas auxiliares ----
muni <- st_read("../LimitesAdmin/Galicia/Municipios_Galicia.gpkg", layer = "municipios315")
muni <- st_simplify(muni, preserveTopology = TRUE, dTolerance = 200)

ccaa <- st_read("/home/edujose/Traballo/Recursos/Datos_Cartografia/LimitesAdmin/IGN/SHP_ETRS89/recintos_autonomicas_inspire_peninbal_etrs89/") 
pt <- st_read("~/Traballo/Recursos/Datos_Cartografia/LimitesAdmin/Portugal/PRT_adm0.shp")
ccaa <- st_simplify(ccaa, preserveTopology = TRUE, dTolerance = 200)
pt <- st_simplify(pt, preserveTopology = TRUE, dTolerance = 200)

# Busca de correspondencias entre nomes de municipios ----
# 1. Simplificamos os nomes
muni$concello2 <- gsub("(A )|(As )|(O )", "", muni$NAMEUNIT)
datos$concello2 <- gsub(" \\(.\\)", "", datos$Var.1)

corresp <- amatch(muni$concello2,
                  datos$concello2,
                  method = "jw",
                  maxDist = 4)

# (Comprobación)
# corresp2 <- data.frame(shape = muni$NAMEUNIT,
#                        datos = datos$Var.1[corresp])
# View(corresp2)

muni2 <- cbind(muni[        , c("NATCODE", "NAMEUNIT")],
               datos[corresp, c("Coníferas", "Eucalipto", "Outras", "TOTAL")]) |> 
  data.table()

muni.long <- melt(muni2,
                  id.vars = c("NAMEUNIT", "NATCODE", "geom"),
                  variable.name = "especies",
                  value.name = "volume",
                  na.rm = FALSE) |> 
  st_as_sf(sf_column_name = "geom")
muni.long$especies <- factor(muni.long$especies, 
                             levels = c("TOTAL", "Coníferas", "Eucalipto", "Outras"))

# Representación ----
mapa2 <- tm_shape(muni.long) + 
  tm_polygons(col = "volume",
              lwd = .5,
              style = "fixed",
              breaks = c(0,125,250,500,1000,1600),
              convert2density = TRUE,
              palette = "BuPu",
              title = "Densidade\n(m³/km²)") +
  tm_facets(by = "especies") +
  tm_shape(ccaa[ccaa$CODNUT2 != "ES11",]) + tm_polygons(lwd = .5, col = "lightgrey") +
  tm_shape(pt)   + tm_polygons(lwd = .5, col = "lightgrey")  +
  tm_layout(legend.format = list(text.separator = "a",
                                 fun=function(x) formatC(x, digits=0, format="d")),
            legend.outside = TRUE,
            main.title = "Densidade de cortas por grupos de especies, 2022",
            main.title.size = 1.2, 
            attr.outside = TRUE,
            attr.position = c(0, .05)) +
  tm_credits(text = "Eduardo Corbelle, 2024. CC-BY 4.0\nDatos: Sistema de indicadores da administración forestal, Xunta de Galicia")

## Saída ----
png("Cortas2.png", width = 15, height = 10, units = "cm", res = 150)
print(mapa2)
dev.off()
