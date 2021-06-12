library(sp)
library(raster)
library(rgdal)
library(RStoolbox)
library(sf)

# 1. Lectura de imagenes satelitales --------------------------------------
setwd("C:/Users/Contreras/Desktop/CURSOS/APROGIS/Teledetecci�n_R/L1/data") # Directorio

# 1.1 Bandas --------------------------------------------------------------
# Son las bandas de Landsat 8 OLI

b2 <-raster("LC08_L1TP_232066_20180724_20180731_01_T1_B2.TIF")# blue
b3 <-raster("LC08_L1TP_232066_20180724_20180731_01_T1_B3.TIF")# green
b4 <-raster("LC08_L1TP_232066_20180724_20180731_01_T1_B4.TIF")# red
b5 <-raster("LC08_L1TP_232066_20180724_20180731_01_T1_B5.TIF")# nir
b6 <-raster("LC08_L1TP_232066_20180724_20180731_01_T1_B6.TIF")# swir1
b7 <-raster("LC08_L1TP_232066_20180724_20180731_01_T1_B7.TIF")# swir2


# 1.2 Apilado -------------------------------------------------------------
# Se hacen con brick tambien

apilado <-stack(b2,b3,b4,b5,b6,b7)
apilado

# 1.3 Visualizacion RGB ---------------------------------------------------
# Combinacion en color natural. Los numeros representan RGB segun index
# strech es un tipo de contraste


# 1.3.1 Color natural -----------------------------------------------------
plotRGB(apilado, r =  3, g = 2, b = 1, stretch="hist") 


# 1.3.2 Falso color ------------------------------------------------------
plotRGB(apilado, 5,4,3, stretch="lin")# Combinacion en falso color



# 1.4 Lectura de archivos MTL (RStoolbox) ---------------------------------
# Son metadatos que no dan las caracter�stica de las imagenes a leer

mtlFile <- "LC08_L1TP_232066_20180724_20180731_01_T1_MTL.txt" 
metaData <- readMeta(mtlFile)
lsat <- stackMeta(mtlFile)
plotRGB(lsat, 5,4,3, stretch="hist") # Falso color


# 2. Listar series temporales (caso NDVI) ----------------------------------------
# 1 y 2 arg son directorio y el patron. all.files cargar los archivos aunquese ocultos
# full.names = F para que solo nos de el nombre del archivo, no pegado al directorio

lista_ndvi <- list.files("st", # con directorio definido arriba
                       "_Clip.tif", all.files = T, recursive = T, full.names = T)

# Ploteo de la posicion 17 luego de cargarlo con raster()
plot(raster(lista_ndvi[[17]])) # Imagen MODIS de 250m de resolucion region sureste de Peru.



# 3. Cortar  --------------------------------------------------------------

# 3.1 Crop ----------------------------------------------------------------
# La peculiaridad de esta funcion es que realiza cortes a través de coordenadas
# cartesianas UTM X y Y , o geodésicas (latitud) y (longitud), y shapefile.

e <- extent(390895.5,424095.03,-982124.4,-952108.4) # (xmin, xmax, ymin, ymax)
apilado_corte <- crop(apilado, e) # Usamos la extension para cortar
plotRGB(apilado_corte, 5,4,3, stretch="hist", main = "Crop") # Ploteamos la imagen cortada


# 3.2 Mask ----------------------------------------------------------------

# Cargamos el shapefile (Con rgdal): Tanto shp como raster deben coincidir en proyeccion y datum
shp <- readOGR("shp", "corte_aleatorio")
shp_sf <- st_read("shp/corte_aleatorio.shp")
# st_crs(shp_sf)$proj4string
# crs(apilado)

# Realizamos el corte
apilado_corte_cp <- crop(apilado, extent(shp)) # Primero hacemos el crop
apilado_corte_msk <- mask(apilado_corte_cp, shp) # Aplicamos el area del shp

# Ploteamos el corte
plotRGB(apilado_corte_m2, 5,4,3, stretch="hist")


# 4. Calculos de Indices espectrales --------------------------------------

# 4.1 NDVI ----------------------------------------------------------------
# El NIR es la reflectancia del infrarrojo cercano b4 para TM/ETM+, b5 para OLI
# El RED es la reflectancia del infrarrojo cercano b3 para TM/ETM+, b4 para OLI
# El El rango de valores del NDVI esta entre -1 a 1.

nlayers(apilado_corte_msk) # Numero de layer (n° de bandas)
names(apilado_corte_msk) # Nombre de cada banda 

# Bandas de interés
red <- apilado_corte_msk[[3]]# red
nir <- apilado_corte_msk[[4]] # nir

# Calculamos NDVI
ndvi <- (nir - red)/(nir + red)

# Ploteamos el Indice
plot(ndvi, main = 'NDVI - LADNSAT 8 OLI', cex.axis = 0.6, cex.main=0.7)



# 4.2 Indice de vegetacion mejorado (EVI) ---------------------------------------
# Capta mejor la señal de vegetacion en zonas con alta biomasa y reduce influencias meteorologicas

# El NIR es la reflectancia del infrarrojo cercano b4 para TM/ETM+, b5 para OLI
# El RED es la reflectancia del infrarrojo cercano b3 para TM/ETM+, b4 para OLI
# El BLUE es la reflectancia azul b1 para TM/ETM+, b2 para OLI
# El G es el factor de ganancia
# El C1, C2 son los coeficientes del término de resistencia a aerosoles, 
#   que utiliza la banda azul para corregir las influencias de aerosoles en 
#   la banda roja y L funciona como el factor de ajuste del suelo
# El El rango de valores del EVI esta entre -1 a 1.


# Leemos las bandas de forma individual y los parametros
blue <- apilado_corte_msk[[1]] # blue
red <- apilado_corte_msk[[3]]# red
nir <- apilado_corte_msk[[4]] # nir

C1 <- 6; C2 <- 7.5; l <- 1; G <- 2.5 # Parametros de calculo

# Calculamos el EVI
evi <- G*(nir - red)/(nir + C1*red + C2*blue + l)

# Ploteamos el Indice
plot(evi, main = 'EVI - LADNSAT 8 OLI', cex.lab=0.6, cex.axis=0.6, cex.main=0.7)


# 4.3 Normalize Difference Water Index ( NDWI) ----------------------------
# El NDWI es muy usado en la deteccion de cuerpos de agua y zonas con presencia de humedad  

# El GREEN es la reflectancia del verde b2 para TM/ETM+, b3 para OLI
# El NIR es la reflectancia del infrarrojo cercano b4 para TM/ETM+, b5 para OLI
# El El rango de valores del NDWI esta entre -1 a 1.


# Leemos las bandas de forma individual
nir <- apilado_corte_msk[[4]]# nir
green <- apilado_corte_msk[[2]] # green

# Calculamos NDWI
ndwi <- (green - nir)/(green + nir)

# Ploteamos el Indice
plot(ndwi, col = rev(topo.colors(100)), main = 'NDWI - LADNSAT 8 OLI'
     ,cex.lab=0.6,cex.axis=0.6, cex.main=0.7)



# 5. Enmascaramientos -----------------------------------------------------


# 5.1 Mascara de cuerpos de agua -----------------------------------------

cuer_agu <- ndwi # hacemos una copia del NDWI para no modificar sus valores
cuer_agu[cuer_agu > 0.1]<-1 # > 0.1 seran cuerpos de agua
cuer_agu[cuer_agu <= 0.1]<-NA # <= 0.1 otros objetos
plot(cuer_agu, col = "blue", main="Cuerpos de Agua", cex.lab=0.6
     ,cex.axis=0.6,cex.main=0.6)


# Dos graficos en uno
plotRGB(apilado_corte_msk, 4,3,2, stretch="lin")
plot(cuer_agu, col = "blue", main="Cuerpos de Agua", cex.lab=0.6
     ,cex.axis=0.6,cex.main=0.6, add=TRUE, legend = F)


# 6. Sistemas de Coordenadas (Reproyeccion) ----------------------------------------------
# Lo haremos gracias a rgdal, se basa en la biblioteca PROJ.4.

# Estandares para describir a los sistemas de coordenadas:
# El PROJ.4, del que ya hemos hablado
# El WKT (texto conocido) es un formato estandarizado utilizado por muchos programas 
#   que se utilizan para definir el sistema de coordenadas del 
#   objeto vectorial (archivo “.prj” de un archivo de forma ESRI)
# El El EPSG (European Petroleum Survey Group) es un sistema de codificacion que asigna un codigo a
#   cada sistema de coordenadas




# 6.1 PROPIEDADES Y REPROYECCION (raster) ---------------------------------

# Cargamos la banda NIR
nir <- raster("L5003069_20090828_NIR.tif") # nir

# 6.1.2 Propiedades -------------------------------------------------------

# Clase de objeto
class(nir) # Raster Layer: una sola banda

# Nombre de elementos
slotNames(nir) 

# Resolucion espacial
res(nir)

# Accedes a los valores del objeto
mean(na.omit(getValues(nir)))

# Escala de grises
escala_grises<-gray.colors(256, # niveles de color
                           start = 0.0, # 0 es negro
                           end = 1.0, # 1 es blanco
                           gamma = 1.2, # correccion entre como una camara digital
                           # ve el mundo y como lo ven los ojos humanos
                           alpha = NULL) #Null=colors are not transparent
# Ploteamos la banda
plot(nir, col=escala_grises)



# 6.1.2 Reproyeccion ------------------------------------------------------

# Comando Projection
projection(nir)

crs(nir)
# La funcion "projecRaster"
nir_wgs<- projectRaster(nir, crs=CRS("+init=epsg:4326"), method = "bilinear")

# Ploteamos la banda reproyectada
plot(nir_wgs,col=escala_grises,cex.lab=0.6,cex.axis=0.6,cex.main=0.6)


# Visualizacion:
layout(matrix(1:2, ncol = 2))
plot(nir,  col = escala_grises, main = "NIR UTM - Zone 19")
plot(nir_wgs,  col = escala_grises, main = "NIR WGS84")

# 6.1.3 Guardar -----------------------------------------------------------

# Usamos la funcion writeRaster para exportar datos raster
ruta<-"C:/Users/Contreras/Desktop/CURSOS/APROGIS/Teledetecci�n_R/L1/data/guardados" # Directorio
img<-"L5003069_20090828_NIR_WGS.tif" # Nombre del archivo
writeRaster(nir_wgs,
            filename = paste(ruta,img,sep="/"),
            drivername="GTiff",
            overwrite=TRUE) # Sobrescribir (si ya existiera)


# 6.2 SRC Y REPROYECCION (Vector) -----------------------------------------

# Sistema de proyeccion
shp@proj4string

# Reroyeccion a otro sistema de coordenadas
shp_wgs <- spTransform(x = shp, CRSobj = CRS("+init=epsg:4326"))

# Verification
shp_wgs@proj4string
