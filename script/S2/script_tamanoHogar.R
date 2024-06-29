#***********************************************************************#
#         
#                     Análisis sociodemográfico con R
#                               Sesión 2
#            VIII Censo de Población y VII de Vivienda (CPV)
#                    Promedio de personas por hogar 
#
#         Creado por:               INEC Ecuador  
#         Fecha de creación:        21-09-2022
#         Actualizado por:          Andrés Peña M.
#         Fecha de actualización:   29-06-2024
#         Institución:              El Colegio de México A.C.
#         Contacto:                 agpena@colmex.mx
#
#***********************************************************************#


# 1. Descargar bases de datos --------------------------------------------------  

# 2. Limpiar objetos -----------------------------------------------------------  

rm(list = ls())

# 3. Cargar librerias----------------------------------------------------------- 
library(data.table) 
library(openxlsx) 
library (foreign) 

# 4. Importar base de datos----------------------------------------------------- 
hogar2022 <- fread("data/CPV_Hogar_2022_Nacional.csv") 

# 5. Calcular indicadores------------------------------------------------------- 
HPM <- hogar2022[INH>=1, .(round(mean(H1303,na.rm=T), 1))] 

setnames (HPM, "V1", "Promedio de personas por hogar") 

## Desagregaciones ----
### Geográfico/Territorial ---- 

# 1. Área 
HPMa <-hogar2022[INH>=1,round(mean(H1303,na.rm=T),1), by=.(AUR)] 
setnames (HPMa, "V1", "Promedio de personas por hogar") 

# 2. Provincia 
HPMp <- hogar2022[INH>=1,round(mean(H1303,na.rm=T),1), by=.(I01)] 
setnames (HPMp, "V1", "Promedio de personas por hogar") 

# 3. Cantonal 
HPMc <-hogar2022[INH>=1,round(mean(H1303,na.rm=T),1), by=.(I01, I02)]

setnames (HPMc, "V1", "Promedio de personas por hogar") 

# 4. Parroquial 
HPMpr <-hogar2022[INH>=1,round(mean(H1303,na.rm=T),1), by=.(I01, I02, I03)] 
setnames (HPMpr, "V1", "Promedio de personas por hogar")
