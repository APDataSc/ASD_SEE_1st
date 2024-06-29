#***********************************************************************#
#         
#                     Análisis sociodemográfico con R
#                               Sesión 2
#            VIII Censo de Población y VII de Vivienda (CPV)
#
#   Porcentaje de viviendas particulares con disponibilidad de energía 
#               eléctrica proveniente de la red pública 
#
#         Creado por:               INEC Ecuador  
#         Fecha de creación:        07-10-2022
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
library(foreign) 
library(dplyr) 
library(haven) 

# 4. Importar base de datos ---------------------------------------------------- 
vivienda2022 <- fread("data/CPV_Vivienda_2022_Nacional.csv") 

# 5. Calcular indicadores------------------------------------------------------- 
vivienda2022[V01<=8 & V0201R==1 , VEE:=0]
vivienda2022[V01<=8 & V0201R==1 & V12==1, VEE := 1] 

# Indicador 
VEE = vivienda2022[V01<=8 & V0201R==1, .(round(mean (VEE *100, na.rm=T),1))]  

setnames(VEE, "V1", 
" Porcentaje de viviendas particulares con disponibilidad de energía eléctrica proveniente de la red pública") 


## Desagregaciones ---- 

### Geográfico/Territorial ---- 
# 1. Area 

VEEa= vivienda2022[V01<=8 & V0201R==1, round(mean (VEE *100, na.rm=T), 1), by=.(AUR)] 

setnames(VEEa, "V1", 
         "Porcentaje de viviendas particulares con disponibilidad de energía eléctrica proveniente de la red pública") 

# 2. Provincial 
VEEp= vivienda2022[V01<=8 & V0201R==1, round(mean (VEE *100, na.rm=T),1), by=.(I01)] 
setnames(VEEp, "V1", 
"Porcentaje de viviendas particulares con disponibilidad de energía eléctrica proveniente de la red pública") 

# 3. Cantonal 
VEEc= vivienda2022[V01<=8 & V0201R==1, round(mean (VEE *100, na.rm=T),1), by=.(I01, I02)] 
setnames(VEEc, 
         "V1", " Porcentaje de viviendas particulares con disponibilidad de energía eléctrica proveniente de la red pública") 

# 4. Parroquial 
VEEpr= vivienda2022[V01<=8 & V0201R==1, round(mean (VEE *100, na.rm=T),1), by=.(I01, I02, I03)] 
setnames(VEEpr, "V1", 
         "Porcentaje de viviendas particulares con disponibilidad de energía eléctrica proveniente de la red pública") 
