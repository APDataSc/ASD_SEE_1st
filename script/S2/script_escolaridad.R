#***********************************************************************#
#         
#                     Análisis sociodemográfico con R
#                               Sesión 2
#            VIII Censo de Población y VII de Vivienda (CPV)
#                    Años promedio de escolaridad 
#
#         Creado por:               INEC Ecuador  
#         Fecha de creación:        14-11-2022
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
library(readr) 
library(haven) 
library(foreign) 
library(data.table) 
library(foreign) 
library(stringr)
library(openxlsx) 
library(labelled) 
library(dplyr) 

# 4. Importar bases de datos---------------------------------------------------- 
poblacion2022 <- read_sav("Censo2022_Poblacion.sav") 
poblacion2022 <- as.data.table(poblacion2022) 

# 5. Calcular indicadores------------------------------------------------------ 

# dado que todos deben contestar la pregunta 17, recodifico NA con 99 
# dado que todas las personas que hayan contestado la pregunta 17 desde el código 3 
# deben contestar la pregunta 18, recodifico NA con 99 

poblacion2022[is.na(P17R), P17R:=99] 
poblacion2022[is.na(P18R) & P17R>=3 ,P18R:=99] 

poblacion2022[, I01:=str_pad(I01, 2, pad = "0")] 
poblacion2022[, I02:=str_pad(I02, 2, pad = "0")] 
poblacion2022[, I03:=str_pad(I03, 2, pad = "0")] 

poblacion2022[,id_provincia:=I01] 
poblacion2022[,id_canton:=paste0(I01, I02,sep="")] 
poblacion2022[,id_parroquia:=paste(I01, I02,I03, sep="")] 

 
poblacion2022[,ANIOS:=fcase (P17R==1, 0, P17R==2, 0, P17R==3, 0,        
                             P17R==4 & P18R==0, 0, P17R==4 & P18R==1, 0, 
                             P17R==4 & P18R==2, 3, #Alfabetización Módulo 1y2, 2do–3ro EGB 
                             P17R==4 & P18R==3, 3, P17R==4 & P18R==4, 5, #Alfabetización Módulo 3y4, 4to–5to EGB 
                             P17R==4 & P18R==5, 5, P17R==4 & P18R==6, 7, #Alfabetización Módulo 5y6, 6to–7mo EGB 
                             P17R==4 & P18R %in% c(7:10),99, 
                             P17R==5 & P18R!=99,0+P18R, P17R==6 & P18R!=99,10+P18R, 
                             P17R==7 & P18R!=99,13+P18R, P17R==8 & P18R!=99,13+P18R, 
                             P17R==9 & P18R!=99,13+P18R, P17R==10 & P18R!=99,18+P18R, 
                             P17R==11 & P18R!=99,20+P18R,                             
                             P17R==99 | P18R==99,99)] 

poblacion2022[((P03>=24) & (P18R<=10 | is.na(P18R)) & P17R!=99 & ANIOS!=99), EM:=ANIOS] 


# Nacional 
APE= as.data.table(poblacion2022[((P03>=24) & EM!=99), round(mean(EM,na.rm=T),1)])  

setnames(APE, "V1", "Anios promedio de escolaridad") 


## Desagregaciones ---- 

### Geográfico territorial ----

# 1. Área 
APE_a= poblacion2022[((P03>=24) & EM!=99), round(mean(EM,na.rm=T),1), by = .(AUR)]  
setnames(APE_a, c("AUR","V1"), c("Área","Anios promedio de escolaridad")) 

# 2. Provincia 
APE_P= poblacion2022[((P03>=24) & EM!=99), round(mean(EM,na.rm=T),1), by = .(id_provincia)]  
setnames(APE_P, c("id_provincia","V1"), c("Provincia","Anios promedio de escolaridad")) 

# 3. Cantonal 
APE_c= poblacion2022[((P03>=24) & EM!=99), round(mean(EM,na.rm=T),1), by = .(id_canton)]  
setnames(APE_c, c("id_canton","V1"), c("Cantón","Anios promedio de escolaridad")) 

# 4. Parroquial 
APE_pr= poblacion2022[((P03>=24) & EM!=99), round(mean(EM,na.rm=T),1), by = .(id_parroquia)]  
setnames(APE_pr, c("id_parroquia","V1"), c("Parroquia","Anios promedio de escolaridad")) 


### Socio Demográfico/Económico ----

# 1. Sexo 

APE_s= poblacion2022[((P03>=24) & EM!=99),round(mean(EM,na.rm=T),1), by = .(P02)]  
setnames(APE_s, c("P02","V1"), c("Sexo","Anios promedio de escolaridad")) 

# 2. Autoidentificación según cultura y costumbres 
APE_ac= poblacion2022[((P03>=24) & EM!=99),round(mean(EM,na.rm=T),1), by = .(P11R)]  
setnames(APE_ac, c("P11R","V1"), c("Autoidentificación","Anios promedio de escolaridad"))
