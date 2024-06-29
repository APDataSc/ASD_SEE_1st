#***********************************************************************#
#         
#                     Análisis sociodemográfico con R
#                               Sesión 2
#            VIII Censo de Población y VII de Vivienda (CPV)
#        Tasa de analfabetismo de la población de 15 años o más
#
#         Creado por:               INEC Ecuador  
#         Fecha de creación:        11-02-2022
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

# 4. Cargar bases de datos-------===--------------------------------------------  
poblacion2022 <- fread("data/CPV_Población_2022_Nacional.csv") 
     

# 5. Calcular indicadores------------------------------------------------------ 
 
poblacion2022[ is.na(P19) & P03>=5 & P17R %between% c(1,5) , P19:=9]

poblacion2022[ , I01:=str_pad(I01, 2, pad = "0")] 
poblacion2022[ , I02:=str_pad(I02, 2, pad = "0")] 
poblacion2022[ , I03:=str_pad(I03, 2, pad = "0")]

poblacion2022[ , id_provincia:=I01] 
poblacion2022[ , id_canton:=paste0(I01, I02,sep="")] 
poblacion2022[ , id_parroquia:=paste(I01, I02,I03, sep="")]  



## Nacional ----
poblacion2022[P03 >=15, Analf:=0] 
poblacion2022[P03 >=15 & P19==9, Analf:=9] 
poblacion2022[P03 >=15 & P17R %between% c(1,6) & P19==2, Analf:=1]  
TA = poblacion2022[P03>=15, .N, 
                   by = .(Analf)][ , freq:=round(100*(N/sum(N, na.rm = T)), 1)] 
TA = TA[Analf ==1, ][order(Analf)][ , Analf:=NULL] 

setnames(TA, c( "N","freq"), 
         c( "Número de personas analfabetas de 15 años o más",
            "Tasa de analfabetismo de la población de 15 años o más"))  


## Desagregaciones ----  

### Geográfico territorial ----  

# 1. Área  
TA_a = poblacion2022[P03>=15,.N, 
                     by = .(Analf,AUR)][ , freq:=round(100*(N/sum(N, na.rm = T)), 1), 
                                         by=.(AUR)] 
TA_a = TA_a[Analf ==1,][order(AUR,Analf)][,Analf:=NULL] 

setnames(TA_a, c("AUR", "N","freq"), 
         c( "Área", "Número de personas analfabetas de 15 años o más",
            "Tasa de analfabetismo de la población de 15 años o más"))  


# 2. Provincia  
TA_P = poblacion2022[P03>=15, .N, 
                     by = .(Analf,id_provincia)][ ,
                                                  freq:=round(100*(N/sum(N, na.rm = T)),1), 
                                                             by=.(id_provincia)] 
TA_P = TA_P[Analf ==1,][order(id_provincia,Analf)][,Analf:=NULL] 
setnames(TA_P, c("id_provincia", "N","freq"), c( "Provincia", 
                                                 "Número de personas analfabetas de 15 años o más",
                                                 "Tasa de analfabetismo de la población de 15 años o más")) 

# 3. Cantonal  
TA_c = poblacion2022[P03>=15,.N, 
                     by = .(Analf,id_canton)][ , 
                                               freq:=round(100*(N/sum(N, na.rm = T)),1), 
                                                          by=.(id_canton)] 
TA_c = TA_c[Analf ==1,][order(id_canton,Analf)][,Analf:=NULL] 

setnames(TA_c, c("id_canton", "N","freq"), 
         c( "Cantón", "Número de personas analfabetas de 15 años o más",
            "Tasa de analfabetismo de la población de 15 años o más"))  

# 4. Parroquial   
TA_pr = poblacion2022[P03>=15,.N, 
                      by = .(Analf,id_parroquia)][ ,
                                                   freq:=round(100*(N/sum(N, na.rm = T)),1), 
                                                              by=.(id_parroquia)] 
TA_pr = TA_pr[Analf ==1,][order(id_parroquia, Analf)][,Analf:=NULL] 

setnames(TA_pr, c("id_parroquia", "N","freq"), 
         c( "Parroquia", "Número de personas analfabetas de 15 años o más",
            "Tasa de analfabetismo de la población de 15 años o más"))   


### Socio Demográfico/Económico ----

# 1. Sexo   
TA_s = poblacion2022[P03>=15,.N, 
                     by = .(Analf, P02)][ ,
                                         freq:=round(100*(N/sum(N, na.rm = T)),1), by=.(P02)] 

TA_s = TA_s[Analf ==1,][order(P02,Analf)][,Analf:=NULL] 

setnames(TA_s, c("P02", "N","freq"), 
         c( "Sexo", "Número de personas analfabetas de 15 años o más",
            "Tasa de analfabetismo de la población de 15 años o más"))   


# 2. Grupos de edad  
poblacion2022 <- poblacion2022[,G_edad := fcase(P03>=15 & P03 <=19, "15 a 19",                     
                                                P03>=20 & P03 <=24, "20 a 24",         
                                                P03>=25 & P03 <=29, "25 a 29",      
                                                P03>=30 & P03 <=34, "30 a 34",                  
                                                P03>=35 & P03 <=39, "35 a 39",           
                                                P03>=40 & P03 <=44, "40 a 44",           
                                                P03>=45 & P03 <=49, "45 a 49",            
                                                P03>=50 & P03 <=54, "50 a 54",            
                                                P03>=55 & P03 <=59, "55 a 59",           
                                                P03>=60 & P03 <=64, "60 a 64",           
                                                P03>=65 & P03 <=69, "65 a 69",         
                                                P03>=70 & P03 <=74, "70 a 74",              
                                                P03>=75 & P03 <=79, "75 a 79",          
                                                P03>=80 & P03 <=84, "80 a 84",          
                                                P03>=85, " 85 o más")]  

TA_e = poblacion2022[P03>=15,.N, 
                     by = .(Analf,G_edad)][ ,
                                            freq:=round(100*(N/sum(N, na.rm = T)),1), 
                                                       by=.(G_edad)] 

TA_e = TA_e[Analf ==1,][order(G_edad,Analf)][,Analf:=NULL] 

setnames(TA_e, c("G_edad", "N","freq"), 
         c( "Grupos quinquenales de edad", 
            "Número de personas analfabetas de 15 años o más",
            "Tasa de analfabetismo de la población de 15 años o más"))   


# 3. Autoidentificación según cultura y costumbres  
TA_ac = poblacion2022[P03>=15,.N, 
                      by = .(Analf,P11R)][ ,
                                           freq:=round(100*(N/sum(N, na.rm = T)),1), 
                                           by=.(P11R)] 

TA_ac = TA_ac[Analf ==1,][order(P11R,Analf)][,Analf:=NULL] 

setnames(TA_ac, c("P11R", "N","freq"), c( "Autoidentificación étnica", 
                                          "Número de personas analfabetas de 15 años o más",
                                          "Tasa de analfabetismo de la población de 15 años o más"))
