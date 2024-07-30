#**************************************************************************************#
#**************************************************************************************#
#
#                          Análisis Sociodemográfico con R                        
#                        Sociedad Ecuatoriana de Estadística
#                              Fecundidad - WPP 2024
#
#         Creado por:               Andrés Peña M.  
#         Fecha de creación:        11-07-2024
#         Actualizado por:          Andrés Peña M.
#         Fecha de actualización:   13-07-2024
#         Institución:              El Colegio de México A.C.
#         Contacto:                 agpena@colmex.mx
#
#**************************************************************************************#
#**************************************************************************************#

library(data.table)
library(R.utils)
library(dplyr)
library(ggplot2)
library(data.table)


# Población total ----

fec <- fread(
  "https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Fertility_by_Age5.csv.gz") 

# table(fec$AgeGrpStart)

fec_sel <- fec[ LocID==218 & Variant=="Medium" ]  
  
  
pop <- fread("https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_PopulationByAge5GroupSex_Medium.csv.gz")

pop_sel <- pop[ LocID==218 & Variant=="Medium" ]



# Tasa Global de Fecundidad ----

tgf <- left_join(fec_sel, pop_sel[ , .(Time, AgeGrpStart, PopFemale)], by=c("Time", "AgeGrpStart"))

tgf <- tgf[ , tef := Births/PopFemale*1000]


ggplot(data=tgf, aes(x=AgeGrp, y=tef, 
                     group=Time, color=Time)) +
  geom_line() + geom_point() +
  theme_minimal() +
  geom_vline(xintercept = 2024, linetype = 'dashed', color = 'red', size = 0.54, alpha = 0.70)


tgf <- tgf[ , mc := AgeGrpStart + 2.5][ , mc_tef := mc*tef] 

em <- tgf[ , .(em = sum(mc_tef)/sum(tef)), .(Time)]  

plot(em$Time, em$em)


#*****************************************FIN******************************************#
#**************************************************************************************#