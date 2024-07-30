#**************************************************************************************#
#**************************************************************************************#
#
#                          Análisis Sociodemográfico con R                        
#                        Sociedad Ecuatoriana de Estadística
#                              Mortalidad - WPP 2024
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

library(DemoTools)

mort <- fread(
  "https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Abridged_Medium_2024-2100.csv.gz") 


mort_sel <- mort[ SexID==1 & LocID==218 & Variant=="Medium" & Time==2024 ]  


# Tabla de Vida

MX.lifetable <- lt_abridged(nMx = mort_sel[ , mx],
                            Age = mort_sel[ , AgeGrpStart], AgeInt = mort_sel[ , AgeGrpSpan],
                            Sex = "m")


#*******************************************FIN****************************************#
#**************************************************************************************#