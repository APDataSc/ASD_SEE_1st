#**************************************************************************************#
#**************************************************************************************#
#
#                          Análisis Sociodemográfico con R                        
#                        Sociedad Ecuatoriana de Estadística
#                       Estructura de la Población - WPP 2024
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

pob_tot <- fread(
  "https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_TotalPopulationBySex.csv.gz") 


cont <- pob_tot[(LocID==904 | LocID==905 | LocID==908) & Variant=="Medium"]
cont <- pob_tot[(LocID==904 | LocID==903 | LocID==905 | LocID==908 | LocID==935) & Variant=="Medium"]

# cont <- pob_tot[(LocID==904 | LocID==903 | LocID==905 | LocID==908 | LocID==935) & Variant=="Medium"]
# cont <- pob_tot[(LocID==900 | LocID==904 | LocID==903 | LocID==905 | LocID==908 | LocID==935) & Variant=="Medium"]

ggplot(data=cont, aes(x=Time, y=PopTotal, group=Location, color=Location)) +
  geom_line() + geom_point() +
  scale_color_brewer(palette="Paired") +
  theme_minimal() +
  geom_vline(xintercept = 2024, linetype = 'dashed', color = 'red', size = 0.54, alpha = 0.70)



# Pirámide de Ecuador ----

pop_age <- fread(
  "https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Population1JanuaryByAge5GroupSex_Medium.csv.gz") 

  
pais <- pop_age[LocID==218 & Variant=="Medium"]


pais %>% 
  filter(Time==2020) %>% 
  ggplot(
    aes(AgeGrpStart, weight=PopTotal)
  ) + geom_histogram(binwidth = 5)


pais %>% 
  filter(Time==2020) %>% 
  ggplot(
    aes(AgeGrpStart, weight=PopTotal)
  ) + geom_histogram(binwidth = 5)  + # verificar el ancho de clase
  coord_flip()


# Pirámide

pais <- pais[ , .(Time, AgeGrpStart, PopMale, PopFemale)]

long <- melt(pais, id.vars = c("Time","AgeGrpStart"), variable.name = "Sex", 
             value.name = "Pop")

long %>% 
  filter(Time==2020) %>% 
  mutate(Pop= if_else(Sex=="PopMale", -Pop, Pop)) %>% 
  ggplot(aes(AgeGrpStart, fill=Sex, weights=Pop))+
  geom_bar() # dibuja la geometría de barra

long %>% 
  filter(Time==1950) %>% 
  mutate(Pop= if_else(Sex=="PopMale", -Pop, Pop)) %>% 
  ggplot(aes(AgeGrpStart, fill=Sex, weights=Pop))+
  geom_bar() + coord_flip() +
  scale_fill_brewer(palette = "Set2") + 
  theme_light() +   theme(axis.text.x  =  element_text(angle = 90)) 



long %>% 
  filter(Time==2024) %>% 
  mutate(Pop= if_else(Sex=="PopMale", -Pop, Pop)) %>% 
  ggplot(aes(AgeGrpStart, fill=Sex, weights=Pop))+
  geom_bar() + coord_flip() +
  scale_y_continuous(breaks = seq(-900, 900, by=100), # cuántos 
                     limits = c(-900,900),
                     labels = paste0(
                       as.character(c(9:0,# sustituye negativos
                                      1:9) # Para lo positivo 
                       ) 
                     ) 
  ) + 
  labs(y="Poblacion - millones", x="Grupos de edad") +
  scale_fill_brewer(palette = "Set2") + 
  theme_light() +   theme(axis.text.x  =  element_text(angle = 90)) 



long %>% 
  filter(Time %in% seq(1950, 2100, by=10)) %>%
  mutate(Pop= if_else(Sex=="PopMale", -Pop, Pop)) %>% 
  ggplot(aes(AgeGrpStart, fill=Sex, weights=Pop))+
  geom_bar() + coord_flip() +
  scale_y_continuous(breaks = seq(-900, 900, by=100), # cuántos 
                     limits = c(-900,900),
                     labels = paste0(
                       as.character(c(9:0,# sustituye negativos
                                      1:9) # Para lo positivo 
                       ) 
                     ) 
  ) + 
  labs(y="Poblacion - millones", x="Grupos de edad") +
  scale_fill_brewer(palette = "Set2") + 
  theme_light() +   theme(axis.text.x  =  element_text(angle = 90)) +
  facet_wrap(~Time)


# Pirámide de varios países ----

paises <- pop_age[(LocID==218 | LocID==484 | LocID==320 | LocID==152) & Variant=="Medium",
                  .(Location, Time, AgeGrpStart, PopMale, PopFemale)]

long <- data.table::melt(paises, id.vars = c("Location", "Time","AgeGrpStart"), variable.name = "Sex", 
             value.name = "Pop")

long %>% 
  filter(Time == 2024) %>%
  mutate(Pop= if_else(Sex=="PopMale", -Pop, Pop)) %>% 
  ggplot(aes(AgeGrpStart, fill=Sex, weights=Pop))+
  geom_bar() + coord_flip() +
  scale_y_continuous(breaks = seq(-6000, 6000, by=1000), # cuántos 
                     limits = c(-6000,6000),
                     labels = paste0(
                       as.character(c(6:0,# sustituye negativos
                                      1:6) # Para lo positivo 
                       ) 
                     ) 
  ) + 
  labs(y="Poblacion - millones", x="Grupos de edad") +
  scale_fill_brewer(palette = "Set2") + 
  theme_light() +   theme(axis.text.x  =  element_text(angle = 90)) +
  facet_wrap(~Location, scales = "free_y")


long[ , Pop_tot := sum(Pop), .(Location, Time)] %>% 
  filter(Time == 2024) %>%
  mutate(Pop= if_else(Sex=="PopMale", -Pop/Pop_tot, Pop/Pop_tot)) %>% 
  ggplot(aes(AgeGrpStart, fill=Sex, weights=Pop))+
  geom_bar() + coord_flip() +
  scale_y_continuous(breaks = seq(-0.06, 0.06, by=0.01), # cuántos
                     limits = c(-0.06, 0.06),
                     labels = paste0(
                       as.character(c(seq(0.06, 0, by=-0.01),# sustituye negativos
                                      seq(0.01, 0.06, by=0.01)) # Para lo positivo
                       )
                     )
  ) +
  labs(y="Población - proporción", x="Grupos de edad") +
  scale_fill_brewer(palette = "Set2") + 
  theme_light() +   theme(axis.text.x  =  element_text(angle = 90)) +
  facet_wrap(~Location, scales = "free_y")


#*******************************************FIN****************************************#
#**************************************************************************************#