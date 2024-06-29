"######################################################################"
#         
#                     Análisis sociodemográfico con R
#                               Sesión 2
#
#         Creado por:               Andrés Peña M.  
#         Fecha de creación:        29-06-2024
#         Actualizado por:          Andrés Peña M.
#         Fecha de actualización:   29-06-2024
#         Institución:              El Colegio de México A.C.
#         Contacto:                 agpena@colmex.mx
#
"######################################################################"

# Preámbulo ----

rm(list = ls()) # limpiar la memoria

# install.packages("data.table", dependencies = T)
# install.packages("dplyr", dependencies = T)
# install.packages("foreign", dependencies = TRUE)
# install.packages("tictoc", dependencies = T)
# install.packages("stringr", dependencies = T)
# install.packages("car", dependencies = T)

# Comentar es el comando: Ctrl + Shift + c

library(dplyr) # Ctrl + Enter
library(data.table)
library(foreign)
library(tictoc)
library(stringr)
library(car)


# Análisis de censo y encuesta por muestreo ----

## Encuesta por muestreo ----

enc <- read.spss("data/10 ENIGHUR11_HOGARES_AGREGADOS.SAV", 
                 use.value.labels = F, 
                 to.data.frame = T  
                 )
  
sum(enc$Fexp_cen2010)


### Lógica dplyr ----
# data.frame

tab1 <- enc %>%
        mutate(ing_per = ing_mon_cor/numpers) %>% 
        mutate(pobre = ifelse(ing_per<80, 1, 0)) %>%
        group_by(Provincia) %>% 
        summarise(pob_inc = mean(pobre, na.rm = T)) %>% 
        arrange(desc(pob_inc))

enc_sel_fil <- enc %>% 
               select(Fexp_cen2010, Provincia, Área, sexo, edad, grupos_edad,
                      ing_cor_tot, dec_nac_per, gas_cor_tot, d1) %>% 
               filter(Área == "Urbana" & Provincia == "Galápagos")

### Lógica data.table ----
# data.table
class(enc) 

enc <- as.data.table(enc)
enc <- setDT(enc)

enc_sel_fil_DT <- enc[Área=="Urbana" & Provincia == "Galápagos"] 
                      .(Fexp_cen2010, Provincia, Área, sexo, edad, grupos_edad,
                        ing_cor_tot, dec_nac_per, gas_cor_tot, d1)]

tab1_DT <- enc[ , ing_per := ing_mon_cor/numpers][ , 
                                                    pobre := ifelse(ing_per<80, 1, 0)][ ,
                                                                                        .(pob_inc = mean(pobre, na.rm = T))]

tab1_DT <- enc[ , ing_per := ing_mon_cor/numpers] %>% 
              .[ , pobre := ifelse(ing_per<80, 1, 0)] %>% 
              .[ , .(pob_inc = mean(pobre, na.rm = T)), .(Provincia) ] %>% 
              setorder(pob_inc)  
                                                   

### Comandos más útiles ----

# Agregar casos

enc_urb <- enc[Área=="Urbana", .(Fexp_cen2010, Provincia, Área, sexo, edad, grupos_edad,
                                 ing_cor_tot, dec_nac_per, gas_cor_tot, d1)]
dim(enc_urb)

enc_rur <- enc[Área=="Rural", .(Fexp_cen2010, Provincia, Área, sexo, edad, grupos_edad,
                                 ing_cor_tot, dec_nac_per, gas_cor_tot, d1)]
dim(enc_rur)


enc_urb_rur <- rbind(enc_urb, enc_rur) # casos agregados
dim(enc_urb_rur)


# Agregar variables (merge)

enc_dem <- enc[ , .(Identif_hog, Fexp_cen2010, Provincia, Área, sexo, edad, grupos_edad)]

enc_eco <- enc[ , .(Identif_hog, ing_cor_tot, dec_nac_per, gas_cor_tot, d1)]
  

enc_dem_eco <- merge(enc_dem, enc_eco[ , .(Identif_hog, ing_cor_tot)], 
                     by = "Identif_hog", all.x = T) # Entre comillas el id
  

# Creación de identificador: ciudad,  zona, sector, vivienda, hogar 

typeof(enc$Ciudad)
class(enc$Ciudad)
unique(enc$Ciudad)
unique(nchar(enc$Ciudad))

enc$ciudad <- str_pad(enc$Ciudad, 6, pad = "0")
enc$zona <- str_pad(enc$Zona, 3, pad = "0")
enc$sector <- str_pad(enc$Sector, 3, pad = "0")
enc$vivienda <- str_pad(enc$Vivienda, 2, pad = "0")

enc <- setDT(enc)
enc[ , id := paste0(ciudad, zona, sector, vivienda, Hogar)]

View(enc[ , c("Identif_hog", "id")])     

sum(enc$Identif_hog==enc$id)==nrow(enc)


# Recodificación de variables

# Categóricas y numéricas
typeof(enc$Provincia)
enc[ , region := recode(Provincia, "1:8='1';
                                    9:16='2';
                                    17:24='3'")]

table(enc$Provincia, enc$region)

enc$region <- factor(enc$Provincia, levels = 1:3, labels = c("Costa",
                                                             "Sierra",
                                                             "Amazonía"))

# Númericas 

table(enc$edad)
hist(enc$edad)
typeof(enc$edad)

enc[ , edad_gru := cut(edad, c(0, 15, 65, 98))]
barplot(table(enc$edad_gru))


## Censo ----

# Script de indicadores censales "analfabetismo", "escolaridad", "electricidad" y "tamanoHogar"
