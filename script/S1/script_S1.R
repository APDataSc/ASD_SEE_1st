"######################################################################"
#         
#                     Análisis sociodemográfico con R
#                               Sesión 1
#
#         Creado por:               Andrés Peña M.  
#         Fecha de creación:        22-06-2024
#         Actualizado por:          Andrés Peña M.
#         Fecha de actualización:   22-06-2024
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
# Comentar es el comando: Ctrl + Shift + c

library(dplyr) # Ctrl + Enter
library(data.table)
library(foreign)
library(tictoc)

# Análisis previo ----

# Alt + "-": "<-"
x <- rnorm(100)
y = x*2 

vec <- c(6, 1, 3, 6, 10, 5)
vec[2]
vec[c(2,6)]
vec[2:6]

x
hist(x)
quantile(x, probs = seq(0, 1, 0.1))

mtx <- matrix (1:12,nrow=3, ncol=4, byrow=FALSE)

mtx[ 2, ] 
mtx[ , 3:4 ] 

# Factores
vec <- c(1, 2, 2, NA, 1, 2, NA, 1, 2)

fac <- factor(vec, levels = 1:2, labels = c("Hombre", "Mujer"))

table(fac, useNA = "ifany")
prop.table(table(fac))
barplot(prop.table(table(fac)))


# Data frames

nomb <- c("John", "Paul", "George", "Ringo")
nac <- c(1940, 1941, 1943, 1940)
instr <- c("guitar", "bass", "guitar", "drums")

df <- data.frame(nomb, nac, instr)

df[df$instr=="guitar", ]


# Manipulación de censos y encuestas ---- 

rm(list = ls())

data <- read.spss("data/10 ENIGHUR11_HOGARES_AGREGADOS.SAV", use.value.labels = T, 
                  to.data.frame = T)

names(data)
sum(data$Fexp_cen2010)


# Ctrl + Shift + m : operador pipe

tab1 <- data %>%
                # group_by(Provincia) %>%
                summarise(tam_medio = mean(numpers))


data1 <- data %>% 
        select(Fexp_cen2010, Provincia, Área, sexo, edad, grupos_edad,
               ing_cor_tot, dec_nac_per, gas_cor_tot, d1)

data1 <- data1 %>% 
         mutate(pobre = ifelse(gas_cor_tot <= 86, 1, 0))

tab2 <- data1 %>% 
        group_by(Provincia) %>% 
        summarise(pobreza = mean(pobre))

data2 <- data1 %>% 
         filter(Área=="Urbana")
  

# data.table
tic()
censo <- fread("data/CPV_Población_2022_Nacional.csv")
toc()


# Hola mundo

