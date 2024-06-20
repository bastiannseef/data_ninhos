library(tidyverse)
library(rio)

resultados <- import("datos_ninos.xlsx", sheet = "resultados")
censo <- import("datos_ninos.xlsx", sheet = "censo-oficial")


# Características Inherentes --------------------------------------------------------------

## Exactitud ---------------------------------------------------------------

exactitud1 <- resultados %>% 
  filter(
    !between(peso,25,58)|!between(talla,1.10,1.50)
    |!between(hemoglobina,9,17)|if_any(everything(), is.na)
    )
exactitud1

exactitud2 <- censo %>% 
  filter(
    if_any(everything(), is.na)
  )
exactitud2

resultados <- resultados %>% 
  mutate(dni = as.character(dni))
resultados %>% glimpse()

censo <- censo %>% 
  mutate(dni = as.character(dni))
censo %>% glimpse()


## Completitud -------------------------------------------------------------

lostcenso <- colSums(is.na(censo))
lostcenso

lostresultados <- colSums(is.na(resultados))
lostresultados

##Consistencia -------------------------------------------------------------

consisdata <- resultados %>% 
  mutate(
    peso = case_when(
      between(peso,28,55) ~ peso,
      peso>=280 ~ peso/10),
    talla = case_when(
      between(talla,1.10,1.50) ~ talla*100,
      is.na(talla) ~ round(mean(talla*100,na.rm = TRUE),1)),
    parasitos = case_when(
      between(parasitos,0,1) ~ parasitos,
      is.na(parasitos) ~ 0)
    ) %>%  
  mutate(
    peso = case_when(
      between(peso,28,55) ~ peso,
      is.na(peso) ~ round(mean(peso,na.rm = TRUE),1)),
      )
    


##Actualidad ---------------------------------------------------------------
censo_actual <- censo %>%
  mutate(
    edad = as.numeric(trunc(difftime(as.Date("2023-06-01"),date_birth)/365))
    )

#Características dependientes del sistema ----------------------------------


## Disponibilidad ----------------------------------------------------------

#https://github.com/bastiannseef/data_ninhos.git

## Portabilidad ------------------------------------------------------------

complete_data <- censo_actual %>% 
  inner_join(consisdata)
complete_data

data.frame(complete_data) %>% 
  saveRDS("C:/Users/Sebastian/OneDrive/Escritorio/Trabajo/data/data_procesada/datos.rds")

readRDS("C:/Users/Sebastian/OneDrive/Escritorio/Trabajo/data/data_procesada/datos.rds") %>%
  mutate(date_birth = as.Date(date_birth)) %>% 
  export("C:/Users/Sebastian/OneDrive/Escritorio/Trabajo/data/data_actualizada/data_ninos_limpia.xlsx")

## Interoperabilidad -------------------------------------------------------

#file:///C:/Users/Sebastian/OneDrive/Escritorio/Trabajo/archivos/Markdown---CDD.html


