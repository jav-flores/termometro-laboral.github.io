#*******************************************************************************************************************
#
#             Proyecto: Automatización de informe Termómetro Laboral
#             Tema: Lectura datos
#             Fecha versión: 04/12/2023
#             Tipo de archivo: Carga datos ENE-regional
#
#
#******************************************************************************************************************


# 1. Cargar librerias -----------------------------------------------------

paquetes <- c("tinytex", "magrittr", "stringr", "kableExtra",
              "lubridate", "knitr", "survey", "tidyverse", 
              "haven", "scales", "dplyr", "officer", "sjlabelled", 
              "readxl", "tidyr", "openxlsx", "ggplot2", "ggrepel", 
              "tibble", "xts", "data.table", "timeDate", "seasonal",
              "labelled", "ggpubr", "showtext", "xtable", "sjmisc",
              "summarytools", "calidad")

for(p in paquetes){
  if (!require(p,character.only = T))    install.packages(p);    library(p,character.only = T)
}


options(pillar.sigfig = 10)
options(scipen = 999)

# 2. Parametros variables  ------------------------------------------------

## Parametrizar valores que cambian mes a mes

mm  <-  9 # Mes central del trimestre respectivo
yy  <-  2023 # Año respectivo
y0  <-  yy - 1
y1  <-  yy
y2  <-  yy + 1
trimestre<- dplyr::case_when(mm == 1 ~ paste0("diciembre ",y0," - febrero ",y1),
                             mm == 2 ~ paste0("enero - marzo ",y1),
                             mm == 3 ~ paste0("febrero - abril ",y1),
                             mm == 4 ~ paste0("marzo - mayo ",y1),
                             mm == 5 ~ paste0("abril - junio ",y1),
                             mm == 6 ~ paste0("mayo - julio ",y1),
                             mm == 7 ~ paste0("junio - agosto ",y1),
                             mm == 8 ~ paste0("julio - septiembre ",y1),
                             mm == 9 ~ paste0("agosto - octubre ",y1),
                             mm == 10 ~ paste0("septiembre - noviembre ",y1),
                             mm == 11 ~ paste0("octubre - diciembre ",y1),
                             mm == 12 ~ paste0("noviembre ",y1," - enero ",y2))

trim <- dplyr::case_when(mm == 1 ~ paste0(" DEF"),
                         mm == 2 ~ paste0(" EFM"),
                         mm == 3 ~ paste0(" FMA"),
                         mm == 4 ~ paste0(" MAM"),
                         mm == 5 ~ paste0(" AMJ"),
                         mm == 6 ~ paste0(" MJJ"),
                         mm == 7 ~ paste0(" JJA"),
                         mm == 8 ~ paste0(" JAS"),
                         mm == 9 ~ paste0(" ASO"),
                         mm == 10 ~ paste0(" SON"),
                         mm == 11 ~ paste0(" OND"),
                         mm == 12 ~ paste0(" NDE"))

# 3. Cargar datos ---------------------------------------------------------



ene <- list()
dat <- c(yy*100 + mm, (yy-1)*100 + mm, (yy-2)*100 + mm, (yy-3)*100 + mm, (yy-4)*100 + mm)


i <- 1
for(t in dat){
  files <- paste0("C:/Users/javie/Desktop/Termómetro Laboral/1. Bases de Datos (Encuestas)/ENE/STATA/Base Censo2017/Bases rectificadas/ene-", t, ".dta")
  ene[[i]] <- read_dta(files, encoding="latin1") %>% 
    sjlabelled::remove_all_labels(.) %>% 
    as_tibble(.) %>% 
    select(ano_trimestre, mes_central, edad, region, sexo, cine, b14_rev4cl_caenes,
           r_p_rev4cl_caenes,cae_especifico, ocup_form, activ, categoria_ocupacion,
           conglomerado, estrato, fact_cal, tramo_edad, habituales, e9, nivel, termino_nivel) %>% 
    mutate(trim = case_when(mes_central == 1 ~ paste0("DEF"),
                            mes_central == 2 ~ paste0("EFM"),
                            mes_central == 3 ~ paste0("FMA"),
                            mes_central == 4 ~ paste0("MAM"),
                            mes_central == 5 ~ paste0("AMJ"),
                            mes_central == 6 ~ paste0("MJJ"),
                            mes_central == 7 ~ paste0("JJA"),
                            mes_central == 8 ~ paste0("JAS"),
                            mes_central == 9 ~ paste0("ASO"),
                            mes_central == 10 ~ paste0("SON"),
                            mes_central == 11 ~ paste0("OND"),
                            mes_central == 12 ~ paste0("NDE")),
           anotrim = paste(ano_trimestre, trim, sep = " "))
  i <- i+1
}


