#*******************************************************************************************************************
#
#             Proyecto: Automatización de informe Termómetro Laboral
#             Autor: Andreas Lafferte (Unidad de Estudios)
#             Tema: Texto
#             Fecha versión: 28/09/2022
#             Tipo de archivo: Cálculos ENE-regional
#
#
#******************************************************************************************************************



# 1. Cargar librerías -----------------------------------------------------

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

mm  <-  9 # Mes central del trim_actestre respectivo
yy  <-  2023 # Año respectivo
y0  <-  yy - 1
y1  <-  yy
y2  <-  yy + 1
trim_actestre<- dplyr::case_when(mm == 1 ~ paste0("diciembre ",y0," - febrero ",y1),
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

trim_act <- dplyr::case_when(mm == 1 ~ paste0(" DEF"),
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


load("C:/Users/javie/Desktop/Termómetro Laboral/minuta-regional/output/calculos.RData")

# 4. Texto ----------------------------------------------------------------


## FUERZA DE TRABAJO ----

# 4.1 Fuerza de trabajo, ocupados, desocupados y fuera fza

txt_fdt_n <- list(
  trim_act = round(fdt[[1]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_12m = round(fdt[[2]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_24m = round(fdt[[3]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_36m = round(fdt[[4]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_48m = round(fdt[[4]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0)
  
)




txt_ocup_n <- list(
  trim_act = round(ocup[[1]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_12m = round(ocup[[2]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_24m = round(ocup[[3]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_36m = round(ocup[[4]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_48m = round(ocup[[5]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0)
  
)



txt_desocup_n <- list(
  trim_act = round(desocup[[1]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_12m = round(desocup[[2]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_24m = round(desocup[[3]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_36m = round(desocup[[4]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_48m = round(desocup[[5]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0)
  
)



txt_fuera_fza_n <- list(
  trim_act = round(fuera_fza[[1]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_24m = round(fuera_fza[[3]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_36m = round(fuera_fza[[4]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_48m = round(fuera_fza[[5]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0)
  
)


# Tasas 

txt_tasa_parti_n <- list(
  trim_act = tasa_parti_n[[1]],
  trim_12m = tasa_parti_n[[2]],
  trim_24m = tasa_parti_n[[3]],
  trim_36m = tasa_parti_n[[4]],
  trim_48m = tasa_parti_n[[5]]
  
)


txt_var_tasa_parti_n <- list(
  trim_act = tasa_parti_n[[1]],
  trim_12m = tasa_parti_n[[1]] - tasa_parti_n[[2]],
  trim_24m = tasa_parti_n[[1]] - tasa_parti_n[[3]],
  trim_36m = tasa_parti_n[[1]] - tasa_parti_n[[4]],
  trim_48m = tasa_parti_n[[1]] - tasa_parti_n[[5]]
  
)


txt_tasa_ocup_n <- list(
  trim_act = tasa_ocup_n[[1]],
  trim_12m = tasa_ocup_n[[2]],
  trim_24m = tasa_ocup_n[[3]],
  trim_36m = tasa_ocup_n[[4]],
  trim_48m = tasa_ocup_n[[5]]
  
)

txt_var_tasa_ocup_n <- list(
  trim_act = tasa_ocup_n[[1]],
  trim_12m = tasa_ocup_n[[1]] - tasa_ocup_n[[2]],
  trim_24m = tasa_ocup_n[[1]] - tasa_ocup_n[[3]],
  trim_36m = tasa_ocup_n[[1]] - tasa_ocup_n[[4]],
  trim_48m = tasa_ocup_n[[1]] - tasa_ocup_n[[5]]
  
)


txt_tasa_desocup_n <- list(
  trim_act = tasa_desocup_n[[1]],
  trim_12m = tasa_desocup_n[[2]],
  trim_24m = tasa_desocup_n[[3]],
  trim_36m = tasa_desocup_n[[4]],
  trim_48m = tasa_desocup_n[[5]]
  
  
  
)


txt_var_tasa_desocup_n <- list(
  trim_act = tasa_desocup_n[[1]],
  trim_12m = tasa_desocup_n[[1]] - tasa_desocup_n[[2]],
  trim_24m = tasa_desocup_n[[1]] - tasa_desocup_n[[3]],
  trim_36m = tasa_desocup_n[[1]] - tasa_desocup_n[[4]],
  trim_48m = tasa_desocup_n[[1]] - tasa_desocup_n[[5]]
  
) 



txt_tasa_informal_n <- list(
  trim_act = tasa_informal_n[[1]],
  trim_12m = tasa_informal_n[[2]],
  trim_24m = tasa_informal_n[[3]],
  trim_36m = tasa_informal_n[[4]],
  trim_48m = tasa_informal_n[[5]]
  
  
)



txt_var_tasa_informal_n <- list(
  trim_act = tasa_informal_n[[1]],
  trim_12m = tasa_informal_n[[1]] - tasa_informal_n[[2]],
  trim_24m = tasa_informal_n[[1]] - tasa_informal_n[[3]],
  trim_36m = tasa_informal_n[[1]] - tasa_informal_n[[4]],
  trim_48m = tasa_informal_n[[1]] - tasa_informal_n[[5]]
  
  
)






# categoria ocupacional

cat_ocup_1 <- categoria_ocup[[1]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(stat_1 = round(stat,0)) %>% 
  select(region, categoria_ocup, stat_1)

cat_ocup_2 <- categoria_ocup[[2]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(stat_2 = round(stat,0)) %>% 
  select(region, categoria_ocup, stat_2)

cat_ocup_2019 <- categoria_ocup[[5]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(stat_2019 = round(stat,0)) %>% 
  select(region, categoria_ocup, stat_2019)

ocup_total <- ocup[[1]] %>%
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(ocup_total = round(stat,0)) %>% 
  select(region, ocup_total)

txt_cat_ocup <- full_join(cat_ocup_1, cat_ocup_2, by = c("region", "categoria_ocup"))
txt_cat_ocup <- full_join(txt_cat_ocup, cat_ocup_2019, by = c("region", "categoria_ocup"))
txt_cat_ocup <- full_join(txt_cat_ocup, ocup_total, by = c("region"))%>%
  mutate(dif_anual = ((stat_1 - stat_2)/stat_1)*100,
         dif_2019 = ((stat_1 - stat_2019)/stat_1)*100,
         tasa_ocup = (stat_1/ocup_total)*100,
         region = case_when(region == "Tarapacá" ~ 1, 
                            region == "Antofagasta" ~ 2, 
                            region == "Atacama" ~ 3, 
                            region == "Coquimbo" ~ 4, 
                            region == "Valparaíso" ~ 5, 
                            region == "O'Higgins" ~ 6, 
                            region == "Maule"~ 7, 
                            region == "Biobío" ~ 8, 
                            region == "Araucanía" ~ 9, 
                            region == "Los Lagos" ~ 10, 
                            region == "Aysen" ~ 11, 
                            region == "Magallanes y Antártica" ~ 12, 
                            region == "Metropolitana" ~ 13, 
                            region == "Los Ríos"  ~ 14, 
                            region == "Arica y Parinacota" ~ 15, 
                            region ==  "Ñuble" ~ 16,
                            TRUE ~ NA_real_))

txt_cat_ocup$dif_anual <- round(txt_cat_ocup$dif_anual, 2)
txt_cat_ocup$dif_2019 <- round(txt_cat_ocup$dif_2019, 2)
txt_cat_ocup$tasa_ocup <- round(txt_cat_ocup$tasa_ocup, 2)


# ocupación por tramo edad

ocup_edad_1 <- ocup_edad[[1]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(ocup_edad_1 = round(stat,0)) %>% 
  select(region, tramo_edad, ocup_edad_1)

ocup_edad_2 <- ocup_edad[[2]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(ocup_edad_2 = round(stat,0)) %>% 
  select(region, tramo_edad, ocup_edad_2)

ocup_edad_2019 <- ocup_edad[[5]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(ocup_edad_2019 = round(stat,0)) %>% 
  select(region, tramo_edad, ocup_edad_2019)


pet_edad_1 <- pet_edad[[1]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(pet_edad_1 = round(stat,0)) %>% 
  select(region, tramo_edad, pet_edad_1)

pet_edad_2 <- pet_edad[[2]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(pet_edad_2 = round(stat,0)) %>% 
  select(region, tramo_edad, pet_edad_2)

pet_edad_2019 <- pet_edad[[5]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(pet_edad_2019 = round(stat,0)) %>% 
  select(region, tramo_edad, pet_edad_2019)

txt_ocup_edad <- full_join(ocup_edad_1, ocup_edad_2, by = c("region", "tramo_edad"))
txt_ocup_edad <- full_join(txt_ocup_edad, ocup_edad_2019, by = c("region", "tramo_edad"))
txt_ocup_edad <- full_join(txt_ocup_edad, pet_edad_1, by = c("region", "tramo_edad"))
txt_ocup_edad <- full_join(txt_ocup_edad, pet_edad_2, by = c("region", "tramo_edad"))
txt_ocup_edad <- full_join(txt_ocup_edad, pet_edad_2019, by = c("region", "tramo_edad"))%>%
  mutate(tasa_ocup_edad_1 = (ocup_edad_1/pet_edad_1)*100,
         tasa_ocup_edad_2 = (ocup_edad_2/pet_edad_2)*100,
         tasa_ocup_edad_2019 = (ocup_edad_2019/pet_edad_2019)*100,
         
         region = case_when(region == "Tarapacá" ~ 1, 
                            region == "Antofagasta" ~ 2, 
                            region == "Atacama" ~ 3, 
                            region == "Coquimbo" ~ 4, 
                            region == "Valparaíso" ~ 5, 
                            region == "O'Higgins" ~ 6, 
                            region == "Maule"~ 7, 
                            region == "Biobío" ~ 8, 
                            region == "Araucanía" ~ 9, 
                            region == "Los Lagos" ~ 10, 
                            region == "Aysen" ~ 11, 
                            region == "Magallanes y Antártica" ~ 12, 
                            region == "Metropolitana" ~ 13, 
                            region == "Los Ríos"  ~ 14, 
                            region == "Arica y Parinacota" ~ 15, 
                            region ==  "Ñuble" ~ 16,
                            TRUE ~ NA_real_))

txt_ocup_edad$tasa_ocup_edad_1 <- round(txt_ocup_edad$tasa_ocup_edad_1, 2)
txt_ocup_edad$tasa_ocup_edad_2 <- round(txt_ocup_edad$tasa_ocup_edad_2, 2)
txt_ocup_edad$tasa_ocup_edad_2019 <- round(txt_ocup_edad$tasa_ocup_edad_2019, 2)



# desocupación por tramo edad

desocup_edad_1 <- desocup_edad[[1]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(desocup_edad_1 = round(stat,0)) %>% 
  select(region, tramo_edad, desocup_edad_1)

desocup_edad_2 <- desocup_edad[[2]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(desocup_edad_2 = round(stat,0)) %>% 
  select(region, tramo_edad, desocup_edad_2)

desocup_edad_2019 <- desocup_edad[[5]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(desocup_edad_2019 = round(stat,0)) %>% 
  select(region, tramo_edad, desocup_edad_2019)


fdt_edad_1 <- fdt_edad[[1]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(fdt_edad_1 = round(stat,0)) %>% 
  select(region, tramo_edad, fdt_edad_1)

fdt_edad_2 <- fdt_edad[[2]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(fdt_edad_2 = round(stat,0)) %>% 
  select(region, tramo_edad, fdt_edad_2)

fdt_edad_2019 <- fdt_edad[[5]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(fdt_edad_2019 = round(stat,0)) %>% 
  select(region, tramo_edad, fdt_edad_2019)

txt_desocup_edad <- full_join(desocup_edad_1, desocup_edad_2, by = c("region", "tramo_edad"))
txt_desocup_edad <- full_join(txt_desocup_edad, desocup_edad_2019, by = c("region", "tramo_edad"))
txt_desocup_edad <- full_join(txt_desocup_edad, fdt_edad_1, by = c("region", "tramo_edad"))
txt_desocup_edad <- full_join(txt_desocup_edad, fdt_edad_2, by = c("region", "tramo_edad"))
txt_desocup_edad <- full_join(txt_desocup_edad, fdt_edad_2019, by = c("region", "tramo_edad"))%>%
  mutate(tasa_desocup_edad_1 = (desocup_edad_1 / fdt_edad_1)*100,
         tasa_desocup_edad_2 = (desocup_edad_2 / fdt_edad_2)*100,
         tasa_desocup_edad_2019 = (desocup_edad_2019 / fdt_edad_2019)*100,
         
         region = case_when(region == "Tarapacá" ~ 1, 
                            region == "Antofagasta" ~ 2, 
                            region == "Atacama" ~ 3, 
                            region == "Coquimbo" ~ 4, 
                            region == "Valparaíso" ~ 5, 
                            region == "O'Higgins" ~ 6, 
                            region == "Maule"~ 7, 
                            region == "Biobío" ~ 8, 
                            region == "Araucanía" ~ 9, 
                            region == "Los Lagos" ~ 10, 
                            region == "Aysen" ~ 11, 
                            region == "Magallanes y Antártica" ~ 12, 
                            region == "Metropolitana" ~ 13, 
                            region == "Los Ríos"  ~ 14, 
                            region == "Arica y Parinacota" ~ 15, 
                            region ==  "Ñuble" ~ 16,
                            TRUE ~ NA_real_))

txt_desocup_edad$tasa_desocup_edad_1 <- round(txt_desocup_edad$tasa_desocup_edad_1, 2)
txt_desocup_edad$tasa_desocup_edad_2 <- round(txt_desocup_edad$tasa_desocup_edad_2, 2)
txt_desocup_edad$tasa_desocup_edad_2019 <- round(txt_desocup_edad$tasa_desocup_edad_2019, 2)




# 4.2 Fuerza de trabajo, ocupados, desocupados y fuera fza segun sexo

txt_fdt_h <- list(
  trim_act = round(fdt_sex[[1]] %>% 
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_12m = round(fdt_sex[[2]] %>%
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0), 
  trim_24m = round(fdt_sex[[3]] %>%
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_36m = round(fdt_sex[[4]] %>%
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_48m = round(fdt_sex[[5]] %>%
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0)
  
)





txt_fdt_m <- list(
  trim_act = round(fdt_sex[[1]] %>% 
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_12m = round(fdt_sex[[2]] %>%
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0), 
  trim_24m = round(fdt_sex[[3]] %>%
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_36m = round(fdt_sex[[4]] %>%
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_48m = round(fdt_sex[[5]] %>%
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0)
)




txt_ocup_h <- list(
  trim_act = round(ocup_sex[[1]] %>% 
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_12m = round(ocup_sex[[2]] %>%
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0), 
  trim_24m = round(ocup_sex[[3]] %>%
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_36m = round(ocup_sex[[4]] %>%
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_48m = round(ocup_sex[[5]] %>%
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0)
  
)


txt_ocup_m <- list(
  trim_act = round(ocup_sex[[1]] %>% 
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_12m = round(ocup_sex[[2]] %>%
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_24m = round(ocup_sex[[3]] %>%
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_36m = round(ocup_sex[[4]] %>%
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_48m = round(ocup_sex[[5]] %>%
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0)
)



txt_desocup_h <- list(
  trim_act = round(desocup_sex[[1]] %>% 
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_12m = round(desocup_sex[[2]] %>%
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_24m = round(desocup_sex[[3]] %>%
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_36m = round(desocup_sex[[4]] %>%
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_48m = round(desocup_sex[[5]] %>%
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0)
  
)



txt_desocup_m <- list(
  trim_act = round(desocup_sex[[1]] %>% 
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_12m = round(desocup_sex[[2]] %>%
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0), 
  trim_24m = round(desocup_sex[[3]] %>%
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_36m = round(desocup_sex[[4]] %>%
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_48m = round(desocup_sex[[5]] %>%
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0)
  
)



txt_fuera_fza_h <- list(
  trim_act = round(fuera_fza_sex[[1]] %>% 
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_12m = round(fuera_fza_sex[[2]] %>%
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),  
  trim_24m = round(fuera_fza_sex[[3]] %>%
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_36m = round(fuera_fza_sex[[4]] %>%
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_48m = round(fuera_fza_sex[[5]] %>%
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0)
)


txt_fuera_fza_m <- list(
  trim_act = round(fuera_fza_sex[[1]] %>% 
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_12m = round(fuera_fza_sex[[2]] %>%
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),  
  trim_24m = round(fuera_fza_sex[[3]] %>%
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_36m = round(fuera_fza_sex[[4]] %>%
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0),
  trim_48m = round(fuera_fza_sex[[5]] %>%
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat), 0)
)




# Tasas por sexo

txt_tasa_parti_h <- list(
  trim_act = tasa_parti_h[[1]],
  trim_12m = tasa_parti_h[[2]],
  trim_24m = tasa_parti_h[[3]],
  trim_36m = tasa_parti_h[[4]],
  trim_48m = tasa_parti_h[[5]]
  
)


txt_var_tasa_parti_h <- list(
  trim_act = tasa_parti_h[[1]],
  trim_12m = tasa_parti_h[[1]] - tasa_parti_h[[2]],
  trim_24m = tasa_parti_h[[1]] - tasa_parti_h[[3]],
  trim_36m = tasa_parti_h[[1]] - tasa_parti_h[[4]],
  trim_48m = tasa_parti_h[[1]] - tasa_parti_h[[5]]
  
)




txt_tasa_parti_m <- list(
  trim_act = tasa_parti_m[[1]],
  trim_12m = tasa_parti_m[[2]],
  trim_24m = tasa_parti_m[[3]],
  trim_36m = tasa_parti_m[[4]],
  trim_48m = tasa_parti_m[[5]]
  
)



txt_var_tasa_parti_m <- list(
  trim_act = tasa_parti_m[[1]],
  trim_12m = tasa_parti_m[[1]] - tasa_parti_m[[2]],
  trim_24m = tasa_parti_m[[1]] - tasa_parti_m[[3]],
  trim_36m = tasa_parti_m[[1]] - tasa_parti_m[[4]],
  trim_48m = tasa_parti_m[[1]] - tasa_parti_m[[5]]
  
)



txt_tasa_ocup_h <- list(
  trim_act = tasa_ocup_h[[1]],
  trim_12m = tasa_ocup_h[[2]],
  trim_24m = tasa_ocup_h[[3]],
  trim_36m = tasa_ocup_h[[4]],
  trim_48m = tasa_ocup_h[[5]]
  
)



txt_var_tasa_ocup_h <- list(
  trim_act = tasa_ocup_h[[1]],
  trim_12m = tasa_ocup_h[[1]] - tasa_ocup_h[[2]],
  trim_24m = tasa_ocup_h[[1]] - tasa_ocup_h[[3]],
  trim_36m = tasa_ocup_h[[1]] - tasa_ocup_h[[4]],
  trim_48m = tasa_ocup_h[[1]] - tasa_ocup_h[[5]]
  
)



txt_tasa_ocup_m <- list(
  trim_act = tasa_ocup_m[[1]],
  trim_12m = tasa_ocup_m[[2]],
  trim_24m = tasa_ocup_m[[3]],
  trim_36m = tasa_ocup_m[[4]],
  trim_48m = tasa_ocup_m[[5]]
  
)


txt_var_tasa_ocup_m <- list(
  trim_act = tasa_ocup_m[[1]],
  trim_12m = tasa_ocup_m[[1]] - tasa_ocup_m[[2]],
  trim_24m = tasa_ocup_m[[1]] - tasa_ocup_m[[3]],
  trim_36m = tasa_ocup_m[[1]] - tasa_ocup_m[[4]],
  trim_48m = tasa_ocup_m[[1]] - tasa_ocup_m[[5]]
  
)  


txt_tasa_desocup_h <- list(
  trim_act = tasa_desocup_h[[1]],
  trim_12m = tasa_desocup_h[[2]],
  trim_24m = tasa_desocup_h[[3]],
  trim_36m = tasa_desocup_h[[4]],
  trim_48m = tasa_desocup_h[[5]]
  
)


txt_var_tasa_desocup_h <- list(
  trim_act = tasa_desocup_h[[1]],
  trim_12m = tasa_desocup_h[[1]] - tasa_desocup_h[[2]],
  trim_24m = tasa_desocup_h[[1]] - tasa_desocup_h[[3]],
  trim_36m = tasa_desocup_h[[1]] - tasa_desocup_h[[4]],
  trim_48m = tasa_desocup_h[[1]] - tasa_desocup_h[[5]]
  
)



txt_tasa_desocup_m <- list(
  trim_act = tasa_desocup_m[[1]],
  trim_12m = tasa_desocup_m[[2]],
  trim_24m = tasa_desocup_m[[3]],
  trim_36m = tasa_desocup_m[[4]],
  trim_48m = tasa_desocup_m[[5]]
  
)



txt_var_tasa_desocup_m <- list(
  trim_act = tasa_desocup_m[[1]],
  trim_12m = tasa_desocup_m[[1]] - tasa_desocup_m[[2]],
  trim_24m = tasa_desocup_m[[1]] - tasa_desocup_m[[3]],
  trim_36m = tasa_desocup_m[[1]] - tasa_desocup_m[[4]],
  trim_48m = tasa_desocup_m[[1]] - tasa_desocup_m[[5]]
  
)



txt_tasa_informal_h <- list(
  trim_act = tasa_informal_h[[1]],
  trim_12m = tasa_informal_h[[2]],
  trim_24m = tasa_informal_h[[3]],
  trim_36m = tasa_informal_h[[4]],
  trim_48m = tasa_informal_h[[5]]
  
)


txt_var_tasa_informal_h <- list(
  trim_act = tasa_informal_h[[1]],
  trim_12m = tasa_informal_h[[1]] - tasa_informal_h[[2]],
  trim_24m = tasa_informal_h[[1]] - tasa_informal_h[[3]],
  trim_36m = tasa_informal_h[[1]] - tasa_informal_h[[4]],
  trim_48m = tasa_informal_h[[1]] - tasa_informal_h[[5]]
  
  
)




txt_tasa_informal_m <- list(
  trim_act = tasa_informal_m[[1]],
  trim_12m = tasa_informal_m[[2]],
  trim_24m = tasa_informal_m[[3]],
  trim_36m = tasa_informal_m[[4]],
  trim_48m = tasa_informal_m[[5]]
  
)


txt_var_tasa_informal_m <- list(
  trim_act = tasa_informal_m[[1]],
  trim_12m = tasa_informal_m[[1]] - tasa_informal_m[[2]],
  trim_24m = tasa_informal_m[[1]] - tasa_informal_m[[3]],
  trim_36m = tasa_informal_m[[1]] - tasa_informal_m[[4]],
  trim_48m = tasa_informal_m[[1]] - tasa_informal_m[[5]]
  
)



## 4.2 CREACION EMPLEO ----


# 4.2.1 Empleo formal año anterior

txt_emple_formal_n <- list(
  trim_act = formal[[1]] %>% 
    select(stat,label) %>% 
    mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
    summarise(stat = round(stat,0)),
  trim_12m = mapply("-", formal[[1]] %>% 
                      select(stat, label) %>% 
                      mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                      summarise(stat = round(stat,0)), 
                    formal[[2]] %>% 
                      select(stat, label) %>% 
                      mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                      summarise(stat = round(stat,0))) 
  
)

txt_emple_formal_h <- list(
  
  trim_act = formal_sex[[1]] %>% 
    filter(sexo == 1) %>% 
    select(stat,label) %>% 
    mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
    summarise(stat = round(stat,0)),
  
  trim_12m = mapply("-", formal_sex[[1]] %>% 
                      filter(sexo == 1) %>% 
                      select(stat, label) %>% 
                      mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                      summarise(stat = round(stat,0)), 
                    formal_sex[[2]] %>% 
                      filter(sexo == 1) %>%
                      select(stat, label) %>% 
                      mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                      summarise(stat = round(stat,0)))
  
)

txt_emple_formal_m <- list(
  
  trim_act = formal_sex[[1]] %>% 
    filter(sexo == 2) %>% 
    select(stat,label) %>% 
    mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
    summarise(stat = round(stat,0)),
  
  trim_12m = mapply("-", formal_sex[[1]] %>% 
                      filter(sexo == 2) %>% 
                      select(stat, label) %>% 
                      mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                      summarise(stat = round(stat,0)), 
                    formal_sex[[2]] %>% 
                      filter(sexo == 2) %>%
                      select(stat, label) %>% 
                      mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                      summarise(stat = round(stat,0)))         
  
)


# 4.2.2 Creacion empleo segun rama

rama_1 <- rama[[1]] %>% 
  filter(label != "no fiable") %>% 
  group_by(region, rama) %>% 
  mutate(stat_1 = round(stat, 0)) %>% 
  select(rama, region, stat_1)

rama_2 <- rama[[2]] %>% 
  filter(label != "no fiable") %>% 
  group_by(region, rama) %>% 
  mutate(stat_2 = round(stat, 0)) %>% 
  select(rama, region, stat_2)

rama_2019 <- rama[[5]] %>% 
  filter(label != "no fiable") %>% 
  group_by(region, rama) %>% 
  mutate(stat_2019 = round(stat, 0)) %>% 
  select(rama, region, stat_2019)


txt_rama <- full_join(rama_1, rama_2, by = c("region", "rama")) %>% 
  mutate(dif_anual = ((stat_1 - stat_2)/stat_1)*100)
txt_rama <- full_join(txt_rama, ocup_total, by = c("region")) %>% 
  mutate(distrib_ocup_rama = (stat_1/ocup_total)*100)
txt_rama <- full_join(txt_rama, rama_2019, by = c("region", "rama")) %>% 
  mutate(dif_2019 = ((stat_1 - stat_2019)/stat_1)*100)
txt_rama <- txt_rama %>% 
  mutate(region = case_when(region == "Tarapacá" ~ 1, 
                            region == "Antofagasta" ~ 2, 
                            region == "Atacama" ~ 3, 
                            region == "Coquimbo" ~ 4, 
                            region == "Valparaíso" ~ 5, 
                            region == "O'Higgins" ~ 6, 
                            region == "Maule"~ 7, 
                            region == "Biobío" ~ 8, 
                            region == "Araucanía" ~ 9, 
                            region == "Los Lagos" ~ 10, 
                            region == "Aysen" ~ 11, 
                            region == "Magallanes y Antártica" ~ 12, 
                            region == "Metropolitana" ~ 13, 
                            region == "Los Ríos"  ~ 14, 
                            region == "Arica y Parinacota" ~ 15, 
                            region ==  "Ñuble" ~ 16,
                            TRUE ~ NA_real_))

txt_rama$dif_anual <- round(txt_rama$dif_anual, 2)
txt_rama$dif_2019 <- round(txt_rama$dif_2019, 2)
txt_rama$distrib_ocup_rama <- round(txt_rama$distrib_ocup_rama, 2)



# INACTIVIDAD

inact_1 <- inact[[1]] %>% 
  filter(label != "no fiable") %>% 
  group_by(region, razon_inact) %>% 
  mutate(stat_1 = round(stat, 0)) %>% 
  select(razon_inact, region, stat_1)

inact_2 <- inact[[2]] %>% 
  filter(label != "no fiable") %>% 
  group_by(region, razon_inact) %>% 
  mutate(stat_2 = round(stat, 0)) %>% 
  select(razon_inact, region, stat_2)


txt_inact <- full_join(inact_1, inact_2, by = c("region", "razon_inact")) %>% 
  mutate(dif_anual = ((stat_1 - stat_2)/stat_1)*100)
txt_inact <- txt_inact %>% 
  mutate(region = case_when(region == "Tarapacá" ~ 1, 
                            region == "Antofagasta" ~ 2, 
                            region == "Atacama" ~ 3, 
                            region == "Coquimbo" ~ 4, 
                            region == "Valparaíso" ~ 5, 
                            region == "O'Higgins" ~ 6, 
                            region == "Maule"~ 7, 
                            region == "Biobío" ~ 8, 
                            region == "Araucanía" ~ 9, 
                            region == "Los Lagos" ~ 10, 
                            region == "Aysen" ~ 11, 
                            region == "Magallanes y Antártica" ~ 12, 
                            region == "Metropolitana" ~ 13, 
                            region == "Los Ríos"  ~ 14, 
                            region == "Arica y Parinacota" ~ 15, 
                            region ==  "Ñuble" ~ 16,
                            TRUE ~ NA_real_))

txt_inact$dif_anual <- round(txt_inact$dif_anual, 2)


# inact mujeres
inact_1_m <- inact_sex[[1]] %>% 
  filter(label != "no fiable" & sexo == 2) %>% 
  group_by(region, razon_inact) %>% 
  mutate(stat_1 = round(stat, 0)) %>% 
  select(razon_inact, region, stat_1)

inact_2_m <- inact_sex[[2]] %>% 
  filter(label != "no fiable" & sexo == 2) %>% 
  group_by(region, razon_inact) %>% 
  mutate(stat_2 = round(stat, 0)) %>% 
  select(razon_inact, region, stat_2)


txt_inact_m <- full_join(inact_1_m, inact_2_m, by = c("region", "razon_inact")) %>% 
  mutate(dif_anual = ((stat_1 - stat_2)/stat_1)*100)
txt_inact_m <- txt_inact_m %>% 
  mutate(region = case_when(region == "Tarapacá" ~ 1, 
                            region == "Antofagasta" ~ 2, 
                            region == "Atacama" ~ 3, 
                            region == "Coquimbo" ~ 4, 
                            region == "Valparaíso" ~ 5, 
                            region == "O'Higgins" ~ 6, 
                            region == "Maule"~ 7, 
                            region == "Biobío" ~ 8, 
                            region == "Araucanía" ~ 9, 
                            region == "Los Lagos" ~ 10, 
                            region == "Aysen" ~ 11, 
                            region == "Magallanes y Antártica" ~ 12, 
                            region == "Metropolitana" ~ 13, 
                            region == "Los Ríos"  ~ 14, 
                            region == "Arica y Parinacota" ~ 15, 
                            region ==  "Ñuble" ~ 16,
                            TRUE ~ NA_real_))

txt_inact_m$dif_anual <- round(txt_inact_m$dif_anual, 2)


# inact hombres
inact_1_h <- inact_sex[[1]] %>% 
  filter(label != "no fiable" & sexo == 1) %>% 
  group_by(region, razon_inact) %>% 
  mutate(stat_1 = round(stat, 0)) %>% 
  select(razon_inact, region, stat_1)

inact_2_h <- inact_sex[[2]] %>% 
  filter(label != "no fiable" & sexo == 1) %>% 
  group_by(region, razon_inact) %>% 
  mutate(stat_2 = round(stat, 0)) %>% 
  select(razon_inact, region, stat_2)


txt_inact_h <- full_join(inact_1_h, inact_2_h, by = c("region", "razon_inact")) %>% 
  mutate(dif_anual = ((stat_1 - stat_2)/stat_1)*100)
txt_inact_h <- txt_inact_h %>% 
  mutate(region = case_when(region == "Tarapacá" ~ 1, 
                            region == "Antofagasta" ~ 2, 
                            region == "Atacama" ~ 3, 
                            region == "Coquimbo" ~ 4, 
                            region == "Valparaíso" ~ 5, 
                            region == "O'Higgins" ~ 6, 
                            region == "Maule"~ 7, 
                            region == "Biobío" ~ 8, 
                            region == "Araucanía" ~ 9, 
                            region == "Los Lagos" ~ 10, 
                            region == "Aysen" ~ 11, 
                            region == "Magallanes y Antártica" ~ 12, 
                            region == "Metropolitana" ~ 13, 
                            region == "Los Ríos"  ~ 14, 
                            region == "Arica y Parinacota" ~ 15, 
                            region ==  "Ñuble" ~ 16,
                            TRUE ~ NA_real_))

txt_inact_h$dif_anual <- round(txt_inact_h$dif_anual, 2)





## BRECHAS GENERO ----


txt_brecha_parti<- list(
  trim_act = tasa_parti_m[[1]] - tasa_parti_h[[1]],
  trim_12m = round((tasa_parti_m[[1]] - tasa_parti_h[[1]]) / (tasa_parti_m[[2]] - tasa_parti_h[[2]]),1)
  
)


txt_brecha_desocup<- list(
  trim_act = tasa_desocup_m[[1]] - tasa_desocup_h[[1]],
  trim_12m = round((tasa_desocup_m[[1]] - tasa_desocup_h[[1]]) / (tasa_desocup_m[[2]] - tasa_desocup_h[[2]]),1)
  
)


## EDUCACION ----
# ocupacion por nivel educacional

ocup_educ_1 <- educ_ocup[[1]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(ocup_educ_1 = round(stat,0)) %>% 
  select(region, educ, ocup_educ_1)

ocup_educ_2 <- educ_ocup[[2]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(ocup_educ_2 = round(stat,0)) %>% 
  select(region, educ, ocup_educ_2)

ocup_educ_2019 <- educ_ocup[[5]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(ocup_educ_2019 = round(stat,0)) %>% 
  select(region, educ, ocup_educ_2019)


pet_educ_1 <- educ_pet[[1]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(pet_educ_1 = round(stat,0)) %>% 
  select(region, educ, pet_educ_1)

pet_educ_2 <- educ_pet[[2]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(pet_educ_2 = round(stat,0)) %>% 
  select(region, educ, pet_educ_2)

pet_educ_2019 <- educ_pet[[5]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(pet_educ_2019 = round(stat,0)) %>% 
  select(region, educ, pet_educ_2019)


txt_educ_ocup <- full_join(ocup_educ_1, ocup_educ_2, by = c("region", "educ"))
txt_educ_ocup <- full_join(txt_educ_ocup, ocup_educ_2019, by = c("region", "educ"))
txt_educ_ocup <- full_join(txt_educ_ocup, pet_educ_1, by = c("region", "educ")) 
txt_educ_ocup <- full_join(txt_educ_ocup, pet_educ_2, by = c("region", "educ")) 
txt_educ_ocup <- full_join(txt_educ_ocup, pet_educ_2019, by = c("region", "educ")) 

txt_educ_ocup <- txt_educ_ocup %>% 
  mutate(tasa_ocup_educ_1 = (ocup_educ_1 / pet_educ_1)*100,
         tasa_ocup_educ_2 = (ocup_educ_2 / pet_educ_2)*100,
         tasa_ocup_educ_2019 = (ocup_educ_2019 / pet_educ_2019)*100)

txt_educ_ocup <- txt_educ_ocup %>% 
  mutate(region = case_when(region == "Tarapacá" ~ 1, 
                            region == "Antofagasta" ~ 2, 
                            region == "Atacama" ~ 3, 
                            region == "Coquimbo" ~ 4, 
                            region == "Valparaíso" ~ 5, 
                            region == "O'Higgins" ~ 6, 
                            region == "Maule"~ 7, 
                            region == "Biobío" ~ 8, 
                            region == "Araucanía" ~ 9, 
                            region == "Los Lagos" ~ 10, 
                            region == "Aysen" ~ 11, 
                            region == "Magallanes y Antártica" ~ 12, 
                            region == "Metropolitana" ~ 13, 
                            region == "Los Ríos"  ~ 14, 
                            region == "Arica y Parinacota" ~ 15, 
                            region ==  "Ñuble" ~ 16,
                            TRUE ~ NA_real_))

txt_educ_ocup$tasa_ocup_educ_1 <- round(txt_educ_ocup$tasa_ocup_educ_1, 2)
txt_educ_ocup$tasa_ocup_educ_2 <- round(txt_educ_ocup$tasa_ocup_educ_2, 2)
txt_educ_ocup$tasa_ocup_educ_2019 <- round(txt_educ_ocup$tasa_ocup_educ_2019, 2)



# desocupacion por nivel educacional

desocup_educ_1 <- educ_desocup[[1]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(desocup_educ_1 = round(stat,0)) %>% 
  select(region, educ, desocup_educ_1)

desocup_educ_2 <- educ_desocup[[2]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(desocup_educ_2 = round(stat,0)) %>% 
  select(region, educ, desocup_educ_2)

desocup_educ_2019 <- educ_desocup[[5]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(desocup_educ_2019 = round(stat,0)) %>% 
  select(region, educ, desocup_educ_2019)


fdt_educ_1 <- educ_fdt[[1]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(fdt_educ_1 = round(stat,0)) %>% 
  select(region, educ, fdt_educ_1)

fdt_educ_2 <- educ_fdt[[2]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(fdt_educ_2 = round(stat,0)) %>% 
  select(region, educ, fdt_educ_2)

fdt_educ_2019 <- educ_fdt[[5]] %>% 
  filter(label != "no fiable") %>%
  group_by(region) %>% 
  mutate(fdt_educ_2019 = round(stat,0)) %>% 
  select(region, educ, fdt_educ_2019)


txt_educ_desocup <- full_join(desocup_educ_1, desocup_educ_2, by = c("region", "educ"))
txt_educ_desocup <- full_join(txt_educ_desocup, desocup_educ_2019, by = c("region", "educ"))
txt_educ_desocup <- full_join(txt_educ_desocup, fdt_educ_1, by = c("region", "educ")) 
txt_educ_desocup <- full_join(txt_educ_desocup, fdt_educ_2, by = c("region", "educ")) 
txt_educ_desocup <- full_join(txt_educ_desocup, fdt_educ_2019, by = c("region", "educ")) 

txt_educ_desocup <- txt_educ_desocup %>% 
  mutate(tasa_desocup_educ_1 = (desocup_educ_1 / fdt_educ_1)*100,
         tasa_desocup_educ_2 = (desocup_educ_2 / fdt_educ_2)*100,
         tasa_desocup_educ_2019 = (desocup_educ_2019 / fdt_educ_2019)*100)

txt_educ_desocup <- txt_educ_desocup %>% 
  mutate(region = case_when(region == "Tarapacá" ~ 1, 
                            region == "Antofagasta" ~ 2, 
                            region == "Atacama" ~ 3, 
                            region == "Coquimbo" ~ 4, 
                            region == "Valparaíso" ~ 5, 
                            region == "O'Higgins" ~ 6, 
                            region == "Maule"~ 7, 
                            region == "Biobío" ~ 8, 
                            region == "Araucanía" ~ 9, 
                            region == "Los Lagos" ~ 10, 
                            region == "Aysen" ~ 11, 
                            region == "Magallanes y Antártica" ~ 12, 
                            region == "Metropolitana" ~ 13, 
                            region == "Los Ríos"  ~ 14, 
                            region == "Arica y Parinacota" ~ 15, 
                            region ==  "Ñuble" ~ 16,
                            TRUE ~ NA_real_))

txt_educ_desocup$tasa_desocup_educ_1 <- round(txt_educ_desocup$tasa_desocup_educ_1, 2)
txt_educ_desocup$tasa_desocup_educ_2 <- round(txt_educ_desocup$tasa_desocup_educ_2, 2)
txt_educ_desocup$tasa_desocup_educ_2019 <- round(txt_educ_desocup$tasa_desocup_educ_2019, 2)




## INFORMALIDAD ----

# Cantidad informales

txt_informal_n <- list(
  
  trim_act = round(informal[[1]] %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat),0),
  
  trim_12m = round(mapply("-", informal[[1]] %>% 
                            select(stat, label) %>% 
                            mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                            summarise(stat),
                          informal[[2]] %>%
                            select(stat, label) %>% 
                            mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                            summarise(stat)),0)
)


txt_informal_h <- list(
  
  
  trim_act = round(informal_sex[[1]] %>% 
                     filter(sexo == 1) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat),0),
  trim_12m = round(mapply("-", informal_sex[[1]] %>% 
                            filter(sexo == 1) %>% 
                            select(stat, label) %>% 
                            mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                            summarise(stat),
                          informal_sex[[2]] %>%
                            filter(sexo == 1) %>% 
                            select(stat, label) %>% 
                            mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                            summarise(stat)),0)
  
)

txt_informal_m <- list(
  
  
  trim_act = round(informal_sex[[1]] %>% 
                     filter(sexo == 2) %>% 
                     select(stat, label) %>% 
                     mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                     summarise(stat),0),
  trim_12m = round(mapply("-", informal_sex[[1]] %>% 
                            filter(sexo == 2) %>% 
                            select(stat, label) %>% 
                            mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                            summarise(stat),
                          informal_sex[[2]] %>%
                            filter(sexo == 2) %>% 
                            select(stat, label) %>% 
                            mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                            summarise(stat)),0)
  
)

# TOI

txt_toi_n <- list(
  trim_act = tasa_informal_n[[1]],
  trim_12m = tasa_informal_n[[1]] - tasa_informal_n[[2]]
)

txt_toi_h <- list(
  trim_act = tasa_informal_h[[1]],
  trim_12m = tasa_informal_h[[1]] - tasa_informal_h[[2]]
)

txt_toi_m <- list(
  trim_act = tasa_informal_m[[1]],
  trim_12m = tasa_informal_m[[1]] - tasa_informal_m[[2]]
)


# Cantidad asal informales

txt_asal_informal_n <- list(
  
  trim_act = asal_informal[[1]] %>% 
    filter(informal == 1) %>% 
    mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
    summarise(stat = round(stat,0)),
  
  trim_12m = round(mapply("-", asal_informal[[1]] %>% 
                            filter(informal == 1) %>%
                            select(stat, label) %>% 
                            mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                            summarise(stat),
                          asal_informal[[2]] %>%
                            filter(informal == 1) %>%
                            select(stat, label) %>% 
                            mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                            summarise(stat)),0)
  
  
)



txt_asal_informal_h <- list(
  
  
  trim_act = asal_informal_sex[[1]] %>% 
    filter(sexo == 1 & informal == 1) %>% 
    mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
    summarise(stat = round(stat,0)),
  
  trim_12m = round(mapply("-", asal_informal_sex[[1]] %>% 
                            filter(sexo == 1 & informal == 1) %>%
                            select(stat, label) %>% 
                            mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                            summarise(stat),
                          asal_informal_sex[[2]] %>%
                            filter(sexo == 1 & informal == 1) %>%
                            select(stat, label) %>% 
                            mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                            summarise(stat)),0)
  
  
)

txt_asal_informal_m <- list(
  
  trim_act = asal_informal_sex[[1]] %>% 
    filter(sexo == 2 & informal == 1) %>% 
    mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
    summarise(stat = round(stat,0)),
  
  trim_12m = round(mapply("-", asal_informal_sex[[1]] %>% 
                            filter(sexo == 2 & informal == 1) %>%
                            select(stat, label) %>% 
                            mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                            summarise(stat),
                          asal_informal_sex[[2]] %>%
                            filter(sexo == 2 & informal == 1) %>%
                            select(stat, label) %>% 
                            mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                            summarise(stat)),0)
  
)

# Tasa asal informales

txt_tasa_asal_informal_n <- list(
  
  trim_act = tasa_asal_informal_n[[1]],
  trim_12m = tasa_asal_informal_n[[1]] - tasa_asal_informal_n[[2]]
  
)  


txt_tasa_asal_informal_h <- list(
  
  trim_act = tasa_asal_informal_h[[1]],
  trim_12m = tasa_asal_informal_h[[1]] - tasa_asal_informal_h[[2]]
  
)  


txt_tasa_asal_informal_m <- list(
  
  trim_act = tasa_asal_informal_m[[1]],
  trim_12m = tasa_asal_informal_m[[1]] - tasa_asal_informal_m[[2]]
  
)  




# 5. Parametrizar regiones -------------------------------------------------

# crear df con regiones
df <- fdt[[1]] %>% 
  select(reg=region) %>% 
  rowid_to_column(var = "region") %>%
  select(region)


# FUERZA DE TRABAJO

txt_fdt_n <- lapply(txt_fdt_n, function(x) { 
  bind_cols(x, df)
})


txt_ocup_n <- lapply(txt_ocup_n, function(x) { 
  bind_cols(x, df)
})


txt_desocup_n <- lapply(txt_desocup_n, function(x) { 
  bind_cols(x, df)
})


txt_fuera_fza_n <- lapply(txt_fuera_fza_n, function(x) { 
  bind_cols(x, df)
})


txt_tasa_parti_n <- lapply(txt_tasa_parti_n, function(x) { 
  bind_cols(x, df)
})


txt_var_tasa_parti_n <- lapply(txt_var_tasa_parti_n, function(x) { 
  bind_cols(x, df)
})


txt_tasa_ocup_n <- lapply(txt_tasa_ocup_n, function(x) { 
  bind_cols(x, df)
})


txt_var_tasa_ocup_n <- lapply(txt_var_tasa_ocup_n, function(x) { 
  bind_cols(x, df)
})


txt_tasa_desocup_n <- lapply(txt_tasa_desocup_n, function(x) { 
  bind_cols(x, df)
})


txt_var_tasa_desocup_n <- lapply(txt_var_tasa_desocup_n, function(x) { 
  bind_cols(x, df)
})


txt_tasa_informal_n <- lapply(txt_tasa_informal_n, function(x) { 
  bind_cols(x, df)
})


txt_var_tasa_informal_n <- lapply(txt_var_tasa_informal_n, function(x) { 
  bind_cols(x, df)
})


txt_fdt_h <- lapply(txt_fdt_h, function(x) { 
  bind_cols(x, df)
})


txt_fdt_m <- lapply(txt_fdt_m, function(x) { 
  bind_cols(x, df)
})


txt_ocup_h <- lapply(txt_ocup_h, function(x) { 
  bind_cols(x, df)
})


txt_ocup_m <- lapply(txt_ocup_m, function(x) { 
  bind_cols(x, df)
})


txt_fuera_fza_h <- lapply(txt_fuera_fza_h, function(x) { 
  bind_cols(x, df)
})


txt_fuera_fza_m <- lapply(txt_fuera_fza_m, function(x) { 
  bind_cols(x, df)
})


txt_desocup_h <- lapply(txt_desocup_h, function(x) { 
  bind_cols(x, df)
})


txt_desocup_m <- lapply(txt_desocup_m, function(x) { 
  bind_cols(x, df)
})





txt_tasa_parti_h <- lapply(txt_tasa_parti_h, function(x) { 
  bind_cols(x, df)
})


txt_tasa_parti_m <- lapply(txt_tasa_parti_m, function(x) { 
  bind_cols(x, df)
})



txt_tasa_ocup_h <- lapply(txt_tasa_ocup_h, function(x) { 
  bind_cols(x, df)
})


txt_tasa_ocup_m <- lapply(txt_tasa_ocup_m, function(x) { 
  bind_cols(x, df)
})



txt_tasa_desocup_h <- lapply(txt_tasa_desocup_h, function(x) { 
  bind_cols(x, df)
})


txt_tasa_desocup_m <- lapply(txt_tasa_desocup_m, function(x) { 
  bind_cols(x, df)
})


txt_tasa_informal_h <- lapply(txt_tasa_informal_h, function(x) { 
  bind_cols(x, df)
})


txt_tasa_informal_m <- lapply(txt_tasa_informal_m, function(x) { 
  bind_cols(x, df)
})


txt_var_tasa_parti_h <- lapply(txt_var_tasa_parti_h, function(x) { 
  bind_cols(x, df)
})


txt_var_tasa_parti_m <- lapply(txt_var_tasa_parti_m, function(x) { 
  bind_cols(x, df)
})



txt_var_tasa_ocup_h <- lapply(txt_var_tasa_ocup_h, function(x) { 
  bind_cols(x, df)
})


txt_var_tasa_ocup_m <- lapply(txt_var_tasa_ocup_m, function(x) { 
  bind_cols(x, df)
})



txt_var_tasa_desocup_h <- lapply(txt_var_tasa_desocup_h, function(x) { 
  bind_cols(x, df)
})


txt_var_tasa_desocup_m <- lapply(txt_var_tasa_desocup_m, function(x) { 
  bind_cols(x, df)
})


txt_var_tasa_informal_h <- lapply(txt_var_tasa_informal_h, function(x) { 
  bind_cols(x, df)
})


txt_var_tasa_informal_m <- lapply(txt_var_tasa_informal_m, function(x) { 
  bind_cols(x, df)
})


# CREACION EMPLEO

txt_emple_formal_n <- lapply(txt_emple_formal_n, function(x) { 
  bind_cols(x, df)
})

txt_emple_formal_h<- lapply(txt_emple_formal_h, function(x) { 
  bind_cols(x, df)
})

txt_emple_formal_m<- lapply(txt_emple_formal_m, function(x) { 
  bind_cols(x, df)
})

# BRECHAS GENERO

txt_brecha_parti <- lapply(txt_brecha_parti, function(x) { 
  bind_cols(x, df)
})


txt_brecha_desocup <- lapply(txt_brecha_desocup, function(x) { 
  bind_cols(x, df)
})


## INFORMALIDAD
txt_informal_n <- lapply(txt_informal_n, function(x){
  bind_cols(x, df)
})


txt_informal_h<- lapply(txt_informal_h, function(x){
  bind_cols(x, df)
})

txt_informal_m<- lapply(txt_informal_m, function(x){
  bind_cols(x, df)
})

txt_toi_n<- lapply(txt_toi_n, function(x){
  bind_cols(x, df)
})

txt_toi_h<- lapply(txt_toi_h, function(x){
  bind_cols(x, df)
})

txt_toi_m<- lapply(txt_toi_m, function(x){
  bind_cols(x, df)
})

txt_asal_informal_n<- lapply(txt_asal_informal_n, function(x){
  bind_cols(x, df)
})

txt_asal_informal_h<- lapply(txt_asal_informal_h, function(x){
  bind_cols(x, df)
})

txt_asal_informal_m<- lapply(txt_asal_informal_m, function(x){
  bind_cols(x, df)
})


txt_tasa_asal_informal_n<- lapply(txt_tasa_asal_informal_n, function(x){
  bind_cols(x, df)
})

txt_tasa_asal_informal_h<- lapply(txt_tasa_asal_informal_h, function(x){
  bind_cols(x, df)
})

txt_tasa_asal_informal_m<- lapply(txt_tasa_asal_informal_m, function(x){
  bind_cols(x, df)
})



txt_crea_ocup <- list(
  nacional = mapply("-", ocup[[1]] %>% 
                      select(stat, label) %>% 
                      mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                      summarise(stat = round(stat, 0)),
                    ocup[[2]] %>% 
                      select(stat, label) %>% 
                      mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                      summarise(stat = round(stat, 0))),
  h = mapply("-", ocup_sex[[1]] %>% 
               filter(sexo == 1) %>% 
               select(stat, label) %>% 
               mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
               summarise(stat = round(stat, 0)),
             ocup_sex[[2]] %>% 
               filter(sexo == 1) %>%
               select(stat, label) %>% 
               mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
               summarise(stat = round(stat, 0))),
  m = mapply("-", ocup_sex[[1]] %>% 
               filter(sexo == 2) %>% 
               select(stat, label) %>% 
               mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
               summarise(stat = round(stat, 0)),
             ocup_sex[[2]] %>% 
               filter(sexo == 2) %>%
               select(stat, label) %>% 
               mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
               summarise(stat = round(stat, 0)))
  
) %>% 
  lapply(., function(x) { 
    bind_cols(x, df)
  })




brecha_parti_n <- mapply("-", round(mapply("/", fdt_brecha[[1]] %>% 
                                             filter(sexo == 2) %>% 
                                             mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                                             summarise(stat), pet_brecha[[1]] %>%
                                             filter(sexo == 2) %>% 
                                             mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                                             summarise(stat))*100, 1), round(mapply("/", fdt_brecha[[1]] %>% 
                                                                                      filter(sexo == 1) %>% 
                                                                                      mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                                                                                      summarise(stat), pet_brecha[[1]] %>%
                                                                                      filter(sexo == 1) %>% 
                                                                                      mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                                                                                      summarise(stat))*100, 1)) 




brecha_desocup_n <- mapply("-", round(mapply("/", desocup_brecha[[1]] %>% 
                                               filter(sexo == 2) %>% 
                                               mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                                               summarise(stat), fdt_brecha[[1]] %>%
                                               filter(sexo == 2) %>% 
                                               mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                                               summarise(stat))*100, 1), round(mapply("/", desocup_brecha[[1]] %>% 
                                                                                        filter(sexo == 1) %>% 
                                                                                        mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                                                                                        summarise(stat), fdt_brecha[[1]] %>%
                                                                                        filter(sexo == 1) %>% 
                                                                                        mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                                                                                        summarise(stat))*100, 1)) 







