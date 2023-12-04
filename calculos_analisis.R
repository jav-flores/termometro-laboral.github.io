#*******************************************************************************************************************
#
#             Proyecto: Automatización de informe Termómetro Laboral
#             Tema: Manipulación y estimación indicadores
#             Fecha versión: 28/11/2023
#             Tipo de archivo: Cálculos ENE-regional
#
#
#******************************************************************************************************************



# 1. Cargar librerías y datos ---------------------------------------------

source(file = "R/scripts/data.R", encoding = "UTF-8")


options(survey.lonely.psu = "certainty")

# 2. Manipulación ---------------------------------------------------------


ene <- lapply(ene, function(x) {  
  data.frame(x) %>%
    mutate(pet = if_else(edad >= 15, 1, 0), 
           fdt = if_else(cae_especifico >= 1 & cae_especifico <= 9, 1, 0),
           ocupado = if_else(cae_especifico >= 1 & cae_especifico <= 7, 1, 0),
           desocupado = if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0),
           fuera_fza = if_else(activ == 3, 1, 0),
           informal = if_else(ocup_form == 2, 1, 0),
           formal = if_else(ocup_form == 1, 1, 0),
           asal = if_else(categoria_ocupacion %in% c(3:6),1,0),
           
           region = case_when(region == 1 ~ "Tarapacá", 
                              region == 2 ~ "Antofagasta", 
                              region == 3 ~ "Atacama", 
                              region == 4 ~ "Coquimbo", 
                              region == 5 ~ "Valparaíso", 
                              region == 6 ~ "O'Higgins", 
                              region == 7 ~ "Maule", 
                              region == 8 ~ "Biobío", 
                              region == 9 ~ "Araucanía", 
                              region == 10 ~ "Los Lagos", 
                              region == 11 ~ "Aysen", 
                              region == 12 ~ "Magallanes y Antártica", 
                              region == 13 ~ "Metropolitana", 
                              region == 14 ~ "Los Ríos", 
                              region == 15 ~ "Arica y Parinacota", 
                              region == 16 ~ "Ñuble",
                              TRUE ~ NA_character_),
           region = factor(region, 
                           levels = c("Tarapacá", 
                                      "Antofagasta", 
                                      "Atacama", 
                                      "Coquimbo", 
                                      "Valparaíso", 
                                      "O'Higgins", 
                                      "Maule", 
                                      "Biobío", 
                                      "Araucanía", 
                                      "Los Lagos", 
                                      "Aysen", 
                                      "Magallanes y Antártica", 
                                      "Metropolitana", 
                                      "Los Ríos", 
                                      "Arica y Parinacota", 
                                      "Ñuble")),

           educ = case_when(nivel %in% (0:2) ~ "Nunca estudió",
                            nivel == 3  ~ "Básica o menos",
                            (nivel %in% (4:6) & termino_nivel == 2)~ "Media incompleta",
                            (nivel %in% (4:6) & termino_nivel == 1)~ "Media completa",
                            (nivel %in% (7:8) & termino_nivel == 2)~ "Técnica superior incompleta",
                            (nivel %in% (7:8) & termino_nivel == 1)~ "Técnica superior completa",
                            (nivel == 9 & termino_nivel == 2)~ "Universitaria incompleta",
                            (nivel == 9 & termino_nivel == 1)~ "Universitaria completa",
                            nivel %in% (10:14) ~ "Postgrado",
                            nivel %in% (88:902) ~ "No sabe no responde"),
           
           educ = factor(educ,
                         levels = c("Nunca estudió",
                                    "Básica o menos",
                                    "Media incompleta",
                                    "Media completa",
                                    "Técnica superior incompleta",
                                    "Técnica superior completa",
                                    "Universitaria incompleta",
                                    "Universitaria completa",
                                    "Postgrado",
                                    "No sabe no responde")),
           
           tramo_edad = case_when(edad %in% c(15:17) ~ "Entre 15 y 17 años",
                                  edad %in% c(18:24) ~ "Entre 18 y 24 años",
                                  edad %in% c(25:34) ~ "Entre 25 y 34 años",
                                  edad %in% c(35:49) ~ "Entre 35 y 49 años",
                                  edad %in% c(50:59) ~ "Entre 50 y 59 años",
                                  edad %in% c(60:105) ~ "Entre 60 años y más"),
           tramo_edad = factor(tramo_edad,
                               levels = c("Entre 15 y 17 años",
                                          "Entre 18 y 24 años",
                                          "Entre 25 y 34 años",
                                          "Entre 35 y 49 años",
                                          "Entre 50 y 59 años",
                                          "Entre 60 años y más")),
           
           rama = case_when(b14_rev4cl_caenes == 1 ~ "Agricultura, ganadería, silvicultura y pesca",
                            b14_rev4cl_caenes == 2 ~ "Explotación de minas y canteras", 
                            b14_rev4cl_caenes == 3 ~ "Industrias manufactureras",
                            b14_rev4cl_caenes == 4 ~ "Suministro de electricidad, gas, vapor y aire",
                            b14_rev4cl_caenes == 5 ~ "Suministro de agua",
                            b14_rev4cl_caenes == 6 ~ "Construcción",
                            b14_rev4cl_caenes == 7 ~ "Comercio al por mayor y al por menor",
                            b14_rev4cl_caenes == 8 ~ "Transporte y almacenamiento",
                            b14_rev4cl_caenes == 9 ~ "Activ. de alojamiento y de servicio de comidas",
                            b14_rev4cl_caenes == 10 ~ "Información y comunicaciones",
                            b14_rev4cl_caenes == 11 ~ "Activ. financieras y de seguros",
                            b14_rev4cl_caenes == 12 ~ "Activ. inmobiliarias",
                            b14_rev4cl_caenes == 13 ~ "Activ. profesionales, científicas y técnicas",
                            b14_rev4cl_caenes == 14 ~ "Activ. de servicios administrativos y de apoyo",
                            b14_rev4cl_caenes == 15 ~ "Administración pública y defensa",
                            b14_rev4cl_caenes == 16 ~ "Enseñanza",
                            b14_rev4cl_caenes == 17 ~ "Activ. de atención de la salud y de asist. social",
                            b14_rev4cl_caenes == 18 ~ "Activ. artísticas, de entretenimiento y recreativas",
                            b14_rev4cl_caenes == 19 ~ "Otras actividades de servicios",
                            b14_rev4cl_caenes == 20 ~ "Activ. de los hogares como empleadores",
                            b14_rev4cl_caenes == 21 ~ "Activ. de organizaciones extraterritoriales"),
           rama = factor(rama, 
                         levels = c("Agricultura, ganadería, silvicultura y pesca",
                                    "Explotación de minas y canteras", 
                                    "Industrias manufactureras",
                                    "Suministro de electricidad, gas, vapor y aire",
                                    "Suministro de agua",
                                    "Construcción",
                                    "Comercio al por mayor y al por menor",
                                    "Transporte y almacenamiento",
                                    "Activ. de alojamiento y de servicio de comidas",
                                    "Información y comunicaciones",
                                    "Activ. financieras y de seguros",
                                    "Activ. inmobiliarias",
                                    "Activ. profesionales, científicas y técnicas",
                                    "Activ. de servicios administrativos y de apoyo",
                                    "Administración pública y defensa",
                                    "Enseñanza",
                                    "Activ. de atención de la salud  y de asist. social",
                                    "Activ. artísticas, de entretenimiento y recreativas",
                                    "Otras actividades de servicios",
                                    "Activ. de los hogares como empleadores",
                                    "Activ. de organizaciones extraterritoriales")),
           
           categoria_ocup = case_when(categoria_ocupacion == 0 ~ "No corresponde",
                                      categoria_ocupacion == 1 ~ "Empleador",
                                      categoria_ocupacion == 2 ~ "Cuenta propia",
                                      categoria_ocupacion == 3 ~ "Asalariado sector privado",
                                      categoria_ocupacion == 4 ~ "Asalariado sector público",
                                      categoria_ocupacion == 5 ~ "Person. de serv. doméstico puertas afuera",
                                      categoria_ocupacion == 6 ~ "Person. de serv. doméstico puertas adentro",
                                      categoria_ocupacion == 7 ~ "Familiar o personal no remunerado"),
           categoria_ocup = factor(categoria_ocup,
                                   levels= c("No corresponde",
                                             "Empleador",
                                             "Cuenta propia",
                                             "Asalariado sector privado",
                                             "Asalariado sector público",
                                             "Person. de serv. doméstico puertas afuera",
                                             "Person. de serv. doméstico puertas adentro",
                                             "Familiar o personal no remunerado")),
           
           razon_inact = case_when((e9 == 1 | e9 == 2 | e9 == 13 ) ~ "Próximo a comenzar un trabajo",
                                   (e9 == 3) ~ "Por responsabilidades familiares permanentes",
                                   e9 == 4 ~ "Está estudiando o preparando estudios",
                                   (e9 == 5 | e9 == 6 | e9 == 7 ) ~ "Jubilado(a), pensionado(a) o rentista",
                                   e9 == 8 ~ "Por motivos de salud permanentes",
                                   (e9 == 10 | e9 == 12) ~ "Por motivos de salud temporal", 
                                   e9 == 11 ~ "Por responsabilidades familiares de carácter temporal",
                                   (e9 == 9 | e9 == 14 | e9 == 15 | e9 == 16 | e9 == 17 | e9 == 18 | e9 == 19 | e9 == 20  | e9 == 22) ~ "Otra razón",
                                   e9 == 21 ~ "No quiere, no necesita trabajar"),
           razon_inact = factor(razon_inact,
                                levels = c("Próximo a comenzar un trabajo",
                                           "Por responsabilidades familiares permanentes",
                                           "Está estudiando o preparando estudios",
                                           "Jubilado(a), pensionado(a) o rentista",
                                           "Por motivos de salud permanentes",
                                           "Por motivos de salud temporal", 
                                           "Por responsabilidades familiares de carácter temporal",
                                           "Otra razón",
                                           "No quiere, no necesita trabajar"),))})

# 2.1. Diseño muestral complejo ----

dc_ene <- list(
  svydesign(ids = ~conglomerado , strata = ~estrato, data = ene[[1]], weights = ~fact_cal),
  svydesign(ids = ~conglomerado , strata = ~estrato, data = ene[[2]], weights = ~fact_cal),
  svydesign(ids = ~conglomerado , strata = ~estrato, data = ene[[3]], weights = ~fact_cal),
  svydesign(ids = ~conglomerado , strata = ~estrato, data = ene[[4]], weights = ~fact_cal),
  svydesign(ids = ~conglomerado , strata = ~estrato, data = ene[[5]], weights = ~fact_cal)
)


# ------------------ INDICE ------------------

# 3.1	Estadisticas totales por región
# 3.1.1   Población en edad de trabajar (PET)
# 3.1.2   Fuerza de trabajo (FDT)
# 3.1.3   Ocupados
# 3.1.4   Desocupados
# 3.1.5   Fuera de la fuerza de trabajo
# 3.1.6   Informal
# 3.1.7   Formal

# 3.2	Estadisticas totales nacional
# 3.2.1   Población en edad de trabajar (PET)
# 3.2.2   Fuerza de trabajo (FDT)
# 3.2.3   Ocupados
# 3.2.4   Desocupados
# 3.2.5   Fuera de la fuerza de trabajo
# 3.2.6   Informal
# 3.2.7   Formal

# 3.3 	Estadisticas por sexo y región 
# 3.3.1   Población en edad de trabajar (PET) por sexo
# 3.3.2   Fuerza de trabajo (FDT) por sexo
# 3.3.3   Ocupados por sexo
# 3.3.4   Desocupados por sexo
# 3.3.5   Fuera de la fuerza de trabajo por sexo
# 3.3.6   Informal por sexo
# 3.3.7   Formal por sexo

# 3.4	Rama por región
# 3.4.1   Ocupados segun rama	

# 3.5 	Categoría ocupacional por región 
# 3.5.1   Ocupados segun categoria ocupacional
# 3.5.2   Distribución de ocupados por categoria ocupacional

# 3.6 	educ por region
# 3.6.1   Ocupados segun educ

# 3.7	Ocupacion por tramo etario y region
# 3.7.1   Ocupados por tramo etario
# 3.7.2   PET por tramo etario 

# 3.8	Asalariados 
# 3.8.1   Asalariados totales
# 3.8.2   Asalariados informales
# 3.8.3   Asalariados totales por sexo
# 3.8.4   Asalariados informales por sexo

# 3.9 	Brechas de genero


# 3. Cálculos -------------------------------------------------------------


## 3.1 Estadisticas totales por región

pet <- list()
ocup <- list()
fdt <- list()
desocup <- list()
informal <- list()
formal <- list()
fuera_fza <- list()



i <- 1
for (i in seq_along(dc_ene)) {
  
  # 3.1.1   Población en edad de trabajar (PET)
  pet[[i]] <- create_total(var = "pet", domains = "region", subpop = "pet", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(region, stat, label)
  
  # 3.1.2   Fuerza de trabajo (FDT)
  fdt[[i]] <- create_total(var = "fdt", domains = "region",subpop = "fdt", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(region, stat, label)
  
  # 3.1.3   Ocupados
  ocup[[i]] <- create_total(var = "ocupado", domains = "region",subpop = "fdt", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(region, stat, label)
  
  # 3.1.4   Desocupados
  desocup[[i]] <- create_total(var = "desocupado", domains = "region",subpop = "fdt", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(region, stat, label)
  
  # 3.1.5   Fuera de la fuerza de trabajo
  fuera_fza[[i]] <- create_total(var = "fuera_fza", domains = "region" ,subpop = "pet", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(region, stat, label)
  
  # 3.1.6   Informal
  informal[[i]] <- create_total(var = "informal", domains = "region",subpop = "ocupado", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(region, stat, label)
  
  # 3.1.7   Formal
  formal[[i]] <- create_total(var = "formal", domains = "region",subpop = "ocupado", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(region, stat, label)
  
  i <- i+1
  
}



# 3.2	Estadisticas totales nacional

ocup_nacional <- list()
fdt_nacional <- list()
desocup_nacional <- list()
pet_nacional <- list()
informal_nacional <- list()
formal_nacional <- list()
fuera_fza_nacional <- list()
tasa_parti_nacional <- list()


i <- 1
for (i in seq_along(dc_ene)) {
  
  # 3.2.1   Población en edad de trabajar (PET)
  pet_nacional[[i]] <- create_total(var = "pet", subpop = "pet", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(stat, label)
  
  # 3.2.2   Fuerza de trabajo (FDT)
  fdt_nacional[[i]] <- create_total(var = "fdt",subpop = "fdt", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(stat, label)
  
  # 3.2.3   Ocupados
  ocup_nacional[[i]] <- create_total(var = "ocupado",subpop = "fdt", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(stat, label)
  
  # 3.2.4   Desocupados
  desocup_nacional[[i]] <- create_total(var = "desocupado",subpop = "fdt", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(stat, label)
  
  # 3.2.5   Fuera de la fuerza de trabajo
  fuera_fza_nacional[[i]] <- create_total(var = "fuera_fza" ,subpop = "pet", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(stat, label)
  
  # 3.2.6   Informal
  informal_nacional[[i]] <- create_total(var = "informal",subpop = "ocupado", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(stat, label)
  
  ## 3.2.7   Formal
  formal_nacional[[i]] <- create_total(var = "formal",subpop = "ocupado", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(stat, label)
  
  i <- i+1
  
}

## 3.2.8 tasa de participación
tasa_parti_nacional <- list(
  round(mapply("/", fdt_nacional[[1]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), pet_nacional[[1]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", fdt_nacional[[2]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), pet_nacional[[2]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", fdt_nacional[[3]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), pet_nacional[[3]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", fdt_nacional[[4]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), pet_nacional[[4]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", fdt_nacional[[5]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), pet_nacional[[5]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1))




# 3.3 Estadisticas por sexo y región 

ocup_sex <- list()
fdt_sex <- list()
desocup_sex <- list()
pet_sex <- list()
informal_sex <- list()
formal_sex <- list()
fuera_fza_sex <- list()


i <- 1
for (i in seq_along(dc_ene)) {
  
  # 3.3.1   Población en edad de trabajar (PET) por sexo
  pet_sex[[i]] <- create_total(var = "pet", domains = "sexo+region", subpop = "pet", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(sexo, region, stat, label)
  
  # 3.3.2   Fuerza de trabajo (FDT) por sexo
  fdt_sex[[i]] <- create_total(var = "fdt", domains = "sexo+region",subpop = "fdt", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(sexo, region, stat, label)
  
  # 3.3.3   Ocupados por sexo
  ocup_sex[[i]] <- create_total(var = "ocupado", domains = "sexo+region",subpop = "fdt", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(sexo, region, stat, label)
  
  # 3.3.4   Desocupados por sexo
  desocup_sex[[i]] <- create_total(var = "desocupado", domains = "sexo+region",subpop = "fdt", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(sexo, region, stat, label)
  
  # 3.3.5   Fuera de la fuerza de trabajo por sexo
  fuera_fza_sex[[i]] <- create_total(var = "fuera_fza", domains = "sexo+region" ,subpop = "pet", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(sexo, region, stat, label)
  
  # 3.3.6   Informal por sexo
  informal_sex[[i]] <- create_total(var = "informal", domains = "sexo+region",subpop = "ocupado", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(sexo, region, stat, label)
  
  # 3.3.7   Formal por sexo
  formal_sex[[i]] <- create_total(var = "formal", domains = "sexo+region",subpop = "ocupado", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(sexo, region, stat, label)
  
  i <- i+1
  
}



# 3.4	Rama por región

rama <- list()

i <- 1
for (i in seq_along(dc_ene)) {
  
  # 3.4.1   Ocupados por rama 
  rama[[i]] <-   create_total(var = "ocupado", domains = "rama+region", subpop = "fdt", design = dc_ene[[i]])  %>% 
    assess(.) %>% 
    select(rama, region, stat, label)
  i <- i+1
  
}



# 3.5 	Categoría ocupacional por región

categoria_ocup <- list()

i <- 1
for (i in seq_along(dc_ene)) {
  
  # 3.5.1   Ocupados segun categoria ocupacional
  categoria_ocup[[i]] <-   create_total(var = "ocupado", domains = "categoria_ocup+region", subpop = "fdt", design = dc_ene[[i]])  %>% 
    assess(.) %>% 
    select(categoria_ocup, region, stat, label)
  i <- i+1
  
}


# 3.5.2   Distribución de ocupados por categoria ocupacional

tasa_categoria_ocup <- list(
  round(mapply("/", categoria_ocup[[1]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), ocup[[1]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", categoria_ocup[[2]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), ocup[[2]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", categoria_ocup[[3]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), ocup[[3]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", categoria_ocup[[4]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), ocup[[4]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", categoria_ocup[[5]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), ocup[[5]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1)
)



# 3.6 	educ por region

educ_ocup <- list()
educ_desocup <- list()
educ_pet <- list()
educ_fdt <- list()


i <- 1
for (i in seq_along(dc_ene)) {
  
  # 3.6.1   Ocupados segun educ
  educ_ocup[[i]] <- create_total(var = "ocupado", domains = "educ+region", subpop = "fdt", design = dc_ene[[i]])  %>% 
    assess(.) %>% 
    select(educ, region, stat, label)
  
  # 3.6.2   PET segun educ
  educ_pet[[i]] <- create_total(var = "pet", domains = "educ+region", subpop = "pet", design = dc_ene[[i]])  %>% 
    assess(.) %>% 
    select(educ, region, stat, label)
  
  
  # 3.6.3   Desocupados segun educ
  educ_desocup[[i]] <- create_total(var = "desocupado", domains = "educ+region", subpop = "fdt", design = dc_ene[[i]])  %>% 
    assess(.) %>% 
    select(educ, region, stat, label)
  
  # 3.6.4   fdt segun educ
  educ_fdt[[i]] <- create_total(var = "fdt", domains = "educ+region", subpop = "fdt", design = dc_ene[[i]])  %>% 
    assess(.) %>% 
    select(educ, region, stat, label)
  
  i <- i+1
  
}



# 3.7	Ocupacion, desocupacion por tramo etario y region
ocup_edad <- list()
pet_edad <- list()
desocup_edad <- list()
fdt_edad <- list()



i <- 1
for (i in seq_along(dc_ene)) {
  
  # 3.7.1   Ocupados por tramo etario
  ocup_edad[[i]] <- create_total(var = "ocupado", domains = "tramo_edad+region", subpop = "fdt", design = dc_ene[[i]])  %>% 
    assess(.) %>% 
    select(tramo_edad, region, stat, label)
  
  # 3.7.2   PET por tramo etario 
  pet_edad[[i]] <- create_total(var = "pet", domains = "tramo_edad+region", subpop = "pet", design = dc_ene[[i]])  %>% 
    assess(.) %>% 
    select(tramo_edad, region, stat, label)
  
  # 3.7.3   desocupados por tramo etario
  desocup_edad[[i]] <- create_total(var = "desocupado", domains = "tramo_edad+region", subpop = "fdt", design = dc_ene[[i]])  %>% 
    assess(.) %>% 
    select(tramo_edad, region, stat, label)
  
  # 3.7.4   fdt por tramo etario 
  fdt_edad[[i]] <- create_total(var = "fdt", domains = "tramo_edad+region", subpop = "fdt", design = dc_ene[[i]])  %>% 
    assess(.) %>% 
    select(tramo_edad, region, stat, label)
  
  i <- i+1
}


# 






# 3.8	Asalariados 
asal_n <- list()
asal_informal <- list()


i <- 1
for (i in seq_along(dc_ene)) {
  
  # 3.8.1   Asalariados totales
  asal_n[[i]] <- create_total(var = "asal", domains = "region", subpop = "ocupado", design = dc_ene[[i]]) %>%
    assess()  %>% 
    select(region, stat, label)
  
  # 3.8.2   Asalariados informales
  asal_informal[[i]] <- create_total(var = "asal", domains = "informal+region", subpop = "ocupado", design = dc_ene[[i]]) %>% 
    assess() %>% 
    select(informal, region, stat, label)
  
  i <- i+1
  
}


asal_sex <- list(
  
)
asal_informal_sex <- list()

i <- 1
for (i in seq_along(dc_ene)) {
  
  # 3.8.3   Asalariados totales por sexo
  asal_sex[[i]] <- create_total(var = "asal", domains = "sexo+region", subpop = "ocupado", design = dc_ene[[i]]) %>%
    assess()  %>% 
    select(sexo, region, stat, label)
  
  # 3.8.4   Asalariados informales por sexo
  asal_informal_sex[[i]] <- create_total(var = "asal", domains = "sexo+informal+region", subpop = "ocupado", design = dc_ene[[i]]) %>% 
    assess() %>% 
    select(sexo, informal, region, stat, label)
  
  i <- i+1
}




# 3.9 	Brechas de genero
pet_brecha <- list()
fdt_brecha<- list()
desocup_brecha <- list()

i <- 1
for (i in seq_along(dc_ene)) {
  
  # Total población edad trabajar según sexo
  pet_brecha[[i]] <- create_total(var = "pet", domains = "sexo", subpop = "pet", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(sexo, stat, label)
  
  # Total fuerza de trabajo según sexo
  fdt_brecha[[i]] <- create_total(var = "fdt", domains = "sexo",subpop = "fdt", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(sexo, stat, label)
  
  # Total desocupados según sexo
  desocup_brecha[[i]] <- create_total(var = "desocupado", domains = "sexo",subpop = "fdt", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(sexo, stat, label)
  
  i <- i+1
  
}


# 3.10 inactivos
inact <- list() 

i <- 1
for (i in seq_along(dc_ene)) {
  
  # 3.10   Inactivos (PET)
  inact[[i]] <- create_total(var = "pet", domains = "razon_inact+region", subpop = "pet", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(region, stat, label, razon_inact)
}


inact_sex <- list() 

i <- 1
for (i in seq_along(dc_ene)) {
  
  # 3.10   Inactivos (PET)
  inact_sex[[i]] <- create_total(var = "pet", domains = "sexo+razon_inact+region", subpop = "pet", design = dc_ene[[i]]) %>% 
    assess(.) %>% 
    select(region, stat, label, razon_inact, sexo)
}



# Tasas participación, ocupación, desocupación y toi nacional


tasa_parti_n <- list(
  round(mapply("/", fdt[[1]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), pet[[1]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", fdt[[2]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), pet[[2]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", fdt[[3]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), pet[[3]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", fdt[[4]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), pet[[4]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", fdt[[5]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), pet[[5]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1)
  
)


tasa_ocup_n <- list(
  round(mapply("/", ocup[[1]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), pet[[1]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", ocup[[2]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), pet[[2]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", ocup[[3]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), pet[[3]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", ocup[[4]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), pet[[4]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", ocup[[5]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), pet[[5]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1)
)



tasa_desocup_n <- list(
  round(mapply("/", desocup[[1]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), fdt[[1]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", desocup[[2]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), fdt[[2]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", desocup[[3]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), fdt[[3]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", desocup[[4]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), fdt[[4]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", desocup[[5]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), fdt[[5]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1)
)


tasa_informal_n <- list(
  round(mapply("/", informal[[1]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), ocup[[1]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", informal[[2]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), ocup[[2]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", informal[[3]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), ocup[[3]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", informal[[4]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), ocup[[4]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", informal[[5]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), ocup[[5]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1)
)



# Tasas participación, desocupación y toi por sexo


tasa_parti_h <- list(
  round(mapply("/", fdt_sex[[1]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[1]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", fdt_sex[[2]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[2]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", fdt_sex[[3]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[3]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", fdt_sex[[4]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[4]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", fdt_sex[[5]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[5]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1)
)

tasa_parti_m <- list(
  round(mapply("/", fdt_sex[[1]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[1]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", fdt_sex[[2]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat),
               pet_sex[[2]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", fdt_sex[[3]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[3]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", fdt_sex[[4]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[4]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", fdt_sex[[5]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[5]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1)
)



tasa_desocup_h <- list(
  round(mapply("/", desocup_sex[[1]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               fdt_sex[[1]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", desocup_sex[[2]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               fdt_sex[[2]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", desocup_sex[[3]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               fdt_sex[[3]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", desocup_sex[[4]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               fdt_sex[[4]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", desocup_sex[[5]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               fdt_sex[[5]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1)
)


tasa_desocup_m <- list(
  round(mapply("/", desocup_sex[[1]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               fdt_sex[[1]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", desocup_sex[[2]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               fdt_sex[[2]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", desocup_sex[[3]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               fdt_sex[[3]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", desocup_sex[[4]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               fdt_sex[[4]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", desocup_sex[[5]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               fdt_sex[[5]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1)
)



tasa_ocup_h <- list(
  round(mapply("/", ocup_sex[[1]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[1]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", ocup_sex[[2]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[2]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", ocup_sex[[3]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[3]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", ocup_sex[[4]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[4]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", ocup_sex[[5]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[5]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1)
)


tasa_ocup_m <- list(
  round(mapply("/", ocup_sex[[1]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[1]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", ocup_sex[[2]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[2]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", ocup_sex[[3]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[3]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", ocup_sex[[4]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[4]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", ocup_sex[[5]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               pet_sex[[5]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1)
)



tasa_informal_h <- list(
  round(mapply("/", informal_sex[[1]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               ocup_sex[[1]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", informal_sex[[2]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               ocup_sex[[2]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", informal_sex[[3]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               ocup_sex[[3]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", informal_sex[[4]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               ocup_sex[[4]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", informal_sex[[5]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               ocup_sex[[5]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1)
)


tasa_informal_m <- list(
  round(mapply("/", informal_sex[[1]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               ocup_sex[[1]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", informal_sex[[2]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               ocup_sex[[2]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", informal_sex[[3]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               ocup_sex[[3]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", informal_sex[[4]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               ocup_sex[[4]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", informal_sex[[5]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat), 
               ocup_sex[[5]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1)
)


# Tasa asalariados informales

tasa_asal_informal_n <- list(
  round(mapply("/", asal_informal[[1]] %>% 
                 filter(informal == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat),
               asal_n[[1]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", asal_informal[[2]] %>% 
                 filter(informal == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat),
               asal_n[[2]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", asal_informal[[3]] %>% 
                 filter(informal == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat),
               asal_n[[3]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", asal_informal[[4]] %>% 
                 filter(informal == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat),
               asal_n[[4]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1),
  round(mapply("/", asal_informal[[5]] %>% 
                 filter(informal == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat),
               asal_n[[5]] %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100, 1)
  
  
)

tasa_asal_informal_h <- list(
  round(mapply("/", asal_informal_sex[[1]] %>% 
                 filter(sexo == 1 & informal == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat),
               asal_sex[[1]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100,1),
  round(mapply("/", asal_informal_sex[[2]] %>% 
                 filter(sexo == 1 & informal == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat),
               asal_sex[[2]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100,1),
  round(mapply("/", asal_informal_sex[[3]] %>% 
                 filter(sexo == 1 & informal == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat),
               asal_sex[[3]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100,1),
  round(mapply("/", asal_informal_sex[[4]] %>% 
                 filter(sexo == 1 & informal == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat),
               asal_sex[[4]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100,1),
  round(mapply("/", asal_informal_sex[[5]] %>% 
                 filter(sexo == 1 & informal == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat),
               asal_sex[[5]] %>% 
                 filter(sexo == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100,1)
  
)

tasa_asal_informal_m <- list(
  round(mapply("/", asal_informal_sex[[1]] %>% 
                 filter(sexo == 2 & informal == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat),
               asal_sex[[1]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100,1),
  round(mapply("/", asal_informal_sex[[2]] %>% 
                 filter(sexo == 2 & informal == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat),
               asal_sex[[2]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100,1),
  round(mapply("/", asal_informal_sex[[3]] %>% 
                 filter(sexo == 2 & informal == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat),
               asal_sex[[3]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100,1),
  round(mapply("/", asal_informal_sex[[4]] %>% 
                 filter(sexo == 2 & informal == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat),
               asal_sex[[4]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100,1),
  round(mapply("/", asal_informal_sex[[5]] %>% 
                 filter(sexo == 2 & informal == 1) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat),
               asal_sex[[5]] %>% 
                 filter(sexo == 2) %>% 
                 mutate(stat = if_else(label == "no fiable", NA_real_, stat)) %>% 
                 summarise(stat))*100,1)
  
  
)


# 4. Guardar y exportar ---------------------------------------------------

save(pet, pet_sex, pet_brecha,
     fdt, fdt_sex, fdt_brecha, 
     ocup, ocup_sex, 
     desocup, desocup_sex, desocup_brecha,
     fuera_fza, fuera_fza_sex,
     formal, formal_sex,
     informal, informal_sex,
     pet_nacional, pet_brecha,
     fdt_nacional, 
     ocup_nacional, 
     desocup_nacional,
     fuera_fza_nacional,
     formal_nacional,
     informal, informal_sex,
     educ_ocup, educ_pet, educ_fdt, educ_desocup,
     rama, 
     inact, inact_sex,
     categoria_ocup, tasa_categoria_ocup,
     ocup_edad, pet_edad, desocup_edad, fdt_edad,
     asal_n, asal_sex, asal_informal, asal_informal_sex,
     tasa_parti_n, tasa_parti_h, tasa_parti_m,
     tasa_ocup_n, tasa_ocup_h, tasa_ocup_m,
     tasa_desocup_n, tasa_desocup_h, tasa_desocup_m,
     tasa_informal_n, tasa_informal_h, tasa_informal_m,
     tasa_asal_informal_n, tasa_asal_informal_h, tasa_asal_informal_m,
     file = "output/calculos.RData"
     
     
)

