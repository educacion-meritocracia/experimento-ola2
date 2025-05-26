# Preparación 

pacman::p_load(dplyr, sjPlot, stargazer, kableExtra, texreg, haven, sjlabelled, ggplot2, summarytools, ggtext, ggpubr, hrbrthemes, tidyr, stringr, sjmisc, ggalluvial, shadowtext, sjmisc, lme4, Matrix)

# cargamos datos long 
load("./input/data/original/db_proc_students_w02.RData")

# Codificamos los valores 99-88 como missing para todas las variables
datos <- db_students_w02 %>% set_na(., na = c(99, 88))

# seleccionar 

datos <- datos %>% select(p4_o2,
                          d2_o2, 
                          p17_o2, 
                          p18_o2, 
                          p14_o2, 
                          nivel_estudiante_o2, 
                          p19_o2, 
                          merit_esfuerzo = p1_1_o2, 
                          merit_talento = p1_2_o2, 
                          school_esfuerzo = p2_1_o2, 
                          school_talento= p2_2_o2, 
                          school_merecimiento=p2_3_o2, 
                          exp_t4_a, 
                          exp_t4_b, 
                          exp_t4_c, 
                          exp_t4_d,
                          id_estudiante)

# crear variable dependencia 

datos <- datos %>%
  mutate(dependencia = case_when(
    d2_o2 %in% c(1, 2, 3, 5, 6, 7, 8, 9) ~ 1,
    d2_o2 == 4 ~ 2,
    TRUE ~ NA_integer_
  ))


datos$dependencia <- factor(datos$dependencia, 
                            levels=c(1,2),
                            labels=c(
                              "Colegio Particular Subvencionado",
                              "Colegio Municipal"))

# nivel educ-hogar
datos <- datos %>%
  mutate(educ_max = case_when(
    !is.na(p17_o2) & is.na(p18_o2) ~ p17_o2,
    is.na(p17_o2) & !is.na(p18_o2) ~ p18_o2,
    !is.na(p17_o2) & !is.na(p18_o2) ~ pmax(p17_o2, p18_o2, na.rm = TRUE),
    TRUE ~ NA_real_
  )) 


datos$educ_max <- car::recode(datos$educ_max, "1=1; 2=1; 3=1; 4=2; 5=2; 6=2")
datos$educ_max <- factor(datos$educ_max, 
                         levels=c(1,2),
                         labels=c("Enseñanza media o menos","Estudios superiores"))




# libros-hogar

datos$p19_o2 <- car::recode(datos$p19_o2,"1=1;2=1;3=1;4=2;5=2;6=2")

datos$p19_o2 <- factor(datos$p19_o2,
                    levels=c(1,2),
                    labels=c("Menos de 25 libros","Más de 25 libros"))



# genero
datos$p14_o2 <- car::recode(datos$p14_o2, "3=NA")

datos$p14_o2 <- factor(datos$p14_o2,
                    levels=c(1,2),
                    labels=c("Hombre","Mujer"))




# curso 
datos$nivel_estudiante_o2 <- factor(datos$nivel_estudiante_o2,
                                 levels=c(1,2),
                                 labels=c("Básica","Media"))


datos <- datos %>% rename(libros_hogar = p19_o2, 
                          genero = p14_o2,
                          curso = nivel_estudiante_o2) 


# meritocracia dummies


datos <- fastDummies::dummy_cols(datos,select_columns = "merit_esfuerzo", #remove_first_dummy = TRUE
)

datos$merit_esfuerzo_2 <- sjlabelled::set_label(x = datos$merit_esfuerzo_2,
                                             label =  "En descuerdo")

datos$merit_esfuerzo_3 <- sjlabelled::set_label(x = datos$merit_esfuerzo_3,
                                                label =  "De acuerdo")

datos$merit_esfuerzo_4 <- sjlabelled::set_label(x = datos$merit_esfuerzo_4,
                                                label =  "Muy de acuerdo")



datos <- fastDummies::dummy_cols(datos,select_columns = "merit_talento", #remove_first_dummy = TRUE
)

datos$merit_talento_2 <- sjlabelled::set_label(x = datos$merit_talento_2,
                                                label =  "En descuerdo")

datos$merit_talento_3 <- sjlabelled::set_label(x = datos$merit_talento_3,
                                                label =  "De acuerdo")

datos$merit_talento_4 <- sjlabelled::set_label(x = datos$merit_talento_4,
                                                label =  "Muy de acuerdo")


datos <- fastDummies::dummy_cols(datos,select_columns = "school_esfuerzo", #remove_first_dummy = TRUE
)

datos$school_esfuerzo_2 <- sjlabelled::set_label(x = datos$school_esfuerzo_2,
                                                label =  "En descuerdo")

datos$school_esfuerzo_3 <- sjlabelled::set_label(x = datos$school_esfuerzo_3,
                                                label =  "De acuerdo")

datos$school_esfuerzo_4 <- sjlabelled::set_label(x = datos$school_esfuerzo_4,
                                                label =  "Muy de acuerdo")

datos <- fastDummies::dummy_cols(datos,select_columns = "school_talento", #remove_first_dummy = TRUE
)

datos$school_talento_2 <- sjlabelled::set_label(x = datos$school_talento_2,
                                                 label =  "En descuerdo")

datos$school_talento_3 <- sjlabelled::set_label(x = datos$school_talento_3,
                                                 label =  "De acuerdo")

datos$school_talento_4 <- sjlabelled::set_label(x = datos$school_talento_4,
                                                 label =  "Muy de acuerdo")


datos <- fastDummies::dummy_cols(datos,select_columns = "school_merecimiento", #remove_first_dummy = TRUE
)

datos$school_merecimiento_2 <- sjlabelled::set_label(x = datos$school_merecimiento_2,
                                                label =  "En descuerdo")

datos$school_merecimiento_3 <- sjlabelled::set_label(x = datos$school_merecimiento_3,
                                                label =  "De acuerdo")

datos$school_merecimiento_4 <- sjlabelled::set_label(x = datos$school_merecimiento_4,
                                                label =  "Muy de acuerdo")

save(datos,file = "input/data/proc/proc_w02.RData")
