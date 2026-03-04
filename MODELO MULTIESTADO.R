#MODELO 5 ESTADOS#
####Preparacion estados ####
pkgs <- c("mstate", "scales","car", "survminer", "smoothHR","boot","writexl","tidyverse"); lapply(pkgs, require, character.only = TRUE)

"DEFINIENDO FALLO HEMODINAMICO"
aaa1 <- aaa1 %>% mutate(
    hmd_disf_tiempo_hasta = pmin(na0.2_tiempo_hasta, lac3mmol_tiempo_hasta, eb5_tiempo_hasta, na.rm = TRUE),
    hmd_disf_elegida = case_when(
      hmd_disf_tiempo_hasta == na0.2_tiempo_hasta ~ "na0.2_tiempo_hasta",
      hmd_disf_tiempo_hasta == lac3mmol_tiempo_hasta ~ "lac3mmol_tiempo_hasta",
      hmd_disf_tiempo_hasta == eb5_tiempo_hasta ~ "eb5_tiempo_hasta"),
    hmd_disf_censura_tiempo_hasta = case_when(
      hmd_disf_elegida == "na0.2_tiempo_hasta" ~ na0.2_censura_tiempo_hasta,
      hmd_disf_elegida == "lac3mmol_tiempo_hasta" ~ lac3mmol_censura_tiempo_hasta,
      hmd_disf_elegida == "eb5_tiempo_hasta" ~ eb5_censura_tiempo_hasta))
table(aaa1$hmd_disf_elegida);round(prop.table(table(aaa1$hmd_disf_elegida)), digits=4)

"DEFINIENDO SEGUNDO FRACASO ORGANICO"
aaa1 <- aaa1 %>%mutate(
    organ_disf_tiempo_hasta = pmin(pf200_tiempo_hasta, `0.5ml/kg/h_en_6-12h_tiempo_hasta`, crx2_tiempo_hasta, plaq50000_tiempo_hasta, bb6_tiempo_hasta, na.rm = TRUE),
    organ_disf_elegida = case_when(
      organ_disf_tiempo_hasta == pf200_tiempo_hasta ~ "pf200_tiempo_hasta",
      organ_disf_tiempo_hasta == `0.5ml/kg/h_en_6-12h_tiempo_hasta` ~ "0.5ml/kg/h_en_6-12h_tiempo_hasta",
      organ_disf_tiempo_hasta == crx2_tiempo_hasta ~ "crx2_tiempo_hasta",
      organ_disf_tiempo_hasta == plaq50000_tiempo_hasta ~ "plaq50000_tiempo_hasta",
      organ_disf_tiempo_hasta == bb6_tiempo_hasta ~ "bb6_tiempo_hasta"),
    organ_disf_censura_tiempo_hasta = case_when(
      organ_disf_elegida == "pf200_tiempo_hasta" ~ pf200_censura_tiempo_hasta,
      organ_disf_elegida == "0.5ml/kg/h_en_6-12h_tiempo_hasta" ~ `0.5ml/kg/h_en_6-12h_censura_tiempo_hasta`,
      organ_disf_elegida == "crx2_tiempo_hasta" ~ crx2_censura_tiempo_hasta,
      organ_disf_elegida == "plaq50000_tiempo_hasta" ~ plaq50000_censura_tiempo_hasta,
      organ_disf_elegida == "bb6_tiempo_hasta" ~ bb6_censura_tiempo_hasta))
table(aaa1$organ_disf_elegida);round(prop.table(table(aaa1$organ_disf_elegida)), digits=4)

#Comprobación estados y transiciones
aaa1 %>% 
  select(nhc,
         organ_disf_tiempo_hasta, 
         organ_disf_elegida, 
         organ_disf_censura_tiempo_hasta, 
         hmd_disf_tiempo_hasta, 
         hmd_disf_elegida, 
         hmd_disf_censura_tiempo_hasta,
         date_icu_discharge_tiempo_hasta,
         date_icu_discharge_censura_tiempo_hasta,
         date_death_censura_tiempo_hasta,
         date_death_tiempo_hasta) %>%
  filter(hmd_disf_censura_tiempo_hasta==1) %>% 
  filter(organ_disf_censura_tiempo_hasta==1) %>% 
  filter(date_death_censura_tiempo_hasta==1) 

aaa1 %>% # Seleccionar y mostrar las primeras filas
  select(nhc,
         organ_disf_tiempo_hasta, 
         organ_disf_elegida, 
         organ_disf_censura_tiempo_hasta, 
         hmd_disf_tiempo_hasta, 
         hmd_disf_elegida, 
         hmd_disf_censura_tiempo_hasta,
         date_icu_discharge_tiempo_hasta,
         date_icu_discharge_censura_tiempo_hasta,
         date_death_censura_tiempo_hasta,
         date_death_tiempo_hasta) %>%
  filter(hmd_disf_censura_tiempo_hasta==1 & date_icu_discharge_censura_tiempo_hasta==0) 
#solo hubo un paciente que hizo fracaso organico antes que hemodinamico

aaa1 %>% # Seleccionar y mostrar las primeras filas
  select(nhc,
         organ_disf_tiempo_hasta, 
         organ_disf_elegida, 
         organ_disf_censura_tiempo_hasta, 
         hmd_disf_tiempo_hasta, 
         hmd_disf_elegida, 
         hmd_disf_censura_tiempo_hasta,
         date_icu_discharge_tiempo_hasta,
         date_icu_discharge_censura_tiempo_hasta,
         date_death_censura_tiempo_hasta,
         date_death_tiempo_hasta,
         reingreso_censura_tiempo_hasta,
         reingreso_tiempo_hasta) %>% 
  filter(reingreso_censura_tiempo_hasta==1) %>% 
  filter(reingreso_tiempo_hasta<hmd_disf_tiempo_hasta | reingreso_tiempo_hasta<organ_disf_tiempo_hasta)

library(dplyr)

confirmacion <- aaa1 %>%
  # Filtramos solo los que tuvieron reingreso real
  filter(reingreso_censura_tiempo_hasta == 1) %>%
  # Creamos banderas lógicas para verificar la secuencia
  mutate(
    reingreso_post_hmd = reingreso_tiempo_hasta > hmd_disf_tiempo_hasta,
    reingreso_post_organ = reingreso_tiempo_hasta > organ_disf_tiempo_hasta
  )

# Resumen de la cronología
confirmacion %>%
  summarise(
    total_reingresos = n(),
    despues_de_hmd = sum(reingreso_post_hmd, na.rm = TRUE),
    despues_de_organ = sum(reingreso_post_organ, na.rm = TRUE)
  )
#Tfallo < Treingreso

aaa1 %>% # Seleccionar datos con evento y causa del evento
  select(organ_disf_tiempo_hasta, organ_disf_elegida, organ_disf_censura_tiempo_hasta)  %>% 
  group_by(organ_disf_elegida) %>% 
  count()

#Modelo final disfuncion organica
transMat(
  x = list(
    c(2, 4),        # Transiciones posibles desde icu
    c(3, 4),        # Transiciones posibles desde hmd_disf
    c(4, 5),        # Transiciones posibles desde organ_disf           
    c(),            # Transiciones posibles desde death (ninguna)
    c()),           # Transiciones posibles desde discharge (ninguna)
  names = c(
    "icu", 
    "hmd_disf",
    "organ_disf",
    "discharge",
    "death"))->tmat1

#Covariables del modelo
aaa1 %>%
  select(-contains("hasta")) %>%
  select( -nhc, -patient_number, -nombre_apellidos, -year_of_birth, #no interes
         -hs_type, -hs_typeII, -hs_typeIII, -end_hs, -start_hs,  #ya codificadas
         -apacheii24, -sapsii24, -sapsiii24, -antithrombotic, -ecls, #no sentido
         -weight,-size,-lvef_icu,-rvef_icu,sex_factor,#ya recogido imc, pre y post funcion
         -hs_typecia,-hs_typefistula_coronaria,-hs_typemitral_annuloplasty,#n minima
         -hs_typemixoma, -hs_typeParche_pericárdico, -hs_typePseudoaneurisma, -hs_typeResección_de_masa_auricular, -hs_typerotura_cardiaca, #n minima
         -cannulation_type_art, -cannulation_type_v, -cardioplegic_adm, -cardioplegic_sol,#ya recat de forma adecuada
         -lvad_pre,-lvad_ad, -pvc_pre, #n infima pvc pre. lvad recat ya
         -hmd_disf_elegida,-organ_disf_elegida, #no utilidad
         -cava_ett,-it_max,-psap,-trasfusion,-euroscoreII,-sex_factor,-estadio_erc,-n_bypass, #n_bypass poca n subvariable
         -na_dose_pre,-dbt_dose_pre,-mlr_dose_pre,-vsp_dose_pre,-dbt_dose_ad,-mlr_dose_ad, #fuera variables incluidas en el vis
         -pas_pre, -pad_pre, -hr_pre, -pas_ad, -pad_ad, -hr_ad,  #fuera variables incluidas en el IS
         -lvef_ad, -lvef_pre,-rvef_ad,-rvef_pre, #fuera variables recogidas como cambio en la puntuacion
         -ic, -dm , -smoker, -hf, -ckd, -lvs_qx, -time_of_hs)%>%  #fuera variables prevoas de las recategorizadas
  select(-c("pcr_intrauci":"causa_reingreso"))%>% colnames()->covariables_univariante; covariables_univariante

covariables_grafico <- c(covariables_univariante, "hmd_disf_elegida", "organ_disf_elegida", #borrar si requiere: elegir disf
"dbt_dose_ad","mlr_dose_ad") # borrar si requiere: soporte previo

msebmt1<- msprep(
  data = aaa1,  
  trans = tmat1,
  time = c(NA,
           "hmd_disf_tiempo_hasta", 
           "organ_disf_tiempo_hasta", 
           "date_icu_discharge_tiempo_hasta",
           "date_death_tiempo_hasta"), 
  status = c(NA,
             "hmd_disf_censura_tiempo_hasta", 
             "organ_disf_censura_tiempo_hasta", 
             "date_icu_discharge_censura_tiempo_hasta",
             "date_death_censura_tiempo_hasta"), 
  keep = covariables_univariante)#covariables o covariables_grafico


#dead_nhc_aaa1 <- aaa1 %>%filter(date_death_censura_tiempo_hasta == 1) %>%pull(nhc)
#dead_nhc_msebmt1 <- msebmt1 %>% filter(trans==6) %>% filter(status==1) %>% pull(nhc)
#setdiff(dead_nhc_aaa1, dead_nhc_msebmt1)


tmat1 #Ver matriz estados y transiciones
nrow(tmat1)#Ver numero estados
paths(tmat1) #Numero posibilidades en la trayectoria clinica

head(msebmt1)         # Primeras filas del dataset preparado
names(msebmt1)        # Variables contenidas
events(msebmt1)       # Conteo y porcentaje de eventos por transición


# Paso 1: Ajustar el modelo de Cox para cada transición, cada transición tenga su propia función de riesgo base.
#cluster(id) ajusta por dependencia intra-individuo (pacientes con múltiples transiciones).
cmodel <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+cluster(id), data = msebmt1)
summary(cmodel);basehaz(cmodel, centered=FALSE)
# Paso 2: Convertir el modelo en objeto msfit, para calcular probabilidades de transición
msf <- msfit(object =cmodel,vartype = "aalen", trans = tmat1)#methodo greenwood o aalen mejor aalen
# Paso 3: Calcular probabilidades de transición desde el tiempo 0, Calcula las probabilidades acumuladas de estar en cada estado a lo largo del tiempo.
pt <- probtrans(msf, predt = 0) #predt = 0 indica que se parte desde el tiempo 0 HORAS
#objeto pt ya contiene todas las probabilidades condicionales desde cada estado de partida.
summary(pt);plot(pt) 
# Paso 4: Mostrar probabilidades con IC del 95%. pt[[i]] presenta la evolución desde el momento en que el paciente entra en el estado i
summary(pt[[1]], ci = TRUE)


pt_ci <- pt[[1]] # Calcular los IC del 95% para cada estado de forma manual
# Añadir columnas de IC inferior y superior para cada estado
pt_ci$lower1 <- pt_ci$pstate1 - 1.96 * pt_ci$se1
pt_ci$upper1 <- pt_ci$pstate1 + 1.96 * pt_ci$se1
pt_ci$lower2 <- pt_ci$pstate2 - 1.96 * pt_ci$se2
pt_ci$upper2 <- pt_ci$pstate2 + 1.96 * pt_ci$se2
pt_ci$lower3 <- pt_ci$pstate3 - 1.96 * pt_ci$se3
pt_ci$upper3 <- pt_ci$pstate3 + 1.96 * pt_ci$se3
pt_ci$lower4 <- pt_ci$pstate4 - 1.96 * pt_ci$se4
pt_ci$upper4 <- pt_ci$pstate4 + 1.96 * pt_ci$se4
pt_ci$lower5 <- pt_ci$pstate5 - 1.96 * pt_ci$se5
pt_ci$upper5 <- pt_ci$pstate5 + 1.96 * pt_ci$se5
# Visualizar los primeros valores
head(pt_ci[, c("time", "pstate1", "lower1", "upper1",
               "pstate2", "lower2", "upper2",
               "pstate3", "lower3", "upper3",
               "pstate4", "lower4", "upper4",
               "pstate5", "lower5", "upper5")])
pt_ci_2 <- function(pt_object, tiempo_horas) {
  # Nombres clínicos de los estados
  nombres_estados <- c("ICU", "hmd_disf", "organ_disf", "discharge", "death")
  # Extraer el data frame de probabilidades
  df <- pt_object[[1]]
  # Buscar la fila con el tiempo más cercano al especificado
  fila <- df[which.min(abs(df$time - tiempo_horas)), ]
  # Calcular IC del 95% para cada estado
  resultado <- data.frame(
    Estado = nombres_estados,
    Probabilidad = as.numeric(fila[paste0("pstate", 1:5)]),
    IC_Inf = as.numeric(fila[paste0("pstate", 1:5)]) - 1.96 * as.numeric(fila[paste0("se", 1:5)]),
    IC_Sup = as.numeric(fila[paste0("pstate", 1:5)]) + 1.96 * as.numeric(fila[paste0("se", 1:5)])
  )
  return(resultado)
}
# Ejemplo de uso para 2 horas:
pt_ci_2(pt, tiempo_horas = 2)

pt_ci_multi <- function(pt_object, tiempos_horas) {
  # Definir el orden correcto de los estados
  nombres_estados <- c("ps1_ICU", "ps2_hmd_disf", "ps3_organ_disf", "ps4_discharge", "ps5_death")
  df <- pt_object[[1]]
  lista_resultados <- list()
  for (tiempo in tiempos_horas) {
    fila <- df[which.min(abs(df$time - tiempo)), ]
    resultado <- data.frame(
      Estado = nombres_estados,
      Probabilidad = as.numeric(fila[paste0("pstate", 1:5)]),
      IC_Inf = as.numeric(fila[paste0("pstate", 1:5)]) - 1.96 * as.numeric(fila[paste0("se", 1:5)]),
      IC_Sup = as.numeric(fila[paste0("pstate", 1:5)]) + 1.96 * as.numeric(fila[paste0("se", 1:5)])
    )
    colnames(resultado)[2:4] <- paste0(tiempo, "h_", c("Probabilidad", "IC_Inf", "IC_Sup"))
    lista_resultados[[length(lista_resultados) + 1]] <- resultado
  }
  # Unir todos los resultados por Estado
  df_final <- Reduce(function(x, y) merge(x, y, by = "Estado"), lista_resultados)
  # Reordenar según nombres_estados
  df_final <- df_final[match(nombres_estados, df_final$Estado), ]
  return(df_final)
}
# Ejemplo de uso
pt_ci_multi(pt, tiempos_horas = c(0, 2, 4, 6, 12, 24,48,96))

#Sin la funcion con el summary de pt
tiempos_interes <- c(0, 2, 6, 12, 24,96,144) # Tiempos que te interesan (en horas)
seleccion <- do.call(rbind, lapply(tiempos_interes, function(t) { # Filtrar las filas más cercanas a esos tiempos
  pt_ci[which.min(abs(pt_ci$time - t)), ]
}))
seleccion[, c("time",
              "pstate1", "lower1", "upper1",
              "pstate2", "lower2", "upper2",
              "pstate3", "lower3", "upper3",
              "pstate4", "lower4", "upper4",
              "pstate5", "lower5", "upper5")] # Mostrar resultados con IC

table(msebmt1$trans)  # Número de observaciones por transición
table(msebmt1$from)   # Estado de origen de las transiciones
table(msebmt1$to)     # Estado destino de las transiciones


pt_ci_multi <- function(pt_object, tiempos_horas) {
  # Definir el orden correcto de los estados
  nombres_estados <- c("ps1_ICU", "ps2_hmd_disf", "ps3_organ_disf", "ps4_discharge", "ps5_death")
  df <- pt_object  # Ya es un data.frame, no necesitas pt_object[[1]]
  lista_resultados <- list()
  for (tiempo in tiempos_horas) {
    fila <- df[which.min(abs(df$time - tiempo)), ]
    resultado <- data.frame(
      Estado = nombres_estados,
      Probabilidad = as.numeric(fila[paste0("pstate", 1:5)]),
      IC_Inf = as.numeric(fila[paste0("pstate", 1:5)]) - 1.96 * as.numeric(fila[paste0("se", 1:5)]),
      IC_Sup = as.numeric(fila[paste0("pstate", 1:5)]) + 1.96 * as.numeric(fila[paste0("se", 1:5)])
    )
    colnames(resultado)[2:4] <- paste0(tiempo, "h_", c("Probabilidad", "IC_Inf", "IC_Sup"))
    lista_resultados[[length(lista_resultados) + 1]] <- resultado
  }
  # Unir todos los resultados por Estado
  df_final <- Reduce(function(x, y) merge(x, y, by = "Estado"), lista_resultados)
  df_final <- df_final[match(nombres_estados, df_final$Estado), ]
  return(df_final)
}
options(tibble.width = Inf)
# Desde ingreso en UCI (estado 1)
resultado_ingreso_uci <- pt_ci_multi(pt[[1]], tiempos_horas = c(2, 12, 24, 48, 96)) %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), round, 2))
print(resultado_ingreso_uci)
# Desde fallo hemodinámico (estado 2)
resultado_hmd_disf <- pt_ci_multi(pt[[2]], tiempos_horas = c(2, 12, 24, 48, 96)) %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), round, 2))
resultado_hmd_disf
# Desde segundo fallo orgánico (estado 3)
resultado_organ_disf <- pt_ci_multi(pt[[3]], tiempos_horas = c(2, 12, 24, 48, 96)) %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), round, 2))
resultado_organ_disf

# Función para truncar IC y formatear como texto
formatear_probabilidad <- function(df) {
  tiempos <- c("2h", "12h", "24h", "48h", "96h")
  for (t in tiempos) {
    prob <- df[[paste0(t, "_Probabilidad")]]
    inf <- pmax(df[[paste0(t, "_IC_Inf")]], 0)
    sup <- pmin(df[[paste0(t, "_IC_Sup")]], 1)
    df[[t]] <- sprintf("%.2f (%.2f–%.2f)", prob, inf, sup)
  }
  df %>% select(Estado, all_of(tiempos))
}

# Aplicar a cada tibble
tabla_ingreso_uci <- formatear_probabilidad(resultado_ingreso_uci)
tabla_hmd_disf <- formatear_probabilidad(resultado_hmd_disf)
tabla_organ_disf <- formatear_probabilidad(resultado_organ_disf)

# Exportar a Excel
write_xlsx(
  list(
    Ingreso_UCI = tabla_ingreso_uci,
    Fallo_Hemodinamico = tabla_hmd_disf,
    Fallo_Organico = tabla_organ_disf
  ),
  path = "resumen_truncado_resultados.xlsx")



#####Comprobaciones#####
#Validación y conteo de eventos por transición:
msebmt1 %>% filter(trans==1) %>% select(from, to) %>% nrow() #pacientes en riesgo transicion 1
msebmt1 %>% filter(trans==1) %>% filter(from==1 & to==2 & status==1) %>% nrow() #Pacientes expeirmenten evento transicion 1 disf hmd

msebmt1 %>% filter(trans==2) %>% select(from, to) %>% nrow() # pacientes en riesgo transicion 2
msebmt1 %>% filter(trans==2) %>% filter(from==1 & to==4 & status==1) %>% nrow() #Pacientes expeirmenten evento transicion 2 alta

msebmt1 %>% filter(trans==3) %>% select(from, to) %>% nrow() # pacientes en riesgo transicion 3
msebmt1 %>% filter(trans==3) %>% filter(from==2 & to==3 & status==1) %>% nrow() #Pacientes expeirmenten evento transicion 3 disf org

msebmt1 %>% filter(trans==4) %>% select(from, to) %>% nrow() # pacientes en riesgo transicion 4
msebmt1 %>% filter(trans==4) %>% filter(from==2 & to==4 & status==1) %>% nrow() #evento transicion 4 alta desde disfuncion hmd

msebmt1 %>% filter(trans==5) %>% select(from, to) %>% nrow() # pacientes en riesgo transicion 5
msebmt1 %>% filter(trans==5) %>% filter(from==3 & to==4 & status==1) %>% nrow()#evento transicion 5 alta desde disfuncion org

msebmt1 %>% filter(trans==6) %>% select(from, to) %>% nrow() # pacientes en riesgo transicion 6
msebmt1 %>% filter(trans==6) %>% filter(from==3 & to==5 & status==1) %>% nrow() #evento transicion 6 muerte desde disfuncion

#potenciales fallos tiempos invertidos
msebmt1 %>% filter(Tstop < Tstart) %>% arrange(id, trans)

#cuantos tiempos iguales de evento independientemente de transicon. POTENCIAL TRUNCAMIENTO IZQUIERDA EVENTO ANTES OBSERVADO
msebmt1 %>% filter(Tstop == Tstart) %>% nrow() 

msebmt1 %>%
  filter(Tstop == Tstart) %>%
  group_by(trans,status) %>%
  summarise(n = n()) %>%
  arrange(trans)

# Pacientes con evento en trans 1 en tiempo 0
pacientes_trans1_evento <- msebmt1 %>%filter(Tstop == Tstart, trans == 1, status == 1) %>%pull(id) %>% unique()
# Pacientes con censura en trans 2 en tiempo 0
pacientes_trans2_censura <- msebmt1 %>%filter(Tstop == Tstart, trans == 2, status == 0) %>%pull(id) %>% unique()
# Ver cuántos pacientes están en ambas listas (intersección)
intersect_pacientes <- intersect(pacientes_trans1_evento, pacientes_trans2_censura)
length(pacientes_trans1_evento);length(pacientes_trans2_censura)  # Debería ser 42. Debería ser 42
length(intersect_pacientes) # Cuántos están en ambas listas
#Los 42 pacientes están presentes en ambas transiciones pero con estados y eventos distintos.Transición 1 (evento) significa que el paciente pasó inmediatamente a disfunción.Transición 2 (censura) significa que para estos pacientes no aplica pasar directo a alta desde ingreso porque ya “salieron” del estado ingreso.

#Igual pero de distinta forma
msebmt1 %>%  filter(Tstop <= Tstart) %>% filter(trans==1) 
msebmt1 %>%  filter(Tstop <= Tstart) %>% filter(trans==2) 
msebmt1 %>%  filter(Tstop <= Tstart) %>% filter(trans==3)
msebmt1 %>%  filter(Tstop <= Tstart) %>% filter(trans==4)
msebmt1 %>%  filter(Tstop <= Tstart) %>% filter(trans==5)

msebmt1 %>% filter(Tstop == Tstart & status == 0) %>% arrange(id, trans) %>%   select(id, trans, from, to, Tstart, Tstop, status, everything())

#Truncados con VIS>20.
msebmt1 %>%filter(Tstop == Tstart & status == 1, trans == 1) %>% filter(vis_ad>=20) %>% nrow()
msebmt1 %>%filter(Tstop == Tstart & status == 1, trans == 1) %>% filter(na_dose_ad>=0.2) %>% nrow()
msebmt2<-filter(msebmt1, !(Tstop == Tstart & status == 1 & trans == 1 & vis_ad >= 20))
                


######Ajustes preparatorios#######
#Revisando tiempos iguales truncamiento
msebmt1 %>% arrange(id, trans, Tstart) # revisar incoherencias paciente a pacient
msebmt1$Tstop <- ifelse(msebmt1$Tstop <= msebmt1$Tstart, #Tstop == Tstart represents a valid instantaneous event (which is tricky in Cox models), you have a few option
                        msebmt1$Tstart + 1e-5,
                        msebmt1$Tstop)

#No Eventos en cada transiccion
#Preparación de los datos para estimar los modelos:en cada transicción cada covariable tiene un efecto
msebmt1<-expand.covs(msebmt1,
                     covariables_univariante,
                     append=TRUE, 
                     longnames=TRUE) #longnames=T both parts are intersected by the specific labels in the coding
names(msebmt1)
colnames(msebmt1)[which(colnames(msebmt1) == "sexmujer.1"):length(msebmt1)] #Guardar solo desde primera variable estratificada por transicion
colnames(msebmt1)[which(colnames(msebmt1) == "sexmujer.1"):length(msebmt1)]->covariables_transicion

-------------------
  #####MODELOS UNIVARIANTES########
#Anexos univariantes
univ_formulas <- sapply(covariables_transicion, function(x)
  as.formula(paste('Surv(Tstart,Tstop,status) ~', x, '+ cluster(id) + strata(trans)'))) # Crear fórmulas univariantes
univ_models <- lapply(univ_formulas, function(x) coxph(x, data = msebmt1, method = "breslow")) # Ajustar modelos
univ_results <- lapply(univ_models, function(x) {
  x_summary <- summary(x)
  beta <- round(x_summary$coef[1], 3)
  HR_val <- round(x_summary$coef[2], 3)
  HR.lower <- round(x_summary$conf.int[,"lower .95"], 3)
  HR.upper <- round(x_summary$conf.int[,"upper .95"], 3)
  HR <- paste0(HR_val, " (", HR.lower, "-", HR.upper, ")")
  wald.test <- round(x_summary$wald["test"], 3)
  p.value <- round(x_summary$wald["pvalue"], 3)
  score.test <- round(x_summary$sctest["test"], 3)
  score.p.value <- round(x_summary$sctest["pvalue"], 3)
  loglik <- round(-2 * x_summary$loglik[2], 3)
  AIC <- round(AIC(x), 3)
  BIC <- round(BIC(x), 3)
  concordance <- round(x_summary$concordance[1], 3)
  concordance.se <- round(x_summary$concordance[2], 3)
  res <- c(beta, HR, wald.test, p.value, score.test, score.p.value, loglik, AIC, BIC, concordance, concordance.se)
  names(res) <- c("beta", "HR (95% CI for HR)", "wald.test", "p.value", "score.test", "score.p.value", "-2loglik", "AIC", "BIC", "concordance", "concordance.se")
  return(res)# Extraer resultados
})
# Convertir lista a data frame y añadir nombres de variables como columna
univ_results_df <- as.data.frame(t(as.data.frame(univ_results, check.names = FALSE)))
univ_results_df$Variable <- rownames(univ_results_df)
# Reordenar columnas para que 'Variable' sea la primera
univ_results_df <- univ_results_df[, c("Variable", setdiff(names(univ_results_df), "Variable"))]
# Ordenar por número de transición si está incluido en el nombre de la variable
univ_results_df <- univ_results_df %>%
  arrange(as.numeric(gsub(".*\\.(\\d+).*", "\\1", Variable)))
# Exportar a Excel
write_xlsx(univ_results_df, path = "Analisis_univariante_3decimales.xlsx")


--------------------------------------------------------------------------------------------------------
univ_formulas <- sapply(covariables_transicion, function(x)
  as.formula(paste('Surv(Tstart,Tstop,status) ~', x, '+ cluster(id) + strata(trans)')))
univ_models <- lapply(univ_formulas, function(x) coxph(x, data = msebmt1, method = "breslow"))
univ_results <- lapply(univ_models, function(x) {
  x_summary <- summary(x)
  beta <- signif(x_summary$coef[1], digits = 6)
  HR_val <- signif(x_summary$coef[2], digits = 6)
  HR.lower <- signif(x_summary$conf.int[,"lower .95"], 6)
  HR.upper <- signif(x_summary$conf.int[,"upper .95"], 6)
  HR <- paste0(HR_val, " (", HR.lower, "-", HR.upper, ")")
  wald.test <- signif(x_summary$wald["test"], 6)
  p.value <- signif(x_summary$wald["pvalue"], 6)
  score.test <- signif(x_summary$sctest["test"], 6)
  score.p.value <- signif(x_summary$sctest["pvalue"], 6)
  loglik <- signif(-2 * x_summary$loglik[2], 6)
  AIC <- signif(AIC(x), 6)
  BIC <- signif(BIC(x), 6)
  concordance <- signif(x_summary$concordance[1], 6)
  concordance.se <- signif(x_summary$concordance[2], 6)
  res <- c(beta, HR, wald.test, p.value, score.test, score.p.value, loglik, AIC, BIC, concordance, concordance.se)
  names(res) <- c("beta", "HR (95% CI for HR)", "wald.test", "p.value", "score.test", "score.p.value",
                  "-2loglik", "AIC", "BIC", "concordance", "concordance.se")
  return(res)
})
univ_results_df <- as.data.frame(t(as.data.frame(univ_results, check.names = FALSE)))
univ_results_df$Variable <- rownames(univ_results_df)
univ_results_df <- univ_results_df[, c("Variable", setdiff(names(univ_results_df), "Variable"))]
univ_results_df <- univ_results_df %>%
  arrange(as.numeric(gsub(".*\\.(\\d+).*", "\\1", Variable)))
write_xlsx(univ_results_df, path = "Analisis_univariante_todos_decimales.xlsx")



#solo aquellos univariantes <0.15
univ_results%>% filter(p.value<0.2)->univ_results
write_xlsx(univ_results, path = "Analisis_univariante_todos_decimales_p<0.2.xlsx")

#######EVENTOS POR TRANSCIONES CALCULO VARIABLES APROXIMADO######
events(msebmt1); eventos_covariable<-10
#transicion 1
111/eventos_covariable
#transicion2
86/eventos_covariable
#transicion 3
60/eventos_covariable
#transicion 4
50/eventos_covariable
#transcion 5
49/eventos_covariable
#transicion 6
11/eventos_covariable

#Retirada variables no interesantes clinicamente multivariante. Mi criterio
univ_results %>% filter(!(variable %in% c("vis_ad.1","lvs_qx_recatyes_lvs.1",
                                          "hs_type_tricuspid_annuloplastytricuspid_annuloplasty.2","lvs_qx_recatyes_lvs.2","time_of_hs_recaturgent_or_emergent.2",
                                          "plaquetas_post.3",
                                          "htasi_hta.4",
                                          "weight_of_procedurethree_major_procedures.5")))->modelo_maximo #Seleccion variables clinicamente relevantes por transiciones que me parece que no tienen sentido
rownames(modelo_maximo)->covariable 
length(covariable) 


#####CREACION MODELO MAXIMO##### 
formula_string<- paste('Surv(Tstart, Tstop, status) ~', 
                        paste(covariable, collapse = ' + '), 
                        '+ strata(trans) + cluster(id)')
final_formula <- as.formula(formula_string)
final_formula
modelo_maximo<-coxph(final_formula, data= msebmt1, method = "breslow")
summary(modelo_maximo)

#Vamos quitando 1 a 1 variable del modelo minimo respecto al maximo y estudiamos rendimiento modelos

# MAX
modelo_maximo <- update(modelo_maximo, . ~ . 
                        - dm_recatyes_dm.1 
                        - pfc_post.1 
                        - pvc_ad.1 
                        - pm_adpm_dependent.1 
                        - hs_duration.1 
                        - hs_typediseccióndiseccion.1 
                        - pam_pre.1 
                        - age.1 
                        - dldl.1 
                        - pam_ad.1 
                        - hs_type_tata.1 
                        - hs_type_valvularvalvular.1 
                        - cardioplegic_sol_recatsanguinea.1 
                        - is_pre.1 
                        -endocarditisendocarditis.1
                        
                        - weight_of_proceduretwo_major_procedures.2 
                        - pfc_post.2 
                        - hs_type_mvrmvr.2
                        -time_of_hs_recaturgent_or_emergent.2
                        -mortalidad_predicha.2
                        -pam_ad.2
                        -is_ad.2
                        
                        -hypothermia.3
                        -hematíes_post.3
                        -pm_adpm_dependent.3
                        -hs_typediseccióndiseccion.3
                        -rvef_change_pre_ad.3
                        -smoker_recatsmoker.3
                        -cannulation_type_art_recatperiferic.3
                        -is_ad.3
                        -copdcopd.3
                        -vis_ad.3
                        
                        -afaf.aF.4
                        -nyha.4
                        -hypothermia.4
                        -cbp_time.4
                        -cfs.4
                        -ao_cross_clamp_time.4
                        -plaquetas_post.4
                        -pfc_post.4
                        -pvc_ad.4
                        -ckd_cockcroft_gault.4
                        -lvs_qx_recatyes_lvs.4
                        -hf_recatyes_hf.4
                        -mortalidad_predicha.4
                        -hs_duration.4
                        -rvef_change_pre_ad.4 
                        
                        -weight_of_procedurethree_major_procedures.5
                        -copdcopd.5
                        -hypothermia.5
                        -pvc_ad.5
                        -age.5
                        -vis_pre.5
                        -vis_ad.5
                        -rvef_change_pre_ad.5
                        -is_pre.5
                        -hs_type_avravr.5
                        -nyha.5
                        -pam_pre.5
                        -cannulation_type_art_recatperiferic.5
                        -hs_type_valvularvalvular.5
                        -cfs.5
                        -hs_type_tata.5
                        
                        -hematíes_post.6)

# MIN
modelo_minimo <- update(modelo_maximo, . ~ . 
                        - dm_recatyes_dm.1 
                        - pfc_post.1 
                        - pvc_ad.1 
                        - pm_adpm_dependent.1 
                        - hs_duration.1 
                        - hs_typediseccióndiseccion.1 
                        - pam_pre.1 
                        - age.1 
                        - dldl.1 
                        - pam_ad.1 
                        - hs_type_tata.1 
                        - hs_type_valvularvalvular.1 
                        - cardioplegic_sol_recatsanguinea.1 
                        - is_pre.1
                        -endocarditisendocarditis.1 
                        
                        - weight_of_proceduretwo_major_procedures.2 
                        - pfc_post.2 
                        - hs_type_mvrmvr.2
                        -time_of_hs_recaturgent_or_emergent.2
                        -mortalidad_predicha.2
                        -pam_ad.2
                        -is_ad.2
                        
                        -hypothermia.3
                        -hematíes_post.3
                        -pm_adpm_dependent.3
                        -hs_typediseccióndiseccion.3
                        -rvef_change_pre_ad.3
                        -smoker_recatsmoker.3
                        -cannulation_type_art_recatperiferic.3
                        -is_ad.3
                        -copdcopd.3
                        -vis_ad.3
                        
                        -afaf.aF.4
                        -nyha.4
                        -hypothermia.4
                        -cbp_time.4
                        -cfs.4
                        -ao_cross_clamp_time.4
                        -plaquetas_post.4
                        -pfc_post.4
                        -pvc_ad.4
                        -ckd_cockcroft_gault.4
                        -lvs_qx_recatyes_lvs.4
                        -hf_recatyes_hf.4
                        -mortalidad_predicha.4
                        -hs_duration.4
                        -rvef_change_pre_ad.4 
                        
                        -weight_of_procedurethree_major_procedures.5
                        -copdcopd.5
                        -hypothermia.5
                        -pvc_ad.5
                        -age.5
                        -vis_pre.5
                        -vis_ad.5
                        -rvef_change_pre_ad.5
                        -is_pre.5
                        -hs_type_avravr.5
                        -nyha.5
                        -pam_pre.5
                        -cannulation_type_art_recatperiferic.5
                        -hs_type_valvularvalvular.5
                        -cfs.5
                        -hs_type_tata.5
                        
                        -hematíes_post.6)


# Log-likelihood y grados de libertad
logLik_modelo_maximo <- logLik(modelo_maximo);logLik_modelo_minimo <- logLik(modelo_minimo)
df1 <- attr(logLik_modelo_maximo, "df");df2 <- attr(logLik_modelo_minimo, "df")
LRT_statistic <- 2 * (logLik_modelo_maximo - logLik_modelo_minimo);p_value <- pchisq(LRT_statistic, df = abs(df1 - df2), lower.tail = FALSE)# Prueba de la razón de verosimilitud (LRT)
AIC_modelo_maximo <- AIC(modelo_maximo);BIC_modelo_maximo <- BIC(modelo_maximo)# AIC y BIC
AIC_modelo_minimo <- AIC(modelo_minimo);BIC_modelo_minimo <- BIC(modelo_minimo)
# Resultados
cat("Modelo maximo - Log-likelihood:", logLik_modelo_maximo, "| Grados de libertad:", df1, "\n","Modelo maximo - AIC:", AIC_modelo_maximo, "| BIC:", BIC_modelo_maximo, "\n")
cat("Modelo minimo - Log-likelihood:", logLik_modelo_minimo, "| Grados de libertad:", df2, "\n","Modelo minimo - AIC:", AIC_modelo_minimo, "| BIC:", BIC_modelo_minimo, "\n")
cat("Estadístico LRT:", LRT_statistic, "| Valor p:", p_value, "\n")
# Conclusión
if (p_value < 0.05) {cat("El modelo maximo es significativamente mejor que el reducido (p <", p_value, ").\n")} else {cat("No hay una diferencia significativa entre los modelos, mejor el reducido (modelo_minimo) (p =", p_value, ").\n")}



summary(modelo_maximo)
summary(modelo_minimo)

#2º ronda codigo para probar terminos eliminados por crterios de forma individual
coxph(formula = Surv(Tstart, Tstop, status) ~ time_of_hs_recaturgent_or_emergent.1+
        age.2 + lvef_change_pre_ad.2 + pctspcts.3 + ao_cross_clamp_time.3 + 
        vf_post.3 + pvc_ad.3 + hs_duration.3 + pam_pre.3 + pam_ad.3 + 
        time_of_hs_recaturgent_or_emergent.3 + hematíes_post.4 + 
        vis_ad.4 + is_ad.4 + pam_ad.4 + pctspcts.5 + cbp_time.5 + 
        ao_cross_clamp_time.5 + hs_typediseccióndiseccion.5 + is_ad.5 + 
        mortalidad_predicha.5 + smoker_recatsmoker.5 + time_of_hs_recaturgent_or_emergent.5 + 
        lvad_recatiabp.5 + pctspcts.6 + endocarditisendocarditis.6 + 
        hypothermia.6 + cbp_time.6 + pm_adpm_dependent.6 + vis_ad.6 + 
        hs_duration.6 + pam_ad.6 + mortalidad_predicha.6 + cannulation_type_art_recatperiferic.6 + 
        strata(trans), data = msebmt1, method = "breslow", cluster = id)->modelo_maximo2

coxph(formula = Surv(Tstart, Tstop, status) ~ time_of_hs_recaturgent_or_emergent.1 + 
       age.2 + lvef_change_pre_ad.2 + pctspcts.3 + ao_cross_clamp_time.3 + 
       vf_post.3 + pvc_ad.3 + hs_duration.3 + pam_pre.3 + pam_ad.3 + 
       time_of_hs_recaturgent_or_emergent.3 + hematíes_post.4 + 
       vis_ad.4 + is_ad.4 + pam_ad.4 + pctspcts.5 + cbp_time.5 + 
       ao_cross_clamp_time.5 + hs_typediseccióndiseccion.5 + is_ad.5 + 
       mortalidad_predicha.5 + smoker_recatsmoker.5 + time_of_hs_recaturgent_or_emergent.5 + 
       lvad_recatiabp.5 + pctspcts.6 + endocarditisendocarditis.6 + 
       hypothermia.6 + cbp_time.6 + pm_adpm_dependent.6 + vis_ad.6 + 
       hs_duration.6 + pam_ad.6 + mortalidad_predicha.6 + cannulation_type_art_recatperiferic.6 + 
       strata(trans), data = msebmt1, method = "breslow", cluster = id)->modelo_minimo2

#no aportan:
#aportaran: ninguno
#Posible interés 2º ronda y cerquita significación pero no valen 
#pam_pre.1 
#cardioplegic_sol_recatsanguinea.1
#endocarditisendocarditis.1 
#weight_of_proceduretwo_major_procedures.2 
#time_of_hs_recaturgent_or_emergent.2
#cfs.4
#hematíes_post.6

# Log-likelihood y grados de libertad
logLik_modelo_maximo2 <- logLik(modelo_maximo2);logLik_modelo_minimo2 <- logLik(modelo_minimo2); df1 <- attr(logLik_modelo_maximo2, "df");df2 <- attr(logLik_modelo_minimo2, "df")
LRT_statistic2 <- 2 * (logLik_modelo_maximo2 - logLik_modelo_minimo2);p_value <- pchisq(LRT_statistic2, df = abs(df1 - df2), lower.tail = FALSE)# Prueba de la razón de verosimilitud (LRT)
AIC_modelo_maximo2 <- AIC(modelo_maximo2);BIC_modelo_maximo2 <- BIC(modelo_maximo2); AIC_modelo_minimo2 <- AIC(modelo_minimo2);BIC_modelo_minimo2 <- BIC(modelo_minimo2)
# Resultados
cat("Modelo maximo - Log-likelihood:", logLik_modelo_maximo2, "| Grados de libertad:", df1, "\n","Modelo maximo - AIC:", AIC_modelo_maximo2, "| BIC:", BIC_modelo_maximo2, "\n")
cat("Modelo minimo - Log-likelihood:", logLik_modelo_minimo2, "| Grados de libertad:", df2, "\n","Modelo minimo - AIC:", AIC_modelo_minimo2, "| BIC:", BIC_modelo_minimo2, "\n")
cat("Estadístico LRT:", LRT_statistic, "| Valor p:", p_value, "\n")
# Conclusión
if (p_value < 0.05) {cat("El modelo maximo es significativamente mejor que el reducido (p <", p_value, ").\n")} else { cat("No hay una diferencia significativa entre los modelos, mejor el reducido (modelo_minimo) (p =", p_value, ").\n")}

###
#
#lvad_recatiabp.5  EE muy grande por pocas observaciones es significativo el modelo pero hay que cargarsela
#hs_typediseccióndiseccion.5 igual


####MODELO SEMIFINAL####
coxph(formula = Surv(Tstart, Tstop, status) ~ time_of_hs_recaturgent_or_emergent.1 + 
        age.2 + 
        lvef_change_pre_ad.2 + 
        pctspcts.3 + #fuera
        ao_cross_clamp_time.3 + 
        vf_post.3 + 
        pvc_ad.3 + 
        hs_duration.3 + #fuera
        pam_pre.3 + 
        pam_ad.3 + 
        time_of_hs_recaturgent_or_emergent.3 + 
        hematíes_post.4 + 
        vis_ad.4 + 
        is_ad.4 + #fuera
        pam_ad.4 + #fuera
        pctspcts.5 + #fuera
        cbp_time.5 + #fuera
        ao_cross_clamp_time.5 + #fuera 
        is_ad.5 +  #fuera
        mortalidad_predicha.5 + #fuera
        smoker_recatsmoker.5 + 
        time_of_hs_recaturgent_or_emergent.5 + 
        pctspcts.6 + #fuera
        endocarditisendocarditis.6 + #fuera 
        hypothermia.6 + #fuera
        cbp_time.6 + #fuera
        pm_adpm_dependent.6 + #fuera 
        vis_ad.6 + #fuera
        hs_duration.6 + #fuera
        pam_ad.6 + 
        mortalidad_predicha.6 + #fuera
        cannulation_type_art_recatperiferic.6 + #fuera
        strata(trans), data = msebmt1, method = "breslow", cluster = id)->modelo_casi_final;summary(modelo_casi_final)
vif(modelo_casi_final)
##dar una vuelta a ao_cross_clamp_time.5 y .6 con sus respectivos cbp time
       # pam_pre.1 + #nueva
        #endocarditisendocarditis.1+ #nueva
        #cfs.4+#nueva
        #rvef_change_pre_ad.4+#nueva
        #hematíes_post.6+#nueva

#3º ronda codigo para probar quitando terminos colineales 
coxph(formula = Surv(Tstart, Tstop, status) ~ 
        time_of_hs_recaturgent_or_emergent.1 + 
        is_ad.1  + #nueva
        age.1 + #nueva
        pvc_ad.1 +#nueva
        age.2 + 
        lvef_change_pre_ad.2 + 
        ao_cross_clamp_time.3 + 
        vf_post.3 + 
        pam_ad.3 + 
        time_of_hs_recaturgent_or_emergent.3 + 
        hematíes_post.4 + 
        vis_ad.4 + 
        rvef_change_pre_ad.4 + #nueva
        pctspcts.5 + 
        is_ad.5 + 
        smoker_recatsmoker.5 + 
        time_of_hs_recaturgent_or_emergent.5 + 
        pam_ad.6 + 
        hematíes_post.6 + #nueva
        strata(trans), data = msebmt1, method = "breslow", cluster = id)->modelo_maximo3

coxph(formula = Surv(Tstart, Tstop, status) ~ 
        time_of_hs_recaturgent_or_emergent.1 + 
        is_ad.1  + #nueva
        age.1 + #nueva
        pvc_ad.1 +#nueva
        age.2 + 
        lvef_change_pre_ad.2 + 
        ao_cross_clamp_time.3 + 
        vf_post.3 + 
        pam_ad.3 + 
        time_of_hs_recaturgent_or_emergent.3 + 
        hematíes_post.4 + 
        vis_ad.4 + 
        rvef_change_pre_ad.4 + #nueva
        pctspcts.5 + 
        is_ad.5 + 
        smoker_recatsmoker.5 + 
        time_of_hs_recaturgent_or_emergent.5 + 
        pam_ad.6 + 
        hematíes_post.6 + #nueva
        strata(trans), data = msebmt1, method = "breslow", cluster = id)->modelo_minimo3

#termplot(modelo_maximo3,terms = "pspline(lvef_change_pre_ad.2)")
#no aportan: vis_ad.6  cannulation_type_art_recatperiferic.6
#vif(modelo_minimo3)
# Log-likelihood y grados de libertad
logLik_modelo_maximo3 <- logLik(modelo_maximo3);logLik_modelo_minimo3 <- logLik(modelo_minimo3); df1 <- attr(logLik_modelo_maximo3, "df");df2 <- attr(logLik_modelo_minimo3, "df")
LRT_statistic3 <- 2 * (logLik_modelo_maximo3 - logLik_modelo_minimo3);p_value <- pchisq(LRT_statistic3, df = abs(df1 - df2), lower.tail = FALSE)# Prueba de la razón de verosimilitud (LRT)
AIC_modelo_maximo3 <- AIC(modelo_maximo3);BIC_modelo_maximo3 <- BIC(modelo_maximo3); AIC_modelo_minimo3 <- AIC(modelo_minimo3);BIC_modelo_minimo3 <- BIC(modelo_minimo3)
# Resultados
cat("Modelo maximo - Log-likelihood:", logLik_modelo_maximo3, "| Grados de libertad:", df1, "\n","Modelo maximo - AIC:", AIC_modelo_maximo3, "| BIC:", BIC_modelo_maximo3, "\n")
cat("Modelo minimo - Log-likelihood:", logLik_modelo_minimo3, "| Grados de libertad:", df2, "\n","Modelo minimo - AIC:", AIC_modelo_minimo3, "| BIC:", BIC_modelo_minimo3, "\n")
cat("Estadístico LRT:", LRT_statistic, "| Valor p:", p_value, "\n")
# Conclusión
if (p_value < 0.05) {cat("El modelo maximo es significativamente mejor que el reducido (p <", p_value, ").\n")} else { cat("No hay una diferencia significativa entre los modelos, mejor el reducido (modelo_minimo) (p =", p_value, ").\n")}
summary(modelo_minimo3)
summary(modelo_maximo3)

coxph(formula = Surv(Tstart, Tstop, status) ~ 
        time_of_hs_recaturgent_or_emergent.1 + 
        is_ad.1 + #nueva #nueva
        age.1 + #nueva
        pvc_ad.1 +#nueva
        age.2 + 
        lvef_change_pre_ad.2 + 
        ao_cross_clamp_time.3 + 
        vf_post.3 + 
        pam_ad.3 + 
        time_of_hs_recaturgent_or_emergent.3 + 
        hematíes_post.4 + 
        vis_ad.4 + 
        rvef_change_pre_ad.4 + #nueva
        pctspcts.5 + 
        is_ad.5 + 
        smoker_recatsmoker.5 + 
        time_of_hs_recaturgent_or_emergent.5 + 
        pam_ad.6 + 
        hematíes_post.6 + #nueva
        strata(trans), data = msebmt1, method = "breslow", cluster = id)->modelo_final
summary(modelo_final)

---------------------------------------
#######Testeo PROPIEDAD MARJOV#####
 msebmt1$Tstop <- ifelse(msebmt1$Tstop <= msebmt1$Tstart, msebmt1$Tstart + 1e-5, msebmt1$Tstop)

#Paso 1 Variable tiempo dependiente entrada estado
#Añadir tiempo desde entrada al estado anterior. Tstart es el tiempo de entrada al estado actual.
#Al asignar Tstart a time_in_state, “El tiempo que el paciente ha estado en este estado es igual al tiempo desde que entró en él.”
#cada fila representa una transición potencial, y Tstart marca el inicio del intervalo de riesgo para esa transición.
msebmt1$time_in_state <- msebmt1$Tstart

# Crear fórmula del modelo final
formula_markov_final <- as.formula(
  paste("Surv(Tstart, Tstop, status) ~ strata(trans) +", 
        paste(c("time_of_hs_recaturgent_or_emergent.1", "is_ad.1", "age.1", "pvc_ad.1",
                "age.2", "lvef_change_pre_ad.2", "ao_cross_clamp_time.3", "vf_post.3",
                "pam_ad.3", "time_of_hs_recaturgent_or_emergent.3", "hematíes_post.4",
                "vis_ad.4", "rvef_change_pre_ad.4", "pctspcts.5", "is_ad.5",
                "smoker_recatsmoker.5", "time_of_hs_recaturgent_or_emergent.5",
                "pam_ad.6", "hematíes_post.6"), collapse = " + ")))
# Crear fórmula del modelo semi-Markov
formula_semi_markov_final <- update(formula_markov_final, . ~ . + time_in_state)
# Ajustar modelos
cox_markov_final_nocluster <- coxph(formula_markov_final, data = msebmt1, method = "breslow")
cox_semi_markov_final_nocluster <- coxph(formula_semi_markov_final, data = msebmt1, method = "breslow")
# Comparar modelos
anova(cox_markov_final, cox_semi_markov_final) #no signficativo cumple supuesto markoviano

#Paso 2: Incluir covariables de historia previa
#1.Creamos formula extendida
covariables_historia <- c("charlson", "cfs", "nyha", "vis_pre", "is_pre", "pam_pre", 
                          "age", "smoker_recat", "copd", "hta", "dl", "pvd", "af")
formula_historia <- as.formula(
  paste("Surv(Tstart, Tstop, status) ~ strata(trans) +", 
        paste(c("time_of_hs_recaturgent_or_emergent.1", "is_ad.1", "age.1", "pvc_ad.1",
                "age.2", "lvef_change_pre_ad.2", "ao_cross_clamp_time.3", "vf_post.3",
                "pam_ad.3", "time_of_hs_recaturgent_or_emergent.3", "hematíes_post.4",
                "vis_ad.4", "rvef_change_pre_ad.4", "pctspcts.5", "is_ad.5",
                "smoker_recatsmoker.5", "time_of_hs_recaturgent_or_emergent.5",
                "pam_ad.6", "hematíes_post.6", covariables_historia), collapse = " + ")))
#2.ajustamos modelo sin cluster
modelo_historia <- coxph(formula_historia, data = msebmt1, method = "breslow")
cox_markov_final_nocluster$n; modelo_historia$n #difieren en numero observaciones

#Crear vector con todas las variables implicadas
vars_modelo_completo <- c("Tstart", "Tstop", "status", "trans", 
                          "time_of_hs_recaturgent_or_emergent.1", "is_ad.1", "age.1", "pvc_ad.1",
                          "age.2", "lvef_change_pre_ad.2", "ao_cross_clamp_time.3", "vf_post.3",
                          "pam_ad.3", "time_of_hs_recaturgent_or_emergent.3", "hematíes_post.4",
                          "vis_ad.4", "rvef_change_pre_ad.4", "pctspcts.5", "is_ad.5",
                          "smoker_recatsmoker.5", "time_of_hs_recaturgent_or_emergent.5",
                          "pam_ad.6", "hematíes_post.6", covariables_historia)

#Filtrar observaciones completas
msebmt1_completo <- msebmt1[complete.cases(msebmt1[, vars_modelo_completo]), ]
#Ajustar modelos sobre el mismo subconjunto
cox_markov_final_clean <- coxph(formula_markov_final, data = msebmt1_completo, method = "breslow")
modelo_historia_clean <- coxph(formula_historia, data = msebmt1_completo, method = "breslow")
#Comparar modelos
anova(cox_markov_final_clean, modelo_historia_clean)

#p<0.1 no hay evidencia estadísticamente significativa (p < 0.05) de que las covariables de historia previa mejoren el modelo
#o se rechaza el supuesto Markoviano: el riesgo de transición no depende significativamente del historial clínico previo, 

------------------------------------------------------------
#1. Ajustamos un modelo multiestado Markoviano con tus covariables clínicas, usando coxph() y strata(trans). sin cluster para compatibilidad con msfit
modelo_base <- coxph(formula_markov_final, data = msebmt1, method = "breslow")


#3. Creamos newdata con una fila por transición, usando valores genéricos (ceros o valores medios), no todos los pacientes, porque:
#Creas newdata con covariables modelo
#Crear un paciente tipo para estimar riesgos acumulados
newdata <- msebmt1[1, ]  # o usar aggregate para obtener valores medios
# Asegúrate de que newdata tenga todas las covariables del modelo
newdata <- newdata[, all.vars(formula_markov_final)]


#Debes crear un data.frame con una fila por cada transición definida en tu matriz tmat1, y en cada fila incluir:Las covariables del modelo.Una columna "strata" con el número de transición correspondiente.
# Obtener número de transiciones
num_trans <- max(tmat1, na.rm = TRUE)
# Crear una fila por transición con valores medios o ceros
newdata <- data.frame(matrix(0, nrow = num_trans, ncol = length(all.vars(formula_markov_final))))
colnames(newdata) <- all.vars(formula_markov_final)
# Añadir columna 'strata' con valores del 1 al número de transiciones:
newdata$strata <- 1:num_trans

#2. Calculamos los riesgos acumulados por transición usando msfit(), que requiere:
#Un modelo ajustado (modelo_base).
#Una matriz de transiciones (tmat1).
#Un conjunto de covariables (newdata) que represente un perfil clínico
msf <- msfit(modelo_base, trans = tmat1, newdata = newdata, variance = TRUE)

# Graficar riesgos acumulados por transición
plot(msf, xlab = "Tiempo", ylab = "Riesgo acumulado", col = 1:nrow(msf$Haz), lwd = 2)
legend("topright", legend = paste("Transición", msf$trans), col = 1:nrow(msf$Haz), lwd = 2)

plot(msf, use.ggplot = TRUE)
#Cada línea representa una transición específica entre estados clínico
#El eje X muestra el tiempo desde el inicio del seguimiento.El eje Y muestra el riesgo acumulado de realizar esa transición.
#Si las curvas crecen de forma aproximadamente lineal, el riesgo de transición es estable en el tiempo, lo que apoya el supuesto de homogeneidad temporal del modelo Markoviano.
#Si alguna curva muestra aceleración o desaceleración, podría indicar que el riesgo cambia con el tiempo, lo que violaría ese supuesto.
#En tu gráfico, las curvas parecen crecer de forma progresiva pero no abrupta, lo que sugiere que el modelo es razonablemente homogéneo en el tiempo

------------------------------------
#Colinealidad--
vif(modelo_final)
vars <- msebmt1[, c("time_of_hs_recaturgent_or_emergent.1", "is_ad.1", "hs_typediseccióndiseccion.1",
                    "age.1", "pvc_ad.1", "age.2", "lvef_change_pre_ad.2", "ao_cross_clamp_time.3",
                    "vf_post.3", "pam_ad.3", "time_of_hs_recaturgent_or_emergent.3", "hematíes_post.4",
                    "vis_ad.4", "rvef_change_pre_ad.4", "pctspcts.5", "is_ad.5", 
                    "smoker_recatsmoker.5", "time_of_hs_recaturgent_or_emergent.5", "pam_ad.6",
                    "hematíes_post.6")]

cor_matrix <- cor(na.omit(vars))
corrplot::corrplot(cor_matrix, method = "color")
#Proporcionalidad riesgos
cox.zph(modelo_final)
plot(cox.zph(modelo_final))
termplot(modelo_final ,terms = "pspline(pvc_ad.1)")


#pam_pre.3 es tiempo dependiente (viola moderadamente supuesto proporcionalidad) pero con tt() no cambia La violación del supuesto de proporcionalidad no es grave (como en tu caso: solo pam_pre.3, p=0.009, y el test global es aceptable),
#Y es confusora, como indicas (es decir, su exclusión afecta el estimador de otras variables),

########## Modelo sin pvc ##############

# Modelo completo (con PVC) 
modelo_completo <- coxph(
  Surv(Tstart, Tstop, status) ~ 
    time_of_hs_recaturgent_or_emergent.1 + 
    is_ad.1 + 
    age.1 + 
    pvc_ad.1 + 
    age.2 + 
    lvef_change_pre_ad.2 + 
    ao_cross_clamp_time.3 + 
    vf_post.3 + 
    pam_ad.3 + 
    time_of_hs_recaturgent_or_emergent.3 + 
    hematíes_post.4 + 
    vis_ad.4 + 
    rvef_change_pre_ad.4 + 
    pctspcts.5 + 
    is_ad.5 + 
    smoker_recatsmoker.5 + 
    time_of_hs_recaturgent_or_emergent.5 + 
    pam_ad.6 + 
    hematíes_post.6 +
    strata(trans),
  data = msebmt1,
  cluster = id)
# Modelo sin PVC 
modelo_sin_pvc <- coxph(
  Surv(Tstart, Tstop, status) ~ 
    time_of_hs_recaturgent_or_emergent.1 + 
    is_ad.1 + 
    age.1 + 
    age.2 + 
    lvef_change_pre_ad.2 + 
    ao_cross_clamp_time.3 + 
    vf_post.3 + 
    pam_ad.3 + 
    time_of_hs_recaturgent_or_emergent.3 + 
    hematíes_post.4 + 
    vis_ad.4 + 
    rvef_change_pre_ad.4 + 
    pctspcts.5 + 
    is_ad.5 + 
    smoker_recatsmoker.5 + 
    time_of_hs_recaturgent_or_emergent.5 + 
    pam_ad.6 + 
    hematíes_post.6 +
    strata(trans),
  data = msebmt1,
  cluster = id)


# EXTRAER COEFICIENTES
coef_full <- coef(modelo_completo)
coef_reduced <- coef(modelo_sin_pvc)
# Alinear nombres comunes
common_vars <- intersect(names(coef_full), names(coef_reduced))

coef_full_aligned <- coef_full[common_vars]
coef_reduced_aligned <- coef_reduced[common_vars]

# CAMBIO PORCENTUAL EN COEFICIENTES
cambio_pct <- (coef_reduced_aligned - coef_full_aligned) /
  coef_full_aligned * 100
round(cambio_pct, 1)

#Comparar errores estándar robustos
se_full <- sqrt(diag(vcov(modelo_completo)))
se_reduced <- sqrt(diag(vcov(modelo_sin_pvc)))
se_full_aligned <- se_full[common_vars]
se_reduced_aligned <- se_reduced[common_vars]
cambio_se_pct <- (se_reduced_aligned - se_full_aligned) /
  se_full_aligned * 100
round(cambio_se_pct, 1)

#
modelo_completo$concordance
modelo_sin_pvc$concordance


#####LIN + Dx + PH####
msebmt1 %>%
  group_by(trans) %>%
  summarise(n = n(), eventos = sum(status), .groups = "drop")

#######Ajuste global survminer#############
ggcoxdiagnostics(modelo_final, type = "deviance")
ggcoxdiagnostics(modelo_final, type = "martingale")
ggcoxdiagnostics(modelo_final, type = "schoenfeld", title = "Diagnostic plot")
ggcoxdiagnostics(modelo_final, type = "dfbeta", linear.predictions = TRUE)


ggforest(modelo_final, data=msebmt1)
#Ejemplo con transicion 6
subset6 <- subset(msebmt1, trans == 6)
modelo_trans6 <- coxph(
  Surv(Tstart, Tstop, status) ~ pam_ad.6 + hematíes_post.6,
  data = subset6,
  method = "breslow",
  cluster = id)
summary(modelo_trans6)
rmart6 <- resid(modelo_trans6, type = "martingale")# Martingala
rdev6 <- resid(modelo_trans6, type = "deviance")# Deviance
rcs6 <- abs(subset6$status - rmart6) # Cox-Snell
rschoen6 <- cox.zph(modelo_trans6) # Schoenfeld
# Cox-Snell
kmrcs6 <- survfit(Surv(rcs6, subset6$status) ~ 1)
plot(kmrcs6$time, -log(kmrcs6$surv),
     xlab = "Residuos de Cox-Snell", ylab = "-log(S)", main = "Cox-Snell - Transición 6")
abline(0, 1, col = "red", lwd = 2)
# Martingala
plot(rmart6, main = "Residuos de Martingala - Transición 6",
     xlab = "Índice", ylab = "Martingala", col = "blue", pch = 16)
abline(h = 0, col = "red")
# Deviance
plot(rdev6, main = "Residuos de Deviance - Transición 6",
     xlab = "Índice", ylab = "Deviance", col = "darkgreen", pch = 16)
abline(h = 0, col = "red")
# Schoenfeld
plot(rschoen6, main = "Test de Schoenfeld - Transición 6")

#Funcion global estudio de residuos basado en lo previo para poner segun transición
analizar_residuos_transicion <- function(data, transicion, covariables) {
  # Subset por transición
  subset_data <- subset(data, trans == transicion)
  # Crear fórmula dinámica
  formula_text <- paste("Surv(Tstart, Tstop, status) ~", paste(covariables, collapse = " + "))
  formula_modelo <- as.formula(formula_text)
  # Ajustar modelo
  modelo <- coxph(formula_modelo, data = subset_data, method = "breslow", cluster = id)
  print(summary(modelo))
  # Residuos
  rmart <- resid(modelo, type = "martingale")
  rdev <- resid(modelo, type = "deviance")
  rcs <- abs(subset_data$status - rmart)
  rschoen <- cox.zph(modelo)
  # Gráfico Cox-Snell
  kmrcs <- survfit(Surv(rcs, subset_data$status) ~ 1)
  plot(kmrcs$time, -log(kmrcs$surv),
       xlab = "Residuos de Cox-Snell", ylab = "-log(S)",
       main = paste("Cox-Snell - Transición", transicion), type = "s")
  abline(0, 1, col = "red", lwd = 2)
  # Martingala
  plot(rmart, main = paste("Residuos de Martingala - Transición", transicion),
       xlab = "Índice", ylab = "Martingala", col = "blue", pch = 16)
  abline(h = 0, col = "red")
  # Deviance
  plot(rdev, main = paste("Residuos de Deviance - Transición", transicion),
       xlab = "Índice", ylab = "Deviance", col = "darkgreen", pch = 16)
  abline(h = 0, col = "red")
  # Schoenfeld
  plot(rschoen, main = paste("Test de Schoenfeld - Transición", transicion))
  print(rschoen)
  # Martingala vs cada covariable para detectar no linealidades
  for (var in covariables) {
    xvar <- subset_data[[var]]
    if (is.numeric(xvar)) {
      plot(xvar, rmart,
           main = paste("Martingala vs", var),
           xlab = var, ylab = "Residuos de Martingala", pch = 16, col = "purple")
      abline(h = 0, col = "red")
      lines(lowess(xvar, rmart), col = "darkorange", lwd = 2)
    }
  }
}

analizar_residuos_transicion( #ejemplo transicion 6
  data = msebmt1,
  transicion = 6,
  covariables = c("pam_ad.6", "hematíes_post.6"))

#Residulos cada variable
analizar_residuos_transicion(msebmt1, 1, c("time_of_hs_recaturgent_or_emergent.1", "pvc_ad.1"))
analizar_residuos_transicion(msebmt1, 2, c("age.2", "lvef_change_pre_ad.2"))
analizar_residuos_transicion(msebmt1, 3, c("ao_cross_clamp_time.3", "vf_post.3", "pvc_ad.3", "pam_pre.3", "pam_ad.3", "time_of_hs_recaturgent_or_emergent.3"))
analizar_residuos_transicion(msebmt1, 4, c("hematíes_post.4", "vis_ad.4", "rvef_change_pre_ad.4"))
analizar_residuos_transicion(msebmt1, 5, c("pctspcts.5", "is_ad.5", "smoker_recatsmoker.5", "time_of_hs_recaturgent_or_emergent.5"))
analizar_residuos_transicion(msebmt1, 6, c("pam_ad.6", "hematíes_post.6"))


(C<-summary(modelo_final)$concordance[1])
(D<-2*C-1)
confint_df <- confint(modelo_final)  # IC de los coeficientes
hr_df <- exp(cbind(HR = coef(modelo_final), confint_df))  # HR e IC al 95%
round(hr_df, 3)  # Redondear a 2 decimales


"Valor negativo → empeoramiento de la RVEF
Valor positivo → mejora de la RVEF"
"rvef_change_pre_ad = as.numeric(rvef_pre) - as.numeric(rvef_ad)"


#######Bootstrap validacion interna######
set.seed(2025)
# 1. Función de bootstrap
boot_fun <- function(data, indices) {
  d <- data[indices, ]
  tryCatch({
    mod <- coxph(Surv(Tstart, Tstop, status) ~
                   time_of_hs_recaturgent_or_emergent.1 +
                   pvc_ad.1 + age.2 + lvef_change_pre_ad.2 +
                   ao_cross_clamp_time.3 + vf_post.3 + pvc_ad.3 +
                   pam_pre.3 + pam_ad.3 + time_of_hs_recaturgent_or_emergent.3 +
                   hematíes_post.4 + vis_ad.4 + rvef_change_pre_ad.4 +
                   pctspcts.5 + is_ad.5 + smoker_recatsmoker.5 +
                   time_of_hs_recaturgent_or_emergent.5 + pam_ad.6 +
                   hematíes_post.6 + strata(trans),
                 data = d,
                 cluster = id)
    return(summary(mod)$concordance[1])
  }, error = function(e) {
    return(NA)
  })
}
# 2. Ejecutar bootstrap
boot_results <- boot(data = msebmt1, statistic = boot_fun, R = 200)
# 3. Extraer valores válidos
boot_cindex_values <- boot_results$t[!is.na(boot_results$t)]
# 4. Intervalo de confianza BCa
boot_bca <- boot.ci(boot_results, type = "bca")
# 5. Estadísticas
mean_cindex <- mean(boot_cindex_values)
cat("Media del C-index:", round(mean_cindex, 3), "\n")
cat("IC 95% BCa:", round(boot_bca$bca[4:5], 3), "\n")
# 6. Histograma
hist(boot_cindex_values,
     breaks = 30,
     col = "#4C9F70",
     main = "Distribución del C-index (Bootstrap BCa)",
     xlab = "C-index",
     xlim = c(0.5, 0.8),
     border = "white")
abline(v = mean_cindex, col = "red", lwd = 2, lty = 2)
text(mean_cindex, 13, labels = round(mean_cindex, 3), pos =4, col = "red")
# 7. C-index original y corrección por optimismo
cindex_original <- 0.629  # o usa concordance(modelo_tt)$concordance
optimism <- cindex_original - mean_cindex
cindex_corrected <- cindex_original - optimism
cat("Optimismo estimado:", round(optimism, 3), "\n")
cat("C-index corregido:", round(cindex_corrected, 3), "\n")

#######Megafuncion CI bootstrap por transicion segun dos tipos BCA y percentiles#####
transiciones <- unique(msebmt1$trans)
bootstrap_cindex_trans <- function(modelo, datos, R = 1000) {
  boot_fun <- function(data, indices) {
    d <- data[indices, ]
    tryCatch({
      mod <- update(modelo, data = d, control = coxph.control(iter.max = 50))
      return(summary(mod)$concordance[1])
    }, error = function(e) return(NA))
  }
  set.seed(2025)
  boot(data = datos, statistic = boot_fun, R = R)
}


modelos_por_trans <- list()
for (t in transiciones) { #Ajustar modelos por transición
  datos_t <- subset(msebmt1, trans == t)
  modelo_t <- coxph(Surv(Tstart, Tstop, status) ~
                      time_of_hs_recaturgent_or_emergent.1 +
                      is_ad.1 + age.1 + pvc_ad.1 + age.2 + lvef_change_pre_ad.2 +
                      ao_cross_clamp_time.3 + vf_post.3 + pam_ad.3 +
                      time_of_hs_recaturgent_or_emergent.3 + hematíes_post.4 +
                      vis_ad.4 + rvef_change_pre_ad.4 + pctspcts.5 + is_ad.5 +
                      smoker_recatsmoker.5 + time_of_hs_recaturgent_or_emergent.5 +
                      pam_ad.6 + hematíes_post.6 + strata(trans),
                    data = datos_t, cluster = id)
  modelos_por_trans[[as.character(t)]] <- modelo_t
}


cindex_ic_por_trans <- list() # Calcular IC tipo percentil y BCa
for (t in transiciones) {
  datos_t <- subset(msebmt1, trans == t)
  modelo_t <- modelos_por_trans[[as.character(t)]]
  boot_res <- bootstrap_cindex_trans(modelo_t, datos_t, R = 1000)
  ci_perc <- boot.ci(boot_res, type = "perc")
  ci_bca <- boot.ci(boot_res, type = "bca")
  cindex_ic_por_trans[[as.character(t)]] <- list(
    cindex = mean(boot_res$t, na.rm = TRUE),
    lower_perc = ci_perc$percent[4],
    upper_perc = ci_perc$percent[5],
    lower_bca = ci_bca$bca[4],
    upper_bca = ci_bca$bca[5]
  )
}


######Bootstrap por transición #########

# 1. Definir transiciones
transiciones <- unique(msebmt1$trans)
# 2. Función bootstrap para el C-index
bootstrap_cindex_trans <- function(modelo, datos, R = 1000) {
  boot_fun <- function(data, indices) {
    d <- data[indices, ]
    tryCatch({
      mod <- update(modelo, data = d, control = coxph.control(iter.max = 50))
      return(summary(mod)$concordance[1])
    }, error = function(e) return(NA))
  }
  set.seed(2025)
  boot(data = datos, statistic = boot_fun, R = R)
}
# 3. Ajustar modelos por transición
modelos_por_trans <- list()
for (t in transiciones) {
  datos_t <- subset(msebmt1, trans == t)
  modelo_t <- coxph(Surv(Tstart, Tstop, status) ~
                      time_of_hs_recaturgent_or_emergent.1 +
                      is_ad.1 + age.1 + pvc_ad.1 + age.2 + lvef_change_pre_ad.2 +
                      ao_cross_clamp_time.3 + vf_post.3 + pam_ad.3 +
                      time_of_hs_recaturgent_or_emergent.3 + hematíes_post.4 +
                      vis_ad.4 + rvef_change_pre_ad.4 + pctspcts.5 + is_ad.5 +
                      smoker_recatsmoker.5 + time_of_hs_recaturgent_or_emergent.5 +
                      pam_ad.6 + hematíes_post.6 + strata(trans),
                    data = datos_t, cluster = id)
  modelos_por_trans[[as.character(t)]] <- modelo_t
}
# 4. Calcular IC tipo "all" por transición
cindex_ic_por_trans <- list()
for (t in transiciones) {
  datos_t <- subset(msebmt1, trans == t)
  modelo_t <- modelos_por_trans[[as.character(t)]]
  boot_res <- bootstrap_cindex_trans(modelo_t, datos_t, R = 1000)
  ci_all <- boot.ci(boot_res, type = "all")
  cindex_ic_por_trans[[as.character(t)]] <- list(
    cindex = mean(boot_res$t, na.rm = TRUE),
    norm = ci_all$normal[2:3],
    basic = ci_all$basic[4:5],
    perc = ci_all$percent[4:5],
    bca = ci_all$bca[4:5],
    stud = if (!is.null(ci_all$student)) ci_all$student[4:5] else c(NA, NA)
  )
}
#5.Generar data frame de lo previo
# Crear data.frame a partir de la lista
df_cindex <- do.call(rbind, lapply(names(cindex_ic_por_trans), function(t) {
  res <- cindex_ic_por_trans[[t]]
  data.frame(
    Transicion = t,
    C_index = res$cindex,
    Norm_Lower = res$norm[1],
    Norm_Upper = res$norm[2],
    Basic_Lower = res$basic[1],
    Basic_Upper = res$basic[2],
    Perc_Lower = res$perc[1],
    Perc_Upper = res$perc[2],
    BCa_Lower = res$bca[1],
    BCa_Upper = res$bca[2],
    Stud_Lower = res$stud[1],
    Stud_Upper = res$stud[2]
  )
}))
# Ver tabla
print(df_cindex)

# Redondear todas las columnas numéricas a 2 decimales
df_cindex[, sapply(df_cindex, is.numeric)] <- 
  round(df_cindex[, sapply(df_cindex, is.numeric)], 3)
# Ver el resultado
print(df_cindex)
# Convertir df_cindex a formato largo para graficar
df_long <- df_cindex %>%
  pivot_longer(cols = ends_with("Lower"), names_to = "Metodo_Lower", values_to = "Lower") %>%
  pivot_longer(cols = ends_with("Upper"), names_to = "Metodo_Upper", values_to = "Upper") %>%
  filter(sub("_Lower", "", Metodo_Lower) == sub("_Upper", "", Metodo_Upper)) %>%
  mutate(Metodo = sub("_Lower", "", Metodo_Lower))
# Graficar
ggplot(df_long, aes(x = Transicion, y = C_index, color = Metodo, shape = Metodo)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2,
                position = position_dodge(width = 0.5)) +
  labs(title = "Intervalos de confianza del C-index por transición",
       x = "Transición", y = "C-index") +
  theme_minimal()
      
# Crear un nuevo paciente
new_patient <- data.frame(
  Tstart = 0,
  Tstop = 100,
  status = 0,
  trans = 1,  # numérica, igual que en el dataset original
  pam_pre.3 = 80,
  time_of_hs_recaturgent_or_emergent.1 = 1,
  pvc_ad.1 = 0,
  age.2 = 65,
  lvef_change_pre_ad.2 = 0.2,
  ao_cross_clamp_time.3 = 90,
  vf_post.3 = 0,
  pvc_ad.3 = 1,
  pam_ad.3 = 70,
  time_of_hs_recaturgent_or_emergent.3 = 0,
  hematíes_post.4 = 2,
  vis_ad.4 = 0.8,
  rvef_change_pre_ad.4 = 0.1,
  pctspcts.5 = 0,
  is_ad.5 = 1,
  smoker_recatsmoker.5 = 0,
  time_of_hs_recaturgent_or_emergent.5 = 0,
  pam_ad.6 = 75,
  hematíes_post.6 = 1)

# Predecir el riesgo relativo (riesgo instantáneo comparado)
predict(modelo_tt, newdata = new_patient, type = "risk")

###FOREST PLOt#####
# Extraemos los coeficientes (log HR), intervalos de confianza y nombres de las variables
coeficientes <- coef(summary(modelo_final))
variables <- rownames(coeficientes)
logHR <- coeficientes[, "coef"]
HR <- round(exp(logHR), 3)  # Redondear el HR a 3 decimales
conf.low <- round(exp(confint(modelo_final)[, 1]), 3)  # Redondear conf.low a 3 decimales
conf.high <- round(exp(confint(modelo_final)[, 2]), 3)  # Redondear conf.high a 3 decimales
p.values <- round(coeficientes[, "Pr(>|z|)"], 3)  # Redondear los p.values a 3 decimales
#tabla_resultados %>%mutate(across(where(is.numeric), ~ round(., 3)))
df_forest <- data.frame(
  term = factor(variables, levels = variables),  # Mantener el orden original de las variables
  HR = HR,
  conf.low = conf.low,
  conf.high = conf.high,
  p.value = p.values)
# Recortar los valores que exceden el rango de 0 a 15
df_forest$significativo <- df_forest$p.value < 0.05 # Añadimos una columna para indicar significancia (p.value < 0.05)
df_forest$label <- sprintf("%.2f (%.2f, %.2f)", df_forest$HR, df_forest$conf.low, df_forest$conf.high) # Creamos etiquetas para mostrar el HR y los intervalos de confianza
# Crear el forest plot
ggplot(df_forest, aes(x = HR, y = term)) +
  geom_point(aes(color = significativo), size = 4) +  # Disminuir el tamaño del punto
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = significativo), height = 0.3, size = 1.2) +  # Aumentar el grosor de las barras de error
  geom_text(aes(label = label), vjust = -0.5, hjust = -0.1, size = 3.5) +  # Mostrar el HR y los intervalos de confianza encima de los puntos
  xlab("Hazard Ratio (HR)") +
  ylab("Covariables") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +  # Línea de referencia en HR = 1
  scale_color_manual(values = c("black", "red"), labels = c("No significativo", "Significativo p<0.05")) +  # Mostrar la leyenda con etiquetas
  theme_minimal() +
  theme(legend.position = "right",  # Mostrar la leyenda a la derecha
        legend.title = element_blank(),  # Ocultar título de la leyenda
        legend.text = element_text(size = 12),  # Tamaño del texto en la leyenda
        axis.text = element_text(size = 12),  # Aumentar tamaño del texto en ejes
        axis.title = element_text(size = 14)) +  # Aumentar tamaño de los títulos de los ejes
  xlim(0, 41)  # Ajustar el eje X para que vaya de 0 a 15
