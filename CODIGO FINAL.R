############# LIMPIEZA ########
pkgs <- c("tidyverse", "gridExtra", "survival", "gtsummary", "skimr","survminer")
lapply(pkgs, require, character.only = TRUE)
#r1<-read_csv("C:/Users/alberto.garrido/Downloads/r1_comorb - Hoja 1.csv")
#r2<- read_csv("C:/Users/alberto.garrido/Downloads/r2_qx_protocol - Hoja 1.csv")
#r3<-read_csv("C:/Users/alberto.garrido/Downloads/r3_hmd_qx - Hoja 1.csv")
#r4<- read_csv("C:/Users/alberto.garrido/Downloads/r4_outcomes - Hoja 1.csv")
#setwd("C:/Users/Usuario/Desktop")
r1<-read_csv("r1_comorb - Hoja 1.csv")
r2<- read_csv("r2_qx_protocol - Hoja 1.csv")
r3<-read_csv("r3_hmd_qx - Hoja 1.csv")
r4<- read_csv("r4_outcomes - Hoja 1.csv")
merge(r1,r2, by = "nhc", all = TRUE)->aaa
merge(aaa,r3, by = "nhc", all = TRUE)->aaa
merge(aaa,r4, by = "nhc", all = TRUE)->aaa
aaa <- aaa[, !duplicated(names(aaa))]
aaa[order(aaa$patient_number),]->aaa

columns_to_convert <- c( "date_icu_ad", "start_hs", "end_hs", "na0.1", "na0.2", "na0.5","date_na_max", "date_dbt_max", 
                         "date_vsp_max", "date_blue_max","date_vasoactive_end", "date_inotropic_end", "date_af", "date_troponine_max", "lac3mmol", "lac5mmol", 
                         "date_lactate_max", "eb10", "eb5", "date_be_max","date_hco3_max", "pf300", "pf200", "pf100", "date_extubation", "date_traqueostomy", 
                         "cr0.3ocrx1.5", "0.5ml/kg/h_en_6-12h", "crx2", "0.5ml/kg/h_en_12h", "crx3o4mg/dl", "0.3ml/kg/h_24h", "date_rrt",  "date_cr_max", "plaq150000", 
                         "plaq100000", "plaq50000", "plaq20000", "bb2", "bb6", "date_drainage", "date_reintervention", "date_icu_discharge", "date_hospital_discharge", "date_death")
#Fechas a formato legible
convert_columns_to_date <- function(df, columns) {
  for (col in columns) {
  df[[col]] <- dmy_hm(df[[col]])
  }
  return(df)}
aaa <- convert_columns_to_date(aaa, columns_to_convert)
dmy(aaa$year_of_birth)->aaa$year_of_birth
dmy_hm(as.character(aaa$reingreso))->aaa$reingreso
#Comas cambiamos por puntos
columns_to_convert <- c("ckd","na_dose_pre", "dbt_dose_pre", "mlr_dose_pre", "vsp_dose_pre", "lvef_pre", "na_dose_ad", "dbt_dose_ad", "mlr_dose_ad", "na_dose_6", "dbt_dose_6", 
                        "vsp_dose_6", "lvef_icu", "lvef_ad", "uresis_6", "balance_6","na_max", "dbt_max", "vsp_max", "date_blue_max","troponine_max", "lactate_max", "be_max", 
                        "hco3_max", "cr_max")
convert_columns_to_numeric <- function(df, columns) {
  for (col in columns) {
    df[[col]] <- as.numeric(gsub(",", ".", df[[col]]))
  }
  return(df)}
aaa <- convert_columns_to_numeric(aaa, columns_to_convert)

#aaa[order(aaa$patient_number),]->aaa
names(aaa)

#Comorbilidades r1
aaa %>%  mutate(imc=weight/((size/100)^2),
               hta = ifelse(is.na(hta), "no_hta", hta),
               dl= ifelse(is.na(dl), "no_dl", dl),
               smoker=ifelse(is.na(smoker),"no_smoker",smoker),
               dm=ifelse(is.na(dm),"no_dm",dm),
               ic=ifelse(is.na(ic),"no_ic",ic),
               pcts=ifelse(is.na(pcts),"no_pcts",pcts),
               pvd=ifelse(is.na(pvd),"no_pvd",pvd),
               af=ifelse(is.na(af),"no_af",af),
               hf=ifelse(is.na(hf),"no_hf",hf),
               copd=ifelse(is.na(copd),"no_copd",copd),
               osas=ifelse(is.na(osas),"no_osas",osas),
               antithrombotic=ifelse(is.na(antithrombotic),"no",antithrombotic),
               nyha = ifelse(is.na(nyha),"nyha_1",nyha),  
               age=floor(as.numeric(difftime(aaa$date_icu_ad,aaa$year_of_birth, units = "days"))/365.25),
               sex_factor = ifelse(sex=="hombre", 1, ifelse(sex == "mujer", 0.85, NA)),
               ckd_cockcroft_gault =((140-age)*weight*sex_factor)/(72*ckd),
               estadio_erc = case_when(ckd_cockcroft_gault >= 90 ~ "Estadio 1",
                                       ckd_cockcroft_gault >= 60 & ckd_cockcroft_gault < 90 ~ "Estadio 2",
                                       ckd_cockcroft_gault >= 45 & ckd_cockcroft_gault < 60 ~ "Estadio 3a",
                                       ckd_cockcroft_gault >= 30 & ckd_cockcroft_gault < 45 ~ "Estadio 3b",
                                       ckd_cockcroft_gault >= 15 & ckd_cockcroft_gault < 30 ~ "Estadio 4",
                                       ckd_cockcroft_gault < 15 ~ "Estadio 5",
                                       TRUE ~ NA_character_),
               vis_pre=dbt_dose_pre+100*na_dose_pre+ifelse(vsp_dose_pre==0,0,10000*vsp_dose_pre/weight),
               vis_ad=dbt_dose_ad+100*na_dose_ad+ifelse(lvs_qx=="no_lvs",0,(12.5/50)*1/60/weight),
               vis_6=dbt_dose_6+100*na_dose_6+ifelse(vsp_dose_6==0,0,10000*vsp_dose_6/weight))->aaa
as.numeric(difftime(aaa$end_hs,aaa$start_hs, units = "min"))->aaa$hs_duration

#Generar variables dummy para cada variable categórica
model.matrix(~ hs_type - 1, data = aaa) %>%as.data.frame() %>% select(-hs_typeavr,-hs_typecabg,-hs_typemvr,-hs_typeta)->bbb
colnames(bbb) <- gsub(" ", "_", colnames(bbb))
cbind(aaa,bbb)->aaa
aaa %>%   mutate(
  hs_type_valvular = ifelse(hs_type %in% c("avr", "mvr", "tvr", "tricuspid_annuloplasty", "mitral_annuloplasty") |
                      hs_typeII %in% c("avr", "mvr", "tvr", "tricuspid_annuloplasty", "mitral_annuloplasty") |
                      hs_typeIII %in% c("avr", "mvr", "tvr", "tricuspid_annuloplasty", "mitral_annuloplasty"), "valvular","no_valvular"),
  hs_type_cabg=ifelse( hs_type %in% c("cabg")|
                 hs_typeII %in% c("cabg")|
                 hs_typeIII%in% c("cabg"), "cabg","no_cabg"),
  hs_type_avr = ifelse(!is.na(hs_type) & hs_type == "avr" |
                         !is.na(hs_typeII) & hs_typeII == "avr" |
                         !is.na(hs_typeIII) & hs_typeIII == "avr", "avr", "no_avr"),
  hs_type_mvr = ifelse(!is.na(hs_type) & hs_type == "mvr" |
                         !is.na(hs_typeII) & hs_typeII == "mvr" |
                         !is.na(hs_typeIII) & hs_typeIII == "mvr", "mvr", "no_mvr"),
  hs_type_ta= ifelse(!is.na(hs_type) & hs_type == "ta" |
                       !is.na(hs_typeII) & hs_typeII == "ta" |
                       !is.na(hs_typeIII) & hs_typeIII == "ta"| hs_typedisección==1, "ta", "no_ta"),
  hs_type_tvr= ifelse(!is.na(hs_type) & hs_type == "tvr" |
                        !is.na(hs_typeII) & hs_typeII == "tvr" |
                        !is.na(hs_typeIII) & hs_typeIII == "tvr", "tvr", "no_tvr"),
  hs_type_tricuspid_annuloplasty= ifelse(!is.na(hs_type) & hs_type == "tricuspid_annuloplasty" |
                                           !is.na(hs_typeII) & hs_typeII == "tricuspid_annuloplasty" |
                                           !is.na(hs_typeIII) & hs_typeIII == "tricuspid_annuloplasty", "tricuspid_annuloplasty", "no_tricuspid_annuloplasty"))->aaa

aaa %>% mutate(
  n_major_procedures = rowSums(across(c(hs_type_avr, hs_type_mvr, hs_type_cabg, hs_type_ta, hs_type_tvr, hs_type_tricuspid_annuloplasty),
                                      ~ . == gsub("hs_type_", "", cur_column())), na.rm = TRUE),
  weight_of_procedure = case_when(
    n_major_procedures == 1 & hs_type_cabg == "cabg" ~ "cabg",
    n_major_procedures == 1 & hs_type_cabg != "cabg" ~ "non_cabg_major",
    n_major_procedures == 2 ~ "two_major_procedures",
    n_major_procedures >= 3 ~ "three_major_procedures",
    TRUE ~ NA_character_)) -> aaa

#Casos con NA en weight_of_procedure
aaa %>% filter(is.na(weight_of_procedure)) %>% select(nhc, hs_type, hs_typeII, hs_typeIII)
table(aaa$weight_of_procedure, useNA = "ifany")

#model.matrix(~ hs_typeII - 1, data = aaa)
#dummy_hs_typeIII <- model.matrix(~ hs_typeIII - 1, data = aaa)
aaa$rvef_ad <- fct_relevel(aaa$rvef_ad, "no_rvef", "mild_rvef", "moderate_rvef", "severe_rvef")
aaa$rvef_pre <- fct_relevel(aaa$rvef_pre, "no_rvef", "mild_rvef", "moderate_rvef", "severe_rvef")
aaa$dl<-fct_relevel(aaa$dl,"no_dl","dl")
aaa$smoker<-fct_relevel(aaa$smoker, "no_smoker","ex_smoker","smoker")
aaa$dm<-fct_relevel(aaa$dm, "no_dm","dm_antidiabetic","dm_insuline")
aaa$af<-fct_relevel(aaa$af, "no_af", "af/aF")
aaa$hf<-fct_relevel(aaa$hf, "no_hf","HFpEF","HFrEF")
aaa$copd<-fct_relevel(aaa$copd, "no_copd","copd" )
aaa$endocarditis<-fct_relevel(aaa$endocarditis,"no_endocarditis","endocarditis")
aaa$cardioplegic_sol<-fct_relevel(aaa$cardioplegic_sol, "sanguinea","nido","buckberg","saline","no_especificada")
aaa$lvs_qx<-fct_relevel(aaa$lvs_qx, "no_lvs","lvs_pre","lvs_intra")
aaa$hs_type_cabg<-fct_relevel(aaa$hs_type_cabg,"no_cabg","cabg")
aaa$hs_type_avr<-fct_relevel(aaa$hs_type_avr, "no_avr","avr")
aaa$hs_type_mvr<-fct_relevel(aaa$hs_type_mvr, "no_mvr","mvr")
aaa$hs_typedisección <- factor(aaa$hs_typedisección, levels = c(0, 1), labels = c("no_diseccion","diseccion"))         
aaa$lvad_pre<-fct_relevel(aaa$lvad_pre,"no_lvad","iabp")
aaa$lvad_ad<-fct_relevel(aaa$lvad_ad,"no","iabp")
aaa%>% mutate(lvad_ad = fct_explicit_na(lvad_ad, na_level = "no_lvad"))->aaa

#Hemodinamica r3
aaa %>% mutate(lvef_change_pre_ad=lvef_pre-lvef_ad,
               #lvef_change_ad_icu=lvef_ad-lvef_icu,
               #lvef_change_pre_icu=lvef_pre-lvef_icu,
               rvef_change_pre_ad = ifelse(is.na(rvef_pre) | is.na(rvef_ad), NA, as.numeric(rvef_pre) - as.numeric(rvef_ad)),
               psap=it_max+pvc_pre,
               is_pre=ifelse(!is.na(pas_pre),hr_pre/pas_pre,NA),
               is_ad=ifelse(!is.na(pas_ad), hr_ad/pas_ad, NA),
               is_6=ifelse(!is.na(pas_6), hr_6/pas_6, NA),
               pam_pre=(1/3)*pas_pre+(2/3)*pad_pre,
               pam_ad=(1/3)*pas_ad+(2/3)*pad_ad,
               pam_6=(1/3)*pas_6+(2/3)*pad_6,
               trasfusion=hematíes_post+plaquetas_post+pfc_post)->aaa
              #indice_shock_modificado_triaje=ifelse(!is.na(tas), fc/(((1/3)*tas)+((2/3)*tad)),NA),
              #indice_shock_diastolico_triaje=ifelse(!is.na(tad), fc/tad,NA),)
aaa %>% select(-na_dose_6,-dbt_dose_6,-vsp_dose_6,-pas_6,-pad_6,-pvc_6,-hr_6,-pm_6,-lvad_6,-uresis_6,-balance_6,-is_6,-pam_6,-...65,-vis_6)->aaa

#r4
aaa %>% mutate_if(is.character, as.factor)->aaa
sapply(aaa, class)

#########Calculo del euroescoreII########
#Definir coeficientes de las variables
coeficientes <- list(
  age = 0.0285181, sex_female = 0.2196434, insulin_dm = 0.3542749,chronic_pulmonary_dysfunction = 0.1886564, severe_neuromuscular_dysfunction = 0.2407181,
  renal_51_85 = 0.303553, renal_50 = 0.8592256, dialysis = 0.6421508,critical_preop = 1.086517, nyha_II = 0.1070545, nyha_III = 0.2958358, nyha_IV = 0.5597929,
  ccs_class_4 = 0.2226147, extracardiac_arteriopathy = 0.5360268, previous_cardiac_surgery = 1.118599, active_endocarditis = 0.6194522,
  lvef_31_50 = 0.3150652, lvef_21_30 = 0.8084096, lvef_20 = 0.9346919,recent_mi = 0.1528943, pasp_31_54 = 0.1788899, pasp_55 = 0.3491475,
  urgency_urgent = 0.3174673, urgency_emergency = 0.7039121, urgency_salvage = 1.362947,thoracic_aorta_surgery = 0.6527205)
#Función para calcular la mortalidad predicha
calcular_mortalidad <- function(euroscoreII) {
  ey <- exp(euroscoreII)
  return(ey / (1 + ey))}
#Calcular EuroSCORE II y mortalidad predicha
aaa %>%
  mutate(
  euroscoreII = -5.324537 +
    case_when(
        weight_of_procedure == "cabg" ~ 0,
        weight_of_procedure == "non_cabg_major" ~ 0.0062118,
        weight_of_procedure == "two_major_procedures" ~ 0.5521478,
        weight_of_procedure == "three_major_procedures" ~ 0.9724533,
        TRUE ~ 0)+
    if_else(age > 60, (age - 60) * coeficientes$age, 0) +
    if_else(sex == "mujer", coeficientes$sex_female, 0) +
    if_else(dm == "dm_insuline", coeficientes$insulin_dm, 0) +
    if_else(copd == "copd", coeficientes$chronic_pulmonary_dysfunction, 0) +
    case_when(
      ckd_cockcroft_gault < 51 ~ coeficientes$renal_50,
      ckd_cockcroft_gault <= 85 & ckd_cockcroft_gault >= 51 ~ coeficientes$renal_51_85,
      ckd_cockcroft_gault == "dialysis" ~ coeficientes$dialysis,
      TRUE ~ 0) +
    if_else(pcts == "pcts", coeficientes$previous_cardiac_surgery, 0) +
    if_else(!(cfs %in% c("1","2","3","4")),coeficientes$severe_neuromuscular_dysfunction, 0) +
    if_else(na_dose_pre > 0 | lvad_pre != "no" | dbt_dose_pre > 0, coeficientes$critical_preop, 0) +
    case_when(
      nyha == "nyha_2" ~ coeficientes$nyha_II,
      nyha == "nyha_3" ~ coeficientes$nyha_III,
      nyha == "nyha_4" ~ coeficientes$nyha_IV,
      TRUE ~ 0) +
    if_else(nyha == "nyha_4",coeficientes$ccs_class_4, 0) +
    if_else(pvd == "pvd", coeficientes$extracardiac_arteriopathy, 0) +
    if_else(endocarditis == "endocarditis", coeficientes$active_endocarditis, 0) +
    case_when(
      lvef_pre <= 20 ~ coeficientes$lvef_20,
      lvef_pre <= 30 & lvef_pre >= 21 ~ coeficientes$lvef_21_30,
      lvef_pre <= 50 & lvef_pre >= 31 ~ coeficientes$lvef_31_50,
      TRUE ~ 0) +
    if_else(ic != "no_ic", coeficientes$recent_mi, 0) +
    case_when(
      psap >= 55 ~ coeficientes$pasp_55,
      psap >= 31 & psap <= 54 ~ coeficientes$pasp_31_54,
      TRUE ~ 0) +
    case_when(
      time_of_hs == "urgent" ~ coeficientes$urgency_urgent,
      time_of_hs == "emergency" ~ coeficientes$urgency_emergency,
      time_of_hs == "salvage" ~ coeficientes$urgency_salvage,
      TRUE ~ 0) +
    if_else(hs_type_ta == "ta", coeficientes$thoracic_aorta_surgery, 0))%>%
  mutate(mortalidad_predicha = calcular_mortalidad(euroscoreII))->aaa

######Cirugias CON bomba
aaa %>% filter(ecls=="ecls") ->aaa

#muertos en total
aaa %>% filter(!is.na(date_death)) %>% select(nhc)

# Definir la función
crear_variables_supervivencia_para_todas <- function(data) {
  # Identificar las columnas de fecha en formato POSIXct
  date_cols <- names(data)[sapply(data, inherits, what = "POSIXct")]
  # Eliminar la columna de inicio si está en la lista de columnas de fecha
  date_cols <- setdiff(date_cols, "date_icu_ad")
  # Iterar sobre cada columna de fecha y crear las variables de tiempo y censura
  for (end_date_col in date_cols) {
    # Crear los nombres de las nuevas columnas
    time_var_name <- paste0(end_date_col, "_tiempo_hasta")
    censor_var_name <- paste0(end_date_col, "_censura_tiempo_hasta")
    # Calcular el tiempo hasta el evento
    tiempo_hasta_evento <- as.numeric(difftime(data[[end_date_col]], data$date_icu_ad, units = "hours"))
    # Identificar los índices de NA
    na_indices <- which(is.na(tiempo_hasta_evento))
    # En los casos donde el evento es NA, usar el tiempo hasta el alta o hasta la muerte
    tiempo_hasta_evento[na_indices] <- pmax(
      as.numeric(difftime(data$date_icu_discharge[na_indices], data$date_icu_ad[na_indices], units = "hours")),
      as.numeric(difftime(data$date_death[na_indices], data$date_icu_ad[na_indices], units = "hours")),
      na.rm = TRUE
    )
    # Crear la variable de censura
    data[[censor_var_name]] <- ifelse(is.na(data[[end_date_col]]), 0, 1)
    # Añadir la nueva columna de tiempo hasta el evento al data frame
    data[[time_var_name]] <- tiempo_hasta_evento
  }
  return(data)
}
aaa1 <- crear_variables_supervivencia_para_todas(aaa)
aaa1$reingreso

crear_variables_supervivencia_para_todas(aaa1$reingreso)->aaa1$reingreso
#### Recodificacion
as.numeric(aaa1$charlson)->aaa1$charlson
as.numeric(aaa1$cfs)->aaa1$cfs
as.numeric(aaa1$nyha)->aaa1$nyha
aaa1 %>% mutate(
  hf_recat  = factor(ifelse(hf  == "no_hf",  "no_hf",  "yes_hf")),
  dm_recat  = factor(ifelse(dm  == "no_dm",  "no_dm",  "yes_dm")),
  smoker_recat = factor(ifelse(smoker == "smoker", "smoker", "no_smoker")),
  ic_recat  = factor(ifelse(ic  == "no_ic",  "no_ic",  "yes_ic")),
  lvs_qx_recat = factor(ifelse(lvs_qx == "no_lvs", "no_lvs", "yes_lvs")),
  time_of_hs_recat = factor(ifelse(time_of_hs == "elective","elective", "urgent_or_emergent")),
  cardioplegic_sol_recat = factor(case_when(
    cardioplegic_sol == "sanguinea"                        ~ "sanguinea",
    cardioplegic_sol %in% c("nido", "buckberg", "saline")  ~ "saline",
    TRUE                                                   ~ "no_especificada")),
  cannulation_type_art_recat = factor(case_when(
    cannulation_type_art == "aortic"                       ~ "central",
    cannulation_type_art %in% c("axilar", "femoral")       ~ "periferic",
    TRUE                                                   ~ "other")),
  lvad_recat = factor(case_when(
    lvad_pre == "iabp" | lvad_ad == "iabp" ~ "iabp",
    TRUE                                   ~ "no_iabp"), levels = c("no_iabp","iabp"))) -> aaa1


rm(list = c("aaa", "bbb", "r1", "r2", "r3", "r4", "coeficientes","columns_to_convert","pkgs",
            "calcular_mortalidad","convert_columns_to_date","convert_columns_to_numeric","crear_variables_supervivencia_para_todas"))

