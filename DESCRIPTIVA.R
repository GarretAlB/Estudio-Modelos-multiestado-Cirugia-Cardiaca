
############# ESTADISTICA DESCRIPTIVA ################
#Función normalidad
plot_distribution <- function(x, variable_name = "variable estudiada") {
  # Convertir x a data frame para ggplot
  data <- data.frame(x = x)
  # Realizar el test de Shapiro-Wilk para normalidad
  shapiro_result <- shapiro.test(x)
  # Histograma
  hist_plot <- ggplot(data, aes(x = x)) +
    geom_histogram(binwidth = diff(range(x)) / 30, fill = "skyblue", color = "black") +
    labs(title = paste("Histograma de", variable_name), x = variable_name, y = "Frecuencia")
  # QQ-plot
  qq_plot <- ggplot(data, aes(sample = x)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = paste("QQ-plot de", variable_name), x = "Cuantiles teóricos", y = "Cuantiles de la muestra")
  # Boxplot
  box_plot <- ggplot(data, aes(y = x)) +
    geom_boxplot(fill = "lightgreen") +
    labs(title = paste("Boxplot de", variable_name), y = variable_name)
  # Densidad
  density_plot <- ggplot(data, aes(x = x)) +
    geom_density(fill = "lightcoral") +
    labs(title = paste("Gráfico de densidad de", variable_name), x = variable_name, y = "Densidad")
  # Combinar los gráficos en una sola visualización
  combined_plot <- grid.arrange(hist_plot, qq_plot, box_plot, density_plot, ncol = 2)
  # Mostrar el resultado del test de normalidad
  print(shapiro_result)
  # Devolver los gráficos combinados
  return(combined_plot)
}
#plot_distribution(aaa1$charlson)

#Descriptivas cualitativas
calculate_percentages <- function(x) {round(prop.table(table(x)), 2)}
aaa1%>%select_if(is.factor) %>%select(-nombre_apellidos) %>% map(calculate_percentages)
theme_gtsummary_journal(journal = "jama")

#Descriptivas cuantitativas
aaa1 %>%select_if(is.numeric) %>% {list(summary = summary(.), skim = skim(.))} %>% view()

#Comorbilidades basales cuant + cual
aaa1 %>%
  select(sex, age, weight, size, imc, hta, dl, smoker, dm, ic, pcts, pvd, af, hf, nyha, estadio_erc, copd, osas, antithrombotic, charlson, cfs) %>%
  rename_with(toupper) %>%
  tbl_summary(statistic = list(all_categorical() ~ "{n} ({p}%)")) %>%
  modify_header(label ~ "**Variables**") %>%
  modify_footnote(
    all_stat_cols() ~ "median (IQR) or n/frequency(%)"
  ) %>%
  modify_caption("**Table 1. Comorbilidad basal**") %>%
  bold_labels()

#Grafico nyhas
nyha_percent <- aaa1 %>%
  group_by(nyha) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Crear el gráfico de barras
ggplot(nyha_percent, aes(x = nyha, y = percentage, fill = "red")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5, size = 4) +
  labs(title = "Clase funcional pacientes según NYHA", x = "NYHA", y = "Porcentaje") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    legend.position = "none")


#Situación quirurgica             
aaa1 %>% select(mortalidad_predicha, weight_of_procedure, time_of_hs, endocarditis,hs_duration, 
               hs_type_cabg, hs_type_valvular,hs_type_ta,hs_type_avr,hs_type_mvr,cannulation_type_art, 
               cannulation_type_v,cardioplegic_adm, cardioplegic_sol,hypothermia,cbp_time, ao_cross_clamp_time) %>% 
  rename_with(toupper) %>%
  tbl_summary(statistic = list(all_categorical() ~ "{n} ({p}%)")) %>%
  modify_header(label ~ "**Variables**") %>%
  modify_footnote(all_stat_cols() ~ "median (IQR) or n/frequency(%)") %>%
  modify_caption("**Table 2. Tipos de cirugia operadas y estrategias ECLS**") %>%
  bold_labels()

#Grafico peso procedimientos  
weight_of_procedure_percent <- aaa1 %>%
  group_by(weight_of_procedure) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>% 
  arrange(desc(count)) 
weight_of_procedure_percent$weight_of_procedure <- factor(weight_of_procedure_percent$weight_of_procedure, 
                                                          levels = weight_of_procedure_percent$weight_of_procedure)

# Crear el gráfico de barras con bordes y detalles estéticos
ggplot(weight_of_procedure_percent, aes(x = weight_of_procedure, y = percentage)) +
  geom_bar(stat = "identity", fill = "#4DBBD5B2", color = "black", width = 0.7) + # Color de relleno y borde negro
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5, size = 4, color = "black") + # Texto negro
  labs(title = "Dificultad procedimiento quirúrgico", 
       x = "Tipo de intervención", y = "Porcentaje") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    legend.position = "none")

#Situacion anestesica y hemodinamica
aaa1 %>% select(hr_pre,pas_pre,pad_pre,pam_pre,pvc_pre,it_max,is_pre,lvef_pre,rvef_pre,na_dose_pre,dbt_dose_pre,vsp_dose_pre,
                mlr_dose_pre,vis_pre,lvad_pre) %>% 
  rename_with(toupper) %>%
  tbl_summary(statistic = list(all_categorical() ~ "{n} ({p}%)"),missing = "no") %>%
  modify_header(label ~ "**Variables**") %>%
  modify_footnote(all_stat_cols() ~ "median (IQR) or n/frequency(%)") %>%
  modify_caption("**Table 3. Situación hemodinamica preoperatoria**") %>%
  bold_labels()

aaa1 %>% select(hr_ad,pm_ad,pas_ad, pad_ad, pam_ad, pvc_ad, is_ad,lvef_ad, lvef_change_pre_ad,rvef_ad,rvef_change_pre_ad,na_dose_ad, dbt_dose_ad, 
                vis_ad,lvs_qx, lvad_ad,vf_post,hematíes_post,plaquetas_post,pfc_post) %>% 
  rename_with(toupper) %>%
  tbl_summary(statistic = list(all_categorical() ~ "{n} ({p}%)"),missing = "no") %>%
  modify_header(label ~ "**Variables**") %>%
  modify_footnote(all_stat_cols() ~ "median (IQR) or n/frequency(%)") %>%
  modify_caption("***Table 4. Situación hemodinamica postperatoria**") %>%
  bold_labels()

# Crear el gráfico de densidades por cuartiles
library("ggridges")
aaa1_long <- data.frame(
  value = c(aaa1$lvef_pre, aaa1$lvef_ad),
  variable = rep(c("LVEF Pre", "LVEF Ad"), each = nrow(aaa1))
)
ggplot(aaa1_long, aes(x = value, y = variable, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Cuartiles") +
  labs(title = "Distribución de LVEF Pre y LVEF Ad",
       x = "LVEF (%)", y = "Condición") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    legend.position = "right")

install.packages("ggstatsplot")
library(ggstatsplot)
ggwithinstats(
  data    = aaa1,
  x       = vis_pre,
  y       = vis_ad,
  title   = "Diferencia prepost")

aaa1$Grupo <- factor(rep(c("Antes", "Después"), each = nrow(aaa1)/2))
df <- data.frame(
  lvef = c(aaa1$lvef_pre, aaa1$lvef_ad),
  grupo = rep(aaa1$Grupo, times = 2))# Unión de las columnas lvef_pre y lvef_ad
ggwithinstats(df, x="grupo",y="lvef")

aaa1$Grupo <- factor(rep(c("Antes", "Después"), each = nrow(aaa1)/2))
df <- data.frame(
  na = c(aaa1$na_dose_pre, aaa1$na_dose_ad),
  grupo = rep(aaa1$Grupo, times = 2))# Unión de las columnas vis_pre y vis_ad
ggwithinstats(df, x="grupo",
              y="na", 
              type = "parametric", 
              pairwise.display="all")

############# ESTADISTICA INFERENCIAL ################
#t.test(lvef_change_ad_icu~ cabg, data = aaa, var.equal = FALSE)
#t.test(aaa$lvef_change_pre_icu ~ factor(aaa$hs_type_mvr, levels = unique(aaa$hs_type_mvr)), var.equal = FALSE, na.action = na.omit) #no significativo
#t.test(aaa$lvef_change_pre_ad ~ factor(aaa$hs_type_mvr, levels = unique(aaa$hs_type_mvr)), var.equal = FALSE, na.action = na.omit) #no significativo
#t.test(aaa$lvef_change_ad_icu ~ factor(aaa$hs_type_mvr, levels = unique(aaa$hs_type_mvr)), var.equal = FALSE, na.action = na.omit) #no significativo

############# ANALISIS SUPERVIVENCIA ################
#aaa1 %>% filter(plaq150000_tiempo_hasta<0) %>% select(nhc)

#Endocarditis
survfit(Surv(date_death_tiempo_hasta/24,date_death_censura_tiempo_hasta)~1,data=aaa1) %>% summary()
survdiff(Surv(date_death_tiempo_hasta/24,date_death_censura_tiempo_hasta)~endocarditis,data =aaa1)
survfit(Surv(date_death_tiempo_hasta/24,date_death_censura_tiempo_hasta)~aaa1$hs_type_ta,data =aaa1)->surv_endocar;
summary(surv_endocar)
ggsurvplot(
  surv_endocar,# survfit object with calculated statistics.
  data = aaa1,             # data used to fit survival curves.
  risk.table = TRUE,       # show risk table.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimates of survival curves.
  palette = c("#9999CC", "#66CC99"),
  xlim = c(0,125),         # present narrower X axis, but not affect survival estimates.
  xlab = "Time in days",   # customize X axis label.
  break.time.by = 25,     # break X axis in time intervals by .
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.height = 0.25, # the height of the risk table
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = TRUE,      # plot the number of censored subjects at time t
  ncensor.plot.height = 0.25,  
  surv.median.line = "hv",  # add the median survival pointer.
  legend.labs = 
    c("Endocarditis", "No endocarditis"))

survfit(Surv(date_reintervention_tiempo_hasta/24, aaa1$date_reintervention_censura_tiempo_hasta)~1,data=aaa1) %>% summary()
survdiff(Surv(date_reintervention_tiempo_hasta/24,date_death_censura_tiempo_hasta)~endocarditis,data =aaa1)
survfit(Surv(date_reintervention_tiempo_hasta/24,date_reintervention_censura_tiempo_hasta)~aaa1$hs_type_valvular,data =aaa1)->surv_endocar;
summary(surv_endocar)
ggsurvplot(
  surv_endocar,# survfit object with calculated statistics.
  data = aaa1,             # data used to fit survival curves.
  risk.table = TRUE,       # show risk table.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimates of survival curves.
  palette = c("#9999CC", "#66CC99"),
  xlim = c(0,125),         # present narrower X axis, but not affect survival estimates.
  xlab = "Time in days",   # customize X axis label.
  break.time.by = 25,     # break X axis in time intervals by .
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.height = 0.25, # the height of the risk table
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = TRUE,      # plot the number of censored subjects at time t
  ncensor.plot.height = 0.25,  
  surv.median.line = "hv",  # add the median survival pointer.
  legend.labs = 
    c("No valvular", "Valvular"))


#DATOS BIEN RECOGIDOS EN ESCALA DE TIEMPO?
aaa1 %>% select(patient_number,nhc,date_death,date_death_tiempo_hasta,date_death_censura_tiempo_hasta) %>% drop_na()
aaa1 %>% select(patient_number,nhc,na0.1,na0.1_tiempo_hasta) %>% drop_na() %>% filter(na0.1_tiempo_hasta<0)
aaa1 %>% select(patient_number,nhc,date_reintervention_tiempo_hasta,date_reintervention) %>% drop_na() %>% filter(date_reintervention_tiempo_hasta<0)
aaa1 %>% select(patient_number,nhc,date_icu_discharge_tiempo_hasta,date_icu_discharge) %>% drop_na() %>% filter(date_icu_discharge_tiempo_hasta<0)
aaa1$date_icu_discharge_tiempo_hasta
aaa1$date_icu_discharge_tiempo_hasta
aaa1$date_icu_ad


#Porcentaje outcomes/censuras
aaa1 %>% select(contains("censura")) %>%select(-start_hs_censura_tiempo_hasta, -end_hs_censura_tiempo_hasta,-date_troponine_max_censura_tiempo_hasta,
                                               -date_vasoactive_end_censura_tiempo_hasta, -date_inotropic_end_censura_tiempo_hasta,-date_lactate_max_censura_tiempo_hasta,
                                               -date_be_max_censura_tiempo_hasta,-date_hco3_max_censura_tiempo_hasta,-date_cr_max_censura_tiempo_hasta,
                                               date_drainage_censura_tiempo_hasta,-date_hospital_discharge_censura_tiempo_hasta) %>% rename_with(toupper) %>% 
tbl_summary(statistic = list(all_categorical() ~ "{n} ({p}%)")) %>%
modify_header(label ~ "**Variables censuradas**") %>%
modify_footnote(all_stat_cols() ~ "n/frequency(%)") %>%
modify_caption("**Table 4. Eventos**") %>%
bold_labels()

#Extra
aaa1 %>% select(date_icu_discharge_censura_tiempo_hasta:date_death_tiempo_hasta)%>% select(contains("censura")) %>% summarise(across(everything(), ~ mean(.)))

#Endpoints Descriptivos
table(aaa1$na0.5_censura_tiempo_hasta)
prop.table(table(aaa1$na0.5_censura_tiempo_hasta))
table(aaa1$date_rrt_censura_tiempo_hasta)
prop.table(table(aaa1$date_rrt_censura_tiempo_hasta))
table(aaa1$date_traqueostomy_censura_tiempo_hasta)
prop.table(table(aaa1$date_traqueostomy_censura_tiempo_hasta))
sum(table(aaa1$reingreso))/nrow(aaa1) #reingresos
aaa1 %>% filter(date_extubation_censura_tiempo_hasta==1) %>% skim(date_extubation_tiempo_hasta) #mediana hasta extubación
aaa1 %>% filter(date_death_censura_tiempo_hasta==0) %>%  skim(date_icu_discharge_tiempo_hasta) #estancia mediana
aaa1 %>% filter(date_death_censura_tiempo_hasta==0) %>%  skim(date_hospital_discharge_tiempo_hasta)








