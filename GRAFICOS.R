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
  keep = covariables_grafico)
tmat1 #Ver matriz estados y transiciones
nrow(tmat1)#Ver numero estados
paths(tmat1)
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

######Graficos probailidad acumulada modelo nulo desde ingreso uci momento 0#####
cmodel <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans)+cluster(id), data = msebmt1) # Paso 1: Ajustar el modelo de Cox para cada transición
msf <- msfit(object =cmodel,vartype = "aalen", trans = tmat1)# Paso 2: Convertir el modelo en objeto msfit
pt <- probtrans(msf, predt = 0) # Paso 3: Calcular probabilidades de transición desde el tiempo 0


library(RColorBrewer);library(patchwork)
colores <- brewer.pal(n = 5, name = "RdY") 
display.brewer.all(n = NULL, type = "all", select = TRUE,colorblindFriendly = TRUE)
display.brewer.pal(n = 15, name = "Dark2")
brewer.pal(n =11, name = "Paired")

# --- Definir colores globales según estados accesibles desde cada uno ---
colores1 <- c("#a50044", "#004d98", "#edbb00", "#FFFFFF", "#000000")  # Desde UCI
colores2 <- c("#004d98", "#edbb00", "#FFFFFF", "#000000")             # Desde hmd_disf
colores3 <- c("#edbb00", "#FFFFFF", "#000000")                        # Desde organ_disf

# --- Gráfico 1: Desde ingreso en UCI (from = 1) ---
g1 <- plot(pt,
           from = 1,
           ylab = "Riesgo acumulado",
           xlab = "Horas desde ingreso",
           las = 1,
           type = "filled",
           conf.type = "log",
           xlim = c(0, 120),
           use.ggplot = TRUE,
           col = colores1) +
  ggtitle("Desde ingreso en UCI") +
  theme(axis.title = element_text(face = "bold", size = 14),
        axis.text  = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
  scale_x_continuous(limits = c(0, 120),
                     breaks = c(12, 24, 48, 96),
                     labels = c("12h", "24h", "48h", "96h")) +
  geom_vline(xintercept = c(12, 24, 48, 96),
             linetype = "dashed", colour = "black") +
  scale_fill_manual(values = colores1,
                    name   = "Estado",
                    labels = c("UCI", 
                               "Fallo hemodinámico", 
                               "2º fallo orgánico", 
                               "Alta hospitalaria", 
                               "Muerte"))+ 
  guides(fill = guide_legend(title = "Estado"))
g1
# --- Gráfico 2: Desde fallo hemodinámico (from = 2) ---
g2 <- plot(pt,
           from = 2,
           ylab = "Riesgo acumulado",
           xlab = "Horas desde ingreso",
           las = 1,
           type = "filled",
           conf.type = "log",
           xlim = c(0, 120),
           use.ggplot = TRUE,
           col = colores2) +
  ggtitle("Desde fallo hemodinámico") +
  theme(axis.title = element_text(face = "bold", size = 14),
        axis.text  = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
  scale_x_continuous(limits = c(0, 120),
                     breaks = c(12, 24, 48, 96),
                     labels = c("12h", "24h", "48h", "96h")) +
  geom_vline(xintercept = c(12, 24, 48, 96),
             linetype = "dashed", colour = "black")+
  scale_fill_manual(values = colores2,
                    name   = "Estado",
                    labels = c("Fallo hemodinámico", 
                               "2º fallo orgánico", 
                               "Alta hospitalaria", 
                               "Muerte"))+
guides(fill = guide_legend(title = "Estado")) 
g2
# --- Gráfico 3: Desde 2º fallo orgánico (from = 3) ---
g3 <- plot(pt,
           from = 3,
           ylab = "Riesgo acumulado",
           xlab = "Horas desde ingreso",
           las = 1,
           type = "filled",
           conf.type = "log",
           conf.int = 0.95,
           xlim = c(0, 120),
           use.ggplot = TRUE,
           col = colores3) +
  ggtitle("Desde 2º fallo orgánico") +
  theme(axis.title = element_text(face = "bold", size = 14),
        axis.text  = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
  scale_x_continuous(limits = c(0, 120),
                     breaks = c(12, 24, 48, 96),
                     labels = c("12h", "24h", "48h", "96h")) +
  geom_vline(xintercept = c(12, 24, 48, 96),
             linetype = "dashed", colour = "black") +
  scale_fill_manual(values = colores3,
                    name   = "Estado",
                    labels = c("2º fallo orgánico", 
                               "Alta hospitalaria", 
                               "Muerte"))+
  guides(fill = guide_legend(title = "Estado")) 
g3
# --- Combinar los 3 gráficos en una figura con leyenda compartida ---
combo <- (g1 / g2 / g3) + plot_layout(ncol = 1)  # Sin guides = "collect"
final_plot <- combo + 
  plot_annotation(
    title = "Riesgo acumulado de transición entre estados clínicos",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )
final_plot

####ELOS funcion####
# Calcular tiempo esperado de estancia e IC en cada estado desde t=0
elos_ci <- function(pt, tau = NULL, units = "hours") {
  ## --- recorte por horizonte ------------------------------------------
  if (!is.null(tau)) pt <- pt[pt$time <= tau, ] #Permite limitar el análisis a un horizonte temporal (por ejemplo, 720 horas = 30 días).
  time <- pt$time
  dt   <- diff(time)
  ## --- localizar columnas de probabilidades ---------------------------
  p_cols <- grep("^pstate", names(pt), value = TRUE)
  ## --- intentar localizar 'lower' y 'upper'; si no existen, crear metodo del ta y SE-----
  lower_cols <- sub("^pstate", "lower", p_cols)
  upper_cols <- sub("^pstate", "upper", p_cols)
  if (!all(lower_cols %in% names(pt)) || !all(upper_cols %in% names(pt))) {
    ## columnas lower/upper no existen -> construimos con SE
    se_cols <- sub("^pstate", "se", p_cols)
    if (!all(se_cols %in% names(pt)))
      stop("El objeto probtrans no contiene columnas 'se…' para calcular IC")
    z <- qnorm(0.975)                 # 1.96
    p_mat <- as.matrix(pt[ , p_cols])
    se_mat <- as.matrix(pt[ , se_cols])
    lower_mat <- p_mat - z * se_mat
    upper_mat <- p_mat + z * se_mat
    ## limitar al rango [0,1]
    lower_mat[lower_mat < 0] <- 0
    upper_mat[upper_mat > 1] <- 1
  } else {
    ## columnas lower/upper sí existen
    p_mat     <- as.matrix(pt[ , p_cols])
    lower_mat <- as.matrix(pt[ , lower_cols])
    upper_mat <- as.matrix(pt[ , upper_cols])
  }
  ## --- integración por trapecios --------------------------------------
  trap_int <- function(m) {
    mids <- (m[-1, ] + m[-nrow(m), ]) / 2
    colSums(mids * dt)
  }
  elos       <- trap_int(p_mat)
  elos_lower <- trap_int(lower_mat)
  elos_upper <- trap_int(upper_mat)
  ## --- conversión de horas a días si se pide --------------------------Calcula el área bajo la curva de cada probabilidad de estado → esto da el tiempo medio esperado en cada estado.
  scale <- if (units == "days") 1/24 else 1
  data.frame(
    Estado       = sub("^pstate\\.?","", p_cols),
    ELOS         = round(elos       * scale, 2),
    IC95_lo      = round(elos_lower * scale, 2),
    IC95_hi      = round(elos_upper * scale, 2)
  )
}
# hasta 30 días (720 h) y resultados en días
elos_tab <- elos_ci(pt[[1]], tau = 720, units = "hours")
elos_tab; #tiempo medio esperado que un paciente permanece en cada estado clínico del modelo multiestado, 
#junto con sus intervalos de confianza del 95%.


#####ELOS mstate#####
ELOS(pt) #tiempo medio esperado (en horas) que un paciente pasará en el estado destino si comienza en el estado origen
"Valores son promedios ponderados según las probabilidades de transición."
#Filas: representan el estado de destino (donde el paciente permanece).
#Columnas: representan el estado de origen (donde el paciente inicia).
#Celdas: indican el tiempo medio esperado en horas que un paciente pasará en el estado de destino si comienza en el estado de origen.
round(ELOS(pt),3) #Expected Length of Stay in a state 



plot(pt,
     from = 1,
     ylab="Probabilidad cambiar de estado desde UCI",
     xlab="horas",
     las=1,
     type="separate",  
     xlim = c(0, 100),
     use.ggplot = TRUE)+
  ggtitle("``Riesgo acumulado cambio estado desde modelo nulo´´" ) +
  theme(axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 10,face = "bold"),
        plot.title = element_text(hjust = 0.5,face = "bold", size = 16))
plot(pt,
     from = 1,
     to=2,
     ylab="Probabilidad cambiar de estado desde UCI",
     xlab="horas",
     las=1,
     type="single",  
     xlim = c(0, 100),
     use.ggplot = TRUE)+
  ggtitle("``Riesgo acumulado cambio estado desde modelo nulo´´" ) +
  theme(axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 10,face = "bold"),
        plot.title = element_text(hjust = 0.5,face = "bold", size = 16))

#1. Extraer solo esa transición del objeto pt0
# Suponiendo que esta es la transición ICU → hmd_disf
# Verifica con attr(pt0, "trans") para ver los números
# ICU = estado 1, hmd_disf = estado 2
# Entonces la transición deseada es: from = 1, to = 2

######Transicion1 Disfuncion hemodinamica 1->2#####
# Extraer solo la transición 1 -> 2
pt_hmd <- pt[[1]][, c("time", "pstate2", "se2")]  # Probabilidad estado 2 y su error estándar
#2. Graficar esa transición manualmente con ggplot
ggplot(pt_hmd, aes(x = time, y = pstate2)) +
  geom_line(color = "#0072B2", size = 1.2) +
  geom_ribbon(aes(ymin = pstate2 - 1.96 * se2,
                  ymax = pstate2 + 1.96 * se2),
              fill = "#0072B2", alpha = 0.2) +
  labs(
    title = "Riesgo acumulado: ICU → Disfunción hemodinámica",
    x = "Horas desde ingreso en UCI",
    y = "Probabilidad acumulada"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  ) +
  coord_cartesian(xlim = c(0, 96)) +  # Limitar eje 
  scale_x_continuous(breaks = seq(0, 96, by = 12))  # Marcas cada 12 horas
plot(pt ,use.ggplot = TRUE, 
     type = "single",
     conf.int = 0.95, # 95% level
     conf.type = "log")   

## 1. Filtrar la primera transición (UCI -> hmd_disf)
hmd <- msebmt1 %>%  filter(trans == 1,status == 1)# el evento ocurrió
## 2a. Resumen rápido
hmd %>% summarise(
  pacientes = n(),
  mediana_h = median(Tstop),
  IQR_h     = IQR(Tstop),
  n_2h_or_more  = sum(Tstop >= 2),
  pct_2h_or_more = mean(Tstop >= 2) * 100,
  n_4h_or_more  = sum(Tstop >= 4),
  pct_4h_or_more = mean(Tstop >= 4) * 100,
  n_6h_or_more  = sum(Tstop >= 6),
  pct_6h_or_more = mean(Tstop >= 6) * 100,
  n_12h_or_more = sum(Tstop >= 12),
  pct_12h_or_more = mean(Tstop >= 12) * 100,
  n_24h_or_more = sum(Tstop >= 24),
  pct_24h_or_more = mean(Tstop >= 24) * 100)

hmd %>% summarise(
  pacientes = n(),
  mediana_h = median(Tstop),
  IQR_h     = IQR(Tstop),
  n_less_2h  = sum(Tstop < 2),
  pct_less_2h = mean(Tstop < 2) * 100,
  n_less_4h  = sum(Tstop < 4),
  pct_less_4h = mean(Tstop < 4) * 100,
  n_less_6h  = sum(Tstop < 6),
  pct_less_6h = mean(Tstop < 6) * 100,
  n_less_12h = sum(Tstop < 12),
  pct_less_12h = mean(Tstop < 12) * 100,
  n_less_24h = sum(Tstop < 24),
  pct_less_24h = mean(Tstop < 24) * 100)
#pct_2h = 33% significa que el 33% de pacientes desarrollaron disfunción a las 2 horas o después.
# Calcular estadísticos básicos
q1 <- quantile(hmd$Tstop, 0.25, na.rm = TRUE)
q3 <- quantile(hmd$Tstop, 0.75, na.rm = TRUE)
med <- median(hmd$Tstop, na.rm = TRUE)
cut_pts <- c(2, 4, 6, 12, 24) # Puntos de corte (puedes ajustar según tus necesidades)
labels_tbl <- data.frame(mid = cut_pts,lab = paste0("<", cut_pts, "h")) # Etiquetas para los puntos de corte
dens <- density(hmd$Tstop, na.rm = TRUE) # Densidad suavizada
density_tbl <- data.frame( mid = dens$x, count = dens$y * length(hmd$Tstop) * 2)  # Ajuste por binwidth = 2

# 1. Filtrar la primera transición
ggplot(hmd, aes(x = Tstop, fill = hmd_disf_elegida)) +
  geom_rect(
    xmin = q1, xmax = q3,
    ymin = 0,  ymax = 120,
    fill = alpha("#FEF3E2", 0.08),
    colour = NA,
    inherit.aes = FALSE) +
  geom_histogram(
    binwidth = 2, boundary = 0, closed = "left",
    colour = "white") +
  geom_vline(xintercept = cut_pts,
             linetype = "dashed", colour = "grey40") +
  geom_text(data = labels_tbl,
            aes(x = mid, y = 85, label = lab),
            inherit.aes = FALSE,
            vjust = 0, size = 4.2, fontface = "bold") +
  geom_vline(xintercept = med, colour = "black", linewidth = 1) +
  geom_vline(xintercept = c(q1, q3), colour = "red", linetype = "dotted") +
  annotate("label",
           x = med, y = 95,
           label = sprintf("Mediana = %.2f h\nIQR = %.2f – %.2f h", med, q1, q3),
           fill = alpha("#FEF3E2", 0.08),
           colour = "black",
           fontface = "bold",
           size = 4,
           label.size = 0.25,
           hjust = -0.25) +
  geom_line(data = density_tbl,
            aes(x = mid, y = count),
            inherit.aes = FALSE,
            colour = "darkred",
            linewidth = 1.4) +
  geom_point(data = density_tbl,
             aes(x = mid, y = count),
             colour = "darkred",
             size = 2.5,
             inherit.aes = FALSE) +
  scale_x_continuous(breaks = seq(0, 48, 2), limits = c(0, 48), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Tiempo hasta disfunción hemodinámica primeras 48h",
    x     = "Horas desde ingreso en UCI",
    y     = "Número de pacientes",
    fill  = "Tipo de disfunción") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.85, 0.50),  # posición dentro del gráfico (x,y)
    legend.background = element_rect(fill = alpha("white", 0.7), colour = NA),
    legend.title = element_text(face = "bold"))


# ─────────────────────────────────────────────────────────────
# 1. Parámetros y rescalado
# ─────────────────────────────────────────────────────────────
binwidth        <- 2                                   # ancho del histograma
total_pacientes <- nrow(hmd[hmd$Tstop <= 48, ])        # N pacientes ≤ 48 h

# Curva de riesgo (a nº pacientes)
pt_hmd_esc <- subset(pt_hmd, time <= 48)
pt_hmd_esc$pstate2_resc <- pt_hmd_esc$pstate2 * total_pacientes
pt_hmd_esc$se2_resc     <- pt_hmd_esc$se2     * total_pacientes

# Datos filtrados a 48 h
hmd_48        <- subset(hmd,        Tstop <= 48)
labels_tbl_48 <- subset(labels_tbl, mid   <= 48)

# Densidad kernel → conteos por bin de 2 h
dens_k <- density(hmd_48$Tstop, from = 0, to = 48, bw = binwidth)
dens_df <- data.frame(
  mid   = dens_k$x,
  count = dens_k$y * total_pacientes * binwidth)

# Altura máxima para escalar anotaciones
max_y <- max(
  hist(hmd_48$Tstop, plot = FALSE, breaks = seq(0, 48, by = binwidth))$counts,
  dens_df$count,
  pt_hmd_esc$pstate2_resc + 1.96 * pt_hmd_esc$se2_resc) * 1.10   # 10 % extra arriba

# Paleta pastel para los rellenos del histograma
cols_fill <- c("eb5_tiempo_hasta"     = "#FDBCB4",
               "lac3mmol_tiempo_hasta"= "#B8E3B3",
               "na0.2_tiempo_hasta"   = "#AFCFFA")
# ─────────────────────────────────────────────────────────────
# 2. Gráfico
# ─────────────────────────────────────────────────────────────
ggplot() +
  # Sombreado IQR de toda la distribución
  geom_rect(aes(xmin = q1, xmax = q3, ymin = 0, ymax = max_y),
            fill = alpha("#FEF3E2", .15)) +
  # Histograma (conteo)
  geom_histogram(
    data = hmd_48,
    aes(x = Tstop,
        y = after_stat(count),
        fill = hmd_disf_elegida),
    binwidth = binwidth, boundary = 0, closed = "left",
    colour = "white") +
  # Densidad suavizada (línea + puntos)
  geom_line(data = dens_df, aes(x = mid, y = count),
            colour = "#C93312", linewidth = 0.8) +
  geom_point(data = dens_df, aes(x = mid, y = count),
             colour = "#C93312", size = 0.8) +
  # Curva de riesgo acumulado (nº pacientes) + IC 95 %
  geom_ribbon(
    data = pt_hmd_esc,
    aes(x = time,
        ymin = pstate2_resc - 1.96 * se2_resc,
        ymax = pstate2_resc + 1.96 * se2_resc),
    fill = "#045A8D", alpha = 0.20) +
  geom_line(
    data = pt_hmd_esc,
    aes(x = time, y = pstate2_resc),
    colour = "#045A8D", linewidth = 1.2) +
  # Líneas verticales: cortes, mediana, IQR
  geom_vline(xintercept = cut_pts, linetype = "dashed", colour = "grey40") +
  geom_vline(xintercept = med,        colour = "black", linewidth = 1) +
  geom_vline(xintercept = c(q1, q3),  colour = "#E41A1C", linetype = "dotted") +
  # Etiqueta Mediana + IQR
  annotate("label",
           x = med, y = max_y*0.95,
           label = sprintf("Mediana = %.2f h\nIQR = %.2f – %.2f h", med, q1, q3),
           fill = alpha("#FEF3E2", .8), colour = "black",
           fontface = "bold", label.size = 0.10, hjust = 0) +
  
  # Etiquetas de cortes si las tienes
  geom_text(data = labels_tbl_48,
            aes(x = mid, y = max_y, label = lab),
            size = 4, fontface = "bold") +
  # ── Leyenda “manual” dentro del gráfico ──
  annotate("segment", x = 39, xend = 40, y = max_y*1.02, yend = max_y*1.02,
           colour = "#C93312", size = 1.2) +
  annotate("text", x = 41, y = max_y*1.02, hjust = 0,
           label = "Densidad suavizada", colour = "#C93312",
           fontface = "bold", size = 3.8) +
  annotate("segment", x = 39, xend = 40, y = max_y*0.97, yend = max_y*0.97,
           colour = "#045A8D", size = 1.2) +
  annotate("text", x = 41, y = max_y*0.97, hjust = 0,
           label = "Riesgo acumulado ± IC 95 %", colour = "#045A8D",
           fontface = "bold", size = 3.8) +
  
  # Escalas
  scale_x_continuous(breaks = seq(0, 48, 4), limits = c(0, 48)) +
  scale_y_continuous(
    name     = "Número de pacientes",
    limits   = c(0, max_y*1.08),
    sec.axis = sec_axis(~ . / total_pacientes,
                        name = "Probabilidad acumulada")) +
  scale_fill_manual(values = cols_fill, name = "Tipo de disfunción") +
  
  # Tema
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(colour = "grey90"),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(size = 15, face = "bold", hjust = .5),
    axis.title.y.right = element_text(colour = "#045A8D", face = "bold"),
    axis.title.y.left  = element_text(face = "bold"),
    legend.position    = c(.85, .60),
    legend.title       = element_text(face = "bold"),
    legend.background  = element_rect(fill = alpha("white", .85), colour = NA)) +
  labs(
    title = "Tiempo hasta disfunción hemodinámica y riesgo acumulado",
    x     = "Horas desde ingreso en UCI")

---------------------------------------------
  binwidth        <- 2                                   # ancho del histograma
total_pacientes <- nrow(hmd[hmd$Tstop <= 24, ])        # N pacientes ≤ 24 h

# Curva de riesgo (a nº pacientes)
pt_hmd_esc <- subset(pt_hmd, time <= 24)
pt_hmd_esc$pstate2_resc <- pt_hmd_esc$pstate2 * total_pacientes
pt_hmd_esc$se2_resc     <- pt_hmd_esc$se2     * total_pacientes

# Datos filtrados a 24 h
hmd_24        <- subset(hmd,        Tstop <= 24)
labels_tbl_24 <- subset(labels_tbl, mid   <= 24)

# Densidad kernel → conteos por bin de 2 h
dens_k <- density(hmd_24$Tstop, from = 0, to = 24, bw = binwidth)
dens_df <- data.frame(
  mid   = dens_k$x,
  count = dens_k$y * total_pacientes * binwidth)

# Altura máxima para escalar anotaciones
max_y <- max(
  hist(hmd_24$Tstop, plot = FALSE, breaks = seq(0, 24, by = binwidth))$counts,
  dens_df$count,
  pt_hmd_esc$pstate2_resc + 1.96 * pt_hmd_esc$se2_resc) * 1.10   # 10 % extra arriba

# Paleta pastel para los rellenos del histograma
cols_fill <- c("eb5_tiempo_hasta"     = "#FDBCB4",
               "lac3mmol_tiempo_hasta"= "#B8E3B3",
               "na0.2_tiempo_hasta"   = "#AFCFFA")
# ─────────────────────────────────────────────────────────────
# 2. Gráfico para primeras 24 horas
# ─────────────────────────────────────────────────────────────
ggplot() +
  # Sombreado IQR de toda la distribución
  geom_rect(aes(xmin = q1, xmax = q3, ymin = 0, ymax = max_y),
            fill = alpha("#FEF3E2", .15)) +
  # Histograma (conteo)
  geom_histogram(
    data = hmd_24,
    aes(x = Tstop,
        y = after_stat(count),
        fill = hmd_disf_elegida),
    binwidth = binwidth, boundary = 0, closed = "left",
    colour = "white") +
  # Densidad suavizada (línea + puntos)
  geom_line(data = dens_df, aes(x = mid, y = count),
            colour = "#C93312", linewidth = 0.8) +
  geom_point(data = dens_df, aes(x = mid, y = count),
             colour = "#C93312", size = 0.8) +
  # Curva de riesgo acumulado (nº pacientes) + IC 95 %
  geom_ribbon(
    data = pt_hmd_esc,
    aes(x = time,
        ymin = pstate2_resc - 1.96 * se2_resc,
        ymax = pstate2_resc + 1.96 * se2_resc),
    fill = "#045A8D", alpha = 0.20) +
  geom_line(
    data = pt_hmd_esc,
    aes(x = time, y = pstate2_resc),
    colour = "#045A8D", linewidth = 1.2) +
  # Líneas verticales: cortes, mediana, IQR
  geom_vline(xintercept = cut_pts, linetype = "dashed", colour = "grey40") +
  geom_vline(xintercept = med,        colour = "black", linewidth = 1) +
  geom_vline(xintercept = c(q1, q3),  colour = "#E41A1C", linetype = "dotted") +
  # Etiqueta Mediana + IQR
  annotate("label",
           x = med, y = max_y*0.95,
           label = sprintf("Mediana = %.2f h\nIQR = %.2f – %.2f h", med, q1, q3),
           fill = alpha("#FEF3E2", .8), colour = "black",
           fontface = "bold", label.size = 0.10, hjust = 0) +
  # Etiquetas de cortes
  geom_text(data = labels_tbl_24,
            aes(x = mid, y = max_y, label = lab),
            size = 4, fontface = "bold") +
  # Leyenda interna
  annotate("segment", x = 18, xend = 19, y = max_y*1.02, yend = max_y*1.02,
           colour = "#C93312", size = 1.2) +
  annotate("text", x = 20, y = max_y*1.02, hjust = 0,
           label = "Densidad suavizada", colour = "#C93312",
           fontface = "bold", size = 3.8) +
  annotate("segment", x = 18, xend = 19, y = max_y*0.97, yend = max_y*0.97,
           colour = "#045A8D", size = 1.2) +
  annotate("text", x = 20, y = max_y*0.97, hjust = 0,
           label = "Riesgo acumulado ± IC 95 %", colour = "#045A8D",
           fontface = "bold", size = 3.8) +
  # Escalas
  scale_x_continuous(breaks = seq(0, 24, 4), limits = c(0, 24)) +
  scale_y_continuous(
    name     = "Número de pacientes",
    limits   = c(0, max_y*1.08),
    sec.axis = sec_axis(~ . / total_pacientes,
                        name = "Probabilidad acumulada")) +
  scale_fill_manual(values = cols_fill, name = "Tipo de disfunción") +
  # Tema
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(colour = "grey90"),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(size = 15, face = "bold", hjust = .5),
    axis.title.y.right = element_text(colour = "#045A8D", face = "bold"),
    axis.title.y.left  = element_text(face = "bold"),
    legend.position    = c(.85, .60),
    legend.title       = element_text(face = "bold"),
    legend.background  = element_rect(fill = alpha("white", .85), colour = NA)) +
  labs(
    title = "Tiempo hasta disfunción hemodinámica y riesgo acumulado",
    x     = "Horas desde ingreso en UCI")
-----------------------------------------------------
  
#####Prediccion####
 

newd<- data.frame(trans=c(1,2),
                  ao_cross_clamp_time.1 = c(100,120),  # Tiempo de pinzamiento aórtico
                  age.1 = c(65,70) ,             # Edad del paciente
                  vis_ad.1 =c(0,5),             # Uso de vasopresores en el tiempo 1
                  is_ad.1 =c(0.7,0.8),                  # Soporte inotrópico en el tiempo 1
                  lvef_change_pre_ad.2 = c(-0.2,-0.1),  # Cambio en fracción de eyección ventricular izquierda en el tiempo 2
                  pctspcts.3 =c(1,0),               # Procedimiento en el tiempo 3
                  hypothermia.3=c(30,28),            # Hipotermia en el tiempo 3
                  pvc_ad.3 = c(5,7),                 # PVC en el tiempo 3
                  pam_ad.3 = c(85,75),                # PAM en el tiempo 3
                  time_of_hs_recaturgent_or_emergent.3 = c(0,1), # Urgencia en el tiempo 3
                  vis_ad.4 = c(10, 8),                # Uso de vasopresores en el tiempo 4
                  is_ad.4 = c(1,1.2),                  # Soporte inotrópico en el tiempo 4
                  hematíes_post.4 = c(1,1.5),        # Recuento de hematíes en el tiempo 4
                  cbp_time.5 = c(90,95),              # Tiempo de circulación extracorpórea en el tiempo 5
                  lvad_adiabp.5= c(0,0),            # Uso de asistencia ventricular izquierda en el tiempo 5
                  vis_pre.5 = c(0,0),                # Uso de vasopresores previo al tiempo 5
                  vis_ad.5 = c(1,1),                 # Uso de vasopresores en el tiempo 5
                  pctspcts.6 = c(0,0),               # Procedimiento en el tiempo 6
                  cbp_time.6 = c(75,70),              # Tiempo de circulación extracorpórea en el tiempo 6
                  pm_adpm_dependent.6 = c(0,1),      # Dependencia de marcapasos en el tiempo 6
                  hs_duration.6 = c(150,200),           # Duración de la intervención quirúrgica en el tiempo 6
                  pam_ad.6 = c(70,90),
                  strata=c(1,1))

attr(newd,"trans")<-tmat1 
class(newd)<-c("msdata","data.frame") 
msf_final <- msfit(object = modelo_final, 
                   vartype = "aalen", 
                   trans = tmat1, 
                   newdata = newd)
plot(msf_final, from=2, type="filled",
     #desde disfuncion hemodinamica
     ylab="``Cumulative baseline hazards from null model´´",
     xlab="Días (desde las 12h ingreso hasta el dia 30)",
     las=1,
     type="filled",
     xlim = c(0, 30),
     use.ggplot = TRUE)+
  ggtitle("Predicción de la probabilidad de cambiar de estado desde disfunción hemodinamica" ) +
  theme(axis.title = element_text(face = "bold", size = 14),
        plot.title = element_text(hjust = 0.5,face = "bold", size = 14))

