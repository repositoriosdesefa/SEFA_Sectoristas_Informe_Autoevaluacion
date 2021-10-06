####################################################################-
##########  Consultas SQL a bases de Oracle a través de R  #########-
############################# By LE ################################-

################ I. Librerías, drivers y directorio ################
# I.1 Librerías ----
#install.packages("dplyr")
library(dplyr)
#install.packages("readxl")
library(readxl)
#install.packages("rmarkdown")
library(rmarkdown)
#install.packages("purrr")
library(purrr)
#install.packages("lubridate")
library(lubridate)
#install.packages("stringi")
library(stringi)

# I.2 Parámetros ----
# i) Conexión de la base
FUENTE_AUTO_ODES <- ""
tp1 <- tempfile() # Creación de un archivo temporal

# ii) Definición de parámetros
# Periodo
f_fecha <- function(x) {
  as.Date(format(x, "%Y-%m-01"))
}

periodo_mes <- Sys.Date() %m-% months(3)
F_INICIO <- as.Date(f_fecha(periodo_mes))
F_FIN <- as.Date(F_INICIO %m+% months(1) - 1)
MES <- month(F_INICIO, label=TRUE, abbr = FALSE)
PERIODO <- year(F_INICIO) 

# ODES de análisis
download.file(FUENTE_AUTO_ODES, tp1, mode ="wb")
COMPILADO <- as.data.frame(read_xlsx(tp1, sheet = "CONSOLIDADO-TOTAL"))

PARAMETROS <- COMPILADO %>%
  mutate(FECHA_FIN_SISEFA = as.Date(`FECHA DE ESTADO TERMINADO EN SISEFA`)) %>%
  filter(FECHA_FIN_SISEFA >= F_INICIO,
         FECHA_FIN_SISEFA <= F_FIN) %>%
  select(OD) %>%
  summarise(ODE_NOM=unique(OD)) %>%
  arrange(ODE_NOM)

# Directorio
dir <- ""
reportes_dir <- file.path(dir, "RM")

########################### II. Funciones ###########################
# II.1 Función de renderizado de documento ----
auto_lec_rep <- function(nombre){
  
  # Se eliminan carácteres especiales
  nombre_n = gsub("Ñ", "N", nombre)
  
  # Eliminación de tildes
  con_tilde_may <- c("Á", "É", "Í", "Ó", "Ú")
  sin_tilde_may <- c("A", "E", "I", "O", "U")
  con_tilde_min <- c("á", "é", "í", "ó", "ú")
  sin_tilde_min <- c("a", "e", "i", "o", "u")
  
  nombre_n = stri_replace_all_regex(nombre_n, con_tilde_may, sin_tilde_may, vectorize = F)
  nombre_n = stri_replace_all_regex(nombre_n, con_tilde_min, sin_tilde_min, vectorize = F)
  
  
  rmarkdown::render(input = file.path(reportes_dir, "Reporte_Mensual.Rmd"),
                    params = list(ODE_NOM = nombre),
                    output_file = paste0("Reporte Mensual - ",
                                         nombre_n,
                                         " - ",
                                         MES,
                                         " de ",
                                         PERIODO))
}

# II.2 Función robustecida ----
R_auto_lec_rep <- function(nombre){
  out = tryCatch(auto_lec_rep(nombre),
                 error = function(e){
                   auto_lec_rep(nombre) 
                 })
  return(out)
}


########################### III. Renderizado ###########################
pwalk(list(PARAMETROS$ODE_NOM),
      slowly(R_auto_lec_rep, 
             rate_backoff(10, max_times = Inf)))
