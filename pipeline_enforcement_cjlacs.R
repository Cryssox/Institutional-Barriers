################################################################################
#                                                                              #
#  PIPELINE UNICO - VERSION CJLACS (v3.5, 6 de noviembre de 2026)              #
#  v3.5: anios de E-Verify tomados de las leyes, con una regla explicita       #
#  (EV_RULE) [E15]; lectura del Excel de licencias aunque los datos no         #
#  empiecen en la columna A, y parada si un componente del indice queda        #
#  vacio [E16]. CAMBIAN las cifras.                                            #
#  v3.4: Utah [E14].                                                           #
#  v3.3: autoempleo y empleo asalariado solo si la persona estaba ocupada en   #
#  la semana de referencia [E13]. CAMBIAN las cifras.                          #
#  v3.2: indice sin la tasa total de deportaciones ICE y con Secure            #
#  Communities por no ciudadano [E11]; Colorado explicito [E12]. CAMBIAN las   #
#  cifras: hay que volver a correrlo y actualizar el texto.                    #
#  v3.1: figuras con fondo blanco, Figura 4 con un eje por panel y Figura A2   #
#  sin intervalos de Sun-Abraham.                                              #
#                                                                              #
#  "Immigration Enforcement, Institutional Barriers, and Latinx                #
#   Self-Employment in the U.S.: The Cost of Ethnic Identifiability"           #
#                                                                              #
#  Ejecuta la metodologia del manuscrito de principio a fin, desde los CSV     #
#  de IPUMS-CPS y los Excel de politicas hasta las cifras, tablas y figuras,   #
#  para HOMBRES y MUJERES. Sustituye a 01v2_microdatos.R,                      #
#  02v4_1_datos_panel_estado_ano.R, 04 (bloque B3), 05, 06v2 (M1, M2),         #
#  06v2_m3m4_corregido.R y b.R. No necesita cps_merged.rds ni                  #
#  state_panel.rds. No incluye lo que el manuscrito no usa: script 03 (d.R),   #
#  diagnosticos B1, B2 y B4 del 04, PIB estatal y politicas santuario.         #
#                                                                              #
#  COMO USARLO                                                                 #
#   1. Revisa DATA_PATH en la seccion 0.                                       #
#   2. Ejecuta el script entero (en RStudio: Ctrl+Shift+S).                    #
#   3. Abre OUT_DIR/key_numbers.txt. Lee primero "0. COMPROBACIONES";          #
#      despues las cifras van en el orden del manuscrito.                      #
#                                                                              #
#  CRITERIO                                                                    #
#   - Todo lo que hace el manuscrito se ejecuta con sus definiciones y sus     #
#     especificaciones (secciones Data, Index, Identification, H1-H5 y         #
#     Alternative Explanations), tal como estaban en los scripts originales.   #
#   - Solo se cambia lo que es un error comprobable. Cada cambio esta en el    #
#     registro [E1]-[E9] con su justificacion y, cuando depende de los datos,  #
#     con una comprobacion que se imprime en "0. COMPROBACIONES".              #
#   - Las mujeres se estiman con las mismas definiciones, cambiando solo SEX.  #
#   - Lo que no esta en el manuscrito va en el bloque "ADICIONAL" y se         #
#     desactiva con RUN_EXTRAS <- FALSE. No cambia ninguna cifra principal.    #
#                                                                              #
#  ------------------------------------------------------------------------   #
#  REGISTRO DE CORRECCIONES (errores de los scripts originales)                #
#  ------------------------------------------------------------------------   #
#  [E1] Nacidos fuera. El 01 usaba NATIVITY %in% c(4, 5). En IPUMS-CPS,        #
#       CITIZEN 4 = naturalizado y 5 = no ciudadano; 1-3 son nacidos con la    #
#       ciudadania (en EE. UU., en Puerto Rico u otros territorios, o en el    #
#       extranjero de padres estadounidenses). Nacido fuera = CITIZEN 4 o 5.   #
#       La tabla A0 cruza NATIVITY x CITIZEN con tus datos: si las filas con   #
#       NATIVITY 4 tienen CITIZEN 1-3, son ciudadanos de nacimiento (hijos de  #
#       inmigrantes) y el 01 los contaba como inmigrantes.                     #
#  [E2] Naturalizados. La regla de Borjas usaba CITIZEN %in% c(1, 2) con el    #
#       comentario "1 = born abroad of US parents; 2 = naturalized", pero en   #
#       IPUMS-CPS 1 y 2 son nacidos en EE. UU. y en territorios. Los           #
#       naturalizados (4) quedaban como "indocumentados" salvo que otra regla  #
#       los marcara. Ahora CITIZEN == 4.                                       #
#  [E3] YRIMMIG. Las reglas del 01 ("llego en 1980 o antes": YRIMMIG >=      #
#       YEAR - 1980; H-1B: YRIMMIG <= 6) suponen anios desde la llegada. La    #
#       documentacion de IPUMS-CPS lo describe como anio de llegada; con ese   #
#       formato la primera regla marcaria como legal a todo nacido fuera. El   #
#       script mira los datos (mediana) y aplica la regla que corresponda. Si  #
#       son anios desde la llegada (tu extracto: mediana 23), el resultado es  #
#       identico al del 01. Frecuencias en tables/A0g.                         #
#  [E4] Ecuacion principal sin Phi_st. El manuscrito dice que beta mide la     #
#       respuesta "relative to the control group's trajectory"; sin Phi_st,    #
#       la respuesta del grupo de control a Phi queda fijada en cero y beta    #
#       mezcla ambas. Especificacion principal ("phi") = ecuacion del          #
#       manuscrito + Phi_st, nada mas. La escalera de robustez reporta         #
#       tambien la ecuacion sin Phi ("paper") y dos con mas efectos fijos.     #
#  [E5] Paro retardado. shift() se aplicaba sobre filas de individuos, asi     #
#       que el "retardo" era casi siempre el paro del mismo anio. Ahora se     #
#       retarda en el panel estado-anio.                                       #
#  [E6] H5, educacion alta: an[educ_years > 12] eliminaba todo el grupo de     #
#       control (<= 12 anios), y el coeficiente dejaba de ser un DiD. Ahora,   #
#       como dice el manuscrito, cada submuestra va "paired with the control   #
#       group": tratados con > 12 anios + el mismo grupo de control.           #
#  [E7] Event study (06v2_m3m4). M3 excluia los estados que nunca adoptan      #
#       E-Verify (NA -> -999 -> fuera), aunque el propio codigo los describe   #
#       como "controles puros", y no incluia el nivel del grupo tratado, asi   #
#       que t = -1 no era una referencia real. M4 usaba sunab(cohort, YEAR)    #
#       sin interactuar con el tratamiento: estimaba el efecto de E-Verify     #
#       sobre todos (tratados y control juntos), no la diferencia. Ahora los   #
#       dos son triple diferencia (EF estado x anio + grupo x estado + grupo   #
#       x anio); el resto (ventana [-6, 6], fuera las cohortes 2010-2011,      #
#       test F conjunto en t = -6..-2) es como en el original. Como cada       #
#       coeficiente se identifica con muy pocos estados, el F con errores      #
#       agrupados rechaza de mas aunque no haya pretendencias (MacKinnon y     #
#       Webb 2017); se anade el p-valor del wild cluster bootstrap.            #
#  [E8] HISPAN 901/902 ("no sabe" / "sin respuesta") ya no cuentan como        #
#       hispanos. IPUMS los integra en 0 desde 2003, asi que no deberia        #
#       cambiar nada: el script cuenta cuantos hay.                            #
#  [E9] Salvaguarda: si un estado-anio aparece dos veces en los archivos de    #
#       paro o de TRAC, el merge duplicaba individuos. Ahora el script se      #
#       detiene y dice que archivo revisar.                                    #
#  [E10] ASEC 2014. IPUMS-CPS incluye en 2014 dos submuestras (3/8 y 5/8),     #
#       cada una ponderada a toda la poblacion, asi que juntas duplican 2014:  #
#       pesa el doble en las regresiones y su poblacion indocumentada (el      #
#       denominador de las tasas de deportacion) se duplica. Con la variable   #
#       HFLAG el script se queda con una (ASEC2014_SUBSAMPLE).                 #
#  [E11] Deportaciones. TRAC asigna cada deportacion ICE al estado del puerto  #
#       por el que la persona sale del pais ("the state ... of the port of    #
#       entry where the individual was deported from"), no al estado donde     #
#       vivia o fue detenida. El denominador, en cambio, son residentes. Por   #
#       eso Louisiana 2019-2023 da 3.242-14.343 deportaciones por 1.000        #
#       indocumentados y el 99 % de los estado-anios queda por debajo de 0,10  #
#       en el componente. La tasa total ICE SALE del indice principal.         #
#       Secure Communities se mantiene: TRAC la asigna al estado donde se      #
#       enviaron las huellas, es decir, donde se detuvo a la persona. Pero su  #
#       denominador (indocumentados imputados de la CPS, celdas pequenas)     #
#       daba hasta 610 por 1.000 (P90 41,6). Ahora: deportaciones SC por      #
#       1.000 no ciudadanos (CITIZEN 5, observado, no imputado), con la media  #
#       2010-2025 de cada estado como denominador, y log(1 + tasa) antes de    #
#       reescalar. Asi el indice tampoco depende de la imputacion de Borjas    #
#       que se usa en H3. El indice de cinco componentes del manuscrito se     #
#       sigue calculando (enforce_ms5) y se compara en la seccion 14.          #
#  [E12] Colorado. La Tabla 1 dice que su E-Verify estuvo "in effect between   #
#       2008 and 2021". La ley de 2006 (HB 06-1017) obligaba a todos los       #
#       empleadores a declarar que habian revisado el I-9 y guardar copias, no #
#       a usar E-Verify; el Basic Pilot (E-Verify) solo se exigia a           #
#       contratistas del estado (HB 06-1343). Se derogo en 2016 (HB 16-1114).  #
#       Con el criterio del manuscrito ("all or most employers") Colorado no   #
#       es restrictivo en ningun anio, que es lo que ya hacia el codigo. Lo    #
#       que hay que corregir es la Tabla 1. El ajuste queda explicito.         #
#  [E13] Autoempleo. CLASSWKR se refiere al empleo de la semana anterior a la  #
#       entrevista (el de mas horas si hay varios), pero a quien no estaba     #
#       ocupado se le pregunta por su ultimo empleo (IPUMS: universo "persons  #
#       age 15+ who ever worked"). Con CLASSWKR solo, un parado o inactivo     #
#       cuyo ultimo empleo fue por cuenta propia contaba como autoempleado.    #
#       Ahora se_uninc, se_inc, se_any y wage_worker exigen estar ocupado en   #
#       la semana de referencia (SE_EMPLOYED_ONLY). El denominador sigue       #
#       siendo toda la poblacion de 20-64 del grupo, ocupada o no, para que    #
#       asalariado + autoempleo + no ocupado sumen 1 (H4). La tabla A0h dice   #
#       cuantos casos cambian; la seccion 14 da la cifra con la definicion     #
#       anterior.                                                              #
#  [E14] Utah. El script 02 lo hacia restrictivo desde 2022, pero la ley es    #
#       de 2010: S.B. 251 obliga desde el 1 de julio de 2010 a los             #
#       empleadores privados con 15 o mas trabajadores a verificar a los       #
#       nuevos contratados (E-Verify u otro sistema, incluido el SSNVS de la   #
#       Seguridad Social, sin sanciones). H.B. 252 (2022) sube el umbral a     #
#       150 desde el 4 de mayo de 2022: restringe la obligacion, no la crea.   #
#       Desde la v3.5 se trata con la regla general [E15].                     #
#  [E15] Anios de E-Verify. La columna de anios del Excel de NCSL no siempre   #
#       es la entrada en vigor de la obligacion para empleadores privados:     #
#       Georgia "2006-2011" (la obligacion empieza en 2012-2013, HB 87);       #
#       Carolina del Norte 2015 (fue 2012-2013, HB 36); Tennessee 2023 (50+    #
#       obligatorio desde 2017); Florida 2021 (hasta julio de 2023 bastaba     #
#       con guardar copias del I-9); Luisiana 2011 (admite guardar copias de   #
#       documentos). Ahora los anios salen de las leyes (tabla EV_LAW, con la  #
#       ley de cada estado) y de una regla explicita (EV_RULE). Solo cuentan   #
#       los 10 estados que NCSL clasifica como "All or Most Employers"; el     #
#       resto vale 0 (Colorado y Pennsylvania incluidos). La comprobacion de   #
#       replica mantiene la codificacion del 02 para reproducir 0.0447.        #
#  [E16] Licencias de conducir. El 02 leia el rango A1:C60 con los estados en  #
#       la columna A. En la version del Excel del 6 de noviembre los datos     #
#       empiezan en la columna B: el componente salia 0 en todos los estados   #
#       sin ningun aviso, y con el todas las cifras. Ahora se busca la         #
#       columna de los estados (las dos siguientes son 1/0 y el anio) y se lee #
#       hasta el primer enlace. Ademas, el script se para si E-Verify,         #
#       licencias o matricula valen 0 en todos los estado-anios.               #
#  Nota: los p-valores de las diferencias entre coeficientes usan una t con    #
#  G - 1 grados de libertad, como fixest para los propios coeficientes (los    #
#  originales usaban la normal, algo menos conservadora).                      #
#                                                                              #
#  BLOQUE ADICIONAL (RUN_EXTRAS; no esta en el manuscrito)                     #
#   Test formal mujeres - hombres; estatus legal desagregado (naturalizados,   #
#   residentes legales, indocumentados, hispanos nativos); H5 con control de   #
#   educacion alta; salario solo de asalariados actuales; resultados del anio  #
#   pasado con el indice del anio pasado; indice con tasas winsorizadas al     #
#   p99; test de anticipacion; event study con los otros componentes del       #
#   indice como controles; jackknife de clusters y leave-one-state-out; wild   #
#   cluster bootstrap; tabla descriptiva y figura descriptiva.                 #
#                                                                              #
#  CODIGOS IPUMS                                                               #
#   Comprobados: CITIZEN 1-5, YRIMMIG (anio de llegada), HISPAN 901/902.       #
#   Copiados tal cual de los scripts originales (no los he cambiado): EDUC ->  #
#   anios, CLASSWKR 13/14/21-28/25-28/26, EMPSTAT 01 (fuerzas armadas),        #
#   FULLPART 1, WORKLY 2, UHRSWORKLY 999, INCLONGJ >= 99999998, OCC50LY de     #
#   ocupaciones con licencia y H-1B, IND1990 de sectores, BPL de origen,       #
#   METRO 2-4, MARST 1-2. Las tablas A0* muestran las frecuencias reales.      #
################################################################################


# ============================================================================
# 0. CONFIGURACION
# ============================================================================

# Carpeta con cps_00005.csv, cps_00006.csv, los Excel de politicas, los 51
# archivos "<Estado> unemployment.xlsx" y los dos Excel de TRAC.
# (\u00e1 = a con tilde; asi el archivo no depende de la codificacion de Windows)
DATA_PATH <- "#######"

FILE_CPS_A <- "cps_00005.csv"   # demograficas y laborales basicas
FILE_CPS_B <- "cps_00006.csv"   # ingresos y prestaciones (ASEC)

# Carpeta de salida (se crea si no existe; relativa al directorio de trabajo).
OUT_DIR <- "resultados_cjlacs"

YEAR_START <- 2010L
YEAR_END   <- 2025L

# Especificacion principal:
#   "phi"   ecuacion del manuscrito + Phi_st   [E4]   <- por defecto
#   "paper" ecuacion del manuscrito tal cual (sin Phi_st)
#   "sxy"   EF estado x anio
#   "full"  EF estado x anio + grupo x estado + grupo x anio
# Las cuatro se reportan siempre en la escalera de robustez.
MAIN_SPEC <- "phi"

# ASEC 2014: IPUMS-CPS trae dos submuestras (3/8 con las preguntas de ingresos
# nuevas, HFLAG = 1; 5/8 con las antiguas, HFLAG = 0) y cada una esta ponderada
# a toda la poblacion: juntas duplican 2014 [E10]. IPUMS recomienda usar una:
# la 3/8 para comparar con 2015 en adelante, la 5/8 para comparar con 2013 y
# antes. Como 11 de los otros 15 anios del panel son 2015-2025, por defecto 3/8.
#   "3/8" | "5/8" | "both" (las dos, como el manuscrito enviado)
# Necesita la variable HFLAG en uno de los dos extractos; si no esta, se avisa.
ASEC2014_SUBSAMPLE <- "3/8"

# Secure Communities: el script 02 deja SIN indice los estado-anios anteriores
# a 2021 con la celda vacia en TRAC (y ICE vacio lo pone a 0). TRUE pone esas
# celdas a 0, como ICE. FALSE = como el manuscrito enviado.
SC_NA_AS_ZERO <- FALSE

# Definicion de autoempleo y empleo asalariado [E13]:
#   TRUE  = solo si la persona estaba ocupada en la semana de referencia
#           (EMPSTAT 01 fuerzas armadas, 10 trabajando, 12 con empleo pero
#           sin trabajar esa semana)   <- por defecto
#   FALSE = solo CLASSWKR, como el manuscrito enviado (incluye el ultimo
#           empleo de quien no estaba ocupado)
SE_EMPLOYED_ONLY <- TRUE

# Indice principal [E11]:
#   "v4"  E-Verify + Secure Communities - licencias - matricula   <- por defecto
#   "ms5" las cinco componentes del manuscrito enviado (con la tasa total ICE)
INDEX_MAIN <- "v4"
# Secure Communities en el indice v4: TRUE = log(1 + tasa) antes de reescalar.
SC_LOG <- TRUE

# E-Verify: desde que anio cuenta cada estado como restrictivo [E15].
#   "estricta" E-Verify obligatorio, sin alternativa, para todos los
#              empleadores privados o para los de 50 o mas trabajadores (o un
#              umbral menor), desde el anio natural de entrada en vigor.
#              Luisiana y Utah no cuentan (sus leyes admiten alternativas).
#                                                               <- por defecto
#   "amplia"   cualquier ley que obligue a esos empleadores a verificar,
#              aunque admita alternativas (copias de documentos, licencia de
#              conducir, otros sistemas). Utah 2010-2021 (desde 2022, 150+).
#   "excel"    anios del Excel de NCSL con los ajustes del script 02 (como la
#              v3.3: Utah desde 2022).
# Los anios de cada estado y su ley estan en la tabla EV_LAW (seccion 4.1).
EV_RULE <- "estricta"

RUN_REPLICATION_CHECK <- TRUE   # reproduce la cifra del manuscrito con la codificacion antigua
RUN_EVENT_STUDY       <- TRUE   # event study E-Verify (TWFE y Sun-Abraham), como en 06v2
RUN_EXTRAS            <- TRUE   # bloque ADICIONAL (no esta en el manuscrito)
RUN_JACKKNIFE         <- TRUE   # (ADICIONAL) jackknife de clusters y leave-one-state-out
RUN_WILD_BOOTSTRAP    <- TRUE   # p-valores wild cluster bootstrap (pretendencias del event study y, en ADICIONAL, H1)
WILD_B                <- 999    # replicas del bootstrap
WINSOR_P_EXTRA        <- 0.99   # (ADICIONAL) percentil de winsorizacion de las tasas
FIG_DPI               <- 600    # Taylor & Francis: >= 300 color, >= 600 grises


# ============================================================================
# 1. PAQUETES, CARPETAS Y REGISTRO
# ============================================================================

pkgs <- c("data.table", "fixest", "ggplot2", "readxl", "scales", "stringi")
faltan <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan) > 0) install.packages(faltan)
suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
  library(readxl)
  library(scales)
  library(stringi)
})
setFixest_notes(FALSE)

if (!MAIN_SPEC %in% c("paper", "phi", "sxy", "full")) {
  stop("MAIN_SPEC debe ser 'paper', 'phi', 'sxy' o 'full'.")
}
if (!ASEC2014_SUBSAMPLE %in% c("3/8", "5/8", "both")) {
  stop("ASEC2014_SUBSAMPLE debe ser '3/8', '5/8' o 'both'.")
}
if (!EV_RULE %in% c("estricta", "amplia", "excel")) {
  stop("EV_RULE debe ser 'estricta', 'amplia' o 'excel'.")
}
if (!dir.exists(DATA_PATH)) stop("La carpeta DATA_PATH no existe: ", DATA_PATH)

while (sink.number() > 0) sink()
TAB_DIR <- file.path(OUT_DIR, "tables")
FIG_DIR <- file.path(OUT_DIR, "figures")
dir.create(TAB_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
sink(file.path(OUT_DIR, "log.txt"), split = TRUE)

T0 <- Sys.time()

# say("formato", ...): imprime una linea en consola y log. Los % literales
# van como %% solo cuando hay argumentos.
say <- function(fmt, ...) {
  s <- if (length(list(...)) == 0) fmt else sprintf(fmt, ...)
  cat(s, "\n", sep = "")
}
section <- function(txt) {
  cat("\n", strrep("=", 78), "\n", txt, "\n", strrep("=", 78), "\n", sep = "")
}

# Cifras clave: se guardan en key_numbers.txt en el orden en que se generan.
KN <- character(0)
kn <- function(fmt, ...) {
  s <- if (length(list(...)) == 0) fmt else sprintf(fmt, ...)
  KN <<- c(KN, s)
  cat(s, "\n", sep = "")
}
kn_head <- function(txt) {
  KN <<- c(KN, "", txt, strrep("-", nchar(txt)))
  cat("\n", txt, "\n", sep = "")
}
# Avisos que se repiten al principio de key_numbers.txt
CHECKS <- character(0)
check <- function(fmt, ...) {
  s <- if (length(list(...)) == 0) fmt else sprintf(fmt, ...)
  CHECKS <<- c(CHECKS, s)
  say("  [COMPROBAR] %s", s)
}

f4  <- function(x) ifelse(is.na(x), "NA", sprintf("%.4f", x))
f3  <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", x))
f2  <- function(x) ifelse(is.na(x), "NA", sprintf("%.2f", x))
f1  <- function(x) ifelse(is.na(x), "NA", sprintf("%.1f", x))
fp  <- function(p) ifelse(is.na(p), "NA", ifelse(p < 0.001, "< 0.001", sprintf("= %.3f", p)))
fN  <- function(n) ifelse(is.na(n), "NA", formatC(n, format = "d", big.mark = ","))
fpc <- function(x) ifelse(is.na(x), "NA", sprintf("%.1f%%", 100 * x))
# p-valor de bootstrap: con 0 replicas por encima, p < 1/B
fpb <- function(wb) {
  if (is.null(wb$p) || is.na(wb$p)) {
    return(if (!is.null(wb$why) && !is.na(wb$why)) sprintf("NA (%s)", wb$why) else "NA")
  }
  if (wb$p == 0) return(sprintf("< %s", format(1 / wb$B, digits = 2)))
  sprintf("= %.3f", wb$p)
}
wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

say("Inicio: %s", format(T0))
say("R %s | fixest %s | data.table %s", getRversion(),
    as.character(packageVersion("fixest")), as.character(packageVersion("data.table")))
say("Especificacion principal: %s", MAIN_SPEC)


# ============================================================================
# 2. FUNCIONES AUXILIARES
# ============================================================================

STATES <- data.table(
  state_fips = c(1L, 2L, 4L, 5L, 6L, 8L, 9L, 10L, 11L, 12L, 13L, 15L, 16L,
                 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L,
                 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L,
                 41L, 42L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 53L, 54L,
                 55L, 56L),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
                 "Colorado", "Connecticut", "Delaware", "District of Columbia",
                 "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
                 "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
                 "Massachusetts", "Michigan", "Minnesota", "Mississippi",
                 "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                 "New Jersey", "New Mexico", "New York", "North Carolina",
                 "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
                 "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
                 "Texas", "Utah", "Vermont", "Virginia", "Washington",
                 "West Virginia", "Wisconsin", "Wyoming")
)

norm_name <- function(x) {
  x <- as.character(x)
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x) & x != ""
  if (any(ok)) {
    v <- stri_trans_general(x[ok], "Latin-ASCII")
    v <- tolower(v)
    v <- gsub("[[:punct:][:space:]]+", "", v)
    out[ok] <- v
  }
  out
}

# Alias del script 02 original
ALIAS <- setNames(STATES$state_fips, norm_name(STATES$state_name))
ALIAS <- c(ALIAS,
           dc = 11L, districtofcolumbiadc = 11L, districtocolumbiadc = 11L,
           hawai = 15L, luisiana = 22L, misisipi = 28L, misuri = 29L,
           nuevahampshire = 33L, nuevajersey = 34L, nuevomexico = 35L,
           nuevayork = 36L, carolinadelnorte = 37L, carolinadelsur = 45L,
           dakotadelnorte = 38L, dakotadelsur = 46L, pensilvania = 42L,
           virginiaoccidental = 54L, utha = 49L)

lookup_fips <- function(x) {
  n <- norm_name(x)
  out <- unname(ALIAS[n])
  out[is.na(n)] <- NA_integer_
  as.integer(out)
}

# Primer numero de 4 cifras (como extract_first_year del script 02)
extract_first_year <- function(x) {
  x <- as.character(x)
  out <- rep(NA_integer_, length(x))
  ok <- !is.na(x)
  m <- regexpr("[0-9]{4}", x[ok])
  hit <- m > 0
  out[ok][hit] <- as.integer(regmatches(x[ok], m))
  out
}

# Busca un archivo en DATA_PATH: primero el nombre exacto, despues un patron.
find_file <- function(exact, pattern, label) {
  f <- file.path(DATA_PATH, exact)
  if (file.exists(f)) return(f)
  hits <- list.files(DATA_PATH, pattern = pattern, ignore.case = TRUE, full.names = TRUE)
  hits <- hits[!grepl("^~\\$", basename(hits))]
  if (length(hits) == 0) {
    stop("No encuentro el archivo de ", label, " en DATA_PATH (busque '", exact,
         "' y el patron '", pattern, "').")
  }
  if (length(hits) > 1) say("  Aviso: varios archivos para %s; uso %s", label, basename(hits[1]))
  say("  %s: uso %s (no existe '%s')", label, basename(hits[1]), exact)
  hits[1]
}

# El script 02 leia la hoja "Sheet1"; si no existe, se usa la primera.
pick_sheet <- function(f) {
  sh <- excel_sheets(f)
  if ("Sheet1" %in% sh) return("Sheet1")
  say("  Aviso: %s no tiene hoja 'Sheet1'; uso '%s'", basename(f), sh[1])
  sh[1]
}

# Reescalado min-max al intervalo [0, 1] (script 02)
rescale01 <- function(x) {
  rng <- suppressWarnings(range(x, na.rm = TRUE))
  if (any(!is.finite(rng)) || diff(rng) == 0) return(ifelse(is.na(x), NA_real_, 0))
  (x - rng[1]) / (rng[2] - rng[1])
}

winsor_up <- function(x, p) {
  if (is.na(p)) return(x)
  pmin(x, quantile(x, p, na.rm = TRUE, names = FALSE))
}


# ============================================================================
# 3. MICRODATOS IPUMS-CPS (ASEC)  -- script 01
# ============================================================================
section("3. MICRODATOS IPUMS-CPS (ASEC)")

KEY   <- c("YEAR", "SERIAL", "PERNUM")
REQ_A <- c(KEY, "ASECWT", "STATEFIP", "AGE", "SEX", "MARST", "EDUC", "HISPAN",
           "NATIVITY", "BPL", "METRO", "CITIZEN", "YRIMMIG", "EMPSTAT",
           "CLASSWKR", "OCC50LY", "IND1990")
REQ_B <- c(KEY, "WORKLY", "WKSWORK1", "UHRSWORKLY", "FULLPART",
           "INCSS", "INCRETIR", "INCLONGJ", "HIMCAIDLY", "PUBHOUS", "RENTSUB")

fA <- file.path(DATA_PATH, FILE_CPS_A)
fB <- file.path(DATA_PATH, FILE_CPS_B)
if (!file.exists(fA)) stop("No existe ", fA)
if (!file.exists(fB)) stop("No existe ", fB)

namesA <- names(fread(fA, nrows = 0))
namesB <- names(fread(fB, nrows = 0))
if (length(setdiff(REQ_A, namesA)) > 0) {
  stop("Faltan columnas en ", FILE_CPS_A, ": ", paste(setdiff(REQ_A, namesA), collapse = ", "))
}
if (length(setdiff(REQ_B, namesB)) > 0) {
  stop("Faltan columnas en ", FILE_CPS_B, ": ", paste(setdiff(REQ_B, namesB), collapse = ", "))
}
if (!"ASECFLAG" %in% namesB) {
  stop(FILE_CPS_B, " no tiene ASECFLAG; el script 01 filtraba ASECFLAG == 1 en este extracto.")
}
selA <- c(REQ_A, intersect(c("ASECFLAG", "HFLAG"), namesA))
selB <- c(setdiff(REQ_B, setdiff(REQ_A, KEY)), "ASECFLAG",
          if (!"HFLAG" %in% namesA) intersect("HFLAG", namesB))

dA <- fread(fA, select = selA)
say("  %s: %s filas", FILE_CPS_A, fN(nrow(dA)))
dB <- fread(fB, select = selB)
say("  %s: %s filas", FILE_CPS_B, fN(nrow(dB)))

# Script 01: el extracto B se filtra a ASECFLAG == 1.
dB <- dB[ASECFLAG == 1L]
dB[, ASECFLAG := NULL]
say("  %s tras ASECFLAG == 1: %s filas", FILE_CPS_B, fN(nrow(dB)))
N_A_NONASEC <- 0L
if ("ASECFLAG" %in% names(dA)) {
  N_A_NONASEC <- dA[is.na(ASECFLAG) | ASECFLAG != 1L, .N]
  dA <- dA[ASECFLAG == 1L]
  dA[, ASECFLAG := NULL]
  if (N_A_NONASEC > 0) {
    check("%s traia %s filas que no son del ASEC (ASECFLAG != 1); se excluyen.",
          FILE_CPS_A, fN(N_A_NONASEC))
  }
}
dupA <- sum(duplicated(dA, by = KEY))
dupB <- sum(duplicated(dB, by = KEY))
if (dupA > 0 || dupB > 0) {
  stop("Hay claves YEAR-SERIAL-PERNUM duplicadas (A: ", dupA, ", B: ", dupB,
       "). El merge duplicaria personas. Revisa los extractos.")
}

# Script 01: merge anclado en el extracto A.
dt <- merge(dA, dB, by = KEY, all.x = TRUE)
rm(dA, dB); invisible(gc(verbose = FALSE))
MATCH_RATE <- mean(!is.na(dt$WORKLY))
say("  Tras el merge: %s filas; con datos del extracto B: %s", fN(nrow(dt)), fpc(MATCH_RATE))
if (MATCH_RATE < 0.95) check("Solo el %s de las filas cruza entre los dos extractos.", fpc(MATCH_RATE))

# Script 01: edad 20-64. Ademas se restringe a YEAR_START-YEAR_END (el panel
# de politicas solo cubre esos anios).
n_raw <- nrow(dt)
n_out_years <- dt[YEAR < YEAR_START | YEAR > YEAR_END, .N]
if (n_out_years > 0) {
  check("Hay %s filas fuera de %d-%d en el extracto; se excluyen.", fN(n_out_years),
        YEAR_START, YEAR_END)
}
dt <- dt[AGE >= 20 & AGE <= 64 & YEAR >= YEAR_START & YEAR <= YEAR_END]
say("  Tras edad 20-64 y anios %d-%d: %s filas (de %s)", YEAR_START, YEAR_END,
    fN(nrow(dt)), fN(n_raw))
n_badw <- dt[is.na(ASECWT) | ASECWT <= 0, .N]
if (n_badw > 0) {
  say("  %s filas con ASECWT ausente o <= 0 (fixest las descarta al ponderar; igual que antes).",
      fN(n_badw))
}

# Poblacion ponderada por anio: un anio con el doble de poblacion delataria
# muestras duplicadas (p. ej., las dos submuestras del ASEC 2014).
pop_year <- dt[, .(n = .N, pop_w = sum(ASECWT, na.rm = TRUE)), by = YEAR][order(YEAR)]
ratio_pop <- pop_year$pop_w / median(pop_year$pop_w)
POP2014_RATIO <- if (2014L %in% pop_year$YEAR) ratio_pop[pop_year$YEAR == 2014L] else NA_real_

# [E10] ASEC 2014: quedarse con una submuestra (HFLAG). La comprobacion de
# replica usa los datos tal como los uso el manuscrito (las dos submuestras).
OLD_RAW <- NULL
N_2014_DROPPED <- 0L
if (ASEC2014_SUBSAMPLE != "both" && "HFLAG" %in% names(dt)) {
  keep_flag <- if (ASEC2014_SUBSAMPLE == "3/8") 1L else 0L
  drop14 <- dt$YEAR == 2014L & !is.na(dt$HFLAG) & dt$HFLAG != keep_flag
  N_2014_DROPPED <- sum(drop14)
  if (N_2014_DROPPED > 0 && RUN_REPLICATION_CHECK) {
    OLD_RAW <- dt[, .(STATEFIP, YEAR, ASECWT, AGE, SEX, MARST, METRO, EDUC, CLASSWKR, NATIVITY,
                      CITIZEN, HISPAN, YRIMMIG, INCSS, INCRETIR, HIMCAIDLY, EMPSTAT, PUBHOUS,
                      RENTSUB, BPL, OCC50LY)]
  }
  dt <- dt[!drop14]
  rm(drop14)
  say("  ASEC 2014: se usa la submuestra %s (HFLAG = %d); se quitan %s filas de la otra.",
      ASEC2014_SUBSAMPLE, keep_flag, fN(N_2014_DROPPED))
  pop_year <- dt[, .(n = .N, pop_w = sum(ASECWT, na.rm = TRUE)), by = YEAR][order(YEAR)]
  ratio_pop <- pop_year$pop_w / median(pop_year$pop_w)
} else if (ASEC2014_SUBSAMPLE != "both" && isTRUE(POP2014_RATIO > 1.4)) {
  check("La poblacion ponderada de 2014 es %s veces la mediana: el extracto trae las dos submuestras del ASEC 2014 y cada una pesa como toda la poblacion. Anade la variable HFLAG al extracto de IPUMS para quedarte con una [E10]. Mientras tanto, 2014 pesa el doble y su poblacion indocumentada (denominador de las tasas) tambien.",
        f2(POP2014_RATIO))
}
fwrite(pop_year, file.path(TAB_DIR, "A0b_population_by_year.csv"))
if (any(ratio_pop > 1.4 | ratio_pop < 0.6)) {
  check("Poblacion ponderada anomala en %s (tabla A0b): revisa si hay muestras duplicadas.",
        paste(pop_year$YEAR[ratio_pop > 1.4 | ratio_pop < 0.6], collapse = ", "))
}

# Frecuencias de los codigos que usa el script (para comprobar contra el codebook)
fwrite(dt[, .N, by = CLASSWKR][order(CLASSWKR)], file.path(TAB_DIR, "A0e_classwkr_codes.csv"))
fwrite(dt[, .N, by = EMPSTAT][order(EMPSTAT)],   file.path(TAB_DIR, "A0f_empstat_codes.csv"))

# EMPSTAT: la regla de fuerzas armadas (EMPSTAT == 1) necesita los codigos
# detallados de 2 cifras (01 = fuerzas armadas, 10/12 ocupados, 20-22 parados,
# 30-36 inactivos). Si el extracto trae solo 1/2/3, 1 significaria "ocupado".
if (!any(dt$EMPSTAT >= 10, na.rm = TRUE)) {
  stop("EMPSTAT solo tiene codigos < 10: parece la version general (1 ocupado, 2 parado, ",
       "3 inactivo). La regla de Borjas EMPSTAT == 1 (fuerzas armadas) necesita los ",
       "codigos detallados. Revisa la tabla A0f y el extracto.")
}

# ---------------------------------------------------------------------------
# 3.1 Recodificaciones basicas (script 01)
# ---------------------------------------------------------------------------
# Anios de educacion: mapa del script 01, sin cambios.
EDUC_MAP <- c("0" = 0, "1" = 0, "2" = 2, "10" = 4, "20" = 6, "30" = 8, "40" = 9,
              "50" = 10, "60" = 11, "71" = 12, "73" = 12, "81" = 13, "91" = 14,
              "92" = 14, "111" = 16, "123" = 18, "124" = 19, "125" = 20)
dt[, educ_years := unname(EDUC_MAP[as.character(EDUC)])]
educ_tab <- dt[, .N, by = EDUC][order(EDUC)]
educ_tab[, educ_years := unname(EDUC_MAP[as.character(EDUC)])]
fwrite(educ_tab, file.path(TAB_DIR, "A0d_educ_codes.csv"))
n_educ_na <- dt[is.na(educ_years), .N]
if (n_educ_na > 0) {
  say("  %s filas con un codigo EDUC fuera del mapa del script 01 (educ_years = NA; ver A0d).",
      fN(n_educ_na))
}

dt[, male     := as.integer(SEX == 1L)]
dt[, married  := as.integer(MARST %in% c(1L, 2L))]
dt[, age_sq   := AGE^2]
dt[, college  := as.integer(!is.na(educ_years) & educ_years >= 16)]
dt[, in_metro := as.integer(METRO %in% c(2L, 3L, 4L))]

# [E8] Hispanos: HISPAN 1-899 (901/902 = no sabe / sin respuesta)
N_HISPAN_9XX <- dt[HISPAN >= 900L, .N]
dt[, hispanic := as.integer(HISPAN > 0L & HISPAN < 900L)]

# [E1][E2] Nacidos fuera y naturalizados
dt[, foreign_born := as.integer(CITIZEN %in% c(4L, 5L))]
dt[, naturalized  := as.integer(CITIZEN == 4L)]
if (!any(dt$CITIZEN %in% c(4L, 5L))) {
  stop("CITIZEN no contiene los codigos 4/5 de IPUMS-CPS. Revisa el extracto.")
}
diag_nc <- dt[, .(n = .N, pop_w = round(sum(ASECWT, na.rm = TRUE))), by = .(NATIVITY, CITIZEN)]
setorder(diag_nc, NATIVITY, CITIZEN)
fwrite(diag_nc, file.path(TAB_DIR, "A0_nativity_x_citizen.csv"))
SH_NAT4_CIT123 <- dt[NATIVITY == 4L, mean(CITIZEN %in% 1:3)]
SH_NAT5_CIT25  <- dt[NATIVITY == 5L, mean(CITIZEN %in% 2:5)]
N_NAT4 <- dt[NATIVITY == 4L, .N]
say("  NATIVITY 4: %s filas; con CITIZEN 1-3 (nacidos con la ciudadania): %s",
    fN(N_NAT4), fpc(SH_NAT4_CIT123))
FB_SHARE_W     <- dt[, wmean(foreign_born, ASECWT)]
FB_SHARE_OLD_W <- dt[, wmean(as.integer(NATIVITY %in% c(4L, 5L)), ASECWT)]
say("  Nacidos fuera, ponderado: %s con CITIZEN 4/5 | %s con NATIVITY 4/5 (definicion del 01)",
    fpc(FB_SHARE_W), fpc(FB_SHARE_OLD_W))

# [E3] YRIMMIG: anio de llegada (IPUMS-CPS) o, si el extracto viniera de otra
# forma, anios desde la llegada. Se decide con la mediana entre nacidos fuera.
yr_pos <- dt[foreign_born == 1L & YRIMMIG > 0L, YRIMMIG]
YRIMMIG_IS_YEAR <- length(yr_pos) > 0 && median(yr_pos) > 1900
dt[, arrival_year := NA_integer_]
if (YRIMMIG_IS_YEAR) {
  dt[YRIMMIG > 0L, arrival_year := as.integer(YRIMMIG)]
} else {
  dt[YRIMMIG > 0L, arrival_year := as.integer(YEAR - YRIMMIG)]
}
dt[, yrs_in_us := YEAR - arrival_year]
YRIMMIG_MEDIAN <- if (length(yr_pos) > 0) median(yr_pos) else NA
fwrite(dt[foreign_born == 1L, .N, by = YRIMMIG][order(YRIMMIG)], file.path(TAB_DIR, "A0g_yrimmig_foreign_born.csv"))
say("  YRIMMIG interpretado como %s (mediana entre nacidos fuera: %s)",
    ifelse(YRIMMIG_IS_YEAR, "ANIO DE LLEGADA", "ANIOS DESDE LA LLEGADA"), YRIMMIG_MEDIAN)
if (!YRIMMIG_IS_YEAR) {
  check("YRIMMIG no parece un anio de calendario (mediana %s): se trata como anios desde la llegada, igual que el script 01. La documentacion de IPUMS-CPS lo describe como anio de llegada; confirma en el codebook de tu extracto que significan los valores (frecuencias en tables/A0g_yrimmig_foreign_born.csv).",
        YRIMMIG_MEDIAN)
}

# Origen (script 01)
dt[, origin := "Other"]
dt[BPL >= 20000L & BPL < 21000L, origin := "Mexico"]
dt[BPL >= 21010L & BPL <= 21070L, origin := "CentralAm"]
dt[BPL == 25000L | (BPL >= 26000L & BPL < 27000L), origin := "Caribbean"]
dt[BPL >= 30000L & BPL < 31000L, origin := "SouthAm"]


# ---------------------------------------------------------------------------
# 3.2 Estatus legal: Borjas (2017) + filtro H-1B de Borjas y Cassidy (2019),
#     reglas del script 01 con [E2] y [E3]. NA para los nativos.
# ---------------------------------------------------------------------------
LIC_OCCS <- c(0:36, 42:69, 73:79)    # OCC50LY con licencia (script 01)
H1B_OCCS <- c(3:6, 10:15, 43:46)     # OCC50LY tipicas de H-1B (script 01)

fbi <- dt$foreign_born == 1L
rules <- list(
  R1_arrival_by_1980     = fbi & !is.na(dt$arrival_year) & dt$arrival_year <= 1980L,  # = YRIMMIG >= YEAR - 1980 del 01
  R2_naturalized         = fbi & dt$CITIZEN == 4L,
  R3_SS_income           = fbi & !is.na(dt$INCSS) & dt$INCSS > 0,
  R3_retirement_income   = fbi & !is.na(dt$INCRETIR) & dt$INCRETIR > 0,
  R3_medicaid            = fbi & !is.na(dt$HIMCAIDLY) & dt$HIMCAIDLY == 2L,
  R4_armed_forces        = fbi & (dt$EMPSTAT %in% 1L | dt$CLASSWKR %in% 26L),
  R5_public_sector       = fbi & dt$CLASSWKR %in% c(25L, 27L, 28L),
  R6_public_housing      = fbi & !is.na(dt$PUBHOUS) & dt$PUBHOUS == 2L,
  R6_rent_subsidy        = fbi & !is.na(dt$RENTSUB) & dt$RENTSUB == 2L,
  R7_cuba                = fbi & dt$BPL %in% 25000L,
  R8_licensed_occupation = fbi & dt$OCC50LY %in% LIC_OCCS,
  R9_h1b                 = fbi & dt$OCC50LY %in% H1B_OCCS & !is.na(dt$yrs_in_us) &
    dt$yrs_in_us <= 6L & dt$college == 1L
)
any_rule <- Reduce(`|`, rules)
dt[, legal := NA_integer_]
dt[foreign_born == 1L, legal := as.integer(any_rule[fbi])]

# Diagnostico por regla (script b.R): marcados por cada regla y marcados SOLO por ella
n_fb_all <- sum(fbi)
borjas_diag <- rbindlist(lapply(names(rules), function(r) {
  others <- Reduce(`|`, rules[setdiff(names(rules), r)])
  data.table(rule = r, flagged = sum(rules[[r]]), pct_of_foreign_born = sum(rules[[r]]) / n_fb_all,
             only_this_rule = sum(rules[[r]] & !others))
}))
fwrite(borjas_diag, file.path(TAB_DIR, "A0c_borjas_rules.csv"))
rm(rules, any_rule, fbi); invisible(gc(verbose = FALSE))

dt[, status := fifelse(foreign_born == 0L, "native",
                       fifelse(CITIZEN == 4L, "naturalized",
                               fifelse(legal == 1L, "legal_resident", "undocumented")))]
dt[, undoc := as.integer(foreign_born == 1L & legal == 0L)]

N_FB  <- dt[foreign_born == 1L, .N]
N_DOC <- dt[foreign_born == 1L & legal == 1L, .N]
N_UND <- dt[foreign_born == 1L & legal == 0L, .N]
UND_W <- dt[foreign_born == 1L, wmean(undoc, ASECWT)]
say("  Nacidos fuera (ambos sexos): %s | documentados %s (%s) | indocumentados %s (%s) | ponderado indoc. %s",
    fN(N_FB), fN(N_DOC), fpc(N_DOC / N_FB), fN(N_UND), fpc(N_UND / N_FB), fpc(UND_W))


# ---------------------------------------------------------------------------
# 3.3 Variables de resultado (script 01, sin cambios)
# ---------------------------------------------------------------------------
# [E13] Ocupado en la semana de referencia: EMPSTAT 01 (fuerzas armadas),
# 10 (trabajando) o 12 (con empleo, sin trabajar esa semana).
if (dt[EMPSTAT %in% c(10L, 12L), .N] == 0) {
  stop("No hay ninguna fila con EMPSTAT 10 o 12 (ocupados). Revisa la tabla A0f y el extracto.")
}
dt[, employed := as.integer(EMPSTAT %in% c(1L, 10L, 12L))]
dt[, emp_ok := if (SE_EMPLOYED_ONLY) employed == 1L else TRUE]
dt[, se_uninc    := as.integer(CLASSWKR == 13L & emp_ok)]
dt[, se_inc      := as.integer(CLASSWKR == 14L & emp_ok)]
dt[, se_any      := as.integer(CLASSWKR %in% c(13L, 14L) & emp_ok)]
dt[, se_diff     := se_uninc - se_inc]          # solo para la comprobacion de H1
dt[, wage_worker := as.integer(CLASSWKR >= 21L & CLASSWKR <= 28L & emp_ok)]
# Definicion anterior (solo CLASSWKR), para comparar en la seccion 14
dt[, se_uninc_cw := as.integer(CLASSWKR == 13L)]
dt[, emp_ok := NULL]
# Tabla A0h: clase de trabajador segun si la persona estaba ocupada
A0H <- dt[, .(cls = fcase(CLASSWKR == 13L, "13 self-employed, not incorporated",
                          CLASSWKR == 14L, "14 self-employed, incorporated",
                          CLASSWKR >= 21L & CLASSWKR <= 28L, "21-28 wage and salary",
                          default = "other / NIU"), employed)][
                            , .N, by = .(cls, employed)][order(cls, -employed)]
fwrite(A0H, file.path(TAB_DIR, "A0h_classwkr_by_employment.csv"))
SHARE_SE_NOTEMP <- dt[CLASSWKR == 13L, mean(employed == 0L)]
SHARE_SEI_NOTEMP <- dt[CLASSWKR == 14L, mean(employed == 0L)]
dt[, fulltime_ly := as.integer(FULLPART == 1L)]
dt[, total_hours_ly := NA_real_]
dt[WORKLY == 2L & UHRSWORKLY < 999,
   total_hours_ly := as.numeric(WKSWORK1) * as.numeric(UHRSWORKLY)]
dt[, log_inclongj := NA_real_]
dt[!is.na(INCLONGJ) & INCLONGJ > 0 & INCLONGJ < 99999998, log_inclongj := log(INCLONGJ)]
# (ADICIONAL) mismo salario, solo para quienes son asalariados en marzo
dt[, log_inclongj_wage := fifelse(wage_worker == 1L, log_inclongj, NA_real_)]
N_CLASSWKR_99 <- dt[CLASSWKR == 99L, .N]

# Sectores con alta concentracion de indocumentados (IND1990, script 01)
dt[, sector_high_undoc := as.integer((IND1990 >= 10L & IND1990 <= 31L) |      # agricultura
                                       (IND1990 >= 60L & IND1990 <= 62L) |      # construccion
                                       (IND1990 >= 761L & IND1990 <= 791L) |    # servicios personales
                                       IND1990 %in% c(641L, 762L))]             # hosteleria

# Grupos (por persona)
dt[, g_natnh    := as.integer(foreign_born == 0L & hispanic == 0L)]
dt[, g_ctrl     := as.integer(g_natnh == 1L & !is.na(educ_years) & educ_years <= 12)]
dt[, g_ctrl_hi  := as.integer(g_natnh == 1L & !is.na(educ_years) & educ_years > 12)]
dt[, g_hfb      := as.integer(foreign_born == 1L & hispanic == 1L)]
dt[, g_nhfb     := as.integer(foreign_born == 1L & hispanic == 0L)]
dt[, g_nathisp  := as.integer(foreign_born == 0L & hispanic == 1L)]


# ============================================================================
# 4. PANEL ESTADO-ANIO E INDICE DE ENFORCEMENT  -- script 02 v4.1
# ============================================================================
section("4. PANEL ESTADO-ANIO E INDICE DE ENFORCEMENT")

YEARS <- YEAR_START:YEAR_END
base <- CJ(state_fips = STATES$state_fips, year = YEARS)

# ---------------------------------------------------------------------------
# 4.1 E-Verify (restrictiva)
# ---------------------------------------------------------------------------
f_ev <- find_file("e_verify_states.xlsx", "verify.*\\.xlsx$", "E-Verify")
ev <- as.data.table(read_excel(f_ev, sheet = pick_sheet(f_ev)))
if (ncol(ev) < 3) stop("El Excel de E-Verify debe tener 3 columnas (Estado, Categoria, Anio).")
setnames(ev, paste0("c", seq_len(ncol(ev))))      # las cabeceras pueden llevar tildes
ev <- ev[, .(Estado = as.character(c1), Categoria = trimws(as.character(c2)), Anio = c3)]
ev <- ev[!is.na(Estado) & Estado != "" & !grepl("^https?", Estado)]
ev[, state_fips := lookup_fips(Estado)]
if (ev[is.na(state_fips), .N] > 0) {
  check("E-Verify: filas con un nombre de estado no reconocido (se ignoran, como en el 02): %s",
        paste(ev[is.na(state_fips), Estado], collapse = "; "))
}
ev[, start_year := extract_first_year(Anio)]
ev[, end_year   := YEAR_END + 1L]
ev[, is_strong  := as.integer(Categoria %in% "All or Most Employers")]
say("  Categorias de E-Verify en el Excel (solo 'All or Most Employers' cuenta como restrictiva):")
print(ev[, .N, by = .(Categoria, is_strong)])
# Ajustes manuales del script 02 (para la replica y para EV_RULE = "excel"):
#   Colorado no restrictivo en ningun anio [E12] (el 02 lo escribia como
#   "2008-2021", pero al poner is_strong = 0 el efecto era el mismo: nunca 1);
#   Pennsylvania no restrictivo desde 2012 (solo contratistas publicos);
#   Utah restrictivo desde 2022 [E14].
apply_ev_overrides <- function(ev_in, warn = TRUE) {
  e <- copy(ev_in)
  ovr <- data.table(state_fips = c(8L, 42L, 49L), start_year = c(YEAR_START, 2012L, 2022L),
                    end_year = c(YEAR_END + 1L, YEAR_END + 1L, YEAR_END + 1L), is_strong = c(0L, 0L, 1L))
  for (i in seq_len(nrow(ovr))) {
    if (warn && e[state_fips == ovr$state_fips[i], .N] == 0) {
      check("El ajuste manual de %s (script 02) no se aplica: el estado no esta en el Excel de E-Verify.",
            STATES[state_fips == ovr$state_fips[i], state_name])
    }
    e[state_fips == ovr$state_fips[i],
      `:=`(start_year = ovr$start_year[i], end_year = ovr$end_year[i], is_strong = ovr$is_strong[i])]
  }
  e
}
ev_flag <- function(e, col) {
  base[, (col) := 0L]
  for (i in seq_len(nrow(e))) {
    if (is.na(e$state_fips[i]) || is.na(e$start_year[i]) || e$is_strong[i] != 1L) next
    base[state_fips == e$state_fips[i] & year >= e$start_year[i] & year <= e$end_year[i],
         (col) := 1L]
  }
  invisible(NULL)
}
ev_flag(apply_ev_overrides(ev), "everify_strong_02")   # codificacion del 02 (replica)

# [E15] Anios de E-Verify tomados de las leyes, para los 10 estados que NCSL
# clasifica como "All or Most Employers". Anio natural de entrada en vigor.
#   estricta   E-Verify obligatorio, sin alternativa, para todos los empleadores
#              privados o para los de 50 o mas (o un umbral menor); NA = nunca
#   amplia     cualquier obligacion de verificar a esos empleadores, aunque
#              admita alternativas
#   fin_amplia ultimo anio con la obligacion en la regla amplia (NA = vigente)
EV_LAW <- data.table(
  state_fips = c(1L, 4L, 12L, 13L, 22L, 28L, 37L, 45L, 47L, 49L),
  estricta   = c(2012L, 2008L, 2023L, 2013L, NA, 2010L, 2013L, 2012L, 2017L, NA),
  amplia     = c(2012L, 2008L, 2021L, 2013L, 2011L, 2010L, 2013L, 2010L, 2013L, 2010L),
  fin_amplia = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 2021L),
  ley = c(
    "Alabama: HB 56 (2011), E-Verify para todos los empleadores desde el 1-IV-2012.",
    "Arizona: Legal Arizona Workers Act (2007), E-Verify para todos desde el 1-I-2008.",
    "Florida: SB 664 (2020), E-Verify o copias del I-9 desde el 1-I-2021; SB 1718 (2023), E-Verify para 25+ desde el 1-VII-2023.",
    "Georgia: HB 87 (2011), E-Verify para 500+ desde el 1-I-2012, 100+ desde el 1-VII-2012 y 11+ desde el 1-VII-2013.",
    "Louisiana: Act 402 (2011), E-Verify o copias de documentos desde el 15-VIII-2011.",
    "Mississippi: Employment Protection Act (2008), E-Verify para 250+ desde 2008, 100+ desde 2009, 30+ desde el 1-VII-2010 y todos desde 2011.",
    "North Carolina: HB 36 (2011), E-Verify para 500+ desde el 1-X-2012, 100+ desde el 1-I-2013 y 25+ desde el 1-VII-2013.",
    "South Carolina: ley de 2008, E-Verify o licencia de conducir para todos desde el 1-I-2010; S.B. 20 (2011), E-Verify para todos desde el 1-I-2012.",
    "Tennessee: HB 1378 (2011), E-Verify o un documento para 6+ desde el 1-I-2013; E-Verify para 50+ desde el 1-I-2017 y 35+ desde el 1-I-2023.",
    "Utah: S.B. 251 (2010), E-Verify u otro sistema (incluido el SSNVS) para 15+ desde el 1-VII-2010; H.B. 252 (2022), 150+ desde el 4-V-2022."))
if (EV_RULE == "excel") {
  base[, everify_strong := everify_strong_02]
} else {
  base[, everify_strong := 0L]
  for (i in seq_len(nrow(EV_LAW))) {
    a <- EV_LAW[[EV_RULE]][i]
    if (is.na(a)) next
    b <- if (EV_RULE == "amplia" && !is.na(EV_LAW$fin_amplia[i])) EV_LAW$fin_amplia[i] else YEAR_END
    base[state_fips == EV_LAW$state_fips[i] & year >= a & year <= b, everify_strong := 1L]
  }
  strong_xl <- ev[is_strong == 1L & !is.na(state_fips), unique(state_fips)]
  no_coinc <- union(setdiff(EV_LAW$state_fips, strong_xl), setdiff(strong_xl, EV_LAW$state_fips))
  if (length(no_coinc) > 0) {
    check("E-Verify [E15]: la tabla de leyes y la categoria 'All or Most Employers' del Excel no coinciden en: %s.",
          paste(STATES[state_fips %in% no_coinc, state_name], collapse = ", "))
  }
}
EV_RULE_TXT <- switch(EV_RULE,
                      estricta = "regla estricta (E-Verify obligatorio, sin alternativa, para todos los empleadores privados o para los de 50 o mas)",
                      amplia   = "regla amplia (cualquier obligacion de verificar, aunque admita alternativas)",
                      excel    = "anios del Excel de NCSL con los ajustes del script 02 (como la v3.3)")
say("  E-Verify [E15]: %s. Estado-anios distintos de la codificacion del script 02: %d.", EV_RULE_TXT,
    base[everify_strong != everify_strong_02, .N])
EV_STATES <- STATES[state_fips %in% base[everify_strong == 1L, unique(state_fips)], state_name]
say("  E-Verify universal: %d estados alguna vez en el panel (%s)", length(EV_STATES),
    paste(EV_STATES, collapse = ", "))

# ---------------------------------------------------------------------------
# 4.2 Licencias de conducir y matricula estatal (inclusivas)
# ---------------------------------------------------------------------------
f_dl <- find_file("drivers licence access.xlsx", "driver.*\\.xlsx$", "licencias de conducir")
# [E16] El 02 leia A1:C60 con los estados en la columna A. Se busca la columna
# con mas nombres de estado; las dos siguientes son 1/0 y el anio. Se lee hasta
# el primer enlace (debajo esta la tabla de NCSL, que no se usa).
dl_raw <- as.data.table(suppressMessages(read_excel(f_dl, sheet = pick_sheet(f_dl),
                                                    col_names = FALSE, col_types = "text")))
dl_hits <- vapply(dl_raw, function(v) sum(!is.na(lookup_fips(v))), integer(1))
dl_j <- which.max(dl_hits)
if (length(dl_j) == 0 || dl_hits[dl_j] < 40 || dl_j + 2L > ncol(dl_raw)) {
  stop("Licencias [E16]: no encuentro en ", basename(f_dl),
       " una columna con los estados seguida de 1/0 y del anio.")
}
dl_col <- dl_raw[[dl_j]]
dl_ini <- which(!is.na(lookup_fips(dl_col)))[1]                   # primera fila con un estado
dl_end <- which(grepl("^https?", dl_col))[1]
dl_end <- if (is.na(dl_end) || dl_end < dl_ini) length(dl_col) else dl_end - 1L
dl_rows <- dl_ini:dl_end
dl <- data.table(Estado = dl_col[dl_rows], Ofrece = dl_raw[[dl_j + 1L]][dl_rows],
                 AnioDesde = dl_raw[[dl_j + 2L]][dl_rows])
dl <- dl[!is.na(Estado) & Estado != ""]
dl[, state_fips := lookup_fips(Estado)]
if (dl[is.na(state_fips), .N] > 0) {
  check("Licencias: filas con un nombre de estado no reconocido (se ignoran, como en el 02): %s",
        paste(dl[is.na(state_fips), Estado], collapse = "; "))
}
dl[, ofrece_int := suppressWarnings(as.integer(as.character(Ofrece)))]
dl[, start_year := extract_first_year(AnioDesde)]
dl[ofrece_int == 1L & is.na(start_year), start_year := YEAR_START - 1L]
base[, drivers_licence := 0L]
for (i in seq_len(nrow(dl))) {
  if (is.na(dl$state_fips[i]) || is.na(dl$ofrece_int[i]) || dl$ofrece_int[i] != 1L ||
      is.na(dl$start_year[i])) next
  base[state_fips == dl$state_fips[i] & year >= dl$start_year[i], drivers_licence := 1L]
}
say("  Licencias de conducir: %d estados en %d",
    uniqueN(base[year == YEAR_END & drivers_licence == 1L, state_fips]), YEAR_END)

f_ist <- find_file("in-state tuiton access.xlsx", "tuit.*\\.xlsx$", "in-state tuition")
ist <- as.data.table(read_excel(f_ist, sheet = pick_sheet(f_ist), range = "A1:D60"))
setnames(ist, c("Estado", "Categoria", "Anios", "Estudiantes"))
ist <- ist[!is.na(Estado) & Estado != "" & !grepl("^https?", Estado) & nchar(Estado) < 50]
ist[, state_fips := lookup_fips(Estado)]
if (ist[is.na(state_fips), .N] > 0) {
  check("Matricula: filas con un nombre de estado no reconocido (se ignoran, como en el 02): %s",
        paste(ist[is.na(state_fips), Estado], collapse = "; "))
}
ist <- ist[!is.na(state_fips)]
ist[, is_inclusive := as.integer(trimws(as.character(Categoria)) %in%
                                   c("Comprehensive Access", "Accessible"))]
ist[, start_year := extract_first_year(Anios)]
ist[is_inclusive == 1L & is.na(start_year), start_year := YEAR_START - 1L]
say("  Categorias de matricula en el Excel (solo 'Comprehensive Access' y 'Accessible' cuentan):")
print(ist[, .N, by = .(Categoria, is_inclusive)])
base[, in_state_tuition := 0L]
for (i in seq_len(nrow(ist))) {
  if (ist$is_inclusive[i] != 1L || is.na(ist$start_year[i])) next
  base[state_fips == ist$state_fips[i] & year >= ist$start_year[i], in_state_tuition := 1L]
}
say("  Matricula estatal inclusiva: %d estados en %d",
    uniqueN(base[year == YEAR_END & in_state_tuition == 1L, state_fips]), YEAR_END)

# [E16] Salvaguarda: un componente que vale 0 en todos los estado-anios indica
# que su Excel no se ha leido bien (cambia el indice y todas las cifras).
for (cc in c("everify_strong", "drivers_licence", "in_state_tuition")) {
  if (base[, all(get(cc) == 0L)]) {
    stop("El componente ", cc, " vale 0 en todos los estado-anios: revisa su Excel en DATA_PATH [E16].")
  }
}

# ---------------------------------------------------------------------------
# 4.3 Paro estatal (51 archivos BLS). Lectura del script 02: cabecera en la
#     fila 1 con una columna "Year" y columnas de meses; coma decimal -> punto.
#     Si la cabecera no esta en la fila 1, se busca la fila "Year" (y se avisa).
# ---------------------------------------------------------------------------
MONTHS <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
read_unemp_file <- function(f) {
  df <- tryCatch(suppressMessages(as.data.table(read_excel(f, sheet = 1))), error = function(e) NULL)
  if (is.null(df)) return(list(u = NULL, how = "no se pudo abrir"))
  setnames(df, tolower(trimws(names(df))))
  how <- "fila 1"
  if (!"year" %in% names(df)) {
    raw <- suppressMessages(as.data.table(read_excel(f, sheet = 1, col_names = FALSE)))
    is_hdr <- apply(raw, 1, function(r) any(tolower(trimws(as.character(r))) == "year", na.rm = TRUE))
    hdr <- which(is_hdr)[1]
    if (is.na(hdr) || hdr >= nrow(raw)) return(list(u = NULL, how = "sin columna Year"))
    nm <- tolower(trimws(as.character(unlist(raw[hdr]))))
    nm[is.na(nm) | nm == ""] <- paste0("v", which(is.na(nm) | nm == ""))
    df <- raw[(hdr + 1):nrow(raw)]
    setnames(df, make.unique(nm))
    how <- sprintf("fila %d", hdr)
  }
  months <- intersect(MONTHS, names(df))
  if (length(months) == 0) return(list(u = NULL, how = "sin columnas de meses"))
  out <- data.table(year = suppressWarnings(as.integer(as.character(df[["year"]]))))
  for (mc in months) {
    v <- df[[mc]]
    if (is.character(v)) v <- gsub(",", ".", v)
    out[, (mc) := suppressWarnings(as.numeric(v))]
  }
  out <- out[!is.na(year)]
  out[, unemp_rate_annual := rowMeans(.SD, na.rm = TRUE), .SDcols = months]
  out[is.nan(unemp_rate_annual), unemp_rate_annual := NA_real_]
  out[, unemp_rate_march := if ("mar" %in% months) mar else NA_real_]
  list(u = out[, .(year, unemp_rate_annual, unemp_rate_march)], how = how)
}
files_u <- list.files(DATA_PATH, pattern = "unemployment\\.xlsx$", ignore.case = TRUE, full.names = TRUE)
files_u <- files_u[!grepl("^~\\$", basename(files_u))]
unemp_list <- list(); u_hdr_moved <- character(0); u_failed <- character(0)
for (f in files_u) {
  st <- sub("(?i)[ _-]*unemployment\\.xlsx$", "", basename(f), perl = TRUE)
  fs <- lookup_fips(st)
  if (is.na(fs)) { u_failed <- c(u_failed, basename(f)); next }
  r <- read_unemp_file(f)
  if (is.null(r$u)) { u_failed <- c(u_failed, sprintf("%s (%s)", basename(f), r$how)); next }
  if (r$how != "fila 1") u_hdr_moved <- c(u_hdr_moved, sprintf("%s (cabecera en %s)", basename(f), r$how))
  r$u[, state_fips := fs]
  unemp_list[[length(unemp_list) + 1]] <- r$u
}
if (length(unemp_list) == 0) stop("No se pudo leer ningun archivo de paro en DATA_PATH.")
unemp <- rbindlist(unemp_list, use.names = TRUE)
dup_u <- unemp[, .N, by = .(state_fips, year)][N > 1]
if (nrow(dup_u) > 0) {                                                       # [E9]
  stop("Hay estado-anios repetidos en los archivos de paro (p. ej., ",
       paste(head(merge(dup_u, STATES, by = "state_fips")[, paste(state_name, year)], 5), collapse = "; "),
       "). Revisa que no haya dos archivos del mismo estado.")
}
if (length(u_hdr_moved) > 0) {
  check("Archivos de paro con la cabecera fuera de la fila 1 (el script 02 los habria saltado): %s",
        paste(u_hdr_moved, collapse = "; "))
}
if (length(u_failed) > 0) check("Archivos de paro no leidos: %s", paste(u_failed, collapse = "; "))
miss_u <- setdiff(STATES$state_fips, unemp$state_fips)
if (length(miss_u) > 0) {
  check("Estados sin archivo de paro (sus observaciones caen en las especificaciones con paro): %s",
        paste(STATES[state_fips %in% miss_u, state_name], collapse = ", "))
}
# [E5] retardo en el panel: paro anual del anio anterior
unemp_lag <- unemp[, .(state_fips, year = year + 1L, unemp_annual_lag1 = unemp_rate_annual)]
base <- merge(base, unemp[, .(state_fips, year, unemp_rate_annual, unemp_rate_march)],
              by = c("state_fips", "year"), all.x = TRUE)
base <- merge(base, unemp_lag, by = c("state_fips", "year"), all.x = TRUE)
say("  Paro: %d archivos leidos; estado-anios sin paro de marzo: %d; sin paro anual de t-1: %d",
    length(unemp_list), base[is.na(unemp_rate_march), .N], base[is.na(unemp_annual_lag1), .N])
if (base[year == YEAR_START & is.na(unemp_annual_lag1), .N] > 0) {
  say("  (los archivos no traen %d: %d sale sin paro retardado en los resultados del anio pasado)",
      YEAR_START - 1L, YEAR_START)
}

# ---------------------------------------------------------------------------
# 4.4 TRAC: deportaciones ICE y bajo Secure Communities (script 02)
# ---------------------------------------------------------------------------
read_trac <- function(f) {
  x <- as.data.table(read_excel(f, sheet = 1))
  setnames(x, 1, "Estado")
  x[, Estado := as.character(Estado)]
  x <- x[!is.na(Estado) & Estado != ""]
  x[, state_fips := lookup_fips(Estado)]
  ignored <- unique(x[is.na(state_fips), Estado])
  x <- x[!is.na(state_fips)]
  if (anyDuplicated(x$state_fips)) {                                         # [E9]
    stop("En ", basename(f), " hay estados repetidos: ",
         paste(unique(x$Estado[duplicated(x$state_fips)]), collapse = ", "))
  }
  nm <- trimws(names(x))
  ycols <- names(x)[grepl("^[0-9]{4}$", nm)]
  how <- "AAAA"
  if (length(ycols) == 0) {
    ycols <- names(x)[grepl("^(FY)?\\s*[0-9]{4}$", nm, ignore.case = TRUE)]
    how <- "FY AAAA"
  }
  if (length(ycols) == 0) stop("No encuentro columnas de anios en ", basename(f))
  x[, (ycols) := lapply(.SD, as.character), .SDcols = ycols]
  long <- melt(x[, c("state_fips", ycols), with = FALSE], id.vars = "state_fips",
               measure.vars = ycols, variable.name = "ycol", value.name = "v_raw",
               variable.factor = FALSE)
  long[, v := suppressWarnings(as.numeric(v_raw))]
  n_bad <- long[!is.na(v_raw) & trimws(v_raw) != "" & is.na(v), .N]
  long[, year := as.integer(sub(".*([0-9]{4})$", "\\1", trimws(ycol)))]
  yr <- range(long$year)
  long <- long[year >= YEAR_START & year <= YEAR_END]
  say("  %s: %d estados, columnas %s de %d a %d; filas ignoradas: %s", basename(f),
      uniqueN(long$state_fips), how, yr[1], yr[2],
      if (length(ignored) == 0) "ninguna" else paste(ignored, collapse = ", "))
  if (n_bad > 0) {
    check("%s: %d celdas con texto que no se puede leer como numero (cuentan como vacias).",
          basename(f), n_bad)
  }
  long[, .(state_fips, year, v)]
}
f_rem <- find_file("ICE_Removals_Immigration_and_Customs_Enforcement_Removals.xlsx",
                   "ICE.*Removals.*\\.xlsx$", "TRAC ICE Removals")
f_sc  <- find_file("Removals_under_the_Secure_Communities_Program.xlsx",
                   "Secure.*Communit.*\\.xlsx$", "TRAC Secure Communities")
rem <- read_trac(f_rem); setnames(rem, "v", "deportation_total")
sc  <- read_trac(f_sc);  setnames(sc,  "v", "sc_removals")
# Script 02: celdas vacias de ICE = 0; estados o anios que no estan en el
# archivo quedan sin dato (NA).
rem[is.na(deportation_total), deportation_total := 0]
base <- merge(base, rem, by = c("state_fips", "year"), all.x = TRUE)
base <- merge(base, sc,  by = c("state_fips", "year"), all.x = TRUE)
# Script 02: Secure Communities = 0 desde 2021 cuando no hay dato.
n_sc_post_pos <- base[year >= 2021L & !is.na(sc_removals) & sc_removals > 0, .N]
base[year >= 2021L & is.na(sc_removals), sc_removals := 0]
if (n_sc_post_pos > 0) {
  check("El Excel de Secure Communities trae %d estado-anios con valores > 0 desde 2021. El script 02 solo ponia 0 en las celdas vacias (se mantiene); el manuscrito dice que el componente vale 0 desde 2021.",
        n_sc_post_pos)
}

# Anio de adopcion de E-Verify universal (primer anio del panel con 1; -1 = nunca)
ev_year <- base[everify_strong == 1L, .(everify_year = min(year)), by = state_fips]
base <- merge(base, ev_year, by = "state_fips", all.x = TRUE)
base[is.na(everify_year), everify_year := -1L]

# ---------------------------------------------------------------------------
# 4.5 Indice (script 02): restrictivas - inclusivas, reescalado a [0, 1].
#     Funcion porque se usa tambien con la codificacion antigua (replica).
# ---------------------------------------------------------------------------
pca_index <- function(p, cols) {
  X <- as.matrix(p[, cols, with = FALSE])
  ok <- complete.cases(X)
  keep <- cols[apply(X[ok, , drop = FALSE], 2, function(v) sd(v) > 0)]
  res <- list(index = rep(NA_real_, nrow(p)), loadings = NULL, var1 = NA_real_, flipped = NA)
  if (length(keep) < 2) return(res)
  pr <- prcomp(X[ok, keep, drop = FALSE], scale. = TRUE)
  ld <- pr$rotation[, 1]
  restr <- intersect(c("everify_strong", "dep_norm", "sc_norm"), keep)
  flip <- length(restr) > 0 && mean(ld[restr]) <= 0     # como el script 02
  sc_ <- rep(NA_real_, nrow(p))
  sc_[ok] <- (if (flip) -1 else 1) * pr$x[, 1]
  res$index <- rescale01(sc_)
  res$loadings <- (if (flip) -1 else 1) * ld
  res$var1 <- summary(pr)$importance[2, 1]
  res$flipped <- flip
  res
}

# Componentes de cada version del indice [E11]
COMP_MS5 <- c("everify_strong", "dep_norm", "sc_norm", "drivers_licence", "in_state_tuition")
COMP_V4  <- c("everify_strong", "sc_norm", "drivers_licence", "in_state_tuition")
if (!INDEX_MAIN %in% c("v4", "ms5")) stop("INDEX_MAIN debe ser \"v4\" o \"ms5\".")

build_panel <- function(undoc_pop, nc_pop, winsor_p = NA, sc_na_zero = SC_NA_AS_ZERO,
                        main = INDEX_MAIN, sc_log = SC_LOG, ev02 = FALSE) {
  p <- merge(copy(base), undoc_pop, by = c("state_fips", "year"), all.x = TRUE)
  if (ev02) p[, everify_strong := everify_strong_02]   # [E15] E-Verify como en el 02 (replica)
  p <- merge(p, nc_pop, by = "state_fips", all.x = TRUE)
  if (sc_na_zero) p[is.na(sc_removals), sc_removals := 0]
  # Tasas por 1.000 indocumentados imputados (manuscrito enviado)
  p[, deportation_rate := fifelse(!is.na(undoc_population) & undoc_population > 0 &
                                    !is.na(deportation_total),
                                  1000 * deportation_total / undoc_population, NA_real_)]
  p[, sc_rate := fifelse(!is.na(undoc_population) & undoc_population > 0 & !is.na(sc_removals),
                         1000 * sc_removals / undoc_population, NA_real_)]
  p[, dep_norm    := rescale01(winsor_up(deportation_rate, winsor_p))]
  p[, sc_norm_ms5 := rescale01(winsor_up(sc_rate, winsor_p))]
  # [E11] Secure Communities por 1.000 no ciudadanos (media del periodo del estado)
  p[, sc_rate_nc := fifelse(!is.na(nc_population) & nc_population > 0 & !is.na(sc_removals),
                            1000 * sc_removals / nc_population, NA_real_)]
  p[, sc_norm_v4      := rescale01(log1p(winsor_up(sc_rate_nc, winsor_p)))]
  p[, sc_norm_v4_nolog := rescale01(winsor_up(sc_rate_nc, winsor_p))]
  # Versiones del indice (todas reescaladas a [0, 1])
  p[, enforce_ms5 := rescale01(everify_strong + dep_norm + sc_norm_ms5 - drivers_licence - in_state_tuition)]
  p[, enforce_v4_log := rescale01(everify_strong + sc_norm_v4 - drivers_licence - in_state_tuition)]
  p[, enforce_v4_nolog := rescale01(everify_strong + sc_norm_v4_nolog - drivers_licence - in_state_tuition)]
  p[, enforce_dejure := rescale01(everify_strong - drivers_licence - in_state_tuition)]
  # Indice principal y su componente de Secure Communities
  if (main == "ms5") {
    p[, `:=`(sc_norm = sc_norm_ms5, enforce_index = enforce_ms5)]
  } else if (sc_log) {
    p[, `:=`(sc_norm = sc_norm_v4, enforce_index = enforce_v4_log)]
  } else {
    p[, `:=`(sc_norm = sc_norm_v4_nolog, enforce_index = enforce_v4_nolog)]
  }
  pc <- pca_index(p, if (main == "ms5") COMP_MS5 else COMP_V4)
  p[, enforce_pca := pc$index]
  setorder(p, state_fips, year)
  attr(p, "pca") <- pc
  p
}

# Poblacion indocumentada estado-anio: todos los nacidos fuera indocumentados
# de 20-64 anios, ambos sexos, ponderados (script 02, B9).
undoc_pop <- dt[undoc == 1L, .(undoc_population = sum(ASECWT, na.rm = TRUE)), by = .(YEAR, STATEFIP)]
setnames(undoc_pop, c("YEAR", "STATEFIP"), c("year", "state_fips"))
# [E11] Poblacion no ciudadana (CITIZEN 5) de 20-64 anios, ponderada: media de los
# totales anuales de cada estado en el periodo (denominador de Secure Communities).
nc_sy <- dt[, .(nc = sum(ASECWT * (CITIZEN == 5L), na.rm = TRUE)), by = .(YEAR, STATEFIP)]
nc_pop <- nc_sy[, .(nc_population = mean(nc), nc_years = .N), by = STATEFIP]
setnames(nc_pop, "STATEFIP", "state_fips")
if (nc_pop[nc_years < length(YEAR_START:YEAR_END), .N] > 0) {
  check("Poblacion no ciudadana: %d estados sin observaciones en algun anio (la media usa los anios disponibles).",
        nc_pop[nc_years < length(YEAR_START:YEAR_END), .N])
}
nc_pop[, nc_years := NULL]
rm(nc_sy)
N_SC_NA_PRE2021 <- base[year < 2021L & is.na(sc_removals), .N]
panel <- build_panel(undoc_pop, nc_pop)
PCA <- attr(panel, "pca")
panel <- merge(panel, STATES, by = "state_fips", all.x = TRUE)

# Variantes (ADICIONAL): tasas winsorizadas y Phi de t-1 y t+1
pw <- build_panel(undoc_pop, nc_pop, winsor_p = WINSOR_P_EXTRA)
panel <- merge(panel, pw[, .(state_fips, year, enforce_index_w = enforce_index)],
               by = c("state_fips", "year"), all.x = TRUE)
panel <- merge(panel, panel[, .(state_fips, year = year + 1L, enforce_index_lag1 = enforce_index)],
               by = c("state_fips", "year"), all.x = TRUE)
panel <- merge(panel, panel[, .(state_fips, year = year - 1L, enforce_index_lead1 = enforce_index)],
               by = c("state_fips", "year"), all.x = TRUE)
rm(pw)

N_NA_IDX <- panel[is.na(enforce_index), .N]
if (N_NA_IDX > 0) {
  why <- panel[is.na(enforce_index), .(
    sin_pob_indoc = sum(is.na(undoc_population) | undoc_population == 0),
    sin_ICE = sum(is.na(deportation_total)), sin_SC = sum(is.na(sc_removals)))]
  if (INDEX_MAIN == "ms5") {
    check("%d estado-anios sin indice (quedan fuera de las regresiones): sin poblacion indocumentada %d, sin dato ICE %d, sin dato Secure Communities %d. Lista en A3_state_panel.csv.",
          N_NA_IDX, why$sin_pob_indoc, why$sin_ICE, why$sin_SC)
  } else {
    check("%d estado-anios sin indice (quedan fuera de las regresiones): sin dato Secure Communities %d. Lista en A3_state_panel.csv.",
          N_NA_IDX, why$sin_SC)
  }
}
fwrite(panel, file.path(TAB_DIR, "A3_state_panel.csv"))
COMP <- if (INDEX_MAIN == "ms5") COMP_MS5 else COMP_V4
fwrite(as.data.table(suppressWarnings(cor(panel[, COMP, with = FALSE], use = "complete.obs")),
                     keep.rownames = "component"),
       file.path(TAB_DIR, "A3b_component_correlations.csv"))

# Unir el panel a los microdatos
pcols <- unique(c("state_fips", "year", "enforce_index", "enforce_pca", "enforce_index_w",
                  "enforce_index_lag1", "enforce_index_lead1", COMP, "enforce_ms5",
                  "enforce_v4_log", "enforce_v4_nolog", "enforce_dejure", "unemp_rate_march",
                  "unemp_annual_lag1", "everify_year"))
pm <- panel[, pcols, with = FALSE]
setnames(pm, c("state_fips", "year"), c("STATEFIP", "YEAR"))
dt <- merge(dt, pm, by = c("STATEFIP", "YEAR"), all.x = TRUE)


# ---------------------------------------------------------------------------
# 4.6 Comprobacion de replica: manuscrito enviado con la codificacion antigua
#     (NATIVITY 4/5, CITIZEN 1/2, YRIMMIG como anios, HISPAN > 0) y la
#     ecuacion sin Phi_st. Si sale 0.0447, los datos leidos son los mismos.
# ---------------------------------------------------------------------------
REPL <- NULL
if (RUN_REPLICATION_CHECK) {
  section("4.6 COMPROBACION DE REPLICA (codificacion del manuscrito enviado)")
  # Datos tal como los uso el manuscrito (con las dos submuestras de 2014)
  old <- if (!is.null(OLD_RAW)) OLD_RAW else
    dt[, .(STATEFIP, YEAR, ASECWT, AGE, SEX, MARST, METRO, EDUC, CLASSWKR, NATIVITY, CITIZEN,
           HISPAN, YRIMMIG, INCSS, INCRETIR, HIMCAIDLY, EMPSTAT, PUBHOUS, RENTSUB, BPL, OCC50LY)]
  OLD_RAW <- NULL
  old[, educ_years := unname(EDUC_MAP[as.character(EDUC)])]
  old[, `:=`(age_sq = AGE^2, married = as.integer(MARST %in% c(1L, 2L)),
             in_metro = as.integer(METRO %in% c(2L, 3L, 4L)), male = as.integer(SEX == 1L),
             college = as.integer(!is.na(educ_years) & educ_years >= 16),
             se_uninc = as.integer(CLASSWKR == 13L))]
  old[, fb_old := as.integer(NATIVITY %in% c(4L, 5L))]
  old[, hisp_old := as.integer(HISPAN > 0L)]
  old[, legal_old := 0L]
  old[fb_old == 1L & YRIMMIG > 0L & YRIMMIG >= (YEAR - 1980L), legal_old := 1L]
  old[fb_old == 1L & CITIZEN %in% c(1L, 2L), legal_old := 1L]
  old[fb_old == 1L & ((INCSS > 0 & !is.na(INCSS)) | (INCRETIR > 0 & !is.na(INCRETIR)) |
                        (HIMCAIDLY == 2L & !is.na(HIMCAIDLY))), legal_old := 1L]
  old[fb_old == 1L & (EMPSTAT == 1L | CLASSWKR == 26L), legal_old := 1L]
  old[fb_old == 1L & CLASSWKR %in% c(25L, 27L, 28L), legal_old := 1L]
  old[fb_old == 1L & ((PUBHOUS == 2L & !is.na(PUBHOUS)) | (RENTSUB == 2L & !is.na(RENTSUB))),
      legal_old := 1L]
  old[fb_old == 1L & BPL == 25000L, legal_old := 1L]
  old[fb_old == 1L & OCC50LY %in% LIC_OCCS, legal_old := 1L]
  old[fb_old == 1L & OCC50LY %in% H1B_OCCS & YRIMMIG > 0L & YRIMMIG <= 6L & college == 1L,
      legal_old := 1L]
  up_old <- old[fb_old == 1L & legal_old == 0L, .(undoc_population = sum(ASECWT, na.rm = TRUE)),
                by = .(YEAR, STATEFIP)]
  setnames(up_old, c("YEAR", "STATEFIP"), c("year", "state_fips"))
  panel_old <- build_panel(up_old, nc_pop, sc_na_zero = FALSE, main = "ms5", ev02 = TRUE)   # indice del manuscrito
  old <- merge(old, panel_old[, .(STATEFIP = state_fips, YEAR = year, idx_old = enforce_index,
                                  unemp_old = unemp_rate_march)],
               by = c("STATEFIP", "YEAR"), all.x = TRUE)
  old[, trt_old  := as.integer(hisp_old == 1L & fb_old == 1L & male == 1L)]
  old[, ctrl_old := as.integer(fb_old == 0L & hisp_old == 0L & !is.na(educ_years) &
                                 educ_years <= 12 & male == 1L)]
  # Composicion de los "indocumentados" de la codificacion antigua
  und_old <- old[fb_old == 1L & legal_old == 0L]
  REPL <- list(b = NA_real_, se = NA_real_, N = NA_integer_, n_fb = old[fb_old == 1L, .N], n_doc = old[fb_old == 1L & legal_old == 1L, .N],
               n_trt = old[trt_old == 1L, .N], n_trt_doc = old[trt_old == 1L & legal_old == 1L, .N],
               n_ctrl = old[ctrl_old == 1L, .N],
               und_nat4 = und_old[, mean(NATIVITY == 4L)], und_cit4 = und_old[, mean(CITIZEN == 4L)],
               und_cit5 = und_old[, mean(CITIZEN == 5L)])
  an_old <- old[(trt_old == 1L | ctrl_old == 1L) & !is.na(educ_years) & !is.na(married) &
                  !is.na(AGE) & !is.na(idx_old)]
  an_old[, trt_x_index := trt_old * idx_old]
  m_old <- tryCatch(
    feols(se_uninc ~ trt_old + trt_x_index + AGE + age_sq + educ_years + married + in_metro +
            unemp_old | STATEFIP + YEAR, data = an_old, weights = ~ASECWT, cluster = ~STATEFIP),
    error = function(e) NULL)
  if (!is.null(m_old) && "trt_x_index" %in% names(coef(m_old))) {
    ct <- coeftable(m_old)
    REPL$b <- ct["trt_x_index", 1]; REPL$se <- ct["trt_x_index", 2]; REPL$N <- nobs(m_old)
  }
  rm(old, an_old, panel_old, up_old, und_old); invisible(gc(verbose = FALSE))
  say("  Codificacion antigua: beta = %s (SE %s), N = %s", f4(REPL$b), f4(REPL$se), fN(REPL$N))
  say("  Manuscrito enviado:   beta = 0.0447 (SE 0.0110), N = 298,648")
}

# Columnas que ya no hacen falta
drop_raw <- intersect(c("SERIAL", "PERNUM", "HISPAN", "YRIMMIG", "INCSS", "INCRETIR",
                        "HIMCAIDLY", "PUBHOUS", "RENTSUB", "OCC50LY", "IND1990", "WKSWORK1",
                        "UHRSWORKLY", "FULLPART", "INCLONGJ", "MARST", "METRO", "EDUC", "BPL",
                        "WORKLY"), names(dt))
dt[, (drop_raw) := NULL]
invisible(gc(verbose = FALSE))


# ============================================================================
# 5. MOTOR DE ESTIMACION
# ============================================================================
CONTROLS <- c("AGE", "age_sq", "educ_years", "married", "in_metro")

# Efectos fijos de cada especificacion. "paper" y "phi" tienen EF de estado y
# de anio; el termino gamma*T_i del manuscrito entra como EF de grupo (grp),
# que es lo mismo (una constante por grupo).
fe_string <- function(spec) {
  switch(spec,
         paper = "STATEFIP + YEAR + grp",
         phi   = "STATEFIP + YEAR + grp",
         sxy   = "STATEFIP^YEAR + grp",
         full  = "STATEFIP^YEAR + grp^STATEFIP + grp^YEAR",
         stop("Especificacion desconocida: ", spec))
}
tname <- function(g, ix = "enforce_index") paste0("TX__", g, "__", ix)

# DiD con intensidad continua. d lleva la columna grp con el grupo de
# referencia "control". Para cada grupo g != control y cada indice ix crea
# 1[grp == g] * ix (la interaccion T x Phi del manuscrito).
#   paper: + controles + paro           | estado + anio + grupo
#   phi:   + controles + paro + Phi_st  | estado + anio + grupo      [E4]
#   sxy:   + controles                  | estado x anio + grupo
#   full:  + controles                  | estado x anio + grupo x estado + grupo x anio
est_did <- function(d, y, idx = "enforce_index", spec = MAIN_SPEC, unemp = "unemp_rate_march",
                    ref = "control", controls = CONTROLS, quiet = FALSE, keep_data = FALSE,
                    require = "enforce_index") {
  need <- unique(c(y, idx, require, controls, "grp", "STATEFIP", "YEAR", "ASECWT",
                   if (spec %in% c("paper", "phi")) unemp))
  miss <- setdiff(need, names(d))
  if (length(miss) > 0) stop("Faltan columnas: ", paste(miss, collapse = ", "))
  dd <- d[, need, with = FALSE]
  dd <- dd[complete.cases(dd)]
  if (nrow(dd) == 0 || !(ref %in% dd$grp)) {
    if (!quiet) say("    [AVISO] %s: muestra vacia o sin grupo de referencia", y)
    return(NULL)
  }
  levs <- setdiff(sort(unique(dd$grp)), ref)
  if (length(levs) == 0) return(NULL)
  terms <- character(0); tg <- character(0); ti <- character(0)
  for (g in levs) for (ix in idx) {
    tn <- tname(g, ix)
    dd[, (tn) := as.numeric(grp == g) * get(ix)]
    terms <- c(terms, tn); tg <- c(tg, g); ti <- c(ti, ix)
  }
  rhs <- c(terms, controls)
  if (spec %in% c("paper", "phi")) rhs <- c(rhs, unemp)
  if (spec == "phi") rhs <- c(rhs, idx)
  fml <- as.formula(paste(y, "~", paste(rhs, collapse = " + "), "|", fe_string(spec)))
  m <- tryCatch(feols(fml, data = dd, weights = ~ASECWT, cluster = ~STATEFIP),
                error = function(e) {
                  if (!quiet) say("    [AVISO] %s no estimable: %s", y, conditionMessage(e))
                  NULL
                })
  if (is.null(m)) return(NULL)
  ct <- coeftable(m)
  est <- rep(NA_real_, length(terms)); se <- est; pv <- est
  hit <- terms %in% rownames(ct)
  est[hit] <- ct[terms[hit], 1]; se[hit] <- ct[terms[hit], 2]; pv[hit] <- ct[terms[hit], 4]
  if (any(!hit) && !quiet) {
    say("    [AVISO] %s: terminos eliminados por colinealidad: %s", y, paste(terms[!hit], collapse = ", "))
  }
  means <- dd[, .(mean_y = sum(get(y) * ASECWT) / sum(ASECWT), n = .N), by = grp]
  list(model = m, y = y, spec = spec, N = nobs(m), G = uniqueN(dd$STATEFIP),
       coefs = data.table(term = terms, group = tg, idx = ti, estimate = est, se = se, p = pv),
       means = means, rhs = rhs, data = if (keep_data) dd else NULL)
}

cf <- function(res, g, ix = "enforce_index") {
  if (is.null(res)) return(list(est = NA_real_, se = NA_real_, p = NA_real_))
  r <- res$coefs[term == tname(g, ix)]
  if (nrow(r) == 0) return(list(est = NA_real_, se = NA_real_, p = NA_real_))
  list(est = r$estimate, se = r$se, p = r$p)
}

# Diferencia entre dos coeficientes del mismo modelo (covarianza incluida)
diff_test <- function(res, ga, gb, ix = "enforce_index") {
  out <- list(est = NA_real_, se = NA_real_, p = NA_real_)
  if (is.null(res)) return(out)
  b <- coef(res$model); V <- vcov(res$model)
  a <- tname(ga, ix); c_ <- tname(gb, ix)
  if (!all(c(a, c_) %in% names(b))) return(out)
  est <- unname(b[a] - b[c_])
  se <- sqrt(V[a, a] + V[c_, c_] - 2 * V[a, c_])
  list(est = est, se = se, p = 2 * pt(-abs(est / se), df = res$G - 1))
}

# Wald: H0 todos los coeficientes = 0
wald_zero <- function(b, V, G) {
  k <- length(b)
  if (k < 1) return(list(F = NA_real_, df1 = NA_integer_, p = NA_real_))
  W <- tryCatch(as.numeric(t(b) %*% solve(V) %*% b), error = function(e) NA_real_)
  list(F = W / k, df1 = k, p = if (is.na(W)) NA_real_ else pf(W / k, k, G - 1, lower.tail = FALSE))
}

# Wild cluster restricted bootstrap (Rademacher) de un test de Wald (H0: los
# coeficientes de `test` valen 0). Con pocos estados que identifican un
# coeficiente, los errores agrupados por estado subestiman la varianza y los
# tests rechazan de mas (MacKinnon y Webb 2017); el bootstrap corrige eso.
# Por FWL: se desconcentran una vez X, los ajustados y los residuos
# restringidos de cada estado; cada replica es algebra matricial.
#   dd: datos de estimacion (sin NA); rhs: regresores; fe_df: data.frame con
#   los efectos fijos; fe_str: los mismos en formula; b_check: coeficientes
#   de fixest para comprobar que el algebra reproduce la estimacion.
wild_wald <- function(dd, y, rhs, test, fe_df, fe_str, b_check, B = WILD_B, seed = 20261028) {
  fail <- function(why) {
    say("    [AVISO] wild bootstrap no calculado: %s", why)
    list(p = NA_real_, B = 0L, why = why)
  }
  keep <- !is.na(dd$ASECWT) & dd$ASECWT > 0
  dd <- dd[keep]; fe_df <- fe_df[keep, , drop = FALSE]
  rhs <- rhs[rhs %in% names(b_check)]
  test <- test[test %in% rhs]
  if (length(test) == 0) return(fail("los coeficientes a contrastar no estan en el modelo"))
  rhs_r <- setdiff(rhs, test)
  fml_r <- if (length(rhs_r) > 0) paste(y, "~", paste(rhs_r, collapse = " + "), "|", fe_str) else
    paste(y, "~ 1 |", fe_str)
  m_r <- tryCatch(feols(as.formula(fml_r), data = dd, weights = ~ASECWT), error = function(e) e)
  if (inherits(m_r, "error")) return(fail(paste("modelo restringido:", conditionMessage(m_r))))
  if (nobs(m_r) != nrow(dd)) {                  # fixest quito filas: se alinea todo con ellas
    used <- obs(m_r)
    dd <- dd[used]; fe_df <- fe_df[used, , drop = FALSE]
  }
  w <- dd$ASECWT
  cl <- as.integer(factor(dd$STATEFIP)); G <- max(cl)
  u_r <- resid(m_r); yhat_r <- dd[[y]] - u_r
  X <- as.matrix(dd[, rhs, with = FALSE])
  Ug <- matrix(0, nrow(dd), G); Ug[cbind(seq_len(nrow(dd)), cl)] <- u_r
  dm <- fixest::demean(cbind(X, yhat_r, yy = dd[[y]], Ug), f = fe_df, weights = w)
  k <- ncol(X)
  Xt <- dm[, 1:k, drop = FALSE]; yhat_t <- dm[, k + 1]; y_t <- dm[, k + 2]
  U_t <- dm[, (k + 3):(k + 2 + G), drop = FALSE]
  rm(dm, Ug); invisible(gc(verbose = FALSE))
  XtW <- Xt * w
  M <- crossprod(XtW, Xt)
  # Si la matriz esta mal condicionada, solve() con tolerancia baja; la
  # comprobacion de abajo (coeficientes = fixest) garantiza que el resultado vale.
  bread <- tryCatch(solve(M), error = function(e) tryCatch(solve(M, tol = 1e-30), error = function(e) NULL))
  if (is.null(bread)) return(fail("la matriz de regresores no es invertible"))
  A <- bread %*% t(XtW)
  jj <- match(test, rhs)
  b0 <- as.numeric(A %*% y_t)
  if (max(abs(b0[jj] - b_check[test])) > 1e-4 * max(1e-3, max(abs(b_check[test])))) {
    return(fail("el algebra no reproduce los coeficientes de fixest"))
  }
  # Inversa de la varianza del bloque contrastado; si es singular (pocos
  # estados detras de los coeficientes), inversa generalizada, igual en la
  # muestra original y en cada replica.
  inv_v <- function(Vb) tryCatch(solve(Vb), error = function(e) MASS::ginv(Vb))
  wstat <- function(yy) {
    b <- A %*% yy; e <- as.numeric(yy - Xt %*% b)
    S <- rowsum(XtW * e, cl); V <- bread %*% crossprod(S) %*% bread
    bt <- b[jj]
    as.numeric(t(bt) %*% inv_v(V[jj, jj, drop = FALSE]) %*% bt)
  }
  W0 <- wstat(y_t)
  if (!is.finite(W0)) return(fail("el estadistico de Wald no es finito"))
  set.seed(seed)
  Wb <- vapply(seq_len(B), function(i) wstat(yhat_t + U_t %*% sample(c(-1, 1), G, replace = TRUE)),
               numeric(1))
  list(p = mean(Wb >= W0, na.rm = TRUE), B = sum(!is.na(Wb)), why = NA_character_)
}
fe_frame <- function(dd, spec) {
  switch(spec,
         paper = , phi = data.frame(a = dd$STATEFIP, b = dd$YEAR, c = dd$grp),
         sxy  = data.frame(a = paste(dd$STATEFIP, dd$YEAR), b = dd$grp),
         full = data.frame(a = paste(dd$STATEFIP, dd$YEAR), b = paste(dd$grp, dd$STATEFIP),
                           c = paste(dd$grp, dd$YEAR)))
}

# Registro de todos los coeficientes
ALL <- list()
record <- function(res, block, sex, sample, label = NULL) {
  if (is.null(res)) return(invisible(NULL))
  x <- copy(res$coefs)
  x[, `:=`(block = block, sex = sex, sample = sample, outcome = res$y,
           label = if (is.null(label)) NA_character_ else label, spec = res$spec,
           N = res$N, G = res$G)]
  ALL[[length(ALL) + 1]] <<- x
  invisible(NULL)
}
record_test <- function(tst, block, sex, sample, outcome, term, spec = MAIN_SPEC, N = NA, G = NA) {
  ALL[[length(ALL) + 1]] <<- data.table(term = term, group = NA_character_, idx = NA_character_,
                                        estimate = tst$est, se = tst$se, p = tst$p,
                                        block = block, sex = sex, sample = sample,
                                        outcome = outcome, label = term, spec = spec,
                                        N = as.integer(N), G = as.integer(G))
  invisible(NULL)
}
line_coef <- function(label, x, N = NULL) {
  sprintf("%s: %s (SE %s), p %s%s", label, f4(x$est), f4(x$se), fp(x$p),
          if (is.null(N)) "" else paste0(", N = ", fN(N)))
}
nobs_or_null <- function(res) if (is.null(res)) NULL else res$N


# ============================================================================
# 6. MUESTRAS (por sexo; las mujeres con las mismas definiciones)
# ============================================================================
SEXES  <- c(men = 1L, women = 2L)
SEX_EN <- c(men = "Men", women = "Women")
SEX_ES <- c(men = "HOMBRES", women = "MUJERES")

# Principal (05 B0): hispanos nacidos fuera vs nativos no hispanos <= 12 anios
s_main <- function(sx) {
  d <- dt[SEX == sx & (g_hfb == 1L | g_ctrl == 1L)]
  d[, grp := fifelse(g_hfb == 1L, "treated", "control")]
  d
}
# H2 (05 B3): + inmigrantes no hispanos
s_eth <- function(sx) {
  d <- dt[SEX == sx & (g_hfb == 1L | g_nhfb == 1L | g_ctrl == 1L)]
  d[, grp := fifelse(g_hfb == 1L, "hisp_fb", fifelse(g_nhfb == 1L, "nonhisp_fb", "control"))]
  d
}
# H3 (05 B4): hispanos nacidos fuera documentados / indocumentados
s_legal2 <- function(sx) {
  d <- dt[SEX == sx & (g_hfb == 1L | g_ctrl == 1L)]
  d[, grp := fifelse(g_ctrl == 1L, "control", fifelse(undoc == 1L, "undocumented", "documented"))]
  d
}
# H3 preliminar (04 B3): triple diferencia con toda la poblacion de ese sexo;
# referencia = nativos no hispanos (sin filtro de educacion, como en el 04)
s_ddd <- function(sx) {
  d <- dt[SEX == sx]
  d[, grp := fifelse(g_natnh == 1L, "control",
                     fifelse(g_nathisp == 1L, "us_born_hisp",
                             paste0(fifelse(undoc == 1L, "undoc_", "doc_"), fifelse(hispanic == 1L, "hisp", "nonhisp_fb"))))]
  d
}
# H5 (05 B5): origen, educacion, sector
s_origin <- function(sx, mexican) {
  d <- dt[SEX == sx & ((g_hfb == 1L & (origin == "Mexico") == mexican) | g_ctrl == 1L)]
  d[, grp := fifelse(g_hfb == 1L, "treated", "control")]
  d
}
s_educ <- function(sx, high, matched_control = FALSE) {                    # [E6]
  tr <- if (high) dt$g_hfb == 1L & !is.na(dt$educ_years) & dt$educ_years > 12 else
    dt$g_hfb == 1L & !is.na(dt$educ_years) & dt$educ_years <= 12
  ct <- if (matched_control && high) dt$g_ctrl_hi == 1L else dt$g_ctrl == 1L
  d <- dt[SEX == sx & (tr | ct)]
  d[, grp := fifelse(g_hfb == 1L, "treated", "control")]
  d
}
# (ADICIONAL) estatus legal desagregado + hispanos nativos
s_legal4 <- function(sx) {
  d <- dt[SEX == sx & (g_hfb == 1L | g_nathisp == 1L | g_ctrl == 1L)]
  d[, grp := fifelse(g_ctrl == 1L, "control", fifelse(g_nathisp == 1L, "native_hisp", status))]
  d
}


# ============================================================================
# 7. DATA AND SAMPLE
# ============================================================================
section("7. DATOS Y MUESTRA")

kn("CIFRAS PARA EL MANUSCRITO (pipeline CJLACS v3)")
kn("Generado: %s | especificacion principal: %s", format(Sys.time(), "%Y-%m-%d %H:%M"), MAIN_SPEC)
kn("Coeficientes = efecto de pasar el indice de 0 (estado-anio mas permisivo) a 1 (mas restrictivo).")
kn("Errores agrupados por estado; p-valores con t de G - 1 grados de libertad.")
kn("Las lineas marcadas (ADICIONAL) no estan en el manuscrito.")

kn_head("0. COMPROBACIONES (revisar antes de usar las cifras)")
kn("YRIMMIG interpretado como: %s (mediana entre nacidos fuera: %s).",
   ifelse(YRIMMIG_IS_YEAR, "anio de llegada", "anios desde la llegada"), YRIMMIG_MEDIAN)
kn("NATIVITY 4: %s filas; %s tienen CITIZEN 1-3 (nacidos con la ciudadania) -> si es ~100%%, el 01 contaba hijos de inmigrantes como inmigrantes [E1].",
   fN(N_NAT4), fpc(SH_NAT4_CIT123))
kn("NATIVITY 5: %s tienen CITIZEN 2-5 (tabla A0).", fpc(SH_NAT5_CIT25))
kn("Nacidos fuera, ponderado (20-64): %s con CITIZEN 4/5 | %s con NATIVITY 4/5 (definicion del 01).",
   fpc(FB_SHARE_W), fpc(FB_SHARE_OLD_W))
kn("HISPAN 901/902: %s filas [E8]. CLASSWKR 99 (desconocido): %s filas (cuentan como 0 en todos los resultados, como en el 01).",
   fN(N_HISPAN_9XX), fN(N_CLASSWKR_99))
kn("Autoempleo [E13]: de las filas con CLASSWKR 13, %s no estaban ocupadas en la semana de referencia; con CLASSWKR 14, %s. %s (tabla A0h).",
   fpc(SHARE_SE_NOTEMP), fpc(SHARE_SEI_NOTEMP),
   if (SE_EMPLOYED_ONLY) "Ahora cuentan como 0" else "Siguen contando como autoempleo (SE_EMPLOYED_ONLY = FALSE)")
kn("Filas que cruzan entre los dos extractos: %s.", fpc(MATCH_RATE))
if (N_2014_DROPPED > 0) {
  kn("ASEC 2014 [E10]: se usa la submuestra %s; se han quitado %s filas de la otra.", ASEC2014_SUBSAMPLE,
     fN(N_2014_DROPPED))
} else if (ASEC2014_SUBSAMPLE == "both") {
  kn("ASEC 2014 [E10]: se usan las dos submuestras (opcion 'both', como el manuscrito enviado).")
}
kn("Secure Communities: %d estado-anios anteriores a 2021 con la celda vacia en TRAC; %s.", N_SC_NA_PRE2021,
   if (SC_NA_AS_ZERO) "se cuentan como 0 (SC_NA_AS_ZERO = TRUE)" else
     "quedan sin indice, como en el script 02 (SC_NA_AS_ZERO = FALSE)")
kn("Estado-anios sin indice: %d.", N_NA_IDX)
kn("E-Verify [E15]: %s.", EV_RULE_TXT)
if (EV_RULE != "excel") {
  kn("  Leyes de los estados 'All or Most Employers' de NCSL (para la Tabla 1 y el apendice):")
  for (l in EV_LAW$ley) kn("    %s", l)
}
kn("  Anio de adopcion de E-Verify universal por estado: %s.",
   paste(sprintf("%s %d", STATES[match(ev_year$state_fips, state_fips), state_name], ev_year$everify_year),
         collapse = ", "))
kn("  -> Corregir la Tabla 1: Colorado nunca exigio E-Verify a todos los empleadores (2006-2016: declaracion del I-9 y copias; E-Verify solo contratistas del estado).")
kn("Indice principal: %s [E11]%s", INDEX_MAIN,
   if (INDEX_MAIN == "v4") sprintf(" (E-Verify + Secure Communities por 1.000 no ciudadanos%s - licencias - matricula)",
                                   if (SC_LOG) ", en log" else "") else " (cinco componentes, como el manuscrito enviado)")
for (s in CHECKS) kn("AVISO: %s", s)
if (!is.null(REPL)) {
  kn("Replica con la codificacion antigua: beta = %s (SE %s), N = %s", f4(REPL$b), f4(REPL$se), fN(REPL$N))
  kn("  nacidos fuera = %s (documentados %s) | tratados hombres = %s (documentados %s, indocumentados %s) | control = %s",
     fN(REPL$n_fb), fpc(REPL$n_doc / REPL$n_fb), fN(REPL$n_trt), fN(REPL$n_trt_doc),
     fN(REPL$n_trt - REPL$n_trt_doc), fN(REPL$n_ctrl))
  kn("  Manuscrito enviado: 0.0447 (SE 0.0110), N = 298,648 | 406,464 nacidos fuera (72.2% documentados) | tratados 107,549 (74,907 / 32,642) | control 204,428")
  kn("  -> Si coincide, los datos leidos son los mismos y las diferencias con las cifras nuevas se deben solo a las correcciones.")
  kn("  Composicion de los 'indocumentados' de la codificacion antigua: %s con NATIVITY 4 (nacidos en EE. UU.), %s con CITIZEN 4 (naturalizados), %s con CITIZEN 5 (no ciudadanos).",
     fpc(REPL$und_nat4), fpc(REPL$und_cit4), fpc(REPL$und_cit5))
}

kn_head("1. DATA AND SAMPLE")
kn("Observaciones 20-64, %d-%d: %s (estados: %d, anios: %d)", YEAR_START, YEAR_END, fN(nrow(dt)),
   uniqueN(dt$STATEFIP), uniqueN(dt$YEAR))
kn("Autoempleo en la muestra completa (ambos sexos): no incorporado %s | incorporado %s | total %s",
   fN(sum(dt$se_uninc, na.rm = TRUE)), fN(sum(dt$se_inc, na.rm = TRUE)), fN(sum(dt$se_any, na.rm = TRUE)))
kn("Nacidos fuera, ambos sexos: %s | documentados: %s (%s) | probables indocumentados: %s (%s)",
   fN(N_FB), fN(N_DOC), fpc(N_DOC / N_FB), fN(N_UND), fpc(N_UND / N_FB))
kn("  ponderado: %s indocumentados entre los nacidos fuera", fpc(UND_W))
kn("  desglose: naturalizados %s | residentes legales no ciudadanos %s | indocumentados %s",
   fN(dt[status == "naturalized", .N]), fN(dt[status == "legal_resident", .N]), fN(N_UND))
kn("  reglas de Borjas: cuantos marca cada una y cuantos solo ella, en tables/A0c_borjas_rules.csv")

sample_rows <- list()
for (s in names(SEXES)) {
  d <- s_main(SEXES[s])
  n_t <- d[grp == "treated", .N]; n_c <- d[grp == "control", .N]
  n_td <- d[grp == "treated" & undoc == 0L, .N]; n_tu <- d[grp == "treated" & undoc == 1L, .N]
  kn("%s: tratados (hispanos nacidos fuera) %s [documentados %s, indocumentados %s] | control %s",
     SEX_ES[s], fN(n_t), fN(n_td), fN(n_tu), fN(n_c))
  sample_rows[[s]] <- data.table(sex = SEX_EN[s], treated = n_t, treated_doc = n_td,
                                 treated_undoc = n_tu, control = n_c)
}
kn("(La muestra analitica, tras quitar valores ausentes, es la N de cada modelo en H1.)")
fwrite(rbindlist(sample_rows), file.path(TAB_DIR, "A1_sample_sizes.csv"))


# ============================================================================
# 8. MEASURING ENFORCEMENT INTENSITY
# ============================================================================
section("8. INDICE DE ENFORCEMENT")
kn_head("2. MEASURING ENFORCEMENT INTENSITY")
kn("Panel: %d estado-anios (%d estados x %d anios); con indice: %d", nrow(panel),
   uniqueN(panel$state_fips), uniqueN(panel$year), panel[!is.na(enforce_index), .N])
kn("Indice aditivo: min %s | P25 %s | mediana %s | P75 %s | max %s",
   f2(min(panel$enforce_index, na.rm = TRUE)), f2(quantile(panel$enforce_index, 0.25, na.rm = TRUE)),
   f2(median(panel$enforce_index, na.rm = TRUE)), f2(quantile(panel$enforce_index, 0.75, na.rm = TRUE)),
   f2(max(panel$enforce_index, na.rm = TRUE)))
ST_MEANS <- panel[, .(m = mean(enforce_index, na.rm = TRUE)), by = state_fips]
Q_ST <- quantile(ST_MEANS$m, c(0.10, 0.25, 0.75, 0.90), na.rm = TRUE)
kn("Medias estatales del indice: P10 %s | P25 %s | P75 %s | P90 %s",
   f2(Q_ST[1]), f2(Q_ST[2]), f2(Q_ST[3]), f2(Q_ST[4]))
top5 <- panel[!is.na(enforce_index)][order(-enforce_index)][1:min(5, .N)]
kn("Cinco estado-anios mas restrictivos: %s",
   paste(sprintf("%s %d (%s)", top5$state_name, top5$year, f3(top5$enforce_index)), collapse = "; "))
if (!is.null(PCA$loadings)) {
  kn("PCA, cargas del primer componente: %s",
     paste(sprintf("%s %+.3f", names(PCA$loadings), PCA$loadings), collapse = " | "))
  kn("PCA, varianza explicada por el primer componente: %s | signo invertido: %s",
     fpc(PCA$var1), ifelse(isTRUE(PCA$flipped), "si", "no"))
  fwrite(data.table(component = names(PCA$loadings), loading = PCA$loadings,
                    var_explained_pc1 = PCA$var1, sign_flipped = PCA$flipped),
         file.path(TAB_DIR, "A3c_pca_loadings.csv"))
}
DQ <- quantile(panel$deportation_rate, c(0.5, 0.9, 0.99, 1), na.rm = TRUE)
kn("Tasa de deportacion ICE por 1.000 indocumentados%s: mediana %s | P90 %s | P99 %s | max %s",
   if (INDEX_MAIN == "v4") " (fuera del indice principal [E11]: TRAC la asigna al puerto de salida)" else "",
   f1(DQ[1]), f1(DQ[2]), f1(DQ[3]), f1(DQ[4]))
kn("  estado-anios con dep_norm < 0,10 (de 0 a 1): %s", fpc(mean(panel$dep_norm < 0.10, na.rm = TRUE)))
top_dep <- panel[!is.na(deportation_rate)][order(-deportation_rate)][1:min(5, .N)]
kn("  tasas mas altas: %s", paste(sprintf("%s %d (%s; poblacion indocumentada estimada %s)", top_dep$state_name,
                                          top_dep$year, f1(top_dep$deportation_rate),
                                          fN(round(top_dep$undoc_population))), collapse = "; "))
SQ <- quantile(panel$sc_rate, c(0.5, 0.9, 0.99, 1), na.rm = TRUE)
kn("Tasa Secure Communities por 1.000 indocumentados (manuscrito enviado): mediana %s | P90 %s | P99 %s | max %s",
   f1(SQ[1]), f1(SQ[2]), f1(SQ[3]), f1(SQ[4]))
kn("  estado-anios con el componente < 0,10 (de 0 a 1): %s", fpc(mean(panel$sc_norm_ms5 < 0.10, na.rm = TRUE)))
SQ2 <- quantile(panel$sc_rate_nc, c(0.5, 0.9, 0.99, 1), na.rm = TRUE)
kn("Tasa Secure Communities por 1.000 no ciudadanos (media del estado) [E11]: mediana %s | P90 %s | P99 %s | max %s",
   f1(SQ2[1]), f1(SQ2[2]), f1(SQ2[3]), f1(SQ2[4]))
kn("  estado-anios con el componente < 0,10: en log %s | sin log %s",
   fpc(mean(panel$sc_norm_v4 < 0.10, na.rm = TRUE)), fpc(mean(panel$sc_norm_v4_nolog < 0.10, na.rm = TRUE)))
top_sc <- panel[!is.na(sc_rate_nc)][order(-sc_rate_nc)][1:min(5, .N)]
kn("  tasas mas altas: %s", paste(sprintf("%s %d (%s)", top_sc$state_name, top_sc$year, f1(top_sc$sc_rate_nc)),
                                  collapse = "; "))
fwrite(top_sc[, .(state_name, year, sc_removals, nc_population, sc_rate_nc, sc_norm_v4)],
       file.path(TAB_DIR, "A3d_sc_top_rates.csv"))
kn("Correlacion tasa total de deportacion - tasa Secure Communities (como en el manuscrito): %s",
   f2(suppressWarnings(cor(panel$dep_norm, panel$sc_norm_ms5, use = "complete.obs"))))
kn("Correlaciones entre versiones del indice (estado-anios con las cuatro):")
IV <- c("enforce_v4_log", "enforce_v4_nolog", "enforce_dejure", "enforce_ms5")
CI <- suppressWarnings(cor(panel[, IV, with = FALSE], use = "complete.obs"))
for (a in IV) kn("  %-17s %s", a, paste(sprintf("%5.2f", CI[a, ]), collapse = " "))
kn("  (columnas en el mismo orden: v4_log, v4_nolog, dejure, ms5)")


# ============================================================================
# 9. H1: EFECTO PRINCIPAL Y ASIMETRIA ORGANIZACIONAL
# ============================================================================
section("9. H1: EFECTO PRINCIPAL Y ASIMETRIA")
kn_head("3. H1 - MAIN EFFECT AND ORGANIZATIONAL ASYMMETRY")

# Regresion apilada (06v2 M1): cada persona aporta dos filas (no incorporado,
# incorporado); todos los regresores y efectos fijos interactuan con la
# ecuacion; cluster bidireccional estado + persona. El coeficiente de la
# interaccion es beta_uninc - beta_inc con la covarianza incluida.
stacked_h1 <- function(d, spec = MAIN_SPEC, idx = "enforce_index", unemp = "unemp_rate_march") {
  out <- list(est = NA_real_, se = NA_real_, p = NA_real_, N = NA_integer_)
  need <- unique(c("se_uninc", "se_inc", idx, CONTROLS, "grp", "STATEFIP", "YEAR", "ASECWT",
                   if (spec %in% c("paper", "phi")) unemp))
  dd <- d[, need, with = FALSE]
  dd <- dd[complete.cases(dd)]
  dd[, pid := .I]
  dd[, TX := as.numeric(grp == "treated") * get(idx)]
  regs <- c("TX", CONTROLS, if (spec %in% c("paper", "phi")) unemp, if (spec == "phi") idx)
  keep <- unique(c(regs, "grp", "STATEFIP", "YEAR", "ASECWT", "pid"))
  lu <- dd[, keep, with = FALSE]; lu[, `:=`(y = dd$se_uninc, eq = "uninc")]
  li <- dd[, keep, with = FALSE]; li[, `:=`(y = dd$se_inc,   eq = "inc")]
  long <- rbind(lu, li); rm(lu, li)
  long[, D := as.numeric(eq == "uninc")]
  inter <- paste0(regs, "_xU")
  for (k in seq_along(regs)) long[, (inter[k]) := get(regs[k]) * D]
  fe <- switch(spec,
               paper = "STATEFIP^eq + YEAR^eq + grp^eq",
               phi   = "STATEFIP^eq + YEAR^eq + grp^eq",
               sxy   = "STATEFIP^YEAR^eq + grp^eq",
               full  = "STATEFIP^YEAR^eq + grp^STATEFIP^eq + grp^YEAR^eq")
  fml <- as.formula(paste("y ~", paste(c(regs, inter), collapse = " + "), "|", fe))
  m <- tryCatch(feols(fml, data = long, weights = ~ASECWT, cluster = ~STATEFIP + pid),
                error = function(e) { say("    [AVISO] regresion apilada: %s", conditionMessage(e)); NULL })
  if (is.null(m) || !"TX_xU" %in% names(coef(m))) return(out)
  ct <- coeftable(m)
  list(est = ct["TX_xU", 1], se = ct["TX_xU", 2], p = ct["TX_xU", 4], N = nobs(m))
}

H1 <- list()
for (s in names(SEXES)) {
  d <- s_main(SEXES[s])
  r_u <- est_did(d, "se_uninc"); record(r_u, "H1", SEX_EN[s], "main")
  r_i <- est_did(d, "se_inc");   record(r_i, "H1", SEX_EN[s], "main")
  r_a <- est_did(d, "se_any");   record(r_a, "H1", SEX_EN[s], "main")
  bu <- cf(r_u, "treated"); bi <- cf(r_i, "treated"); ba <- cf(r_a, "treated")
  G <- if (is.null(r_u)) NA else r_u$G
  # Diferencia: (a) regresion apilada (manuscrito); (b) aproximacion sin
  # covarianza (05 B2); (c) comprobacion: regresion de (se_uninc - se_inc).
  st <- stacked_h1(d)
  record_test(st, "H1", SEX_EN[s], "main", "se_uninc - se_inc", "difference (stacked, two-way cluster)",
              N = st$N, G = G)
  ap_se <- sqrt(bu$se^2 + bi$se^2)
  ap <- list(est = bu$est - bi$est, se = ap_se, p = 2 * pt(-abs((bu$est - bi$est) / ap_se), df = G - 1))
  r_d <- est_did(d, "se_diff", quiet = TRUE)
  bd <- cf(r_d, "treated")
  base_rate <- if (is.null(r_u)) NA_real_ else r_u$means[grp == "treated", mean_y]
  H1[[s]] <- list(u = r_u, i = r_i, a = r_a, st = st)
  
  kn("")
  kn("%s", SEX_ES[s])
  kn(line_coef("Autoempleo NO incorporado (se_uninc)", bu, nobs_or_null(r_u)))
  kn(line_coef("Autoempleo incorporado (se_inc)", bi))
  kn(line_coef("Autoempleo total (se_any)", ba))
  kn("Tasa base ponderada de se_uninc entre los tratados: %s -> efecto relativo (0 a 1): %s",
     fpc(base_rate), fpc(bu$est / base_rate))
  kn("Por cada 0,1 del indice: %s (%s pp)", f4(bu$est / 10), f2(100 * bu$est / 10))
  kn("Diferencias entre estados (medias estatales P25-P75 = %s; P10-P90 = %s): %s pp y %s pp",
     f2(Q_ST[3] - Q_ST[2]), f2(Q_ST[4] - Q_ST[1]), f2(100 * bu$est * (Q_ST[3] - Q_ST[2])),
     f2(100 * bu$est * (Q_ST[4] - Q_ST[1])))
  kn("Razon no incorporado / incorporado: %s", f2(bu$est / bi$est))
  kn(line_coef("Diferencia no incorporado - incorporado, regresion apilada (test H1)", st, st$N))
  kn(line_coef("  misma diferencia con la aproximacion sin covarianza (05 B2)", ap))
  kn(line_coef("  comprobacion: regresion de se_uninc - se_inc (debe dar la misma estimacion)", bd))
}


# ============================================================================
# 10. H2: ESPECIFICIDAD ETNICA (05 B3)
# ============================================================================
section("10. H2: ESPECIFICIDAD ETNICA")
kn_head("4. H2 - ETHNIC SPECIFICITY")
H2 <- list()
for (s in names(SEXES)) {
  r <- est_did(s_eth(SEXES[s]), "se_uninc"); record(r, "H2", SEX_EN[s], "ethnic")
  dtst <- diff_test(r, "hisp_fb", "nonhisp_fb")
  record_test(dtst, "H2", SEX_EN[s], "ethnic", "se_uninc", "hisp_fb - nonhisp_fb",
              N = if (is.null(r)) NA else r$N, G = if (is.null(r)) NA else r$G)
  H2[[s]] <- list(r = r, d = dtst)
  kn("")
  kn("%s (N = %s)", SEX_ES[s], if (is.null(r)) "NA" else fN(r$N))
  kn(line_coef("Inmigrantes no hispanos", cf(r, "nonhisp_fb")))
  kn(line_coef("Inmigrantes hispanos", cf(r, "hisp_fb")))
  kn(line_coef("Diferencia hispanos - no hispanos", dtst))
}


# ============================================================================
# 11. H3: ESTATUS LEGAL (04 B3 preliminar + 05 B4)
# ============================================================================
section("11. H3: ESTATUS LEGAL")
kn_head("5. H3 - LEGAL STATUS")
H3 <- list()
for (s in names(SEXES)) {
  sx <- SEXES[s]
  kn("")
  kn("%s", SEX_ES[s])
  # Triple diferencia preliminar (04 B3): seis grupos, referencia = nativos no hispanos
  kn("Triple diferencia preliminar (seccion Identification; script 04 B3):")
  dd3 <- s_ddd(sx)
  for (y in c("se_uninc", "se_inc")) {
    r3 <- est_did(dd3, y); record(r3, "H3_triple_difference", SEX_EN[s], "all groups")
    t3 <- diff_test(r3, "undoc_hisp", "doc_hisp")
    record_test(t3, "H3_triple_difference", SEX_EN[s], "all groups", y, "undoc_hisp - doc_hisp",
                N = if (is.null(r3)) NA else r3$N, G = if (is.null(r3)) NA else r3$G)
    kn(line_coef(sprintf("  %s, hispanos indocumentados - documentados", y), t3, nobs_or_null(r3)))
  }
  rm(dd3)
  # Heterogeneidad por estatus (05 B4)
  r2 <- est_did(s_legal2(sx), "se_uninc"); record(r2, "H3", SEX_EN[s], "legal two groups")
  t_ud <- diff_test(r2, "undocumented", "documented")
  record_test(t_ud, "H3", SEX_EN[s], "legal two groups", "se_uninc", "undocumented - documented",
              N = if (is.null(r2)) NA else r2$N, G = if (is.null(r2)) NA else r2$G)
  H3[[s]] <- list(r2 = r2)
  kn("Heterogeneidad por estatus (resultados; N = %s):", if (is.null(r2)) "NA" else fN(r2$N))
  kn(line_coef("  Hispanos nacidos fuera documentados", cf(r2, "documented")))
  kn(line_coef("  Hispanos nacidos fuera indocumentados", cf(r2, "undocumented")))
  kn(line_coef("  Diferencia indocumentados - documentados", t_ud))
}


# ============================================================================
# 12. H4: MECANISMO (05 B6)
# ============================================================================
section("12. H4: MECANISMO")
kn_head("6. H4 - MECHANISM (visibility vs. price channel)")
kn("Paro de marzo para wage_worker; paro anual del anio anterior para los resultados del anio pasado [E5].")
MECH <- list(
  wage_worker    = list(label = "Wage earner (March)",            unemp = "unemp_rate_march"),
  fulltime_ly    = list(label = "Full-time last year",            unemp = "unemp_annual_lag1"),
  total_hours_ly = list(label = "Annual hours last year",         unemp = "unemp_annual_lag1"),
  log_inclongj   = list(label = "Log income from main job",       unemp = "unemp_annual_lag1"))
H4 <- list()
for (s in names(SEXES)) {
  d <- s_main(SEXES[s])
  H4[[s]] <- list()
  kn("")
  kn("%s", SEX_ES[s])
  for (y in names(MECH)) {
    r <- est_did(d, y, unemp = MECH[[y]]$unemp); record(r, "H4", SEX_EN[s], "main", MECH[[y]]$label)
    H4[[s]][[y]] <- r
    kn(line_coef(sprintf("%s (%s)", MECH[[y]]$label, y), cf(r, "treated"), nobs_or_null(r)))
  }
}


# ============================================================================
# 13. H5: HETEROGENEIDAD (05 B5)
# ============================================================================
section("13. H5: HETEROGENEIDAD")
kn_head("7. H5 - HETEROGENEITY (each subsample paired with the control group)")
H5 <- list()
for (s in names(SEXES)) {
  sx <- SEXES[s]
  r_mx <- est_did(s_origin(sx, TRUE), "se_uninc");  record(r_mx, "H5", SEX_EN[s], "Mexican-born")
  r_ot <- est_did(s_origin(sx, FALSE), "se_uninc"); record(r_ot, "H5", SEX_EN[s], "other Hispanic-born")
  r_lo <- est_did(s_educ(sx, FALSE), "se_uninc");   record(r_lo, "H5", SEX_EN[s], "schooling <= 12")
  r_hi <- est_did(s_educ(sx, TRUE), "se_uninc");    record(r_hi, "H5", SEX_EN[s], "schooling > 12")
  dm <- s_main(sx)
  r_hs <- est_did(dm[sector_high_undoc == 1L], "se_uninc"); record(r_hs, "H5", SEX_EN[s], "high-undocumented sectors")
  r_os <- est_did(dm[sector_high_undoc == 0L], "se_uninc"); record(r_os, "H5", SEX_EN[s], "other sectors")
  H5[[s]] <- list(mx = r_mx, ot = r_ot, lo = r_lo, hi = r_hi, hs = r_hs, os = r_os)
  kn("")
  kn("%s", SEX_ES[s])
  kn(line_coef("Nacidos en Mexico", cf(r_mx, "treated"), nobs_or_null(r_mx)))
  kn(line_coef("Otros hispanos nacidos fuera", cf(r_ot, "treated"), nobs_or_null(r_ot)))
  kn(line_coef("Educacion <= 12 anios", cf(r_lo, "treated"), nobs_or_null(r_lo)))
  kn(line_coef("Educacion > 12 anios (con el mismo control) [E6]", cf(r_hi, "treated"), nobs_or_null(r_hi)))
  kn(line_coef("Sectores con alta concentracion indocumentada (IND1990)", cf(r_hs, "treated"), nobs_or_null(r_hs)))
  kn(line_coef("Resto de sectores", cf(r_os, "treated"), nobs_or_null(r_os)))
  kn("  Razon sectores alta concentracion / resto: %s",
     f2(cf(r_hs, "treated")$est / cf(r_os, "treated")$est))
}


# ============================================================================
# 14. ALTERNATIVE EXPLANATIONS AND VALIDATION
# ============================================================================
section("14. VALIDACION")
kn_head("8. ALTERNATIVE EXPLANATIONS AND VALIDATION")

ROB <- list()
for (s in names(SEXES)) {
  d <- s_main(SEXES[s])
  kn("")
  kn("%s", SEX_ES[s])
  # Indice PCA (05 B7a)
  r_p <- est_did(d, "se_uninc", idx = "enforce_pca", require = c("enforce_index", "enforce_pca"))
  record(r_p, "validation", SEX_EN[s], "PCA index")
  kn(line_coef("Indice PCA", cf(r_p, "treated", "enforce_pca"), nobs_or_null(r_p)))
  # Horse race conjunto (05 B7b)
  r_h <- est_did(d, "se_uninc", idx = COMP); record(r_h, "validation", SEX_EN[s], "horse race")
  kn("Horse race (los %d componentes a la vez):", length(COMP))
  for (cc in COMP) kn(line_coef(paste0("  ", cc), cf(r_h, "treated", cc)))
  # Componentes uno a uno (06v2 M2)
  kn("Componentes uno a uno (06v2 M2):")
  for (cc in COMP) {
    r1 <- est_did(d, "se_uninc", idx = cc); record(r1, "validation", SEX_EN[s], paste0("single component: ", cc))
    kn(line_coef(paste0("  ", cc), cf(r1, "treated", cc)))
  }
  ROB[[s]] <- list(pca = r_p, hr = r_h)
}

# [E11] Versiones del indice: cada una en su propia muestra de estado-anios y,
# para separar definicion y muestra, el indice principal en la muestra de ms5.
kn("")
kn("Versiones del indice, autoempleo no incorporado [E11]:")
IDX_VERSIONS <- list(
  list("enforce_v4_log",   "v4: E-Verify + SC por no ciudadano (log) - licencias - matricula"),
  list("enforce_v4_nolog", "v4 sin log"),
  list("enforce_dejure",   "solo leyes: E-Verify - licencias - matricula"),
  list("enforce_ms5",      "ms5: cinco componentes, como el manuscrito enviado"))
IDXV <- list()
for (s in names(SEXES)) {
  d <- s_main(SEXES[s])
  for (v in IDX_VERSIONS) {
    r <- est_did(d, "se_uninc", idx = v[[1]], require = v[[1]])
    record(r, "index_versions", SEX_EN[s], v[[2]])
    x <- cf(r, "treated", v[[1]])
    IDXV[[length(IDXV) + 1]] <- data.table(sex = SEX_EN[s], index = v[[1]], sample = "own",
                                           estimate = x$est, se = x$se, p = x$p, N = nobs_or_null(r))
    kn(line_coef(sprintf("  %s, %s", SEX_EN[s], v[[2]]), x, nobs_or_null(r)))
  }
  r <- est_did(d, "se_uninc", idx = "enforce_index", require = c("enforce_index", "enforce_ms5"))
  x <- cf(r, "treated")
  IDXV[[length(IDXV) + 1]] <- data.table(sex = SEX_EN[s], index = "enforce_index", sample = "ms5",
                                         estimate = x$est, se = x$se, p = x$p, N = nobs_or_null(r))
  kn(line_coef(sprintf("  %s, indice principal en la muestra de ms5", SEX_EN[s]), x, nobs_or_null(r)))
}
fwrite(rbindlist(IDXV), file.path(TAB_DIR, "A4b_index_versions.csv"))
kn("  Las versiones tienen escalas distintas: comparar signo y significacion, no magnitudes.")

# [E13] Definicion de autoempleo: ocupados en la semana de referencia frente a
# CLASSWKR solo (manuscrito enviado, incluye el ultimo empleo de los no ocupados).
kn("")
kn("Definicion de autoempleo no incorporado [E13] (indice principal):")
for (s in names(SEXES)) {
  d <- s_main(SEXES[s])
  for (v in list(list("se_uninc", if (SE_EMPLOYED_ONLY) "ocupados en la semana de referencia (principal)" else "solo CLASSWKR (principal)"),
                 list("se_uninc_cw", "solo CLASSWKR, como el manuscrito enviado"))) {
    r <- est_did(d, v[[1]]); record(r, "se_definition", SEX_EN[s], v[[2]])
    base_rate <- if (is.null(r)) NA_real_ else r$means[grp == "treated", mean_y]
    kn("%s | tasa base de los tratados %s", line_coef(sprintf("  %s, %s", SEX_EN[s], v[[2]]), cf(r, "treated"),
                                                      nobs_or_null(r)), fpc(base_rate))
  }
}

# Escalera de especificaciones: muestra el efecto de [E4]
kn("")
kn("Escalera de especificaciones (tratados x indice):")
LADDER <- list()
for (s in names(SEXES)) {
  d <- s_main(SEXES[s])
  for (y in c("se_uninc", "se_inc")) for (sp in c("paper", "phi", "sxy", "full")) {
    r <- est_did(d, y, spec = sp); record(r, "ladder", SEX_EN[s], sp)
    x <- cf(r, "treated")
    LADDER[[length(LADDER) + 1]] <- data.table(sex = SEX_EN[s], outcome = y, spec = sp, estimate = x$est,
                                               se = x$se, p = x$p, N = if (is.null(r)) NA else r$N)
    kn(line_coef(sprintf("  %s, %s, %s", SEX_EN[s], y, sp), x))
  }
}
fwrite(rbindlist(LADDER), file.path(TAB_DIR, "A4_specification_ladder.csv"))
kn("  paper = ecuacion del manuscrito tal cual | phi = + Phi_st [E4] | sxy = EF estado x anio |")
kn("  full = EF estado x anio + grupo x estado + grupo x anio")

# ---------------------------------------------------------------------------
# Event study E-Verify (06v2 M3 y M4 corregidos [E7]). Muestra: tratados y
# control del modelo principal; fuera los estados con cohorte 2010-2011
# (menos de 2 anios previos en el panel); se mantienen los que nunca adoptan.
#   TWFE: ventana [-6, 6] (las observaciones fuera se excluyen, como en M3).
#   Sun-Abraham: cohorte solo para los tratados de estados que adoptan; todos
#   los periodos; test y grafico en [-6, 6] (como en M4 corregido).
# ---------------------------------------------------------------------------
ES_FE <- "STATEFIP^YEAR + grp^STATEFIP + grp^YEAR"
sa_joint_test <- function(m, d_used, periods) {
  b <- m$coefficients; V <- m$cov.scaled
  pat <- "^YEAR::(-?[0-9]+):cohort::([0-9]+)$"
  nm <- names(b); hit <- grepl(pat, nm)
  if (!any(hit)) return(list(F = NA_real_, df1 = NA_integer_, p = NA_real_, agg = NULL))
  info <- data.table(name = nm[hit], rel = as.integer(sub(pat, "\\1", nm[hit])),
                     cohort = as.integer(sub(pat, "\\2", nm[hit])))
  cells <- d_used[cohort < 10000L, .(w = sum(ASECWT)), by = .(cohort, rel = YEAR - cohort)]
  info <- merge(info, cells, by = c("cohort", "rel"), all.x = TRUE)
  info <- info[rel %in% periods & !is.na(w) & w > 0]
  per <- sort(unique(info$rel))
  if (length(per) == 0) return(list(F = NA_real_, df1 = NA_integer_, p = NA_real_, agg = NULL))
  Wm <- matrix(0, length(per), length(b), dimnames = list(per, nm))
  for (k in seq_along(per)) {
    z <- info[rel == per[k]]
    Wm[k, z$name] <- z$w / sum(z$w)
  }
  ab <- as.numeric(Wm %*% b); aV <- Wm %*% V %*% t(Wm)
  G <- uniqueN(d_used$STATEFIP)
  c(wald_zero(ab, aV, G), list(agg = data.table(time = per, estimate = ab, se = sqrt(diag(aV)))))
}
# other_controls = TRUE (ADICIONAL): anade tratados x los otros componentes
# del indice al TWFE, para separar E-Verify de los cambios en licencias,
# matricula y deportaciones de esos mismos estados (sin Sun-Abraham).
OTHER_COMP <- setdiff(COMP, "everify_strong")
event_study <- function(sx, y, other_controls = FALSE) {
  d <- s_main(sx)
  need <- c(y, "everify_year", "enforce_index", CONTROLS, "grp", "STATEFIP", "YEAR", "ASECWT",
            if (other_controls) OTHER_COMP)
  d <- d[, need, with = FALSE]
  d <- d[complete.cases(d)]
  d <- d[!(everify_year %in% c(2010L, 2011L))]
  d[, treated_num := as.numeric(grp == "treated")]
  oc <- character(0)
  if (other_controls) {
    for (v in OTHER_COMP) {
      if (isTRUE(sd(d[[v]]) > 0)) {
        d[, (paste0("TXc_", v)) := treated_num * get(v)]
        oc <- c(oc, paste0("TXc_", v))
      }
    }
  }
  d[, rel := fifelse(everify_year > 0L, YEAR - everify_year, NA_integer_)]
  n_adopt <- uniqueN(d[everify_year > 0L, STATEFIP])
  if (n_adopt == 0) return(NULL)
  G <- uniqueN(d$STATEFIP)
  # TWFE: una dummy por anio relativo para los tratados de estados que
  # adoptan (referencia: t = -1 y estados que nunca adoptan)
  tw <- d[is.na(rel) | (rel >= -6L & rel <= 6L)]
  tw[is.na(rel), rel := -1000L]
  rels <- setdiff(sort(unique(tw$rel)), c(-1L, -1000L))
  esn <- function(r) ifelse(r < 0, paste0("ES_m", -r), paste0("ES_p", r))
  for (r in rels) tw[, (esn(r)) := as.numeric(rel == r) * treated_num]
  es_terms <- esn(rels)
  rhs_tw <- c(es_terms, CONTROLS, oc)
  m_tw <- tryCatch(feols(as.formula(paste(y, "~", paste(rhs_tw, collapse = " + "), "|", ES_FE)),
                         data = tw, weights = ~ASECWT, cluster = ~STATEFIP),
                   error = function(e) { say("    [AVISO] TWFE: %s", conditionMessage(e)); NULL })
  co_tw <- NULL; pre_tw <- list(F = NA, df1 = NA, p = NA, wb = list(p = NA_real_, B = 0L))
  if (!is.null(m_tw)) {
    ct <- coeftable(m_tw)
    hit <- es_terms %in% rownames(ct)
    co_tw <- data.table(time = rels[hit], estimate = ct[es_terms[hit], 1], se = ct[es_terms[hit], 2],
                        p = ct[es_terms[hit], 4])
    pre <- es_terms[hit & rels %in% -6:-2]
    if (length(pre) > 0) {
      pre_tw <- c(wald_zero(coef(m_tw)[pre], vcov(m_tw)[pre, pre, drop = FALSE], G),
                  list(wb = list(p = NA_real_, B = 0L)))
      if (RUN_WILD_BOOTSTRAP) {
        say("    wild bootstrap de las pretendencias (%d replicas)...", WILD_B)
        fe_tw <- data.frame(a = paste(tw$STATEFIP, tw$YEAR), b = paste(tw$grp, tw$STATEFIP),
                            c = paste(tw$grp, tw$YEAR))
        pre_tw$wb <- wild_wald(tw, y, rhs_tw, pre, fe_tw, ES_FE, coef(m_tw))
      }
    }
    co_tw <- rbind(co_tw, data.table(time = -1L, estimate = 0, se = 0, p = NA_real_))
  }
  # Sun-Abraham
  d[, cohort := fifelse(treated_num == 1 & everify_year > 0L, everify_year, 10000L)]
  m_sa <- if (other_controls) NULL else tryCatch(feols(as.formula(paste(y, "~ sunab(cohort, YEAR) +",
                                                                        paste(CONTROLS, collapse = " + "), "|", ES_FE)),
                                                       data = d, weights = ~ASECWT, cluster = ~STATEFIP),
                                                 error = function(e) { say("    [AVISO] Sun-Abraham: %s", conditionMessage(e)); NULL })
  co_sa <- NULL; pre_sa <- list(F = NA, df1 = NA, p = NA)
  if (!is.null(m_sa)) {
    ct <- coeftable(m_sa)
    rn <- grep("^YEAR::-?[0-9]+$", rownames(ct), value = TRUE)
    co_sa <- data.table(time = as.integer(sub("^YEAR::", "", rn)), estimate = ct[rn, 1],
                        se = ct[rn, 2], p = ct[rn, 4])
    co_sa <- rbind(co_sa, data.table(time = -1L, estimate = 0, se = 0, p = NA_real_))
    pre_sa <- sa_joint_test(m_sa, d[obs(m_sa)], -6:-2)
    # Comprobacion: la agregacion propia debe reproducir la de fixest
    if (!is.null(pre_sa$agg)) {
      chk <- merge(pre_sa$agg, co_sa, by = "time", suffixes = c("_own", "_fixest"))
      if (nrow(chk) > 0 && max(abs(chk$estimate_own - chk$estimate_fixest)) > 1e-6) {
        say("    [AVISO] la agregacion Sun-Abraham no cuadra con fixest; no se reporta el test F.")
        pre_sa <- list(F = NA, df1 = NA, p = NA)
      }
    }
  }
  if (!is.null(co_tw)) setorder(co_tw, time)
  if (!is.null(co_sa)) setorder(co_sa, time)
  diff_post <- NA_real_
  if (!is.null(co_tw) && !is.null(co_sa)) {
    cmp <- merge(co_tw[time >= 0, .(time, a = estimate)], co_sa[time >= 0 & time <= 6, .(time, b = estimate)],
                 by = "time")
    if (nrow(cmp) > 0) diff_post <- max(abs(cmp$a - cmp$b))
  }
  list(tw = co_tw, sa = co_sa, pre_tw = pre_tw, pre_sa = pre_sa, n_adopt = n_adopt,
       cohorts = sort(unique(d[everify_year > 0L, everify_year])), G = G,
       N_tw = if (is.null(m_tw)) NA else nobs(m_tw), N_sa = if (is.null(m_sa)) NA else nobs(m_sa),
       diff_post = diff_post)
}
ES <- list()
if (RUN_EVENT_STUDY) {
  section("14.1 EVENT STUDY E-VERIFY")
  kn("")
  kn("Event study alrededor de E-Verify universal (06v2 M3/M4 corregidos [E7]); triple diferencia, cohortes 2010-2011 fuera:")
  for (s in names(SEXES)) for (y in c("se_uninc", "se_inc")) {
    say("  Event study %s, %s...", SEX_EN[s], y)
    es <- event_study(SEXES[s], y)
    ES[[paste(s, y)]] <- es
    if (is.null(es)) { kn("  %s, %s: no estimable (ningun estado adopta dentro del panel)", SEX_EN[s], y); next }
    kn("  %s, %s: %d estados adoptan (cohortes %s)", SEX_EN[s], y, es$n_adopt, paste(es$cohorts, collapse = ", "))
    kn("    pretendencias t = -6..-2: TWFE F = %s, p cluster %s, p wild bootstrap %s | Sun-Abraham F = %s, p cluster %s | max |TWFE - SA| post: %s",
       f2(es$pre_tw$F), fp(es$pre_tw$p), fpb(es$pre_tw$wb), f2(es$pre_sa$F), fp(es$pre_sa$p), f4(es$diff_post))
    tab <- es$tw[, .(time, twfe = estimate, twfe_se = se, twfe_p = p)]
    if (!is.null(es$sa)) {
      tab <- merge(tab, es$sa[, .(time, sunab = estimate, sunab_se = se, sunab_p = p)], by = "time", all = TRUE)
    }
    fwrite(tab, file.path(TAB_DIR, sprintf("A5_event_study_%s_%s.csv", s, y)))
    record_test(list(est = es$pre_tw$F, se = NA_real_, p = es$pre_tw$p), "event_study", SEX_EN[s],
                "TWFE", y, "joint F pre-trends t=-6..-2", spec = "full", N = es$N_tw, G = es$G)
    record_test(list(est = es$pre_tw$F, se = NA_real_, p = es$pre_tw$wb$p), "event_study", SEX_EN[s],
                "TWFE", y, "joint F pre-trends t=-6..-2, wild bootstrap p", spec = "full", N = es$N_tw, G = es$G)
    record_test(list(est = es$pre_sa$F, se = NA_real_, p = es$pre_sa$p), "event_study", SEX_EN[s],
                "Sun-Abraham", y, "joint F pre-trends t=-6..-2", spec = "full", N = es$N_sa, G = es$G)
  }
  kn("  Coeficientes por anio en tables/A5_event_study_*.csv.")
  kn("  Nota: cada coeficiente del event study se identifica con muy pocos estados. Con tan pocos clusters tratados, el F con")
  kn("  errores agrupados por estado rechaza de mas aunque no haya pretendencias (MacKinnon y Webb 2017); el p del wild")
  kn("  bootstrap restringido es el fiable. El F de Sun-Abraham tiene el mismo problema (no se calcula su bootstrap).")
  kn("  [v3.1] Las columnas sunab_se y sunab_p de las tablas A5 NO son fiables: la matriz de varianzas agrupada de")
  kn("  Sun-Abraham tiene mas coeficientes que estados y no es definida positiva. Usad solo sus estimaciones puntuales.")
}


# ============================================================================
# 15. ADICIONAL (no esta en el manuscrito)
# ============================================================================
EXTRA <- list()
if (RUN_EXTRAS) {
  section("15. ADICIONAL")
  kn_head("9. ADICIONAL (no esta en el manuscrito)")
  
  # 15.1 Test formal mujeres - hombres (modelo conjunto, todo interactuado por sexo)
  sexdiff <- function(y, idx = "enforce_index", unemp = "unemp_rate_march", spec = MAIN_SPEC) {
    d <- dt[(g_hfb == 1L | g_ctrl == 1L)]
    d[, sexf := fifelse(SEX == 1L, "m", "f")]
    d[, grp := paste0(sexf, "_", fifelse(g_hfb == 1L, "treated", "control"))]
    need <- unique(c(y, idx, CONTROLS, "grp", "sexf", "STATEFIP", "YEAR", "ASECWT",
                     if (spec %in% c("paper", "phi")) unemp))
    d <- d[, need, with = FALSE]
    d <- d[complete.cases(d)]
    d[, TX_m := as.numeric(grp == "m_treated") * get(idx)]
    d[, TX_f := as.numeric(grp == "f_treated") * get(idx)]
    rhs <- c("TX_m", "TX_f")
    for (v in c(CONTROLS, if (spec %in% c("paper", "phi")) unemp, if (spec == "phi") idx)) {
      d[, (paste0(v, "_m")) := get(v) * (sexf == "m")]
      d[, (paste0(v, "_f")) := get(v) * (sexf == "f")]
      rhs <- c(rhs, paste0(v, "_m"), paste0(v, "_f"))
    }
    fe <- switch(spec, paper = "STATEFIP^sexf + YEAR^sexf + grp", phi = "STATEFIP^sexf + YEAR^sexf + grp",
                 sxy = "STATEFIP^YEAR^sexf + grp", full = "STATEFIP^YEAR^sexf + grp^STATEFIP + grp^YEAR")
    m <- tryCatch(feols(as.formula(paste(y, "~", paste(rhs, collapse = " + "), "|", fe)),
                        data = d, weights = ~ASECWT, cluster = ~STATEFIP), error = function(e) NULL)
    if (is.null(m) || !all(c("TX_m", "TX_f") %in% names(coef(m)))) {
      return(list(est = NA_real_, se = NA_real_, p = NA_real_, G = NA))
    }
    b <- coef(m); V <- vcov(m); G <- uniqueN(d$STATEFIP)
    est <- unname(b["TX_f"] - b["TX_m"])
    se <- sqrt(V["TX_f", "TX_f"] + V["TX_m", "TX_m"] - 2 * V["TX_f", "TX_m"])
    list(est = est, se = se, p = 2 * pt(-abs(est / se), df = G - 1), G = G)
  }
  kn("Diferencia mujeres - hombres (modelo conjunto, todo interactuado por sexo):")
  for (y in c("se_uninc", "se_inc")) {
    t_ <- sexdiff(y)
    record_test(t_, "extra_sexdiff", "Women - Men", "main", y, "women - men", G = t_$G)
    kn(line_coef(sprintf("  %s, mujeres - hombres", y), t_))
  }
  
  # 15.2 Estatus legal desagregado e hispanos nativos
  kn("")
  kn("Estatus legal desagregado (se_uninc):")
  EXTRA$legal4 <- list()
  for (s in names(SEXES)) {
    r <- est_did(s_legal4(SEXES[s]), "se_uninc"); record(r, "extra_legal_detail", SEX_EN[s], "legal detail")
    EXTRA$legal4[[s]] <- r
    t_un <- diff_test(r, "undocumented", "naturalized")
    record_test(t_un, "extra_legal_detail", SEX_EN[s], "legal detail", "se_uninc", "undocumented - naturalized")
    kn("  %s (N = %s)", SEX_ES[s], if (is.null(r)) "NA" else fN(r$N))
    kn(line_coef("    Hispanos nativos (nacidos con la ciudadania)", cf(r, "native_hisp")))
    kn(line_coef("    Naturalizados", cf(r, "naturalized")))
    kn(line_coef("    Residentes legales no ciudadanos", cf(r, "legal_resident")))
    kn(line_coef("    Probables indocumentados", cf(r, "undocumented")))
    kn(line_coef("    Indocumentados - naturalizados", t_un))
  }
  
  # 15.3 H5 educacion alta con control de educacion alta
  kn("")
  kn("H5, educacion > 12 frente a nativos no hispanos con > 12 anios:")
  for (s in names(SEXES)) {
    r <- est_did(s_educ(SEXES[s], TRUE, matched_control = TRUE), "se_uninc")
    record(r, "extra_H5", SEX_EN[s], "schooling > 12, matched control")
    kn(line_coef(sprintf("  %s", SEX_EN[s]), cf(r, "treated"), nobs_or_null(r)))
  }
  
  # 15.4 H4 con salario solo de asalariados actuales y con Phi del anio pasado
  kn("")
  kn("H4: salario del trabajo principal solo para asalariados en marzo (CLASSWKR 21-28):")
  for (s in names(SEXES)) {
    r <- est_did(s_main(SEXES[s]), "log_inclongj_wage", unemp = "unemp_annual_lag1")
    record(r, "extra_H4", SEX_EN[s], "main", "Log income from main job, current wage earners")
    kn(line_coef(sprintf("  %s", SEX_EN[s]), cf(r, "treated"), nobs_or_null(r)))
  }
  kn("H4: resultados del anio pasado con el indice del anio pasado (Phi t-1):")
  for (s in names(SEXES)) for (y in c("fulltime_ly", "total_hours_ly", "log_inclongj")) {
    r <- est_did(s_main(SEXES[s]), y, idx = "enforce_index_lag1", unemp = "unemp_annual_lag1")
    record(r, "extra_H4", SEX_EN[s], "main", paste(MECH[[y]]$label, "(index t-1)"))
    kn(line_coef(sprintf("  %s, %s", SEX_EN[s], y), cf(r, "treated", "enforce_index_lag1"), nobs_or_null(r)))
  }
  
  # 15.5 Indice con tasas winsorizadas y test de anticipacion
  kn("")
  for (s in names(SEXES)) {
    d <- s_main(SEXES[s])
    r_w <- est_did(d, "se_uninc", idx = "enforce_index_w", require = c("enforce_index", "enforce_index_w"))
    record(r_w, "extra_robust", SEX_EN[s], sprintf("index, rates winsorized at p%d", round(100 * WINSOR_P_EXTRA)))
    kn(line_coef(sprintf("%s: indice con las tasas de deportacion del indice winsorizadas al p%d", SEX_EN[s],
                         round(100 * WINSOR_P_EXTRA)), cf(r_w, "treated", "enforce_index_w")))
    r_l <- est_did(d, "se_uninc", idx = c("enforce_index", "enforce_index_lead1"))
    record(r_l, "extra_robust", SEX_EN[s], "lead test")
    kn(line_coef(sprintf("%s: test de anticipacion, Phi t", SEX_EN[s]), cf(r_l, "treated", "enforce_index")))
    kn(line_coef(sprintf("%s: test de anticipacion, Phi t+1 (deberia ser ~0)", SEX_EN[s]),
                 cf(r_l, "treated", "enforce_index_lead1")))
  }
  
  # 15.6 Event study con los otros componentes como controles
  if (RUN_EVENT_STUDY) {
    kn("")
    kn("Event study TWFE controlando por tratados x los otros %d componentes del indice (pretendencias t = -6..-2):",
       length(OTHER_COMP))
    for (s in names(SEXES)) for (y in c("se_uninc", "se_inc")) {
      es <- event_study(SEXES[s], y, other_controls = TRUE)
      if (is.null(es) || is.null(es$tw)) next
      kn("  %s, %s: F = %s, p cluster %s, p wild bootstrap %s", SEX_EN[s], y, f2(es$pre_tw$F),
         fp(es$pre_tw$p), fpb(es$pre_tw$wb))
      fwrite(es$tw, file.path(TAB_DIR, sprintf("A5b_event_study_othercontrols_%s_%s.csv", s, y)))
      record_test(list(est = es$pre_tw$F, se = NA_real_, p = es$pre_tw$p), "extra_event_study", SEX_EN[s],
                  "TWFE + other components", y, "joint F pre-trends t=-6..-2", spec = "full",
                  N = es$N_tw, G = es$G)
    }
  }
  
  # 15.7 Jackknife de clusters (CV3) y leave-one-state-out
  if (RUN_JACKKNIFE) {
    jackknife_state <- function(d, y, g = "treated", idx = "enforce_index") {
      b_full <- cf(est_did(d, y, idx = idx, quiet = TRUE), g, idx)$est
      states <- sort(unique(d[!is.na(get(idx)), STATEFIP]))
      bs <- vapply(states, function(st) cf(est_did(d[STATEFIP != st], y, idx = idx, quiet = TRUE), g, idx)$est,
                   numeric(1))
      G <- sum(!is.na(bs))
      se <- sqrt((G - 1) / G * sum((bs - mean(bs, na.rm = TRUE))^2, na.rm = TRUE))
      list(b = b_full, se = se, p = 2 * pt(-abs(b_full / se), df = G - 1),
           loso = data.table(STATEFIP = states, estimate = bs))
    }
    kn("")
    kn("Jackknife de clusters (CV3) y leave-one-state-out, se_uninc:")
    for (s in names(SEXES)) {
      say("  Jackknife %s...", SEX_EN[s])
      jk <- jackknife_state(s_main(SEXES[s]), "se_uninc")
      lo <- merge(jk$loso, STATES, by.x = "STATEFIP", by.y = "state_fips")
      imin <- which.min(lo$estimate); imax <- which.max(lo$estimate)
      kn("  %s: beta %s, SE jackknife %s, p %s | sin un estado: entre %s (sin %s) y %s (sin %s)",
         SEX_EN[s], f4(jk$b), f4(jk$se), fp(jk$p), f4(lo$estimate[imin]), lo$state_name[imin],
         f4(lo$estimate[imax]), lo$state_name[imax])
      fwrite(lo[, .(state_name, estimate_without_state = estimate)],
             file.path(TAB_DIR, sprintf("A6_leave_one_state_out_%s.csv", s)))
      record_test(list(est = jk$b, se = jk$se, p = jk$p), "extra_robust", SEX_EN[s], "cluster jackknife CV3",
                  "se_uninc", "treated (jackknife SE)")
    }
  }
  
  # 15.8 Wild cluster restricted bootstrap para H1. Con 51 clusters los
  # errores agrupados pueden quedarse cortos (lo dice el manuscrito en las
  # limitaciones); el bootstrap (Cameron, Gelbach y Miller 2008) da p-valores
  # mas fiables. Con una sola restriccion, el Wald es t^2: test bilateral.
  if (RUN_WILD_BOOTSTRAP) {
    kn("")
    kn("Wild cluster bootstrap restringido (Rademacher, %d replicas), especificacion %s:", WILD_B, MAIN_SPEC)
    for (s in names(SEXES)) {
      d <- s_main(SEXES[s])
      for (y in c("se_uninc", "se_inc", "se_diff")) {
        say("  Wild bootstrap %s, %s...", SEX_EN[s], y)
        r <- est_did(d, y, keep_data = TRUE, quiet = TRUE)
        wb <- if (is.null(r)) list(p = NA_real_, B = 0L) else
          wild_wald(r$data, y, r$rhs, tname("treated"), fe_frame(r$data, r$spec), fe_string(r$spec),
                    coef(r$model))
        record_test(list(est = cf(r, "treated")$est, se = NA_real_, p = wb$p), "extra_robust", SEX_EN[s],
                    "wild cluster bootstrap", y, "treated (WCR bootstrap p)")
        kn("  %s, %s: beta %s | p cluster %s | p wild bootstrap %s", SEX_EN[s],
           if (y == "se_diff") "se_uninc - se_inc" else y,
           f4(cf(r, "treated")$est), fp(cf(r, "treated")$p), fpb(wb))
        rm(r); invisible(gc(verbose = FALSE))
      }
    }
  }
  
  # 15.9 Tabla descriptiva ponderada
  desc_groups <- c("Foreign-born Hispanic" = "g_hfb", "Non-Hispanic foreign-born" = "g_nhfb",
                   "Native-born Hispanic" = "g_nathisp",
                   "Native non-Hispanic, <= 12 years (control)" = "g_ctrl")
  desc_vars <- c("AGE", "educ_years", "married", "in_metro", "se_uninc", "se_inc", "wage_worker",
                 "undoc", "naturalized", "enforce_index")
  desc_rows <- list()
  for (s in names(SEXES)) for (gname in names(desc_groups)) {
    d <- dt[SEX == SEXES[s] & get(desc_groups[[gname]]) == 1L]
    if (nrow(d) == 0) next
    row <- data.table(sex = SEX_EN[s], group = gname, n = nrow(d))
    for (v in desc_vars) row[, (v) := wmean(d[[v]], d$ASECWT)]
    row[, mexican_born := wmean(as.numeric(d$origin == "Mexico"), d$ASECWT)]
    desc_rows[[length(desc_rows) + 1]] <- row
  }
  fwrite(rbindlist(desc_rows, fill = TRUE), file.path(TAB_DIR, "A2_descriptives_weighted.csv"))
  kn("")
  kn("Tabla descriptiva ponderada por grupo y sexo: tables/A2_descriptives_weighted.csv")
}


# ============================================================================
# 16. EXPORTACION (antes de las figuras, por seguridad)
# ============================================================================
section("16. EXPORTACION")
res_all <- rbindlist(ALL, fill = TRUE)
setcolorder(res_all, c("block", "sex", "sample", "outcome", "label", "group", "idx", "spec",
                       "estimate", "se", "p", "N", "G", "term"))
fwrite(res_all, file.path(OUT_DIR, "results_all.csv"))
writeLines(KN, file.path(OUT_DIR, "key_numbers.txt"), useBytes = TRUE)
say("  results_all.csv (%d filas) y key_numbers.txt guardados.", nrow(res_all))


# ============================================================================
# 17. FIGURAS (600 dpi, etiquetas en ingles, sin titulo: en Taylor & Francis
#     el titulo va en la leyenda). Un fallo en una figura no detiene el script.
# ============================================================================
section("17. FIGURAS")
COL_SEX <- c(Men = "#2a78d6", Women = "#eb6834")
SHP_SEX <- c(Men = 16, Women = 17)
THEME_FIG <- theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

# JPG para insertar en Word y TIFF-LZW para el envio (lleva la resolucion grabada)
# [v3.1] bg = "white": en v3 los TIFF salian con fondo transparente (canal alfa);
# algunos programas lo convierten en negro y el texto del grafico desaparece.
save_fig <- function(file, p, width = 16, height = 10) {
  ggsave(file, p, width = width, height = height, units = "cm", dpi = FIG_DPI, quality = 95,
         bg = "white")
  ggsave(sub("\\.jpg$", ".tiff", file), p, width = width, height = height, units = "cm",
         dpi = FIG_DPI, compression = "lzw", bg = "white")
}
safe_fig <- function(name, expr) {
  ok <- tryCatch({ force(expr); TRUE }, error = function(e) {
    say("  [AVISO] La figura %s no se pudo generar: %s", name, conditionMessage(e))
    FALSE
  })
  if (ok) say("  %s", name)
  invisible(ok)
}
coef_table <- function(spec_list) {
  rows <- lapply(spec_list, function(z) {
    x <- cf(z$res, z$group, if (is.null(z$idx)) "enforce_index" else z$idx)
    data.table(label = z$label, sex = z$sex, estimate = x$est, se = x$se,
               G = if (is.null(z$res)) NA_integer_ else z$res$G)
  })
  out <- rbindlist(rows)[!is.na(estimate) & !is.na(se)]
  out[, crit := qt(0.975, pmax(G - 1, 1))]
  out[, `:=`(lo = estimate - crit * se, hi = estimate + crit * se)]
  out
}
plot_coefs <- function(tab, levels_x, file, flip = FALSE,
                       ylab = "Effect of moving the index from 0 to 1 (pp)", height = 9) {
  if (nrow(tab) == 0) stop("sin coeficientes que dibujar")
  tab <- copy(tab)
  tab[, `:=`(estimate = 100 * estimate, lo = 100 * lo, hi = 100 * hi)]
  tab[, label := factor(label, levels = if (flip) rev(levels_x) else levels_x)]
  tab[, sex := factor(sex, levels = c("Men", "Women"))]
  tab[, dodge_grp := factor(sex, levels = if (flip) c("Women", "Men") else c("Men", "Women"))]
  p <- ggplot(tab, aes(x = label, y = estimate, ymin = lo, ymax = hi, colour = sex, shape = sex,
                       group = dodge_grp)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
    geom_pointrange(position = position_dodge(width = 0.5)) +
    scale_colour_manual(values = COL_SEX, drop = FALSE) +
    scale_shape_manual(values = SHP_SEX, drop = FALSE) +
    labs(x = NULL, y = ylab, colour = NULL, shape = NULL) +
    THEME_FIG
  if (flip) p <- p + coord_flip()
  save_fig(file, p, height = height)
}
by_sex <- function(f) unlist(lapply(names(SEXES), f), recursive = FALSE)

# Figura 1: asimetria organizacional (H1)
safe_fig("Figure_1_asymmetry.jpg", {
  t1 <- coef_table(by_sex(function(s) list(
    list(res = H1[[s]]$u, group = "treated", label = "Unincorporated", sex = SEX_EN[[s]]),
    list(res = H1[[s]]$i, group = "treated", label = "Incorporated", sex = SEX_EN[[s]]))))
  plot_coefs(t1, c("Unincorporated", "Incorporated"), file.path(FIG_DIR, "Figure_1_asymmetry.jpg"))
})

# Figura 2: especificidad etnica (H2)
safe_fig("Figure_2_ethnic_specificity.jpg", {
  t2 <- coef_table(by_sex(function(s) list(
    list(res = H2[[s]]$r, group = "nonhisp_fb", label = "Non-Hispanic immigrants", sex = SEX_EN[[s]]),
    list(res = H2[[s]]$r, group = "hisp_fb", label = "Hispanic immigrants", sex = SEX_EN[[s]]))))
  plot_coefs(t2, c("Non-Hispanic immigrants", "Hispanic immigrants"),
             file.path(FIG_DIR, "Figure_2_ethnic_specificity.jpg"))
})

# Figura 3: estatus legal (H3)
safe_fig("Figure_3_legal_status.jpg", {
  t3 <- coef_table(by_sex(function(s) list(
    list(res = H3[[s]]$r2, group = "documented", label = "Documented", sex = SEX_EN[[s]]),
    list(res = H3[[s]]$r2, group = "undocumented", label = "Likely undocumented", sex = SEX_EN[[s]]))))
  plot_coefs(t3, c("Documented", "Likely undocumented"), file.path(FIG_DIR, "Figure_3_legal_status.jpg"))
})

# Figura 4: mecanismo (H4). Cada panel en sus propias unidades.
safe_fig("Figure_4_mechanism.jpg", {
  P1 <- "Probability (pp)"; P2 <- "Annual hours (hundreds)"; P3 <- "Log income (x100)"
  spec4 <- list(list("wage_worker", P1, "Wage\nearner", 100), list("fulltime_ly", P1, "Full-time\nlast year", 100),
                list("total_hours_ly", P2, "Hours\nlast year", 1 / 100),
                list("log_inclongj", P3, "Income from\nmain job", 100))
  rows <- list()
  for (s in names(SEXES)) for (z in spec4) {
    r <- H4[[s]][[z[[1]]]]; x <- cf(r, "treated")
    rows[[length(rows) + 1]] <- data.table(panel = z[[2]], label = z[[3]], sex = SEX_EN[[s]],
                                           estimate = z[[4]] * x$est, se = z[[4]] * x$se,
                                           G = if (is.null(r)) NA_integer_ else r$G)
  }
  t4 <- rbindlist(rows)[!is.na(estimate) & !is.na(se)]
  if (nrow(t4) == 0) stop("sin coeficientes que dibujar")
  t4[, crit := qt(0.975, pmax(G - 1, 1))]
  t4[, `:=`(lo = estimate - crit * se, hi = estimate + crit * se)]
  t4[, label := factor(label, levels = vapply(spec4, function(z) z[[3]], character(1)))]
  t4[, sex := factor(sex, levels = c("Men", "Women"))]
  t4[, panel := factor(panel, levels = c(P1, P2, P3))]
  p4 <- ggplot(t4, aes(x = label, y = estimate, ymin = lo, ymax = hi, colour = sex, shape = sex)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
    geom_pointrange(position = position_dodge(width = 0.5)) +
    # [v3.1] facet_wrap con escalas libres: con facet_grid en una sola fila el eje Y era comun
    # a los tres paneles y el intervalo de las horas no se veia.
    facet_wrap(~ panel, nrow = 1, scales = "free", labeller = labeller(panel = label_wrap_gen(14))) +
    scale_colour_manual(values = COL_SEX, drop = FALSE) +
    scale_shape_manual(values = SHP_SEX, drop = FALSE) +
    labs(x = NULL, y = "Effect of moving the index from 0 to 1", colour = NULL, shape = NULL) +
    THEME_FIG + theme(axis.text.x = element_text(size = 7), strip.text = element_text(size = 8))
  save_fig(file.path(FIG_DIR, "Figure_4_mechanism.jpg"), p4, height = 9)
})

# Figura A1: heterogeneidad (H5)
lab5 <- list(list("mx", "Mexican-born"), list("ot", "Other Hispanic-born"),
             list("lo", "Up to 12 years of schooling"), list("hi", "More than 12 years of schooling"),
             list("hs", "High-undocumented sectors"), list("os", "Other sectors"))
safe_fig("Figure_A1_heterogeneity.jpg", {
  t5 <- coef_table(by_sex(function(s) lapply(lab5, function(z)
    list(res = H5[[s]][[z[[1]]]], group = "treated", label = z[[2]], sex = SEX_EN[[s]]))))
  plot_coefs(t5, vapply(lab5, function(z) z[[2]], character(1)),
             file.path(FIG_DIR, "Figure_A1_heterogeneity.jpg"), flip = TRUE, height = 10)
})

# Figura A2: event study (se_uninc), TWFE y Sun-Abraham
if (RUN_EVENT_STUDY) {
  safe_fig("Figure_A2_event_study.jpg", {
    rows <- list()
    for (s in names(SEXES)) {
      es <- ES[[paste(s, "se_uninc")]]
      if (is.null(es)) next
      if (!is.null(es$tw)) rows[[length(rows) + 1]] <- es$tw[, .(time, estimate, se, method = "TWFE", sex = SEX_EN[[s]], G = es$G)]
      if (!is.null(es$sa)) rows[[length(rows) + 1]] <- es$sa[time >= -6 & time <= 6,
                                                             .(time, estimate, se, method = "Sun-Abraham", sex = SEX_EN[[s]], G = es$G)]
    }
    ea <- rbindlist(rows)
    if (nrow(ea) == 0) stop("event study no estimado")
    ea[, crit := qt(0.975, pmax(G - 1, 1))]
    ea[, method := factor(method, levels = c("TWFE", "Sun-Abraham"))]
    # [v3.1] Sun-Abraham solo como puntos, sin intervalo. Con seis estados que adoptan, su matriz
    # de varianzas agrupada tiene mas coeficientes que clusters y no es definida positiva: en v3
    # daba intervalos de +-20.000 puntos que aplastaban el grafico. Las barras son solo del TWFE.
    ea[, x := time + fifelse(method == "TWFE", -0.15, 0.15)]
    ea[, `:=`(lo = 100 * (estimate - crit * se), hi = 100 * (estimate + crit * se))]
    pA <- ggplot(ea, aes(x = x, y = 100 * estimate, colour = method, shape = method)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
      geom_vline(xintercept = -0.5, linetype = "dotted", colour = "grey40") +
      geom_linerange(data = ea[method == "TWFE"], aes(ymin = lo, ymax = hi), show.legend = FALSE) +
      geom_point(size = 1.6) +
      facet_wrap(~ sex, ncol = 2) +
      scale_x_continuous(breaks = -6:6) +
      scale_colour_manual(values = c(TWFE = "#2a78d6", `Sun-Abraham` = "#1f9e74")) +
      labs(x = "Years relative to adoption of universal E-Verify",
           y = "Treated minus control (pp)", colour = NULL, shape = NULL) +
      THEME_FIG
    save_fig(file.path(FIG_DIR, "Figure_A2_event_study.jpg"), pA, height = 9)
  })
}

if (RUN_EXTRAS) {
  # Figura A3 (ADICIONAL): estatus legal desagregado
  lab_a3 <- c(native_hisp = "Native-born Hispanics", naturalized = "Naturalized citizens",
              legal_resident = "Legal residents (non-citizens)", undocumented = "Likely undocumented")
  safe_fig("Figure_A3_legal_detail.jpg", {
    ta3 <- coef_table(by_sex(function(s) lapply(names(lab_a3), function(g)
      list(res = EXTRA$legal4[[s]], group = g, label = lab_a3[[g]], sex = SEX_EN[[s]]))))
    plot_coefs(ta3, unname(lab_a3), file.path(FIG_DIR, "Figure_A3_legal_detail.jpg"), flip = TRUE)
  })
  
  # Figura 0 (ADICIONAL, descriptiva): brecha bruta por estado-anio frente al indice, hombres
  safe_fig("Figure_0_descriptive_gap.jpg", {
    g0 <- dt[SEX == 1L & (g_hfb == 1L | g_ctrl == 1L) & !is.na(enforce_index) & !is.na(ASECWT),
             .(rate = sum(se_uninc * ASECWT) / sum(ASECWT), w = sum(ASECWT), n = .N),
             by = .(STATEFIP, YEAR, enforce_index, tr = g_hfb)]
    g0 <- dcast(g0, STATEFIP + YEAR + enforce_index ~ tr, value.var = c("rate", "w", "n"))
    if (!all(c("rate_1", "rate_0", "n_1", "w_1") %in% names(g0))) stop("faltan grupos")
    g0 <- g0[!is.na(rate_1) & !is.na(rate_0) & n_1 >= 20]
    g0[, gap := 100 * (rate_1 - rate_0)]
    if (uniqueN(g0$enforce_index) <= 10) {
      g0[, bin := factor(enforce_index)]
    } else {
      g0[, bin := cut(enforce_index, breaks = unique(quantile(enforce_index, seq(0, 1, 0.1))),
                      include.lowest = TRUE)]
    }
    gb <- g0[, .(x = mean(enforce_index), y = mean(gap), se = sd(gap) / sqrt(.N), n = .N), by = bin][n >= 2]
    p0 <- ggplot(gb, aes(x = x, y = y)) +
      geom_hline(yintercept = 0, colour = "grey70") +
      geom_smooth(data = g0, aes(x = enforce_index, y = gap, weight = w_1), method = "lm",
                  formula = y ~ x, se = FALSE, colour = "grey45", linetype = "dashed") +
      geom_pointrange(aes(ymin = y - 1.96 * se, ymax = y + 1.96 * se), colour = "#2a78d6") +
      labs(x = "Enforcement index (0 = most permissive, 1 = most restrictive)",
           y = "Gap in unincorporated\nself-employment (pp)") +
      THEME_FIG
    save_fig(file.path(FIG_DIR, "Figure_0_descriptive_gap.jpg"), p0, height = 9)
  })
}


# ============================================================================
# 18. LEYENDAS Y CIERRE
# ============================================================================
kn_head("FIGURAS (carpeta figures/: .jpg para Word, .tiff para el envio) Y LEYENDAS PROPUESTAS")
CI_TXT <- paste("Coefficients show the change in the outcome, in percentage points, from moving the enforcement",
                "index from the least to the most restrictive state-year; bars are 95 percent confidence",
                "intervals with standard errors clustered by state.")
SRC_TXT <- "Source: Authors' elaboration based on IPUMS-CPS (Flood et al. 2025)."
kn("Figure_1_asymmetry: Enforcement and the organizational form of self-employment, foreign-born Hispanic men and women relative to native non-Hispanics of the same sex with up to 12 years of schooling. %s %s", CI_TXT, SRC_TXT)
kn("Figure_2_ethnic_specificity: Response of unincorporated self-employment among Hispanic and non-Hispanic immigrants, by sex. %s %s", CI_TXT, SRC_TXT)
kn("Figure_3_legal_status: Response of unincorporated self-employment among documented and likely undocumented foreign-born Hispanics, by sex. Legal status is imputed with the residual method of Borjas (2017a). %s %s", CI_TXT, SRC_TXT)
kn("Figure_4_mechanism: Effect of enforcement on wage employment, full-time work, annual hours, and log income from the main job, by sex. Each panel is in its own units. Bars are 95 percent confidence intervals with standard errors clustered by state. %s", SRC_TXT)
kn("Figure_A1_heterogeneity: Response of unincorporated self-employment by origin, schooling, and sector, by sex; each subsample is compared with the control group. %s %s", CI_TXT, SRC_TXT)
if (RUN_EVENT_STUDY) {
  # [E14] numero de estados que adoptan, de la propia estimacion (era "six" fijo)
  N_ADOPT <- unique(unlist(lapply(ES, function(e) if (is.null(e)) NULL else e$n_adopt)))
  NUM_EN <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
  NA_TXT <- if (length(N_ADOPT) == 1 && N_ADOPT >= 1 && N_ADOPT <= 10) NUM_EN[N_ADOPT] else
    paste(N_ADOPT, collapse = "/")
  kn("Figure_A2_event_study: Event study around the adoption of universal E-Verify, unincorporated self-employment. Triple-difference coefficients (state-by-year, group-by-state and group-by-year fixed effects) relative to the year before adoption, estimated by two-way fixed effects and by Sun and Abraham (2021); cohorts adopting in 2010-2011 excluded. Bars are 95 percent confidence intervals for the two-way fixed effects estimates, with standard errors clustered by state; with %s adopting states these intervals can be too narrow, and pre-trends are tested in the text with a wild cluster bootstrap. Sun and Abraham estimates are shown without intervals because their clustered standard errors are not reliable with so few adopting states. %s", NA_TXT, SRC_TXT)
}
if (RUN_EXTRAS) {
  kn("Figure_A3_legal_detail (ADICIONAL): Response of unincorporated self-employment by legal status (native-born Hispanics, naturalized citizens, non-citizen legal residents, and likely undocumented immigrants), by sex. %s %s", CI_TXT, SRC_TXT)
  kn("Figure_0_descriptive_gap (ADICIONAL): Gap in unincorporated self-employment between foreign-born Hispanic men and native non-Hispanic men with up to 12 years of schooling, by decile of the enforcement index. Each point averages the state-years in a decile; the dashed line is a weighted linear fit. Descriptive pattern, not a causal estimate. %s", SRC_TXT)
}
writeLines(KN, file.path(OUT_DIR, "key_numbers.txt"), useBytes = TRUE)
writeLines(capture.output(sessionInfo()), file.path(OUT_DIR, "sessionInfo.txt"))

say("\nArchivos en %s:", normalizePath(OUT_DIR))
for (f in list.files(OUT_DIR, recursive = TRUE)) say("  %s", f)
say("\nFin: %s (duracion %s min)", format(Sys.time()),
    f1(as.numeric(difftime(Sys.time(), T0, units = "mins"))))
sink()