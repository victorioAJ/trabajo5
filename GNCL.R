# GNCL: Generador de Números Con Congruencia Lineal

# Tabla de parámetros por país
parametros_paises <- list(
  "Perú"     = list(talla = c(1.62, 0.07), peso = c(68, 10)),
  "Suecia"   = list(talla = c(1.75, 0.08), peso = c(75, 11)),
  "EEUU"     = list(talla = c(1.70, 0.09), peso = c(80, 15)),
  "México"   = list(talla = c(1.64, 0.07), peso = c(75, 12)),
  "España"   = list(talla = c(1.70, 0.08), peso = c(72, 10)),
  "Argentina"= list(talla = c(1.69, 0.07), peso = c(74, 11)),
  "Japón"    = list(talla = c(1.67, 0.06), peso = c(62, 9)),
  "China"    = list(talla = c(1.66, 0.06), peso = c(65, 10)),
  "India"    = list(talla = c(1.65, 0.06), peso = c(66, 9)),
  "Brasil"   = list(talla = c(1.68, 0.07), peso = c(70, 11)),
  "Alemania" = list(talla = c(1.74, 0.08), peso = c(76, 10)),
  "Francia"  = list(talla = c(1.72, 0.08), peso = c(73, 10)),
  "Italia"   = list(talla = c(1.71, 0.08), peso = c(72, 9))
)

# Función para generar talla en metros
generar_talla <- function(n = 1, pais = "global") {
  if (pais %in% names(parametros_paises)) {
    mu <- parametros_paises[[pais]]$talla[1]
    sd <- parametros_paises[[pais]]$talla[2]
  } else {
    mu <- 1.68
    sd <- 0.08
  }
  return(round(rnorm(n, mean = mu, sd = sd), 2))
}

# Función para generar peso en kilogramos
generar_peso <- function(n = 1, pais = "global") {
  if (pais %in% names(parametros_paises)) {
    mu <- parametros_paises[[pais]]$peso[1]
    sd <- parametros_paises[[pais]]$peso[2]
  } else {
    mu <- 72
    sd <- 12
  }
  return(round(rnorm(n, mean = mu, sd = sd), 1))
}

# Función para generar temperatura corporal en grados Celsius (discreta)
generar_temperatura <- function(n = 1) {
  return(sample(34:39, n, replace = TRUE))
}
