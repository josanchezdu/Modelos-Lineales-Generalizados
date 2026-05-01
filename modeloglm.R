library(ggplot2)
library(data.table)
library(readr)
library(dplyr)
library(urca)
library(GGally)
library(forecast)
library(dplyr)
library(psych)
library(gamlss)
library(performance)
library(glmtoolbox)
library(car)


#getwd()
#setwd("C:/Users/thetr/OneDrive/Documentos/R/modelos")
datos <- fread("ESTRUCTURA CENSO ARBOLADO FASE IV.txt")

#############################################################
# -------------------- BASE DE DATOS ------------------------#
#############################################################

# Reemplazar las categorC-as en fisiologia
datos <- datos %>%
  mutate(V8 = case_when(
    V8 %in% c("semicaducifolio", "Semicaducifolio") ~ "semicaducifolio",
    TRUE ~ V8
  ))

# Verificar el resultado
datos %>%
  count(V8)
summary(datos)

# Lista de las variables categC3ricas
variables_categoricas <- c("V6", "V8", "V9", "V15", "V16", "V21", "V58")

# Iterar sobre cada variable categC3rica
for (var in variables_categoricas) {
  # Verificar si la variable existe en los datos
  if (var %in% colnames(datos)) {
    # Convertir a factor si no lo es
    if (!is.factor(datos[[var]])) {
      datos[[var]] <- factor(datos[[var]])
    }
    
    # Imprimir los niveles de la variable
    cat("\nNiveles de", var, ":\n")
    print(levels(datos[[var]]))
  } else {
    cat("\nLa variable", var, "no existe en los datos.\n")
  }
}


# Filtrar y eliminar los individuos 
datos <- datos[datos$V14 != "F", ]
datos <- datos[datos$V25 != "T", ]
datos <- datos[!is.na(datos$V28), ]
datos <- datos[datos$V45 != "T", ]
datos <- datos[datos$V49 != "T", ]
datos <- datos[datos$V61 != "F", ]
datos <- datos[!is.na(datos$V20), ]


# Inspeccionar los datos cargados
str(datos)
head(datos)


ncol(datos)
nrow(datos)
#lapply(datos, levels)
#summary(datos)

# Seleccionar las variables deseadas usando indexaciC3n base
datos <- datos[, c("V6", "V8", "V9", "V15", "V16", "V21", 
                   "V28", "V30", "V31", "V32", "V58", "V33")]

# Verificar que no haya NA en la variable
sum(is.na(datos))

# NC:mero de valores faltantes por variable
valores_faltantes <- colSums(is.na(datos))
valores_faltantes[valores_faltantes > 0]

datos <- datos[!is.na(datos$V8), ]
#datos <- datos[datos$V8 != "semicaducifolio", ] # se elimina la categoria pues son muy pocos y en el muestreo se eliminan
datos <- datos[datos$V9 != "NN", ]
datos <- datos[datos$V9 != "Ot", ]

# Asignar los nuevos nombres a las columnas de 'datos'
colnames(datos) <- c( "Nom_cientif", "fisiologia", "tipo_arbol", "densidad", "transparen", "formatronc", 
                      "diam_ecuat", "perim_basa", "ang_inclin", "diam_polar", "geolocalizacion", "altura_tot")

summary(datos)


# Eliminar niveles sin observaciones de todas las variables tipo factor en la base de datos
datos <- datos %>% mutate(across(where(is.factor), droplevels))

# Ver la estructura de la tabla seleccionada
str(datos)
#head(datos)
summary(datos)
#lapply(datos, levels)

#datos <- datos %>% filter(fisiologia != "Perennifolio")

# Realizar un muestreo aleatorio sin reemplazo de 500 individuos en datos_seleccionados
#set.seed(521)  # Para reproducibilidad
#datos <- datos[sample(1:nrow(datos), 500), ]

datos_Palma <- datos %>% filter(tipo_arbol == "Palma")
datos_Arbusto <- datos %>% filter(tipo_arbol == "Arbusto")
datos_Arbol <- datos %>% filter(tipo_arbol == "Arbol")

summary(datos_Palma)
summary(datos_Arbusto)
summary(datos_Arbol)

# Ver nC:mero de NA por columna en cada base
colSums(is.na(datos_Palma))
colSums(is.na(datos_Arbusto))
colSums(is.na(datos_Arbol))


# FunciC3n para limpiar categorC-as vacC-as
limpiar_categorias_vacias <- function(df) {
  df <- droplevels(df)  # Elimina los niveles no usados en factores
  return(df)
}

# Aplicar a cada base
datos_Palma <- limpiar_categorias_vacias(datos_Palma)
datos_Arbusto <- limpiar_categorias_vacias(datos_Arbusto)
datos_Arbol <- limpiar_categorias_vacias(datos_Arbol)

# seleccionar variables relevantes, mas de una categoria
datos_Palma <- datos_Palma %>% select(where(~ n_distinct(.) > 1))
datos_Arbusto <- datos_Arbusto %>% select(where(~ n_distinct(.) > 1))
datos_Arbol <- datos_Arbol %>% select(where(~ n_distinct(.) > 1))

##############################
# Evauaci??n bases de datos
##############################
########## ??rboles ###########
##############################

# hare una muestra de tama??o n = 500 de arboles

set.seed(123)
datos_Arbol = datos_Arbol[sample(nrow(datos_Arbol)),]
# datos Arbol sample
dt.A.s = datos_Arbol[1:5000]

summary(datos_Arbol)
summary(dt.A.s) # me gusta esta muestra, se asemejan

pairs.panels(dt.A.s)

colnames(dt.A.s) 

# altura muy correlacionada con fisiologia, diametro, perimetro, polar, y geolocalizacion
# tratare de hacer un gamlss con la gamma generalizada (4p)

formula.1 = altura_tot ~ diam_ecuat + perim_basa + diam_polar
# oooO Max Verstappen

m.1 = gamlss(formula = formula.1,
             family = JSU,
             data = dt.A.s,
             control = gamlss.control(n.cyc = 200))
# convergiC3

plot(m.1)

summary(m.1)

ajuste = function(estimados, reales){
  cat("RMSE",sqrt(mean((reales - estimados)^2)),"\n")
  cat(" MAE",(mean(abs(reales - estimados))),"\n")
}

# medidas de estimacion
ajuste(fitted.values(m.1), dt.A.s$altura_tot); ajuste(predict(m.1, newdata = datos_Arbol, type = "response"), datos_Arbol$altura_tot)

m.1.2 = gamlss(formula = altura_tot ~ diam_ecuat + perim_basa + diam_polar
               ,family = JSU, sigma.formula = ~ transparen + formatronc  + densidad + ang_inclin,
               data = dt.A.s,
               control = gamlss.control(n.cyc = 2000))
#ctoc::toc()

#LR.test(m.1,m.1.2)

# Es significativamente mejor un modelo JSU donde la varianza sea explciada por
# el diametro polar

summary(m.1.2)
plot(m.1.2)

ajuste(fitted.values(m.1.2), dt.A.s$altura_tot); ajuste(predict(m.1.2, newdata = datos_Arbol, type = "response"), datos_Arbol$altura_tot)

##############################
########## Arbustos ##########
##############################



##############################
########### Palmas ###########
##############################


##############################
########## General ###########
##############################


set.seed(123)
datos = datos[sample(nrow(datos)),]
# datos Arbol sample
dt.s = datos[1:5000]

summary(datos_Arbol)
summary(dt.s) # me gusta esta muestra, se asemejan

pairs.panels(dt.s)

#colnames(dt.s) 

# altura muy correlacionada con tipo ??rbol, diametro ecuatorial / polar, perimetro basal
# tratare de hacer un gamlss con la gamma generalizada (4p)

formula.1 = altura_tot ~ diam_ecuat + perim_basa + diam_polar + tipo_arbol
# oooO Max Verstappen

par(mfrow = c(3,1))
plot(datos$diam_ecuat, datos$altura_tot)
plot(datos$diam_polar, datos$altura_tot)
plot(datos$perim_basa, datos$altura_tot)
par(mfrow = c(1,1))

m.1 = gamlss(formula = formula.1,
             family = JSU,
             data = dt.s,
             control = gamlss.control(n.cyc = 200))
plot(m.1)

summary(m.1)

ajuste = function(estimados, reales){
  cat("RMSE",sqrt(mean((reales - estimados)^2)),"\n")
  cat(" MAE",(mean(abs(reales - estimados))),"\n")
}


# Validacion

# Ajusta un GLM temporal para VIF (ya que GAMLSS no lo tiene directo)
glm_temp <- glm(altura_tot ~ diam_ecuat + perim_basa + diam_polar + tipo_arbol, data = dt.s)
vif(glm_temp)  # ningun VIF >5

# medidas de estimacion
ajuste(fitted.values(m.1), dt.s$altura_tot); ajuste(predict(m.1, newdata = datos, type = "response"), datos$altura_tot)

# Worm plot detallado (detecta desviaciones locales)
wp(m.1, ylim.all = 0.5, line = TRUE, n.inter = 9)  # M??s intervalos para precisi??n


#gamlss.demo()
# formplots
term.plot(m.1, pages = 1)



# modela ?? con tipo_arbol (si varianza difiere por tipo)
m.2 <- gamlss(altura_tot ~ diam_ecuat + perim_basa + diam_polar + tipo_arbol,
              sigma.formula = ~ tipo_arbol,  # O ~diam_ecuat si varianza crece con di??metro
              family = JSU, data = dt.s, c.crit = 0.01, n.cyc = 200)
GAIC(m.1, m.2)  # Compara AIC; menor es mejor

summary(m.2)

plot(m.2)
ajuste(fitted.values(m.2), dt.s$altura_tot); ajuste(predict(m.2, newdata = datos, type = "response"), datos$altura_tot)
# Worm plot de los residuos
wp(m.2, ylim.all = 0.5, line = TRUE, n.inter = 9)
#gamlss.demo()
# formplots
term.plot(m.2, pages = 1)



# Modelo m.3: Extiende m.2 modelando nu (asimetr??a) con tipo_arbol
m.3 <- gamlss(altura_tot ~ diam_ecuat + perim_basa + diam_polar + tipo_arbol,
              sigma.formula = ~ tipo_arbol,  # Como en m.2
              nu.formula = ~ tipo_arbol,     # Nueva: asimetr??a var??a por tipo
              family = JSU, 
              data = dt.s, 
              c.crit = 0.01, 
              n.cyc = 200,
              method = mixed(2, 20))  # Recomendado para correlaci??n sigma-nu

# Comparaci??n con m.2 (menor AIC/SBC indica mejor ajuste)
GAIC(m.2, m.3)

# Resumen del modelo
summary(m.3)

# M??tricas de predicci??n (usa tu funci??n ajuste)
ajuste(fitted.values(m.3), dt.s$altura_tot) ; ajuste(predict(m.3, newdata = datos, type = "response"), datos$altura_tot)

# Diagn??sticos: Residuos y worm plot
wp(m.3, ylim.all = 0.5, line = TRUE, n.inter = 9)



# Modelo m.4: Extiende m.3 modelando tau (curtosis) con tipo_arbol
m.4 <- gamlss(altura_tot ~ diam_ecuat + perim_basa + diam_polar + tipo_arbol,
              sigma.formula = ~ tipo_arbol,  # Como en m.2/m.3
              nu.formula = ~ tipo_arbol,     # Como en m.3
              tau.formula = ~ tipo_arbol,    # Nueva: curtosis var??a por tipo
              family = JSU, 
              data = dt.s, 
              c.crit = 0.005,                # Criterio de convergencia m??s estricto
              n.cyc = 200,
              method = mixed(5, 20))         # Para correlaciones entre sigma/nu/tau

# Comparaci??n con m.3 (menor AIC/SBC indica mejora)
GAIC(m.3, m.4)

# Resumen del modelo
summary(m.4)

# M??tricas de predicci??n
ajuste(fitted.values(m.4), dt.s$altura_tot) ; ajuste(predict(m.4, newdata = datos, type = "response"), datos$altura_tot)

# Diagn??sticos
wp(m.4, ylim.all = 0.5)  # Worm plot para desviaciones en colas/curtosis



# --------------------------------------------------------
# COMPARACI??N DE LOS 4 MODELOS
# --------------------------------------------------------
metricas <- function(modelo, datos_train, datos_test, nombre="modelo"){
  
  # estimaci??n
  pred_train <- fitted(modelo)
  real_train <- datos_train$altura_tot
  
  # predicci??n
  pred_test <- predict(modelo, newdata = datos_test, type="response")
  real_test <- datos_test$altura_tot
  
  # m??tricas
  RMSE_train <- sqrt(mean((real_train - pred_train)^2))
  MAE_train  <- mean(abs(real_train - pred_train))
  R2_train   <- 1 - sum((real_train - pred_train)^2) / sum((real_train - mean(real_train))^2)
  
  RMSE_test  <- sqrt(mean((real_test - pred_test)^2))
  MAE_test   <- mean(abs(real_test - pred_test))
  R2_test    <- 1 - sum((real_test - pred_test)^2) / sum((real_test - mean(real_test))^2)
  
  df <- data.frame(
    Modelo = nombre,
    AIC = AIC(modelo),
    BIC = BIC(modelo),
    logLik = logLik(modelo),
    RMSE_train, MAE_train, R2_train,
    RMSE_test,  MAE_test,  R2_test
  )
  
  # redondeo a 3 decimales para todas las columnas num??ricas
  df[, -1] <- round(df[, -1], 3)
  return(df)
}


res_comp <- rbind(
  metricas(m.1, dt.s, datos, "m.1 (solo ??)"),
  metricas(m.2, dt.s, datos, "m.2 (?? + ??)"),
  metricas(m.3, dt.s, datos, "m.3 (?? + ?? + ??)"),
  metricas(m.4, dt.s, datos, "m.4 (?? + ?? + ?? + ??)")
)

print(res_comp)
