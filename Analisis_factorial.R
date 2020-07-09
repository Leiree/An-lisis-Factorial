

rm(list=ls())

###############################################################################
# Librerias
###############################################################################

library(corpcor);
library(GPArotation);
library(psych)

library(readr)
library(readxl)

library(ggplot2)
library(dplyr)

library(tidyr)
library(likert)

library(FactoMineR)
library(factoextra)

# Análisis confirmatorio
library(lavaan)

###############################################################################
# Recodificación de variables
###############################################################################

# la variable sexo: 1 = Hombre, 2 = Mujer
df$sexo <- factor( df$sexo, labels = c( "Hombre", "Mujer" ) )
# actividadS: 1 = Nada, 2 = Poco, 3 = Mucho, 4 = Muchísimo
df$actividadS <- factor( df$actividadS, labels = c( "Nada", "Poco", "Mucho", "Muchísimo" ) )
# la variable origen: A = Albacete, M = Murcia, H = Helsinki
df$origen <- factor( df$origen, labels = c( "Albacete", "Helsinki", "Murcia" ) )
# Cambiamos el orden para que Murcia sea la segunda
df$origen <- factor( df$origen, levels = levels( df$origen )[ c( 1, 3, 2 ) ] )
# la variable nivel de inglés: nos dan los niveles del 1 al 10
nivelesIngles  <- c( "Nulo", "CasiNulo", "A1", "A2", "B1", "B2", "C1", "C2"
                     , "IsabelII", "Shakespeare" )
df$nivelIngles <- factor( df$nivelIngles, labels = nivelesIngles )

# se convierten en factores
nivelesQ     <- c( "Muy en desacuerdo", "En desacuerdo", "Neutro", "De acuerdo", "Muy de acuerdo" )
df[ , 7:29 ] <- lapply( df[ , 7:29 ], factor, labels = nivelesQ )
# recomendable: evitar índices númericos
questions         <- c( paste0( "Q0", 1:9 ), paste0( "Q", 10:23 ) )
df[ , questions ] <- lapply( df[ , questions ], factor, labels = nivelesQ )


###############################################################################
# Cargar los datos
###############################################################################

setwd("G:/Mi unidad/Análisis factorial/Codigo")
load( "saeraq.RData" )

###############################################################################
# Analisis exploratorio de los datos
###############################################################################

summary(df)
str(df)
dim(df) # 2571 * 24


###############################################################################
# Analisis exploratorio
###############################################################################

dfLikert <- df[ , grep( "^Q", colnames( df ) ) ]
colnames( dfLikert ) <- dicc[ grep( "^Q", dicc$item ), "spanish" ]

bloque1 <- 1:8
bloque2 <- 9:17
bloque3 <- 18:23

items1 <- likert( items = dfLikert[ , bloque1 ] )
items2 <- likert( items = dfLikert[ , bloque2 ] )
items3 <- likert( items = dfLikert[ , bloque3 ] )

# Gráfico de barras
plot( items1, centered = TRUE, group.order = colnames( items1$items ),
      legend.position = "right" ) +
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) )

# Graficos de densidad
plot( items1, type = "density" )

# Graficos de calor
plot( items1, type = "heat", group.order = colnames( items1$items ) ) +
  theme( axis.text.x = element_text( size = 8 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) )


###############################################################################
# Variables descriptivas
###############################################################################

# Boxplot
ggplot( df, aes( x = nivelIngles, y = ingresos, colour = sexo ) ) +
  geom_boxplot() +
  facet_grid( origen ~ . )

# Grafico de perfiles
mediasNivelIngles <- df %>% 
  select( origen, nivelIngles, sexo, ingresos ) %>% 
  group_by( origen, nivelIngles, sexo ) %>% 
  summarise( ingresos = mean( ingresos ) )

ggplot( mediasNivelIngles, aes( x = nivelIngles, y = ingresos, group = sexo, colour = sexo ) ) +
  geom_point() +
  geom_line() +
  facet_grid( . ~ origen ) +
  theme_bw() +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

# Grafico de barras
ggplot( df, aes( x = Q01, fill = sexo ) ) +
  geom_bar( position = "dodge" ) +
  theme_minimal()

# Grafico de barras
df %>% 
  select( sexo, Q01:Q23 ) %>% 
  gather( "pregunta", "valor", Q01:Q23 ) %>%
  ggplot( aes( x = valor, fill = sexo ) ) +
  geom_bar( position = "dodge" ) +
  facet_wrap( ~ pregunta ) +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

###############################################################################
# Correlaciones
###############################################################################

# Solo cojo las preguntas
dfCor <- df[ , grep( "^Q", colnames( df ) ) ]
# Las convierto en numericas
dfCor <- lapply( dfCor, as.numeric ) # devuelve una lista 
# Lo convierto en data frame
dfCor <- as.data.frame( dfCor )      # coarcionamos a data.frame
# Calculo la correlacion
corr <- cor( dfCor )
# Visualizo con dos decimales
head(round(corr, 2))
# Grafico las correlacioens
corrplot.mixed( corr, tl.pos = "lt", diag = 'n', upper = "ellipse",
                number.cex = 0.4, tl.cex = 0.8,
                order = "hclust" )


###############################################################################
# Análisis de Componentes Principales
###############################################################################

# ¿Cuantos factores?
indicesPreg <- grep( "^Q", colnames( df ) )
df[ , indicesPreg ] <- as.data.frame( lapply( df[ , indicesPreg ], as.numeric ) )


pca <- PCA( df, quali.sup = c( 1:3, 5:6 ), quanti.sup = 4,
            scale.unit = TRUE, graph = FALSE )

fviz_screeplot( pca, addlabels = TRUE ) + 
  ggtitle("") +
  scale_x_discrete( name = "Componentes principales" ) +
  scale_y_continuous( name = "Autovalor y varianza explicada" ) +
  theme_minimal()

# Graficamos
fviz_pca_var( pca, axes = c( 1, 2 ), geom = "text", alpha.var = "contrib",
              labelsize = 2 ) +
  ggtitle("") +
  theme_minimal()

fviz_pca_biplot( pca, pointsize = 1.5, labelsize = 2.5, #alpha.var="contrib",
                 habillage = df$sexo, addEllipses = TRUE,
                 ellipse.level = 0.95, select.var = list( contrib = 25 ) ) +
  ggtitle("") +
  theme_minimal()


# Otra forma mas sencilla
pc1 <- principal(dfCor, nfactors=23, rotate="none")
pc1

# Plot
plot(pc1$values, type="b") 

###############################################################################
# Comprobaciones antes de empezar el análisi
###############################################################################

# Determinante de la matriz
corrMatrix <- cor(dfCor)
det(corrMatrix)

# Multicolinealidad
cat("Nº de variables que podrían causar multicolinealidad: ",
    sum( sapply(as.data.frame(corrMatrix), function(x) any(x >= 0.9 & x != 1)) ) )

# Poca correlación
cat("Nº de variables que no correlacionan bien: ",
    sum( sapply(as.data.frame(corrMatrix), function(x) all(x < 0.3)) ) )

# Test de Bartlett
cortest.bartlett(corrMatrix, nrow(dfCor))

# Coeficiente Kaiser-Meyer-Olkin(KMO)
# kmo(dfCor)[1:3]
# Ver url

###############################################################################
# Análisis de Componentes Principales
###############################################################################

# 4 factores + Rotación
pc3 <- principal(corrMatrix, nfactors=5, rotate="varimax")
print.psych(pc3, cut = 0.3, sort = TRUE)

###############################################################################
# Alpha de Cronbach's - Análisis de Fiabilidad
###############################################################################

# Agrupamos las variables en sus respectivos grupos
miedoComputadores <- c(6, 7, 10, 13, 14, 15, 18)
miedoEstadistica <- c(1, 3, 4, 5, 12, 16, 20, 21)
miedoMatematicas <- c(8, 11, 17)
evaluacionPares <- c(2, 9, 19, 22, 23)
library( psych )
psych::alpha( dfCor[ , miedoComputadores] )

psych::alpha( dfCor[ , miedoEstadistica] )

psych::alpha( dfCor[ , miedoMatematicas] )

psych::alpha( dfCor[ , evaluacionPares] )

############################################################################################
### Analisis confirmatorio
#############################################################################################

model <- "
# latent variable definitions
miedoComputadores =~ Q06 + Q07 + Q10 + Q13 + Q14 + Q15 + Q18

miedoEstadistica =~ Q01+ Q03 + Q04 + Q05 + Q12 + Q16 + Q20 + Q21

miedoMatematicas =~ Q08 + Q11 + Q17

evaluacionPares =~ Q02 + Q09 + Q19 + Q22 + Q23

# covariance between factor_1 and factor_2

# residual covariances

"

models <- list()
models$m1 <- cfa(model, data = dfCor)

summary(models$m1, fit.measures = TRUE, modindices = TRUE)
fitMeasures(models$m1) 
standardizedsolution(models$m1)

library(semPlot)
library(ggm)
semPaths(models$m1, curvePivot = TRUE)

###############################################################################
# Biografia
###############################################################################


# http://gauss.inf.um.es/8jornadasR/
# http://gauss.inf.um.es/8jornadasR/filestaller/T4_analisisAF.pdf
# https://gaopinghuang0.github.io/2018/02/09/exploratory-factor-analysis-notes-and-R-code


# http://www.rpubs.com/marcelo-chavez/multivariado_1
# http://www.diegocalvo.es/analisis-factorial-en-r/
# https://rstudio-pubs-static.s3.amazonaws.com/297667_18cdc2e42898492aaa9dcf38c6a87b08.html
# https://rpubs.com/PacoParra/293407

# https://rpubs.com/merm/EFA_shiny_app
# http://www.real-statistics.com/multivariate-statistics/factor-analysis/basic-concepts-factor-analysis/

# KMO
# https://tolstoy.newcastle.edu.au/R/e2/help/07/08/22816.html
                
# Teoria muy buena
http://www.fuenterrebollo.com/Economicas/ECONOMETRIA/MULTIVARIANTE/FACTORIAL/analisis-factorial.pdf
