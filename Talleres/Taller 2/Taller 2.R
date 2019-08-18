###########################################
#### TALLER 2 - ESTIMADORES CALIBRADOS ####
###########################################

# diseño: estratificado por conglomerados en dos etapas 
# zona censal (manzana) no está disponible
# está disponible el segmento censal (grupo de 12 manzanas)
# 01      01        001           001
# depto   seccion   segmento      zona
#                   solo mvdeo
# para el interior en lugar del segmento, tenemos la localidad

library(foreign)
library(survey)
library(dplyr)
library(xlsx)
load(file = "Talleres/Taller 2/taller2.rdata")

#### RATIO ####

# personas <- read.spss("Talleres/Taller 2/P_2016_terceros.sav", to.data.frame=T, use.value.labels=F)
personas <- personas[, c(1:5,8,9,156:162,403,418:ncol(personas))]

# Para crear el segmento se debe concatenar dpto, secc, segm
personas$dpto <- ifelse(nchar(personas$dpto==1),
                        paste0("0", as.character(personas$dpto)), 
                        as.character(personas$dpto))
personas$codsegm <- paste0(personas$dpto, personas$secc, personas$segm)
personas$des <- ifelse(personas$pobpcoac==3 | 
                         personas$pobpcoac==4 | 
                         personas$pobpcoac==5, 1, 0)
personas$pea <- ifelse(personas$pobpcoac==2 | 
                         personas$pobpcoac==3 | 
                         personas$pobpcoac==4 | 
                         personas$pobpcoac==5, 1, 0)
# findInterval usa < y \leq
personas$edad <- findInterval(personas$e27, vec=c(14,60))
personas$edad <- as.factor(personas$edad)
levels(personas$edad) <- c("menoresde14","entre14y59","masde60")
personas$e26 <- as.factor(personas$e26)
levels(personas$e26) <- c("Masculino","Femenino")

mvdeo <- personas[personas$dpto == "01",]
mvdeo$edad <- as.factor(mvdeo$edad)
levels(mvdeo$edad) <- c("menoresde14","entre14y59","masde60")
ps <- survey::svydesign(id=~codsegm, strata=~estred13, weights=~pesoano,
                        data=mvdeo)
summary(ps)
svyratio(~des, ~pea, ps)
confint(svyratio(~des, ~pea, ps))

#### CALIBRADO ####

# total <- read.xlsx("Talleres/Taller 2/proyecciones_revision_2005/Totpais_deptos_edad_ambos_1996-2025.xls", 1)
# mdeo <- read.xlsx("Talleres/Taller 2/proyecciones_revision_2005/Totpais_deptos_edad_ambos_1996-2025.xls", 11)

mdeoH2016 <- sum(mdeo[32:51, 22])
mdeoM2016 <- sum(mdeo[56:74, 22])

mdeoE12016 <- sum(mdeo[32:34, 22]) + sum(mdeo[56:58, 22])
mdeoE32016 <- sum(mdeo[45:51, 22]) + sum(mdeo[68:74, 22])
mdeoE22016 <- sum(mdeo[32:51, 22]) + sum(mdeo[56:74, 22]) - mdeoE12016 - mdeoE32016

# calibrate: dise?o, variables, pop.totals
per.mc0 <- calibrate(ps, ~e26+edad, c("(Intercept)"=mdeoH2016+mdeoM2016,
                                      e26Femenino=mdeoM2016,  
                                      edadentre14y59=mdeoE22016, 
                                      edadmasde60=mdeoE32016))
summary(per.mc0)

plot(weights(ps), weights(per.mc0), pch=16, col="red")
abline(0,1)

library(car)
mvdeo$wk <- weights(per.mc0)

scatterplot(wk~pesoano|e26, data=mvdeo, smooth=FALSE, reg.line=FALSE)
abline(0, 1)

scatterplot(wk~pesoano|edad, data=mvdeo, smooth=FALSE, reg.line=FALSE)
abline(0, 1)

################################
#### FIN DE LA PROGRAMACI?N ####
################################