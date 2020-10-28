################## PREDICCION SALARIOS NBA ###################

library(readr)
nba <- read.csv('./nba.csv')

# Creacion modelo lineal
mod1 <- lm(Salary ~ NBA_Country + NBA_DraftNumber + 
             Age + Tm + G + MP + PER + TS. + X3PAr +
             FTr + ORB. + DRB. + TRB. + AST. + STL. +
             BLK. + TOV. + USG. + OWS + DWS + WS + WS.48 +
             OBPM + DBPM + BPM + VORP, data=nba)
head(nba, 5)
summary(mod1)

# Seleccion de un nuevo modelo mediante criterio AIC
library(MASS)
stepAIC(object = mod1, direction = "both")

# Creacion del nuevo modelo lineal
mod2 <- lm(Salary ~ NBA_DraftNumber + Age + G + MP + PER + 
             X3PAr + ORB. + TRB. + USG. + WS + OBPM, data=nba)
summary(mod2)

# Predicción
nba$SalaryPredict <- predict(mod2, newdata=nba)

# Validacion
mod3 <- lm(Salary ~ NBA_DraftNumber + Age + G + MP + PER + 
             X3PAr + ORB. + TRB. + USG. + WS + OBPM, data=nba[1:339,])

nba$Validation <- predict(mod3, newdata=nba)

nba <- na.omit(nba) # Eliminamos ciertos valores NA
mean(1/nrow(nba)*((nba$SalaryPredict/nba&Validation)^2)) # Calculamos error cuadratico medio
