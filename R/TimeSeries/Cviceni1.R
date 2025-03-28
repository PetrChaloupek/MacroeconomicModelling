################################################################################
############################## CARA - cvičení 1 ################################
################################################################################

rm(list = ls())
cat("\014")

set.seed(123456) # Nastaveni generatoru nahodnych cisiel, aby byl kod replikovatelny

library(lmtest)
library(forecast)

# 1.1) Definování parametrů procesu ############################################

delka_rady <- 150
a_0 <- 2
a_1 <- 0.4
b_1 <- 0.3
b_2 <- 0.5
sigma_2 <- 1

y <- rep(0, delka_rady)

# Generování vektoru náhodných složek z normálního rozdělení
eps <- rnorm(n = delka_rady, sd = sqrt(sigma_2))

# 1.2) Generování ARMA(1,2) procesu ############################################
for (t in seq(3, delka_rady, by = 1)) {
  y[t] <- a_0 + a_1 * y[t-1] + eps[t] + b_1 * eps[t-1] + b_2 * eps[t-2]
}

y <- y[51:delka_rady]

# Vykreslení časové řady, ACF a PACF
y_graf <- ts.plot(y)
acf_y <- acf(y, lag.max = 20)
pacf_y <- pacf(y, lag.max = 20)

# 1.3) Odhad modelu ############################################################
# Odhad AR(1) modelu
AR_1 <- arima(y, order = c(1,0,0))

print(AR_1)
coeftest(AR_1)
plot(AR_1)

e1 <- residuals(AR_1)
acf_e1 <- acf(e1, lag.max = 20)
pacf_e1 <- pacf(e1, lag.max = 20)

# Odhad ARMA(1,2) modelu
ARMA_12 <- arima(y, order = c(1,0,2))

print(ARMA_12)
coeftest(ARMA_12)
plot(ARMA_12)

e2 <- residuals(ARMA_12)
acf_e2 <- acf(e2, lag.max = 20)
pacf_e2 <- pacf(e2, lag.max = 20)

# 2.1) Nestacionární proces ####################################################
delka_rady <- 150
a_0 <- 1
a_1 <- 1.1
b_1 <- 0.3
b_2 <- 0.5
sigma_2 <- 1

y <- rep(0, delka_rady)

# Generování vektoru náhodných složek z normálního rozdělení
eps <- rnorm(n = delka_rady, sd = sqrt(sigma_2))

# Generování ARMA(1,2) procesu #################################################
for (t in seq(3, delka_rady, by = 1)) {
  y[t] <- a_0 + a_1 * y[t-1] + eps[t] + b_1 * eps[t-1] + b_2 * eps[t-2]
}

y <- y[51:delka_rady]

# Vykreslení časové řady, ACF a PACF
y_graf <- ts.plot(y)
acf_y <- acf(y, lag.max = 20)
pacf_y <- pacf(y, lag.max = 20)

# 2.2) Odhad AR(1) modelu ######################################################
AR_1 <- arima(y, order = c(1,0,0))

print(AR_1)
coeftest(AR_1)
plot(AR_1)

e1 <- residuals(AR_1)
acf_e1 <- acf(e1, lag.max = 20)
pacf_e1 <- pacf(e1, lag.max = 20)