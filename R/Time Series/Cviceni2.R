################################################################################
############################## CARA - cvičení 2 ################################
################################################################################

rm(list = ls())
cat("\014")

library(forecast)
library(readxl)
library(lmtest)
library(ggfortify)
library(dynlm)

# 1) Načtění časové řady #######################################################

quarterly <- readxl::read_xls(paste(getwd(),"/quarterly.xls", sep = ""))

ts_quarterly <- ts(quarterly, start = c(1960,1), frequency = 4)

log_indprod <- log(ts_quarterly[,"indpro"])
y <- diff(log_indprod)

# Vykreslení log_indprod a y
par(mfrow = c(1,3))
ts.plot(log_indprod, col = "green")
ts.plot(y, col = "red")
ts.plot(ts_quarterly[,"indpro"], col = "blue")

# 2) Autokorelační fce proměnné ################################################
par(mfrow = c(2,2)) #rozdeleni plochy pro vykresleni na 4 casti 
acf(log_indprod, lag.max = 20, col = "green")
pacf(log_indprod, lag.max = 20, col = "green")
acf(y, lag.max = 20, col = "red")
pacf(y, lag.max = 20, col = "red")

# 3) Odhad AR(1) modelu ########################################################
Model_AR1 <- arima(y, order = c(1, 0, 0))

print(Model_AR1)
coeftest(Model_AR1)

# Vykreslení INVERZNÍCH kořenu polynomu AR a MA části 
# (pro stabilitu musí ležet uvnitř jednotkové kružnice)
par(mfrow = c(1,1))
plot(Model_AR1)

# Diagnostika modelu - ověření nekorelovanosti reziduí
par(mfrow = c(1,2))

e <- residuals(Model_AR1)

acf(e, lag.max = 20, col = "brown")
pacf(e, lag.max = 20, col = "navy")

# Ljung-Box Q-test
Box.test(e, lag = 8, type = c("Ljung-Box"), fitdf = 1)

max_test = 20
p = 1
for (my_lag in seq(p + 1, max_test, by = 1)){
  pom_test <- Box.test(e, lag = my_lag, type = c("Ljung-Box"), fitdf = p)
  sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ", 
          my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}  

# 4) Odhad AR(1) procesu s MA(8) členem #########################################
Model_ARMA_18 <- arima(y, order = c(1, 0, 8), fixed = c(NA,0,0,0,0,0,0,0,NA,NA))

print(Model_ARMA_18)
coeftest(Model_ARMA_18)
plot(Model_ARMA_18)

par(mfrow = c(1,2))
e <- residuals(Model_ARMA_18)
acf(e, lag.max = 20)
pacf(e, lag.max = 20)

Box.test(e, lag = 8, type = c("Ljung-Box"), fitdf = 1)

max_test = 20
p = 1
for (my_lag in seq(p + 1, max_test, by = 1)){
  pom_test <- Box.test(e, lag = my_lag, type = c("Ljung-Box"), fitdf = p)
  sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ", 
          my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}  

# 5) Odhad a vyhodnocení různých ARMA(p,q) modelů ##############################
vyber_model_aic <- forecast::auto.arima( y, d = 0, D = 0, max.p = 3, max.q = 3,
                                         max.P = 0, max.Q = 0, stepwise = FALSE,
                                         max.d = 0, max.D = 0, start.p = 0,
                                         start.q = 0, ic = "aic" )

vyber_model_bic <- forecast::auto.arima( y, d = 0, D = 0, max.p = 3, max.q = 3,
                                         max.P = 0, max.Q = 0, stepwise = FALSE,
                                         max.d = 0, max.D = 0, start.p = 0,
                                         start.q = 0, ic = "bic" )
vyber_model_aic
vyber_model_bic

# 6) Najít vhodný model pro hodnoty nezaměstnanosti ############################
log_urate <- log(ts_quarterly[,"urate"])
y <- diff(log_urate)

par(mfrow = c(1,2))
ts.plot(log_urate, col = "green")
ts.plot(y, col = "red")

par(mfrow = c(2,2))
acf(log_urate, lag.max = 20, col = "green")
pacf(log_urate, lag.max = 20, col = "green")
acf(y, lag.max = 20, col = "red")
pacf(y, lag.max = 20, col = "red")


Model_AR1 <- arima(y, order = c(1, 0, 0))

print(Model_AR1)
coeftest(Model_AR1)

par(mfrow = c(1,1))
plot(Model_AR1)

par(mfrow = c(1,2))
e <- residuals(Model_AR1)
acf(e, lag.max = 20, col = "brown")
pacf(e, lag.max = 20, col = "navy")


Model_ARMA_18 <- arima(y, order = c(1, 0, 8), fixed = c(NA,0,0,0,0,0,0,0,NA,NA))

print(Model_ARMA_18)
coeftest(Model_ARMA_18)
plot(Model_ARMA_18)

par(mfrow = c(1,2))
e <- residuals(Model_ARMA_18)
acf(e, lag.max = 20)
pacf(e, lag.max = 20)

vyber_model_aic <- forecast::auto.arima( y, d = 0, D = 0, max.p = 3, max.q = 3,
                                         max.P = 0, max.Q = 0, stepwise = FALSE,
                                         max.d = 0, max.D = 0, start.p = 0,
                                         start.q = 0, ic = "aic" )

vyber_model_bic <- forecast::auto.arima( y, d = 0, D = 0, max.p = 3, max.q = 3,
                                         max.P = 0, max.Q = 0, stepwise = FALSE,
                                         max.d = 0, max.D = 0, start.p = 0,
                                         start.q = 0, ic = "bic" )
vyber_model_aic
vyber_model_bic

Model_ARMA_03 <- arima(y, order = c(0,0,3), fixed = c(0,0,0,NA,NA))

print(Model_ARMA_03)
coeftest(Model_ARMA_03)
plot(Model_ARMA_03)