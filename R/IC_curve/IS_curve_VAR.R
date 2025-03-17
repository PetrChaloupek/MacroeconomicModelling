



rm(list = ls())
cat("\014")

# Load libraries
library(readxl)
library(here)
library(mFilter)
library(AER)
library(ggplot2)
library(dplyr)
library(vars)
library(tseries)

# Import data
file_path <- file.path("df.xlsx")
df <- read_excel(file_path, 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", "numeric"))

# Transform data
df$l_GDP_CZ <- 100 * log(df$GDP_CZ)
df$l_GDP_GER <- 100 * log(df$GDP_GER)
df$l_s <- 100 * log(df$XR)

# Output gaps
df$l_GDP_CZ_trnd <- hpfilter(df$l_GDP_CZ, freq = 1600)$trend
df$l_GDP_CZ_gap <- df$l_GDP_CZ - df$l_GDP_CZ_trnd
df$l_GDP_GER_trnd <- hpfilter(df$l_GDP_GER, freq = 1600)$trend
df$l_GDP_GER_gap <- df$l_GDP_GER - df$l_GDP_GER_trnd

# Inflation and real interest rate
df$cpi_cz_index <- cumprod(1 + df$CPI_CZ / 100)
df$cpi_ger_index <- cumprod(1 + df$CPI_GER / 100)
df$pi_cz <- c(NA, 400 * diff(log(df$cpi_cz_index), lag = 1))
df$r <- df$IR_CZ - lead(df$pi_cz, 1)
df$r_trnd <- NA
df$r_trnd[!is.na(df$r)] <- hpfilter(na.omit(df$r), freq = 1600)$trend
df$r_gap <- df$r - df$r_trnd

# Real exchange rate
df$z <- df$XR * (df$cpi_ger_index / df$cpi_cz_index)
df$l_z <- 100 * log(df$z)
df$l_z_trnd <- hpfilter(df$l_z, freq = 1600)$trend
df$l_z_gap <- df$l_z - df$l_z_trnd

# Lead and lag variables (pro informaci, ale v VAR je nepoužijeme přímo)
df$GDP_CZ_gap_lead <- lead(df$l_GDP_CZ_gap, 1)
df$GDP_CZ_gap_lag <- lag(df$l_GDP_CZ_gap, 1)

# Příprava dat pro VAR
df_var <- df[, c("l_GDP_CZ_gap", "r_gap", "l_z_gap", "l_GDP_GER_gap")]
df_var <- na.omit(df_var)  # Odstranění NA

# Odhad VAR modelu
var_model <- VAR(df_var, p = 1, type = "const")  # p = 1 lag, můžeš zkusit více
summary(var_model)

# Impulzní odezvy (IRFs)
irf_result <- irf(var_model, impulse = c("r_gap", "l_z_gap", "l_GDP_GER_gap"), 
                  response = "l_GDP_CZ_gap", n.ahead = 20, boot = TRUE)
plot(irf_result)
roots(var_model)


adf.test(df_var$l_GDP_CZ_gap)
adf.test(df_var$r_gap)