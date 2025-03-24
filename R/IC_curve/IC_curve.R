################################################################################
###################### IS CURVE FOR CZECH REPUBLIC #############################
################################################################################

rm(list = ls())
cat("\014")

# Load libraries
library(readxl)
library(here)
library(mFilter)
library(AER)
library(ggplot2)
library(dplyr)



# Import dataą
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

# Lead and lag variables
df$GDP_CZ_gap_lead <- lead(df$l_GDP_CZ_gap, 1)
df$GDP_CZ_gap_lag <- lag(df$l_GDP_CZ_gap, 1)

###########
library(mgcv)
is_gam <- gam(l_GDP_CZ_gap ~ s(GDP_CZ_gap_lead) + s(GDP_CZ_gap_lag) + s(r_gap) + 
                s(l_z_gap) + s(l_GDP_GER_gap), data = df, method = "REML")
summary(is_gam)
plot(is_gam, pages = 1)  # Vizualizace nelineárních efektů


#############
# Nelineární odhad pomocí GAM
library(mgcv)
df_clean <- na.omit(df)  # Odstranění NA pro regresi
is_gam <- gam(l_GDP_CZ_gap ~ s(GDP_CZ_gap_lead) + s(GDP_CZ_gap_lag) + 
                s(r_gap) + s(l_z_gap) + s(l_GDP_GER_gap), 
              data = df_clean, method = "REML")
summary(is_gam)
plot(is_gam, pages = 1)  # Vizualizace nelineárních efektů






ggplot()+
  geom_line(data = df, aes(x = Date, y = z), color ="orange")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0,5, hjust = 1))










library(ggThemeAssist)
ggThemeAssistGadget()


