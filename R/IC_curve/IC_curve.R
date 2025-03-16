################################################################################
###################### IC CURVE FOR CZECH REPUBLIC #############################
################################################################################

rm(list = ls())
cat("\014")

# Load libraries
library(readxl)
library(here)
library(mFilter)
library(AER)
library(ggplot2)

# Import data
file_path <- file.path("df.xlsx")

df <- read_excel(file_path)
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric")

# Transform data



df$l_GDP_CZ <- 100*log(df$GDP_CZ)
df$l_GDP_GER <- 100*log(df$GDP_GER)
df$l_s <- 100*log(df$XR)
df$l_GDP_CZ_trnd <- hpfilter(df$l_GDP_CZ, freq = 1600)$trend
df$l_GDP_CZ_gap <- 100 * (df$l_GDP_CZ - df$l_GDP_CZ_trnd) / df$l_GDP_CZ_trnd
df$l_GDP_GER_trnd <- hpfilter(df$l_GDP_GER, freq = 1600)$trend
df$l_GDP_GER_gap <- 100 * (df$l_GDP_GER - df$l_GDP_GER_trnd) / df$l_GDP_GER_trnd
df$r <- df$IR_CZ - dplyr::lead(df$CPI_CZ, 1)
df$z <- df$XR * (df$CPI_GER / df$CPI_CZ)
df$l_z <- 100 * log(abs(df$z))*sign(df$z)
df$GDP_CZ_gap_lag <- dplyr::lag(df$l_GDP_CZ_gap, 1)
df$GDP_GER_gap_lag <- dplyr::lag(df$l_GDP_GER_gap, 1)
df$r_lag <- dplyr::lag(df$r, 1)
df$l_z_lag <- dplyr::lag(df$l_z, 1)







ggplot()+
  geom_line(data = df, aes(x = Date, y = z), color ="orange")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0,5, hjust = 1))

