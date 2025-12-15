################################################################################
#################### CASOVE RADY - ZAVERECNY PROJEKT ###########################
################################################################################

rm(list = ls())
cat("\014")

# Nacteni potrebnych balicku
library(dplyr)
library(ggplot2)
library(tidyverse)
library(zoo)
library(forecast)
library(fredr)
library(gridExtra)

# Import dat ###################################################################

# Ceny bydlení
property_prices <- fredr(
  series_id = "QCAN628BIS",
  observation_start = as.Date("1970-01-01"),
  observation_end = as.Date("2020-01-01"),
  frequency = "q"
)

# Smenny kurz (USD/CAD)
XR <- fredr(
  series_id = "CCUSMA02CAM618N",
  observation_start = as.Date("1970-01-01"),
  observation_end = as.Date("2020-01-01"),
  frequency = "q"
)

# Menove politicka urokova mira
IR <- fredr(
  series_id = "IRSTCB01CAM156N",
  observation_start = as.Date("1970-01-01"),
  observation_end = as.Date("2020-01-01"),
  frequency = "q"
)

# Index spotrebitelskych cen
CPI <- fredr(
  series_id = "CPALCY01CAM661N",
  observation_start = as.Date("1970-01-01"),
  observation_end = as.Date("2020-01-01"),
  frequency = "q"
)

# Realny Hruby domaci produkt
GDP <- fredr(
  series_id = "NGDPRSAXDCCAQ",
  observation_start = as.Date("1970-01-01"),
  observation_end = as.Date("2020-01-01"),
  frequency = "q"
)

# Menovy agregat M2
M <- fredr(
  series_id = "MAM2A2CAM189S",
  observation_start = as.Date("1970-01-01"),
  observation_end = as.Date("2020-01-01"),
  frequency = "q"
)

# Index ceny prace
WPI <- fredr(
  series_id = "CANHOUREAMISMEI",
  observation_start = as.Date("1970-01-01"),
  observation_end = as.Date("2020-01-01"),
  frequency = "q"
)

# Vykresleni dat ##############################################################
datasets <- list(
  list(
    data = property_prices,
    title = "Ceny bydlení",
    color = "darkblue",
    y_label = "Index cen (2010 = 100)"
  ),
  list(
    data = XR,
    title = "Směnný kurz",
    color = "darkred",
    y_label = "USD/CAD"
  ),
  list(
    data = IR,
    title = "Měnově politická úroková míra",
    color = "darkgreen",
    y_label = "%"
  ),
  list(
    data = CPI,
    title = "Index spotřebitelských cen",
    color = "purple",
    y_label = "CPI (2015 = 100)"
  ),
  list(
    data = GDP,
    title = "Reálný HDP",
    color = "orange",
    y_label = "Miliony CAD (stálé ceny)"
  ),
  list(
    data = M,
    title = "Měnový agregát M2",
    color = "brown",
    y_label = "Miliony CAD"
  ),
  list(
    data = WPI,
    title = "Index ceny práce",
    color = "darkturquoise",
    y_label = "Index (2015 = 100)"
  )
)

plots <- list()
for (i in 1:length(datasets)) {
  plots[[i]] <- ggplot(datasets[[i]]$data, aes(x = date, y = value)) +
    geom_line(color = datasets[[i]]$color, size = 1) +
    labs(
      title = datasets[[i]]$title,
      x = "Rok",
      y = datasets[[i]]$y_label
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8)
    )
}

grid.arrange(
  grobs = plots,
  ncol = 2,
  top = "Původní časové řady"
)

# Transformace dat #############################################################



# Spojeni dat do jednoho data framu
data_all <- property_prices %>%
  select(date, value) %>%
  rename(Property = value) %>%
  left_join(XR %>% select(date, value) %>% rename(XR = value), by = "date") %>%
  left_join(IR %>% select(date, value) %>% rename(IR = value), by = "date") %>%
  left_join(CPI %>% select(date, value) %>% rename(CPI = value), by = "date") %>%
  left_join(GDP %>% select(date, value) %>% rename(GDP = value), by = "date") %>%
  left_join(M %>% select(date, value) %>% rename(M2 = value), by = "date") %>%
  left_join(WPI %>% select(date, value) %>% rename(WPI = value), by = "date")

# Logaritmicka transformace (krome urokove miry)
data_log <- data_all %>%
  mutate(
    lProperty = log(Property),
    lXR = log(XR),
    lCPI = log(CPI),
    lGDP = log(GDP),
    lM2 = log(M2),
    lWPI = log(WPI)
    # IR nechavame v puvodnich hodnotach (urokove miry se obvykle nelogaritmuji)
  )

# Zobrazeni prvnich radek logaritmovanych dat
head(data_log)

# Vykresleni logaritmovanych dat
log_datasets <- list(
  list(data = data_log, var = "lProperty", title = "Log Ceny bydlení", color = "darkblue", y = "Log Index"),
  list(data = data_log, var = "lXR", title = "Log Směnný kurz", color = "darkred", y = "Log USD/CAD"),
  list(data = data_log, var = "IR", title = "Měnově politická úroková míra (Levels)", color = "darkgreen", y = "%"), # IR nelogaritmovano
  list(data = data_log, var = "lCPI", title = "Log Index spotřebitelských cen", color = "purple", y = "Log CPI"),
  list(data = data_log, var = "lGDP", title = "Log Reálný HDP", color = "orange", y = "Log Miliony CAD"),
  list(data = data_log, var = "lM2", title = "Log Měnový agregát M2", color = "brown", y = "Log Miliony CAD"),
  list(data = data_log, var = "lWPI", title = "Log Index ceny práce", color = "darkturquoise", y = "Log Index")
)

log_plots <- list()
for (i in 1:length(log_datasets)) {
  log_plots[[i]] <- ggplot(log_datasets[[i]]$data, aes_string(x = "date", y = log_datasets[[i]]$var)) +
    geom_line(color = log_datasets[[i]]$color, size = 1) +
    labs(
      title = log_datasets[[i]]$title,
      x = "Rok",
      y = log_datasets[[i]]$y
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8)
    )
}

grid.arrange(
  grobs = log_plots,
  ncol = 2,
  top = "Logaritmované časové řady"
)
