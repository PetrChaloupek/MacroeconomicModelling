################################################################################
########################## HYBATELÉ ČESKÉHO EXPORTU ############################
############################### Petr Chaloupek #################################
################################################################################

rm(list = ls())
cat("\014")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Načtení knihoven #############################################################

library(tidyverse)
library(readxl)
library(fredr)
library(ggplot2)
library(dplyr)
library(tibble)
library(scales)
library(tseries)
library(vars)
library(lubridate)
library(urca)
library(mFilter)
library(forecast)
library(patchwork)

# Načtení dat ##################################################################

ZahranicniObchod <- read_excel("ZahranicniObchodBezneCeny.xlsx", 
                                        col_types = c("text", "numeric", 
                                                      "numeric", "numeric",
                                                      "numeric", "numeric", 
                                                      "numeric", "numeric", 
                                                      "numeric", "numeric", 
                                                      "numeric"))

HDPger <- fredr(
  series_id = "CLVMNACSCAB1GQDE",
  observation_start = as.Date("2005-01-01"),
  observation_end = as.Date("2023-07-01"),
  frequency = "q"
)

HDPus <- fredr(
  series_id = "GDPC1",
  observation_start = as.Date("2005-01-01"),
  observation_end = as.Date("2023-07-01"),
  frequency = "q"
)

REER <- fredr(
  series_id = "RBCZBIS",
  observation_start = as.Date("2005-01-01"),
  observation_end = as.Date("2023-07-01"),
  frequency = "q"
)

Export <- fredr(
  series_id = "NAEXKP06CZQ652S",
  observation_start = as.Date("2005-01-01"),
  observation_end = as.Date("2023-07-01"),
  frequency = "q"
)


y_ger <-dplyr::select(HDPger, date, HDPger = value)
y_us <- dplyr::select(HDPus, date, HDPus = value)
reer <- dplyr::select(REER, date, REER = value)
ex <- dplyr::select(Export, date, Export = value)

data <- y_ger %>%
  left_join(y_us, by = "date") %>%
  left_join(reer, by = "date") %>%
  left_join(ex, by = "date")

# Dekompozice exportu podle států ##############################################

data_ZahranicniObchod <- ZahranicniObchod %>%
  mutate(Date = seq(as.Date("2015-01-01"), by = "quarter", length.out = nrow(.)))
data_zeme <- data_ZahranicniObchod %>%
  mutate(
    Celkem = Nemecko + Slovensko + Polsko + Francie + USA + Zbytek
  ) %>%
  pivot_longer(
    cols = c(Nemecko, Slovensko, Polsko, Francie, USA, Zbytek),
    names_to = "Zeme", 
    values_to = "Hodnota"
  ) %>%
  mutate(
    Hodnota_podíl = Hodnota / Celkem,
    Zeme = factor(
      case_when(
        Zeme == "Nemecko" ~ "Německo",
        Zeme == "Zbytek" ~ "Ostatní",
        TRUE ~ Zeme
      ),
      levels = c("Německo", "Slovensko", "Polsko", "Francie", "USA", "Ostatní")
    )
  )

graf_zeme <- data_zeme %>%
  ggplot(aes(x = Date, y = Hodnota_podíl, fill = Zeme)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Dekompozice českého exportu (2015-2024)",
    x = "Rok",
    y = "Podíl v %",
    fill = "Období",
    caption = "Zdroj: ČSÚ"
  ) +
  scale_fill_brewer(palette = "BrBG") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
  )
print(graf_zeme)
ggsave(filename = "grafy/graf_zeme.png", plot = graf_zeme, 
       width = 10, height = 6, dpi = 300)

# Vykreslení všech proměnných ##################################################

data_long <- data %>%
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  )

CARA <- data_long %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.2) +
  facet_wrap(~variable, nrow = 2, ncol = 2, scales = "free_y") +
  scale_color_brewer(palette = "BrBG") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y", expand = c(0, 0)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    strip.text = element_text(face = "bold")
  ) +
  labs(
    title = "Časové řady",
    x = "Datum",
    y = "Hodnota",
    caption = "Zdroj: FRED"
  )  +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
  )
print(CARA)
ggsave(filename = "grafy/vsechny_CARA.png", plot = CARA, width = 13, 
       height = 10, dpi = 300)

# Transformace dat #############################################################

data$l_y_ger <- log(data$HDPger)
data$l_y_us <- log(data$HDPus)
data$l_z <- log(data$REER)
data$l_X <- log(data$Export)

data$df_l_y_ger <- c(NA, diff(data$l_y_ger))
data$df_l_y_us <- c(NA, diff(data$l_y_us))
data$df_l_z <- c(NA, diff(data$l_z))
data$df_l_X <- c(NA, diff(data$l_X))

data$df_l_z<- data$df_l_z - mean(data$df_l_z, na.rm = TRUE)

ts_l_y_ger <- ts(data$l_y_ger, start = c(2005, 1), frequency = 4)
ts_l_y_us <- ts(data$l_y_us, start = c(2005, 1), frequency = 4)
ts_l_z <- ts(data$l_z, start = c(2005, 1), frequency = 4)
ts_l_X <- ts(data$l_X, start = c(2005, 1), frequency = 4)

hp_l_y_ger <- hpfilter(ts_l_y_ger, freq = 1600)
hp_l_y_us <- hpfilter(ts_l_y_us, freq = 1600)
hp_l_z <- hpfilter(ts_l_z, freq = 1600)
hp_l_X <- hpfilter(ts_l_X, freq = 1600)

data$l_y_ger_gap <- as.numeric(hp_l_y_ger$cycle)
data$l_y_us_gap <- as.numeric(hp_l_y_us$cycle)
data$l_z_gap <- as.numeric(hp_l_z$cycle)
data$l_X_gap <- as.numeric(hp_l_X$cycle)

# Testy stacionarity ###########################################################

variables <- c("df_l_y_ger", "df_l_y_us", "df_l_z", "df_l_X", "l_y_ger_gap", 
               "l_y_us_gap", "l_z_gap", "l_X_gap")
adf_results <- data.frame(
  Variable = character(),
  P_Value = numeric(),
  Stationary = character(),
  stringsAsFactors = FALSE
)

for (var in variables) {
  series <- na.omit(data[[var]])
  test_result <- adf.test(series)
  p_value <- test_result$p.value
  stationary <- ifelse(p_value < 0.05, "Ano", "Ne")
  adf_results <- rbind(adf_results, data.frame(
    Variable = var,
    P_Value = p_value,
    Stationary = stationary
  ))
}
adf_results
write.table(adf_results, file = "tabulky/vysledky_ADF_testu.txt", sep = "\t", 
            row.names = FALSE, quote = FALSE)

# Vykreslení transformovaných proměnných #######################################

data_long <- data %>%
  select(date, df_l_y_ger, df_l_y_us, df_l_z, df_l_X) %>%
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  )

CARA_diff <- data_long %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.2) +
  facet_wrap(~variable, nrow = 2, ncol = 2, scales = "free_y") +
  scale_color_brewer(palette = "BrBG") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y", expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
  ) +
  labs(
    title = expression(Delta * log("proměnná")),
    x = "Datum",
    y = "Hodnota",
    caption = "Zdroj: FRED, vlastní výpočty"
  )
print(CARA_diff)
ggsave(filename = "grafy/transformovane_CARA.png", plot = CARA_diff, width = 13, 
       height = 10, dpi = 300)

# Vykreslení gapu proměnných ###################################################

data_gap_long <- data %>%
  dplyr::select(date, l_y_ger_gap, l_y_us_gap, l_z_gap, l_X_gap) %>%
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  )

CARA_gap <- data_gap_long %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.2) +
  facet_wrap(~variable, nrow = 2, ncol = 2, scales = "free_y") +
  scale_color_brewer(palette = "BrBG") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y", expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
  ) +
  labs(
    title = "GAP",
    x = "Datum",
    y = "Gap v %)",
    caption = "Zdroj: FRED, vlastní výpočty"
  )
print(CARA_gap)
ggsave(filename = "grafy/gap_CARA.png", plot = CARA_gap, width = 13, 
       height = 10, dpi = 300)

# Diff VAR model ###############################################################

var_diff_data <- data[, c("df_l_y_ger", "df_l_y_us", "df_l_z", "df_l_X")]
var_diff_data_ts <- ts(var_diff_data, start = c(2005, 1), frequency = 4)
var_diff_data_ts <- na.omit(var_diff_data_ts)

# Vytvoření dummy proměnné COVID
start_time <- start(var_diff_data_ts)
n_obs <- nrow(var_diff_data_ts)
years <- floor((0:(n_obs - 1)) / 4) + start_time[1]
quarters <- ((0:(n_obs - 1)) %% 4) + 1
COVID <- ifelse(years >= 2020 & quarters >= 2 | years > 2020, 1, 0)
COVID <- matrix(COVID, ncol = 1)

modely <- list()
bic_values_diff <- numeric()

sink("tabulky/Diff_VAR_modely.txt")
cat("--------------------------------------------------\n")
cat("VAR modely s df_l_X jako vysvětlovanou proměnnou\n")
cat("--------------------------------------------------\n\n")

for (p in 1:8) {
  var_model <- VAR(var_diff_data_ts, p = p, type = "const", exogen = COVID)
  modely[[p]] <- var_model
  bic_values_diff[p] <- BIC(var_model)
  summary_df_l_X <- summary(var_model)$varresult$df_l_X
  
  cat("============================================\n")
  cat(paste("VAR model se zpožděním p =", p, "\n"))
  cat("BIC modelu:", round(bic_values_diff[p], 4), "\n")
  cat("--------------------------------------------\n")
  print(summary_df_l_X)
  cat("\n\n")
}

best_p <- which.min(bic_values_diff)
cat("============================================\n")
cat(paste("Nejlepší model podle BIC má zpoždění p =", best_p, "\n"))
cat("BIC hodnoty pro všechna zpoždění:\n")
print(round(bic_values_diff, 4))
sink()

best_model_diff <- modely[[best_p]]

# IRF nejlepsiho modelu
irf_result <- irf(best_model_diff, impulse = NULL, response = "df_l_X", 
                  n.ahead = 10, boot = TRUE, ci = 0.95)
irf_data_diff <- lapply(names(irf_result$irf), function(impulse_name) {
  data.frame(
    impulse = impulse_name,
    period = 0:10,
    response = irf_result$irf[[impulse_name]][, "df_l_X"],
    lower = irf_result$Lower[[impulse_name]][, "df_l_X"],
    upper = irf_result$Upper[[impulse_name]][, "df_l_X"]
  )
}) %>% bind_rows()

IRF_diff_var <- ggplot(irf_data_diff, aes(x = period, y = response)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#71B2AC", alpha = 0.5) +
  geom_line(color = "#2B645E", size = 1.2) +
  facet_wrap(~ impulse, nrow = 2, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = 0:10) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
  ) +
  labs(
    title = expression("Impulsní odezvy diff VAR modelu"),
    x = "Období",
    y = "Relativní změna",
    caption = "Zdroj: vlastní výpočty"
  )
print(IRF_diff_var)
ggsave(filename = "grafy/IRF_diff_VAR.png", plot = IRF_diff_var, 
       width = 13, height = 10, dpi = 300)


# Gap VAR model ################################################################

var_gap_data <- data[, c("l_y_ger_gap", "l_y_us_gap", "l_z_gap", "l_X_gap")]
var_gap_data_ts <- ts(var_gap_data, start = c(2005, 1), frequency = 4)
var_gap_data_ts <- na.omit(var_gap_data_ts)

# Vytvoření dummy proměnné COVID
start_time <- start(var_gap_data_ts)
n_obs <- nrow(var_gap_data_ts)
years <- floor((0:(n_obs - 1)) / 4) + start_time[1]
quarters <- ((0:(n_obs - 1)) %% 4) + 1
COVID <- ifelse(years >= 2020 & quarters >= 2 | years > 2020, 1, 0)
COVID <- matrix(COVID, ncol = 1)

modely <- list()
bic_values_gap <- numeric()

sink("tabulky/Gap_VAR_modely.txt")
cat("--------------------------------------------------\n")
cat("VAR modely s l_X_gap jako vysvětlovanou proměnnou\n")
cat("--------------------------------------------------\n\n")

for (p in 1:8) {
  var_model <- VAR(var_gap_data_ts, p = p, type = "const", exogen = COVID)
  modely[[p]] <- var_model
  bic_values_gap[p] <- BIC(var_model)
  summary_l_X_gap <- summary(var_model)$varresult$l_X_gap
  
  cat("============================================\n")
  cat(paste("VAR model se zpožděním p =", p, "\n"))
  cat("BIC modelu:", round(bic_values_gap[p], 4), "\n")
  cat("--------------------------------------------\n")
  print(summary_l_X_gap)
  cat("\n\n")
}

best_p <- which.min(bic_values_gap)
cat("============================================\n")
cat(paste("Nejlepší model podle BIC má zpoždění p =", best_p, "\n"))
cat("BIC hodnoty pro všechna zpoždění:\n")
print(round(bic_values_gap, 4))
sink()

best_model <- modely[[best_p]]

# IRF nejlepšího modelu
irf_result <- irf(best_model, impulse = NULL, response = "l_X_gap", 
                  n.ahead = 10, boot = TRUE, ci = 0.95)

irf_data <- lapply(names(irf_result$irf), function(impulse_name) {
  data.frame(
    impulse = impulse_name,
    period = 0:10,
    response = irf_result$irf[[impulse_name]][, "l_X_gap"],
    lower = irf_result$Lower[[impulse_name]][, "l_X_gap"],
    upper = irf_result$Upper[[impulse_name]][, "l_X_gap"]
  )
}) %>% bind_rows()

IRF_gap_var <- ggplot(irf_data, aes(x = period, y = response)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#D2B470", alpha = 0.5) +
  geom_line(color = "#84541E", size = 1.2) +
  facet_wrap(~ impulse, nrow = 2, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = 0:10) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
  ) +
  labs(
    title = expression("Impulsní odezvy Gap VAR modelu"),
    x = "Období",
    y = "Relativní změna",
    caption = "Zdroj: vlastní výpočty"
  )

print(IRF_gap_var)

ggsave(filename = "grafy/IRF_gap_VAR.png", plot = IRF_gap_var, 
       width = 13, height = 10, dpi = 300)

# Porovnání modelů #############################################################

best_bic_diff <- min(bic_values_diff)
best_bic_gap <- min(bic_values_gap)

sink("tabulky/porovnani_modelu1.txt")
cat("--------------------------------------------------\n")
cat("Nejlepší BIC pro Gap model:", round(best_bic_gap, 4), "\n")
cat("Nejlepší BIC pro Diff model:", round(best_bic_diff, 4), "\n")
cat("--------------------------------------------------\n")
if (best_bic_diff < best_bic_gap) {
  cat("Diff model má lepší fit podle BIC.\n")
} else {
  cat("Gap model má lepší fit podle BIC.\n")
}
cat("--------------------------------------------------\n")
sink()

# Predikční výkonost Diff VAR modelu ###########################################

start_forecast_index_diff <- 45  # Q1 2016
actual_diff <- var_diff_data_ts[, "df_l_X"]
n_obs_diff <- length(actual_diff)
pred_values_diff <- rep(NA, n_obs_diff)
forecast_errors_diff <- rep(NA, n_obs_diff)

for (i in start_forecast_index_diff:(n_obs_diff - 1)) {
  train_data_diff <- var_diff_data_ts[1:i, ]
  
  var_model_diff <- VAR(train_data_diff, p = best_model_diff$p, type = "const")
  fc_diff <- predict(var_model_diff, n.ahead = 1)
  pred_diff <- fc_diff$fcst$df_l_X[1, "fcst"]
  
  if (!is.na(pred_diff) && is.finite(pred_diff)) {
    pred_values_diff[i + 1] <- pred_diff
    forecast_errors_diff[i + 1] <- actual_diff[i + 1] - pred_diff
  }
}

forecast_df_diff <- data.frame(
  time = time(actual_diff),
  actual = as.numeric(actual_diff),
  prediction = pred_values_diff,
  error = forecast_errors_diff
)

# Predikční výkonost Gap VAR modelu ###########################################

start_forecast_index_gap <- 45  # Q1 2016

actual_gap <- var_gap_data_ts[, "l_X_gap"]
n_obs_gap <- length(actual_gap)

pred_values_gap <- rep(NA, n_obs_gap)
forecast_errors_gap <- rep(NA, n_obs_gap)

for (i in start_forecast_index_gap:(n_obs_gap - 1)) {
  train_data_gap <- var_gap_data_ts[1:i, ]
  
  # VAR model bez exogenní proměnné
  var_model_gap <- VAR(train_data_gap, p = best_model$p, type = "const")
  fc_gap <- predict(var_model_gap, n.ahead = 1)
  
  pred_gap <- fc_gap$fcst$l_X_gap[1, "fcst"]
  
  if (!is.na(pred_gap) && is.finite(pred_gap)) {
    pred_values_gap[i + 1] <- pred_gap
    forecast_errors_gap[i + 1] <- actual_gap[i + 1] - pred_gap
  }
}

forecast_df_gap <- data.frame(
  time = time(actual_gap),
  actual = as.numeric(actual_gap),
  prediction = pred_values_gap,
  error = forecast_errors_gap
)

# Vizualizace a uložení chyb predikcí ##########################################

forecast_df_plot_diff <- forecast_df_diff[!is.na(forecast_df_diff$prediction), ]
forecast_df_plot_diff$time_date <- as.Date(as.yearqtr(forecast_df_plot_diff$time),
                                           frac = 0)

diff_var_forecast <- ggplot(forecast_df_plot_diff, aes(x = time_date)) +
  geom_line(aes(y = actual, color = "Skutečnost"), size = 1.2) +
  geom_line(aes(y = prediction, color = "Predikce"), size = 1.2, 
            linetype = "dashed") +
  scale_color_manual(values = c("Skutečnost" = "#2B645E", 
                                "Predikce" = "#71B2AC")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = expression(Delta * log("X")),
    x = "Datum",
    y = "Hodnota",
    color = NULL,
    caption = "Zdroj: vlastní výpočty na základě dat"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
  )


forecast_df_plot_gap <- forecast_df_gap[!is.na(forecast_df_gap$prediction), ]
forecast_df_plot_gap$time_date <- as.Date(as.yearqtr(forecast_df_plot_gap$time), 
                                          frac = 0)

gap_var_forecast <- ggplot(forecast_df_plot_gap, aes(x = time_date)) +
  geom_line(aes(y = actual, color = "Skutečnost"), size = 1.2) +
  geom_line(aes(y = prediction, color = "Predikce"), size = 1.2, 
            linetype = "dashed") +
  scale_color_manual(values = c("Skutečnost" = "#84541E", 
                                "Predikce" = "#D2B470")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = expression("Gap X"),
    x = "Datum",
    y = "Hodnota",
    color = NULL,
    caption = "Zdroj: vlastní výpočty"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
  )

combined_forecast <- diff_var_forecast / gap_var_forecast +
  plot_annotation(
    title = "Jednokrokové predikce Diff VAR vs Gap VAR",
    theme = theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16)
    )
  )
print(combined_forecast)
ggsave(filename = "grafy/combined_forecast.png", plot = combined_forecast, 
       width = 13, height = 10, dpi = 300)


# Cyhyb predikci
mse_diff <- mean(forecast_errors_diff^2, na.rm = TRUE)
mae_diff <- mean(abs(forecast_errors_diff), na.rm = TRUE)
mse_gap <- mean(forecast_errors_gap^2, na.rm = TRUE)
mae_gap <- mean(abs(forecast_errors_gap), na.rm = TRUE)

better_mse_model <- if (mse_diff < mse_gap) "Diff VAR" else "Gap VAR"
better_mae_model <- if (mae_diff < mae_gap) "Diff VAR" else "Gap VAR"

sink("tabulky/predikce_modelu.txt", append = TRUE)
cat("--------------------------------------------------\n")
cat("Predikční výkonnost modelů (jednokroková predikce)\n")
cat("--------------------------------------------------\n")
cat("Diff VAR model:\n")
cat(" - MSE:", round(mse_diff, 4), "\n")
cat(" - MAE:", round(mae_diff, 4), "\n\n")
cat("Gap VAR model:\n")
cat(" - MSE:", round(mse_gap, 4), "\n")
cat(" - MAE:", round(mae_gap, 4), "\n\n")
cat("Srovnání:\n")
cat(" - Nižší MSE má:", better_mse_model, "\n")
cat(" - Nižší MAE má:", better_mae_model, "\n")
cat("--------------------------------------------------\n\n")
sink()
