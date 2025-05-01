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

HDPpl <- fredr(
  series_id = "CLVMEURSCAB1GQPL",
  observation_start = as.Date("2005-01-01"),
  observation_end = as.Date("2023-07-01"),
  frequency = "q"
)

HDPfr <- fredr(
  series_id = "CLVMNACSCAB1GQFR",
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
y_pl <- dplyr::select(HDPpl, date, HDPpl = value)
y_fr <- dplyr::select(HDPfr, date, HDPfr = value)
y_us <- dplyr::select(HDPus, date, HDPus = value)
reer <- dplyr::select(REER, date, REER = value)
ex <- dplyr::select(Export, date, Export = value)

data <- y_ger %>%
  left_join(y_pl, by = "date") %>%
  left_join(y_fr, by = "date") %>%
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
  facet_wrap(~variable, nrow = 3, ncol = 2, scales = "free_y") +
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
data$l_y_pl <- log(data$HDPpl)
data$l_y_fr <- log(data$HDPfr)
data$l_y_us <- log(data$HDPus)
data$l_z <- log(data$REER)
data$l_X <- log(data$Export)

data$df_l_y_ger <- c(NA, diff(data$l_y_ger))
data$df_l_y_pl <- c(NA, diff(data$l_y_pl))
data$df_l_y_fr <- c(NA, diff(data$l_y_fr))
data$df_l_y_us <- c(NA, diff(data$l_y_us))
data$df_l_z <- c(NA, diff(data$l_z))
data$df_l_X <- c(NA, diff(data$l_X))

data$df_l_z<- data$df_l_z - mean(data$df_l_z, na.rm = TRUE)

ts_l_y_ger <- ts(data$l_y_ger, start = c(2005, 1), frequency = 4)
ts_l_y_pl <- ts(data$l_y_pl, start = c(2005, 1), frequency = 4)
ts_l_y_fr <- ts(data$l_y_fr, start = c(2005, 1), frequency = 4)
ts_l_y_us <- ts(data$l_y_us, start = c(2005, 1), frequency = 4)
ts_l_z <- ts(data$l_z, start = c(2005, 1), frequency = 4)
ts_l_X <- ts(data$l_X, start = c(2005, 1), frequency = 4)

hp_l_y_ger <- hpfilter(ts_l_y_ger, freq = 1600)
hp_l_y_pl <- hpfilter(ts_l_y_pl, freq = 1600)
hp_l_y_fr <- hpfilter(ts_l_y_fr, freq = 1600)
hp_l_y_us <- hpfilter(ts_l_y_us, freq = 1600)
hp_l_z <- hpfilter(ts_l_z, freq = 1600)
hp_l_X <- hpfilter(ts_l_X, freq = 1600)

data$l_y_ger_gap <- as.numeric(hp_l_y_ger$cycle)
data$l_y_pl_gap <- as.numeric(hp_l_y_pl$cycle)
data$l_y_fr_gap <- as.numeric(hp_l_y_fr$cycle)
data$l_y_us_gap <- as.numeric(hp_l_y_us$cycle)
data$l_z_gap <- as.numeric(hp_l_z$cycle)
data$l_X_gap <- as.numeric(hp_l_X$cycle)

# Testy stacionarity ###########################################################

variables <- c("df_l_y_ger", "df_l_y_pl", "df_l_y_fr", "df_l_y_us", "df_l_z",
               "df_l_X", "l_y_ger_gap", "l_y_pl_gap", "l_y_fr_gap", "l_y_us_gap",
               "l_z_gap", "l_X_gap")
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
  select(date, df_l_y_ger, df_l_y_pl, df_l_y_fr, df_l_y_us, df_l_z, df_l_X) %>%
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  )

CARA_diff <- data_long %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.2) +
  facet_wrap(~variable, nrow = 3, ncol = 2, scales = "free_y") +
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
  dplyr::select(date, l_y_ger_gap, l_y_pl_gap, l_y_fr_gap, l_y_us_gap, l_z_gap, 
                l_X_gap) %>%
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  )

CARA_gap <- data_gap_long %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.2) +
  facet_wrap(~variable, nrow = 3, ncol = 2, scales = "free_y") +
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

# RUČNÍ Diff VAR model $#########################################################

var_diff_data <- data[, c("df_l_y_ger", "df_l_y_pl", "df_l_y_fr", "df_l_y_us", 
                          "df_l_z", "df_l_X")]
var_diff_data_ts <- ts(var_data, start = c(2005, 1), frequency = 4)
var_diff_data_ts <- na.omit(var_data_ts)

lag_diff_selection <- VARselect(var_diff_data_ts, lag.max = 8, type = "const")
optimal_lag_diff <- lag_diff_selection$selection[3]
print("Optimální zpoždění podle BIC:")
print(optimal_lag_diff)

var_diff_model <- VAR(var_diff_data_ts, p = optimal_lag_diff, type = "const")
var_diff_summary <- summary(var_diff_model)

sink("nejlepsi_diff_var_model.txt")
print("--------------------------------------------------")
print("Výsledky nejlepšího VAR modelu podle BIC kritéria")
print("--------------------------------------------------")
print(paste("Optimální zpoždění (p):", optimal_lag_diff))
print("\nVýsledky výběru zpoždění:")
print("\nShrnutí VAR modelu:")
print(var_diff_summary)
sink()

# GPT Diff VAR model ###############################################################

library(vars)

# Výběr relevantních proměnných
var_diff_data <- data[, c("df_l_y_ger", 
                          "df_l_z", "df_l_X")]
var_diff_data_ts <- ts(var_diff_data, start = c(2005, 1), frequency = 4)
var_diff_data_ts <- na.omit(var_diff_data_ts)

# Vytvoření dummy proměnné COVID (1 od 2020 Q2 dál)
start_time <- start(var_diff_data_ts)
n_obs <- nrow(var_diff_data_ts)
years <- floor((0:(n_obs - 1)) / 4) + start_time[1]
quarters <- ((0:(n_obs - 1)) %% 4) + 1
COVID <- ifelse(years >= 2020 & quarters >= 2 | years > 2020, 1, 0)
COVID <- matrix(COVID, ncol = 1)  # musí být matice, pokud je použita jako exogen

# Inicializace pro uložení modelů a jejich BIC
modely <- list()
bic_values <- numeric()

# Zahájení výpisu do souboru
sink("vsechny_df_l_X_VAR_modely_COVID.txt")
cat("--------------------------------------------------\n")
cat("VAR modely s df_l_X jako vysvětlovanou proměnnou a dummy COVID\n")
cat("--------------------------------------------------\n\n")

# Smyčka přes zpoždění p = 1 až 8
for (p in 1:8) {
  var_model <- VAR(var_diff_data_ts, p = p, type = "const", exogen = COVID)
  modely[[p]] <- var_model
  bic_values[p] <- BIC(var_model)
  
  summary_df_l_X <- summary(var_model)$varresult$df_l_X
  
  cat("============================================\n")
  cat(paste("VAR model se zpožděním p =", p, "\n"))
  cat("BIC tohoto modelu:", round(bic_values[p], 4), "\n")
  cat("--------------------------------------------\n")
  print(summary_df_l_X)
  cat("\n\n")
}

# Závěr – nejlepší model
best_p <- which.min(bic_values)
cat("============================================\n")
cat(paste("Nejlepší model podle BIC má zpoždění p =", best_p, "\n"))
cat("BIC hodnoty pro všechna zpoždění:\n")
print(round(bic_values, 4))
sink()

best_model <- modely[[best_p]]

# IRF pro odezvu df_l_X na všechny šoky
irf_result <- irf(best_model, impulse = NULL, response = "df_l_X", 
                  n.ahead = 10, boot = TRUE, ci = 0.95)

# Převod výsledků do data.frame
irf_data <- lapply(names(irf_result$irf), function(impulse_name) {
  data.frame(
    impulse = impulse_name,
    period = 0:10,
    response = irf_result$irf[[impulse_name]][, "df_l_X"],
    lower = irf_result$Lower[[impulse_name]][, "df_l_X"],
    upper = irf_result$Upper[[impulse_name]][, "df_l_X"]
  )
}) %>% bind_rows()

# Vykreslení pomocí ggplot2 – pouze zobrazení
IRF_diff_var <- ggplot(irf_data, aes(x = period, y = response)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#CEE9E5", alpha = 0.5) +
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