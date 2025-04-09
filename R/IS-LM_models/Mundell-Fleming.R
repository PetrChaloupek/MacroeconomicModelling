################################################################################
########################## MUNDELL-FLEMING MODEL ###############################
################################################################################

rm(list = ls())
cat("\014")

# install.packages("ggplot2")
# install.packages("gridExtra")
library(ggplot2)
library(gridExtra)

# Theme of the plots
my_theme <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

#### IS-LM curves ##############################################################
# IS curve
is_data <- data.frame(
  y = seq(0, 20, 0.1),
  r = 10 - 0.5 * seq(0, 20, 0.1)  # r = a - b*y
)

# LM curve
lm_data <- data.frame(
  y = seq(0, 20, 0.1),
  r = 1 + 0.5 * seq(0, 20, 0.1)   # r = c + d*y
)

# IS-LM plot
is_lm_plot <- ggplot() +
  geom_line(data = is_data, aes(x = y, y = r, color = "IS"), linewidth = 1.2) +
  geom_line(data = lm_data, aes(x = y, y = r, color = "LM"), linewidth = 1.2) +
  scale_color_manual(values = c("IS" = "blue", "LM" = "red")) +
  scale_x_continuous(limits = c(0, 20)) +
  labs(
    title = "IS-LM curves in Mundell-Fleming model",
    x = "Output",
    y = "Interest rate",
    color = "Curves"
  ) +
  my_theme
print(is_lm_plot)

#### BP curve ##################################################################
# BP curve
bp_data <- data.frame(
  y = seq(0, 20, 0.1),
  r = 3 + 0.2 * seq(0, 20, 0.1)   # r = e + f*y
)

# BP plot
bp_plot <- ggplot() +
  geom_line(data = is_data, aes(x = y, y = r, color = "IS"), size = 1.2) +
  geom_line(data = lm_data, aes(x = y, y = r, color = "LM"), size = 1.2) +
  geom_line(data = bp_data, aes(x = y, y = r, color = "BP"), size = 1.2) +
  scale_color_manual(values = c("IS" = "blue", "LM" = "red", "BP" = "green")) +
  scale_x_continuous(limits = c(0, 20)) +
  labs(
    title = "IS-LM-BP curves Mundell-Fleming modelu",
    x = "Output",
    y = "Interest rate",
    color = "Curves"
  ) +
  my_theme
print(bp_plot)

#### Simulating different policies #############################################




# 1. Fiskální expanze s pevným směnným kurzem a vysokou kapitálovou mobilitou
is_shift <- data.frame(
  y = seq(0, 10, 0.1),
  r = 12 - 0.5 * seq(0, 10, 0.1)  # posunutá IS křivka (expanzivní fiskální politika)
)

fixed_exchange_high_mobility <- ggplot() +
  geom_line(data = is_data, aes(x = y, y = r, color = "IS"), size = 1, linetype = "dashed") +
  geom_line(data = is_shift, aes(x = y, y = r, color = "IS'"), size = 1.2) +
  geom_line(data = lm_data, aes(x = y, y = r, color = "LM"), size = 1.2) +
  geom_line(data = bp_data, aes(x = y, y = r, color = "BP"), size = 1.2) +
  scale_color_manual(values = c("IS" = "lightblue", "IS'" = "blue", "LM" = "red", "BP" = "green")) +
  labs(
    title = "Fiskální expanze: Pevný směnný kurz, vysoká kapitálová mobilita",
    x = "Výstup (Y)",
    y = "Úroková míra (r)",
    color = "Křivky"
  ) +
  my_theme
print(fixed_exchange_high_mobility)
# 2. Monetární expanze s plovoucím směnným kurzem
lm_shift <- data.frame(
  y = seq(0, 10, 0.1),
  r = 0 + 0.5 * seq(0, 10, 0.1)   # posunutá LM křivka (expanzivní monetární politika)
)

floating_exchange_monetary <- ggplot() +
  geom_line(data = is_data, aes(x = y, y = r, color = "IS"), size = 1.2) +
  geom_line(data = lm_data, aes(x = y, y = r, color = "LM"), size = 1, linetype = "dashed") +
  geom_line(data = lm_shift, aes(x = y, y = r, color = "LM'"), size = 1.2) +
  geom_line(data = bp_data, aes(x = y, y = r, color = "BP"), size = 1.2) +
  scale_color_manual(values = c("IS" = "blue", "LM" = "pink", "LM'" = "red", "BP" = "green")) +
  labs(
    title = "Monetární expanze: Plovoucí směnný kurz",
    x = "Výstup (Y)",
    y = "Úroková míra (r)",
    color = "Křivky"
  ) +
  my_theme

# ----------------------------------------------
# Zobrazení grafů
# ----------------------------------------------

# Funkce pro zobrazení všech grafů
display_mundell_fleming_graphs <- function() {
  # Jednotlivé grafy
  print(is_lm_plot)
  print(bp_plot)
  print(fixed_exchange_high_mobility)
  print(floating_exchange_monetary)
  
  # Kombinovaný graf
  combined_plot <- grid.arrange(
    is_lm_plot, bp_plot,
    fixed_exchange_high_mobility, floating_exchange_monetary,
    ncol = 2,
    top = "Mundell-Fleming Model"
  )
  
  return(combined_plot)
}

# Zobrazení grafů
display_mundell_fleming_graphs()

# ----------------------------------------------
# Funkce pro simulaci různých scénářů
# ----------------------------------------------

simulate_scenario <- function(scenario = "fiscal_fixed", 
                              is_shift_amount = 2, 
                              lm_shift_amount = 1, 
                              bp_slope = 0.2) {
  
  # Výchozí IS, LM křivky
  is_data <- data.frame(
    y = seq(0, 10, 0.1),
    r = 10 - 0.5 * seq(0, 10, 0.1)
  )
  
  lm_data <- data.frame(
    y = seq(0, 10, 0.1),
    r = 1 + 0.5 * seq(0, 10, 0.1)
  )
  
  # BP křivka s nastavitelnou směrnicí (pro různé úrovně kapitálové mobility)
  bp_data <- data.frame(
    y = seq(0, 10, 0.1),
    r = 3 + bp_slope * seq(0, 10, 0.1)
  )
  
  # Posunuté křivky dle scénáře
  is_shift <- data.frame(
    y = seq(0, 10, 0.1),
    r = (10 + is_shift_amount) - 0.5 * seq(0, 10, 0.1)
  )
  
  lm_shift <- data.frame(
    y = seq(0, 10, 0.1),
    r = (1 - lm_shift_amount) + 0.5 * seq(0, 10, 0.1)
  )
  
  # Vytvoření grafu dle scénáře
  if (scenario == "fiscal_fixed") {
    title <- "Fiskální expanze: Pevný směnný kurz"
    p <- ggplot() +
      geom_line(data = is_data, aes(x = y, y = r, color = "IS"), size = 1, linetype = "dashed") +
      geom_line(data = is_shift, aes(x = y, y = r, color = "IS'"), size = 1.2) +
      geom_line(data = lm_data, aes(x = y, y = r, color = "LM"), size = 1.2) +
      geom_line(data = bp_data, aes(x = y, y = r, color = "BP"), size = 1.2)
  } else if (scenario == "monetary_fixed") {
    title <- "Monetární expanze: Pevný směnný kurz"
    p <- ggplot() +
      geom_line(data = is_data, aes(x = y, y = r, color = "IS"), size = 1.2) +
      geom_line(data = lm_data, aes(x = y, y = r, color = "LM"), size = 1, linetype = "dashed") +
      geom_line(data = lm_shift, aes(x = y, y = r, color = "LM'"), size = 1.2) +
      geom_line(data = bp_data, aes(x = y, y = r, color = "BP"), size = 1.2)
  } else if (scenario == "fiscal_floating") {
    title <- "Fiskální expanze: Plovoucí směnný kurz"
    p <- ggplot() +
      geom_line(data = is_data, aes(x = y, y = r, color = "IS"), size = 1, linetype = "dashed") +
      geom_line(data = is_shift, aes(x = y, y = r, color = "IS'"), size = 1.2) +
      geom_line(data = lm_data, aes(x = y, y = r, color = "LM"), size = 1.2) +
      geom_line(data = bp_data, aes(x = y, y = r, color = "BP"), size = 1.2)
  } else if (scenario == "monetary_floating") {
    title <- "Monetární expanze: Plovoucí směnný kurz"
    p <- ggplot() +
      geom_line(data = is_data, aes(x = y, y = r, color = "IS"), size = 1.2) +
      geom_line(data = lm_data, aes(x = y, y = r, color = "LM"), size = 1, linetype = "dashed") +
      geom_line(data = lm_shift, aes(x = y, y = r, color = "LM'"), size = 1.2) +
      geom_line(data = bp_data, aes(x = y, y = r, color = "BP"), size = 1.2)
  }
  
  p <- p + scale_color_manual(values = c("IS" = "lightblue", "IS'" = "blue", 
                                         "LM" = "pink", "LM'" = "red", 
                                         "BP" = "green")) +
    labs(
      title = title,
      x = "Výstup (Y)",
      y = "Úroková míra (r)",
      color = "Křivky"
    ) +
    my_theme
  
  return(p)
}

# Příklad použití simulační funkce:
# simulate_scenario(scenario = "fiscal_floating", bp_slope = 0.1)  # Nízká kapitálová mobilita
# simulate_scenario(scenario = "monetary_fixed", bp_slope = 0.9)   # Vysoká kapitálová mobilita