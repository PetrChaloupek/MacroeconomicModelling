#### CSV NA EXCEL ##############################################################
# potřebné balíčky
library(readr)     # pro čtení csv
library(openxlsx)  # pro export do Excelu

# 1. Import .csv souboru
df <- read_csv("GBP:EUR.csv", col_names = FALSE)


# 2. Přejmenuj sloupce
colnames(df) <- c("Date1", "Date2", "ZL/EUR")

# 3. Vezmi jen vektor "ZL/EUR"
zl_eur_vector <- df[["ZL/EUR"]]

# 4. Ulož jako excelový soubor (např. "zl_eur_vector.xlsx")
write.xlsx(data.frame(ZL_EUR = zl_eur_vector), 
           file = "gbp_eur_vector.xlsx", 
           rowNames = FALSE)

#### SEZÓNNÍ OČIŠTĚNÍ ##########################################################
library(readxl)
library(openxlsx)
library(seasonal)

# 1. Načti data
data <- read_excel("HDP_Slovakia.xlsx", sheet = "Quarterly")

# 2. Časová řada (uprav start podle svých dat)
ts_gdp <- ts(data$GDP_Sk, start = c(1995, 1), frequency = 4)

# 3. Sezónní očištění
adj <- seas(ts_gdp)

# 4. Nový sloupec s očištěnými daty
# převedeme čísla na text s desetinnou čárkou
data$GDP_Sk_SA <- gsub("\\.", ",", format(round(final(adj), 2), nsmall = 2))

# 5. Ulož zpět do Excelu
write.xlsx(data, file = "HDP_Slovakia.xlsx", sheetName = "Quarterly", overwrite = TRUE)


#### QS TEST SEZÓNNOSTI ########################################################

# Nejprve nainstalujte potřebné balíčky, pokud ještě nejsou nainstalovány:
# install.packages(c("readxl", "dplyr", "lubridate", "zoo", "tseries", "seasonal"))

# Načtení potřebných balíčků
library(readxl)
library(dplyr)
library(lubridate)
library(zoo)
library(tseries)
library(seasonal)

# 1. Import dat z Excelu
# Předpokládáme, že soubor se jmenuje "cenové hladiny.xlsx" (přidejte příponu, pokud není)
data <- read_excel("cenové hladiny.xlsx", sheet = "Data")

# Zobrazení prvních řádků pro kontrolu
head(data)

# 2. Převod sloupce Date na datum (předpokládáme formát YYYY-MM nebo podobný)
data$Date <- as.Date(paste(data$Date, "-01", sep = ""), format = "%Y-%m-%d")  # Pokud je Date ve formátu YYYY-MM, přidej den

# Seřazení dat podle data (pro jistotu)
data <- data %>% arrange(Date)

# Vytvoření čtvrtletního indexu
data$Quarter <- as.yearqtr(data$Date)

# 3. Převod na čtvrtletní data - průměr za čtvrtletí pro každý sloupec (kromě Date a Quarter)
quarterly_data <- data %>%
  group_by(Quarter) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))  # Předpokládáme, že všechny sloupce kromě Date jsou numerické (cenové hladiny)

# Zobrazení čtvrtletních dat
print(quarterly_data)

# 4. Formální test sezónnosti pro každou zemi (sloupec)
# Předpokládáme, že první sloupec je Quarter, další jsou země
countries <- names(quarterly_data)[-1]  # Vyloučit Quarter

for (country in countries) {
  cat("\nTest sezónnosti pro zemi:", country, "\n")
  
  # Vytvoření časové řady (čtvrtletní frekvence = 4)
  ts_data <- ts(quarterly_data[[country]], start = c(year(min(quarterly_data$Quarter)), quarter(min(quarterly_data$Quarter))), frequency = 4)
  
  # Dekompozice pomocí STL (Seasonal-Trend decomposition using LOESS)
  stl_decomp <- tryCatch(stl(ts_data, s.window = "periodic"), error = function(e) NULL)
  if (!is.null(stl_decomp)) {
    plot(stl_decomp)
    cat("STL dekompozice ukazuje sezónní komponentu. Pokud je významná, je třeba očistit.\n")
  } else {
    cat("STL dekompozice selhala (možná nedostatek dat).\n")
  }
  
  # Test na sezónní jednotkovou kořen (OCSB test z balíčku seasonal)
  ocsb_test <- tryCatch(ocsb.test(ts_data), error = function(e) NULL)
  if (!is.null(ocsb_test)) {
    print(ocsb_test)
    cat("Pokud je p-hodnota < 0.05 pro sezónní kořen, existuje sezónnost.\n")
  } else {
    cat("OCSB test selhal.\n")
  }
  
  # Alternativně: ADF test na stacionaritu (pro kontrolu)
  adf_test <- adf.test(ts_data)
  print(adf_test)
  cat("Pokud ADF test ukazuje nestacionaritu, může to souviset se sezónností nebo trendem.\n")
}

# Nejprve nainstalujte potřebné balíčky, pokud ještě nejsou nainstalovány:
# install.packages(c("readxl", "dplyr", "lubridate", "zoo", "tseries", "seasonal"))

# Načtení potřebných balíčků
library(readxl)
library(dplyr)
library(lubridate)
library(zoo)
library(tseries)
library(seasonal)

# 1. Import dat z Excelu
data <- read_excel("cenové hladiny.xlsx", sheet = "Data")

# 2. Převod sloupce Date na datum
data$Date <- as.Date(data$Date)  # Už je ve formátu Date, jak vidět z head(data)

# Seřazení dat podle data
data <- data %>% arrange(Date)

# Vytvoření čtvrtletního indexu
data$Quarter <- as.yearqtr(data$Date)

# 3. Převod na čtvrtletní data - průměr za čtvrtletí
quarterly_data <- data %>%
  group_by(Quarter) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# 4. Sezónní očištění pro každou zemi pomocí seas()
countries <- names(quarterly_data)[-1]  # Vyloučit Quarter

# Vytvoření data.frame pro očištěná data
adjusted_data <- quarterly_data %>% select(Quarter)  # Začneme jen s Quarter

for (country in countries) {
  cat("\nSezónní očištění pro zemi:", country, "\n")
  
  # Vytvoření časové řady (čtvrtletní frekvence = 4)
  ts_data <- ts(quarterly_data[[country]], start = c(year(min(quarterly_data$Quarter)), quarter(min(quarterly_data$Quarter))), frequency = 4)
  
  # Sezónní adjustace pomocí seas() z balíčku seasonal (X-13-ARIMA-SEATS)
  seas_model <- tryCatch(seas(ts_data), error = function(e) {
    cat("Chyba při seas pro", country, ":", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(seas_model)) {
    # Extrakce sezónně adjustovaných dat
    adjusted_ts <- final(seas_model)  # final() vrací adjustovaná data
    
    # Přidání do data.frame
    adjusted_data[[paste0(country, "_adjusted")]] <- as.numeric(adjusted_ts)
    
    # Volitelně: Zobrazení summary modelu
    summary(seas_model)
    
    # Volitelně: Plot adjustovaných dat
    plot(adjusted_ts, main = paste("Sezónně adjustovaná data pro", country))
  } else {
    cat("Sezónní očištění selhalo pro", country, "\n")
  }
}

# Zobrazení očištěných dat
print(adjusted_data)

write.xlsx(adjusted_data, file = "cenové_hladiny_adjusted.xlsx", sheetName = "Adjusted_Data", overwrite = TRUE)



#### SEZÓNNÍ OČIŠTĚNÍ IMPORT INDEXU ############################################

library(readxl)
library(dplyr)
library(lubridate)
library(zoo)
library(tseries)
library(seasonal)
library(forecast)  # Pro OCSB test
library(openxlsx)  # Pro uložení do Excelu

# 1. Import dat z Excelu
# Předpokládáme soubor "Import_index.xlsx" a sheet "Data" s podobnou strukturou jako předtím (Date a sloupce pro země)
file_path <- "Import_index.xlsx"
data <- read_excel(file_path)

# Zobrazení prvních řádků pro kontrolu
head(data)

# 2. Převod sloupce Date na datum
data$Date <- as.Date(data$Date)  # Předpokládáme, že je již v datovém formátu

# Seřazení dat podle data
data <- data %>% arrange(Date)

# Vytvoření čtvrtletního indexu
data$Quarter <- as.yearqtr(data$Date)

# 3. Převod na čtvrtletní data - průměr za čtvrtletí pro každý numerický sloupec
quarterly_data <- data %>%
  group_by(Quarter) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Zobrazení čtvrtletních dat
print(quarterly_data)

# 4. Formální test sezónnosti a očištění pro každý sloupec (zemi)
countries <- names(quarterly_data)[-1]  # Vyloučit Quarter

# Vytvoření data.frame pro očištěná data
adjusted_data <- quarterly_data %>% select(Quarter)

for (country in countries) {
  cat("\nTest sezónnosti a očištění pro:", country, "\n")
  
  # Vytvoření časové řady (čtvrtletní frekvence = 4)
  ts_data <- ts(quarterly_data[[country]], start = c(year(min(quarterly_data$Quarter)), quarter(min(quarterly_data$Quarter))), frequency = 4)
  
  # STL dekompozice
  stl_decomp <- tryCatch(stl(ts_data, s.window = "periodic"), error = function(e) NULL)
  if (!is.null(stl_decomp)) {
    plot(stl_decomp)
    # Výpočet síly sezónnosti (pokud > 0.64, považujeme za významnou)
    seasonal_strength <- max(0, 1 - var(stl_decomp$time.series[, "remainder"]) / var(stl_decomp$time.series[, "remainder"] + stl_decomp$time.series[, "seasonal"]))
    cat("Síla sezónnosti (STL):", seasonal_strength, "\n")
  } else {
    cat("STL dekompozice selhala.\n")
    seasonal_strength <- 0
  }
  
  # OCSB test na sezónní jednotkovou kořen
  ocsb_test <- tryCatch(OCSBtest(ts_data, lag.method = "AIC"), error = function(e) NULL)
  ocsb_p <- if (!is.null(ocsb_test)) ocsb_test$p.value else 1
  if (!is.null(ocsb_test)) {
    print(ocsb_test)
    cat("OCSB p-hodnota:", ocsb_p, "\n")
  } else {
    cat("OCSB test selhal.\n")
  }
  
  # ADF test na stacionaritu
  adf_test <- adf.test(ts_data)
  print(adf_test)
  adf_p <- adf_test$p.value
  
  # Rozhodnutí o sezónnosti: Pokud seasonal_strength > 0.64 nebo OCSB p < 0.05 nebo ADF p > 0.05 (nestacionarita)
  if (seasonal_strength > 0.64 || ocsb_p < 0.05 || adf_p > 0.05) {
    cat("Data vykazují sezónnost/nestacionaritu - provádím očištění.\n")
    
    # Sezónní adjustace pomocí seas()
    seas_model <- tryCatch(seas(ts_data), error = function(e) NULL)
    if (!is.null(seas_model)) {
      adjusted_ts <- final(seas_model)
      adjusted_data[[paste0(country, "_adjusted")]] <- as.numeric(adjusted_ts)
      plot(adjusted_ts, main = paste("Sezónně adjustovaná data pro", country))
    } else {
      cat("Sezónní očištění selhalo pro", country, "\n")
      adjusted_data[[paste0(country, "_adjusted")]] <- quarterly_data[[country]]  # Použij původní, pokud selže
    }
  } else {
    cat("Data nevyžadují sezónní očištění - používám původní.\n")
    adjusted_data[[paste0(country, "_adjusted")]] <- quarterly_data[[country]]
  }
}

# Zobrazení očištěných dat
print(adjusted_data)

# 5. Uložení do stejného Excel souboru do nového sheetu (např. "Adjusted_Data")
wb <- loadWorkbook(file_path)
addWorksheet(wb, "Adjusted_Data")
writeData(wb, "Adjusted_Data", adjusted_data)
saveWorkbook(wb, file_path, overwrite = TRUE)
cat("Data uložena do souboru", file_path, "v sheetu 'Adjusted_Data'.\n")
