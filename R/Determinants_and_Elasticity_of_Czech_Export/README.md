# R Script for Master's Thesis

**Thesis title:** *Determinants and Elasticity of Czech Exports*  
**Author:** Petr Chaloupek ([petrchaloupek@gmail.com](mailto:petrchaloupek@gmail.com))  
**Faculty:** Faculty of Economics and Administration, Masaryk University  
**Year:** 2025  
**LinkedIn:** [linkedin.com/in/petrchaloupek](https://www.linkedin.com/in/petrchaloupek/)  

---

## About

This repository contains the R script used in my master's thesis *"Determinants and Elasticity of Czech Exports"*.  
The script implements the full empirical workflow, including data preprocessing, transformation into gap form, estimation of the Bayesian VAR-X model, recursive estimation, and generation of all figures and tables presented in the thesis.

---

## Data

The required datasets are provided in `.xlsx` format and are:  
- included in the electronic submission of the thesis,  
- optionally available in this repository (if permitted by data licensing).  

All paths used in the script can be adjusted in the configuration section at the beginning of the main R file.

---

## Dependencies

The analysis relies on the following R packages:

- `readxl` – import of Excel files  
- `dplyr` – data manipulation  
- `tidyr` – data structuring  
- `ggplot2` – visualisation  
- `mFilter` – extraction of cyclical components (Hodrick–Prescott filter)  
- `lubridate` – date handling  
- Bayesian VAR packages, depending on implementation:  
  - `BMR`  
  - `BVAR`  
  - or `bayesm`  

Install required packages using:

```r
install.packages(c("readxl", "dplyr", "tidyr", "ggplot2",
                   "mFilter", "lubridate", "vars"))
