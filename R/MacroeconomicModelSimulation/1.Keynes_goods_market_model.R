
# ŘEŠENÍ STATICKÉHO MODELU #####################################################

rm(list = ls (all = TRUE))

# počty parametrů, které budou zohledněny v modelu - nastavuje délku vektoru
# a tím nám říká kolik hodnot rovnic nás zajímá
S <- 2 

# hodnoty fixních parametrů
c0 <- 3
c1 <- 0.8

# vytvoření vektorů pro řešení modelu
Y_eq <- vector(length = S)
C_eq <- vector(length = S)

# vytvoření vektorů s měnícími se paramtery
I0 <- vector(length = S)
I0[1] <- 5
I0[2] <- 6

# pozitivní hodnota pro endogenní proměnné
Y = C = 1

# numerické řešení s 1000 iteracemi
for (i in 1:S){
  for (iteration in 1:1000){
    Y = C + I0[i]
    C = c0 + c1 * Y
  }
  
  # uložení výsledků do vektorů
  Y_eq[i] <- Y
  C_eq[i] <- C
}

# zobrazení výsledků
Y_eq
C_eq

# ŘEŠENÍ DYNAMICKÉHO MODELU ####################################################

rm(list = ls (all = TRUE))

# počet period pro simulaci
Q  <- 100

# počty parametrů, které budou zohledněny v modelu - nastavuje délku vektoru
S <- 2 

# délka periody pro trvání šoku
s <- 15 

# hodnoty fixních parametrů
c1 <- 0.8
beta <- 0.6

# vytvoření matice (S x Q) pro řešení modelu
C <- matrix(data = 1, nrow = S, ncol = Q)
I <- matrix(data = 1, nrow = S, ncol = Q)

# vytvoření matice pro exogenní proměnné
G0 <- matrix (data = 5, nrow = S, ncol = Q)

# nastavení parametrů pro různé scénáře
G0[2, s:Q] <- 6

# iterace řešení problému
for (i in 1:S){
  for (t in 2:Q){
    C[i,t] = c1 * (C[i,t-1] + I[i,t-1] + G0[i,t])
    I[i,t] = beta * (c1 * (C[i,t-1] + I[i,t-1] + G0[i,t]) - C[i,t-1])
  } # close time loop
}   # close scenarios loop

# výpočet výstupu
Y = C + I + G0

# zobrazení výsledků
Y[,Q]

# GRAF KAUZÁLNÍCH VZTAHŮ #######################################################

# vytvoření pomocné Jacobianovy matice pro 4 proměnné - 1 znamená, že proměnná 
# ve sloupci ovlivňuje proměnnou v řádku, 0 znamená, že neovlivňuje

#              Y  C  I  G0
M_mat=matrix(c(0, 1, 1, 1, # Y
               1, 0, 0, 0, # C
               0, 1, 0, 0, # I
               0, 0, 0, 0),# G0 
             4, 4, byrow=TRUE)

# transponování matice
A_mat <- t(M_mat)

# vytvoření orientovaného grafu z matice
library(igraph)

dg <- graph_from_adjacency_matrix(A_mat, mode = "directed", weighted = NULL)

# definice názvů
V(dg)$name <- c("Y", "C", "I", expression(G[0]))

plot(dg, main = "Graf kauzálních vztahů", vertex.size = 20, 
     vertex.color = "lightblue", vertex.label.color = "black", 
     edge.arrow.size = 0.3, edge.width = 1.1, edge.size = 1.2,
     edge.arrow.width = 1.2, edge.color = "black", vertex.label.cex = 1.2, 
     vertex.frame.color = "NA", margin = -0.08)
