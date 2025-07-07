################################################################################
########################## NEOKLASICKÝ MAKRO MODEL #############################
################################################################################

rm(list=ls(all=TRUE))

# Počet scénářů
S <- 6

# Vektory pro uložení řešení různých scénářů
Y_star <- vector(length = S) # Income/output
w_star <- vector(length=S) # Real wage
C_star <- vector(length=S) # Consumption
I_star <- vector(length=S) # Investment
r_star <- vector(length=S) # Real interest rate
rn_star <- vector(length=S) # Nominal interest rate
N_star <- vector(length=S) # Employment
P_star <- vector(length=S) # Price level

# Vektory exogenních proměnných
M0 <- vector(length=S) # money supply
G0 <- vector(length=S) # government expenditures
A <- vector(length=S)  # productivity
Yf <- vector(length=S) # expected future income
b1 <- vector(length=S) # household preference for leisure
M0[] <- 5
G0[] <- 1
A[] <- 2
Yf[] <- 1
b1[] <- 0.4

# Nastavení hodnot parametrů pro různé scénáře
M0[2] <- 6   # scenario 2: monetary expansion 
G0[3] <- 2   # scenario 3: fiscal expansion
A[4] <- 2.5  # scenario 4: productivity boost
Yf[5] <- 0.2 # scenario 5: lower expected future income
b1[6] <- 0.8 # scenario 6: increased preference for leisure

# Nastavení hodnod fixních parametrů
a <- 0.3   # Capital elasticity of output
b2 <- 0.9  # discount rate
b3 <- 0.6  # household preference for money
K <- 5     # Exogenous capital stock
pe <- 0.02 # Expected rate of inflation
Gf <- 1    # Future government spending

# Iniciální hodnoty endogenních parametrů (jakékoliv kladné číslo)
W = C = I = Y = r = N = P = 1

# Numerické řešení modelu
for (i in 1:S){
  for (iteration in 1:1000){
    
    # Rovnice modelu
    Y = A[i] * (K^a) * N^(1-a)
    w = A[i] * (1-a) * (K^a) * N^(-a) 
    N = 1 - (b1[i])/w
    C = (1/(1+b2+b3)) * (Y - G0[i] + (Yf[i]-Gf)/(1+r) - b1[i] * (b2+b3) * log(b1[i]/w))
    r = (I^(a-1)) * a * A[i] * N^(1-a) 
    I = Y - C - G0[i]
    rn = r + pe
    P = (M0[i] * rn)/((1 + rn) * b3 * C) 
  }
  
  # Uložení výsledků do vektorů
  Y_star[i] <- Y
  w_star[i] <- w
  C_star[i] <- C
  I_star[i] <- I
  r_star[i] <- r
  N_star[i] <- N
  P_star[i] <- P
  rn_star[i] <- rn
}

# Vizualizace výsledků
# Výstup
barplot(Y_star, ylab="Y", names.arg=c("1: Baseline", "2: Increase in M0", 
                                      "3: Increase in G0","4: Increase in A", 
                                      "5: Decrease in Yf", "6: Increase in b1"), 
        cex.names = 0.6)

# Cenová hladina
barplot(P_star, ylab="P", names.arg=c("1: Baseline", "2: Increase in M0", 
                                      "3: Increase in G0","4: Increase in A", 
                                      "5: Decrease in Yf", "6: Increase in b1"), 
        cex.names = 0.6)

# Zaměstnanost
barplot(N_star, ylab="N", names.arg=c("1: Baseline", "2: Increase in M0", 
                                      "3: Increase in G0","4: Increase in A", 
                                      "5: Decrease in Yf", "6: Increase in b1"), 
        cex.names = 0.6)

# Spotřeba
barplot(C_star, ylab="C", names.arg=c("1: Baseline", "2: Increase in M0", 
                                      "3: Increase in G0","4: Increase in A", 
                                      "5: Decrease in Yf", "6: Increase in b1"), 
        cex.names = 0.6)

# Investice
barplot(I_star, ylab="I", names.arg=c("1: Baseline", "2: Increase in M0", 
                                      "3: Increase in G0","4: Increase in A", 
                                      "5: Decrease in Yf", "6: Increase in b1"), 
        cex.names = 0.6)

# Úroková sazba
barplot(r_star, ylab="r", names.arg=c("1: Baseline", "2: Increase in M0", 
                                      "3: Increase in G0","4: Increase in A", 
                                      "5: Decrease in Yf", "6: Increase in b1"), 
        cex.names = 0.6)

# Graf kauzálních vztahů
M_mat=matrix(c(0,0,1,0,0,0,0,0,0,0,1,0,0,
               0,0,1,0,0,0,0,0,0,0,1,0,0,
               0,1,0,0,0,0,0,0,0,0,0,0,0,
               1,1,0,0,0,1,0,0,0,1,0,1,0,
               0,0,1,0,0,1,0,0,0,0,0,0,0,
               1,0,0,1,1,0,0,0,0,1,0,0,0,
               0,0,0,0,0,0,0,0,1,0,0,0,1,
               0,0,0,0,0,1,0,0,0,0,0,0,0,
               0,0,0,0,0,0,0,0,0,0,0,0,0,
               0,0,0,0,0,0,0,0,0,0,0,0,0,
               0,0,0,0,0,0,0,0,0,0,0,0,0,
               0,0,0,0,0,0,0,0,0,0,0,0,0,
               0,0,0,1,0,0,1,1,0,0,0,0,0), 13, 13, byrow=TRUE)

A_mat=t(M_mat)

library(igraph)
dg= graph_from_adjacency_matrix(A_mat, mode="directed", weighted= NULL)

V(dg)$name=c("Y","w","N","C","I","r","P", expression(r[n]), expression(M[0]),
             expression(G[0]),"A",expression(Y^f), expression(M[d]))

plot(dg, main="", vertex.size=20, vertex.color="lightblue", 
     vertex.label.color="black", edge.arrow.size=0.3, edge.width=1.1, edge.size=1.2,
     edge.arrow.width=1.2, edge.color="black", vertex.label.cex=1.2, 
     vertex.frame.color="NA", margin=-0.08)
