if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                                
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix) 
if(!require(reshape)) install.packages("reshape") 
library(reshape) 
if(!require(PMCMRplus)) install.packages("PMCMRplus") 
library(PMCMRplus)   
if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2) 
if(!require(EnvStats)) install.packages("EnvStats") 
library(EnvStats)
if(!require(NSM3)) install.packages("NSM3") 
library(NSM3)
if(!require(sna)) install.packages("sna") 
library(sna)

# CÓDIGO COPIADO DA INTERNET



P  <- 3                     # número de grupos
Nj <- c(2, 2, 2)            # Tamanhos dos grupos
N  <- sum(Nj)               # N total
IV <- factor(rep(1:P, Nj))  # fator de agrupamento
alpha <- 0.05               # alpha-level


nPerms <- min(factorial(N), 720)


permIdx <- unique(round(runif(nPerms) * (factorial(N)-1)))
nPerms  <- length(permIdx) #Número de permutações
H       <- numeric(nPerms)  # vetor que contém as estatísticas T
#t       <- numeric(nPerms)
# função para calcular a estatística de teste de uma dada permutação de classificação
getH <- function(ranks) {
  Rj <- tapply(ranks, IV, sum)
  (12 / (N*(N+1))) * sum((1/Nj) * (Rj-(Nj*(N+1) / 2))^2)
}

# getHAss <- function(ranks) {
#   Rj <- tapply(ranks, IV, sum)
#   (1/((1/(N-1))*(sum((Rj^2)-N*((N+1)^2)/4))))*(sum(((1/Nj)*(Rj^2))-N*((N+1)^2)/4))
# }


# todas as estatísticas de teste para a amostra aleatória de permutações de classificação (quebras para N maior)
# numperm () ordena internamente todos os N! permutações e retorna aquele com um índice desejado
          
for(i in seq(along=permIdx)) { H[i] <- getH(numperm(N, permIdx[i]-1)) }
#probabilidade de T ser igual a t
prob <- table(round(H, 4)) / nPerms

# frequências relativas cumulativas de estatística de teste de permutações aleatórias
pKWH   <- cumsum(table(round(H, 4)) / nPerms)
qPerm  <- quantile(H, probs=1-alpha)  
qAsymp <- qchisq(1-alpha, P-1)        

# gráfico com as probabilidades acumuladas da distribuição exata 
plot(names(pKWH), pKWH, main="Distribuição Exata",
     type="n", xlab="T", ylab="P(T <= t)", cex.lab=1.4)
points(names(pKWH), pKWH, pch=16, col="red")
abline(h=0.95, col="blue")                         # level alpha
abline(v=c(qPerm, qAsymp), col=c("red", "black"))  # critical values


# GRÁFICO DA DISTRIBUIÇÃO ASSINTÓTICA
plot(names(pKWH), pKWH, main="Distribuição Assintótica",
     type="n", xlab="T", ylab="P(T <= t)", cex.lab=1.4)
curve(pchisq(x, P-1), lwd=2, n=200, add=TRUE)
abline(h=0.95, col="blue")                         # level alpha
abline(v=c(qPerm, qAsymp), col=c("red", "black"))  # critical values
pch=c(16, NA), col=c("red", "black"), lty=c(NA, 1), lwd=c(NA, 2))

#GRÁFICO DA DISTRIBUIÇÃO ASSINTÓTICA VS EXATA
plot(names(pKWH), pKWH, main="Distribuição Assintótica x Exata",
     type="n", xlab="T", ylab="P(T <= t)", cex.lab=1.4)
points(names(pKWH), pKWH, pch=16, col="red")
curve(pchisq(x, P-1), lwd=2, n=200, add=TRUE)
abline(h=0.95, col="blue")                         # level alpha
abline(v=c(qPerm, qAsymp), col=c("red", "black"))  # critical values
legend(x="bottomright", legend=c("Exata", "Assintótica"),
pch=c(16, NA), col=c("red", "black"), lty=c(NA, 1), lwd=c(NA, 2))

