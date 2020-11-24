if(!require(rstatix)) install.packages("rstatix") 
library(rstatix) 
if(!require(reshape)) install.packages("reshape") 
library(reshape) 
if(!require(PMCMRplus)) install.packages("PMCMRplus") 
library(PMCMRplus) 




# QUESTÃO 1 CAP 15 13)

metodo1 <- c(3,5,2,4,8,4,3,9)
metodo2 <- c(4,4,3,8,7,4,2,5)
metodo3 <- c(6,7,8,6,7,9,10,9)

questao1 <- tibble(metodo1,metodo2,metodo3)

questao1_1 <- gather(questao1, key = "metodos", value = "observacoes")
questao1_1$observacoes <- as.numeric(questao1_1$observacoes)

LeveneTest(questao1_1$observacoes, questao1_1$metodos, center = mean)

bartlett.test(questao1_1$observacoes,questao1_1$metodos)

aov(data = questao1_1, observacoes ~ metodos)
summary(anova)

pairwise.t.test(questao1_1$observacoes, questao1_1$metodos, p.adjust.method = "bonferroni" )

#Questao 3

Id <- c(1,2,3,4,5,6,7,8)
A <- c(18,7,13,15,12,11,15,10)
B <- c(14,6,14,10,11,9,16,8)
C <- c(16,5,16,12,12,9,10,11)
D <- c(20,10,17,14,18,16,14,16)

banco <- data.frame(Id, A,B,C,D)

glimpse(banco)


dadosl <- melt(banco,
               id = "Id",
               measured = C("A","B","C","D"))

colnames(dadosl) = c("Id", "Listas","Nota")


dadosl <- sort_df(dadosl, vars = "Id")

dadosl$Id <- factor(dadosl$Id)

friedman.test(Nota ~ Listas | Id, data = dadosl)

frdAllPairsSiegelTest(dadosl$Nota, dadosl$Listas,
                      dadosl$Id, p.adjust.method = "bonferroni")
