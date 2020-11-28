# ATIVIDADE 4.5 (COMPARAÇÃO ENTRE VÁRIAS POPULAÇÕES)

# INSTALANDO OS PACOTES
pacman::p_load(readr, tidyverse, ggcorrplot, stringr, cowplot, e1071, moments, devtools, statsr, goftest, nortest, EnvStats)

#CARREGANDO BANCO DE DADOS DA AMOSTRA

amostra <- read.csv("C:\\Users\\Gabriela\\Documents\\amostra_500.csv")





# amostra <- bd[sample(1:nrow(bd),500,replace=F),]   (CRIANDO A AMOSTRA)

# write.table(amostra, "amostra_500.csv", sep = ",") (SALVANDO A AMOSTRA)




# JÁ FOI TESTADA A NORMALIDADE EM OUTRA ATIVIDADE (ASSUME NORMALIDADE)

#TESTANDO A VARIÂNCIA (BARTLETT)

bartlett.test (amostra$NOTA_MT ~ amostra$REGIAO) #variâncias diferentes

tempo_uso <- amostra[ ,c(8,17)]
tempo_uso <- na.omit(tempo_uso)


tempo_uso %>% distinct(USO_TEMPO_TELAS)

tempo_uso$USO_TEMPO_TELAS <- tempo_uso$USO_TEMPO_TELAS %>% str_replace("Não vejo TV, não navego na internet e não jogo jogos eletrônicos", "Menos de 1 hora")


bartlett.test(tempo_uso$NOTA_LP ~ tempo_uso$USO_TEMPO_TELAS) #variâncias iguais





#  COMPARANDO AS MÉDIAS

# REGIÃO E NOTAS DE MATEMÁTICA

anova_mat <- aov(amostra$NOTA_MT ~ amostra$REGIAO)
summary(anova_mat)

pairwise.t.test(amostra$NOTA_MT, amostra$REGIAO, p.adjust.method = "bonferroni" )

#TEMPO DE USO E NOTAS DE PORTUGUÊS

anova_port <- aov(tempo_uso$NOTA_LP ~ tempo_uso$USO_TEMPO_TELAS)
summary(anova_port)

pairwise.t.test(tempo_uso$NOTA_LP, tempo_uso$USO_TEMPO_TELAS, p.adjust.method = "bonferroni")



# GRÁFICOS 

# BOX-PLOT MAT

titulos <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")

ggplot(amostra, aes(x=factor(REGIAO, levels = titulos), y=NOTA_MT)) +
  geom_boxplot(fill=c("#a11d21"), width = 0.5) +
  labs(x="Região", y="Notas de Matemática") +
  stat_summary(fun.y=mean, geom="point", shape=15, size=2) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))



# BOX-PLOT DA PORT

ordem <- c("Menos de 1 hora", "Entre 1 e 2 horas", "Mais de 2 horas, até 3 horas", "Mais de 3 horas")

ggplot(tempo_uso, aes(x=factor(USO_TEMPO_TELAS, levels = ordem), y=NOTA_LP)) +
  geom_boxplot(fill=c("#a11d21"), width = 0.5) +
  labs(x="Tempo de Uso das Telas ", y="Notas de Português") +
  stat_summary(fun.y=mean, geom="point", shape=15, size=2) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
