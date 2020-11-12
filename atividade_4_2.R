pacman::p_load(purrr, plyr, tidyverse, ggcorrplot, scales, stringr, cowplot, RColorBrewer, xtable, readxl,lubridate, janitor, e1071, moments)

pacman::p_load(devtools, statsr,goftest, nortest, dgof,leaflet,leaflet.extras, BSDA, gridExtra)


amostras_30 <- read.csv("C:\\Users\\Gabriela\\Documents\\atividade met2\\amostras_30_1.csv")
amostras_100 <- read.csv("C:\\Users\\Gabriela\\Documents\\atividade met2\\amostras_100.csv")




#Box-PLOT DAS NOTAS DE MAT POR LOCALIZAÇÃO (URBANA E RURAL)


# BOX-PLOT DA AMOSTRA DE TAMANHO 100

ggplot(amostras_100, aes(x=LOCALIZACAO, y=NOTA_MT)) +
  geom_boxplot(fill=c("#a11d21"), width = 0.5) +
  labs(x="Localização", y="Notas de Matemática") +
  stat_summary(fun.y=mean, geom="point", shape=15, size=2) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))



# BOX-PLOT DA AMOSTRA DE TAMANHO 30

ggplot(amostras_30, aes(x=LOCALIZACAO, y=NOTA_MT)) +
  geom_boxplot(fill=c("#a11d21"), width = 0.5) +
  labs(x="Localização", y="Notas de Matemática") +
  stat_summary(fun.y=mean, geom="point", shape=15, size=2) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))


# BOX-PLOT DAS NOTAS DE PORTUGUES POR CATEGORIA ADMINISTRATIVA (ESTADUAL E MUNICIPAL)

# BOX-PLOT DA AMOSTRA DE TAMANHO 100

ggplot(amostras_100, aes(x=DEPENDENCIA_ADM, y=NOTA_LP)) +
  geom_boxplot(fill=c("#a11d21"), width = 0.5) +
  labs(x="Categoria Administrativa", y="Notas de Português") +
  stat_summary(fun.y=mean, geom="point", shape=15, size=2) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))


# BOX-PLOT DA AMOSTRA DE TAMANHO 30

ggplot(amostras_30, aes(x=DEPENDENCIA_ADM, y=NOTA_LP)) +
  geom_boxplot(fill=c("#a11d21"), width = 0.5) +
  labs(x="Categoria Administrativa", y="Notas de Português") +
  stat_summary(fun.y=mean, geom="point", shape=15, size=2) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))




# TESTES PARA VER SE AS DUAS POPULAÇÕES TEM DISTRIBUIÇÕES DIFERENTES

# TESTES PARA LOCALIZAÇÃO E NOTAS DE MATEMÁTICA


#AMOSTRAS DE TAMANHO 30

urbana_30 <- amostras_30[amostras_30$LOCALIZACAO == "Urbana",]
rural_30 <- amostras_30[amostras_30$LOCALIZACAO == "Rural", ]

var.test(urbana_30$NOTA_MT, rural_30$NOTA_MT)
ks.test(rural_30$NOTA_MT, urbana_30$NOTA_MT, alternative = "two.sided")
t.test(urbana_30$NOTA_MT, rural_30$NOTA_MT, alternative = "two.sided", var.equal = TRUE)
wilcox.test(urbana_30$NOTA_MT, rural_30$NOTA_MT, alternative = "two.sided")

# AMOSTRAS DE TAMANHO 100

urbana_100 <- amostras_100[amostras_100$LOCALIZACAO == "Urbana",]
rural_100 <- amostras_100[amostras_100$LOCALIZACAO == "Rural", ]

var.test(urbana_100$NOTA_MT, rural_100$NOTA_MT)
ks.test(rural_100$NOTA_MT, urbana_100$NOTA_MT, alternative = "greater")
t.test(urbana_100$NOTA_MT, rural_100$NOTA_MT, alternative = "greater", var.equal = TRUE)
wilcox.test(urbana_100$NOTA_MT, rural_100$NOTA_MT, alternative = "greater")





# TESTES PARA CATEGORIA ADMINISTRATIVA E NOTAS DE PORTUGUÊS

#AMOSTRAS DE TAMANHO 30

estadual_30 <- amostras_30[amostras_30$DEPENDENCIA_ADM == "Estadual",]
municipal_30 <- amostras_30[amostras_30$DEPENDENCIA_ADM == "Municipal", ]

var.test(estadual_30$NOTA_LP, municipal_30$NOTA_LP)
ks.test(estadual_30$NOTA_LP, municipal_30$NOTA_LP, alternative = "two.sided")
t.test(estadual_30$NOTA_LP, municipal_30$NOTA_LP, alternative = "two.sided", var.equal = TRUE)
wilcox.test(estadual_30$NOTA_LP, municipal_30$NOTA_LP, alternative = "two.sided")

# AMOSTRAS DE TAMANHO 100

estadual_100 <- amostras_100[amostras_100$DEPENDENCIA_ADM == "Estadual",]
municipal_100 <- amostras_100[amostras_100$DEPENDENCIA_ADM == "Municipal", ]

var.test(estadual_100$NOTA_LP, municipal_100$NOTA_LP) #teste f para homocedasticidade
ks.test(estadual_100$NOTA_LP, municipal_100$NOTA_LP, alternative = "two.sided")
t.test(estadual_100$NOTA_LP, municipal_100$NOTA_LP, alternative = "two.sided", var.equal = TRUE)
wilcox.test(estadual_100$NOTA_LP, municipal_100$NOTA_LP, alternative = "two.sided")





# TESTE PARA AMOSTRA DE TAMANHO 30  

#VER SE HÁ DIFERENÇA ENTRE AS NOTAS DE PORTUGUÊS E MATEMÁTICA


SIGN.test(amostras_30$NOTA_LP, amostras_30$NOTA_MT, md=0, alternative="two.sided") #teste dos sinais
wilcox.test(amostras_30$NOTA_LP, amostras_30$NOTA_MT, paired=TRUE,alternative = "two.sided") #teste de postos com sinais de wilcoxon
t.test(amostras_30$NOTA_LP, amostras_30$NOTA_MT, paired=TRUE,alternative = "two.sided") #teste t para amostras pareadas




# BOX-PLOT PARA NOTAS DE PORTUGUÊS E MATEMÁTICA

box_1 <- amostras_30 %>% ggplot(aes(x=NOTA_LP)) +
  geom_histogram(breaks = seq(137,342,41), 
                 aes(col=I("white"), y = ..density..), 
                 fill = "#a11d21") + 
  scale_x_continuous(breaks = seq(137,342,41)) +
  labs(x='Notas de Português', 
       y="Densidade") + 
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))



box_2 <- amostras_30 %>% ggplot(aes(x=NOTA_MT)) + 
  geom_histogram(breaks = seq(143, 333,38), 
                 aes(col=I("white"), y = ..density..), 
                 fill = "#a11d21") + 
  scale_x_continuous(breaks = seq(143, 333,38)) +
  labs(x='Notas de Matemática', 
       y="Densidade") + 
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
  

grid.arrange(box_1, box_2)
