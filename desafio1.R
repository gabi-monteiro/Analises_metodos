
#BAIXANDO E INTASLANDO OS PACOTES


pacman::p_load(purrr, plyr, tidyverse, ggcorrplot, scales, stringr, cowplot, RColorBrewer, xtable, readxl,lubridate, janitor, e1071, moments)

pacman::p_load(devtools, statsr,goftest, nortest, dgof)


## CRIANDO O DATA FRAME

media <- sample(c(100:200),1)

dp <- media*0.1

matriz <- replicate(n = 1000,
                    expr = rnorm(n = 15, mean = media, sd = dp))

matriz <- data.frame(matriz)

write.csv(matriz, file = "amostras_1000.csv", sep = ",")



### USANDO KOLMOGOROV


estatisticas <- vector()


for (i in 1:1000) {
  estatisticas[i] <- ks.test(matriz[ , i], "pnorm", media, dp )
}

estatisticas <- unlist(estatisticas)



max(estatisticas)
min(estatisticas)

estatisticas_1 <- data.frame(id = 1:1000,estatisticas)



estatisticas_2 <- cut(estatisticas, 
                           breaks = seq(0.074, 0.53, 0.076),
                           labels = c('0.074 a 0.150',
                                      '0.150 a 0.226',
                                      '0.226 a 0.302',
                                      '0.302 a 0.378',
                                      '0.378 a 0.454', 
                                      '0.454 a 0.530'))

estatisticas_2 <- data.frame(estatisticas_2)





estatisticas_2 <- estatisticas_2 %>% group_by(estatisticas_2) %>%
  summarise(freq = n()) %>% 
  mutate(freq_relativa = round((freq/sum(freq)),4)) %>% 
  mutate(freq_acumulada = cumsum(freq_relativa))




ggplot(estatisticas_1, aes(x=estatisticas_1$estatisticas)) + 
  geom_histogram(colour="white", fill="#696969",breaks = seq(0.074, 0.53, 0.076))+
  labs(x="Estimativas da estatística do Teste de Kolmogorov-Smirnov", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))



percentil_80_1 <- (0.9221*0.226 + 0.80*0.076)/0.9221
percentil_85_1 <- (0.9221*0.226 + 0.85*0.076)/0.9221
percentil_90_1 <- (0.9221*0.226 + 0.90*0.076)/0.9221
percentil_95_1 <- (0.982*0.302 + 0.95*0.076)/0.982
percentil_97_1 <- (0.982*0.302 + 0.975*0.076)/0.982
percentil_99_1 <- (1*0.454 + 0.99*0.076)






#### USANDO LILLIEFORS






estatisticas_lillie <- vector()

for (i in 1:1000) {
  estatisticas_lillie[i] <- lillie.test(matriz[ , i])
}
estatisticas_lillie <- unlist(estatisticas_lillie)


max(estatisticas_lillie)
min(estatisticas_lillie)

estatisticas_lillie_1 <- data.frame(id = 1:1000,estatistica = estatisticas_lillie)




estatisticas_lillie_2 <- cut(estatisticas_lillie, 
                           breaks = seq(0.068, 0.284, 0.036),
                           labels = c('0.068 a 0.104',
                                      '0.104 a 0.140',
                                      '0.140 a 0.176',
                                      '0.176 a 0.212',
                                      '0.212 a 0.248', 
                                      '0.248 a 0.284'))



estatisticas_lillie_2 <- data.frame(estatisticas_lillie_2)



estatisticas_lillie_2 <- estatisticas_lillie_2 %>% group_by(estatisticas_lillie_2) %>%
  summarise(freq = n()) %>% 
  mutate(freq_relativa = round((freq/sum(freq)),4)) %>% 
  mutate(freq_acumulada = cumsum(freq_relativa))


ggplot(estatisticas_lillie_1, aes(x=estatisticas_lillie_1$estatistica)) + 
  geom_histogram(colour="white", fill="#696969",breaks = seq(0.068, 0.284, 0.036))+
  labs(x="Estimativas da estatística do Teste de Kolmogorov-Smirnov(LillieFors)", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


percentil_80 <- (0.94*0.176 + 0.80*0.036)/0.94
percentil_85 <- (0.94*0.176 + 0.85*0.036)/0.94
percentil_90 <- (0.94*0.176 + 0.90*0.036)/0.94
percentil_95 <- (0.99*0.212 + 0.95*0.036)/0.99
percentil_97 <- (0.99*0.212 + 0.975*0.036)/0.99
percentil_99 <- (0.99*0.212 + 0.99*0.036)/0.99





### ESCOLHENDO 5 AMOSTRAS


amostras_5 <- matriz[ , 1:5]



shapiro.test(amostras_5$X1)
shapiro.test(amostras_5$X2)
shapiro.test(amostras_5$X3)
shapiro.test(amostras_5$X4)
shapiro.test(amostras_5$X5)


ad.test(amostras_5$X1)
ad.test(amostras_5$X2)
ad.test(amostras_5$X3)
ad.test(amostras_5$X4)
ad.test(amostras_5$X5)







