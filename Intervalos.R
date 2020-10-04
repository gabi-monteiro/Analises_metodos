### INTERVALO DE CONFIANÇA DAS AMOSTRAS



#BAIXANDO E INTASLANDO OS PACOTES


pacman::p_load(purrr, plyr, tidyverse, ggcorrplot, scales, stringr, cowplot, RColorBrewer, xtable, readxl,lubridate, janitor, e1071, moments)

pacman::p_load(devtools, statsr)


#LENDO O BANCO DE DADOS

banco_dados <- read.csv("C:\\Users\\Gabriela\\Documents\\analise_descritiva_sabe\\bd.csv", encoding = "UTF-8")






z_95 <- qnorm(0.975)

media_lp <- mean(banco_dados$NOTA_LP)

media_mt <- mean(banco_dados$NOTA_MT)

prop_area_interior <- (nrow(banco_dados[banco_dados$AREA == "Interior", ])/2000)

prop_sexo_feminino <- (nrow(banco_dados[banco_dados$SEXO == "Feminino", ])/2000)

amostras_30 <- banco_dados %>% rep_sample_n(size = 30, reps = 50, replace = FALSE) #AMOSTRAS TAMANHO 30

amostras_100 <- banco_dados %>% rep_sample_n(size = 100, reps = 50, replace = TRUE) #AMOSTRAS TAMANHO 100










#INTERVALOS PARA A MEDIA DE LP (AMOSTRAS TAMANHO 30)



notaslp_amostra30 <- amostras_30 %>% summarise(lower = mean(NOTA_LP) - z_95 * (sd(NOTA_LP) / sqrt(30)), 
                                               upper = mean(NOTA_LP) + z_95 * (sd(NOTA_LP) / sqrt(30)))



notaslp_amostra30 <- notaslp_amostra30  %>%
  mutate(legenda = ifelse(lower < media_lp & upper > media_lp, "Contém", "Não Contém"))

banco_lp30 <- data.frame(lp_id = c(1:50, 1:50),
                         lp_intervalo = c(notaslp_amostra30$lower, notaslp_amostra30$upper),
                         legenda = c(notaslp_amostra30$legenda, notaslp_amostra30$legenda))

ggplot(data = banco_lp30, aes(x = lp_intervalo, y = lp_id, 
                              group = lp_id, color = legenda)) +
  geom_point(size = 2) +  
  geom_line(size = 1.05) + 
  scale_colour_manual(name="O intervalo contém o parâmetro?", values = c("#7B68EE", "#FF1493"))+
  labs(x="Intervalos de Confiança", y="Amostras") +
  geom_vline(xintercept = media_lp, color = "#000000") + 
  annotate(x = media_lp,y=-3, label = expression(mu == 253.88) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))




#INTERVALOS PARA MEDIA DE MT (AMOSTRAS TAMNHO 30)




notasmt_amostra30 <- amostras_30 %>% summarise(lower = mean(NOTA_MT) - z_95 * (sd(NOTA_MT) / sqrt(30)), 
                                               upper = mean(NOTA_MT) + z_95 * (sd(NOTA_MT) / sqrt(30)))



notasmt_amostra30 <- notasmt_amostra30  %>%
  mutate(legenda = ifelse(lower < media_mt & upper > media_mt, "Contém", "Não Contém"))

banco_mt30 <- data.frame(mt_id = c(1:50, 1:50),
                         mt_intervalo = c(notasmt_amostra30$lower, notasmt_amostra30$upper),
                         legenda = c(notasmt_amostra30$legenda, notasmt_amostra30$legenda))

ggplot(data = banco_mt30, aes(x = mt_intervalo, y = mt_id, 
                              group = mt_id, color = legenda)) +
  geom_point(size = 2) +  
  geom_line(size = 1.05) + 
  scale_colour_manual(name="O intervalo contém o parâmetro?", values = c("#7B68EE", "#FF1493"))+
  labs(x="Intervalos de Confiança", y="Amostras") +
  geom_vline(xintercept = media_mt, color = "#000000") + 
  annotate(x = media_mt,y=-3, label = expression(mu == 252.342) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))






# INTERVALOS PARA A MÉDIA DE NOTA DE PORTUGUÊS (AMOSTRAS TAMANHO 100)




notaslp_amostra100 <- amostras_100 %>% summarise(lower = mean(NOTA_LP) - z_95 * (sd(NOTA_LP) / sqrt(30)), 
                                              upper = mean(NOTA_LP) + z_95 * (sd(NOTA_LP) / sqrt(30)))







notaslp_amostra100 <- notaslp_amostra100  %>%
  mutate(legenda = ifelse(lower < media_lp & upper > media_lp, "Contém", "Não Contém"))

banco_lp100 <- data.frame(lp_id = c(1:50, 1:50),
                       lp_intervalo = c(notaslp_amostra100$lower, notaslp_amostra100$upper),
                       legenda = c(notaslp_amostra100$legenda, notaslp_amostra100$legenda))

ggplot(data = banco_lp100, aes(x = lp_intervalo, y = lp_id, 
                            group = lp_id, color = legenda)) +
  geom_point(size = 2) +  
  geom_line(size = 1.05) + 
  scale_colour_manual(name="O intervalo contém o parâmetro?", values = c("#7B68EE", "#FF1493"))+
  labs(x="Intervalos de Confiança", y="Amostras") +
  geom_vline(xintercept = media_lp, color = "#000000") + 
  annotate(x = media_lp,y=-3, label = expression(mu == 253.88) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))







# INTERVALOS PARA MT (AMOSTRAS TAMANHO 100)




notasmt_amostra100 <- amostras_100 %>% summarise(lower = mean(NOTA_MT) - z_95 * (sd(NOTA_MT) / sqrt(30)), 
                                               upper = mean(NOTA_MT) + z_95 * (sd(NOTA_MT) / sqrt(30)))



notasmt_amostra100 <- notasmt_amostra100  %>%
  mutate(legenda = ifelse(lower < media_mt & upper > media_mt, "Contém", "Não Contém"))

banco_mt100 <- data.frame(mt_id = c(1:50, 1:50),
                         mt_intervalo = c(notasmt_amostra100$lower, notasmt_amostra100$upper),
                         legenda = c(notasmt_amostra100$legenda, notasmt_amostra100$legenda))

ggplot(data = banco_mt100, aes(x = mt_intervalo, y = mt_id, 
                              group = mt_id, color = legenda)) +
  geom_point(size = 2) +  
  geom_line(size = 1.05) + 
  scale_colour_manual(name="O intervalo contém o parâmetro?", values = c("#7B68EE", "#FF1493"))+
  labs(x="Intervalos de Confiança", y="Amostras") +
  geom_vline(xintercept = media_mt, color = "#000000") + 
  annotate(x = media_mt,y=-3, label = expression(mu == 252.342) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))



#intervalos para proporção interior (amostras tamanho 30)

amostras_30_n <- amostras_30[amostras_30$AREA == "Interior", ]


propinte_amostra30 <- amostras_30_n %>% summarise(lower = (length(replicate)/30) - z_95 * sqrt(((length(replicate)/30)*(1 - (length(replicate)/30))/30)),
                                                  upper = (length(replicate)/30) + z_95 * sqrt(((length(replicate)/30)*(1 - (length(replicate)/30))/30)))



propinte_amostra30 <- propinte_amostra30  %>%
  mutate(legenda = ifelse(lower < prop_area_interior & upper > prop_area_interior, "Contém", "Não Contém"))

banco_interior30 <- data.frame(int_id = c(1:50, 1:50),
                         int_intervalo = c(propinte_amostra30$lower, propinte_amostra30$upper),
                         legenda = c(propinte_amostra30$legenda, propinte_amostra30$legenda))

ggplot(data = banco_interior30, aes(x = int_intervalo, y = int_id, 
                              group = int_id, color = legenda)) +
  geom_point(size = 2) +  
  geom_line(size = 1.05) +
  scale_colour_manual(name="O intervalo contém o parâmetro?", values = c("#7B68EE", "#FF1493"))+
  labs(x="Intervalos de Confiança", y="Amostras") +
  geom_vline(xintercept = prop_area_interior, color = "black") + 
  annotate(x = prop_area_interior,y=-3, label = expression(mu == 0.829) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))


#intervalo de prop para sexo (amostras tamanho 30)

amostras_30_f <- amostras_30[amostras_30$SEXO == "Feminino", c(1, 11) ]

amostras_30_f <- na.omit(amostras_30_f)


propsexo_amostra30 <- amostras_30_f %>% summarise(lower = (length(replicate)/30) - z_95 * sqrt(((length(replicate)/30)*(1 - (length(replicate)/30))/30)),
                                                  upper = (length(replicate)/30) + z_95 * sqrt(((length(replicate)/30)*(1 - (length(replicate)/30))/30)))



propsexo_amostra30 <- propsexo_amostra30  %>%
  mutate(legenda = ifelse(lower < prop_sexo_feminino & upper > prop_sexo_feminino, "Contém", "Não Contém"))

banco_sexo30 <- data.frame(sexo_id = c(1:50, 1:50),
                               sexo_intervalo = c(propsexo_amostra30$lower, propsexo_amostra30$upper),
                               legenda = c(propsexo_amostra30$legenda, propsexo_amostra30$legenda))

ggplot(data = banco_sexo30, aes(x = sexo_intervalo, y = sexo_id, 
                                    group = sexo_id, color = legenda)) +
  geom_point(size = 2) +  
  geom_line(size = 1.05) +
  scale_colour_manual(name="O intervalo contém o parâmetro?", values = c("#7B68EE", "#FF1493"))+
  labs(x="Intervalos de Confiança", y="Amostras") +
  geom_vline(xintercept = prop_sexo_feminino, color = "black") + 
  annotate(x = prop_sexo_feminino,y=-3, label = expression(mu == 0.525) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))



#INTERVALOS PARA PROP SEXO (AMOSTRAS TAMANHO 100)


amostras_100_f <- amostras_100[amostras_100$SEXO == "Feminino", c(1, 11) ]

amostras_100_f <- na.omit(amostras_100_f)


propsexo_amostra100 <- amostras_100_f %>% summarise(lower = (length(replicate)/100) - z_95 * sqrt(((length(replicate)/100)*(1 - (length(replicate)/100))/100)),
                                                  upper = (length(replicate)/100) + z_95 * sqrt(((length(replicate)/100)*(1 - (length(replicate)/100))/100)))



propsexo_amostra100 <- propsexo_amostra100  %>%
  mutate(legenda = ifelse(lower < prop_sexo_feminino & upper > prop_sexo_feminino, "Contém", "Não Contém"))

banco_sexo100 <- data.frame(sexo_id = c(1:50, 1:50),
                           sexo_intervalo = c(propsexo_amostra100$lower, propsexo_amostra100$upper),
                           legenda = c(propsexo_amostra100$legenda, propsexo_amostra100$legenda))

ggplot(data = banco_sexo100, aes(x = sexo_intervalo, y = sexo_id, 
                                group = sexo_id, color = legenda)) +
  geom_point(size = 2) +  
  geom_line(size = 1.05) +
  scale_colour_manual(name="O intervalo contém o parâmetro?", values = c("#7B68EE", "#FF1493"))+
  labs(x="Intervalos de Confiança", y="Amostras") +
  geom_vline(xintercept = prop_sexo_feminino, color = "black") + 
  annotate(x = prop_sexo_feminino,y=-3, label = expression(mu == 0.525) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))



#INTERVALOS PARA PROP INTERIOR (AMOSTRAS TAMANHO 100)


amostras_100_n <- amostras_100[amostras_100$AREA == "Interior", ]


propinte_amostra100 <- amostras_100_n %>% summarise(lower = (length(replicate)/100) - z_95 * sqrt(((length(replicate)/100)*(1 - (length(replicate)/100))/100)),
                                                  upper = (length(replicate)/100) + z_95 * sqrt(((length(replicate)/100)*(1 - (length(replicate)/100))/100)))



propinte_amostra100 <- propinte_amostra100  %>%
  mutate(legenda = ifelse(lower < prop_area_interior & upper > prop_area_interior, "Contém", "Não Contém"))

banco_interior100 <- data.frame(int_id = c(1:50, 1:50),
                               int_intervalo = c(propinte_amostra100$lower, propinte_amostra100$upper),
                               legenda = c(propinte_amostra100$legenda, propinte_amostra100$legenda))

ggplot(data = banco_interior100, aes(x = int_intervalo, y = int_id, 
                                    group = int_id, color = legenda)) +
  geom_point(size = 2) +  
  geom_line(size = 1.05) +
  scale_colour_manual(name="O intervalo contém o parâmetro?", values = c("#7B68EE", "#FF1493"))+
  labs(x="Intervalos de Confiança", y="Amostras") +
  geom_vline(xintercept = prop_area_interior, color = "black") + 
  annotate(x = prop_area_interior,y=-3, label = expression(mu == 0.829) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))


