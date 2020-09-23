pacman::p_load(plyr, tidyverse, ggcorrplot, scales, stringr, cowplot, RColorBrewer, xtable, readxl,lubridate, janitor)

pacman::p_load(e1071, moments)

bd <- read.csv("bd.csv", encoding = "UTF-8")


bd_codigo <- read_xlsx("relatorio_codigos.xlsx", sheet=1, col_names=TRUE)
bd_codigo$MUNICIPIO <- as.numeric(bd_codigo$MUNICIPIO)
bd_codigo <- bd_codigo[, c("MUNICIPIO", "Nome_Municipio") ]


bd_municipios <- join(bd, bd_codigo, type = "inner")



##----------------------------------------area--------------------------------------------------------

bd %>% distinct(AREA)

bd_area <- bd %>% group_by(AREA) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq)),4))

ggplot(bd_area, aes(x=AREA, y=freq_relativa)) + 
  geom_bar(stat="identity",position='dodge', fill = "#696969") + 
  scale_y_continuous(limits = c(0,1), expand = c(0,0), breaks = seq(0,1,.1),labels = paste0(seq(0,100,10),'%')) +
  labs(x="Área (Capital e Interior)", y="Porcentagem") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")

xtable(table(bd$AREA))


##----------------------------------------Ano de nascimento --------------------------------------------------
bd %>% distinct(ANO_NASC)

bd_ano <- bd %>% group_by(ANO_NASC) %>%
  summarise(freq = n())


bd_ano <- bd_ano %>%  na.omit()  %>%
  mutate(freq_relativa = round((freq/sum(freq)),4))

ggplot(bd_ano, aes(x=ANO_NASC, y=freq_relativa)) + 
  geom_bar(stat="identity",position='dodge', fill = "#696969") + 
  scale_y_continuous(limits = c(0,0.5), expand = c(0,0), breaks = seq(0,0.5,.1),labels = paste0(seq(0,50,10),'%')) +
  labs(x="Ano de Nascimento", y="Porcentagem") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")



#----------------------------------- Afazeres----------------------------
bd %>% distinct(AFAZERES_DOM)


bd_afazeres <- bd %>% group_by(AFAZERES_DOM) %>% summarise(freq_1 = n()) 
  


bd_afazeres <- bd_afazeres %>% na.omit() %>% mutate(freq_relativa_1 = round((freq_1/sum(freq_1))*100,2))


level_order <- c("Menos de 1 hora", "Entre 1 e 2 horas", "Não faço trabalhos domésticos", "Mais de 2 horas, até 3 horas", "Mais de 3 horas")


porcentagem_1<- str_c(bd_afazeres$freq_relativa_1, "%") %>%
  str_replace('\\.',',')



ggplot(bd_afazeres, aes(x ="", y=freq_relativa_1, fill= AFAZERES_DOM )) + geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0, direction = -1) + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_blank(),
    legend.title = element_blank()) + 
  geom_text(data = bd_afazeres, 
            aes(x ="", y=freq_relativa_1, label = porcentagem_1),
            position = position_stack(vjust = 0.5))

xtable(table(bd$AFAZERES_DOM))


##----------------------------------Computador----------------------------------------------------------------------


bd %>% distinct(COMPUTADOR)


bd_computador <- bd %>% group_by(COMPUTADOR) %>%
  summarise(freq_2 = n()) 


bd_computador <- bd_computador %>% na.omit() %>%
  mutate(freq_relativa_2 = round((freq_2/sum(freq_2)),4))


level_order2 <- c("Não tem", "Sim, um", "Sim, dois", "Sim, três", "Sim, quatro ou mais")


ggplot(bd_computador, aes(x=factor(COMPUTADOR, levels = level_order2) , y=freq_relativa_2)) + 
  geom_bar(stat="identity",position='dodge', fill = "#696969") + 
  scale_y_continuous(limits = c(0,0.5), expand = c(0,0), breaks = seq(0,0.5,.1),labels = paste0(seq(0,50,10),'%')) +
  labs(x="Computador", y="Porcentagem") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")


#-----------------------------------------Dependência------------------------------------------------



bd %>% distinct(DEPENDENCIA_ADM)


bd_dependencia <- bd %>% group_by(DEPENDENCIA_ADM) %>%
  summarise(freq_3 = n()) %>%
  mutate(freq_relativa_3 = round((freq_3/sum(freq_3)),4))




level_order3 <- c("Estadual", "Municipal", "Federal")


ggplot(bd_dependencia, aes(x= factor(DEPENDENCIA_ADM, levels = level_order3 ), y=freq_relativa_3)) + 
  geom_bar(stat="identity",position='dodge', fill = "#696969") + 
  scale_y_continuous(limits = c(0,0.6), expand = c(0,0), breaks = seq(0,0.6,.1),labels = paste0(seq(0,60,10),'%')) +
  labs(x="Dependência", y="Porcentagem") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")

xtable(table(bd$COMPUTADOR))



##---------------------------------Escolaridade Mãe------------------------------------------------





bd %>% distinct(ESC_MAE)



bd_mae <- bd %>% group_by(ESC_MAE) %>%
  summarise(freq_4 = n())

bd_mae <- bd_mae %>% na.omit() %>%
  mutate(freq_relativa4 = round((freq_4/sum(freq_4)*100),2))


level_order4 <- c("Completou o Ensino Médio, mas não completou a Faculdade", "Não sei",
                  "Completou a 8.ª série/9.º ano do Ensino Fundamental, mas não completou o Ensino Médio",
                  "Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano do Ensino Fundamental",
                  "Não completou a 4.ª série/5.º ano do Ensino Fundamental",
                  "Completou a Faculdade", "Nunca estudou")

porcentagens_est<- str_c(bd_mae$freq_relativa4, "%") %>%
  str_replace('\\.',',')



ggplot(bd_mae, aes(x ="", y=freq_relativa4, fill= ESC_MAE )) + geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0, direction = -1) + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_blank(),
    legend.title = element_blank()) + 
  geom_text(data = bd_mae, 
            aes(x ="", y=freq_relativa4, label = porcentagens_est),
            position = position_stack(vjust = 0.5))




##-------------------------------------------- EScolaridade Pai ----------------------------------------------





bd %>% distinct(ESC_PAI)



bd_pai <- bd %>% group_by(ESC_PAI) %>%
  summarise(freq_5 = n())


bd_pai <- bd_pai %>% na.omit() %>%
  mutate(freq_relativa5 = round((freq_5/sum(freq_5)*100),2))


level_order5 <- c("Completou o Ensino Médio, mas não completou a Faculdade", "Não sei",
                  "Completou a 8.ª série/9.º ano do Ensino Fundamental, mas não completou o Ensino Médio",
                  "Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano do Ensino Fundamental",
                  "Não completou a 4.ª série/5.º ano do Ensino Fundamental",
                  "Completou a Faculdade", "Nunca estudou")

porcentagens_5<- str_c(bd_pai$freq_relativa5, "%") %>%
  str_replace('\\.',',')



ggplot(bd_pai, aes(x ="", y=freq_relativa5, fill= ESC_PAI )) + geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0, direction = -1) + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_blank(),
    legend.title = element_blank()) + 
  geom_text(data = bd_pai, 
            aes(x ="", y=freq_relativa5, label = porcentagens_5),
            position = position_stack(vjust = 0.5))



##--------------------------------------- Local ---------------------------------------



bd %>% distinct(LOCALIZACAO)


bd_local <- bd %>% group_by(LOCALIZACAO) %>%
  summarise(freq_6 = n()) %>%
  mutate(freq_relativa6 = round((freq_6/sum(freq_6)),4))





ggplot(bd_local, aes(x= LOCALIZACAO, y=freq_relativa6)) + 
  geom_bar(stat="identity",position='dodge', fill = "#696969") + 
  scale_y_continuous(limits = c(0,0.9), expand = c(0,0), breaks = seq(0,0.9,.1),labels = paste0(seq(0,90,10),'%')) +
  labs(x="Localização (Rural e Urbana)", y="Porcentagem") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")





##------------------------------Mês de nascimento----------------------------------------------------------------------------------



bd %>% distinct(MES_NASC)


bd_mes <- bd %>% group_by(MES_NASC) %>% 
  summarise(freq_7 = n())

bd_mes <- bd_mes  %>% na.omit()  %>%
  mutate(freq_relativa7 = round((freq_7/sum(freq_7)),4))



level_order7 <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro",
                  "Novembro", "Dezembro")


ggplot(bd_mes, aes(x=factor(MES_NASC, levels = level_order7) , y=freq_relativa7)) + 
  geom_bar(stat="identity",position='dodge', fill = "#696969") + 
  scale_y_continuous(limits = c(0,0.15), expand = c(0,0), breaks = seq(0,0.15, by= 0.05),labels = paste0(seq(0,15,5),'%')) +
  labs(x="Mês de Nascimento", y="Porcentagem") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")



### Perspectiva (Gráfico de Colunas Univariado)




bd %>% distinct(PERSPECTIVAS)


bd_perspectiva <- bd %>% group_by(PERSPECTIVAS) %>%
  summarise(freq_8 = n()) 


bd_perspectiva <- bd_perspectiva %>% na.omit() %>%
  mutate(freq_relativa8 = round((freq_8/sum(freq_8)),4))


level_order8 <- c("Continuar estudando e trabalhar", "Somente continuar estudando", "Ainda não sei", "Somente trabalhar")


ggplot(bd_perspectiva, aes(x=factor(PERSPECTIVAS, levels = level_order8) , y=freq_relativa8)) + 
  geom_bar(stat="identity",position='dodge', fill = "#696969") + 
  scale_y_continuous(limits = c(0,0.6), expand = c(0,0), breaks = seq(0,0.6, by= 0.1),labels = paste0(seq(0,60,10),'%')) +
  labs(x="Perspectiva", y="Porcentagem") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")


##----------------------------------------------- Raça -----------------------------------------------------------------------




bd %>% distinct(RACA_COR)


bd_raca <- bd %>% group_by(RACA_COR) %>%
  summarise(freq_9 = n())


bd_raca <- bd_raca %>% na.omit()  %>%
  mutate(freq_relativa9 = round((freq_9/sum(freq_9)),4))


level_order9 <- c("Parda", "Branca", "Preta", "Não quero declarar", "Amarela", "Indígena")


ggplot(bd_raca, aes(x=factor(RACA_COR, levels = level_order9) , y=freq_relativa9)) + 
  geom_bar(stat="identity",position='dodge', fill = "#696969") + 
  scale_y_continuous(limits = c(0,0.5), expand = c(0,0), breaks = seq(0,0.5, by= 0.1),labels = paste0(seq(0,50,10),'%')) +
  labs(x="Raça/Cor", y="Porcentagem") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")









### ----------------------------------------Municípios-------------------------------------------------------

bd_muni <- bd_municipios %>% group_by(Nome_Municipio) %>% 
  summarise(Freq = n()) %>%
  mutate(Freq_relativa = round((Freq/sum(Freq)),4)) %>% top_n(Freq, n = 10) %>% arrange(desc(Freq) )




level_order10 <- c("São Paulo", "Rio de Janeiro", "Brasília", "Belo Horizonte",
                  "Manaus", "Fortaleza", "Salvador", "Curitiba", "Recife", "Guarulhos")


ggplot(bd_muni, aes(x= factor(Nome_Municipio, levels = level_order10) , y=Freq_relativa)) + 
  geom_bar(stat="identity",position='dodge', fill = "#696969") + 
  scale_y_continuous(limits = c(0,0.05), expand = c(0,0), breaks = seq(0,0.05, by= 0.01),labels = paste0(seq(0,5,1),'%')) +
  labs(x="Municípios", y="Porcentagem") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")



##--------------------------------------------Sexo-------------------------------------------------

bd %>% distinct(SEXO)



bd_sexo <- bd %>% group_by(SEXO) %>% 
  summarise(Freq_1 = n())  

bd_sexo <-bd_sexo %>% na.omit() %>%
  mutate(Freq_relativa1 = round((Freq_1/sum(Freq_1))*100,2))

porcentagens_1<- str_c(bd_sexo$Freq_relativa1, "%") %>%
  str_replace('\\.',',')

ggplot(bd_sexo, aes(x ="", y=Freq_relativa1, fill= SEXO)) + geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0, direction = -1) + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_blank(),
    legend.title = element_blank()) + 
  geom_text(data = bd_sexo, 
            aes(x ="", y=Freq_relativa1, label = porcentagens_1),
            position = position_stack(vjust = 0.5))




##### Trabalho --------------------------------------------------------


bd %>% distinct(TRABALHO)



bd_trabalho <- bd %>% group_by(TRABALHO) %>% 
  summarise(Frequencia = n())  


porcentagem<- str_c(bd_trabalho$Freq_Relativa, "%") %>%
  str_replace('\\.',',')
bd_trabalho <- bd_trabalho  %>% na.omit() %>%
  mutate(Freq_Relativa = round((Frequencia/sum(Frequencia))*100,2))


ggplot(bd_trabalho, aes(x ="", y=Freq_Relativa, fill= TRABALHO)) + geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0, direction = -1) + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_blank(),
    legend.title = element_blank()) + 
  geom_text(data = bd_trabalho, 
            aes(x ="", y=Freq_Relativa, label = porcentagem),
            position = position_stack(vjust = 0.5))




##### USO DO TEMPO ------------------------------------------------------------------------------------------------






bd %>% distinct(USO_TEMPO_TELAS)



bd_uso_telas <- bd %>% group_by(USO_TEMPO_TELAS) %>% 
  summarise(Freq_2 = n()) 

bd_uso_telas <- bd_uso_telas  %>% na.omit()  %>%
  mutate(Freq_relativa2 = round((Freq_2/sum(Freq_2))*100,2))


level_order11 <- c("Mais de 3 horas", "Entre 1 e 2 horas", "Mais de 2 horas, até 3 horas",
                   "Menos de 1 hora", "Não vejo TV, não navego na internet e não jogo jogos eletrônicos" = "Não vejo TV, não navego na internet e não jogo jogos eletrônicos")




porcentagem1<- str_c(bd_uso_telas$Freq_relativa2, "%") %>%
  str_replace('\\.',',')



ggplot(bd_uso_telas, aes(x ="", y=Freq_relativa2, fill= USO_TEMPO_TELAS)) + geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0, direction = -1) + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_blank(),
    legend.title = element_blank()) + 
  geom_text(data = bd_uso_telas, 
            aes(x ="", y=Freq_relativa2, label = porcentagem1),
            position = position_stack(vjust = 0.5))









###### REGIÃO ----------------------------------------------------------------------------------------



bd %>% distinct(REGIAO)


bd_regiao <- bd %>% group_by(REGIAO) %>%
  summarise(freq_absoluta = n()) %>%
  mutate(frequencia_relativa = round((freq_absoluta/sum(freq_absoluta)),4))





ordem <- c("Sudeste", "Nordeste", "Sul", "Norte", "Centro-Oeste")


ggplot(bd_regiao, aes(x=factor(REGIAO, levels = ordem) , y=frequencia_relativa)) + 
  geom_bar(stat="identity",position='dodge', fill = "#696969") + 
  scale_y_continuous(limits = c(0,0.5), expand = c(0,0), breaks = seq(0,0.5, by= 0.1),labels = paste0(seq(0,50,10),'%')) +
  labs(x="Região", y="Porcentagem") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")



####### UF ---------------------------------------------------------------------------------------------------



bd_uf %>% distinct(UF)


bd_uf <- bd %>% group_by(UF) %>% 
  summarise(freq_absoluta1 = n()) 

bd_uf <- bd_uf %>% na.omit() 
bd_uf <-  bd_uf %>% 
  mutate(frequencia_relativa1 = round((freq_absoluta1/sum(freq_absoluta1)),4)) %>% 
  top_n(frequencia_relativa1, n = 9)


ordem1 <- c("SP", "MG", "CE", "BA", "RJ", "PE",
            "PR", "RS", "GO")


ggplot(bd_uf, aes(x=factor(UF, levels = ordem1) , y=frequencia_relativa1)) + 
  geom_bar(stat="identity",position='dodge', fill = "#696969") + 
  scale_y_continuous(limits = c(0,0.3), expand = c(0,0), breaks = seq(0,0.3, by= 0.1),labels = paste0(seq(0,30,10),'%')) +
  labs(x="UF", y="Porcentagem") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")










##--------------------------------------classes das notas de português-----------------------------------

bd_lp <- bd











max(bd$NOTA_LP)
min(bd$NOTA_LP)





bd_lp$NOTA_LP <- cut(bd$NOTA_LP,
                     breaks = seq(133, 375,22),
                     labels = c('133 a 155',
                                '155 a 177',
                                '177 a 199',
                                '199 a 221',
                                '221 a 243',
                                '243 a 265',
                                '265 a 287',
                                '287 a 309',
                                '309 a 331',
                                '331 a 353',
                                '353 a 375'))




bd_lp <- bd_lp %>% group_by(NOTA_LP) %>%
  summarise(freq = n()) %>%
  arrange(desc(NOTA_LP)) %>%
  mutate(freq_relativa = round((freq/sum(freq)*100),2))




ggplot(bd, aes(x=NOTA_LP)) + 
  geom_histogram(colour="white", fill="#696969",breaks = seq(133, 375, by = 22))+
  labs(x="Notas", y="Número de crianças") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

ggplot(bd, aes(x="", y=NOTA_LP)) +
  geom_boxplot(fill=c("#696969"), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Português", y="Notas") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))


stem(bd$NOTA_LP)
mean(bd$NOTA_LP)
median(bd$NOTA_LP)
var(bd$NOTA_LP)
round(kurtosis(bd$NOTA_LP), 2)
round(skewness(bd$NOTA_LP), 3)








##-----------------------------------classes para notas de matemática---------------------------------------------------


bd_mt <- bd



max(bd$NOTA_MT)
min(bd$NOTA_MT)
bd_mt$NOTA_MT <- cut(bd_mt$NOTA_MT,
                     breaks = seq(130, 430,25),
                     labels = c('130 a 155',
                                '155 a 180',
                                '180 a 205',
                                '205 a 230',
                                '230 a 255',
                                '255 a 280',
                                '280 a 305',
                                '305 a 330',
                                '330 a 355',
                                '355 a 380',
                                '380 a 405',
                                '405 a 430'))




bd_mt <- bd_mt %>% group_by(NOTA_MT) %>%
  summarise(Frequencia_Absoluta = n()) %>%
  arrange(desc(NOTA_MT)) %>%
  mutate(Frequencia_Relativa= round((Frequencia_Absoluta/sum(Frequencia_Absoluta)*100),2))





ggplot(bd, aes(x=NOTA_MT)) + 
  geom_histogram(colour="white", fill="#696969",breaks = seq(133, 375, by = 22))+
  labs(x="Notas", y="Número de crianças") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

ggplot(bd, aes(x="", y=NOTA_MT)) +
  geom_boxplot(fill=c("#696969"), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Matemática", y="Notas") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))




stem(bd$NOTA_MT)
mean(bd$NOTA_MT)
median(bd$NOTA_MT)
var(bd$NOTA_MT)
round(kurtosis(bd$NOTA_MT), 3)
round(skewness(bd$NOTA_MT), 3)
