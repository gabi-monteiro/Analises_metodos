
#BAIXANDO E INTASLANDO OS PACOTES


pacman::p_load(purrr, plyr, tidyverse, ggcorrplot, scales, stringr, cowplot, RColorBrewer, xtable, readxl,lubridate, janitor, e1071, moments)

pacman::p_load(devtools, statsr,goftest, nortest)


#LENDO O BANCO DE DADOS

banco_dados <- read.csv("C:\\Users\\JOÉLIO\\Documents\\Gabi\\UnB\\MET_2\\Trabs_MET2\\amostra_190028262.csv", encoding = "UTF-8")


# AMOSTRAS

amostras_30 <- read.csv("amostras_30.csv")
amostras_100 <- read.csv("amostras_100.csv")



amostras_30 <- amostras_30[amostras_30$replicate == 1, ]





# NORMALIDADE NOTAS DE PORTUGUÊS  n = 100

bd_100_lp <- amostras_100


max(bd_100_lp$NOTA_LP)
min(bd_100_lp$NOTA_LP)





bd_100_lp$NOTA_LP <- cut(bd_100_lp$NOTA_LP,
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


bd_100_lp1 <- bd_100_lp %>% group_by(NOTA_LP) %>%
  summarise(freq = n()) 



amostras_100 %>% ggplot(aes(x=NOTA_LP)) + 
  geom_histogram(breaks = seq(133,375,22), 
                 aes(col=I("white"), y = ..density..), 
                 fill = "#7B68EE") + 
  scale_x_continuous(breaks = seq(133,375,22)) +
  labs(x='Notas de Português', 
       y="Densidade") + 
  theme_classic() + 
  theme(text = element_text(family = 'serif', size = 12))+ 
  stat_function(fun=dnorm,
                color="#464e51",
                size=0.4,
                args=list(mean=mean(banco_dados$NOTA_LP), 
                          sd=sd(banco_dados$NOTA_LP)))


fe_lp <- c(0.059463471,
           0.061791174,
           0.096976177,
           0.130915411,
           0.152022335,
           0.151850717,
           0.130472536,
           0.096430014,
           0.061304504,
           0.033523848,
           0.025249814)

chisq.test(bd_100_lp1$freq, p =fe_lp)

shapiro.test(amostras_100$NOTA_LP)
lillie.test(amostras_100$NOTA_LP)
ad.test(amostras_100$NOTA_LP)


#NOTAS DE PORTUGUÊS N = 30


bd_30_lp <- amostras_30


max(bd_30_lp$NOTA_LP)
min(bd_30_lp$NOTA_LP)


bd_30_lp$NOTA_LP <- cut(bd_30_lp$NOTA_LP,
                         breaks = seq(137, 342,41),
                         labels = c('137 a 178',
                                    '178 a 219',
                                    '219 a 260',
                                    '260 a 301',
                                    '301 a 342'))


bd_30_lp1 <- bd_30_lp %>% group_by(NOTA_LP) %>%
  summarise(freq = n()) 



amostras_30 %>% ggplot(aes(x=NOTA_LP)) + 
  geom_histogram(breaks = seq(137,342,41), 
                 aes(col=I("white"), y = ..density..), 
                 fill = "#7B68EE") + 
  scale_x_continuous(breaks = seq(137,342,41)) +
  labs(x='Notas de Português', 
       y="Densidade") + 
  theme_classic() + 
  theme(text = element_text(family = 'serif', size = 12))+ 
  stat_function(fun=dnorm,
                color="#464e51",
                size=0.4,
                args=list(mean=mean(banco_dados$NOTA_LP), 
                          sd=sd(banco_dados$NOTA_LP)))



shapiro.test(amostras_30$NOTA_LP)
lillie.test(amostras_30$NOTA_LP)
ad.test(amostras_30$NOTA_LP)



# NOTAS DE MATEMÁTICA n = 100

bd_100_mt <- amostras_100

max(bd_100_mt$NOTA_MT)
min(bd_100_mt$NOTA_MT)


mean(amostras_100$NOTA_MT)
sd(amostras_100$NOTA_MT)





bd_100_mt$NOTA_MT <- cut(bd_100_mt$NOTA_MT,
                         breaks = seq(149.7, 369.7, 20),
                         labels = c('149.7 a 169.7',
                                    '169.7 a 189.7',
                                    '189.7 a 209.7',
                                    '209.7 a 229.7',
                                    '229.7 a 249.7',
                                    '249.7 a 269.7',
                                    '269.7 a 289.7',
                                    '289.7 a 309.7',
                                    '309.7 a 329.7',
                                    '329.7 a 349.7',
                                    '349.7 a 369.7'))


bd_100_mt1 <- bd_100_mt %>% group_by(NOTA_MT) %>%
  summarise(freq = n()) 



amostras_100 %>% ggplot(aes(x=NOTA_MT)) + 
  geom_histogram(breaks = seq(149.7, 369.7, 20), 
                 aes(col=I("white"), y = ..density..), 
                 fill = "#7B68EE") + 
  scale_x_continuous(breaks = seq(149.7, 369.7, 20)) +
  labs(x='Notas de Matemática', 
       y="Densidade") + 
  theme_classic() + 
  theme(text = element_text(family = 'serif', size = 12))+ 
  stat_function(fun=dnorm,
                color="#464e51",
                size=0.4,
                args=list(mean=mean(banco_dados$NOTA_LP), 
                          sd=sd(banco_dados$NOTA_LP)))


fe_mt <- c(0.097086764,
        0.075681917,
        0.105420256,
        0.129622083,
        0.140688434,
        0.13479161,
        0.113996647,
        0.085102994,
        0.056081426,
        0.032622086,
        0.028905781)

chisq.test(bd_100_mt1$freq, p=fe_mt)

shapiro.test(amostras_100$NOTA_MT)
lillie.test(amostras_100$NOTA_MT)
ad.test(amostras_100$NOTA_MT)




#NOTAS DE MATEMÁTICA N = 30

bd_30_mt <- amostras_30


max(bd_30_mt$NOTA_MT)
min(bd_30_mt$NOTA_MT)


bd_30_mt$NOTA_MT <- cut(bd_30_mt$NOTA_MT,
                        breaks = seq(143, 333,38),
                        labels = c('143 a 181',
                                   '181 a 219',
                                   '219 a 257',
                                   '257 a 295',
                                   '295 a 333'))


bd_30_mt1 <- bd_30_mt %>% group_by(NOTA_MT) %>%
  summarise(freq = n()) 



amostras_30 %>% ggplot(aes(x=NOTA_MT)) + 
  geom_histogram(breaks = seq(143, 333,38), 
                 aes(col=I("white"), y = ..density..), 
                 fill = "#7B68EE") + 
  scale_x_continuous(breaks = seq(143, 333,38)) +
  labs(x='Notas de Matemática', 
       y="Densidade") + 
  theme_classic() + 
  theme(text = element_text(family = 'serif', size = 12))+ 
  stat_function(fun=dnorm,
                color="#464e51",
                size=0.4,
                args=list(mean=mean(banco_dados$NOTA_MT), 
                          sd=sd(banco_dados$NOTA_MT)))



shapiro.test(amostras_30$NOTA_MT)
lillie.test(amostras_30$NOTA_MT)
ad.test(amostras_30$NOTA_MT)

