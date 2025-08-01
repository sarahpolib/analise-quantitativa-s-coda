#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%% Análise /s/ em coda - Qualificação %%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 15/07/2025 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% V1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
                # Carregar amostras e manipulação dos dados # 

# Carregar pacotes ###
#install.packages("ggplot2"); install.packages("tidyverse"); install.packages("lme4"); install.packages("lmerTest"); install.packages("effects"); install.packages("openxlsx"); install.packages("rms"); install.packages("statmod"); install.packages("RColorBrewer"); install.packages("stargazer");install.packages("hrbrthemes"); install.packages("scales"); install.packages("performance");install.packages("patchwork")

library(ggplot2); library(tidyverse); library(lme4); library(lmerTest); library(effects); library(openxlsx); library(rms); library(statmod); library(RColorBrewer); library(stargazer); library(hrbrthemes); library(scales); library(performance); library(patchwork)

rm(list = ls())

# Carregar dados ####
setwd("C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa")

## amostra2 ####
amostra2 <- read_csv("dadosS-amostra2-planilha.csv", locale = locale(encoding = "latin1"),
                     col_types = cols(.default = col_character(),
                                      CONT_PREC = col_character(),
                                      OCORRENCIA = col_character(),
                                      CONT_SEGUINTE =  col_character(),
                                      ANO.ENTREVISTA = col_integer(),
                                      LOCALIZACAO = col_character())) %>%
  mutate(across(where(is.character), as.factor))


head(amostra2)
str(amostra2)
unique(amostra2$PARTICIPANTE)


## poli ####
poli <- read_csv("dadosS-poli-planilha.csv", locale = locale(encoding = "latin1"),
                    col_types = cols(.default = col_character(),
                                     CONT_PREC = col_character(),
                                     OCORRENCIA = col_character(),
                                     CONT_SEGUINTE =  col_character(),
                                     ANO.ENTREVISTA = col_integer(),
                                     LOCALIZACAO = col_character()))%>%
  mutate(across(where(is.character), as.factor))
head(poli)
str(poli)

levels(poli$VD)



## Juntar Barbosa e Poli ####
dadosAmostra2Poli <- bind_rows(amostra2, poli)
str(dadosAmostra2Poli)
#View(dadosAmostra2Poli)
unique(dadosAmostra2Poli$PARTICIPANTE)
 
## infs ####
infs <- read_csv("infs2025-amostra2ePoli-V2.csv", locale = locale(encoding = "UTF-8"),
                 col_types = cols(.default = col_character(),
                                  IDADE = col_integer(),
                                  INDICE_SOCIO = col_double(),
                                  INDICE_ESCOL = col_double(),
                                  INDICE_OCUPACAO = col_integer(),
                                  INDICE_OUTRO_CARGO = col_integer(),
                                  INDICE_OCUPACAO_SONHOS = col_integer(),
                                  IDADE_MIGRACAO = col_integer(),
                                  TEMPO_RESIDENCIA = col_integer(),
                                  #NPESSOAS = col_integer(),
                                  NRENDA = col_integer(),
                                  INDICE_OCUPACAO_PAI = col_integer(),
                                  INDICE_OCUPACAO_MAE = col_integer(),
                                  INDICE_ESCOL_PAI = col_double(),
                                  INDICE_ESCOL_MAE = col_double(),
                                  AMIGO1_CLASSIFICACAO = col_integer(),
                                  AMIGO2_CLASSIFICACAO = col_integer(),
                                  AMIGO3_CLASSIFICACAO = col_integer(),
                                  AMIGO4_CLASSIFICACAO = col_integer(),
                                  AMIGO5_CLASSIFICACAO = col_integer(),
                                  ESTADUALIDADE = col_integer(),
                                  PAULISTIDADE = col_integer(),
                                  NORDESTINIDADE = col_integer(),
                                  INDICE_REDE_EST = col_integer(),
                                  INDICE_REDE_NE = col_integer(),
                                  INDICE_HABITOS = col_integer()
                                  )) %>%
  mutate(across(where(is.character), as.factor))

#mudar nível de variaveis
infs$RENDA_IND <- factor(infs$RENDA_IND, levels = c("1SM", "1a2SM", "2a4SM", "4a9SM", "10a19SM"))
levels(infs$RENDA_IND)

#juntar 2 ultimas faizas salariais
infs$RENDA_IND <- fct_collapse(infs$RENDA_IND, "4a9/10a19SM" = c("4a9SM", "10a19SM"))
levels(infs$RENDA_IND)

#simplificar ambição para não tem =  0 e tem = 1
infs$INDICE_OUTRO_CARGO <- fct_collapse(as.factor(infs$INDICE_OUTRO_CARGO), "1" = c("2", "3", "4", "5", "6", "7"))
levels(infs$INDICE_OUTRO_CARGO)

#simplificar ambição para não tem =  0 e tem mas está entre os índices de 1 a 5 = 2, tem e envolve grauação = 3
infs$INDICE_OCUPACAO_SONHOS <- fct_collapse(as.factor(infs$INDICE_OCUPACAO_SONHOS), "1" = c("1","2", "4", "5"), "2" = "6")
levels(infs$INDICE_OCUPACAO_SONHOS)

infs$RENDA_FAM <- factor(infs$RENDA_FAM, levels = c("1SM", "1a2SM", "2a4SM", "4a9SM", "10a19SM", "20+SM"))
levels(infs$RENDA_FAM)


infs$LAZER_CAMPINAS_CARACTERISTICA <- fct_collapse(as.factor(infs$LAZER_CAMPINAS_CARACTERISTICA), "nsai.ntem" = c("nao", "nao.sai"))
levels(infs$LAZER_CAMPINAS_CARACTERISTICA)

#juntando individuos que citaram viagem internacional
infs$LAZER_VIAGEM_VONTADE2 <- fct_collapse(as.factor(infs$LAZER_VIAGEM_VONTADE2), "nacional.internacional" = c("internacional", "nacional-internacional"))
levels(infs$LAZER_VIAGEM_VONTADE2)


head(infs)
str(infs)

## m2 ####

m2 <- read_csv("m2-bairros.csv", locale = locale(encoding = "UTF-8"), 
               col_types = cols(.default = col_character(),
                                PRECO = col_double(),
                                M2 = col_number(),
                                PRECOMETRO2 = col_number())) %>%
  mutate(across(where(is.character), as.factor))
str(m2)
head(m2)

#calcular média do preço do m2 por bairro
m2$PRECOMETRO2
media_m2_bairro <- m2 %>% 
  group_by(BAIRRO) %>% 
  summarise(media_m2 = round(mean(PRECOMETRO2, na.rm = TRUE), 2)) %>% 
  arrange(media_m2) %>%
  print(n = 22)


## juntar infs e m2 ####
setdiff(levels(infs$BAIRRO), levels(media_m2_bairro$BAIRRO))
infs2 <- left_join(infs, media_m2_bairro, by = "BAIRRO")
infs2$media_m2


## juntar dadosAmostra2Poli com dados ifds ####
dados <- left_join(dadosAmostra2Poli, infs2, by = "PARTICIPANTE")
#View(dados)
str(dados)

setdiff(levels(dadosAmostra2Poli$PARTICIPANTE), levels(infs$PARTICIPANTE))


## Filtragens e manipulações de variáveis ####
dados1 <- dados %>% 
  filter(VD %in% c("A", "P", "0", "H")) %>% 
  droplevels()

dados1$VD <- factor(dados1$VD, levels = c("A", "P", "0", "H"))
levels(dados1$VD)

dados2 <- dados1 %>% 
mutate(CFP_abertura2 = case_when(
  CONT_FON_PREC %in% c("i", "I", "j", "J", "u", "U", "w", "W") ~ "fechada",
  CONT_FON_PREC %in% c("e", "o", "O") ~ "meio.fechada",
  CONT_FON_PREC %in% c("3", "c") ~ "meio.aberta",
  CONT_FON_PREC %in% c("a", "A") ~ "aberta"),
  CFP_abertura2 = factor(CFP_abertura2, levels = c("fechada", "meio.fechada", "meio.aberta", "aberta")),
  CFS_pontoc2 = case_when(
    CONT_FON_SEG == "#" ~ "pausa",
    CONT_FON_SEG %in% c("t", "d", "l", "n") ~ "coronal",
    CONT_FON_SEG %in% c("h", "k", "g") ~ "dorsal",
    CONT_FON_SEG %in% c("b", "f", "m", "p", "v") ~ "labial"),
  CFS_pontoc2 = factor(CFS_pontoc2, levels = c("pausa", "coronal", "dorsal", "labial")),
  CFS_sonoridade = case_when(
    CONT_FON_SEG == "#" ~ "pausa",
    CONT_FON_SEG %in% c("b", "d","g", "l", "m", "n", "v") ~ "sonora",
    CONT_FON_SEG %in% c("f", "h", "k", "p", "t") ~ "surda"),
    CFS_sonoridade = factor(CFS_sonoridade, levels = c("pausa", "sonora", "surda"))
  )
levels(dados2$ESCOLARIDADE)


### PALATALIZAÇÃO ####
dados_AP <- dados2 %>% 
  filter(VD %in% c("P", "A")) %>%
  droplevels()

levels(dados_AP$VD)




### APAGAMENTO ####
dados_S0 <- dados2 %>%
  filter(!MORFEMA.PLURAL == "sim") %>% 
  mutate(VD = fct_collapse(VD,
                           S = c("A", "P", "H"), 
                           `0` = "0")) %>% 
         droplevels()
levels(dados_S0$MORFEMA.PLURAL)


### ASPIRAÇÃO ####
dados_HAP <- dados2 %>% 
  filter(VD %in% c("A", "H", "P")) %>% 
  mutate(VD = fct_collapse(VD,
                           H = "H",
                           AP = c("A", "P"))) %>%
  droplevels()

levels(dados_HAP$VD) 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# DISTRIBUIÇÃO GERAL ####
distribuicao.geral <- dados2 %>% 
  count(VD) %>%
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>%
  print()


png("C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa/graficos/VD.png", width = 6.5, height = 4.5, units = "in", res = 300)
distribuicao.geral %>%   
ggplot(aes(x = VD, y = prop, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Proporção de Ocorrência", fill = "VD") + 
  scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(aes(label = label), 
            vjust = -0.2,
            size = 3.5) +
  scale_fill_brewer(palette = "Reds")+
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     expand = expansion(mult = c(0, 0.15))) +  # aumenta espaço no topo para texto
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9),   # tamanho do título eixo Y
    legend.position = "none")
dev.off()


# DISTRIBUIÇÃO GERAL POR PARTICIPANTE####
distribuicao.geral.participante <- dados2 %>% 
  count(PARTICIPANTE, VD) %>%
  group_by(PARTICIPANTE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>%
  print()


png("C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa/graficos/VD-participante.png", width = 6.5, height = 4.5, units = "in", res = 300)
distribuicao.geral.participante %>%   
  ggplot(aes(x = VD, y = prop, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Proporção de Ocorrência", fill = "VD") + 
  scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(aes(label = label), 
            vjust = -0.2,
            size = 3.5) +
  facet_wrap(. ~ PARTICIPANTE)+
  scale_fill_brewer(palette = "Reds")+
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     expand = expansion(mult = c(0, 0.15))) + #espaço no topo para texto
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9),   # tamanho do título eixo Y
    legend.position = "bottom")
dev.off()

