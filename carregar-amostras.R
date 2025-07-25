#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%% Análise /s/ em coda - Qualificação %%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 15/07/2025 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% V1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
                # Carregar amostras e manipulação dos dados # 

# Carregar pacotes ###
#install.packages("ggplot2"); install.packages("tidyverse"); install.packages("lme4"); install.packages("lmerTest"); install.packages("effects"); install.packages("openxlsx"); install.packages("rms"); install.packages("statmod"); install.packages("RColorBrewer"); install.packages("stargazer");install.packages("hrbrthemes"); install.packages("scales"); install.packages("performance")

library(ggplot2); library(tidyverse); library(lme4); library(lmerTest); library(effects); library(openxlsx); library(rms); library(statmod); library(RColorBrewer); library(stargazer); library(hrbrthemes); library(scales); library(performance)

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

infs$RENDA_FAM <- factor(infs$RENDA_FAM, levels = c("1SM", "1a2SM", "2a4SM", "4a9SM", "10a19SM", "20+SM"))
levels(infs$RENDA_FAM)

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
                           AP = c("A", "P"), 
                           H = "H")) %>% 
  droplevels()

levels(dados_HAP$VD) 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# DISTRIBUIÇÃO GERAL ####
distribuicao.geral <- dados2 %>% 
  count(VD) %>%
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>%
  print()


png("VD.png", width = 6.5, height = 4.5, units = "in", res = 300)
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

