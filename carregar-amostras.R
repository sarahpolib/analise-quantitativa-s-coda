#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%% Análise /s/ em coda - Qualificação %%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 15/07/2025 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% V1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
                # Carregar amostras e manipulação dos dados # 

# Carregar pacotes ###
#install.packages("ggplot2"); install.packages("tidyverse"); install.packages("lme4"); install.packages("lmerTest"); install.packages("effects"); install.packages("openxlsx"); install.packages("rms"); install.packages("statmod"); install.packages("RColorBrewer"); install.packages("stargazer");install.packages("hrbrthemes"); install.packages("scales"); install.packages("performance");install.packages("patchwork"); install.packages("factoextra"); install.packages("glmnet")

library(ggplot2); library(tidyverse); library(lme4); library(lmerTest); library(effects); library(openxlsx); library(rms); library(statmod); library(RColorBrewer); library(stargazer); library(hrbrthemes); library(scales); library(performance); library(patchwork); library(factoextra); library(glmnet); library(MuMIn)

rm(list = ls())

# Carregar dados ####
setwd("C:/Users/sah/Downloads/analise-quantitativa-s-coda")

## AMOSTRA2 ####
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


## POLI ####
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



## Amostra2Poli ####
dadosAmostra2Poli <- bind_rows(amostra2, poli)
str(dadosAmostra2Poli)
#View(dadosAmostra2Poli)
unique(dadosAmostra2Poli$PARTICIPANTE)
 
## INFS ####
infs <- read_csv("infs2025-amostra2ePoli-V2.csv", locale = locale(encoding = "UTF-8"),
                 col_types = cols(.default = col_character(),
                                  IDADE = col_integer(),
                                  #INDICE_ESCOL_OUSHIRO = col_double(),
                                  INDICE_ESCOL3 = col_integer(),
                                  INDICE_OCUPACAO = col_integer(),
                                  INDICE_OUTRO_CARGO2 = col_integer(),
                                  INDICE_OCUPACAO_SONHOS2 = col_integer(),
                                  INDICE_LOCOMOCAO = col_integer(), 
                                  INDICE_MEGA = col_integer(),
                                  INDICE_LAZER= col_integer(),
                                  INDICE_LAZER_CAMPINAS = col_integer(),
                                  INDICE_VIAGEM = col_integer(),
                                  INDICE_VIAGEM_LUGAR = col_integer(),
                                  INDICE_VIAGEM_VONTADE = col_integer(),
                                  INDICE_BAIRRO = col_integer(),
                                  INDICE_INFANCIA = col_integer(),
                                  INDICE_SOCIO_OUSHIRO = col_double(),
                                  IDADE_MIGRACAO = col_integer(),
                                  TEMPO_RESIDENCIA = col_integer(),
                                  DENSIDADE_HABITACAO =  col_double(),
                                  NRENDA = col_integer(),
                                  INDICE_RENDA_IND = col_integer(),
                                  INDICE_RENDA_FAM = col_integer(),
                                  INDICE_OCUPACAO_PAI = col_integer(),
                                  INDICE_OCUPACAO_MAE = col_integer(),
                                  INDICE_ESCOL_PAI = col_integer(),
                                  INDICE_ESCOL_MAE = col_integer(),
                                  INDICE_IMOVEL = col_integer(),
                                  # AMIGO1_CLASSIFICACAO = col_integer(),
                                  # AMIGO2_CLASSIFICACAO = col_integer(),
                                  # AMIGO3_CLASSIFICACAO = col_integer(),
                                  # AMIGO4_CLASSIFICACAO = col_integer(),
                                  # AMIGO5_CLASSIFICACAO = col_integer(),
                                  ESTADUALIDADE = col_integer(),
                                  PAULISTIDADE = col_integer(),
                                  NORDESTINIDADE = col_integer(),
                                  INDICE_REDE_EST = col_integer(),
                                  INDICE_REDE_NE = col_integer(),
                                  INDICE_HABITOS = col_integer()
                                  )) %>%
  mutate(across(where(is.character), as.factor))
str(infs)

### Mudar nível de variaveis ####
# Renda individual #
infs$RENDA_IND <- factor(infs$RENDA_IND, levels = c("1SM", "1a2SM", "2a4SM", "4a9SM", "10a19SM"))
levels(infs$RENDA_IND)

#juntar 2 ultimas faizas salariais
infs$RENDA_IND <- fct_collapse(infs$RENDA_IND, "4a9/10a19SM" = c("4a9SM", "10a19SM"))
levels(infs$RENDA_IND)

infs$RENDA_FAM <- factor(infs$RENDA_FAM, levels = c("1SM", "1a2SM", "2a4SM", "4a9SM", "10a19SM", "20+SM"))
levels(infs$RENDA_FAM)

#tipo de viagem, juntou-se os níveis considerando viagem pra conhecer novos lugares vs viagem pra locais mais pertos ou pro estado pra visitar a familia
infs$VIAGEM_LUGAR <- fct_collapse(as.factor(infs$VIAGEM_LUGAR), "nacional.internacional" = c("nacional", "nacional-internacional"), "SP-estado" = c("estado", "SP", "SP-estado"))
levels(infs$VIAGEM_LUGAR)

#juntando individuos que citaram viagem internacional
infs$LAZER_VIAGEM_VONTADE2 <- fct_collapse(as.factor(infs$LAZER_VIAGEM_VONTADE2), "nacional.internacional" = c("internacional", "nacional-internacional"))
levels(infs$LAZER_VIAGEM_VONTADE2)

#ascesão de cargo
infs$INDICE_OUTRO_CARGO <- fct_collapse(as.factor(infs$INDICE_OUTRO_CARGO), "sem.perspec" = "0", "com.perspec" = c("1", "2", "3", "5","6"))
levels(infs$INDICE_OUTRO_CARGO)


#ocupação dos sonhos
infs$INDICE_OCUPACAO_SONHOS <- fct_collapse(as.factor(infs$INDICE_OCUPACAO_SONHOS), "nenhuma" = "0", "prof.intermediaria" = c("1", "2", "3", "4"), "prof.especializacao" = "5")
levels(infs$INDICE_OCUPACAO_SONHOS)

#Lazer 
infs$LAZER_CARACTERISTICA <- fct_collapse(as.factor(infs$LAZER_CARACTERISTICA), "custo" = c("custo", "ambos"))
levels(infs$LAZER_CARACTERISTICA)


#Lazer em campinas
infs$LAZER_CAMPINAS_CARACTERISTICA <- fct_collapse(as.factor(infs$LAZER_CAMPINAS_CARACTERISTICA), "custo" = c("custo", "ambos"))
levels(infs$LAZER_CAMPINAS_CARACTERISTICA)



### Normalização de índices ####
escalas <- infs %>% 
  select(INDICE_ESCOL3, INDICE_OCUPACAO, INDICE_OUTRO_CARGO2, INDICE_OCUPACAO_SONHOS2, INDICE_LOCOMOCAO, INDICE_MEGA, INDICE_LAZER, INDICE_LAZER_CAMPINAS, INDICE_VIAGEM, INDICE_VIAGEM_LUGAR, INDICE_VIAGEM_VONTADE, INDICE_BAIRRO, INDICE_INFANCIA, INDICE_RENDA_IND, INDICE_RENDA_FAM, INDICE_ESCOL_PAI, INDICE_OCUPACAO_PAI, INDICE_ESCOL_MAE, INDICE_OCUPACAO_MAE, DENSIDADE_HABITACAO, INDICE_IMOVEL) %>% 
  mutate(across(everything(), as.numeric)) %>%
  print()

normalizar <- function(x, n = max(x, na.rm = TRUE)) {
  (x * 5) / n}


infs <- infs %>%
  mutate(across(all_of(names(escalas)), 
              ~ normalizar(.), 
              .names = "{.col}_norm")) %>% 
  print()

infs$INDICE_IMOVEL_norm

head(infs)
str(infs)


### m2 ####
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


### juntar infs e m2 ####
setdiff(levels(infs$BAIRRO), levels(media_m2_bairro$BAIRRO))
infs2 <- left_join(infs, media_m2_bairro, by = "BAIRRO")
infs2$media_m2

### CALCULO INDICE SOCIOECONÔMICO ####
#escolaridade
#Media escolaridade e ocupação dos pais
#Ocupação sonhos
#Mega sena
#Renda ind
#média lazer e Lazer em Campinas
#média Viagem, Viagem Lugar e Viagem vontade
#Infância


# Função para calcular o índice ajustado
calcular_indice <- function(df) {
  apply(df, 1, function(x) {
    
    esc <- as.numeric(x["INDICE_ESCOL3_norm"])
    
    pais <- mean(as.numeric(c(x["INDICE_ESCOL_PAI_norm"],
                              x["INDICE_ESCOL_MAE_norm"],
                              x["INDICE_OCUPACAO_PAI_norm"],
                              x["INDICE_OCUPACAO_MAE_norm"])), na.rm = TRUE)
    
    lazer <- mean(as.numeric(c(x["INDICE_LAZER_norm"],
                               x["INDICE_LAZER_CAMPINAS_norm"])), na.rm = TRUE)
    
    viagem <- mean(as.numeric(c(x["INDICE_VIAGEM_norm"],
                                x["INDICE_VIAGEM_LUGAR_norm"],
                                x["INDICE_VIAGEM_VONTADE_norm"])), na.rm = TRUE)
    
    ocup_sonhos <- as.numeric(x["INDICE_OCUPACAO_SONHOS2_norm"])
    mega        <- as.numeric(x["INDICE_MEGA_norm"])
    renda       <- as.numeric(x["INDICE_RENDA_IND_norm"])
    infancia    <- as.numeric(x["INDICE_INFANCIA_norm"])
    
    componentes <- c(esc, pais, ocup_sonhos, mega, renda, infancia, lazer, viagem)
    
    mean(componentes, na.rm = TRUE)
  })
}

#calculo
infs2$INDICE_SOCIO_POLI <- calcular_indice(infs2)
infs2$INDICE_SOCIO_POLI

## Amostra2Poli + INFS ####
dados <- left_join(dadosAmostra2Poli, infs2, by = "PARTICIPANTE")
#View(dados)
str(dados)

setdiff(levels(dadosAmostra2Poli$PARTICIPANTE), levels(infs$PARTICIPANTE))


### Filtragens e manipulações de variáveis linguísticas ####
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

### Classes para LASSO ####

dados2$INDICE_OCUPACAO_MAE_norm <- as.factor(dados2$INDICE_OCUPACAO_MAE_norm)
class(dados2$INDICE_OCUPACAO_MAE_norm)


dados2$INDICE_OCUPACAO_PAI_norm <- as.factor(dados2$INDICE_OCUPACAO_PAI_norm)
class(dados2$INDICE_OCUPACAO_PAI_norm)

#densidade
infs$DENSIDADE_HABITACAO <- fct_collapse(as.factor(infs$DENSIDADE_HABITACAO), "-uma" = c("0.25", "0.5", "0.7", "0.75"), "uma" = "1", "+uma" = c("1.25", "1.3",  "1.5", "1.75", "2", "2.3", "2.5"))
levels(infs$DENSIDADE_HABITACAO)

### PALATALIZAÇÃO ####
dados_AP <- dados2 %>% 
  filter(VD %in% c("P", "A"),
         CFS_pontoc2 == "coronal",
         #PARTICIPANTE != "IsabelaS"
         ) %>%
  droplevels()

levels(dados_AP$VD)
levels(dados_AP$CFS_pontoc2)




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
  filter(VD %in% c("A", "H", "P"),
         CFS_sonoridade == "sonora") %>%
  mutate(VD = fct_collapse(VD,
                           H = "H",
                           AP = c("A", "P"))) %>%
  droplevels()

levels(dados_HAP$VD) 
levels(dados_HAP$CFS_sonoridade) 

# EXPORTAR dados2 ####

write.csv(dados2, "dados2.csv", row.names = TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Distribuição Geral ####
distribuicao.geral <- dados2 %>% 
  count(VD) %>%
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>%
  print()


png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/VD.png", width = 6.5, height = 4.5, units = "in", res = 300)
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


## Por amostra ####

distribuicao.amostra <- dados2 %>% 
  count(AMOSTRA, VD) %>%
  group_by(AMOSTRA) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>%
  print()


png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/VD-amostra.png", width = 6.5, height = 4.5, units = "in", res = 300)
distribuicao.amostra %>%   
  ggplot(aes(x = VD, y = prop, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Proporção de Ocorrência", fill = "VD") + 
  scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(aes(label = label), 
            vjust = -0.2,
            size = 3.5) +
  facet_wrap(. ~ AMOSTRA)+
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



## Por participante ####
distribuicao.geral.participante <- dados2 %>% 
  count(PARTICIPANTE, VD) %>%
  group_by(PARTICIPANTE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>%
  print()


png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/VD-participante.png", width = 6.5, height = 4.5, units = "in", res = 300)
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



