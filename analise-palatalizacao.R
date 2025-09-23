#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%# PROCESSO DE PALATALIZAÇÃO #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 15/07/2025 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% V1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# CONT.FON.SEG ####
# filtragem foi feita no arquivo carregar-amostras. Dados mostram envelope de variação semelhante ao de Barbosa 2023, ou seja, ocorrência de palatalização se dá só quando seguida por consoante coronal

# AP.prop_CONT_FON_SEG <- dados_AP %>% 
#   count(VD, CONT_FON_SEG) %>%
#   group_by(CONT_FON_SEG) %>% 
#   mutate(prop = prop.table(n),
#          label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
#   print()
# 
# ggplot(AP.prop_CONT_FON_SEG, aes(x = CONT_FON_SEG, y = prop * 100, fill = VD, label = label)) + 
#   geom_bar(stat = "identity", color = "white") + 
#   labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
#   #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
#   geom_text(size = 3, position = position_stack(vjust = 0.5)) +
#   scale_fill_brewer(palette = "Reds")+
#   theme_minimal()+
#   theme(
#     panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
#     panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
#     axis.title.x = element_text(size = 9),  # tamanho do título eixo X
#     axis.title.y = element_text(size = 9))
# 
# #CFS_coronal de acordo com BARBOSA (2023)
# AP.prop_CFS_pontoc2<- dados_AP %>% 
#   count(VD, CFS_pontoc2) %>%
#   group_by(CFS_pontoc2) %>% 
#   mutate(prop = prop.table(n),
#          label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
#   print()
# 
# ggplot(AP.prop_CFS_pontoc2, aes(x = CFS_pontoc2, y = prop * 100, fill = VD, label = label)) + 
#   geom_bar(stat = "identity", color = "white") + 
#   labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
#   #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
#   geom_text(size = 3, position = position_stack(vjust = 0.5)) +
#   scale_fill_brewer(palette = "Reds")+
#   theme_minimal()+
#   theme(
#     panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
#     panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
#     axis.title.x = element_text(size = 9),  # tamanho do título eixo X
#     axis.title.y = element_text(size = 9))


# VD ####
AP.prop_VD <- dados_AP %>% 
  count(VD) %>%
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_VD, aes(x = VD, y = prop, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  scale_x_discrete(labels = c("Alveolar", "Palatal"))+
  geom_text(aes(label = label), vjust = -0.3, size = 3.5) + 
  scale_fill_brewer(palette = "Reds")+
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     expand = expansion(mult = c(0, 0.15))) + #aumenta espaço no topo
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9),   # tamanho do título eixo Y
    legend.position = "none")


# TONICIDADE ####
AP.prop_TONICIDADE <- dados_AP %>%
  count(VD, TONICIDADE) %>%
  group_by(TONICIDADE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_TONICIDADE, aes(x = TONICIDADE, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))

(AP.tab_TONICIDADE <- with(dados_AP, table(TONICIDADE, VD)))
chisq.test(AP.tab_TONICIDADE)


# POSICAO ####
AP.prop_POSICAO <- dados_AP %>%
  count(VD, POSICAO_S) %>%
  group_by(POSICAO_S) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_POSICAO, aes(x = POSICAO_S, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))

(AP.tab_POSICAO <- with(dados_AP, table(POSICAO_S, VD)))
chisq.test(AP.tab_POSICAO)


# CONT.FON.PREC ####

AP.prop_CONT_FON_PREC <- dados_AP %>% 
  count(VD, CONT_FON_PREC) %>%
  group_by(CONT_FON_PREC) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_CONT_FON_PREC, aes(x = CONT_FON_PREC, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


#CFS_abertura assim como Barbosa (2023)
AP.prop_CFP_abertura2 <- dados_AP %>%
  count(VD, CFP_abertura2) %>%
  group_by(CFP_abertura2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_CFP_abertura2, aes(x = CFP_abertura2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.tab_CFP_abertura2 <- with(dados_AP, table(CFP_abertura2, VD)))
chisq.test(AP.tab_CFP_abertura2[c(1,2),]) #sem diferença entre fechada e meio fechada


# CLASSE MORFOLOGICA ####
AP.prop_CLASSE_MORFOLOGICA3 <- dados_AP %>%

  count(VD, CLASSE_MORFOLOGICA3) %>%
  group_by(CLASSE_MORFOLOGICA3) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_CLASSE_MORFOLOGICA3, aes(x = CLASSE_MORFOLOGICA3, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.tab_CLASSE_MORFOLOGICA3 <- with(dados_AP, table(CLASSE_MORFOLOGICA3, VD)))
chisq.test(AP.tab_CLASSE_MORFOLOGICA3)

# ESTILO ####
AP.prop_ESTILO <- dados_AP %>%
  count(VD, ESTILO) %>%
  group_by(ESTILO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_ESTILO, aes(x = ESTILO, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.tab_ESTILO <- with(dados_AP, table(ESTILO, VD)))
chisq.test(AP.tab_ESTILO)


# GENERO ####
AP.prop_GENERO <- dados_AP %>%
  count(VD, GENERO) %>%
  group_by(GENERO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_GENERO, aes(x = GENERO, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.tab_GENERO <- with(dados_AP, table(GENERO, VD)))
chisq.test(AP.tab_GENERO)


# IDADE DE MIGRACAO ####
AP.prop_IDADE_MIGRACAO <- dados_AP %>% 
  count(VD, IDADE_MIGRACAO) %>%
  group_by(IDADE_MIGRACAO) %>% 
  mutate(prop = prop.table(n)) %>% 
  print(n = 52)

#png("C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa/graficos/C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa/graficos/VD_AP-10.idade_migracao.png")
ggplot(AP.prop_IDADE_MIGRACAO[27:52,], aes(x = IDADE_MIGRACAO, y = prop * 100, label = round(prop * 100, 1))) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  #labs(x = "Idade de Migração", y = "Proporção de Palatalização") +
  #geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  theme_light()
#dev.off()

AP.mod_IDADE_MIGRACAO <- glm(VD ~ IDADE_MIGRACAO, data = dados_AP, family = binomial)
summary(AP.mod_IDADE_MIGRACAO)
lrm(VD ~ IDADE_MIGRACAO, data = dados_AP)

plot(allEffects(AP.mod_IDADE_MIGRACAO), type = "response")


# TEMPO DE RESIDENCIA ####
AP.prop_TEMPO_RESIDENCIA <- dados_AP %>% 
  count(VD, TEMPO_RESIDENCIA) %>%
  group_by(TEMPO_RESIDENCIA) %>% 
  mutate(prop = prop.table(n)) %>% 
  print(n = 46)

png("C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa/graficos/VD_AP-tempo-residencia.png", width = 6.5, height = 4.5, units = "in", res = 300)
ggplot(AP.prop_TEMPO_RESIDENCIA[24:46,], aes(x = TEMPO_RESIDENCIA, y = prop * 100, label = round(prop * 100, 1))) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Tempo de Residência", y = "Proporção de Palatalização") +
  #geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  theme_light()
dev.off()

AP.mod_TEMPO_RESIDENCIA <- glm(VD ~ TEMPO_RESIDENCIA, data = dados_AP, family = binomial)
summary(AP.mod_TEMPO_RESIDENCIA)
lrm(VD ~ TEMPO_RESIDENCIA, data = dados_AP)
plot(allEffects(AP.mod_TEMPO_RESIDENCIA), type = "response")

# INDICE SOCIO OUSHIRO ####
AP.prop_INDICE_SOCIO_OUSHIRO <- dados_AP %>% 
  count(VD, INDICE_SOCIO_OUSHIRO) %>%
  group_by(INDICE_SOCIO_OUSHIRO) %>% 
  mutate(prop = prop.table(n)) %>% 
  print(n = 62)

#png("C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa/graficos/VD_AP-tempo-residencia.png", width = 6.5, height = 4.5, units = "in", res = 300)
ggplot(AP.prop_INDICE_SOCIO_OUSHIRO[32:62,], aes(x = INDICE_SOCIO_OUSHIRO, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Índice Socioeconomico (Oushiro, 2015)", y = "Proporção de Palatalização") +
  #geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  theme_light()
#dev.off()

AP.mod_INDICE_SOCIO_OUSHIRO <- glm(VD ~ INDICE_SOCIO_OUSHIRO, data = dados_AP, family = binomial)
summary(AP.mod_INDICE_SOCIO_OUSHIRO)
lrm(VD ~ INDICE_SOCIO_OUSHIRO, data = dados_AP)
plot(allEffects(AP.mod_INDICE_SOCIO_OUSHIRO), type = "response")


# 1 MODELAGEM DE BARBOSA(2023) ####
sort(unique(dados_AP$IDADE_MIGRACAO))
dados_AP$PARTICIPANTE[which(dados_AP$IDADE_MIGRACAO == 45)]
dados_AP

modAP1 <- glmer(VD ~ TONICIDADE + 
                 POSICAO_S +
                 CFP_abertura +
                 CLASSE_MORFOLOGICA3 + 
                 GENERO + 
                 TEMPO_RESIDENCIA + 
                 IDADE_MIGRACAO + 
                 (1|ITEM_LEXICAL) +
                 (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(modAP1)
lrm(VD ~ TONICIDADE + 
      POSICAO_S +
      CFP_abertura +
      CLASSE_MORFOLOGICA3 + 
      GENERO + 
      TEMPO_RESIDENCIA + 
      IDADE_MIGRACAO, data = dados_AP)

car::vif(modAP1)
check_model(modAP1)
check_outliers(modAP1)


# 2 MODELAGEM DE POLI INDICE SOCIO OUSHIRO ####
modAP2 <- glmer(VD ~ TONICIDADE + 
                  POSICAO_S +
                  CFP_abertura +
                  CLASSE_MORFOLOGICA3 + 
                  GENERO + 
                  TEMPO_RESIDENCIA + 
                  IDADE_MIGRACAO +
                  INDICE_SOCIO_OUSHIRO +
                  (1|ITEM_LEXICAL) +
                  (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(modAP2)
lrm(VD ~ TONICIDADE + 
      POSICAO_S +
      CFP_abertura +
      CLASSE_MORFOLOGICA3 + 
      GENERO + 
      TEMPO_RESIDENCIA + 
      IDADE_MIGRACAO +
      INDICE_SOCIO_OUSHIRO, data = dados_AP)

car::vif(modAP1)
check_model(modAP1)
check_outliers(modAP1)


# INDICE SOCIOECONOMICO ####
## Escolaridade ####
AP.prop_ESCOLARIDADE2 <- dados_AP %>%
  count(VD, ESCOLARIDADE2) %>%
  group_by(ESCOLARIDADE2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_ESCOLARIDADE2, aes(x = ESCOLARIDADE2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Escolaridade", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.tab_ESCOLARIDADE2 <- with(dados_AP, table(ESCOLARIDADE2, VD)))
chisq.test(AP.tab_ESCOLARIDADE2)
chisq.test(AP.tab_ESCOLARIDADE2[c(2,3),])

#analise com efeitos mistos
AP.mod_escolaridade <-  glmer(VD ~ ESCOLARIDADE2 +
                              (1|ITEM_LEXICAL) +
                              (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_escolaridade)
lrm(VD ~ ESCOLARIDADE2, data = dados_AP)
plot(allEffects(AP.mod_escolaridade), type = "response")


#escolaridade1
#chisq.test(AP.tab_ESCOLARIDADE[c(1,2),]) #sem diferença pra fund1 e 2
#chisq.test(AP.tab_ESCOLARIDADE[c(4,5),]) #sem diferença pra superior e pósgrad 
### Escolaridade dos Pais ####
#### Pai ####
AP.prop_ESCOLA_PAI2 <- dados_AP %>%
  count(VD, ESCOLA_PAI2) %>%
  group_by(ESCOLA_PAI2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_ESCOLA_PAI2, aes(x = ESCOLA_PAI2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.tab_ESCOLA_PAI2<- with(dados_AP, table(ESCOLA_PAI2, VD)))
chisq.test(AP.tab_ESCOLA_PAI2) #sim
chisq.test(AP.tab_ESCOLA_PAI2[c(1,2),]) 
chisq.test(AP.tab_ESCOLA_PAI2[c(2,3),]) 

#analise com efeitos mistos
AP.mod_escolaridade_pai <-  glmer(VD ~ ESCOLA_PAI2 +
                                (1|ITEM_LEXICAL) +
                                (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_escolaridade_pai)
lrm(VD ~ ESCOLA_PAI2, data = dados_AP)
plot(allEffects(AP.mod_escolaridade_pai), type = "response")


#### Mãe ####
AP.prop_ESCOLA_MAE2 <- dados_AP %>%
  count(VD, ESCOLA_MAE2) %>%
  group_by(ESCOLA_MAE2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_ESCOLA_MAE2, aes(x = ESCOLA_MAE2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


AP.grafico_escolaridade_mae <- AP.prop_ESCOLA_MAE2 %>% 
  filter(VD == "P") %>% 
  ggplot(aes(x = ESCOLA_MAE2, y = prop * 100, group = VD, color = VD, label = label)) + 
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, vjust = -0.5) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal()
AP.grafico_escolaridade_mae


(AP.tab_ESCOLA_MAE2<- with(dados_AP, table(ESCOLA_MAE2, VD)))
chisq.test(AP.tab_ESCOLA_MAE2)

#analise com efeitos mistos
AP.mod_escolaridade_mae <-  glmer(VD ~ ESCOLA_MAE2 +
                                (1|ITEM_LEXICAL) +
                                (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_escolaridade_mae)
lrm(VD ~ ESCOLA_MAE2, data = dados_AP)
plot(allEffects(AP.mod_escolaridade_mae), type = "response")


### Ocupação ####
AP.prop_INDICE_OCUPACAO <- dados_AP %>%
  count(VD, INDICE_OCUPACAO) %>%
  group_by(INDICE_OCUPACAO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_INDICE_OCUPACAO[9:16,], aes(x = INDICE_OCUPACAO, y = prop * 100, label = round(prop * 100, 1))) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Índice de Ocupação", y = "Proporção de Palatalização") +
  theme_light()

ggplot(AP.prop_INDICE_OCUPACAO, aes(x = INDICE_OCUPACAO, y = prop * 100, fill = VD, label = label)) +
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


AP.mod_INDICE_OCUPACAO <- glmer(VD ~ INDICE_OCUPACAO+
                                (1|ITEM_LEXICAL) +
                                (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_INDICE_OCUPACAO)
lrm(VD ~ INDICE_OCUPACAO, data = dados_AP)
plot(allEffects(AP.mod_INDICE_OCUPACAO), type = "response")



### Ocupação outro cargo ####
AP.prop_INDICE_OUTRO_CARGO <- dados_AP %>%
  count(VD, INDICE_OUTRO_CARGO) %>%
  group_by(INDICE_OUTRO_CARGO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_INDICE_OUTRO_CARGO, aes(x = INDICE_OUTRO_CARGO, y = prop * 100, fill = VD, label = label)) +
  geom_bar(stat = "identity", color = "white") + 
 # labs(x = "Outro Cargo", y = "Proporção de Ocorrência") + 
#  scale_x_discrete(labels = c("Não tem", "Tem", "Não se aplica"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))

(AP.tab_INDICE_OUTRO_CARGO <- with(dados_AP, table(INDICE_OUTRO_CARGO, VD)))
chisq.test(AP.tab_INDICE_OUTRO_CARGO)

#analise com efeitos mistos
AP.mod_ocupacao_outro <-  glmer(VD ~ INDICE_OUTRO_CARGO +
                                    (1|ITEM_LEXICAL) +
                                    (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_ocupacao_outro)
lrm(VD ~ INDICE_OUTRO_CARGO, data = dados_AP)
plot(allEffects(AP.mod_ocupacao_outro), type = "response")

### Ocupação SONHOS ####
AP.prop_INDICE_OCUPACAO_SONHOS <- dados_AP %>%
  count(VD, INDICE_OCUPACAO_SONHOS) %>%
  group_by(INDICE_OCUPACAO_SONHOS) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()


ggplot(AP.prop_INDICE_OCUPACAO_SONHOS, aes(x = INDICE_OCUPACAO_SONHOS, y = prop * 100, fill = VD, label = label)) +
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Ocupação dos sonhos", y = "Proporção de Ocorrência") + 
  scale_x_discrete(labels = c("Nenhuma", "Prof. Intermediárias", "Prof. com Especialização", "Não se aplica"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9))

(AP.tab_INDICE_OCUPACAO_SONHOS <- with(dados_AP, table(INDICE_OCUPACAO_SONHOS, VD)))
chisq.test(AP.tab_INDICE_OCUPACAO_SONHOS)


AP.mod_ocupacao_sonhos <- glmer(VD ~ INDICE_OCUPACAO_SONHOS+
                                  (1|ITEM_LEXICAL) +
                                  (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_ocupacao_sonhos)
lrm(VD ~ INDICE_OCUPACAO_SONHOS, data = dados_AP)
plot(allEffects(AP.mod_ocupacao_sonhos), type = "response")

### Ocupação distancia ####
AP.prop_OCUPACAO_DIST <- dados_AP %>%
  count(VD, OCUPACAO_DIST) %>%
  group_by(OCUPACAO_DIST) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



ggplot(AP.prop_OCUPACAO_DIST, aes(x = OCUPACAO_DIST, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Individual", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(
      color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(
      color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.tab_OCUPACAO_DIST<- with(dados_AP, table(OCUPACAO_DIST, VD)))
chisq.test(AP.tab_OCUPACAO_DIST) #sim
chisq.test(AP.tab_OCUPACAO_DIST[c(2,3),])

#teste efeitos mistos
AP.mod_ocupacao_dist <- glmer(VD ~ OCUPACAO_DIST+
                                  (1|ITEM_LEXICAL) +
                                  (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_ocupacao_dist)
lrm(VD ~ OCUPACAO_DIST, data = dados_AP)
plot(allEffects(AP.mod_ocupacao_dist), type = "response")


### Ocupação locomoção ####
#analise de locomoção com todos os itens foi transformada na seguinte OCUPACAO_LOCOMOCAO2
AP.prop_OCUPACAO_LOCOMOCAO2 <- dados_AP %>%
  count(VD, OCUPACAO_LOCOMOCAO2) %>%
  group_by(OCUPACAO_LOCOMOCAO2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



ggplot(AP.prop_OCUPACAO_LOCOMOCAO2, aes(x = OCUPACAO_LOCOMOCAO2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Individual", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(
      color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(
      color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.tab_OCUPACAO_LOCOMOCAO2 <- with(dados_AP, table(OCUPACAO_LOCOMOCAO2, VD)))
chisq.test(AP.tab_OCUPACAO_LOCOMOCAO2) #sim
chisq.test(AP.tab_OCUPACAO_LOCOMOCAO2[c(2,3),])

#teste efeitos mistos
AP.mod_ocupacao_locomocao <- glmer(VD ~ OCUPACAO_LOCOMOCAO2+
                                  (1|ITEM_LEXICAL) +
                                  (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_ocupacao_locomocao)
lrm(VD ~ OCUPACAO_LOCOMOCAO2, data = dados_AP)
plot(allEffects(AP.mod_ocupacao_locomocao), type = "response")



### Ocupação dos Pais ####
#### Pai ####
AP.prop_INDICE_OCUPACAO_PAI <- dados_AP %>%
  count(VD, INDICE_OCUPACAO_PAI) %>%
  group_by(INDICE_OCUPACAO_PAI) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_INDICE_OCUPACAO_PAI, aes(x = INDICE_OCUPACAO_PAI, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Índice Ocupação do Pai", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(
      color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(
      color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))

AP.mod_INDICE_OCUPACAO_PAI <- glmer(VD ~ INDICE_OCUPACAO_PAI+
                                      (1|ITEM_LEXICAL) +
                                      (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_INDICE_OCUPACAO_PAI)
lrm(VD ~ INDICE_OCUPACAO_PAI, data = dados_AP)
plot(allEffects(AP.mod_INDICE_OCUPACAO_PAI), type = "response")


#### Mãe ####
AP.prop_INDICE_OCUPACAO_MAE <- dados_AP %>%
  count(VD, INDICE_OCUPACAO_MAE) %>%
  group_by(INDICE_OCUPACAO_MAE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_INDICE_OCUPACAO_MAE, aes(x = INDICE_OCUPACAO_MAE, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Índice de Ocupação da Mãe", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(
      color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(
      color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))

AP.mod_INDICE_OCUPACAO_MAE <- glmer(VD ~ INDICE_OCUPACAO_MAE+
                                      (1|ITEM_LEXICAL) +
                                      (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_INDICE_OCUPACAO_MAE)
lrm(VD ~ INDICE_OCUPACAO_MAE, data = dados_AP)
plot(allEffects(AP.mod_INDICE_OCUPACAO_MAE), type = "response")


### Mega sena ####
AP.prop_MEGA_SENA2 <- dados_AP %>%
  count(VD, MEGA_SENA2) %>%
  group_by(MEGA_SENA2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



ggplot(AP.prop_MEGA_SENA2, aes(x = MEGA_SENA2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Individual", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(
      color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(
      color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.tab_MEGA_SENA2 <- with(dados_AP, table(MEGA_SENA2, VD)))
chisq.test(AP.tab_MEGA_SENA2) #sim
chisq.test(AP.tab_MEGA_SENA2[c(1,2),])
chisq.test(AP.tab_MEGA_SENA2[c(2,3),])
chisq.test(AP.tab_MEGA_SENA2[c(1,3),])
chisq.test(AP.tab_MEGA_SENA2[c(3,4),])
chisq.test(AP.tab_MEGA_SENA2[c(4,5),])
chisq.test(AP.tab_MEGA_SENA2[c(3,5),])



#teste efeitos mistos
AP.mod_megasena <- glmer(VD ~ MEGA_SENA2 +
                           (1|ITEM_LEXICAL) +
                           (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_megasena)
lrm(VD ~ MEGA_SENA2, data = dados_AP)
plot(allEffects(AP.mod_megasena), type = "response")


#### Mega sena trabalhar ####
AP.prop_MEGASENA_TRABALHAR2 <- dados_AP %>%
  count(VD, MEGASENA_TRABALHAR2) %>%
  group_by(MEGASENA_TRABALHAR2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



ggplot(AP.prop_MEGASENA_TRABALHAR2, aes(x = MEGASENA_TRABALHAR2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Individual", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(
      color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(
      color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.tab_MEGASENA_TRABALHAR2 <- with(dados_AP, table(MEGASENA_TRABALHAR2, VD)))
chisq.test(AP.tab_MEGASENA_TRABALHAR2) #nao


#teste efeitos mistos
AP.mod_megasena_trabalhar <- glmer(VD ~ MEGASENA_TRABALHAR2 +
                           (1|ITEM_LEXICAL) +
                           (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_megasena_trabalhar)
lrm(VD ~ MEGA_SENA2, data = dados_AP)
plot(allEffects(AP.mod_megasena_trabalhar), type = "response")




### Renda Individual ####
AP.prop_RENDA_IND <- dados_AP %>%
  count(VD, RENDA_IND) %>%
  group_by(RENDA_IND) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_RENDA_IND, aes(x = RENDA_IND, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Renda Individual", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.tab_RENDA_IND <- with(dados_AP, table(RENDA_IND, VD)))
chisq.test(AP.tab_RENDA_IND) #tem correlação
chisq.test(AP.tab_RENDA_IND[c(1,2),]) #nao
chisq.test(AP.tab_RENDA_IND[c(4,5),]) #sim
chisq.test(AP.tab_RENDA_IND[c(2,4),]) #sim


#teste efeitos mistos
AP.mod_renda_ind <- glmer(VD ~ RENDA_IND +
                           (1|ITEM_LEXICAL) +
                           (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_renda_ind)
lrm(VD ~ RENDA_IND, data = dados_AP)
plot(allEffects(AP.mod_renda_ind), type = "response")


### Renda Familiar ####
AP.prop_RENDA_FAM <- dados_AP %>%
  count(VD, RENDA_FAM) %>%
  group_by(RENDA_FAM) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_RENDA_FAM, aes(x = RENDA_FAM, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.tab_RENDA_FAM <- with(dados_AP, table(RENDA_FAM, VD)))
chisq.test(AP.tab_RENDA_FAM) #tem correlação
chisq.test(AP.tab_RENDA_FAM[c(1,2),]) #sim
chisq.test(AP.tab_RENDA_FAM[c(2,3),]) #nao
chisq.test(AP.tab_RENDA_FAM[c(3,4),]) #nao
chisq.test(AP.tab_RENDA_FAM[c(4,5),]) #nao



#teste efeitos mistos
AP.mod_renda_fam <- glmer(VD ~ RENDA_FAM +
                            (1|ITEM_LEXICAL) +
                            (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_renda_fam)
lrm(VD ~ RENDA_IND, data = dados_AP)
plot(allEffects(AP.mod_renda_fam), type = "response")




### m2 ####
AP.prop_media_m2 <- dados_AP %>%
  count(VD, media_m2) %>%
  group_by(media_m2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print(n = 40)

ggplot(AP.prop_media_m2[21:40,], aes(x = media_m2, y = prop * 100, label = round(prop * 100, 1))) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Índice de Ocupação", y = "Proporção de Palatalização") +
  theme_light()


AP.mod_media_m2 <- glmer(VD ~ media_m2+
                           (1|ITEM_LEXICAL)+
                           (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_media_m2)
lrm(VD ~ media_m2, data = dados_AP)
plot(allEffects(AP.mod_media_m2), type = "response")

### Bairro ####
ordem_bairros <- dados_AP %>%
  group_by(BAIRRO) %>%
  summarise(media_geral = mean(media_m2, na.rm = TRUE)) %>%
  arrange(media_geral) %>%  # ou arrange(media_geral) para ordem crescente
  pull(BAIRRO)

AP.prop_BAIRRO <- dados_AP %>%
  count(VD, BAIRRO, media_m2) %>%
  mutate(BAIRRO = factor(BAIRRO, levels = ordem_bairros)) %>%  # Reordena os níveis
  group_by(BAIRRO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  ungroup()

AP.prop_BAIRRO %>% 
ggplot(aes(x = BAIRRO, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  coord_flip() +  # Barras horizontais
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))



#### Região ####
AP.prop_BAIRRO_REGIAO <- dados_AP %>%
  count(VD, BAIRRO_REGIAO2) %>%
  group_by(BAIRRO_REGIAO2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_BAIRRO_REGIAO, aes(x = BAIRRO_REGIAO2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Região", y = "Proporção de Ocorrência") + 
  scale_x_discrete(labels = c("Centro", "Periferia Norte", "Periferia Sul"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.tab_BAIRRO_REGIAO <- with(dados_AP, table(BAIRRO_REGIAO2, VD)))
chisq.test(AP.tab_BAIRRO_REGIAO) #tem correlação

#teste efeitos mistos
AP.mod_BAIRRO_REGIAO <- glmer(VD ~ BAIRRO_REGIAO2 +
                            (1|ITEM_LEXICAL) +
                            (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_BAIRRO_REGIAO)
lrm(VD ~ BAIRRO_REGIAO, data = dados_AP)
plot(allEffects(AP.mod_BAIRRO_REGIAO), type = "response")


### Número de Banheiros ####
AP.prop_NBANHEIROS <- dados_AP %>%
  count(VD, NBANHEIROS) %>%
  group_by(NBANHEIROS) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

AP.prop_NBANHEIROS %>% 
  ggplot(aes(x = NBANHEIROS, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.prop_NBANHEIROS <- with(dados_AP, table(NBANHEIROS, VD)))
chisq.test(AP.prop_NBANHEIROS)
chisq.test(AP.prop_NBANHEIROS[c(1,2)])
chisq.test(AP.prop_NBANHEIROS[c(2,3)])

#teste efeitos mistos
AP.mod_NBANHEIROS <- glmer(VD ~ NBANHEIROS +
                                (1|ITEM_LEXICAL) +
                                (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_NBANHEIROS)
lrm(VD ~ NBANHEIROS, data = dados_AP)
plot(allEffects(AP.mod_NBANHEIROS), type = "response")




### Número de Quartos ####
AP.prop_NQUARTOS <- dados_AP %>%
  count(VD, NQUARTOS) %>%
  group_by(NQUARTOS) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

AP.prop_NQUARTOS %>% 
  ggplot(aes(x = NQUARTOS, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.prop_NQUARTOS <- with(dados_AP, table(NQUARTOS, VD)))
chisq.test(AP.prop_NQUARTOS)
chisq.test(AP.prop_NQUARTOS[c(1,2)])
chisq.test(AP.prop_NQUARTOS[c(2,3)])

#teste efeitos mistos
AP.mod_NQUARTOS <- glmer(VD ~ NQUARTOS +
                             (1|ITEM_LEXICAL) +
                             (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_NQUARTOS)
lrm(VD ~ NQUARTOS, data = dados_AP)
plot(allEffects(AP.mod_NQUARTOS), type = "response")

### Densidade ####
#teste efeitos mistos
AP.mod_DENSIDADE_HABITACAO <- glmer(VD ~ DENSIDADE_HABITACAO +
                           (1|ITEM_LEXICAL) +
                           (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_DENSIDADE_HABITACAO)
lrm(VD ~ NQUARTOS, data = dados_AP)
plot(allEffects(AP.mod_NQUARTOS), type = "response")


### Tipo Moradia ####
AP.prop_IMOVEL <- dados_AP %>%
  count(VD, IMOVEL) %>%
  group_by(IMOVEL) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

AP.prop_IMOVEL %>% 
  ggplot(aes(x = IMOVEL, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.prop_IMOVEL <- with(dados_AP, table(IMOVEL, VD)))
chisq.test(AP.prop_IMOVEL)
chisq.test(AP.prop_IMOVEL[c(1,2)])
chisq.test(AP.prop_IMOVEL[c(1,3)])

#teste efeitos mistos
AP.mod_IMOVEL <- glmer(VD ~ IMOVEL +
                             (1|ITEM_LEXICAL) +
                             (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_IMOVEL)
lrm(VD ~ IMOVEL, data = dados_AP)
plot(allEffects(AP.mod_IMOVEL), type = "response")



### Propriedade característica ####
AP.prop_PROPRIEDADE <- dados_AP %>%
  count(VD, PROPRIEDADE) %>%
  group_by(PROPRIEDADE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

AP.prop_PROPRIEDADE %>% 
  ggplot(aes(x = PROPRIEDADE, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.prop_PROPRIEDADE <- with(dados_AP, table(PROPRIEDADE, VD)))
chisq.test(AP.prop_PROPRIEDADE)
chisq.test(AP.prop_PROPRIEDADE[c(1,2)])
chisq.test(AP.prop_PROPRIEDADE[c(1,3)])

#teste efeitos mistos
AP.mod_PROPRIEDADE <- glmer(VD ~ PROPRIEDADE +
                         (1|ITEM_LEXICAL) +
                         (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_PROPRIEDADE)
lrm(VD ~ PROPRIEDADE, data = dados_AP)
plot(allEffects(AP.mod_PROPRIEDADE), type = "response")




### Número de Pessoas ####
AP.prop_NPESSOAS <- dados_AP %>%
  count(VD, NPESSOAS) %>%
  group_by(NPESSOAS) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

AP.prop_NPESSOAS %>% 
  ggplot(aes(x = NPESSOAS, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.prop_NPESSOAS <- with(dados_AP, table(NPESSOAS, VD)))
chisq.test(AP.prop_NPESSOAS)
chisq.test(AP.prop_NPESSOAS[c(1,2)])
chisq.test(AP.prop_NPESSOAS[c(2,3)])


#teste efeitos mistos
AP.mod_NPESSOAS <- glmer(VD ~ NPESSOAS +
                              (1|ITEM_LEXICAL) +
                              (1|PARTICIPANTE), 
                         data = dados_AP, family = binomial)
summary(AP.mod_NPESSOAS)
lrm(VD ~ NPESSOAS, data = dados_AP)
plot(allEffects(AP.mod_PROPRIEDADE), type = "response")



### Lazer ####
AP.prop_LAZER_CARACTERISTICA <- dados_AP %>%
  count(VD, LAZER_CARACTERISTICA) %>%
  group_by(LAZER_CARACTERISTICA) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_LAZER_CARACTERISTICA, aes(x = LAZER_CARACTERISTICA, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Opções de Lazer", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.tab_LAZER_CARACTERISTICA <- with(dados_AP, table(LAZER_CARACTERISTICA, VD)))
chisq.test(AP.tab_LAZER_CARACTERISTICA) #tem correlação


#teste efeitos mistos
AP.mod_LAZER_CARACTERISTICA <- glmer(VD ~ LAZER_CARACTERISTICA +
                              (1|ITEM_LEXICAL) +
                              (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_LAZER_CARACTERISTICA)
lrm(VD ~ LAZER_CARACTERISTICA, data = dados_AP)
plot(allEffects(AP.mod_LAZER_CARACTERISTICA), type = "response")



### Lazer Campinas####
AP.prop_LAZER_CAMPINAS_CARACTERISTICA <- dados_AP %>%
  count(VD, LAZER_CAMPINAS_CARACTERISTICA) %>%
  group_by(LAZER_CAMPINAS_CARACTERISTICA) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_LAZER_CAMPINAS_CARACTERISTICA, aes(x = LAZER_CAMPINAS_CARACTERISTICA, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Lazer em Campinas", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.tab_LAZER_CAMPINAS_CARACTERISTICA <- with(dados_AP, table(LAZER_CAMPINAS_CARACTERISTICA, VD)))
chisq.test(AP.tab_LAZER_CAMPINAS_CARACTERISTICA)

#teste efeitos mistos
AP.mod_LAZER_CAMPINAS_CARACTERISTICA <- glmer(VD ~ LAZER_CAMPINAS_CARACTERISTICA +
                                       (1|ITEM_LEXICAL) +
                                       (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_LAZER_CAMPINAS_CARACTERISTICA)
lrm(VD ~ LAZER_CAMPINAS_CARACTERISTICA, data = dados_AP)
plot(allEffects(AP.mod_LAZER_CAMPINAS_CARACTERISTICA), type = "response")




### Viagem ####
#costuma viajar?
AP.prop_VIAGEM <- dados_AP %>%
  count(VD, VIAGEM) %>%
  group_by(VIAGEM) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_VIAGEM, aes(x = VIAGEM, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.tab_VIAGEM <- with(dados_AP, table(VIAGEM, VD)))
chisq.test(AP.tab_VIAGEM) #tem correlação

#teste efeitos mistos
AP.mod_VIAGEM <- glmer(VD ~ VIAGEM +
                         (1|ITEM_LEXICAL) +
                         (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_VIAGEM)
lrm(VD ~ VIAGEM, data = dados_AP)
plot(allEffects(AP.mod_VIAGEM), type = "response")



### Viagem lugar ####
AP.prop_VIAGEM_LUGAR <- dados_AP %>%
  count(VD, VIAGEM_LUGAR) %>%
  group_by(VIAGEM_LUGAR) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_VIAGEM_LUGAR, aes(x = VIAGEM_LUGAR, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.tab_VIAGEM_LUGAR <- with(dados_AP, table(VIAGEM_LUGAR, VD)))
chisq.test(AP.tab_VIAGEM_LUGAR) #tem correlação
chisq.test(AP.tab_VIAGEM_LUGAR[c(1,3),])

#teste efeitos mistos
AP.mod_VIAGEM_LUGAR <- glmer(VD ~ VIAGEM_LUGAR +
                         (1|ITEM_LEXICAL) +
                         (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_VIAGEM_LUGAR)
lrm(VD ~ VIAGEM_LUGAR, data = dados_AP)
plot(allEffects(AP.mod_VIAGEM_LUGAR), type = "response")



### Viagem vontade ####
AP.prop_LAZER_VIAGEM_VONTADE2 <- dados_AP %>%
  count(VD, LAZER_VIAGEM_VONTADE2) %>%
  group_by(LAZER_VIAGEM_VONTADE2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_LAZER_VIAGEM_VONTADE2, aes(x = LAZER_VIAGEM_VONTADE2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.tab_LAZER_VIAGEM_VONTADE2 <- with(dados_AP, table(LAZER_VIAGEM_VONTADE2, VD)))
chisq.test(AP.tab_LAZER_VIAGEM_VONTADE2) #tem correlação
chisq.test(AP.tab_LAZER_VIAGEM_VONTADE2[c(1,2),])

#teste efeitos mistos
AP.mod_LAZER_VIAGEM_VONTADE2 <- glmer(VD ~ LAZER_VIAGEM_VONTADE2 +
                         (1|ITEM_LEXICAL) +
                         (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_LAZER_VIAGEM_VONTADE2)
lrm(VD ~ LAZER_VIAGEM_VONTADE2, data = dados_AP)
plot(allEffects(AP.mod_LAZER_VIAGEM_VONTADE2), type = "response")



### Infancia ####
AP.prop_INFANCIA_MEMORIA <- dados_AP %>%
  count(VD, INFANCIA_MEMORIA) %>%
  group_by(INFANCIA_MEMORIA) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_INFANCIA_MEMORIA, aes(x = INFANCIA_MEMORIA, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(AP.prop_INFANCIA_MEMORIA <- with(dados_AP, table(INFANCIA_MEMORIA, VD)))
chisq.test(AP.prop_INFANCIA_MEMORIA) #tem correlação
chisq.test(AP.prop_INFANCIA_MEMORIA[c(1,2),])


#teste efeitos mistos
AP.mod_INFANCIA_MEMORIA <- glmer(VD ~ INFANCIA_MEMORIA +
                                        (1|ITEM_LEXICAL) +
                                        (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_INFANCIA_MEMORIA)
lrm(VD ~ INFANCIA_MEMORIA, data = dados_AP)
plot(allEffects(AP.mod_INFANCIA_MEMORIA), type = "response")



# PCA ####
escalas_AP <- dados_AP %>%
  select(INDICE_ESCOL3_norm, 
         INDICE_ESCOL_PAI_norm,
         INDICE_ESCOL_MAE_norm, 
         INDICE_OCUPACAO_norm, 
         INDICE_OCUPACAO_PAI_norm, 
         INDICE_OCUPACAO_MAE_norm,
         #INDICE_OUTRO_CARGO2_norm, 
         INDICE_OCUPACAO_SONHOS2_norm,
         #INDICE_LOCOMOCAO_norm, 
         INDICE_MEGA_norm,
         INDICE_RENDA_IND_norm, 
         #INDICE_RENDA_FAM_norm,
         #INDICE_BAIRRO_norm,
         #DENSIDADE_HABITACAO_norm,
         INDICE_IMOVEL_norm,
         INDICE_LAZER_norm, 
         INDICE_LAZER_CAMPINAS_norm,
         INDICE_VIAGEM_norm, 
         INDICE_VIAGEM_LUGAR_norm, 
         INDICE_VIAGEM_VONTADE_norm, 
         INDICE_INFANCIA_norm
  ) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

pca_AP <- prcomp(escalas_AP, center = TRUE, scale. = TRUE)
summary(pca_AP)          # variância explicada por cada componente
pca_AP$rotation # cargas (contribuição das variáveis)
pca_AP$x                  # coordenadas dos indivíduos



# Scree plot
fviz_eig(pca_AP, 
         addlabels = TRUE,    # mostra valores no gráfico
         ylim = c(0, 40))     # ajusta o eixo Y para ver melhor

fviz_contrib(pca_AP, choice = "var", axes = 1, top = 10)
fviz_contrib(pca_AP, choice = "var", axes = 2, top = 10)
fviz_contrib(pca_AP, choice = "var", axes = 3, top = 10)
fviz_contrib(pca_AP, choice = "var", axes = 4, top = 10)



principal(dados_AP, nfactors= 6, rotate="none") 

#Resumindo, as variáveis que mais se repetem entre os PCs que explicam 84% são:

#INDICE_ESCOL_PAI_norm, INDICE_ESCOL_MAE_norm, INDICE_OCUPACAO_PAI_norm
#INDICE_OCUPACAO_norm
#INDICE_VIAGEM_VONTADE_norm, INDICE_VIAGEM_LUGAR_norm, INDICE_VIAGEM_norm
#INDICE_LAZER_CAMPINAS_norm, INDICE_INFANCIA_norm
#INDICE_RENDA_IND_norm
#INDICE_IMOVEL_norm
