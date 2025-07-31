#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%# PROCESSO DE PALATALIZAÇÃO #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 15/07/2025 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% V1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# CONT.FON.SEG ####

AP.prop_CONT_FON_SEG <- dados_AP %>% 
  count(VD, CONT_FON_SEG) %>%
  group_by(CONT_FON_SEG) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_CONT_FON_SEG, aes(x = CONT_FON_SEG, y = prop * 100, fill = VD, label = label)) + 
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

#CFS_coronal de acordo com BARBOSA (2023)
AP.prop_CFS_pontoc2<- dados_AP %>% 
  count(VD, CFS_pontoc2) %>%
  group_by(CFS_pontoc2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_CFS_pontoc2, aes(x = CFS_pontoc2, y = prop * 100, fill = VD, label = label)) + 
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


# VD ####
AP.prop_VD <- dados_AP %>% 
  filter(CFS_pontoc2 == "coronal") %>% 
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
  filter(CFS_pontoc2 == "coronal") %>% 
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
  filter(CFS_pontoc2 == "coronal") %>% 
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
  filter(CFS_pontoc2 == "coronal") %>% 
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
  filter(CFS_pontoc2 == "coronal") %>% 
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
  filter(CFS_pontoc2 == "coronal") %>% 
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
  filter(CFS_pontoc2 == "coronal") %>% 
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
  filter(CFS_pontoc2 == "coronal") %>% 
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
  filter(CFS_pontoc2 == "coronal") %>% 
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
  filter(CFS_pontoc2 == "coronal") %>% 
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

# MODELAGEM DE BARBOSA(2023) ####
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

# INDICE SOCIOECONOMICO ####
## Escolaridade ####
AP.prop_ESCOLARIDADE2 <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
  count(VD, ESCOLARIDADE2) %>%
  group_by(ESCOLARIDADE2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_ESCOLARIDADE2, aes(x = ESCOLARIDADE2, y = prop * 100, fill = VD, label = label)) + 
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


(AP.tab_ESCOLARIDADE2 <- with(dados_AP, table(ESCOLARIDADE2, VD)))
chisq.test(AP.tab_ESCOLARIDADE2)
chisq.test(AP.tab_ESCOLARIDADE2[c(2,3),])

#escolaridade1
#chisq.test(AP.tab_ESCOLARIDADE[c(1,2),]) #sem diferença pra fund1 e 2
#chisq.test(AP.tab_ESCOLARIDADE[c(4,5),]) #sem diferença pra superior e pósgrad 
### Escolaridade dos Pais ####
#### Pai ####
AP.prop_ESCOLA_PAI2 <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
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
chisq.test(AP.tab_ESCOLA_PAI2[c(3,4),]) 

#### Mãe ####
AP.prop_ESCOLA_MAE2 <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
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
chisq.test(AP.tab_ESCOLA_MAE2) #sim
chisq.test(AP.tab_ESCOLA_MAE2[c(1,2),])
chisq.test(AP.tab_ESCOLA_MAE2[c(2,3),]) 
chisq.test(AP.tab_ESCOLA_MAE2[c(2,4),])


### Ocupação ####
AP.prop_INDICE_OCUPACAO <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
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


AP.mod_INDICE_OCUPACAO <- glm(VD ~ INDICE_OCUPACAO, data = dados_AP, family = binomial)
summary(AP.mod_INDICE_OCUPACAO)
lrm(VD ~ INDICE_OCUPACAO, data = dados_AP)
plot(allEffects(AP.mod_INDICE_OCUPACAO), type = "response")


### Ocupação outro cargo ####
AP.prop_INDICE_OUTRO_CARGO <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
  count(VD, INDICE_OUTRO_CARGO) %>%
  group_by(INDICE_OUTRO_CARGO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_INDICE_OUTRO_CARGO[10:16,], aes(x = INDICE_OUTRO_CARGO, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  #labs(x = "Índice de Ocupação", y = "Proporção de Palatalização") +
  theme_light()

AP.mod_INDICE_OUTRO_CARGO <- glm(VD ~ INDICE_OUTRO_CARGO, data = dados_AP, family = binomial)
summary(AP.mod_INDICE_OUTRO_CARGO)
lrm(VD ~ INDICE_OUTRO_CARGO, data = dados_AP)
plot(allEffects(AP.mod_INDICE_OUTRO_CARGO), type = "response")


### Ocupação SONHOS ####
AP.prop_INDICE_OCUPACAO_SONHOS <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
  count(VD, INDICE_OCUPACAO_SONHOS) %>%
  group_by(INDICE_OCUPACAO_SONHOS) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_INDICE_OCUPACAO_SONHOS[8:14,], aes(x = INDICE_OCUPACAO_SONHOS, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  #labs(x = "Índice de Ocupação", y = "Proporção de Palatalização") +
  theme_light()


AP.mod_INDICE_OCUPACAO_SONHOS <- glm(VD ~ INDICE_OCUPACAO_SONHOS, data = dados_AP, family = binomial)
summary(AP.mod_INDICE_OCUPACAO_SONHOS)
lrm(VD ~ INDICE_OCUPACAO_SONHOS, data = dados_AP)
plot(allEffects(AP.mod_INDICE_OCUPACAO_SONHOS), type = "response")

### Ocupação distancia ####
AP.prop_OCUPACAO_DIST <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
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

### Ocupação locomoção ####
#analise de locomoção com todos os itens foi transformada na seguinte OCUPACAO_LOCOMOCAO2
AP.prop_OCUPACAO_LOCOMOCAO2 <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
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


### Ocupação dos Pais ####
#### Pai ####
AP.prop_INDICE_OCUPACAO_PAI <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
  count(VD, INDICE_OCUPACAO_PAI) %>%
  group_by(INDICE_OCUPACAO_PAI) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_INDICE_OCUPACAO_PAI[5:8,], aes(x = INDICE_OCUPACAO_PAI, y = prop * 100, label = round(prop * 100, 1))) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Índice de Ocupação", y = "Proporção de Palatalização") +
  theme_light()

AP.mod_INDICE_OCUPACAO_PAI <- glm(VD ~ INDICE_OCUPACAO_PAI, data = dados_AP, family = binomial)
summary(AP.mod_INDICE_OCUPACAO_PAI)
lrm(VD ~ INDICE_OCUPACAO_PAI, data = dados_AP)
plot(allEffects(AP.mod_INDICE_OCUPACAO_PAI), type = "response")


#### Mãe ####
AP.prop_INDICE_OCUPACAO_MAE <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
  count(VD, INDICE_OCUPACAO_MAE) %>%
  group_by(INDICE_OCUPACAO_MAE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_INDICE_OCUPACAO_MAE[6:10,], aes(x = INDICE_OCUPACAO_MAE, y = prop * 100, label = round(prop * 100, 1))) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Índice de Ocupação", y = "Proporção de Palatalização") +
  theme_light()

AP.mod_INDICE_OCUPACAO_MAE <- glm(VD ~ INDICE_OCUPACAO_MAE, data = dados_AP, family = binomial)
summary(AP.mod_INDICE_OCUPACAO_MAE)
lrm(VD ~ INDICE_OCUPACAO_MAE, data = dados_AP)
plot(allEffects(AP.mod_INDICE_OCUPACAO_MAE), type = "response")


### Mega sena ####
AP.prop_MEGA_SENA2 <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
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


### Mega sena ####
AP.prop_MEGASENA_TRABALHAR2 <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
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


### Renda Individual ####
AP.prop_RENDA_IND <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal", !is.na(RENDA_IND)) %>% 
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


### Renda Familiar ####
AP.prop_RENDA_FAM <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
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


### m2 ####
AP.prop_media_m2 <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
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


AP.mod_media_m2 <- glm(VD ~ media_m2, data = dados_AP, family = binomial)
summary(AP.mod_media_m2)
lrm(VD ~ INDICE_OCUPACAO, data = dados_AP)
plot(allEffects(AP.mod_media_m2), type = "response")

### Bairro ####

ordem_bairros <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal", !is.na(media_m2)) %>%
  group_by(BAIRRO) %>%
  summarise(media_geral = mean(media_m2, na.rm = TRUE)) %>%
  arrange(media_geral) %>%  # ou arrange(media_geral) para ordem crescente
  pull(BAIRRO)

AP.prop_BAIRRO <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal", !is.na(media_m2)) %>% 
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


(AP.prop_BAIRRO <- with(dados_AP, table(BAIRRO, VD)))
chisq.test(AP.prop_BAIRRO)


### Número de Banheiros ####
AP.prop_NBANHEIROS <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
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

### Número de Quartos ####
AP.prop_NQUARTOS <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
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

### Tipo Moradia ####
AP.prop_IMOVEL <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
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

### Propriedade característica ####
AP.prop_PROPRIEDADE <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
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

### Número de Pessoas ####
AP.prop_NPESSOAS <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
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

### Lazer ####
AP.prop_LAZER_CARACTERISTICA <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
  count(VD, LAZER_CARACTERISTICA) %>%
  group_by(LAZER_CARACTERISTICA) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_LAZER_CARACTERISTICA, aes(x = LAZER_CARACTERISTICA, y = prop * 100, fill = VD, label = label)) + 
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


(AP.tab_LAZER_CARACTERISTICA <- with(dados_AP, table(LAZER_CARACTERISTICA, VD)))
chisq.test(AP.tab_LAZER_CARACTERISTICA) #tem correlação
chisq.test(AP.tab_LAZER_CARACTERISTICA[c(1,2),])
chisq.test(AP.tab_LAZER_CARACTERISTICA[c(2,3),])
chisq.test(AP.tab_LAZER_CARACTERISTICA[c(3,4),])
chisq.test(AP.tab_LAZER_CARACTERISTICA[c(1,4),])


### Lazer Campinas####
AP.prop_LAZER_CAMPINAS_CARACTERISTICA <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
  count(VD, LAZER_CAMPINAS_CARACTERISTICA) %>%
  group_by(LAZER_CAMPINAS_CARACTERISTICA) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_LAZER_CAMPINAS_CARACTERISTICA, aes(x = LAZER_CAMPINAS_CARACTERISTICA, y = prop * 100, fill = VD, label = label)) + 
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


(AP.tab_LAZER_CAMPINAS_CARACTERISTICA <- with(dados_AP, table(LAZER_CAMPINAS_CARACTERISTICA, VD)))
chisq.test(AP.tab_LAZER_CAMPINAS_CARACTERISTICA) #tem correlação
chisq.test(AP.tab_LAZER_CAMPINAS_CARACTERISTICA[c(1,2),])
chisq.test(AP.tab_LAZER_CAMPINAS_CARACTERISTICA[c(2,3),])
chisq.test(AP.tab_LAZER_CAMPINAS_CARACTERISTICA[c(3,4),]) #falantes que nfalaram que não tem e que não sae não tem correlação
chisq.test(AP.tab_LAZER_CAMPINAS_CARACTERISTICA[c(2,5),])


### Viagem ####
#costuma viajar?
AP.prop_VIAGEM <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
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



### Tipo de Viagem ####

AP.prop_VIAGEM_LUGAR <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
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
chisq.test(AP.tab_VIAGEM_LUGAR[c(4,5),])
chisq.test(AP.tab_VIAGEM_LUGAR[c(2,3),])


### Viagem vontade ####
AP.prop_LAZER_VIAGEM_VONTADE2 <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
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
chisq.test(AP.tab_LAZER_VIAGEM_VONTADE2[c(2,3),])
chisq.test(AP.tab_LAZER_VIAGEM_VONTADE2[c(1,4),])


### Infancia ####
AP.prop_INFANCIA_MEMORIA <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
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







