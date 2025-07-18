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

#png("VD_AP-10.idade_migracao.png")
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

#png("VD_AP-10.idade_migracao.png")
ggplot(AP.prop_TEMPO_RESIDENCIA[24:46,], aes(x = TEMPO_RESIDENCIA, y = prop * 100, label = round(prop * 100, 1))) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  #labs(x = "Idade de Migração", y = "Proporção de Palatalização") +
  #geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  theme_light()
#dev.off()

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
## Variáveis Sociais ####
### Escolaridade ####
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
chisq.test(AP.tab_ESCOLARIDADE2[c(1,3),])

#escolaridade1
#chisq.test(AP.tab_ESCOLARIDADE[c(1,2),]) #sem diferença pra fund1 e 2
#chisq.test(AP.tab_ESCOLARIDADE[c(4,5),]) #sem diferença pra superior e pósgrad 

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
  #labs(x = "Idade de Migração", y = "Proporção de Palatalização") +
  #geom_text(size = 4, position = position_stack(vjust = 0.5)) +
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


### Renda Individual ####
AP.prop_RENDA_IND <- dados_AP %>%
  filter(CFS_pontoc2 == "coronal") %>% 
  count(VD, RENDA_IND) %>%
  group_by(RENDA_IND) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_RENDA_IND, aes(x = RENDA_IND, y = prop * 100, fill = VD, label = label)) + 
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


(AP.tab_RENDA_IND <- with(dados_AP, table(RENDA_IND, VD)))
chisq.test(AP.tab_RENDA_IND)
chisq.test(AP.tab_RENDA_IND[c(1,2),])
chisq.test(AP.tab_RENDA_IND[c(4,5),])
chisq.test(AP.tab_RENDA_IND[c(2,4),])

