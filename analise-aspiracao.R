#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%# PROCESSO DE ASPIRAÇÃO #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 15/07/2025 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% V1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# DISTRIBUIÇÃO GERAL ####
distribuicao.geral <- dados2 %>% 
  count(VD) %>%
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>%
  print()

distribuicao.geral %>%   
  ggplot(aes(x = VD, y = prop, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Proporção de Ocorrência", fill = "VD") + 
  scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(aes(label = label), 
            vjust = -0.2,
            size = 3.5) +
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


# CONT.FON.SEG ####
HAP.prop_CONT_FON_SEG <- dados_HAP %>% 
  count(VD, CONT_FON_SEG) %>%
  group_by(CONT_FON_SEG) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_CONT_FON_SEG, aes(x = CONT_FON_SEG, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))

#CFS_sonoridade de acordo com BARBOSA (2023)
HAP.prop_CFS_sonoridade <- dados_HAP %>% 
  count(VD, CFS_sonoridade) %>%
  group_by(CFS_sonoridade) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_CFS_sonoridade, aes(x = CFS_sonoridade, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


# VD ####
HAP.prop_VD <- dados_HAP %>% 
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD) %>%
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_VD, aes(x = VD, y = prop, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(aes(label = label), vjust = -0.3, size = 4) + 
  scale_fill_brewer(palette = "Reds")+
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     expand = expansion(mult = c(0, 0.18))) + #aumenta espaço no topo
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9),   # tamanho do título eixo Y
    legend.position = "none")


# TONICIDADE ####
HAP.prop_TONICIDADE <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, TONICIDADE) %>%
  group_by(TONICIDADE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_TONICIDADE, aes(x = TONICIDADE, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))

(HAP.tab_TONICIDADE <- with(dados_HAP, table(TONICIDADE, VD)))
chisq.test(HAP.tab_TONICIDADE)


# POSICAO ####
HAP.prop_POSICAO <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, POSICAO_S) %>%
  group_by(POSICAO_S) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_POSICAO, aes(x = POSICAO_S, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))

(HAP.tab_POSICAO <- with(dados_HAP, table(POSICAO_S, VD)))
chisq.test(HAP.tab_POSICAO)


# CONT.FON.PREC ####
HAP.prop_CONT_FON_PREC <- dados_HAP %>% 
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, CONT_FON_PREC) %>%
  group_by(CONT_FON_PREC) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_CONT_FON_PREC, aes(x = CONT_FON_PREC, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


#CFS_abertura assim como Barbosa (2023)
HAP.prop_CFP_abertura2 <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, CFP_abertura2) %>%
  group_by(CFP_abertura2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_CFP_abertura2, aes(x = CFP_abertura2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))

(HAP.tab_CFP_abertura2 <- with(dados_HAP, table(CFP_abertura2, VD)))
chisq.test(HAP.tab_CFP_abertura2)


# CLASSE MORFOLOGICA ####
HAP.prop_CLASSE_MORFOLOGICA3 <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, CLASSE_MORFOLOGICA3) %>%
  group_by(CLASSE_MORFOLOGICA3) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_CLASSE_MORFOLOGICA3, aes(x = CLASSE_MORFOLOGICA3, y = prop * 100, fill = VD, label = label)) +
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.tab_CLASSE_MORFOLOGICA3 <- with(dados_HAP, table(CLASSE_MORFOLOGICA3, VD)))
chisq.test(HAP.tab_CLASSE_MORFOLOGICA3)

# ESTILO ####
HAP.prop_ESTILO <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, ESTILO) %>%
  group_by(ESTILO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_ESTILO, aes(x = ESTILO, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.tab_ESTILO <- with(dados_HAP, table(ESTILO, VD)))
chisq.test(HAP.tab_ESTILO)


# GENERO ####
HAP.prop_GENERO <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, GENERO) %>%
  group_by(GENERO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_GENERO, aes(x = GENERO, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.tab_GENERO <- with(dados_HAP, table(GENERO, VD)))
chisq.test(HAP.tab_GENERO)


# IDADE DE MIGRACAO ####
HAP.prop_IDADE_MIGRACAO <- dados_HAP %>% 
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, IDADE_MIGRACAO) %>%
  group_by(IDADE_MIGRACAO) %>% 
  mutate(prop = prop.table(n)) %>% 
  print(n = 47)

png("VD_HAP-idade-migracao.png", width = 6.5, height = 4.5, units = "in", res = 300)
ggplot(HAP.prop_IDADE_MIGRACAO[27:47,], aes(x = IDADE_MIGRACAO, y = prop * 100, label = round(prop * 100, 1))) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Idade de Migração", y = "Proporção de Aspiração") +
  #geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  theme_light()
dev.off()

HAP.mod_IDADE_MIGRACAO <- glm(VD ~ IDADE_MIGRACAO, data = dados_HAP, family = binomial)
summary(HAP.mod_IDADE_MIGRACAO)
lrm(VD ~ IDADE_MIGRACAO, data = dados_HAP)

plot(allEffects(HAP.mod_IDADE_MIGRACAO), type = "response")



# TEMPO DE RESIDENCIA ####
HAP.prop_TEMPO_RESIDENCIA <- dados_HAP %>% 
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, TEMPO_RESIDENCIA) %>%
  group_by(TEMPO_RESIDENCIA) %>% 
  mutate(prop = prop.table(n)) %>% 
  print(n = 39)

#png("VD_HAP-10.idade_migracao.png")
ggplot(HAP.prop_TEMPO_RESIDENCIA[24:39,], aes(x = TEMPO_RESIDENCIA, y = prop * 100, label = round(prop * 100, 1))) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  #labs(x = "Idade de Migração", y = "Proporção de Aspiradaização") +
  #geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  theme_light()
#dev.off()

HAP.mod_TEMPO_RESIDENCIA <- glm(VD ~ TEMPO_RESIDENCIA, data = dados_HAP, family = binomial)
summary(HAP.mod_TEMPO_RESIDENCIA)
lrm(VD ~ TEMPO_RESIDENCIA, data = dados_HAP)

plot(allEffects(HAP.mod_TEMPO_RESIDENCIA), type = "response")


# MODELAGEM DE BARBOSA(2023) ####
modHAP1 <- glmer(VD ~ TONICIDADE + 
                  POSICAO_S +
                  CFP_abertura +
                  CLASSE_MORFOLOGICA3 + 
                  GENERO + 
                  TEMPO_RESIDENCIA + 
                  IDADE_MIGRACAO + 
                  (1|ITEM_LEXICAL) +
                  (1|PARTICIPANTE), data = dados_HAP, family = binomial)
summary(modHAP1)
lrm(VD ~ TONICIDADE + 
      POSICAO_S +
      CFP_abertura +
      CLASSE_MORFOLOGICA3 + 
      GENERO + 
      TEMPO_RESIDENCIA + 
      IDADE_MIGRACAO, data = dados_HAP)

car::vif(modHAP1)
check_model(modHAP1)
#check_outliers(modHAP1)


# INDICE SOCIOECONOMICO ####
### Escolaridade ####
HAP.prop_ESCOLARIDADE2 <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, ESCOLARIDADE2) %>%
  group_by(ESCOLARIDADE2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_ESCOLARIDADE2, aes(x = ESCOLARIDADE2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.tab_ESCOLARIDADE2 <- with(dados_HAP, table(ESCOLARIDADE2, VD)))
chisq.test(HAP.tab_ESCOLARIDADE2)
chisq.test(HAP.tab_ESCOLARIDADE2[c(1,3),])

#escolaridade1
#chisq.test(HAP.tab_ESCOLARIDADE[c(1,2),]) #sem diferença pra fund1 e 2
#chisq.test(HAP.tab_ESCOLARIDADE[c(4,5),]) #sem diferença pra superior e pósgrad 
### Escolaridade dos Pais ####
#### Pai ####
HAP.prop_ESCOLA_PAI2 <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, ESCOLA_PAI2) %>%
  group_by(ESCOLA_PAI2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_ESCOLA_PAI2, aes(x = ESCOLA_PAI2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.tab_ESCOLA_PAI2<- with(dados_HAP, table(ESCOLA_PAI2, VD)))
chisq.test(HAP.tab_ESCOLA_PAI2) #sim mas pode estar incorreto
chisq.test(HAP.tab_ESCOLA_PAI2[c(1,4),]) 
chisq.test(HAP.tab_ESCOLA_PAI2[c(1,2),]) 
chisq.test(HAP.tab_ESCOLA_PAI2[c(3,4),]) 

#### Mãe ####
HAP.prop_ESCOLA_MAE2 <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, ESCOLA_MAE2) %>%
  group_by(ESCOLA_MAE2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_ESCOLA_MAE2, aes(x = ESCOLA_MAE2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


HAP.grafico_escolaridade_mae <- HAP.prop_ESCOLA_MAE2 %>% 
  filter(VD == "H") %>% 
  ggplot(aes(x = ESCOLA_MAE2, y = prop * 100, group = VD, color = VD, label = label)) + 
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, vjust = -0.5) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal()


(HAP.tab_ESCOLA_MAE2<- with(dados_HAP, table(ESCOLA_MAE2, VD)))
chisq.test(HAP.tab_ESCOLA_MAE2) #sim
chisq.test(HAP.tab_ESCOLA_MAE2[c(1,2),])
chisq.test(HAP.tab_ESCOLA_MAE2[c(2,3),]) 
chisq.test(HAP.tab_ESCOLA_MAE2[c(1,4),])


### Ocupação ####
HAP.prop_INDICE_OCUPACAO <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, INDICE_OCUPACAO) %>%
  group_by(INDICE_OCUPACAO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_INDICE_OCUPACAO[9:16,], aes(x = INDICE_OCUPACAO, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Índice de Ocupação", y = "Proporção de Aspiradaização") +
  theme_light()

HAP.mod_INDICE_OCUPACAO <- glm(VD ~ INDICE_OCUPACAO, data = dados_HAP, family = binomial)
summary(HAP.mod_INDICE_OCUPACAO)
lrm(VD ~ INDICE_OCUPACAO, data = dados_HAP)
plot(allEffects(HAP.mod_INDICE_OCUPACAO), type = "response")


### Ocupação outro cargo ####
HAP.prop_INDICE_OUTRO_CARGO <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, INDICE_OUTRO_CARGO) %>%
  group_by(INDICE_OUTRO_CARGO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_INDICE_OUTRO_CARGO[10:16,], aes(x = INDICE_OUTRO_CARGO, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  #labs(x = "Índice de Ocupação", y = "Proporção de Aspiradaização") +
  theme_light()


HAP.mod_INDICE_OUTRO_CARGO <- glm(VD ~ INDICE_OUTRO_CARGO, data = dados_HAP, family = binomial)
summary(HAP.mod_INDICE_OUTRO_CARGO)
lrm(VD ~ INDICE_OUTRO_CARGO, data = dados_HAP)
plot(allEffects(HAP.mod_INDICE_OUTRO_CARGO), type = "response")


### Ocupação SONHOS ####
HAP.prop_INDICE_OCUPACAO_SONHOS <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, INDICE_OCUPACAO_SONHOS) %>%
  group_by(INDICE_OCUPACAO_SONHOS) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_INDICE_OCUPACAO_SONHOS[8:14,], aes(x = INDICE_OCUPACAO_SONHOS, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  #labs(x = "Índice de Ocupação", y = "Proporção de Aspiradaização") +
  theme_light()


HAP.mod_INDICE_OCUPACAO_SONHOS <- glm(VD ~ INDICE_OCUPACAO_SONHOS, data = dados_HAP, family = binomial)
summary(HAP.mod_INDICE_OCUPACAO_SONHOS)
lrm(VD ~ INDICE_OCUPACAO_SONHOS, data = dados_HAP)
plot(allEffects(HAP.mod_INDICE_OCUPACAO_SONHOS), type = "response")

### Ocupação distancia ####
HAP.prop_OCUPACAO_DIST <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, OCUPACAO_DIST) %>%
  group_by(OCUPACAO_DIST) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



ggplot(HAP.prop_OCUPACAO_DIST, aes(x = OCUPACAO_DIST, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Individual", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar/Palatal/Aspirada", "Aspirada"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(
      color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(
      color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.tab_OCUPACAO_DIST<- with(dados_HAP, table(OCUPACAO_DIST, VD)))
chisq.test(HAP.tab_OCUPACAO_DIST) #sim
chisq.test(HAP.tab_OCUPACAO_DIST[c(2,3),])

### Ocupação locomoção ####
#analise de locomoção com todos os itens foi transformada na seguinte OCUPACAO_LOCOMOCAO2

HAP.prop_OCUPACAO_LOCOMOCAO2 <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, OCUPACAO_LOCOMOCAO2) %>%
  group_by(OCUPACAO_LOCOMOCAO2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



ggplot(HAP.prop_OCUPACAO_LOCOMOCAO2, aes(x = OCUPACAO_LOCOMOCAO2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Individual", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar/Palatal", "Aspirada"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(
      color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(
      color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.tab_OCUPACAO_LOCOMOCAO2<- with(dados_HAP, table(OCUPACAO_LOCOMOCAO2, VD)))
chisq.test(HAP.tab_OCUPACAO_LOCOMOCAO2) #sim
chisq.test(HAP.tab_OCUPACAO_LOCOMOCAO2[c(1,2),])


### Ocupação dos Pais ####
#### Pai ####
HAP.prop_INDICE_OCUPACAO_PAI <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, INDICE_OCUPACAO_PAI) %>%
  group_by(INDICE_OCUPACAO_PAI) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_INDICE_OCUPACAO_PAI[5:8,], aes(x = INDICE_OCUPACAO_PAI, y = prop * 100, label = round(prop * 100, 1))) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Índice de Ocupação", y = "Proporção de Aspiradaização") +
  theme_light()

HAP.mod_INDICE_OCUPACAO_PAI <- glm(VD ~ INDICE_OCUPACAO_PAI, data = dados_HAP, family = binomial)
summary(HAP.mod_INDICE_OCUPACAO_PAI)
lrm(VD ~ INDICE_OCUPACAO_PAI, data = dados_HAP)
plot(allEffects(HAP.mod_INDICE_OCUPACAO_PAI), type = "response")


#### Mãe ####
HAP.prop_INDICE_OCUPACAO_MAE <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, INDICE_OCUPACAO_MAE) %>%
  group_by(INDICE_OCUPACAO_MAE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_INDICE_OCUPACAO_MAE[6:10,], aes(x = INDICE_OCUPACAO_MAE, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Índice de Ocupação", y = "Proporção de Aspiradaização") +
  theme_light()

HAP.mod_INDICE_OCUPACAO_MAE <- glm(VD ~ INDICE_OCUPACAO_MAE, data = dados_HAP, family = binomial)
summary(HAP.mod_INDICE_OCUPACAO_MAE)
lrm(VD ~ INDICE_OCUPACAO_MAE, data = dados_HAP)
plot(allEffects(HAP.mod_INDICE_OCUPACAO_MAE), type = "response")


### Mega sena ####
HAP.prop_MEGA_SENA2 <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, MEGA_SENA2) %>%
  group_by(MEGA_SENA2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



ggplot(HAP.prop_MEGA_SENA2, aes(x = MEGA_SENA2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Individual", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar/Palatal", "Aspirada"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(
      color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(
      color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.tab_MEGA_SENA2<- with(dados_HAP, table(MEGA_SENA2, VD)))
chisq.test(HAP.tab_MEGA_SENA2) #sim
chisq.test(HAP.tab_MEGA_SENA2[c(1,2),])


### Mega sena Trabalhar ####
HAP.prop_MEGASENA_TRABALHAR2 <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, MEGASENA_TRABALHAR2) %>%
  group_by(MEGASENA_TRABALHAR2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



ggplot(HAP.prop_MEGASENA_TRABALHAR2, aes(x = MEGASENA_TRABALHAR2, y = prop * 100, fill = VD, label = label)) + 
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


(HAP.tab_MEGASENA_TRABALHAR2 <- with(dados_HAP, table(MEGASENA_TRABALHAR2, VD)))
chisq.test(HAP.tab_MEGASENA_TRABALHAR2) #nao


### Renda Individual ####
HAP.prop_RENDA_IND <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora", !is.na(RENDA_IND)) %>% 
  count(VD, RENDA_IND) %>%
  group_by(RENDA_IND) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_RENDA_IND, aes(x = RENDA_IND, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Renda Individual", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar/Palatal", "Aspirada"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.tab_RENDA_IND <- with(dados_HAP, table(RENDA_IND, VD)))
chisq.test(HAP.tab_RENDA_IND) #tem correlação
chisq.test(HAP.tab_RENDA_IND[c(1,2),]) #nao
chisq.test(HAP.tab_RENDA_IND[c(4,5),]) #não
chisq.test(HAP.tab_RENDA_IND[c(2,3),]) #sim
chisq.test(HAP.tab_RENDA_IND[c(1,4),]) #não


### Renda Familiar ####
HAP.prop_RENDA_FAM <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, RENDA_FAM) %>%
  group_by(RENDA_FAM) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_RENDA_FAM, aes(x = RENDA_FAM, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar/Palatal", "Aspirada"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.tab_RENDA_FAM <- with(dados_HAP, table(RENDA_FAM, VD)))
chisq.test(HAP.tab_RENDA_FAM) #tem correlação
chisq.test(HAP.tab_RENDA_FAM[c(1,2),]) #nao
chisq.test(HAP.tab_RENDA_FAM[c(2,3),]) #sim
chisq.test(HAP.tab_RENDA_FAM[c(2,4),]) #não
chisq.test(HAP.tab_RENDA_FAM[c(4,5),]) #sim
chisq.test(HAP.tab_RENDA_FAM[c(1,4),]) #nao


### m2 ####
HAP.prop_media_m2 <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, media_m2) %>%
  group_by(media_m2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print(n = 40)

ggplot(HAP.prop_media_m2[21:40,], aes(x = media_m2, y = prop * 100, label = round(prop * 100, 1))) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Índice de Ocupação", y = "Proporção de Aspiração") +
  theme_light()


HAP.mod_media_m2 <- glm(VD ~ media_m2, data = dados_HAP, family = binomial)
summary(HAP.mod_media_m2)
lrm(VD ~ INDICE_OCUPACAO, data = dados_HAP)
plot(allEffects(HAP.mod_media_m2), type = "response")

### Bairro ####

ordem_bairros <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora", !is.na(media_m2)) %>%
  group_by(BAIRRO) %>%
  summarise(media_geral = mean(media_m2, na.rm = TRUE)) %>%
  arrange(media_geral) %>%  # ou arrange(media_geral) para ordem crescente
  pull(BAIRRO)

HAP.prop_BAIRRO <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora", !is.na(media_m2)) %>% 
  count(VD, BAIRRO, media_m2) %>%
  mutate(BAIRRO = factor(BAIRRO, levels = ordem_bairros)) %>%  # Reordena os níveis
  group_by(BAIRRO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  ungroup()

HAP.prop_BAIRRO %>% 
  ggplot(aes(x = BAIRRO, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar/Palatal", "Aspirada"))+
  coord_flip() +  # Barras horizontais
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.prop_BAIRRO <- with(dados_HAP, table(BAIRRO, VD)))
chisq.test(HAP.prop_BAIRRO)


### Número de Banheiros ####
HAP.prop_NBANHEIROS <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, NBANHEIROS) %>%
  group_by(NBANHEIROS) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

HAP.prop_NBANHEIROS %>% 
  ggplot(aes(x = NBANHEIROS, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Número de Banheiros", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar/Palatal", "Aspirada"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.prop_NBANHEIROS <- with(dados_HAP, table(NBANHEIROS, VD)))
chisq.test(HAP.prop_NBANHEIROS)
chisq.test(HAP.prop_NBANHEIROS[c(1,2)])
chisq.test(HAP.prop_NBANHEIROS[c(2,3)])

### Número de Quartos ####
HAP.prop_NQUARTOS <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, NQUARTOS) %>%
  group_by(NQUARTOS) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

HAP.prop_NQUARTOS %>% 
  ggplot(aes(x = NQUARTOS, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Número de Quartos", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar/Palatal", "Aspirada"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.prop_NQUARTOS <- with(dados_HAP, table(NQUARTOS, VD)))
chisq.test(HAP.prop_NQUARTOS)
chisq.test(HAP.prop_NQUARTOS[c(1,2)])
chisq.test(HAP.prop_NQUARTOS[c(2,3)])

### Tipo Moradia ####
HAP.prop_IMOVEL <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, IMOVEL) %>%
  group_by(IMOVEL) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

HAP.prop_IMOVEL %>% 
  ggplot(aes(x = IMOVEL, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Tipo de Imóvel", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar/Palatal", "Aspirada"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.prop_IMOVEL <- with(dados_HAP, table(IMOVEL, VD)))
chisq.test(HAP.prop_IMOVEL)
chisq.test(HAP.prop_IMOVEL[c(1,2)])
chisq.test(HAP.prop_IMOVEL[c(2,3)])

### Propriedade característica ####
HAP.prop_PROPRIEDADE <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, PROPRIEDADE) %>%
  group_by(PROPRIEDADE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

HAP.prop_PROPRIEDADE %>% 
  ggplot(aes(x = PROPRIEDADE, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar/Palatal", "Aspirada"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.prop_PROPRIEDADE <- with(dados_HAP, table(PROPRIEDADE, VD)))
chisq.test(HAP.prop_PROPRIEDADE)

### Número de Pessoas ####
HAP.prop_NPESSOAS <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, NPESSOAS) %>%
  group_by(NPESSOAS) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

HAP.prop_NPESSOAS %>% 
  ggplot(aes(x = NPESSOAS, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar/Palatal", "Aspirada"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.prop_NPESSOAS <- with(dados_HAP, table(NPESSOAS, VD)))
chisq.test(HAP.prop_NPESSOAS)
chisq.test(HAP.prop_NPESSOAS[c(1,2)])
chisq.test(HAP.prop_NPESSOAS[c(3,4)])
chisq.test(HAP.prop_NPESSOAS[c(5,7)])
chisq.test(HAP.prop_NPESSOAS[c(3,5)])



### Lazer ####
HAP.prop_LAZER_CARACTERISTICA <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, LAZER_CARACTERISTICA) %>%
  group_by(LAZER_CARACTERISTICA) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_LAZER_CARACTERISTICA, aes(x = LAZER_CARACTERISTICA, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar/Palatal", "Aspirada"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.tab_LAZER_CARACTERISTICA <- with(dados_HAP, table(LAZER_CARACTERISTICA, VD)))
chisq.test(HAP.tab_LAZER_CARACTERISTICA) #tem correlação
chisq.test(HAP.tab_LAZER_CARACTERISTICA[c(1,2),])
chisq.test(HAP.tab_LAZER_CARACTERISTICA[c(2,4),])
chisq.test(HAP.tab_LAZER_CARACTERISTICA[c(3,4),])


### Lazer Campinas####
HAP.prop_LAZER_CAMPINAS_CARACTERISTICA <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, LAZER_CAMPINAS_CARACTERISTICA) %>%
  group_by(LAZER_CAMPINAS_CARACTERISTICA) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_LAZER_CAMPINAS_CARACTERISTICA, aes(x = LAZER_CAMPINAS_CARACTERISTICA, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar/Palatal", "Aspirada"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.tab_LAZER_CAMPINAS_CARACTERISTICA <- with(dados_HAP, table(LAZER_CAMPINAS_CARACTERISTICA, VD)))
chisq.test(HAP.tab_LAZER_CAMPINAS_CARACTERISTICA) #tem correlação
chisq.test(HAP.tab_LAZER_CAMPINAS_CARACTERISTICA[c(1,2),])
chisq.test(HAP.tab_LAZER_CAMPINAS_CARACTERISTICA[c(2,5),])
chisq.test(HAP.tab_LAZER_CAMPINAS_CARACTERISTICA[c(3,4),]) #falantes que nfalaram quenão tem e que não sae não tem correlação



### Viagem ####
#costuma viajar?
HAP.prop_VIAGEM <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, VIAGEM) %>%
  group_by(VIAGEM) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_VIAGEM, aes(x = VIAGEM, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar/Palatal", "Aspirada"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.tab_VIAGEM <- with(dados_HAP, table(VIAGEM, VD)))
chisq.test(HAP.tab_VIAGEM) #tem correlação



### Tipo de Viagem ####
HAP.prop_VIAGEM_LUGAR <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, VIAGEM_LUGAR) %>%
  group_by(VIAGEM_LUGAR) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_VIAGEM_LUGAR, aes(x = VIAGEM_LUGAR, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar/Palatal", "Aspirada"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.tab_VIAGEM_LUGAR <- with(dados_HAP, table(VIAGEM_LUGAR, VD)))
chisq.test(HAP.tab_VIAGEM_LUGAR) #tem correlação
chisq.test(HAP.tab_VIAGEM_LUGAR[c(1,2),])
chisq.test(HAP.tab_VIAGEM_LUGAR[c(2,3),])
chisq.test(HAP.tab_VIAGEM_LUGAR[c(3,4),])
chisq.test(HAP.tab_VIAGEM_LUGAR[c(4,5),])


### Viagem vontade ####

HAP.prop_LAZER_VIAGEM_VONTADE2 <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, LAZER_VIAGEM_VONTADE2) %>%
  group_by(LAZER_VIAGEM_VONTADE2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_LAZER_VIAGEM_VONTADE2, aes(x = LAZER_VIAGEM_VONTADE2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar/Palatal", "Aspirada"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.tab_LAZER_VIAGEM_VONTADE2 <- with(dados_HAP, table(LAZER_VIAGEM_VONTADE2, VD)))
chisq.test(HAP.tab_LAZER_VIAGEM_VONTADE2) #tem correlação
chisq.test(HAP.tab_LAZER_VIAGEM_VONTADE2[c(1,2),])
chisq.test(HAP.tab_LAZER_VIAGEM_VONTADE2[c(2,3),])
chisq.test(HAP.tab_LAZER_VIAGEM_VONTADE2[c(3,4),])


### Infancia ####
HAP.prop_INFANCIA_MEMORIA <- dados_HAP %>%
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD, INFANCIA_MEMORIA) %>%
  group_by(INFANCIA_MEMORIA) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_INFANCIA_MEMORIA, aes(x = INFANCIA_MEMORIA, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar/Palatal", "Aspirada"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(HAP.tab_INFANCIA_MEMORIA <- with(dados_HAP, table(INFANCIA_MEMORIA, VD)))
chisq.test(HAP.tab_INFANCIA_MEMORIA) #tem correlação
chisq.test(HAP.tab_LAZER_VIAGEM_VONTADE2[c(3,4),])








