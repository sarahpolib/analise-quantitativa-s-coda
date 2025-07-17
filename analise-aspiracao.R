#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%# PROCESSO DE ASPIRAÇÃO #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 15/07/2025 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% V1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%# PROCESSO DE PALATALIZAÇÃO #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
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
  scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
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
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
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
HAP.prop_VD <- dados_HAP %>% 
  filter(CFS_sonoridade == "sonora") %>% 
  count(VD) %>%
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_VD, aes(x = VD, y = prop, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  scale_x_discrete(labels = c("Alveolar", "Palatal"))+
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
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
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
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
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
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
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
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
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
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
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

#png("VD_HAP-10.idade_migracao.png")
ggplot(HAP.prop_IDADE_MIGRACAO[27:47,], aes(x = IDADE_MIGRACAO, y = prop * 100, label = round(prop * 100, 1))) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  #labs(x = "Idade de Migração", y = "Proporção de Palatalização") +
  #geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  theme_light()
#dev.off()

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
  #labs(x = "Idade de Migração", y = "Proporção de Palatalização") +
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

