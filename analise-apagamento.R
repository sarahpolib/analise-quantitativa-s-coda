#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%# PROCESSO DE S0AGAMENTO #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 15/07/2025 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% V1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# VD ####
S0.prop_VD <- dados_S0 %>% 
  count(VD) %>%
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_VD, aes(x = VD, y = prop, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  scale_x_discrete(labels = c("Realização", "S0agamento"))+
  geom_text(aes(label = label), vjust = -0.3, size = 4) + 
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
S0.prop_TONICIDADE <- dados_S0 %>%
  count(VD, TONICIDADE) %>%
  group_by(TONICIDADE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_TONICIDADE, aes(x = TONICIDADE, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Proporção de Ocorrência") + 
  scale_x_discrete(labels = c("Átona", "Tônica"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))

(S0.tab_TONICIDADE <- with(dados_S0, table(TONICIDADE, VD)))
chisq.test(S0.tab_TONICIDADE)


# POSICAO ####
S0.prop_POSICAO <- dados_S0 %>%
  count(VD, POSICAO_S) %>%
  group_by(POSICAO_S) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_POSICAO, aes(x = POSICAO_S, y = prop * 100, fill = VD, label = label)) + 
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

(S0.tab_POSICAO <- with(dados_S0, table(POSICAO_S, VD)))
chisq.test(S0.tab_POSICAO)


# CONT.FON.PREC ####
S0.prop_CONT_FON_PREC <- dados_S0 %>% 
  count(VD, CONT_FON_PREC) %>%
  group_by(CONT_FON_PREC) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_CONT_FON_PREC, aes(x = CONT_FON_PREC, y = prop * 100, fill = VD, label = label)) + 
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
S0.prop_CFP_abertura2 <- dados_S0 %>%
  count(VD, CFP_abertura2) %>%
  group_by(CFP_abertura2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_CFP_abertura2, aes(x = CFP_abertura2, y = prop * 100, fill = VD, label = label)) + 
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


(S0.tab_CFP_abertura2 <- with(dados_S0, table(CFP_abertura2, VD)))
chisq.test(S0.tab_CFP_abertura2) # tem diferença entre todos os niveis inclusive 1 e 2

# CONT.FON.SEG ####
S0.prop_CONT_FON_SEG <- dados_S0 %>% 
  count(VD, CONT_FON_SEG) %>%
  group_by(CONT_FON_SEG) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_CONT_FON_SEG, aes(x = CONT_FON_SEG, y = prop * 100, fill = VD, label = label)) + 
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

## CFS_sonoridade de acordo com BARBOSA (2023) ####
S0.prop_CFS_sonoridade<- dados_S0 %>% 
  count(VD, CFS_sonoridade) %>%
  group_by(CFS_sonoridade) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_CFS_sonoridade, aes(x = CFS_sonoridade, y = prop * 100, fill = VD, label = label)) +
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


(S0.tab_CFS_sonoridade <- with(dados_S0, table(CFS_sonoridade, VD)))
chisq.test(S0.tab_CFS_sonoridade)
chisq.test(S0.tab_CFS_sonoridade[c(1,3),])



# CLASSE MORFOLOGICA ####
S0.prop_CLASSE_MORFOLOGICA3 <- dados_S0 %>%
  count(VD, CLASSE_MORFOLOGICA3) %>%
  group_by(CLASSE_MORFOLOGICA3) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_CLASSE_MORFOLOGICA3, aes(x = CLASSE_MORFOLOGICA3, y = prop * 100, fill = VD, label = label)) + 
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


(S0.tab_CLASSE_MORFOLOGICA3 <- with(dados_S0, table(CLASSE_MORFOLOGICA3, VD)))
chisq.test(S0.tab_CLASSE_MORFOLOGICA3)


# ESTILO ####
S0.prop_ESTILO <- dados_S0 %>%
  filter(CFS_pontoc2 == "coronal") %>% 
  count(VD, ESTILO) %>%
  group_by(ESTILO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_ESTILO, aes(x = ESTILO, y = prop * 100, fill = VD, label = label)) + 
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


(S0.tab_ESTILO <- with(dados_S0, table(ESTILO, VD)))
chisq.test(S0.tab_ESTILO)

# GENERO ####
S0.prop_GENERO <- dados_S0 %>%
  count(VD, GENERO) %>%
  group_by(GENERO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_GENERO, aes(x = GENERO, y = prop * 100, fill = VD, label = label)) + 
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


(S0.tab_GENERO <- with(dados_S0, table(GENERO, VD)))
chisq.test(S0.tab_GENERO)


# IDADE DE MIGRACAO ####

S0.prop_IDADE_MIGRACAO <- dados_S0 %>% 
  count(VD, IDADE_MIGRACAO) %>%
  group_by(IDADE_MIGRACAO) %>% 
  mutate(prop = prop.table(n)) %>% 
  print(n = 51)

#png("VD_S0-10.idade_migracao.png")
ggplot(S0.prop_IDADE_MIGRACAO[27:51,], aes(x = IDADE_MIGRACAO, y = prop * 100, label = round(prop * 100, 1))) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  #labs(x = "Idade de Migração", y = "Proporção de Palatalização") +
  #geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  theme_light()
#dev.off()

S0.mod_IDADE_MIGRACAO <- glm(VD ~ IDADE_MIGRACAO, data = dados_S0, family = binomial)
summary(S0.mod_IDADE_MIGRACAO)
lrm(VD ~ IDADE_MIGRACAO, data = dados_S0)

plot(allEffects(S0.mod_IDADE_MIGRACAO), type = "response")



# TEMPO DE RESIDENCIA ####
S0.prop_TEMPO_RESIDENCIA <- dados_S0 %>% 
  count(VD, TEMPO_RESIDENCIA) %>%
  group_by(TEMPO_RESIDENCIA) %>% 
  mutate(prop = prop.table(n)) %>% 
  print(n = 45)

#png("VD_S0-10.idade_migracao.png")
ggplot(S0.prop_TEMPO_RESIDENCIA[24:45,], aes(x = TEMPO_RESIDENCIA, y = prop * 100, label = round(prop * 100, 1))) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  #labs(x = "Idade de Migração", y = "Proporção de Palatalização") +
  #geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  theme_light()
#dev.off()

S0.mod_TEMPO_RESIDENCIA <- glm(VD ~ TEMPO_RESIDENCIA, data = dados_S0, family = binomial)
summary(S0.mod_TEMPO_RESIDENCIA)
lrm(VD ~ TEMPO_RESIDENCIA, data = dados_S0)

plot(allEffects(S0.mod_TEMPO_RESIDENCIA), type = "response")


# MODELAGEM DE BARBOSA(2023) ####
modS01 <- glmer(VD ~ TONICIDADE + 
                  POSICAO_S +
                  CFP_abertura +
                  CFS_sonoridade +
                  CLASSE_MORFOLOGICA3 + 
                  GENERO + 
                  TEMPO_RESIDENCIA + 
                  IDADE_MIGRACAO + 
                  (1|ITEM_LEXICAL) +
                  (1|PARTICIPANTE), data = dados_S0, family = binomial)
summary(modS01)
lrm(VD ~ TONICIDADE + 
      POSICAO_S +
      CFP_abertura +
      CFS_sonoridade +
      CLASSE_MORFOLOGICA3 + 
      GENERO + 
      TEMPO_RESIDENCIA + 
      IDADE_MIGRACAO, data = dados_S0)

car::vif(modS01)
check_model(modS01)
#check_outliers(modS01)