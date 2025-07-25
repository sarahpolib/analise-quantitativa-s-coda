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


png("VD_S0-genero.png", width = 6.5, height = 4.5, units = "in", res = 300)
ggplot(S0.prop_GENERO, aes(x = GENERO, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Gênero", y = "Proporção de Ocorrência") + 
  scale_x_discrete(labels = c("Feminino", "Masculino"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variante", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))
dev.off()

(S0.tab_GENERO <- with(dados_S0, table(GENERO, VD)))
chisq.test(S0.tab_GENERO)


# IDADE DE MIGRACAO ####

S0.prop_IDADE_MIGRACAO <- dados_S0 %>% 
  count(VD, IDADE_MIGRACAO) %>%
  group_by(IDADE_MIGRACAO) %>% 
  mutate(prop = prop.table(n)) %>% 
  print(n = 51)

png("VD_S0-idade-migracao.png", width = 6.5, height = 4.5, units = "in", res = 300)
ggplot(S0.prop_IDADE_MIGRACAO[27:51,], aes(x = IDADE_MIGRACAO, y = prop * 100, label = round(prop * 100, 1))) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Idade de Migração", y = "Proporção de Apagamento") +
  #geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  theme_light()
dev.off()

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


# INDICE SOCIOECONOMICO ####
## Variáveis Sociais ####
### Escolaridade ####
AP.prop_ESCOLARIDADE2 <- dados_S0 %>%
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


(AP.tab_ESCOLARIDADE2 <- with(dados_S0, table(ESCOLARIDADE2, VD)))
chisq.test(AP.tab_ESCOLARIDADE2)
chisq.test(AP.tab_ESCOLARIDADE2[c(1,3),])

#escolaridade1
#chisq.test(AP.tab_ESCOLARIDADE[c(1,2),]) #sem diferença pra fund1 e 2
#chisq.test(AP.tab_ESCOLARIDADE[c(4,5),]) #sem diferença pra superior e pósgrad 
### Escolaridade dos Pais ####
#### Pai ####
AP.prop_ESCOLA_PAI2 <- dados_S0 %>%
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


(AP.tab_ESCOLA_PAI2<- with(dados_S0, table(ESCOLA_PAI2, VD)))
chisq.test(AP.tab_ESCOLA_PAI) #sim
chisq.test(AP.tab_ESCOLA_PAI2[c(1,2),]) 
chisq.test(AP.tab_ESCOLA_PAI2[c(2,3),]) 
chisq.test(AP.tab_ESCOLA_PAI2[c(3,4),]) 

#### Mãe ####
AP.prop_ESCOLA_MAE2 <- dados_S0 %>%
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


(AP.tab_ESCOLA_MAE2<- with(dados_S0, table(ESCOLA_MAE2, VD)))
chisq.test(AP.tab_ESCOLA_MAE2) #sim
chisq.test(AP.tab_ESCOLA_MAE2[c(1,2),])
chisq.test(AP.tab_ESCOLA_MAE2[c(2,3),]) 
chisq.test(AP.tab_ESCOLA_PAI[c(3,4),])


### Ocupação ####
AP.prop_INDICE_OCUPACAO <- dados_S0 %>%
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


AP.mod_INDICE_OCUPACAO <- glm(VD ~ INDICE_OCUPACAO, data = dados_S0, family = binomial)
summary(AP.mod_INDICE_OCUPACAO)
lrm(VD ~ INDICE_OCUPACAO, data = dados_S0)
plot(allEffects(AP.mod_INDICE_OCUPACAO), type = "response")


### Ocupação outro cargo ####
AP.prop_INDICE_OUTRO_CARGO <- dados_S0 %>%
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

ggplot(AP.prop_INDICE_OUTRO_CARGO, aes(x = INDICE_OUTRO_CARGO, y = prop * 100, fill = VD, label = label)) +
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


AP.mod_INDICE_OUTRO_CARGO <- glm(VD ~ INDICE_OUTRO_CARGO, data = dados_S0, family = binomial)
summary(AP.mod_INDICE_OUTRO_CARGO)
lrm(VD ~ INDICE_OUTRO_CARGO, data = dados_S0)
plot(allEffects(AP.mod_INDICE_OUTRO_CARGO), type = "response")


### Ocupação SONHOS ####
AP.prop_INDICE_OCUPACAO_SONHOS <- dados_S0 %>%
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


AP.mod_INDICE_OCUPACAO_SONHOS <- glm(VD ~ INDICE_OCUPACAO_SONHOS, data = dados_S0, family = binomial)
summary(AP.mod_INDICE_OCUPACAO_SONHOS)
lrm(VD ~ INDICE_OCUPACAO_SONHOS, data = dados_S0)
plot(allEffects(AP.mod_INDICE_OCUPACAO_SONHOS), type = "response")

### Ocupação distancia ####
AP.prop_OCUPACAO_DIST <- dados_S0 %>%
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


(AP.tab_OCUPACAO_DIST<- with(dados_S0, table(OCUPACAO_DIST, VD)))
chisq.test(AP.tab_OCUPACAO_DIST) #sim
chisq.test(AP.tab_OCUPACAO_DIST[c(2,3),])

### Ocupação locomoção ####
AP.prop_OCUPACAO_LOCOMOCAO <- dados_S0 %>%
  filter(CFS_pontoc2 == "coronal") %>% 
  count(VD, OCUPACAO_LOCOMOCAO) %>%
  group_by(OCUPACAO_LOCOMOCAO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



ggplot(AP.prop_OCUPACAO_LOCOMOCAO, aes(x = OCUPACAO_LOCOMOCAO, y = prop * 100, fill = VD, label = label)) + 
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


(AP.tab_OCUPACAO_DIST<- with(dados_S0, table(OCUPACAO_DIST, VD)))
chisq.test(AP.tab_OCUPACAO_DIST) #sim
chisq.test(AP.tab_OCUPACAO_DIST[c(2,3),])


### Ocupação dos Pais ####
#### Pai ####
AP.prop_INDICE_OCUPACAO_PAI <- dados_S0 %>%
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

AP.mod_INDICE_OCUPACAO_PAI <- glm(VD ~ INDICE_OCUPACAO_PAI, data = dados_S0, family = binomial)
summary(AP.mod_INDICE_OCUPACAO_PAI)
lrm(VD ~ INDICE_OCUPACAO_PAI, data = dados_S0)
plot(allEffects(AP.mod_INDICE_OCUPACAO_PAI), type = "response")


#### Mãe ####
AP.prop_INDICE_OCUPACAO_MAE <- dados_S0 %>%
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

AP.mod_INDICE_OCUPACAO_MAE <- glm(VD ~ INDICE_OCUPACAO_MAE, data = dados_S0, family = binomial)
summary(AP.mod_INDICE_OCUPACAO_MAE)
lrm(VD ~ INDICE_OCUPACAO_MAE, data = dados_S0)
plot(allEffects(AP.mod_INDICE_OCUPACAO_MAE), type = "response")


### Mega sena ####
AP.prop_MEGA_SENA <- dados_S0 %>%
  filter(CFS_pontoc2 == "coronal") %>% 
  count(VD, MEGA_SENA) %>%
  group_by(MEGA_SENA) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



ggplot(AP.prop_MEGA_SENA, aes(x = MEGA_SENA, y = prop * 100, fill = VD, label = label)) + 
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


(AP.tab_MEGA_SENA<- with(dados_S0, table(MEGA_SENA, VD)))
chisq.test(AP.tab_MEGA_SENA) #sim
chisq.test(AP.tab_MEGA_SENA[c(4,3),])

### Renda Individual ####
AP.prop_RENDA_IND <- dados_S0 %>%
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


(AP.tab_RENDA_IND <- with(dados_S0, table(RENDA_IND, VD)))
chisq.test(AP.tab_RENDA_IND) #tem correlação
chisq.test(AP.tab_RENDA_IND[c(1,2),]) #nao
chisq.test(AP.tab_RENDA_IND[c(4,5),]) #sim
chisq.test(AP.tab_RENDA_IND[c(2,4),]) #sim


### Renda Familiar ####
AP.prop_RENDA_FAM <- dados_S0 %>%
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


(AP.tab_RENDA_FAM <- with(dados_S0, table(RENDA_FAM, VD)))
chisq.test(AP.tab_RENDA_FAM) #tem correlação
chisq.test(AP.tab_RENDA_FAM[c(1,2),]) #sim
chisq.test(AP.tab_RENDA_FAM[c(2,3),]) #nao
chisq.test(AP.tab_RENDA_FAM[c(3,4),]) #nao
chisq.test(AP.tab_RENDA_FAM[c(4,5),]) #nao


### m2 ####
AP.prop_media_m2 <- dados_S0 %>%
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


AP.mod_media_m2 <- glm(VD ~ media_m2, data = dados_S0, family = binomial)
summary(AP.mod_media_m2)
lrm(VD ~ INDICE_OCUPACAO, data = dados_S0)
plot(allEffects(AP.mod_media_m2), type = "response")

### Bairro ####

ordem_bairros <- dados_S0 %>%
  filter(CFS_pontoc2 == "coronal", !is.na(media_m2)) %>%
  group_by(BAIRRO) %>%
  summarise(media_geral = mean(media_m2, na.rm = TRUE)) %>%
  arrange(media_geral) %>%  # ou arrange(media_geral) para ordem crescente
  pull(BAIRRO)

AP.prop_BAIRRO <- dados_S0 %>%
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


(AP.prop_BAIRRO <- with(dados_S0, table(BAIRRO, VD)))
chisq.test(AP.prop_BAIRRO)


### Número de Banheiros ####
AP.prop_NBANHEIROS <- dados_S0 %>%
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


(AP.prop_NBANHEIROS <- with(dados_S0, table(NBANHEIROS, VD)))
chisq.test(AP.prop_NBANHEIROS)
chisq.test(AP.prop_NBANHEIROS[c(1,2)])
chisq.test(AP.prop_NBANHEIROS[c(2,3)])

### Número de Quartos ####
AP.prop_NQUARTOS <- dados_S0 %>%
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


(AP.prop_NQUARTOS <- with(dados_S0, table(NQUARTOS, VD)))
chisq.test(AP.prop_NQUARTOS)
chisq.test(AP.prop_NQUARTOS[c(1,2)])
chisq.test(AP.prop_NQUARTOS[c(2,3)])

### Tipo Moradia ####
AP.prop_IMOVEL <- dados_S0 %>%
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


(AP.prop_IMOVEL <- with(dados_S0, table(IMOVEL, VD)))
chisq.test(AP.prop_IMOVEL)
chisq.test(AP.prop_IMOVEL[c(1,2)])
chisq.test(AP.prop_IMOVEL[c(1,3)])

### Propriedade característica ####
AP.prop_PROPRIEDADE <- dados_S0 %>%
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


(AP.prop_PROPRIEDADE <- with(dados_S0, table(PROPRIEDADE, VD)))
chisq.test(AP.prop_PROPRIEDADE)
chisq.test(AP.prop_PROPRIEDADE[c(1,2)])
chisq.test(AP.prop_PROPRIEDADE[c(1,3)])

### Número de Pessoas ####
AP.prop_NPESSOAS <- dados_S0 %>%
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


(AP.prop_NPESSOAS <- with(dados_S0, table(NPESSOAS, VD)))
chisq.test(AP.prop_NPESSOAS)
chisq.test(AP.prop_NPESSOAS[c(1,2)])
chisq.test(AP.prop_NPESSOAS[c(2,3)])

### Lazer ####
AP.prop_LAZER_CARACTERISTICA <- dados_S0 %>%
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


(AP.tab_LAZER_CARACTERISTICA <- with(dados_S0, table(LAZER_CARACTERISTICA, VD)))
chisq.test(AP.tab_LAZER_CARACTERISTICA) #tem correlação
chisq.test(AP.tab_LAZER_CARACTERISTICA[c(1,2),])
chisq.test(AP.tab_LAZER_CARACTERISTICA[c(2,3),])
chisq.test(AP.tab_LAZER_CARACTERISTICA[c(3,4),])


### Lazer Campinas####
AP.prop_LAZER_CAMPINAS_CARACTERISTICA <- dados_S0 %>%
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


(AP.tab_LAZER_CAMPINAS_CARACTERISTICA <- with(dados_S0, table(LAZER_CAMPINAS_CARACTERISTICA, VD)))
chisq.test(AP.tab_LAZER_CAMPINAS_CARACTERISTICA) #tem correlação
chisq.test(AP.tab_LAZER_CAMPINAS_CARACTERISTICA[c(1,2),])
chisq.test(AP.tab_LAZER_CAMPINAS_CARACTERISTICA[c(2,3),])
chisq.test(AP.tab_LAZER_CAMPINAS_CARACTERISTICA[c(3,4),]) #falantes que nfalaram quenão tem e que não sae não tem correlação



### Viagem ####
#costuma viajar?
AP.prop_VIAGEM <- dados_S0 %>%
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


(AP.tab_VIAGEM <- with(dados_S0, table(VIAGEM, VD)))
chisq.test(AP.tab_VIAGEM) #tem correlação



### Tipo de Viagem ####

AP.prop_VIAGEM_LUGAR <- dados_S0 %>%
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


(AP.tab_VIAGEM_LUGAR <- with(dados_S0, table(VIAGEM_LUGAR, VD)))
chisq.test(AP.tab_VIAGEM_LUGAR) #tem correlação
chisq.test(AP.tab_VIAGEM_LUGAR[c(2,3),])


### Viagem vontade ####

AP.prop_LAZER_VIAGEM_VONTADE2 <- dados_S0 %>%
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


(AP.tab_LAZER_VIAGEM_VONTADE2 <- with(dados_S0, table(LAZER_VIAGEM_VONTADE2, VD)))
chisq.test(AP.tab_LAZER_VIAGEM_VONTADE2) #tem correlação
chisq.test(AP.tab_LAZER_VIAGEM_VONTADE2[c(1,2),])
chisq.test(AP.tab_LAZER_VIAGEM_VONTADE2[c(3,4),])


### Infancia ####
AP.prop_INFANCIA_MEMORIA <- dados_S0 %>%
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


(AP.tab_LAZER_VIAGEM_VONTADE2 <- with(dados_S0, table(LAZER_VIAGEM_VONTADE2, VD)))
chisq.test(AP.tab_LAZER_VIAGEM_VONTADE2) #tem correlação







