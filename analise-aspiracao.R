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
  labs(x = "Variável Resposta", y = "Proporção de Ocorrência", fill = "VD") + 
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
# filtragem foi feita no arquivo carregar-amostras. Dados mostram envelope de variação semelhante ao de Barbosa 2023, ou seja, ocorrência de aspiração se dá só quando seguida por consoante sonora
# HAP.prop_CONT_FON_SEG <- dados_HAP %>% 
#   count(VD, CONT_FON_SEG) %>%
#   group_by(CONT_FON_SEG) %>% 
#   mutate(prop = prop.table(n),
#          label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
#   print()
# 
# ggplot(HAP.prop_CONT_FON_SEG, aes(x = CONT_FON_SEG, y = prop * 100, fill = VD, label = label)) + 
#   geom_bar(stat = "identity", color = "white") + 
#   labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
#   #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
#   geom_text(size = 3, position = position_stack(vjust = 0.5)) +
#   scale_fill_brewer(palette = "Reds")+
#   theme_minimal()+
#   theme(
#     panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
#     panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
#     axis.title.x = element_text(size = 9),  # tamanho do título eixo X
#     axis.title.y = element_text(size = 9))
# 
# #CFS_sonoridade de acordo com BARBOSA (2023)
# HAP.prop_CFS_sonoridade <- dados_HAP %>% 
#   count(VD, CFS_sonoridade) %>%
#   group_by(CFS_sonoridade) %>% 
#   mutate(prop = prop.table(n),
#          label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
#   print()
# 
# ggplot(HAP.prop_CFS_sonoridade, aes(x = CFS_sonoridade, y = prop * 100, fill = VD, label = label)) + 
#   geom_bar(stat = "identity", color = "white") + 
#   #labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
#   #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
#   geom_text(size = 3, position = position_stack(vjust = 0.5)) +
#   scale_fill_brewer(palette = "Reds")+
#   theme_minimal()+
#   theme(
#     panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
#     panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
#     axis.title.x = element_text(size = 9),  # tamanho do título eixo X
#     axis.title.y = element_text(size = 9))


# VD ####
HAP.prop_VD <- dados_HAP %>% 
  count(VD) %>%
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()


png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/HAP/1HAP_vd.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(HAP.prop_VD, aes(x = VD, y = prop, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
  scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(aes(label = label), vjust = -0.3, size = 4) + 
  scale_fill_brewer(palette = "Reds")+
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     expand = expansion(mult = c(0, 0.18))) + #aumenta espaço no topo
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    legend.position = "none")
dev.off()

## Por participante ####
HAP.participante <- dados_HAP %>% 
  count(PARTICIPANTE, VD) %>%
  group_by(PARTICIPANTE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>%
  print()


#png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/VD-participante.png", width = 6.5, height = 4.5, units = "in", res = 300)
HAP.participante %>%   
  ggplot(aes(x = VD, y = prop, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Resposta", y = "Proporção de Ocorrência", fill = "VD") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(aes(label = label), 
            vjust = -0.2,
            size = 3.5) +
  facet_wrap(. ~ PARTICIPANTE)+
  scale_fill_brewer(palette = "Reds")+
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     expand = expansion(mult = c(0, 0.9))) + #espaço no topo para texto
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9),   # tamanho do título eixo Y
    legend.position = "bottom")
#dev.off()


# TONICIDADE ####
HAP.prop_TONICIDADE <- dados_HAP %>%
  count(VD, TONICIDADE) %>%
  group_by(TONICIDADE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()


png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/HAP/2HAP_tonicidade.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(HAP.prop_TONICIDADE, aes(x = TONICIDADE, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Tonicidade", y = "Proporção de Ocorrência") + 
  scale_x_discrete(labels = c("Átona", "Tônica"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variável \nResposta", labels = c("Alveolar/Palatal", "Aspiração"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25))
dev.off()


(HAP.tab_TONICIDADE <- with(dados_HAP, table(TONICIDADE, VD)))
chisq.test(HAP.tab_TONICIDADE)


# POSICAO ####
HAP.prop_POSICAO <- dados_HAP %>%
  count(VD, POSICAO_S) %>%
  group_by(POSICAO_S) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_POSICAO, aes(x = POSICAO_S, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
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
  count(VD, CONT_FON_PREC) %>%
  group_by(CONT_FON_PREC) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_CONT_FON_PREC, aes(x = CONT_FON_PREC, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
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
  count(VD, CFP_abertura2) %>%
  group_by(CFP_abertura2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_CFP_abertura2, aes(x = CFP_abertura2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
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
  count(VD, CLASSE_MORFOLOGICA3) %>%
  group_by(CLASSE_MORFOLOGICA3) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_CLASSE_MORFOLOGICA3, aes(x = CLASSE_MORFOLOGICA3, y = prop * 100, fill = VD, label = label)) +
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
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
  count(VD, ESTILO) %>%
  group_by(ESTILO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_ESTILO, aes(x = ESTILO, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
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
  count(VD, GENERO) %>%
  group_by(GENERO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_GENERO, aes(x = GENERO, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
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



# TEMPO DE RESIDENCIA ####
HAP.prop_TEMPO_RESIDENCIA <- dados_HAP %>% 
  count(VD, TEMPO_RESIDENCIA) %>%
  group_by(TEMPO_RESIDENCIA) %>% 
  mutate(prop = prop.table(n)) %>% 
  print(n = 39)

png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/HAP/3HAP_tempo_residencia.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(HAP.prop_TEMPO_RESIDENCIA[24:39,], aes(x = TEMPO_RESIDENCIA, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Tempo de Residência", y = "Proporção de Aspiração") +
  #geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  theme_light()
dev.off()

HAP.mod_TEMPO_RESIDENCIA <- glm(VD ~ TEMPO_RESIDENCIA, data = dados_HAP, family = binomial)
summary(HAP.mod_TEMPO_RESIDENCIA)
lrm(VD ~ TEMPO_RESIDENCIA, data = dados_HAP)
plot(allEffects(HAP.mod_TEMPO_RESIDENCIA), type = "response")



# IDADE DE MIGRACAO ####
HAP.prop_IDADE_MIGRACAO <- dados_HAP %>% 
  count(VD, IDADE_MIGRACAO) %>%
  group_by(IDADE_MIGRACAO) %>% 
  mutate(prop = prop.table(n)) %>% 
  print(n = 47)

png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/HAP/4HAP_idade_migracao.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(HAP.prop_IDADE_MIGRACAO[27:47,], aes(x = IDADE_MIGRACAO, y = prop * 100)) + 
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




# INDICE SOCIO POLI ####
HAP.prop_INDICE_SOCIO_POLI <- dados_HAP %>% 
  count(VD, INDICE_SOCIO_POLI) %>%
  group_by(INDICE_SOCIO_POLI) %>% 
  mutate(prop = prop.table(n)) %>% 
  print(n = 92)

png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/HAP/5HAP_indicesocio.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(HAP.prop_INDICE_SOCIO_POLI[46:77,], aes(x = INDICE_SOCIO_POLI, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Índice Socioeconômico", y = "Proporção de Aspiração") +
  theme_light()
dev.off()

HAP.mod_INDICE_SOCIO_POLI <- glm(VD ~ INDICE_SOCIO_POLI, data = dados_HAP, family = binomial)
summary(HAP.mod_INDICE_SOCIO_POLI)
lrm(VD ~ INDICE_SOCIO_POLI, data = dados_HAP)
plot(allEffects(HAP.mod_INDICE_SOCIO_POLI), type = "response") 



# 1 MODELAGEM DE BARBOSA(2023) ####
modHAP1 <- glmer(VD ~ TONICIDADE + 
                  POSICAO_S +
                  CFP_abertura2 +
                  CLASSE_MORFOLOGICA3 + 
                  GENERO + 
                  TEMPO_RESIDENCIA + 
                  IDADE_MIGRACAO + 
                  (1|ITEM_LEXICAL) +
                  (1|PARTICIPANTE), data = dados_HAP, family = binomial)
summary(modHAP1)
lrm(VD ~ TONICIDADE + 
      POSICAO_S +
      CFP_abertura2 +
      CLASSE_MORFOLOGICA3 + 
      GENERO + 
      TEMPO_RESIDENCIA + 
      IDADE_MIGRACAO, data = dados_HAP)

car::vif(modHAP1)
check_model(modHAP1)
#check_outliers(modHAP1)
r.squaredGLMM(modHAP1)



addmargins(table(dados_HAP$TONICIDADE, dados_HAP$VD))
addmargins(table(dados_HAP$POSICAO_S, dados_HAP$VD))
addmargins(table(dados_HAP$CFP_abertura2, dados_HAP$VD))
addmargins(table(dados_HAP$CLASSE_MORFOLOGICA3, dados_HAP$VD))
addmargins(table(dados_HAP$GENERO, dados_HAP$VD))


# 2 MODELAGEM DE POLI INDICE SOCIO OUSHIRO ####
modHAP2 <- glmer(VD ~ TONICIDADE + 
                  POSICAO_S +
                  CFP_abertura2 +
                  CLASSE_MORFOLOGICA3 + 
                  GENERO + 
                  TEMPO_RESIDENCIA + 
                  IDADE_MIGRACAO +
                  INDICE_SOCIO_OUSHIRO +
                  (1|ITEM_LEXICAL) +
                  (1|PARTICIPANTE), data = dados_HAP, family = binomial)
summary(modHAP2)
lrm(VD ~ TONICIDADE + 
      POSICAO_S +
      CFP_abertura2 +
      CLASSE_MORFOLOGICA3 + 
      GENERO + 
      TEMPO_RESIDENCIA + 
      IDADE_MIGRACAO +
      INDICE_SOCIO_OUSHIRO, data = dados_HAP)

car::vif(modHAP2)
check_model(modHAP2)
check_outliers(modHAP2)
r.squaredGLMM(modHAP2)


# 3 MODELAGEM DE POLI INDICE SOCIO POLI ####
modHAP3 <- glmer(VD ~ TONICIDADE + 
                  POSICAO_S +
                  CFP_abertura2 +
                  CLASSE_MORFOLOGICA3 + 
                  GENERO + 
                  TEMPO_RESIDENCIA + 
                  IDADE_MIGRACAO +
                  INDICE_SOCIO_POLI +
                  (1|ITEM_LEXICAL) +
                  (1|PARTICIPANTE), data = dados_HAP, family = binomial)
summary(modHAP3)
lrm(VD ~ TONICIDADE + 
      POSICAO_S +
      CFP_abertura2 +
      CLASSE_MORFOLOGICA3 + 
      GENERO + 
      TEMPO_RESIDENCIA + 
      IDADE_MIGRACAO +
      INDICE_SOCIO_POLI, data = dados_HAP)

car::vif(modHAP3)
check_model(modHAP3)
check_outliers(modHAP3)
r.squaredGLMM(modHAP3)


compare_performance(modHAP2, modHAP3)


# 4 MODELAGEM DE POLI INDICE SOCIO POLI sem CFS_abertura e classe ###
modHAP4 <- glmer(VD ~ TONICIDADE + 
                   POSICAO_S +
                   #CFP_abertura2 +
                   #CLASSE_MORFOLOGICA3 + 
                   GENERO + 
                   TEMPO_RESIDENCIA + 
                   IDADE_MIGRACAO +
                   INDICE_SOCIO_POLI +
                   (1|ITEM_LEXICAL) +
                   (1|PARTICIPANTE), data = dados_HAP, family = binomial)
summary(modHAP4)
lrm(VD ~ TONICIDADE + 
      POSICAO_S +
      GENERO + 
      TEMPO_RESIDENCIA + 
      IDADE_MIGRACAO +
      INDICE_SOCIO_POLI, data = dados_HAP)

car::vif(modHAP4)
check_model(modHAP4)
check_outliers(modHAP4)
r.squaredGLMM(modHAP4)


## interações ####
summary(glm(VD ~ IDADE_MIGRACAO * INDICE_SOCIO_POLI, data = dados_HAP, family = binomial))
summary(glm(VD ~ IDADE_MIGRACAO * TEMPO_RESIDENCIA, data = dados_HAP, family = binomial))
summary(glm(VD ~ TEMPO_RESIDENCIA * INDICE_SOCIO_POLI, data = dados_HAP, family = binomial))
summary(glm(VD ~ TEMPO_RESIDENCIA * INDICE_SOCIO_OUSHIRO, data = dados_HAP, family = binomial))
summary(glm(VD ~ IDADE_MIGRACAO * MEGA_SENA2, data = dados_HAP, family = binomial))


# INDICE SOCIOECONOMICO ####
### Escolaridade ####
HAP.prop_ESCOLARIDADE2 <- dados_HAP %>%
  count(VD, ESCOLARIDADE2) %>%
  group_by(ESCOLARIDADE2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_ESCOLARIDADE2, aes(x = ESCOLARIDADE2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
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


#teste efeitos mistos
HAP.mod_ESCOLARIDADE2 <- glmer(VD ~ ESCOLARIDADE2 +
                                   (1|ITEM_LEXICAL) +
                                   (1|PARTICIPANTE), 
                                 data = dados_HAP, family = binomial)
summary(HAP.mod_ESCOLARIDADE2)
lrm(VD ~ ESCOLARIDADE2, data = dados_HAP)
plot(allEffects(HAP.mod_ESCOLARIDADE2), type = "response")



#escolaridade1
#chisq.test(HAP.tab_ESCOLARIDADE[c(1,2),]) #sem diferença pra fund1 e 2
#chisq.test(HAP.tab_ESCOLARIDADE[c(4,5),]) #sem diferença pra superior e pósgrad 
### Escolaridade dos Pais ####
#### Pai ####
HAP.prop_ESCOLA_PAI2 <- dados_HAP %>%
  count(VD, ESCOLA_PAI2) %>%
  group_by(ESCOLA_PAI2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_ESCOLA_PAI2, aes(x = ESCOLA_PAI2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
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

#teste efeitos mistos
HAP.mod_ESCOLA_PAI2  <- glmer(VD ~ ESCOLA_PAI2 +
                                 (1|ITEM_LEXICAL) +
                                 (1|PARTICIPANTE), 
                               data = dados_HAP, family = binomial)
summary(HAP.mod_ESCOLA_PAI2)
lrm(VD ~ ESCOLA_PAI2, data = dados_HAP)
plot(allEffects(HAP.mod_ESCOLA_PAI2), type = "response")



#### Mãe ####
HAP.prop_ESCOLA_MAE2 <- dados_HAP %>%
  count(VD, ESCOLA_MAE2) %>%
  group_by(ESCOLA_MAE2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_ESCOLA_MAE2, aes(x = ESCOLA_MAE2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
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
  #labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar/Palatal", "Aspirada"))+
  geom_text(size = 3, vjust = -0.5) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal()
HAP.grafico_escolaridade_mae

(HAP.tab_ESCOLA_MAE2<- with(dados_HAP, table(ESCOLA_MAE2, VD)))
chisq.test(HAP.tab_ESCOLA_MAE2) #sim
chisq.test(HAP.tab_ESCOLA_MAE2[c(1,2),])


#teste efeitos mistos
HAP.mod_ESCOLA_MAE2 <- glmer(VD ~ ESCOLA_MAE2 +
                                (1|ITEM_LEXICAL) +
                                (1|PARTICIPANTE), 
                              data = dados_HAP, family = binomial)
summary(HAP.mod_ESCOLA_MAE2)
lrm(VD ~ ESCOLA_MAE2, data = dados_HAP)
plot(allEffects(HAP.mod_ESCOLA_MAE2), type = "response")

### Ocupação ####
HAP.prop_INDICE_OCUPACAO <- dados_HAP %>%
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

#teste efeitos mistos
HAP.mod_INDICE_OCUPACAO <- glmer(VD ~ INDICE_OCUPACAO+
                                   (1|ITEM_LEXICAL)+
                                   (1|PARTICIPANTE), 
                                 data = dados_HAP, family = binomial)
summary(HAP.mod_INDICE_OCUPACAO)
lrm(VD ~ INDICE_OCUPACAO, data = dados_HAP)
plot(allEffects(HAP.mod_INDICE_OCUPACAO), type = "response")


### Ocupação outro cargo ####
HAP.prop_INDICE_OUTRO_CARGO <- dados_HAP %>%
  count(VD, INDICE_OUTRO_CARGO) %>%
  group_by(INDICE_OUTRO_CARGO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()


HAP.mod_INDICE_OUTRO_CARGO <- glmer(VD ~ INDICE_OUTRO_CARGO+
                                      (1|ITEM_LEXICAL)+
                                      (1|PARTICIPANTE), 
                                    data = dados_HAP, family = binomial)
summary(HAP.mod_INDICE_OUTRO_CARGO)
lrm(VD ~ INDICE_OUTRO_CARGO, data = dados_HAP)
plot(allEffects(HAP.mod_INDICE_OUTRO_CARGO), type = "response")


### Ocupação SONHOS ####
HAP.prop_INDICE_OCUPACAO_SONHOS <- dados_HAP %>%
  count(VD, INDICE_OCUPACAO_SONHOS) %>%
  group_by(INDICE_OCUPACAO_SONHOS) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()


HAP.mod_INDICE_OCUPACAO_SONHOS <- glmer(VD ~ INDICE_OCUPACAO_SONHOS+
                                          (1|ITEM_LEXICAL)+
                                          (1|PARTICIPANTE),
                                        data = dados_HAP, family = binomial)
summary(HAP.mod_INDICE_OCUPACAO_SONHOS)
lrm(VD ~ INDICE_OCUPACAO_SONHOS, data = dados_HAP)
plot(allEffects(HAP.mod_INDICE_OCUPACAO_SONHOS), type = "response")

### Ocupação distancia ####
HAP.prop_OCUPACAO_DIST <- dados_HAP %>%
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



#teste efeitos mistos
HAP.mod_OCUPACAO_DIST <- glmer(VD ~ OCUPACAO_DIST+
                                   (1|ITEM_LEXICAL)+
                                   (1|PARTICIPANTE), 
                                 data = dados_HAP, family = binomial)
summary(HAP.mod_OCUPACAO_DIST)
lrm(VD ~ OCUPACAO_DIST, data = dados_HAP)
plot(allEffects(HAP.mod_OCUPACAO_DIST), type = "response")

### Ocupação locomoção ####
#analise de locomoção com todos os itens foi transformada na seguinte OCUPACAO_LOCOMOCAO2

HAP.prop_OCUPACAO_LOCOMOCAO2 <- dados_HAP %>%
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



#teste efeitos mistos
HAP.mod_OCUPACAO_LOCOMOCAO2 <- glmer(VD ~ OCUPACAO_LOCOMOCAO2+
                                   (1|ITEM_LEXICAL)+
                                   (1|PARTICIPANTE), 
                                 data = dados_HAP, family = binomial)
summary(HAP.mod_OCUPACAO_LOCOMOCAO2)
lrm(VD ~ OCUPACAO_LOCOMOCAO2, data = dados_HAP)
plot(allEffects(HAP.mod_OCUPACAO_LOCOMOCAO2), type = "response")

### Ocupação dos Pais ####
#### Pai ####
HAP.prop_INDICE_OCUPACAO_PAI <- dados_HAP %>%
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

HAP.mod_INDICE_OCUPACAO_PAI <- glmer(VD ~ INDICE_OCUPACAO_PAI+
                                       (1|ITEM_LEXICAL)+
                                       (1|PARTICIPANTE), data = dados_HAP, family = binomial)
summary(HAP.mod_INDICE_OCUPACAO_PAI)
lrm(VD ~ INDICE_OCUPACAO_PAI, data = dados_HAP)
plot(allEffects(HAP.mod_INDICE_OCUPACAO_PAI), type = "response")


#### Mãe ####
HAP.prop_INDICE_OCUPACAO_MAE <- dados_HAP %>%
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

HAP.mod_INDICE_OCUPACAO_MAE <- glmer(VD ~ INDICE_OCUPACAO_MAE+
                                       (1|ITEM_LEXICAL)+
                                       (1|PARTICIPANTE), 
                                     data = dados_HAP, family = binomial)
summary(HAP.mod_INDICE_OCUPACAO_MAE)
lrm(VD ~ INDICE_OCUPACAO_MAE, data = dados_HAP)
plot(allEffects(HAP.mod_INDICE_OCUPACAO_MAE), type = "response")


### Mega sena ####
HAP.prop_MEGA_SENA2 <- dados_HAP %>%
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

#teste efeitos mistos
HAP.mod_MEGA_SENA2 <- glmer(VD ~ MEGA_SENA2+
                                     (1|ITEM_LEXICAL)+
                                     (1|PARTICIPANTE), 
                                   data = dados_HAP, family = binomial)
summary(HAP.mod_MEGA_SENA2)
lrm(VD ~ MEGA_SENA2, data = dados_HAP)
plot(allEffects(HAP.mod_MEGA_SENA2), type = "response")(1|PARTICIPANTE)


### Mega sena Trabalhar ####
HAP.prop_MEGASENA_TRABALHAR2 <- dados_HAP %>%
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


#teste efeitos mistos
HAP.mod_MEGASENA_TRABALHAR2 <- glmer(VD ~ MEGASENA_TRABALHAR2+
                                   (1|ITEM_LEXICAL)+
                                   (1|PARTICIPANTE), 
                                 data = dados_HAP, family = binomial)
summary(HAP.mod_MEGASENA_TRABALHAR2)
lrm(VD ~ MEGASENA_TRABALHAR2, data = dados_HAP)
plot(allEffects(HAP.mod_MEGASENA_TRABALHAR2), type = "response")



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
chisq.test(HAP.tab_RENDA_IND[c(1,2),])


#teste efeitos mistos
HAP.mod_RENDA_IND <- glmer(VD ~ RENDA_IND+
                                   (1|ITEM_LEXICAL)+
                                   (1|PARTICIPANTE), 
                                 data = dados_HAP, family = binomial)
summary(HAP.mod_RENDA_IND)
lrm(VD ~ RENDA_IND, data = dados_HAP)
plot(allEffects(HAP.mod_RENDA_IND), type = "response")



### Renda Familiar ####
HAP.prop_RENDA_FAM <- dados_HAP %>%
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


#teste efeitos mistos
HAP.mod_RENDA_FAM <- glmer(VD ~ RENDA_FAM+
                             (1|ITEM_LEXICAL)+
                             (1|PARTICIPANTE), 
                           data = dados_HAP, family = binomial)
summary(HAP.mod_RENDA_FAM)
lrm(VD ~ RENDA_FAM, data = dados_HAP)
plot(allEffects(HAP.mod_RENDA_FAM), type = "response")

### m2 ####
HAP.prop_media_m2 <- dados_HAP %>%
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


HAP.mod_media_m2 <- glmer(VD ~ media_m2+
                            (1|ITEM_LEXICAL)+
                            (1|PARTICIPANTE), 
                          data = dados_HAP, family = binomial)
summary(HAP.mod_media_m2)
lrm(VD ~ media_m2, data = dados_HAP)
plot(allEffects(HAP.mod_media_m2), type = "response")

### Bairro ####

ordem_bairros <- dados_HAP %>%
  group_by(BAIRRO) %>%
  summarise(media_geral = mean(media_m2, na.rm = TRUE)) %>%
  arrange(media_geral) %>%  # ou arrange(media_geral) para ordem crescente
  pull(BAIRRO)

HAP.prop_BAIRRO <- dados_HAP %>%
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


#### Região ####
HAP.prop_BAIRRO_REGIAO <- dados_HAP %>%
  count(VD, BAIRRO_REGIAO2) %>%
  group_by(BAIRRO_REGIAO2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(HAP.prop_BAIRRO_REGIAO, aes(x = BAIRRO_REGIAO2, y = prop * 100, fill = VD, label = label)) + 
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


(HAP.tab_BAIRRO_REGIAO <- with(dados_HAP, table(BAIRRO_REGIAO, VD)))
chisq.test(HAP.tab_BAIRRO_REGIAO) #tem correlação
chisq.test(HAP.tab_BAIRRO_REGIAO[c(1,3),])


#teste efeitos mistos
HAP.mod_BAIRRO_REGIAO <- glmer(VD ~ BAIRRO_REGIAO2 +
                                (1|ITEM_LEXICAL) +
                                (1|PARTICIPANTE),
                              data = dados_HAP, family = binomial)
summary(HAP.mod_BAIRRO_REGIAO)
lrm(VD ~ BAIRRO_REGIAO, data = dados_HAP)
plot(allEffects(HAP.mod_BAIRRO_REGIAO), type = "response")



### Número de Banheiros ####
HAP.prop_NBANHEIROS <- dados_HAP %>%
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

#teste efeitos mistos
HAP.mod_NBANHEIROS <- glmer(VD ~ NBANHEIROS+
                             (1|ITEM_LEXICAL)+
                             (1|PARTICIPANTE), 
                           data = dados_HAP, family = binomial)
summary(HAP.mod_NBANHEIROS)
lrm(VD ~ NBANHEIROS, data = dados_HAP)
plot(allEffects(HAP.mod_NBANHEIROS), type = "response")



### Número de Quartos ####
HAP.prop_NQUARTOS <- dados_HAP %>%
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


#teste efeitos mistos
HAP.mod_NQUARTOS <- glmer(VD ~ NQUARTOS+
                              (1|ITEM_LEXICAL)+
                              (1|PARTICIPANTE), 
                            data = dados_HAP, family = binomial)
summary(HAP.mod_NQUARTOS)
lrm(VD ~ NQUARTOS, data = dados_HAP)
plot(allEffects(HAP.mod_NQUARTOS), type = "response")



### Densidade ####
#teste efeitos mistos
HAP.mod_DENSIDADE_HABITACAO <- glmer(VD ~ DENSIDADE_HABITACAO +
                                      (1|ITEM_LEXICAL) +
                                      (1|PARTICIPANTE), data = dados_HAP, family = binomial)
summary(HAP.mod_DENSIDADE_HABITACAO)
lrm(VD ~ DENSIDADE_HABITACAO, data = dados_HAP)
plot(allEffects(HAP.mod_DENSIDADE_HABITACAO), type = "response")


### Tipo Moradia ####
HAP.prop_IMOVEL <- dados_HAP %>%
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

#teste efeitos mistos
HAP.mod_IMOVEL <- glmer(VD ~ IMOVEL+
                              (1|ITEM_LEXICAL)+
                              (1|PARTICIPANTE), 
                            data = dados_HAP, family = binomial)
summary(HAP.mod_IMOVEL)
lrm(VD ~ IMOVEL, data = dados_HAP)
plot(allEffects(HAP.mod_IMOVEL), type = "response")


### Propriedade característica ####
HAP.prop_PROPRIEDADE <- dados_HAP %>%
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

#teste efeitos mistos
HAP.mod_PROPRIEDADE <- glmer(VD ~ PROPRIEDADE+
                              (1|ITEM_LEXICAL)+
                              (1|PARTICIPANTE), 
                            data = dados_HAP, family = binomial)
summary(HAP.mod_PROPRIEDADE)
lrm(VD ~ PROPRIEDADE, data = dados_HAP)
plot(allEffects(HAP.mod_PROPRIEDADE), type = "response")


### Número de Pessoas ####
HAP.prop_NPESSOAS <- dados_HAP %>%
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

#teste efeitos mistos
HAP.mod_NPESSOAS <- glmer(VD ~ NPESSOAS+
                              (1|ITEM_LEXICAL)+
                              (1|PARTICIPANTE), 
                            data = dados_HAP, family = binomial)
summary(HAP.mod_NPESSOAS)
lrm(VD ~ NPESSOAS, data = dados_HAP)
plot(allEffects(HAP.mod_NPESSOAS), type = "response")



### Lazer ####
HAP.prop_LAZER_CARACTERISTICA <- dados_HAP %>%
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


#teste efeitos mistos
HAP.mod_LAZER_CARACTERISTICA <- glmer(VD ~ LAZER_CARACTERISTICA+
                            (1|ITEM_LEXICAL)+
                            (1|PARTICIPANTE), 
                          data = dados_HAP, family = binomial)
summary(HAP.mod_LAZER_CARACTERISTICA)
lrm(VD ~ LAZER_CARACTERISTICA, data = dados_HAP)
plot(allEffects(HAP.mod_LAZER_CARACTERISTICA), type = "response")


### Lazer Campinas####
HAP.prop_LAZER_CAMPINAS_CARACTERISTICA <- dados_HAP %>%
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

#teste efeitos mistos
HAP.mod_LAZER_CAMPINAS_CARACTERISTICA <- glmer(VD ~ LAZER_CAMPINAS_CARACTERISTICA+
                            (1|ITEM_LEXICAL)+
                            (1|PARTICIPANTE), 
                          data = dados_HAP, family = binomial)
summary(HAP.mod_LAZER_CAMPINAS_CARACTERISTICA)
lrm(VD ~ LAZER_CAMPINAS_CARACTERISTICA, data = dados_HAP)
plot(allEffects(HAP.mod_LAZER_CAMPINAS_CARACTERISTICA), type = "response")


### Viagem ####
#costuma viajar?
HAP.prop_VIAGEM <- dados_HAP %>%
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

#teste efeitos mistos
HAP.mod_VIAGEM <- glmer(VD ~ VIAGEM+
                            (1|ITEM_LEXICAL)+
                            (1|PARTICIPANTE), 
                          data = dados_HAP, family = binomial)
summary(HAP.mod_VIAGEM)
lrm(VD ~ VIAGEM, data = dados_HAP)
plot(allEffects(HAP.mod_VIAGEM), type = "response")


### Tipo de Viagem ####
HAP.prop_VIAGEM_LUGAR <- dados_HAP %>%
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

#teste efeitos mistos
HAP.mod_VIAGEM_LUGAR <- glmer(VD ~ VIAGEM_LUGAR+
                            (1|ITEM_LEXICAL)+
                            (1|PARTICIPANTE), 
                          data = dados_HAP, family = binomial)
summary(HAP.mod_VIAGEM_LUGAR)
lrm(VD ~ VIAGEM_LUGAR, data = dados_HAP)
plot(allEffects(HAP.mod_VIAGEM_LUGAR), type = "response")

### Viagem vontade ####

HAP.prop_LAZER_VIAGEM_VONTADE2 <- dados_HAP %>%
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


#teste efeitos mistos
HAP.mod_LAZER_VIAGEM_VONTADE2 <- glmer(VD ~ LAZER_VIAGEM_VONTADE2+
                            (1|ITEM_LEXICAL)+
                            (1|PARTICIPANTE), 
                          data = dados_HAP, family = binomial)
summary(HAP.mod_LAZER_VIAGEM_VONTADE2)
lrm(VD ~ LAZER_VIAGEM_VONTADE2, data = dados_HAP)
plot(allEffects(HAP.mod_LAZER_VIAGEM_VONTADE2), type = "response")

### Infancia ####
HAP.prop_INFANCIA_MEMORIA <- dados_HAP %>%
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

#teste efeitos mistos
HAP.mod_INFANCIA_MEMORIA <- glmer(VD ~ INFANCIA_MEMORIA+
                            (1|ITEM_LEXICAL)+
                            (1|PARTICIPANTE), 
                          data = dados_HAP, family = binomial)
summary(HAP.mod_INFANCIA_MEMORIA)
lrm(VD ~ INFANCIA_MEMORIA, data = dados_HAP)
plot(allEffects(HAP.mod_INFANCIA_MEMORIA), type = "response")


# PCA ####
escalas_HAP <- dados_HAP %>%
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

pca_HAP <- prcomp(escalas_HAP, center = TRUE, scale. = TRUE)
summary(pca_HAP)          # variância explicada por cada componente
pca_HAP$rotation           # cargas (contribuição das variáveis)
#pca_S0$x                  # coordenadas dos indivíduos

# Scree plot
fviz_eig(pca_HAP, 
         addlabels = TRUE,    # mostra valores no gráfico
         ylim = c(0, 40))     # ajusta o eixo Y para ver melhor

fviz_contrib(pca_HAP, choice = "var", axes = 1, top = 10)

write.csv(pca_HAP$rotation[,1:4], "pca_HAP_scores.csv", row.names = TRUE)

# FEATURE SELECTION - LASSO ####
escalas_lasso_HAP <- dados_HAP %>%
  select(VD,
         INDICE_ESCOL3_norm, 
         #INDICE_ESCOL_PAI_norm,
         #INDICE_ESCOL_MAE_norm, 
         PAIS,
         INDICE_OCUPACAO_norm, 
         #INDICE_OCUPACAO_PAI_norm, 
         #INDICE_OCUPACAO_MAE_norm,
         #INDICE_OUTRO_CARGO2_norm, 
         INDICE_OCUPACAO_SONHOS2_norm,
         #INDICE_LOCOMOCAO_norm, 
         INDICE_MEGA_norm,
         INDICE_RENDA_IND_norm, 
         #INDICE_RENDA_FAM_norm,
         #INDICE_BAIRRO_norm,
         #DENSIDADE_HABITACAO_norm,
         #INDICE_IMOVEL_norm,
         #INDICE_LAZER_norm, 
         #INDICE_LAZER_CAMPINAS_norm,
         LAZER,
         #INDICE_VIAGEM_norm, 
         #INDICE_VIAGEM_LUGAR_norm, 
         #INDICE_VIAGEM_VONTADE_norm, 
         VIAGEM,
         INDICE_INFANCIA_norm
  ) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

x_HAP <- model.matrix(VD ~ ., escalas_lasso_HAP)[, -1]
y_HAP <- escalas_lasso_HAP$VD

lasso_HAP <- cv.glmnet(x_HAP, y_HAP, alpha = 1)
COEF_HAP <- coef(lasso_HAP, s = "lambda.min")
COEF_HAP

plot(lasso_HAP$glmnet.fit, xvar = "lambda", label = TRUE)

