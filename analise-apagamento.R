#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%# PROCESSO DE Apagamento #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 15/07/2025 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% V1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# VD ####
S0.prop_VD <- dados_S0 %>% 
  count(VD) %>%
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/S0/1S0_VD.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(S0.prop_VD, aes(x = VD, y = prop, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
  scale_x_discrete(labels = c("Realização", "Apagamento"))+
  geom_text(aes(label = label), vjust = -0.3, size = 4) + 
  scale_fill_brewer(palette = "Reds")+
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     expand = expansion(mult = c(0, 0.15))) + #aumenta espaço no topo
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    legend.position = "none")
dev.off()


## Por participante ####
S0.participante <- dados_S0 %>% 
  count(PARTICIPANTE, VD) %>%
  group_by(PARTICIPANTE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>%
  print()


#png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/S0/VD-participante.png", width = 6.5, height = 4.5, units = "in", res = 300)
S0.participante %>%   
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
S0.prop_TONICIDADE <- dados_S0 %>%
  count(VD, TONICIDADE) %>%
  group_by(TONICIDADE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/S0/2S0_tonicidade.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(S0.prop_TONICIDADE, aes(x = TONICIDADE, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
  scale_x_discrete(labels = c("Átona", "Tônica"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variável \nResposta", labels = c("Realização", "Apagamanto"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25))
dev.off()

(S0.tab_TONICIDADE <- with(dados_S0, table(TONICIDADE, VD)))
chisq.test(S0.tab_TONICIDADE)


# POSICAO ####
S0.prop_POSICAO <- dados_S0 %>%
  count(VD, POSICAO_S) %>%
  group_by(POSICAO_S) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()


png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/S0/3S0_posicao.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(S0.prop_POSICAO, aes(x = POSICAO_S, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Posição na palavra", y = "Proporção de Ocorrência") + 
  scale_x_discrete(labels = c("Final", "Medial"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variável \nResposta", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25))
dev.off()




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
  labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
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
  #labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
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
  labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
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


png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/S0/4S0_cfs.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(S0.prop_CFS_sonoridade, aes(x = CFS_sonoridade, y = prop * 100, fill = VD, label = label)) +
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Contexto Fonológico Seguinte", y = "Proporção de Ocorrência") + 
  scale_x_discrete(labels = c("Pausa", "Sonoro", "Surdo"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variável \nResposta", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25))
dev.off()

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
  #labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
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
   
  count(VD, ESTILO) %>%
  group_by(ESTILO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_ESTILO, aes(x = ESTILO, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
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


png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/S0/5S0_genero.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(S0.prop_GENERO, aes(x = GENERO, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Gênero", y = "Proporção de Ocorrência") + 
  scale_x_discrete(labels = c("Feminino", "Masculino"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variável \nResposta", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))
dev.off()

(S0.tab_GENERO <- with(dados_S0, table(GENERO, VD)))
chisq.test(S0.tab_GENERO)

# TEMPO DE RESIDENCIA ####
S0.prop_TEMPO_RESIDENCIA <- dados_S0 %>% 
  count(VD, TEMPO_RESIDENCIA) %>%
  group_by(TEMPO_RESIDENCIA) %>% 
  mutate(prop = prop.table(n)) %>%
  print(n = 45)

png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/S0/6S0_tempo_residencia.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(S0.prop_TEMPO_RESIDENCIA[24:45,], aes(x = TEMPO_RESIDENCIA, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Tempo de Residência", y = "Proporção de Apagamento") +
  theme_light()
dev.off()

S0.mod_TEMPO_RESIDENCIA <- glm(VD ~ TEMPO_RESIDENCIA, data = dados_S0, family = binomial)
summary(S0.mod_TEMPO_RESIDENCIA)
lrm(VD ~ TEMPO_RESIDENCIA, data = dados_S0)

plot(allEffects(S0.mod_TEMPO_RESIDENCIA), type = "response")


# IDADE DE MIGRACAO ####

S0.prop_IDADE_MIGRACAO <- dados_S0 %>% 
  count(VD, IDADE_MIGRACAO) %>%
  group_by(IDADE_MIGRACAO) %>% 
  mutate(prop = prop.table(n)) %>%
  print(n = 51)

png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/S0/7S0_idade_migracao.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(S0.prop_IDADE_MIGRACAO[27:51,], aes(x = IDADE_MIGRACAO, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Idade de Migração", y = "Proporção de Apagamento") +
  theme_light()
dev.off()

S0.mod_IDADE_MIGRACAO <- glm(VD ~ IDADE_MIGRACAO, data = dados_S0, family = binomial)
summary(S0.mod_IDADE_MIGRACAO)
lrm(VD ~ IDADE_MIGRACAO, data = dados_S0)

plot(allEffects(S0.mod_IDADE_MIGRACAO), type = "response")




# INDICE SOCIO POLI ####
S0.prop_INDICE_SOCIO_POLI <- dados_S0 %>% 
  count(VD, INDICE_SOCIO_POLI) %>%
  group_by(INDICE_SOCIO_POLI) %>% 
  mutate(prop = prop.table(n)) %>%
  print(n = 92)

png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/S0/8S0_indicesocio.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(S0.prop_INDICE_SOCIO_POLI[46:87,], aes(x = INDICE_SOCIO_POLI, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Índice Socioeconômico", y = "Proporção de Apagamento") +
  theme_light()
dev.off()

AP.mod_INDICE_SOCIO_POLI <- glm(VD ~ INDICE_SOCIO_POLI, data = dados_S0, family = binomial)
summary(AP.mod_INDICE_SOCIO_POLI)
lrm(VD ~ INDICE_SOCIO_POLI, data = dados_S0)
plot(allEffects(AP.mod_INDICE_SOCIO_POLI), type = "response") 




# 1 MODELAGEM DE BARBOSA(2023) ####
modS01 <- glmer(VD ~ TONICIDADE + 
                  POSICAO_S +
                  CFP_abertura2 +
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
r.squaredGLMM(modS01)

addmargins(table(dados_S0$TONICIDADE, dados_S0$VD))
addmargins(table(dados_S0$POSICAO_S, dados_S0$VD))
addmargins(table(dados_S0$CFP_abertura2, dados_S0$VD))
addmargins(table(dados_S0$CFS_sonoridade, dados_S0$VD))
addmargins(table(dados_S0$CLASSE_MORFOLOGICA3, dados_S0$VD))
addmargins(table(dados_S0$GENERO, dados_S0$VD))


# 2 MODELAGEM DE barbosa INDICE SOCIO OUSHIRO ####
modS02 <- glmer(VD ~ TONICIDADE + 
                  POSICAO_S +
                  CFP_abertura2 +
                  CFS_sonoridade +
                  CLASSE_MORFOLOGICA3 + 
                  GENERO + 
                  TEMPO_RESIDENCIA + 
                  IDADE_MIGRACAO +
                  INDICE_SOCIO_OUSHIRO +
                  (1|ITEM_LEXICAL) +
                  (1|PARTICIPANTE), data = dados_S0, family = binomial)
summary(modS02)
lrm(VD ~ TONICIDADE + 
      POSICAO_S +
      CFP_abertura +
      CFS_sonoridade +
      CLASSE_MORFOLOGICA3 + 
      GENERO + 
      TEMPO_RESIDENCIA + 
      IDADE_MIGRACAO +
      INDICE_SOCIO_OUSHIRO, data = dados_S0)

car::vif(modS02)
check_model(modS02)
check_outliers(modS02)
r.squaredGLMM(modS02)


# 3 MODELAGEM DE POLI INDICE SOCIO POLI ####
modS03 <- glmer(VD ~ TONICIDADE + 
                  POSICAO_S +
                  CFP_abertura2 +
                  CFS_sonoridade +
                  CLASSE_MORFOLOGICA3 + 
                  GENERO + 
                  TEMPO_RESIDENCIA + 
                  IDADE_MIGRACAO +
                  INDICE_SOCIO_POLI +
                  (1|ITEM_LEXICAL) +
                  (1|PARTICIPANTE), data = dados_S0, family = binomial)
summary(modS03)
lrm(VD ~ TONICIDADE + 
      POSICAO_S +
      CFP_abertura2 +
      CFS_sonoridade +
      CLASSE_MORFOLOGICA3 + 
      GENERO + 
      TEMPO_RESIDENCIA + 
      IDADE_MIGRACAO +
      INDICE_SOCIO_POLI, data = dados_S0)

car::vif(modS03)
check_model(modS03)
check_outliers(modS03)
r.squaredGLMM(modS03)

compare_performance(modS02, modS03)



# 4 MODELAGEM DE POLI INDICE SOCIO POLI sem CFP_abertura E CLASSE ####
modS04 <- glmer(VD ~ TONICIDADE + 
                  POSICAO_S +
                  #CFP_abertura +
                  #CFS_sonoridade +
                  #CLASSE_MORFOLOGICA3 + 
                  GENERO + 
                  TEMPO_RESIDENCIA + 
                  IDADE_MIGRACAO +
                  INDICE_SOCIO_POLI +
                  (1|ITEM_LEXICAL) +
                  (1|PARTICIPANTE), data = dados_S0, family = binomial)
summary(modS04)
lrm(VD ~ TONICIDADE + 
      POSICAO_S +
      #CFP_abertura +
      CFS_sonoridade +
      CLASSE_MORFOLOGICA3 + 
      GENERO + 
      TEMPO_RESIDENCIA + 
      IDADE_MIGRACAO +
      INDICE_SOCIO_POLI, data = dados_S0)

car::vif(modS04)
check_model(modS04)
check_outliers(modS04)
r.squaredGLMM(modS04)




# INDICE SOCIOECONOMICO ####
### Escolaridade ####
S0.prop_ESCOLARIDADE2 <- dados_S0 %>%
  count(VD, ESCOLARIDADE2) %>%
  group_by(ESCOLARIDADE2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_ESCOLARIDADE2, aes(x = ESCOLARIDADE2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.tab_ESCOLARIDADE2 <- with(dados_S0, table(ESCOLARIDADE2, VD)))
chisq.test(S0.tab_ESCOLARIDADE2)
chisq.test(S0.tab_ESCOLARIDADE2[c(2,3),])

#teste efeitos mistos
S0.mod_ESCOLARIDADE2 <- glmer(VD ~ ESCOLARIDADE2 +
                                   (1|ITEM_LEXICAL) +
                                   (1|PARTICIPANTE), data = dados_S0, family = binomial)
summary(S0.mod_ESCOLARIDADE2)
lrm(VD ~ ESCOLARIDADE2, data = dados_S0)
plot(allEffects(S0.mod_ESCOLARIDADE2), type = "response")



#escolaridade1
#chisq.test(S0.tab_ESCOLARIDADE[c(1,2),]) #sem diferença pra fund1 e 2
#chisq.test(S0.tab_ESCOLARIDADE[c(4,5),]) #sem diferença pra superior e pósgrad 



### Escolaridade dos Pais ####
#### Pai ####
S0.prop_ESCOLA_PAI2 <- dados_S0 %>%
  count(VD, ESCOLA_PAI2) %>%
  group_by(ESCOLA_PAI2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_ESCOLA_PAI2, aes(x = ESCOLA_PAI2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.tab_ESCOLA_PAI2<- with(dados_S0, table(ESCOLA_PAI2, VD)))
chisq.test(S0.tab_ESCOLA_PAI2) #sim
chisq.test(S0.tab_ESCOLA_PAI2[c(1,2),]) 
chisq.test(S0.tab_ESCOLA_PAI2[c(1,3),]) 
chisq.test(S0.tab_ESCOLA_PAI2[c(3,4),]) 



#teste efeitos mistos
S0.mod_ESCOLA_PAI2 <- glmer(VD ~ ESCOLA_PAI2 +
                                (1|ITEM_LEXICAL) +
                                (1|PARTICIPANTE), data = dados_S0, family = binomial)
summary(S0.mod_ESCOLA_PAI2)
lrm(VD ~ ESCOLA_PAI2, data = dados_S0)
plot(allEffects(S0.mod_ESCOLA_PAI2), type = "response")

#### Mãe ####
S0.prop_ESCOLA_MAE2 <- dados_S0 %>%
  count(VD, ESCOLA_MAE2) %>%
  group_by(ESCOLA_MAE2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_ESCOLA_MAE2, aes(x = ESCOLA_MAE2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Resposta", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))

(S0.tab_ESCOLA_MAE2<- with(dados_S0, table(ESCOLA_MAE2, VD)))
chisq.test(S0.tab_ESCOLA_MAE2) #sim
chisq.test(S0.tab_ESCOLA_MAE2[c(1,2),])


#teste efeitos mistos
S0.mod_ESCOLA_MAE2 <- glmer(VD ~ ESCOLA_MAE2 +
                              (1|ITEM_LEXICAL) +
                              (1|PARTICIPANTE), data = dados_S0, family = binomial)
summary(S0.mod_ESCOLA_MAE2)
lrm(VD ~ ESCOLA_MAE2, data = dados_S0)
plot(allEffects(S0.mod_ESCOLA_MAE2), type = "response")

### Ocupação ####
S0.prop_INDICE_OCUPACAO <- dados_S0 %>%
  count(VD, INDICE_OCUPACAO) %>%
  group_by(INDICE_OCUPACAO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_INDICE_OCUPACAO[9:16,], aes(x = INDICE_OCUPACAO, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Índice de Ocupação", y = "Proporção de Apagamento") +
  theme_light()

S0.mod_INDICE_OCUPACAO <- glmer(VD ~ INDICE_OCUPACAO +
                                (1|ITEM_LEXICAL) +
                                (1|PARTICIPANTE), data = dados_S0, family = binomial)
summary(S0.mod_INDICE_OCUPACAO)
lrm(VD ~ INDICE_OCUPACAO, data = dados_S0)
plot(allEffects(S0.mod_INDICE_OCUPACAO), type = "response")



### Ocupação outro cargo ####
S0.prop_INDICE_OUTRO_CARGO <- dados_S0 %>%
  count(VD, INDICE_OUTRO_CARGO) %>%
  group_by(INDICE_OUTRO_CARGO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



S0.mod_INDICE_OUTRO_CARGO <- glmer(VD ~ INDICE_OUTRO_CARGO+
                                   (1|ITEM_LEXICAL) +
                                   (1|PARTICIPANTE), 
                                   data = dados_S0, family = binomial)
summary(S0.mod_INDICE_OUTRO_CARGO)
lrm(VD ~ INDICE_OUTRO_CARGO, data = dados_S0)
plot(allEffects(S0.mod_INDICE_OUTRO_CARGO), type = "response")


### Ocupação SONHOS ####
S0.prop_INDICE_OCUPACAO_SONHOS <- dados_S0 %>%
  count(VD, INDICE_OCUPACAO_SONHOS) %>%
  group_by(INDICE_OCUPACAO_SONHOS) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

S0.mod_INDICE_OCUPACAO_SONHOS <- glmer(VD ~ INDICE_OCUPACAO_SONHOS+
                                      (1|ITEM_LEXICAL) +
                                      (1|PARTICIPANTE), 
                                      data = dados_S0, family = binomial)
summary(S0.mod_INDICE_OCUPACAO_SONHOS)
lrm(VD ~ INDICE_OCUPACAO_SONHOS, data = dados_S0)
plot(allEffects(S0.mod_INDICE_OCUPACAO_SONHOS), type = "response")

### Ocupação distancia ####
S0.prop_OCUPACAO_DIST <- dados_S0 %>%
  count(VD, OCUPACAO_DIST) %>%
  group_by(OCUPACAO_DIST) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()


ggplot(S0.prop_OCUPACAO_DIST, aes(x = OCUPACAO_DIST, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Individual", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variantes", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(
      color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(
      color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.tab_OCUPACAO_DIST<- with(dados_S0, table(OCUPACAO_DIST, VD)))
chisq.test(S0.tab_OCUPACAO_DIST) #sim
chisq.test(S0.tab_OCUPACAO_DIST[c(1,2),])
chisq.test(S0.tab_OCUPACAO_DIST[c(2,3),])


#teste efeitos mistos
S0.mod_OCUPACAO_DIST <- glmer(VD ~ OCUPACAO_DIST +
                              (1|ITEM_LEXICAL) +
                              (1|PARTICIPANTE), 
                              data = dados_S0, family = binomial)
summary(S0.mod_OCUPACAO_DIST)
lrm(VD ~ OCUPACAO_DIST, data = dados_S0)
plot(allEffects(S0.mod_OCUPACAO_DIST), type = "response")


### Ocupação locomoção ####
#analise de locomoção com todos os itens foi transformada na seguinte OCUPACAO_LOCOMOCAO2
S0.prop_OCUPACAO_LOCOMOCAO2 <- dados_S0 %>%
  count(VD, OCUPACAO_LOCOMOCAO2) %>%
  group_by(OCUPACAO_LOCOMOCAO2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()


ggplot(S0.prop_OCUPACAO_LOCOMOCAO2, aes(x = OCUPACAO_LOCOMOCAO2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Individual", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variantes", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(
      color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(
      color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.tab_OCUPACAO_LOCOMOCAO2<- with(dados_S0, table(OCUPACAO_LOCOMOCAO2, VD)))
chisq.test(S0.tab_OCUPACAO_LOCOMOCAO2) #sim
chisq.test(S0.tab_OCUPACAO_LOCOMOCAO2[c(1,2),])

#teste efeitos mistos
S0.mod_OCUPACAO_LOCOMOCAO2 <- glmer(VD ~ OCUPACAO_LOCOMOCAO2 +
                                (1|ITEM_LEXICAL) +
                                (1|PARTICIPANTE), 
                              data = dados_S0, family = binomial)
summary(S0.mod_OCUPACAO_LOCOMOCAO2)
lrm(VD ~ OCUPACAO_LOCOMOCAO2, data = dados_S0)
plot(allEffects(S0.mod_OCUPACAO_LOCOMOCAO2), type = "response")


### Ocupação dos Pais ####
#### Pai ####
S0.prop_INDICE_OCUPACAO_PAI <- dados_S0 %>%
  count(VD, INDICE_OCUPACAO_PAI) %>%
  group_by(INDICE_OCUPACAO_PAI) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_INDICE_OCUPACAO_PAI[5:8,], aes(x = INDICE_OCUPACAO_PAI, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Índice de Ocupação", y = "Proporção de Apagamento") +
  theme_light()

S0.mod_INDICE_OCUPACAO_PAI <- glmer(VD ~ INDICE_OCUPACAO_PAI+
                                      (1|ITEM_LEXICAL) +
                                      (1|PARTICIPANTE), 
                                    data = dados_S0, family = binomial)
summary(S0.mod_INDICE_OCUPACAO_PAI)
lrm(VD ~ INDICE_OCUPACAO_PAI, data = dados_S0)
plot(allEffects(S0.mod_INDICE_OCUPACAO_PAI), type = "response")


#### Mãe ####
S0.prop_INDICE_OCUPACAO_MAE <- dados_S0 %>%
  count(VD, INDICE_OCUPACAO_MAE) %>%
  group_by(INDICE_OCUPACAO_MAE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()


S0.prop_INDICE_OCUPACAO_MAE %>% 
  filter(VD == "0", !is.na(INDICE_OCUPACAO_MAE)) %>% 
ggplot(aes(x = INDICE_OCUPACAO_MAE, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Índice de Ocupação", y = "Proporção de Apagamento") +
  theme_light()



S0.mod_INDICE_OCUPACAO_MAE <- glmer(VD ~ INDICE_OCUPACAO_MAE+
                                      (1|ITEM_LEXICAL) +
                                      (1|PARTICIPANTE), data = dados_S0, family = binomial)
summary(S0.mod_INDICE_OCUPACAO_MAE)
lrm(VD ~ INDICE_OCUPACAO_MAE, data = dados_S0)
plot(allEffects(S0.mod_INDICE_OCUPACAO_MAE), type = "response")


### Mega sena ####
S0.prop_MEGA_SENA2 <- dados_S0 %>%
  count(VD, MEGA_SENA2) %>%
  group_by(MEGA_SENA2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



ggplot(S0.prop_MEGA_SENA2, aes(x = MEGA_SENA2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Individual", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variantes", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(
      color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(
      color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.tab_MEGA_SENA2<- with(dados_S0, table(MEGA_SENA2, VD)))
chisq.test(S0.tab_MEGA_SENA2) #sim
chisq.test(S0.tab_MEGA_SENA2[c(1,2),])
chisq.test(S0.tab_MEGA_SENA2[c(3,4),])

#teste efeitos mistos
S0.mod_MEGA_SENA2 <- glmer(VD ~ MEGA_SENA2 +
                                (1|ITEM_LEXICAL) +
                                (1|PARTICIPANTE), 
                                data = dados_S0, family = binomial)
summary(S0.mod_MEGA_SENA2)
lrm(VD ~ MEGA_SENA2, data = dados_S0)
plot(allEffects(S0.mod_MEGA_SENA2), type = "response")



### Mega sena ####
S0.prop_MEGASENA_TRABALHAR2 <- dados_S0 %>%
  count(VD, MEGASENA_TRABALHAR2) %>%
  group_by(MEGASENA_TRABALHAR2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



ggplot(S0.prop_MEGASENA_TRABALHAR2, aes(x = MEGASENA_TRABALHAR2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Individual", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(
      color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(
      color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.tab_MEGASENA_TRABALHAR2 <- with(dados_S0, table(MEGASENA_TRABALHAR2, VD)))
chisq.test(S0.tab_MEGASENA_TRABALHAR2) #nao

#teste efeitos mistos
S0.mod_MEGASENA_TRABALHAR2 <- glmer(VD ~ MEGASENA_TRABALHAR2 +
                             (1|ITEM_LEXICAL) +
                             (1|PARTICIPANTE), 
                           data = dados_S0, family = binomial)
summary(S0.mod_MEGASENA_TRABALHAR2)
lrm(VD ~ MEGASENA_TRABALHAR2, data = dados_S0)
plot(allEffects(S0.mod_MEGASENA_TRABALHAR2), type = "response")



### Renda Individual ####
S0.prop_RENDA_IND <- dados_S0 %>%
  count(VD, RENDA_IND) %>%
  group_by(RENDA_IND) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_RENDA_IND, aes(x = RENDA_IND, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Renda Individual", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variantes", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.tab_RENDA_IND <- with(dados_S0, table(RENDA_IND, VD)))
chisq.test(S0.tab_RENDA_IND) #tem correlação
chisq.test(S0.tab_RENDA_IND[c(1,2),]) #sim
chisq.test(S0.tab_RENDA_IND[c(4,5),]) #sim
chisq.test(S0.tab_RENDA_IND[c(2,3),]) #sim
chisq.test(S0.tab_RENDA_IND[c(1,4),])

#teste efeitos mistos
S0.mod_RENDA_IND <- glmer(VD ~ RENDA_IND +
                            (1|ITEM_LEXICAL) +
                            (1|PARTICIPANTE),
                          data = dados_S0, family = binomial)
summary(S0.mod_RENDA_IND)
lrm(VD ~ RENDA_IND, data = dados_S0)
plot(allEffects(S0.mod_RENDA_IND), type = "response")





### Renda Familiar ####
S0.prop_RENDA_FAM <- dados_S0 %>%
  count(VD, RENDA_FAM) %>%
  group_by(RENDA_FAM) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_RENDA_FAM, aes(x = RENDA_FAM, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variantes", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.tab_RENDA_FAM <- with(dados_S0, table(RENDA_FAM, VD)))
chisq.test(S0.tab_RENDA_FAM) #tem correlação
chisq.test(S0.tab_RENDA_FAM[c(1,2),]) #nao
chisq.test(S0.tab_RENDA_FAM[c(2,3),]) #nao
chisq.test(S0.tab_RENDA_FAM[c(3,4),]) #sim
chisq.test(S0.tab_RENDA_FAM[c(4,5),]) #sim


#teste efeitos mistos
S0.mod_RENDA_FAM <- glmer(VD ~ RENDA_FAM +
                            (1|ITEM_LEXICAL) +
                            (1|PARTICIPANTE),
                          data = dados_S0, family = binomial)
summary(S0.mod_RENDA_FAM)
lrm(VD ~ RENDA_FAM, data = dados_S0)
plot(allEffects(S0.mod_RENDA_FAM), type = "response")




### m2 ####
S0.prop_media_m2 <- dados_S0 %>%
  count(VD, media_m2) %>%
  group_by(media_m2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print(n = 40)

ggplot(S0.prop_media_m2[21:40,], aes(x = media_m2, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Índice de Ocupação", y = "Proporção de Apagamento") +
  theme_light()


S0.mod_media_m2 <- glmer(VD ~ media_m2+
                           (1|ITEM_LEXICAL) +
                           (1|PARTICIPANTE), 
                         data = dados_S0, family = binomial)
summary(S0.mod_media_m2)
lrm(VD ~ INDICE_OCUPACAO, data = dados_S0)
plot(allEffects(S0.mod_media_m2), type = "response")

### Bairro ####
ordem_bairros <- dados_S0 %>%
  group_by(BAIRRO) %>%
  summarise(media_geral = mean(media_m2, na.rm = TRUE)) %>%
  arrange(media_geral) %>%  # ou arrange(media_geral) para ordem crescente
  pull(BAIRRO)

S0.prop_BAIRRO <- dados_S0 %>%
  count(VD, BAIRRO, media_m2) %>%
  mutate(BAIRRO = factor(BAIRRO, levels = ordem_bairros)) %>%  # Reordena os níveis
  group_by(BAIRRO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  ungroup()

S0.prop_BAIRRO %>% 
  ggplot(aes(x = BAIRRO, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variantes", labels = c("Realização", "Apagamento"))+
  coord_flip() +  # Barras horizontais
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.prop_BAIRRO <- with(dados_S0, table(BAIRRO, VD)))
chisq.test(S0.prop_BAIRRO)

#### Região ####
S0.prop_BAIRRO_REGIAO <- dados_S0 %>%
  count(VD, BAIRRO_REGIAO2) %>%
  group_by(BAIRRO_REGIAO2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_BAIRRO_REGIAO, aes(x = BAIRRO_REGIAO2, y = prop * 100, fill = VD, label = label)) + 
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


(S0.tab_BAIRRO_REGIAO <- with(dados_S0, table(BAIRRO_REGIAO, VD)))
chisq.test(S0.tab_BAIRRO_REGIAO) #tem correlação
chisq.test(S0.tab_BAIRRO_REGIAO[c(1,3),])


#teste efeitos mistos
S0.mod_BAIRRO_REGIAO <- glmer(VD ~ BAIRRO_REGIAO2 +
                            (1|ITEM_LEXICAL) +
                            (1|PARTICIPANTE),
                          data = dados_S0, family = binomial)
summary(S0.mod_BAIRRO_REGIAO)
lrm(VD ~ BAIRRO_REGIAO2, data = dados_S0)
plot(allEffects(S0.mod_BAIRRO_REGIAO), type = "response")

### Número de Banheiros ####
S0.prop_NBANHEIROS <- dados_S0 %>%
  count(VD, NBANHEIROS) %>%
  group_by(NBANHEIROS) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

S0.prop_NBANHEIROS %>% 
  ggplot(aes(x = NBANHEIROS, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Numero de Banheiros", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variantes", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.prop_NBANHEIROS <- with(dados_S0, table(NBANHEIROS, VD)))
chisq.test(S0.prop_NBANHEIROS)
chisq.test(S0.prop_NBANHEIROS[c(1,2)])
chisq.test(S0.prop_NBANHEIROS[c(2,3)])

#teste efeitos mistos
S0.mod_NBANHEIROS <- glmer(VD ~ NBANHEIROS +
                                (1|ITEM_LEXICAL) +
                                (1|PARTICIPANTE),
                              data = dados_S0, family = binomial)
summary(S0.mod_NBANHEIROS)
lrm(VD ~ NBANHEIROS, data = dados_S0)
plot(allEffects(S0.mod_NBANHEIROS), type = "response")


### Número de Quartos ####
S0.prop_NQUARTOS <- dados_S0 %>%
  count(VD, NQUARTOS) %>%
  group_by(NQUARTOS) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

S0.prop_NQUARTOS %>% 
  ggplot(aes(x = NQUARTOS, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Numero de Quartos", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variantes", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.prop_NQUARTOS <- with(dados_S0, table(NQUARTOS, VD)))
chisq.test(S0.prop_NQUARTOS)
chisq.test(S0.prop_NQUARTOS[c(1,2)])
chisq.test(S0.prop_NQUARTOS[c(2,3)])


#teste efeitos mistos
S0.mod_NQUARTOS <- glmer(VD ~ NQUARTOS +
                             (1|ITEM_LEXICAL) +
                             (1|PARTICIPANTE),
                           data = dados_S0, family = binomial)
summary(S0.mod_NQUARTOS)
lrm(VD ~ NQUARTOS, data = dados_S0)
plot(allEffects(S0.mod_NQUARTOS), type = "response")


### Densidade ####
#teste efeitos mistos
S0.mod_DENSIDADE_HABITACAO <- glmer(VD ~ DENSIDADE_HABITACAO +
                                      (1|ITEM_LEXICAL) +
                                      (1|PARTICIPANTE), data = dados_S0, family = binomial)
summary(S0.mod_DENSIDADE_HABITACAO)
lrm(VD ~ DENSIDADE_HABITACAO, data = dados_S0)
plot(allEffects(S0.mod_DENSIDADE_HABITACAO), type = "response")


### Tipo Moradia ####
S0.prop_IMOVEL <- dados_S0 %>%
  count(VD, IMOVEL) %>%
  group_by(IMOVEL) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

S0.prop_IMOVEL %>% 
  ggplot(aes(x = IMOVEL, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Tipo de Imóvel", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variantes", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.prop_IMOVEL <- with(dados_S0, table(IMOVEL, VD)))
chisq.test(S0.prop_IMOVEL)
chisq.test(S0.prop_IMOVEL[c(1,2)])
chisq.test(S0.prop_IMOVEL[c(2,3)])


#teste efeitos mistos
S0.mod_IMOVEL <- glmer(VD ~ IMOVEL +
                           (1|ITEM_LEXICAL) +
                           (1|PARTICIPANTE),
                         data = dados_S0, family = binomial)
summary(S0.mod_IMOVEL)
lrm(VD ~ IMOVEL, data = dados_S0)
plot(allEffects(S0.mod_IMOVEL), type = "response")


### Propriedade característica ####
#acho que faz mais sentindo juntar financiado e próprio
S0.prop_PROPRIEDADE <- dados_S0 %>%
  count(VD, PROPRIEDADE) %>%
  group_by(PROPRIEDADE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

S0.prop_PROPRIEDADE %>% 
  ggplot(aes(x = PROPRIEDADE, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Característica do Imóvel", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variantes", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.prop_PROPRIEDADE <- with(dados_S0, table(PROPRIEDADE, VD)))
chisq.test(S0.prop_PROPRIEDADE)
chisq.test(S0.prop_PROPRIEDADE[c(1,2)])
chisq.test(S0.prop_PROPRIEDADE[c(1,3)])

#teste efeitos mistos
S0.mod_PROPRIEDADE <- glmer(VD ~ PROPRIEDADE +
                         (1|ITEM_LEXICAL) +
                         (1|PARTICIPANTE),
                       data = dados_S0, family = binomial)
summary(S0.mod_PROPRIEDADE)
lrm(VD ~ PROPRIEDADE, data = dados_S0)
plot(allEffects(S0.mod_PROPRIEDADE), type = "response")


### Número de Pessoas ####
S0.prop_NPESSOAS <- dados_S0 %>%
  count(VD, NPESSOAS) %>%
  group_by(NPESSOAS) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

S0.prop_NPESSOAS %>% 
  ggplot(aes(x = NPESSOAS, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variantes", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.prop_NPESSOAS <- with(dados_S0, table(NPESSOAS, VD)))
chisq.test(S0.prop_NPESSOAS)
chisq.test(S0.prop_NPESSOAS[c(2,6)])
chisq.test(S0.prop_NPESSOAS[c(2,3)])
chisq.test(S0.prop_NPESSOAS[c(2,4)])
chisq.test(S0.prop_NPESSOAS[c(4,5)])

#teste efeitos mistos
S0.mod_NPESSOAS <- glmer(VD ~ NPESSOAS +
                         (1|ITEM_LEXICAL) +
                         (1|PARTICIPANTE),
                       data = dados_S0, family = binomial)
summary(S0.mod_NPESSOAS)
lrm(VD ~ NPESSOAS, data = dados_S0)
plot(allEffects(S0.mod_NPESSOAS), type = "response")


### Lazer ####
S0.prop_LAZER_CARACTERISTICA <- dados_S0 %>%
  count(VD, LAZER_CARACTERISTICA) %>%
  group_by(LAZER_CARACTERISTICA) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_LAZER_CARACTERISTICA, aes(x = LAZER_CARACTERISTICA, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variantes", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.tab_LAZER_CARACTERISTICA <- with(dados_S0, table(LAZER_CARACTERISTICA, VD)))
chisq.test(S0.tab_LAZER_CARACTERISTICA) #tem correlação
chisq.test(S0.tab_LAZER_CARACTERISTICA[c(1,2),])
chisq.test(S0.tab_LAZER_CARACTERISTICA[c(2,3),])
chisq.test(S0.tab_LAZER_CARACTERISTICA[c(3,4),])
chisq.test(S0.tab_LAZER_CARACTERISTICA[c(1,4),])


#teste efeitos mistos
S0.mod_LAZER_CARACTERISTICA <- glmer(VD ~ LAZER_CARACTERISTICA +
                           (1|ITEM_LEXICAL) +
                           (1|PARTICIPANTE), 
                         data = dados_S0, family = binomial)
summary(S0.mod_LAZER_CARACTERISTICA)
lrm(VD ~ LAZER_CARACTERISTICA, data = dados_S0)
plot(allEffects(S0.mod_LAZER_CARACTERISTICA), type = "response")



### Lazer Campinas####
S0.prop_LAZER_CAMPINAS_CARACTERISTICA <- dados_S0 %>%
  count(VD, LAZER_CAMPINAS_CARACTERISTICA) %>%
  group_by(LAZER_CAMPINAS_CARACTERISTICA) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_LAZER_CAMPINAS_CARACTERISTICA, aes(x = LAZER_CAMPINAS_CARACTERISTICA, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variantes", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.tab_LAZER_CAMPINAS_CARACTERISTICA <- with(dados_S0, table(LAZER_CAMPINAS_CARACTERISTICA, VD)))
chisq.test(S0.tab_LAZER_CAMPINAS_CARACTERISTICA) #tem correlação
chisq.test(S0.tab_LAZER_CAMPINAS_CARACTERISTICA[c(1,2),])


#teste efeitos mistos
S0.mod_LAZER_CAMPINAS_CARACTERISTICA <- glmer(VD ~ LAZER_CAMPINAS_CARACTERISTICA +
                                       (1|ITEM_LEXICAL) +
                                       (1|PARTICIPANTE), 
                                     data = dados_S0, family = binomial)
summary(S0.mod_LAZER_CAMPINAS_CARACTERISTICA)
lrm(VD ~ LAZER_CAMPINAS_CARACTERISTICA, data = dados_S0)
plot(allEffects(S0.mod_LAZER_CAMPINAS_CARACTERISTICA), type = "response")



### Viagem ####
#costuma viajar?
S0.prop_VIAGEM <- dados_S0 %>%
  count(VD, VIAGEM) %>%
  group_by(VIAGEM) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_VIAGEM, aes(x = VIAGEM, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variantes", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.tab_VIAGEM <- with(dados_S0, table(VIAGEM, VD)))
chisq.test(S0.tab_VIAGEM) #tem correlação

#teste efeitos mistos
S0.mod_VIAGEM <- glmer(VD ~ VIAGEM +
                         (1|ITEM_LEXICAL) +
                         (1|PARTICIPANTE), 
                      data = dados_S0, family = binomial)
summary(S0.mod_VIAGEM)
lrm(VD ~ VIAGEM, data = dados_S0)
plot(allEffects(S0.mod_VIAGEM), type = "response")




### Tipo de Viagem ####
S0.prop_VIAGEM_LUGAR <- dados_S0 %>%
  count(VD, VIAGEM_LUGAR) %>%
  group_by(VIAGEM_LUGAR) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_VIAGEM_LUGAR, aes(x = VIAGEM_LUGAR, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variantes", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.tab_VIAGEM_LUGAR <- with(dados_S0, table(VIAGEM_LUGAR, VD)))
chisq.test(S0.tab_VIAGEM_LUGAR) #tem correlação
chisq.test(S0.tab_VIAGEM_LUGAR[c(2,3),])
chisq.test(S0.tab_VIAGEM_LUGAR[c(4,5),])
chisq.test(S0.tab_VIAGEM_LUGAR[c(1,4),])

#teste efeitos mistos
S0.mod_VIAGEM_LUGAR <- glmer(VD ~ VIAGEM_LUGAR +
                         (1|ITEM_LEXICAL) +
                         (1|PARTICIPANTE), 
                       data = dados_S0, family = binomial)
summary(S0.mod_VIAGEM_LUGAR)
lrm(VD ~ VIAGEM_LUGAR, data = dados_S0)
plot(allEffects(S0.mod_VIAGEM_LUGAR), type = "response")


### Viagem vontade ####
S0.prop_LAZER_VIAGEM_VONTADE2 <- dados_S0 %>%
  count(VD, LAZER_VIAGEM_VONTADE2) %>%
  group_by(LAZER_VIAGEM_VONTADE2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_LAZER_VIAGEM_VONTADE2, aes(x = LAZER_VIAGEM_VONTADE2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variantes", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.tab_LAZER_VIAGEM_VONTADE2 <- with(dados_S0, table(LAZER_VIAGEM_VONTADE2, VD)))
chisq.test(S0.tab_LAZER_VIAGEM_VONTADE2) #tem correlação
chisq.test(S0.tab_LAZER_VIAGEM_VONTADE2[c(1,2),])
chisq.test(S0.tab_LAZER_VIAGEM_VONTADE2[c(2,4),])


#teste efeitos mistos
S0.mod_LAZER_VIAGEM_VONTADE2 <- glmer(VD ~ LAZER_VIAGEM_VONTADE2 +
                         (1|ITEM_LEXICAL) +
                         (1|PARTICIPANTE), 
                       data = dados_S0, family = binomial)
summary(S0.mod_LAZER_VIAGEM_VONTADE2)
lrm(VD ~ LAZER_VIAGEM_VONTADE2, data = dados_S0)
plot(allEffects(S0.mod_LAZER_VIAGEM_VONTADE2), type = "response")

### Infancia ####
S0.prop_INFANCIA_MEMORIA <- dados_S0 %>%
  count(VD, INFANCIA_MEMORIA) %>%
  group_by(INFANCIA_MEMORIA) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(S0.prop_INFANCIA_MEMORIA, aes(x = INFANCIA_MEMORIA, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Renda Familiar", y = "Proporção de Ocorrência") + 
  #scale_x_discrete(labels = c("Realização", "Apagamento", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variantes", labels = c("Realização", "Apagamento"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    axis.title.x = element_text(size = 9),  # tamanho do título eixo X
    axis.title.y = element_text(size = 9))


(S0.tab_INFANCIA_MEMORIA <- with(dados_S0, table(INFANCIA_MEMORIA, VD)))
chisq.test(S0.tab_INFANCIA_MEMORIA) #tem correlação
chisq.test(S0.tab_INFANCIA_MEMORIA[c(1,2),])


#teste efeitos mistos
S0.mod_INFANCIA_MEMORIA <- glmer(VD ~ INFANCIA_MEMORIA +
                                        (1|ITEM_LEXICAL) +
                                        (1|PARTICIPANTE), 
                                      data = dados_S0, family = binomial)
summary(S0.mod_INFANCIA_MEMORIA)
lrm(VD ~ INFANCIA_MEMORIA, data = dados_S0)
plot(allEffects(S0.mod_INFANCIA_MEMORIA), type = "response")




# PCA ####
escalas_S0 <- dados_S0 %>%
  select(#VD,
         INDICE_ESCOL3_norm, 
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
         INDICE_INFANCIA_norm) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

pca_S0 <- prcomp(escalas_S0, center = TRUE, scale. = TRUE)
summary(pca_S0)          # variância explicada por cada componente
pca_S0$rotation           # cargas (contribuição das variáveis)
#pca_S0$x                  # coordenadas dos indivíduos

# Scree plot
fviz_eig(pca_S0, 
         addlabels = TRUE,    # mostra valores no gráfico
         ylim = c(0, 40))     # ajusta o eixo Y para ver melhor

fviz_contrib(pca_S0, choice = "var", axes = 1, top = 10)

write.csv(pca_S0$rotation[,1:4], "pca_S0_scores.csv", row.names = TRUE)

# FEATURE SELECTION - LASSO ####
escalas_lasso_S0 <- dados_S0 %>%
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


x_S0 <- model.matrix(VD ~ ., escalas_lasso_S0)[, -1]
y_S0 <- escalas_lasso_S0$VD

lasso_S0 <- cv.glmnet(x_S0, y_S0, alpha = 1)
COEF_S0 <- coef(lasso_S0, s = "lambda.min")
COEF_S0
plot(lasso_S0$glmnet.fit, xvar = "lambda", label = TRUE)

