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
#   labs(x = "Variável Resposta", y = "Proporção de Ocorrência (%)") + 
#   #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
#   geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
#   scale_fill_brewer(palette = "Reds")+
#   theme_minimal()+
#   theme(
#     panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
#     panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
#    
#     )
# 
# #CFS_coronal de acordo com BARBOSA (2023)
AP.prop_CFS_pontoc2<- dados_AP.cfs %>%
  count(VD, CFS_pontoc2) %>%
  group_by(CFS_pontoc2) %>%
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>%
  print()


png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/AP/1.1AP_cfs.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(AP.prop_CFS_pontoc2, aes(x = CFS_pontoc2, y = prop * 100, fill = VD, label = label)) +
  geom_bar(stat = "identity", color = "white") +
  labs(x = "Contexto Fonológico Seguinte", y = "Proporção de Ocorrência (%)") +
  scale_x_discrete(labels = c("Pausa", "Coronal", "Dorsal", "Labial"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variável \nResposta", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25))
dev.off()

# VD ####
AP.prop_VD <- dados_AP %>% 
  count(VD) %>%
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/AP/1AP_VD.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(AP.prop_VD, aes(x = VD, y = prop, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Resposta", y = "Proporção de Ocorrência (%)") + 
  scale_x_discrete(labels = c("Alveolar", "Palatal"))+
  geom_text(aes(label = label), vjust = -0.3, size = 3.5) + 
  scale_fill_brewer(palette = "Reds")+
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     expand = expansion(mult = c(0, 0.15))) + #aumenta espaço no topo
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        legend.position = "none")
dev.off()



## Por participante ####
AP.participante <- dados_AP %>% 
  count(PARTICIPANTE, VD) %>%
  group_by(PARTICIPANTE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>%
  print()


#png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/VD-participante.png", width = 6.5, height = 4.5, units = "in", res = 300)
AP.participante %>%   
  ggplot(aes(x = VD, y = prop, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Resposta", y = "Proporção de Ocorrência (%)", fill = "VD") + 
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
    legend.position = "bottom")
#dev.off()


# TONICIDADE ####
AP.prop_TONICIDADE <- dados_AP %>%
  count(VD, TONICIDADE) %>%
  group_by(TONICIDADE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()


png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/AP/2AP_tonicidade.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(AP.prop_TONICIDADE, aes(x = TONICIDADE, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Tonicidade", y = "Proporção de Ocorrência (%)") + 
  scale_x_discrete(labels = c("Átona", "Tônica"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variável \nResposta", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25))
dev.off()




(AP.tab_TONICIDADE <- with(dados_AP, table(TONICIDADE, VD)))
chisq.test(AP.tab_TONICIDADE)


# POSICAO ####
AP.prop_POSICAO <- dados_AP %>%
  count(VD, POSICAO_S) %>%
  group_by(POSICAO_S) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/AP/3AP_posicao.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(AP.prop_POSICAO, aes(x = POSICAO_S, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Posição", y = "Proporção de Ocorrência (%)") + 
  scale_x_discrete(labels = c("Final", "Medial"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variável \nResposta",labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25))
dev.off()


(AP.tab_POSICAO <- with(dados_AP, table(POSICAO_S, VD)))
chisq.test(AP.tab_POSICAO)


# CLASSE MORFOLOGICA ####
AP.prop_CLASSE_MORFOLOGICA3 <- dados_AP %>%
  count(VD, CLASSE_MORFOLOGICA3) %>%
  group_by(CLASSE_MORFOLOGICA3) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/AP/4AP_classe_morfologica.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(AP.prop_CLASSE_MORFOLOGICA3, aes(x = CLASSE_MORFOLOGICA3, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Classe Morfológica", y = "Proporção de Ocorrência (%)") + 
  scale_x_discrete(labels = c("Gramatical", "Lexical"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variável \nResposta", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25))
dev.off()


(AP.tab_CLASSE_MORFOLOGICA3 <- with(dados_AP, table(CLASSE_MORFOLOGICA3, VD)))
chisq.test(AP.tab_CLASSE_MORFOLOGICA3)



# CONT.FON.PREC ####
AP.prop_CONT_FON_PREC <- dados_AP %>% 
  count(VD, CONT_FON_PREC) %>%
  group_by(CONT_FON_PREC) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_CONT_FON_PREC, aes(x = CONT_FON_PREC, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Resposta", y = "Proporção de Ocorrência (%)") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25))


#CFS_abertura assim como Barbosa (2023)
AP.prop_CFP_abertura2 <- dados_AP %>%
  count(VD, CFP_abertura2) %>%
  group_by(CFP_abertura2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()


png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/AP/5AP_cfp.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(AP.prop_CFP_abertura2, aes(x = CFP_abertura2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Contexto Fonológico Precedente", y = "Proporção de Ocorrência (%)") + 
  scale_x_discrete(labels = c("Fechada", "Meio fechada", "Meio aberta", "Aberta"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variável \nResposta", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25))
dev.off()

(AP.tab_CFP_abertura2 <- with(dados_AP, table(CFP_abertura2, VD)))
chisq.test(AP.tab_CFP_abertura2[c(1,2),]) #sem diferença entre fechada e meio fechada



# ESTILO ####
AP.prop_ESTILO <- dados_AP %>%
  count(VD, ESTILO) %>%
  group_by(ESTILO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_ESTILO, aes(x = ESTILO, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Resposta", y = "Proporção de Ocorrência (%)") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
   
    )


(AP.tab_ESTILO <- with(dados_AP, table(ESTILO, VD)))
chisq.test(AP.tab_ESTILO)


# GENERO ####
AP.prop_GENERO <- dados_AP %>%
  count(VD, GENERO) %>%
  group_by(GENERO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/AP/6AP_genero.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(AP.prop_GENERO, aes(x = GENERO, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Gênero", y = "Proporção de Ocorrência (%)") + 
  scale_x_discrete(labels = c("Feminino", "Masculino"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variável \nResposta", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25))
dev.off()


(AP.tab_GENERO <- with(dados_AP, table(GENERO, VD)))
chisq.test(AP.tab_GENERO)


# TEMPO DE RESIDENCIA ####
AP.prop_TEMPO_RESIDENCIA <- dados_AP %>% 
  count(VD, TEMPO_RESIDENCIA) %>%
  group_by(TEMPO_RESIDENCIA) %>% 
  mutate(prop = prop.table(n)) %>% 
  print(n = 46)

png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/AP/7AP_tempo_residencia.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(AP.prop_TEMPO_RESIDENCIA[24:46,], aes(x = TEMPO_RESIDENCIA, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Tempo de Residência", y = "Proporção de Palatalização") +
  #geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  theme_minimal()
dev.off()

AP.mod_TEMPO_RESIDENCIA <- glm(VD ~ TEMPO_RESIDENCIA, data = dados_AP, family = binomial)
summary(AP.mod_TEMPO_RESIDENCIA)
lrm(VD ~ TEMPO_RESIDENCIA, data = dados_AP)
plot(allEffects(AP.mod_TEMPO_RESIDENCIA), type = "response")

# IDADE DE MIGRACAO ####
AP.prop_IDADE_MIGRACAO <- dados_AP %>% 
  count(VD, IDADE_MIGRACAO) %>%
  group_by(IDADE_MIGRACAO) %>% 
  mutate(prop = prop.table(n)) %>% 
  print(n = 52)

png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/AP/8AP_idade_migracao.png", width = 5, height = 4.5, units = "in", res = 300)
ggplot(AP.prop_IDADE_MIGRACAO[27:52,], aes(x = IDADE_MIGRACAO, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Idade de Migração", y = "Proporção de Palatalização") +
  theme_minimal()
dev.off()

AP.mod_IDADE_MIGRACAO <- glm(VD ~ IDADE_MIGRACAO, data = dados_AP, family = binomial)
summary(AP.mod_IDADE_MIGRACAO)
lrm(VD ~ IDADE_MIGRACAO, data = dados_AP)

plot(allEffects(AP.mod_IDADE_MIGRACAO), type = "response")


# INDICE SOCIO OUSHIRO ####
AP.prop_INDICE_SOCIO_OUSHIRO <- dados_AP %>% 
  count(VD, INDICE_SOCIO_OUSHIRO) %>%
  group_by(INDICE_SOCIO_OUSHIRO) %>% 
  mutate(prop = prop.table(n)) %>% 
  print(n = 62)

#png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/VD_AP-tempo-residencia.png", width = 6.5, height = 4.5, units = "in", res = 300)
ggplot(AP.prop_INDICE_SOCIO_OUSHIRO[32:62,], aes(x = INDICE_SOCIO_OUSHIRO, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Índice Socioeconomico (Oushiro, 2015)", y = "Proporção de Palatalização") +
  #geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  theme_minimal()
#dev.off()

AP.mod_INDICE_SOCIO_OUSHIRO <- glm(VD ~ INDICE_SOCIO_OUSHIRO, data = dados_AP, family = binomial)
summary(AP.mod_INDICE_SOCIO_OUSHIRO)
lrm(VD ~ INDICE_SOCIO_OUSHIRO, data = dados_AP)
plot(allEffects(AP.mod_INDICE_SOCIO_OUSHIRO), type = "response")

# INDICE SOCIO POLI ####
AP.prop_INDICE_SOCIO_POLI <- dados_AP %>% 
  count(VD, INDICE_SOCIO_POLI) %>%
  group_by(INDICE_SOCIO_POLI) %>% 
  mutate(prop = prop.table(n)) %>% 
  print(n = 92)

png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/AP/9AP_indicesocio.png", width = 6, height = 4.5, units = "in", res = 300)
ggplot(AP.prop_INDICE_SOCIO_POLI[44:86,], aes(x = INDICE_SOCIO_POLI, y = prop * 100)) + 
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  labs(x = "Índice Socioeconômico", y = "Proporção de Palatalização") +
  theme_minimal()
dev.off()

AP.mod_INDICE_SOCIO_POLI <- glm(VD ~ INDICE_SOCIO_POLI, data = dados_AP, family = binomial)
summary(AP.mod_INDICE_SOCIO_POLI)
lrm(VD ~ INDICE_SOCIO_POLI, data = dados_AP)
plot(allEffects(AP.mod_INDICE_SOCIO_POLI), type = "response") 



# 1 MODELAGEM DE BARBOSA(2023) ####
sort(unique(dados_AP$IDADE_MIGRACAO))
dados_AP$PARTICIPANTE[which(dados_AP$IDADE_MIGRACAO == 45)]
dados_AP

modAP1 <- glmer(VD ~ TONICIDADE + 
                 POSICAO_S +
                 CFP_abertura2 +
                 CLASSE_MORFOLOGICA3 + 
                 GENERO + 
                 TEMPO_RESIDENCIA + 
                 IDADE_MIGRACAO + 
                 (1|ITEM_LEXICAL) +
                 (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(modAP1)
lrm(VD ~ TONICIDADE + 
      POSICAO_S +
      CFP_abertura2 +
      CLASSE_MORFOLOGICA3 + 
      GENERO + 
      TEMPO_RESIDENCIA + 
      IDADE_MIGRACAO, data = dados_AP)

addmargins(table(dados_AP$TONICIDADE, dados_AP$VD))
addmargins(table(dados_AP$POSICAO_S, dados_AP$VD))
addmargins(table(dados_AP$CFP_abertura2, dados_AP$VD))
addmargins(table(dados_AP$CLASSE_MORFOLOGICA3, dados_AP$VD))
addmargins(table(dados_AP$GENERO, dados_AP$VD))

car::vif(modAP1)
check_model(modAP1)
check_outliers(modAP1)
# R² marginal e condicional
r.squaredGLMM(modAP1)

# 2 MODELAGEM DE POLI INDICE SOCIO OUSHIRO ####
modAP2 <- glmer(VD ~ TONICIDADE + 
                  POSICAO_S +
                  CFP_abertura2 +
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
      CFP_abertura2 +
      CLASSE_MORFOLOGICA3 + 
      GENERO + 
      TEMPO_RESIDENCIA + 
      IDADE_MIGRACAO +
      INDICE_SOCIO_OUSHIRO, data = dados_AP)

car::vif(modAP2)
check_model(modAP2)
check_outliers(modAP2)
r.squaredGLMM(modAP2)


# 3 MODELAGEM DE POLI INDICE SOCIO POLI ####
modAP3 <- glmer(VD ~ TONICIDADE + 
                  POSICAO_S +
                  CFP_abertura2 +
                  CLASSE_MORFOLOGICA3 + 
                  GENERO + 
                  TEMPO_RESIDENCIA + 
                  IDADE_MIGRACAO +
                  INDICE_SOCIO_POLI +
                  (1|ITEM_LEXICAL) +
                  (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(modAP3)
lrm(VD ~ TONICIDADE + 
      POSICAO_S +
      CFP_abertura2 +
      CLASSE_MORFOLOGICA3 + 
      GENERO + 
      TEMPO_RESIDENCIA + 
      IDADE_MIGRACAO +
      INDICE_SOCIO_POLI, data = dados_AP)

car::vif(modAP3)
check_model(modAP3)
check_outliers(modAP3)
r.squaredGLMM(modAP3)

addmargins(table(dados_AP$TONICIDADE, dados_AP$VD))
addmargins(table(dados_AP$POSICAO_S, dados_AP$VD))
addmargins(table(dados_AP$CFP_abertura2, dados_AP$VD))
addmargins(table(dados_AP$CLASSE_MORFOLOGICA3, dados_AP$VD))
addmargins(table(dados_AP$GENERO, dados_AP$VD))


AP.comparacao <- plot(compare_performance(modAP2, modAP3))



# 4 MODELAGEM DE POLI INDICE SOCIO POLI sem CFP CLASSE MORFO####
modAP4 <- glmer(VD ~ TONICIDADE + 
                  POSICAO_S +
                  #CFP_abertura +
                  #CLASSE_MORFOLOGICA3 + 
                  GENERO + 
                  TEMPO_RESIDENCIA + 
                  IDADE_MIGRACAO +
                  INDICE_SOCIO_POLI +
                  (1|ITEM_LEXICAL) +
                  (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(modAP4)
lrm(VD ~ TONICIDADE + 
      POSICAO_S +
      #CFP_abertura +
      CLASSE_MORFOLOGICA3 + 
      GENERO + 
      TEMPO_RESIDENCIA + 
      IDADE_MIGRACAO +
      INDICE_SOCIO_POLI, data = dados_AP)

car::vif(modAP4)
check_model(modAP4)
check_outliers(modAP4)
r.squaredGLMM(modAP4)



## interações ####
idade_migracao_indice <- glmer(VD ~ IDADE_MIGRACAO * INDICE_SOCIO_POLI +
                               (1|ITEM_LEXICAL) +
                               (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(idade_migracao_indice)
plot(allEffects(idade_migracao_indice))


# IDADE MIGRAÇAO X TEMPO DE RESIDENCIA
idade_migracao_tempo_residencia <- glmer(VD ~ IDADE_MIGRACAO * TEMPO_RESIDENCIA +
                                         (1|ITEM_LEXICAL) +
                                         (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(idade_migracao_tempo_residencia)
plot(allEffects(idade_migracao_tempo_residencia))

#TEMPO DE RESIDENCIA X INDICE SOCIO

dados_AP$INDICE_SOCIOECONOMICO <- dados_AP$INDICE_SOCIO_POLI

tempo_residencia_indicesocio <- glmer(VD ~ TEMPO_RESIDENCIA * INDICE_SOCIOECONOMICO +
                                    (1|ITEM_LEXICAL) +
                                    (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(tempo_residencia_indicesocio)


png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/AP/10AP_interacao.png", width = 9, height = 13, units = "in", res = 300)
plot(allEffects(tempo_residencia_indicesocio),      
     xlab = "Tempo de Residência",
     ylab = "Proporção de Palatalização",
     main = "Efeito da Idade de Migração por Índice Socioeconômico")
dev.off()


  #IDADE_MIGRACAO X INDICE SOCIO
idade_migracao_indicesocio_oushiro <- glmer(VD ~ IDADE_MIGRACAO * INDICE_SOCIO_OUSHIRO +
                                      (1|ITEM_LEXICAL) +
                                      (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(idade_migracao_indicesocio_oushiro)
plot(allEffects(idade_migracao_indicesocio_oushiro),
     xlab = "Idade de Migração",
     ylab = "Proporção de Palatalização",
     main = "Efeito da Idade de Migração por Índice Socioeconômico",
)

#IDADE MIGRACAO X ESCOLARIDADE
idade_migracao_escolaridade <- glmer(VD ~ IDADE_MIGRACAO * ESCOLARIDADE2 +
                (1|ITEM_LEXICAL) +
                (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(idade_migracao_escolaridade)



#IDADE MIGRACAO X ESCOLARIDADE
tempo_residencia_escolaridade <- glmer(VD ~ TEMPO_RESIDENCIA * ESCOLARIDADE2 +
                                       (1|ITEM_LEXICAL) +
                                       (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(tempo_residencia_escolaridade)



# GRAFICOS JUNTOS ####

AP.tonicidade <- ggplot(AP.prop_TONICIDADE, aes(x = TONICIDADE, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Tonicidade", y = "Proporção de Ocorrência (%)") + 
  scale_x_discrete(labels = c("Átona", "Tônica"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variável \nResposta", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        legend.position = "none"
        )

AP.cfp <- ggplot(AP.prop_CFP_abertura2, aes(x = CFP_abertura2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Contexto Fonológico Precedente", y = "Proporção de Ocorrência (%)") + 
  scale_x_discrete(labels = c("Fechada", "Meio fechada", "Meio aberta", "Aberta"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variável \nResposta", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
    legend.position = "none")

AP.classemofo <- ggplot(AP.prop_CLASSE_MORFOLOGICA3, aes(x = CLASSE_MORFOLOGICA3, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Classe Morfológica", y = "Proporção de Ocorrência (%)") + 
  scale_x_discrete(labels = c("Gramatical", "Lexical"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variável \nResposta", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25))

AP.semcorrelacao <- (AP.tonicidade | AP.cfp | AP.classemofo) + 
  plot_layout(guides = "collect")
AP.semcorrelacao

ggsave(filename = "C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/AP/AP_semcorrelacao.png",
  plot = AP.semcorrelacao,
  width = 13,
  height = 4.5,
  units = "in",
  dpi = 300
)



# INDICE SOCIOECONOMICO ####
## Escolaridade ####
AP.prop_ESCOLARIDADE2 <- dados_AP %>%
  filter(!is.na(ESCOLARIDADE2), ESCOLARIDADE2 != "NA") %>% 
  count(VD, ESCOLARIDADE2) %>%
  group_by(ESCOLARIDADE2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

(g.AP.prop_ESCOLARIDADE2 <- ggplot(AP.prop_ESCOLARIDADE2, aes(x = ESCOLARIDADE2, y = prop * 100, color = VD, group = VD, label = label)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    geom_text(size = 3.5, color = "Black") +
    scale_y_continuous(limits = c(0, 100)) +
    labs(title = "Palatalização N = 3.076", x = "Escolaridade", y = "Proporção de Ocorrência (%)") + 
    scale_x_discrete(labels = c("E. Fundamental", "E. Médio", "E. Superior"))+
    scale_color_brewer(palette = "Reds", name = "Variável Resposta", labels = c("Alveolar","Palatal"))+
    theme_minimal()+
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "top"))




(AP.tab_ESCOLARIDADE2 <- with(dados_AP, table(ESCOLARIDADE2, VD)))
chisq.test(AP.tab_ESCOLARIDADE2)
chisq.test(AP.tab_ESCOLARIDADE2[c(2,3),])

#analise com efeitos mistos
AP.mod_escolaridade <-  glmer(VD ~ ESCOLARIDADE2 +
                              (1|ITEM_LEXICAL) +
                              (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_escolaridade)
addmargins(table(dados_AP$ESCOLARIDADE2, dados_AP$VD))


lrm(VD ~ ESCOLARIDADE2, data = dados_AP)
plot(allEffects(AP.mod_escolaridade), type = "response")


#escolaridade1
#chisq.test(AP.tab_ESCOLARIDADE[c(1,2),]) #sem diferença pra fund1 e 2
#chisq.test(AP.tab_ESCOLARIDADE[c(4,5),]) #sem diferença pra superior e pósgrad 
### Escolaridade dos Pais ####
#### Pai ####
AP.prop_ESCOLA_PAI2 <- dados_AP %>%
  filter(!is.na(ESCOLA_PAI2), ESCOLA_PAI2 != "NA") %>% 
  count(VD, ESCOLA_PAI2) %>%
  group_by(ESCOLA_PAI2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

(g.AP.prop_ESCOLA_PAI2 <- ggplot(AP.prop_ESCOLA_PAI2, aes(x = ESCOLA_PAI2, y = prop * 100, color = VD, group = VD, label = label)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    geom_text(size = 3.5, color = "Black") +
    scale_y_continuous(limits = c(0, 100)) +
    labs(title = "Palatalização N = 3.076", x = "Escolaridade - Pai", y = "Proporção de Ocorrência (%)") + 
    scale_x_discrete(labels = c("Analfabeto", "E. Fundamental", "E. Médio/Superior"))+
    scale_color_brewer(palette = "Reds", name = "Variável Resposta", labels = c("Alveolar","Palatal"))+
    theme_minimal()+
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "top"))



(AP.tab_ESCOLA_PAI2<- with(dados_AP, table(ESCOLA_PAI2, VD)))
chisq.test(AP.tab_ESCOLA_PAI2) #sim
chisq.test(AP.tab_ESCOLA_PAI2[c(1,2),]) 
chisq.test(AP.tab_ESCOLA_PAI2[c(2,3),]) 

#analise com efeitos mistos
AP.mod_escolaridade_pai <-  glmer(VD ~ ESCOLA_PAI2 +
                                (1|ITEM_LEXICAL) +
                                (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_escolaridade_pai)
addmargins(table(dados_AP$ESCOLA_PAI2, dados_AP$VD))

lrm(VD ~ ESCOLA_PAI2, data = dados_AP)
plot(allEffects(AP.mod_escolaridade_pai), type = "response")


#### Mãe ####
AP.prop_ESCOLA_MAE2 <- dados_AP %>%
  filter(!is.na(ESCOLA_MAE2), ESCOLA_MAE2 != "NA") %>% 
  count(VD, ESCOLA_MAE2) %>%
  group_by(ESCOLA_MAE2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

(g.AP.prop_ESCOLA_MAE2 <- ggplot(AP.prop_ESCOLA_MAE2, aes(x = ESCOLA_MAE2, y = prop * 100,color = VD, group = VD, label = label)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    geom_text(size = 3.5, color = "Black") +
    labs(title = "Palatalização N = 3.076", x = "Escolaridade - Mãe", y = "Proporção de Ocorrência (%)") + 
    scale_x_discrete(labels = c("Analfabeta", "E. Fundamental", "E. Médio/Superior"))+
    scale_color_brewer(palette = "Reds", name = "Variável Resposta", labels = c("Alveolar","Palatal"))+
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))


AP.grafico_escolaridade_mae <- AP.prop_ESCOLA_MAE2 %>% 
  filter(VD == "P") %>% 
  ggplot(aes(x = ESCOLA_MAE2, y = prop * 100, group = VD, color = VD, label = label)) + 
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(size = 3.5, vjust = -0.5) +
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
addmargins(table(dados_AP$ESCOLA_MAE2, dados_AP$VD))
lrm(VD ~ ESCOLA_MAE2, data = dados_AP)
plot(allEffects(AP.mod_escolaridade_mae), type = "response")


### Ocupação ####
AP.prop_INDICE_OCUPACAO <- dados_AP %>%
  count(VD, INDICE_OCUPACAO) %>%
  group_by(INDICE_OCUPACAO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

class(dados_AP$INDICE_OCUPACAO)


(g.AP.prop_INDICE_OCUPACAO <- ggplot(AP.prop_INDICE_OCUPACAO,aes(x = INDICE_OCUPACAO,y = prop * 100,color = VD, group = VD, label = label)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  geom_text(size = 3.5, color = "Black") +
  labs(title = "Palatalização N = 3.076", x = "Índice de Ocupação", y = "Proporção (%)", color = "Variável \nResposta") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_brewer(palette = "Reds", label = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))



AP.mod_INDICE_OCUPACAO <- glmer(VD ~ INDICE_OCUPACAO+
                                (1|ITEM_LEXICAL) +
                                (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_INDICE_OCUPACAO)
lrm(VD ~ INDICE_OCUPACAO, data = dados_AP)
plot(allEffects(AP.mod_INDICE_OCUPACAO), type = "response")

### Ocupação dos Pais ####
#### Pai ###
AP.prop_INDICE_OCUPACAO_PAI <- dados_AP %>%
  count(VD, INDICE_OCUPACAO_PAI) %>%
  group_by(INDICE_OCUPACAO_PAI) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_INDICE_OCUPACAO_PAI, aes(x = INDICE_OCUPACAO_PAI, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Índice Ocupação do Pai", y = "Proporção de Ocorrência (%)") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(
      color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(
      color = alpha("gray85", 0.1), linewidth = 0.25),
    
  )

AP.mod_INDICE_OCUPACAO_PAI <- glmer(VD ~ INDICE_OCUPACAO_PAI+
                                      (1|ITEM_LEXICAL) +
                                      (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_INDICE_OCUPACAO_PAI)
lrm(VD ~ INDICE_OCUPACAO_PAI, data = dados_AP)
plot(allEffects(AP.mod_INDICE_OCUPACAO_PAI), type = "response")


#### Mãe ###
AP.prop_INDICE_OCUPACAO_MAE <- dados_AP %>%
  count(VD, INDICE_OCUPACAO_MAE) %>%
  group_by(INDICE_OCUPACAO_MAE) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_INDICE_OCUPACAO_MAE, aes(x = INDICE_OCUPACAO_MAE, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Índice de Ocupação da Mãe", y = "Proporção de Ocorrência (%)") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(
      color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(
      color = alpha("gray85", 0.1), linewidth = 0.25),
    
  )

AP.mod_INDICE_OCUPACAO_MAE <- glmer(VD ~ INDICE_OCUPACAO_MAE+
                                      (1|ITEM_LEXICAL) +
                                      (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_INDICE_OCUPACAO_MAE)
lrm(VD ~ INDICE_OCUPACAO_MAE, data = dados_AP)
plot(allEffects(AP.mod_INDICE_OCUPACAO_MAE), type = "response")


### Ocupação outro cargo ####
AP.prop_INDICE_OUTRO_CARGO <- dados_AP %>%
  count(VD, INDICE_OUTRO_CARGO) %>%
  group_by(INDICE_OUTRO_CARGO) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_INDICE_OUTRO_CARGO, aes(x = INDICE_OUTRO_CARGO, y = prop * 100, fill = VD, label = label)) +
  geom_bar(stat = "identity", color = "white") + 
 # labs(x = "Outro Cargo", y = "Proporção de Ocorrência (%)") + 
#  scale_x_discrete(labels = c("Não tem", "Tem", "Não se aplica"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
   
    )

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
  filter(!is.na(INDICE_OCUPACAO_SONHOS), INDICE_OCUPACAO_SONHOS != "NA") %>% 
  count(VD, INDICE_OCUPACAO_SONHOS) %>%
  group_by(INDICE_OCUPACAO_SONHOS) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

(g.AP.prop_INDICE_OCUPACAO_SONHOS <- ggplot(AP.prop_INDICE_OCUPACAO_SONHOS, aes(x = INDICE_OCUPACAO_SONHOS, y = prop * 100,color = VD, group = VD, label = label)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    geom_text(size = 3.5, color = "Black") +
    labs(title = "Palatalização N = 3.076", x = "Ocupação dos Sonhos", y = "Proporção de Ocorrência (%)") + 
    scale_x_discrete(labels = c("Nenhuma", "Ocupações \nintermediárias", "Ocupações \ncom especialização"))+
    scale_color_brewer(palette = "Reds", name = "Variável Resposta", labels = c("Alveolar","Palatal"))+
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))




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
  #labs(x = "Renda Individual", y = "Proporção de Ocorrência (%)") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(
      color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(
      color = alpha("gray85", 0.1), linewidth = 0.25),
   
    )


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
  #labs(x = "Renda Individual", y = "Proporção de Ocorrência (%)") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(
      color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(
      color = alpha("gray85", 0.1), linewidth = 0.25),
   
    )


(AP.tab_OCUPACAO_LOCOMOCAO2 <- with(dados_AP, table(OCUPACAO_LOCOMOCAO2, VD)))
chisq.test(AP.tab_OCUPACAO_LOCOMOCAO2) #sim

#teste efeitos mistos
AP.mod_ocupacao_locomocao <- glmer(VD ~ OCUPACAO_LOCOMOCAO2+
                                  (1|ITEM_LEXICAL) +
                                  (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_ocupacao_locomocao)
lrm(VD ~ OCUPACAO_LOCOMOCAO2, data = dados_AP)
plot(allEffects(AP.mod_ocupacao_locomocao), type = "response")





### Mega sena ####
AP.prop_MEGA_SENA2 <- dados_AP %>%
  filter(!is.na(MEGA_SENA2), MEGA_SENA2 != "NA") %>% 
  count(VD, MEGA_SENA2) %>%
  group_by(MEGA_SENA2) %>% 
  mutate(prop = prop.table(n),
         MEGA_SENA2 = fct_relevel(MEGA_SENA2, "gastar", "voltar.estado", "ajudar.outros", "investir"),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

(g.AP.prop_MEGA_SENA2 <- ggplot(AP.prop_MEGA_SENA2, aes(x = MEGA_SENA2, y = prop * 100,color = VD, group = VD, label = label)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    geom_text(size = 3.5, color = "Black") +
    labs(title = "Palatalização N = 3.076", x = "Mega-Sena", y = "Proporção de Ocorrência (%)") + 
    scale_x_discrete(labels = c("Gastar", "Voltar para o \nestado de origem", "Ajudar outras \npessoas", "Investir"))+
    scale_color_brewer(palette = "Reds", name = "Variável Resposta", labels = c("Alveolar","Palatal"))+
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))


(AP.tab_MEGA_SENA2 <- with(dados_AP, table(MEGA_SENA2, VD)))
chisq.test(AP.tab_MEGA_SENA2) #sim


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
  #labs(x = "Renda Individual", y = "Proporção de Ocorrência (%)") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(
      color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(
      color = alpha("gray85", 0.1), linewidth = 0.25),
   
    )


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
  filter(!is.na(RENDA_IND), RENDA_IND != "NA") %>% 
  count(VD, RENDA_IND) %>%
  group_by(RENDA_IND) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

(g.AP.prop_RENDA_IND <- ggplot(AP.prop_RENDA_IND, aes(x = RENDA_IND, y = prop * 100,color = VD, group = VD, label = label)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    geom_text(size = 3.5, color = "Black") +
    labs(title = "Palatalização N = 3.076", x = "Renda Individual", y = "Proporção de Ocorrência (%)") + 
    scale_x_discrete(labels = c("Até 1 SM", "1 a 2 SM", "2 a 4 SM", "Mais de 4 SM"))+
    scale_color_brewer(palette = "Reds", name = "Variável Resposta", labels = c("Alveolar","Palatal"))+
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))


(AP.tab_RENDA_IND <- with(dados_AP, table(RENDA_IND, VD)))
chisq.test(AP.tab_RENDA_IND) #tem correlação


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
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência (%)") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
   
    )


(AP.tab_RENDA_FAM <- with(dados_AP, table(RENDA_FAM, VD)))
chisq.test(AP.tab_RENDA_FAM) #tem correlação


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
  theme_minimal()


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
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência (%)") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  coord_flip() +  # Barras horizontais
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
   
    )



#### Região ####
AP.prop_BAIRRO_REGIAO <- dados_AP %>%
  count(VD, BAIRRO_REGIAO2) %>%
  group_by(BAIRRO_REGIAO2) %>% 
  mutate(prop = prop.table(n),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

ggplot(AP.prop_BAIRRO_REGIAO, aes(x = BAIRRO_REGIAO2, y = prop * 100, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Região", y = "Proporção de Ocorrência (%)") + 
  scale_x_discrete(labels = c("Centro", "Periferia Norte", "Periferia Sul"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
   
    )


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
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência (%)") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
   
    )


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
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência (%)") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
   
    )


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
plot(allEffects(AP.mod_DENSIDADE_HABITACAO), type = "response")


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
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência (%)") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
   
    )


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
  labs(x = "Renda Familiar", y = "Proporção de Ocorrência (%)") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
   
    )


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
  #labs(x = "Renda Familiar", y = "Proporção de Ocorrência (%)") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "variantes", labels = c("Alveolar", "Palatal"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
    panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
   
    )


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
  filter(!is.na(LAZER_CARACTERISTICA), LAZER_CARACTERISTICA != "NA") %>% 
  count(VD, LAZER_CARACTERISTICA) %>%
  group_by(LAZER_CARACTERISTICA) %>% 
  mutate(prop = prop.table(n),
         LAZER_CARACTERISTICA = fct_relevel(LAZER_CARACTERISTICA, "nao.sai", "sem.custo", "custo"),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()

(g.AP.prop_LAZER_CARACTERISTICA <- ggplot(AP.prop_LAZER_CARACTERISTICA, aes(x = LAZER_CARACTERISTICA, y = prop * 100,color = VD, group = VD, label = label)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    geom_text(size = 3.5, color = "Black") +
    labs(title = "Palatalização N = 3.076", x = "Lazer", y = "Proporção de Ocorrência (%)") + 
    scale_x_discrete(labels = c("Não sai", "Sem custo \nfinanceiro", "Com custo \nfinanceiro"))+
    scale_color_brewer(palette = "Reds", name = "Variável Resposta", labels = c("Alveolar","Palatal"))+
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))


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
  filter(!is.na(LAZER_CAMPINAS_CARACTERISTICA), LAZER_CAMPINAS_CARACTERISTICA != "NA") %>% 
  count(VD, LAZER_CAMPINAS_CARACTERISTICA) %>%
  group_by(LAZER_CAMPINAS_CARACTERISTICA) %>% 
  mutate(prop = prop.table(n),
        LAZER_CAMPINAS_CARACTERISTICA = fct_relevel(LAZER_CAMPINAS_CARACTERISTICA, "nsai.ntem", "sem.custo", "custo"),
       label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



(g.AP.prop_LAZER_CAMPINAS_CARACTERISTICA <- ggplot(AP.prop_LAZER_CAMPINAS_CARACTERISTICA, aes(x = LAZER_CAMPINAS_CARACTERISTICA, y = prop * 100,color = VD, group = VD, label = label)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    geom_text(size = 3.5, color = "Black") +
    labs(title = "Palatalização N = 3.076", x = "Lazer em Campinas", y = "Proporção de Ocorrência (%)") + 
    scale_x_discrete(labels = c("Não sai/ \nNão tem", "Sem custo \nfinanceiro", "Com custo \nfinanceiro"))+
    scale_color_brewer(palette = "Reds", name = "Variável Resposta", labels = c("Alveolar","Palatal"))+
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))





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
  filter(!is.na(VIAGEM), VIAGEM != "NA") %>% 
  count(VD, VIAGEM) %>%
  group_by(VIAGEM) %>% 
  mutate(prop = prop.table(n),
         VIAGEM = fct_relevel(VIAGEM, "nao", "sim"),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



(g.AP.prop_VIAGEM <- ggplot(AP.prop_VIAGEM, aes(x = VIAGEM, y = prop * 100,color = VD, group = VD, label = label)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    geom_text(size = 3.5, color = "Black") +
    labs(title = "Palatalização N = 3.076", x = "Hábitos de Viagem", y = "Proporção de Ocorrência (%)") + 
    scale_x_discrete(labels = c("Não tem costume de viajar", "Tem costume de viajar"))+
    scale_color_brewer(palette = "Reds", name = "Variável Resposta", labels = c("Alveolar","Palatal"))+
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))





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
  filter(!is.na(VIAGEM_LUGAR), VIAGEM_LUGAR != "NA") %>% 
  count(VD, VIAGEM_LUGAR) %>%
  group_by(VIAGEM_LUGAR) %>% 
  mutate(prop = prop.table(n),
         VIAGEM_LUGAR = fct_relevel(VIAGEM_LUGAR, "SP-estado", "nacional.internacional"),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



(g.AP.prop_VIAGEM_LUGAR <- ggplot(AP.prop_VIAGEM_LUGAR, aes(x = VIAGEM_LUGAR, y = prop * 100,color = VD, group = VD, label = label)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    geom_text(size = 3.5, color = "Black") +
    labs(title = "Palatalização N = 3.076", x = "Tipo de Viagem", y = "Proporção de Ocorrência (%)") + 
    scale_x_discrete(labels = c("Estado de São Paulo/\n estado de origem", "Nacional/internacional"))+
    scale_color_brewer(palette = "Reds", name = "Variável Resposta", labels = c("Alveolar","Palatal"))+
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))


(AP.tab_VIAGEM_LUGAR <- with(dados_AP, table(VIAGEM_LUGAR, VD)))
chisq.test(AP.tab_VIAGEM_LUGAR) #tem correlação

#teste efeitos mistos
AP.mod_VIAGEM_LUGAR <- glmer(VD ~ VIAGEM_LUGAR +
                         (1|ITEM_LEXICAL) +
                         (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_VIAGEM_LUGAR)
lrm(VD ~ VIAGEM_LUGAR, data = dados_AP)
plot(allEffects(AP.mod_VIAGEM_LUGAR), type = "response")



### Viagem vontade ####
AP.prop_LAZER_VIAGEM_VONTADE2 <- dados_AP %>%
  filter(!is.na(LAZER_VIAGEM_VONTADE2), LAZER_VIAGEM_VONTADE2 != "NA") %>% 
  count(VD, LAZER_VIAGEM_VONTADE2) %>%
  group_by(LAZER_VIAGEM_VONTADE2) %>% 
  mutate(prop = prop.table(n),
         LAZER_VIAGEM_VONTADE2 = fct_relevel(LAZER_VIAGEM_VONTADE2, "nenhum", "nacional","nacional.internacional"),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



(g.AP.prop_LAZER_VIAGEM_VONTADE2 <- ggplot(AP.prop_LAZER_VIAGEM_VONTADE2, aes(x = LAZER_VIAGEM_VONTADE2, y = prop * 100,color = VD, group = VD, label = label)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    geom_text(size = 3.5, color = "Black") +
    labs(title = "Palatalização N = 3.076", x = "Gostaria de conhecer", y = "Proporção de Ocorrência (%)") + 
    scale_x_discrete(labels = c("Nenhum lugar", "Destinos nacionais", "Destinos nacionais \ne internacionais"))+
    scale_color_brewer(palette = "Reds", name = "Variável Resposta", labels = c("Alveolar","Palatal"))+
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))


(AP.tab_LAZER_VIAGEM_VONTADE2 <- with(dados_AP, table(LAZER_VIAGEM_VONTADE2, VD)))
chisq.test(AP.tab_LAZER_VIAGEM_VONTADE2) #tem correlação

#teste efeitos mistos
AP.mod_LAZER_VIAGEM_VONTADE2 <- glmer(VD ~ LAZER_VIAGEM_VONTADE2 +
                         (1|ITEM_LEXICAL) +
                         (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_LAZER_VIAGEM_VONTADE2)
lrm(VD ~ LAZER_VIAGEM_VONTADE2, data = dados_AP)
plot(allEffects(AP.mod_LAZER_VIAGEM_VONTADE2), type = "response")



### Infancia ####
AP.prop_INFANCIA_MEMORIA <- dados_AP %>%
  filter(!is.na(INFANCIA_MEMORIA), INFANCIA_MEMORIA != "NA") %>% 
  count(VD, INFANCIA_MEMORIA) %>%
  group_by(INFANCIA_MEMORIA) %>% 
   mutate(prop = prop.table(n),
  #        LAZER_VIAGEM_VONTADE2 = fct_relevel(LAZER_VIAGEM_VONTADE2, "nenhum", "nacional","nacional.internacional"),
         label = paste0(round(prop * 100, 1), "%\n(", n, ")")) %>% 
  print()



(g.AP.prop_INFANCIA_MEMORIA <- ggplot(AP.prop_INFANCIA_MEMORIA, aes(x = INFANCIA_MEMORIA, y = prop * 100,color = VD, group = VD, label = label)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    geom_text(size = 3.5, color = "Black") +
    labs(title = "Palatalização N = 3.076", x = "Memória de Infância", y = "Proporção de Ocorrência (%)") + 
    scale_x_discrete(labels = c("Negativa", "Neutra", "Positiva"))+
    scale_color_brewer(palette = "Reds", name = "Variável Resposta", labels = c("Alveolar","Palatal"))+
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top"))


(AP.prop_INFANCIA_MEMORIA <- with(dados_AP, table(INFANCIA_MEMORIA, VD)))
chisq.test(AP.prop_INFANCIA_MEMORIA) #tem correlação


#teste efeitos mistos
AP.mod_INFANCIA_MEMORIA <- glmer(VD ~ INFANCIA_MEMORIA +
                                        (1|ITEM_LEXICAL) +
                                        (1|PARTICIPANTE), data = dados_AP, family = binomial)
summary(AP.mod_INFANCIA_MEMORIA)
lrm(VD ~ INFANCIA_MEMORIA, data = dados_AP)
plot(allEffects(AP.mod_INFANCIA_MEMORIA), type = "response")


# FEATURE SELECTION - LASSO ####
escalas_lasso_AP <- dados_AP %>%
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
    INDICE_INFANCIA_norm,
    PARTICIPANTE,
    ITEM_LEXICAL
  ) %>%
  na.omit()


modelo_lasso_misto <- glmmLasso(
  VD ~ INDICE_ESCOL3_norm + 
    PAIS + 
    INDICE_OCUPACAO_norm +
    INDICE_OCUPACAO_SONHOS2_norm + 
    INDICE_MEGA_norm +
    INDICE_RENDA_IND_norm + 
    LAZER + 
    VIAGEM + 
    INDICE_INFANCIA_norm,
  rnd = list(PARTICIPANTE = ~1, ITEM_LEXICAL = ~1),
  lambda = 10, # ajustar via validação
  family = gaussian(link = "identity"),
  data = escalas_lasso_AP)

x_AP <- model.matrix(VD ~ ., escalas_lasso_AP)[, -1]
y_AP <- escalas_lasso_AP$VD

lasso_AP <- cv.glmnet(x_AP, y_AP, alpha = 1)
COEF_AP <- coef(lasso_AP, s = "lambda.min")
COEF_AP


plot(lasso_AP$glmnet.fit, xvar = "lambda", label = TRUE) 

