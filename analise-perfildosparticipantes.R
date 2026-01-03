#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%# PERFIL DOS PARTICIPANTES #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 15/07/2025 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% V1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

View(infs2)


#DISTRIBUIÇÃO POR PARTICIPANTE ####
## ESCOLARIDADE ####
escolaridade_participante <- infs2 %>% 
  distinct(PARTICIPANTE, ESCOLARIDADE2) %>%  #garante 1 linha por participante
  count(ESCOLARIDADE2) %>%
  print()

(g.escolaridade <- escolaridade_participante %>% 
  ggplot(aes(x = ESCOLARIDADE2, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Escolaridade",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 25))+
  scale_x_discrete(labels = c("Fundamental", "Médio", "Superior"))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))



### Escolaridade dos pais ####
escolaridade_pai <- dados2 %>% 
  distinct(PARTICIPANTE, ESCOLA_PAI2) %>%  #garante 1 linha por participante
  count(ESCOLA_PAI2) %>% 
  mutate(ESCOLA_PAI2 = fct_explicit_na(ESCOLA_PAI2, "Não informado")) %>% 
  arrange(ESCOLA_PAI2) %>% 
  print()

(g.escolaridadepai <- escolaridade_pai %>% 
  ggplot(aes(x = factor(ESCOLA_PAI2), y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Escolaridade - Pai",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_fill_brewer(palette = "Reds")+
  scale_x_discrete(labels=c("Analfabeto", "Fundamental", "Médio/\nSuperior", " não informado"))+
  scale_y_continuous(limits = c(0, 25))+
  theme_minimal()+
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))

escolaridade_mae <- dados2 %>% 
  distinct(PARTICIPANTE, ESCOLA_MAE2) %>%  #garante 1 linha por participante
  count(ESCOLA_MAE2) %>% 
  mutate(ESCOLA_MAE2 = fct_explicit_na(ESCOLA_MAE2, "Não informado")) %>% 
  arrange(ESCOLA_MAE2) %>% 
  print()

(g.escolaridademae <- escolaridade_mae %>% 
  ggplot(aes(x = factor(ESCOLA_MAE2), y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Escolaridade - Mãe",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_fill_brewer(palette = "Reds")+
  scale_x_discrete(labels=c("Analfabeto", "Fundamental", "Médio/\nSuperior", " não informado"))+
  scale_y_continuous(limits = c(0, 25))+
  theme_minimal()+
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))


#### grafico 3 escolaridades ####
g.escolaridades <- (g.escolaridade | g.escolaridadepai | g.escolaridademae) + 
  plot_layout(guides = "collect")
g.escolaridades

ggsave(filename = "C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/_N/1escolaridade.png",
       plot = g.escolaridades,
       width = 15,
       height = 5,
       units = "in",
       dpi = 300)




## OCUPAÇÃO ####
ocupacao <- infs2 %>% 
  distinct(PARTICIPANTE, INDICE_OCUPACAO) %>%  #garante 1 linha por participante
  count(INDICE_OCUPACAO) %>% 
  arrange(INDICE_OCUPACAO) %>% 
  print()


png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/_N/2ocupacao.png", width = 5, height = 5, units = "in", res = 300)
ocupacao %>%
  ggplot(aes(x = INDICE_OCUPACAO, y = n, label = n)) +
  geom_line(linewidth = 1.2, color = "#FCAE91") +
  geom_point(size = 3, color = "#FCAE91") +
  geom_text(aes(label = n),
            vjust = -0.8,
            size = 3.5) +
  scale_x_continuous(breaks = ocupacao$INDICE_OCUPACAO) +
  labs(
    x = "Ocupação",
    y = "Número de participantes") +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10))
dev.off()




## INDICE_OCUPACAO_PAI ####
ocupacaopai_participante <- infs2 %>%
  distinct(PARTICIPANTE, INDICE_OCUPACAO_PAI) %>%
  mutate(
    INDICE_OCUPACAO_PAI = as.factor(INDICE_OCUPACAO_PAI),
    INDICE_OCUPACAO_PAI = fct_explicit_na(INDICE_OCUPACAO_PAI, "Não informado")) %>%
  count(INDICE_OCUPACAO_PAI) %>%
  print()

(g.ocupacaopai <- ocupacaopai_participante %>%
  ggplot(aes(x = INDICE_OCUPACAO_PAI, y = n, label = n, fill = INDICE_OCUPACAO_PAI)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(
    x = "Ocupação - Pai",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 24))+
  scale_fill_brewer(palette = "Reds")+
  scale_x_discrete(labels=c("0. Desempregado/\nsem renda", "1. Trabalhador braçal \ns/ treinamento", "5. Profissionais \nespecializados/\nliberais", "Não informado"))+
  theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))


## INDICE_OCUPACAO_MAE ####
ocupacaomae_participante <- infs2 %>%
  distinct(PARTICIPANTE, INDICE_OCUPACAO_MAE) %>%
  mutate(
    INDICE_OCUPACAO_MAE = as.factor(INDICE_OCUPACAO_MAE),
    INDICE_OCUPACAO_MAE = fct_explicit_na(INDICE_OCUPACAO_MAE, "Não informado")) %>%
  count(INDICE_OCUPACAO_MAE)%>%
  print()

(g.ocupacaomae <-ocupacaomae_participante %>%
  ggplot(aes(x = INDICE_OCUPACAO_MAE, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(
    x = "Ocupação - Mãe",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 24))+
  scale_fill_brewer(palette = "Reds")+
  scale_x_discrete(labels=c("0. Desempregado/\nsem renda", "1. Trabalhador braçal \ns/ treinamento", "5. Profissionais \nespecializados/\nliberais", "Não informado"))+
  theme_minimal()+
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))


#### grafico 2 ocupacao ####
g.ocupacoes <- (g.ocupacaopai | g.ocupacaomae) + 
  plot_layout(guides = "collect")
g.ocupacoes

ggsave(filename = "C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/_N/3ocupacoes.png",
       plot = g.ocupacoes,
       width = 13,
       height = 5,
       units = "in",
       dpi = 300)



## RENDA_IND ####
rendaind_participante <- infs2 %>% 
  distinct(PARTICIPANTE, RENDA_IND) %>%  #garante 1 linha por participante
  count(RENDA_IND) %>%
  mutate(RENDA_IND = fct_explicit_na(RENDA_IND, "Não informado")) %>%  # Transforma NA em categoria
  print()

(g.rendaind <- rendaind_participante %>%
  ggplot(aes(x = RENDA_IND, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Renda Individual",
    y = "Número de Participantes")+
  scale_y_continuous(limits = c(0, 24))+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_fill_brewer(palette = "Reds")+
  scale_x_discrete(labels=c("Até 1 \nSM", "1 a 2 \nSM", "2 a 4 \nSM", "Mais de 4 \nSM", "Não informado"))+
  theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))


## RENDA_FAM ####
rendafam_participante <- infs2 %>% 
  distinct(PARTICIPANTE, RENDA_FAM) %>%  #garante 1 linha por participante
  count(RENDA_FAM) %>%
  mutate(RENDA_FAM = fct_explicit_na(RENDA_FAM, "Não informado")) %>%  # Transforma NA em categoria
  print()

(g.rendafam <- rendafam_participante %>%
  ggplot(aes(x = RENDA_FAM, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Renda Familiar",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  scale_y_continuous(limits = c(0, 24))+
  scale_x_discrete(labels=c("Até 1 \nSM", "1 a 2 \nSM", "2 a 4 \nSM", "Mais de 4 \nSM", "Não informado"))+
  theme_minimal()+
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))



#### grafico 2 renda ####
g.renda <- (g.rendaind | g.rendafam) + 
  plot_layout(guides = "collect")
g.renda


ggsave(filename = "C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/_N/4renda.png",
       plot = g.renda,
       width = 13,
       height = 5,
       units = "in",
       dpi = 300)


## REGIÃO ####
regiao_participante <- infs2 %>% 
  distinct(PARTICIPANTE, BAIRRO_REGIAO2) %>%  #garante 1 linha por participante
  count(BAIRRO_REGIAO2) %>%
  mutate(BAIRRO_REGIAO2 = fct_explicit_na(BAIRRO_REGIAO2, "Não informado")) %>%  # Transforma NA em categoria
  print()


png("C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/_N/5regiao.png", width = 5, height = 5, units = "in", res = 300)
regiao_participante %>%
  ggplot(aes(x = BAIRRO_REGIAO2, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Região",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_fill_brewer(palette = "Reds")+
  scale_x_discrete(labels = c("centro", "periferia", "não informado"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11))
dev.off()


## NQUARTOS ####
quartos_participante <- infs2 %>% 
  distinct(PARTICIPANTE, NQUARTOS) %>%  #garante 1 linha por participante
  count(NQUARTOS) %>%
  mutate(NQUARTOS = fct_explicit_na(NQUARTOS, "Não informado")) %>%  # Transforma NA em categoria
  print()

(g.quartos <- quartos_participante %>%
  ggplot(aes(x = NQUARTOS, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Número de Quartos",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 29))+
  scale_fill_brewer(palette = "Reds")+
  scale_x_discrete(labels = c("um", "dois", "três ou mais", "não informado"))+
  theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))


## NBANHEIROS ####
 banheiros_participante <- infs2 %>% 
  distinct(PARTICIPANTE, NBANHEIROS) %>%  #garante 1 linha por participante
  count(NBANHEIROS) %>%
  mutate(NBANHEIROS = fct_explicit_na(NBANHEIROS, "Não informado")) %>%  # Transforma NA em categoria
  print()

(g.banheiros <- banheiros_participante %>%
  ggplot(aes(x = NBANHEIROS, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Número de Banheiros",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 29))+
  scale_x_discrete(labels = c("um", "dois", "três ou mais", "não informado"))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))


## NCOMODOS####
comodos_participante <- infs2 %>% 
  distinct(PARTICIPANTE, NCOMODOS) %>%  #garante 1 linha por participante
  count(NCOMODOS) %>%
  mutate(NCOMODOS = fct_explicit_na(NCOMODOS, "Não informado")) %>%  # Transforma NA em categoria
  print()

(comodos_participante %>%
  ggplot(aes(x = NCOMODOS, y = n, label = n )) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(
    x = "Comodos",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_fill_brewer(palette = "Reds")+
  scale_y_continuous(limits = c(0, 29))+
  scale_x_discrete(labels = c("dois", "três", "quatro ou mais", "não informado"))+
  theme_minimal()+
    theme(axis.text.x = element_text(size = 10)))

## DENSIDADE_HABITACAO ####
densidade_participante <- infs2 %>%
  distinct(PARTICIPANTE, DENSIDADE_HABITACAO) %>%
  mutate(
    DENSIDADE_HABITACAO = as.character(DENSIDADE_HABITACAO),
    DENSIDADE_HABITACAO_CAT = case_when(
      DENSIDADE_HABITACAO %in% c("0.25", "0.5", "0.7", "0.75") ~ "menos de uma",
      DENSIDADE_HABITACAO == "1" ~ "uma",
      TRUE ~ "mais de uma"),
    DENSIDADE_HABITACAO_CAT = fct_relevel(DENSIDADE_HABITACAO_CAT, 
                                          "mais de uma", "uma", "menos de uma")
  ) %>%
  count(DENSIDADE_HABITACAO_CAT) %>% 
  print()


(g.densidade <- densidade_participante %>%
  ggplot(aes(x = DENSIDADE_HABITACAO_CAT, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Densidade Habitacional (Pessoa/cômodo)",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 29))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))



#### grafico 3 habitacao ####
g.habitacao <- (g.quartos | g.banheiros | g.densidade) + 
  plot_layout(guides = "collect")
g.habitacao


ggsave(filename = "C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/_N/6habitacao.png",
       plot = g.habitacao,
       width = 15,
       height = 5,
       units = "in",
       dpi = 300)





## LOCOMOÇÃO ####
locomocao_participante <- infs2 %>% 
  distinct(PARTICIPANTE, OCUPACAO_LOCOMOCAO2) %>%  #garante 1 linha por participante
  count(OCUPACAO_LOCOMOCAO2) %>%
  mutate(OCUPACAO_LOCOMOCAO2 = fct_explicit_na(OCUPACAO_LOCOMOCAO2, "Não informado")) %>%  # Transforma NA em categoria
  print()

(g.locomocao <- locomocao_participante %>%
  ggplot(aes(x = OCUPACAO_LOCOMOCAO2, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white",fill = "#FCAE91") +
  labs(x = "Tipo de Transporte",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 24))+
  scale_fill_brewer(palette = "Reds")+
  scale_x_discrete(labels= c("compartilhado/\npúblico", "privado", "não informado"))+
  theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))


## DISTÂNCIA ####
distancia_participante <- infs2 %>% 
  distinct(PARTICIPANTE, OCUPACAO_DIST) %>%  #garante 1 linha por participante
  count(OCUPACAO_DIST) %>%
  mutate(OCUPACAO_DIST = fct_relevel(OCUPACAO_DIST, "longe", "perto", "casa")) %>%
  print()

(g.distancia <- distancia_participante %>%
  ggplot(aes(x = OCUPACAO_DIST, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Distância do Trabalho",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 24))+
  scale_fill_brewer(palette = "Reds")+
  scale_x_discrete(labels=c("longe", "perto", "em casa", "não informado"))+
  theme_minimal()+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))

#### grafico 2 locomocao distancia ####
g.locomocaodistancia <- (g.locomocao | g.distancia) + 
  plot_layout(guides = "collect")
g.locomocaodistancia


ggsave(filename = "C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/_N/7locomocao.png",
       plot = g.locomocaodistancia,
       width = 13,
       height = 5,
       units = "in",
       dpi = 300)


## OUTRO CARGO ####
outrocargo_participante <- infs2 %>% 
  distinct(PARTICIPANTE, INDICE_OUTRO_CARGO) %>%  #garante 1 linha por participante
  count(INDICE_OUTRO_CARGO) %>%
  mutate(INDICE_OUTRO_CARGO = fct_explicit_na(INDICE_OUTRO_CARGO, "Não informado")) %>%  # Transforma NA em categoria
  print()

(g.outrocargo <- outrocargo_participante %>%
  ggplot(aes(x = INDICE_OUTRO_CARGO, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Ascensão de Cargo",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 25))+
  scale_fill_brewer(palette = "Reds")+
  scale_x_discrete(labels = c("sem perspectiva", "com perspectiva", "não informado"))+
  theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))



## OCUPAÇÃO DOS SONHOS ####
ocupacaosonhos_participante <- infs2 %>% 
  distinct(PARTICIPANTE, INDICE_OCUPACAO_SONHOS) %>%  #garante 1 linha por participante
  count(INDICE_OCUPACAO_SONHOS) %>%
  mutate(INDICE_OCUPACAO_SONHOS = fct_explicit_na(INDICE_OCUPACAO_SONHOS, "Não informado")) %>%  # Transforma NA em categoria
  print()

(g.ocupacaosonhos <- ocupacaosonhos_participante %>%
  ggplot(aes(x = INDICE_OCUPACAO_SONHOS, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Ocupação dos sonhos",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 25))+
  scale_fill_brewer(palette = "Reds")+
  scale_x_discrete(labels = c("nenhuma", "intermediárias", "com especialização", "não informado"))+
  theme_minimal()+
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))



#### grafico 2 locomocao distancia ####
g.ocupacaoascensao <- (g.outrocargo | g.ocupacaosonhos) + 
  plot_layout(guides = "collect")
g.ocupacaoascensao


ggsave(filename = "C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/_N/8ocupacaoascensao.png",
       plot = g.ocupacaoascensao,
       width = 13,
       height = 5,
       units = "in",
       dpi = 300)




## LAZER ####
lazer_participante <- infs2 %>% 
  distinct(PARTICIPANTE, LAZER_CARACTERISTICA) %>%  #garante 1 linha por participante
  mutate(LAZER_CARACTERISTICA = fct_relevel(LAZER_CARACTERISTICA, "nao.sai", "sem.custo", "custo")) %>%
  count(LAZER_CARACTERISTICA) %>%
  print()

(g.lazer <- lazer_participante %>%
  ggplot(aes(x = LAZER_CARACTERISTICA, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Lazer",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 29))+
  scale_fill_brewer(palette = "Reds")+
  scale_x_discrete(labels=c("não sai", "sem custo", "com custo", "não informado"))+
  theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))




## LAZER CAMPINAS ####
lazercampinas_participante <- infs2 %>% 
  distinct(PARTICIPANTE, LAZER_CAMPINAS_CARACTERISTICA) %>%  #garante 1 linha por participante
  mutate(LAZER_CAMPINAS_CARACTERISTICA = fct_relevel(LAZER_CAMPINAS_CARACTERISTICA, "nsai.ntem", "sem.custo", "custo")) %>%
  count(LAZER_CAMPINAS_CARACTERISTICA) %>%
  print()

(g.lazercampinas <- lazercampinas_participante %>%
  ggplot(aes(x = LAZER_CAMPINAS_CARACTERISTICA, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Lazer em Campinas",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 29))+
  scale_fill_brewer(palette = "Reds", name = "Lazer em Campinas")+
  scale_x_discrete(labels = c("não sai/ não tem", "sem custo", "com custo", "não informado"))+
  theme_minimal()+
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))


#### grafico 2 lazer ####
g.lazeres <- (g.lazer | g.lazercampinas) + 
  plot_layout(guides = "collect")
g.lazeres


ggsave(filename = "C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/_N/9lazer.png",
       plot = g.lazeres,
       width = 13,
       height = 5,
       units = "in",
       dpi = 300)




## VIAGEM ####
viagem_participante <- infs2 %>% 
  distinct(PARTICIPANTE, VIAGEM) %>%  #garante 1 linha por participante
  count(VIAGEM) %>%
  mutate(VIAGEM = fct_explicit_na(VIAGEM, "Não informado")) %>%  # Transforma NA em categoria
  print()

(g.viagem <- viagem_participante %>%
  ggplot(aes(x = VIAGEM, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Hábito de Viagem",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 29))+
  scale_fill_brewer(palette = "Reds")+
  scale_x_discrete(labels = c("não costuma viajar", "costuma de viajar")) +
  theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))


## VIAGEM LUGAR ####
viagemlugar_participante <- infs2 %>% 
  distinct(PARTICIPANTE, VIAGEM_LUGAR) %>%  #garante 1 linha por participante
  count(VIAGEM_LUGAR) %>%
  mutate(VIAGEM_LUGAR = fct_explicit_na(VIAGEM_LUGAR, "Não informado")) %>%  # Transforma NA em categoria
  print()

(g.viagemlugar <- viagemlugar_participante %>%
  ggplot(aes(x = VIAGEM_LUGAR, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(
    x = "Tipo de Viagem",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 29))+
  scale_fill_brewer(palette = "Reds")+
  scale_x_discrete(labels = c("São Paulo e\nestado de origem", "nacional e\ninternacional", "não informado")) +
  theme_minimal()+
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))


## VIAGEM VONTADE ####
viagemvontade_participante <- infs2 %>% 
  distinct(PARTICIPANTE, LAZER_VIAGEM_VONTADE2) %>%  #garante 1 linha por participante
  mutate(LAZER_VIAGEM_VONTADE2 = fct_explicit_na(LAZER_VIAGEM_VONTADE2, "Não informado"),
         LAZER_VIAGEM_VONTADE2 = fct_relevel(
           LAZER_VIAGEM_VONTADE2,
           "nenhum",
           "nacional",
           "nacional.internacional")) %>%  # Transforma NA em categoria
  count(LAZER_VIAGEM_VONTADE2) %>%
  print()

(g.viagemvontade <- viagemvontade_participante %>%
  ggplot(aes(x = LAZER_VIAGEM_VONTADE2, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white",fill = "#FCAE91") +
  labs(
    x = "Lugares que Gostaria de conhecer",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 29))+
  scale_fill_brewer(palette = "Reds")+
  scale_x_discrete(labels = c("nenhum","nacional", "nacional e\ninternacional", "não informado")) +
  theme_minimal()+
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))



#### grafico 3 viagem ####
g.viagens <- (g.viagem | g.viagemlugar | g.viagemvontade) + 
  plot_layout(guides = "collect")
g.viagens


ggsave(filename = "C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/_N/10viagem.png",
       plot = g.viagens,
       width = 15,
       height = 5,
       units = "in",
       dpi = 300)

## MEGA SENA ####
megasena_participante <- infs2 %>% 
  distinct(PARTICIPANTE, MEGA_SENA2) %>%  #garante 1 linha por participante
  count(MEGA_SENA2) %>%
  mutate(MEGA_SENA2 = fct_relevel(
           MEGA_SENA2, "gastar", "voltar.estado", "ajudar.outros", "investir"),
         MEGA_SENA2 = fct_explicit_na(MEGA_SENA2, "Não informado")
         ) %>%  # Transforma NA em categoria
  print()

(g.megasena <- megasena_participante %>%
  ggplot(aes(x = MEGA_SENA2, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(
    x = "Se Ganhasse na Mega-Sena",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 29))+
  scale_x_discrete(labels = c("gastar", "voltar p/ estado \nde origem", "ajudar outros", "investir", "não informado")) +
  theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))


## INFANCIA ####
infancia_participante <- infs2 %>% 
  distinct(PARTICIPANTE, INFANCIA_MEMORIA) %>%
  count(INFANCIA_MEMORIA) %>% 
  print()
  

(g.infancia <- infancia_participante %>% 
  ggplot(aes(x = INFANCIA_MEMORIA, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(
    x = "Memória da Infância",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 29))+
  scale_x_discrete(labels = c("negativa", "neutra", "positiva")) +
  theme_minimal()+
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 11)))



#### grafico 2 mega sena e infancia ####
g.megainfacia <- (g.megasena | g.infancia) + 
  plot_layout(guides = "collect")
g.megainfacia


ggsave(filename = "C:/Users/sah/Downloads/analise-quantitativa-s-coda/graficos/_N/11megainfancia.png",
       plot = g.megainfacia,
       width = 13,
       height = 5,
       units = "in",
       dpi = 300)



## MEGA SENA TRABALAHR ####
megasena_trabalhar_participante <- infs2 %>% 
  distinct(PARTICIPANTE, MEGASENA_TRABALHAR2) %>%  #garante 1 linha por participante
  count(MEGASENA_TRABALHAR2) %>%
  mutate(MEGASENA_TRABALHAR2 = fct_explicit_na(MEGASENA_TRABALHAR2, "Não informado")) %>%  # Transforma NA em categoria
  print()

megasena_trabalhar_participante %>%
  ggplot(aes(x = MEGASENA_TRABALHAR2, y = n, label = n, fill = MEGASENA_TRABALHAR2)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Mega Sena - Trabalho",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25))






## MOTIVO ####
motivo_participante <- infs2 %>% 
  distinct(PARTICIPANTE, MOTIVO) %>%  #garante 1 linha por participante
  count(MOTIVO) %>%
  mutate(MOTIVO = fct_explicit_na(MOTIVO, "Não informado")) %>%  # Transforma NA em categoria
  print()

motivo_participante %>%
  ggplot(aes(x = MOTIVO, y = n, label = n, fill = MOTIVO)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "MOTIVO",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25) 
        
  )



## IMOVEL ####
imovel_participante <- infs2 %>% 
  distinct(PARTICIPANTE, IMOVEL) %>%  #garante 1 linha por participante
  count(IMOVEL) %>%
  mutate(IMOVEL = fct_explicit_na(IMOVEL, "Não informado")) %>%  # Transforma NA em categoria
  print()

imovel_participante %>%
  ggplot(aes(x = IMOVEL, y = n, label = n, fill = IMOVEL)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Tipo de Imóvel",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),legend.position = "none")




## MEDIA PAIS ####
pais_participante <- infs2 %>% 
  distinct(PARTICIPANTE, PAIS) %>%  #garante 1 linha por participante
  mutate(
    PAIS = as.factor(PAIS),
    #PAIS = fct_explicit_na(PAIS, "Não informado")
    ) %>%
  count(PAIS) %>%
  #mutate(PAIS = fct_explicit_na(PAIS, "Não informado")) %>%  # Transforma NA em categoria
  print()


pais_participante %>%
  ggplot(aes(x = PAIS, y = n, label = n, fill = PAIS)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "PAIS",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25) 
        
  )

hist(
  infs2$PAIS,
  main = "Distribuição do índice PAIS",
  xlab = "Média escolaridade e ocupação dos pais",
  col = "lightblue",
  border = "white"
)


##MEDIA LAZER####
hist(
  infs2$LAZER,
  main = "Distribuição do índice LAZER",
#  xlab = "Média escolaridade e ocupação dos pais",
  col = "lightblue",
  border = "white"
)


##MEDIA VIAGEM####
hist(
  infs2$VIAGEM2,
  main = "Distribuição do índice VIAGEM",
  #  xlab = "Média escolaridade e ocupação dos pais",
  col = "lightblue",
  border = "white"
)


# INTERAÇÕES ####
#HABITAÇÃO ####
## PROPRIEDADE ####
### Propriedade x renda individual ####

table(infs2$IMOVEL, infs2$RENDA_IND)
imovel_renda <- infs2 %>%
  count(IMOVEL, RENDA_IND, PARTICIPANTE) %>%
  group_by(IMOVEL, RENDA_IND) %>%
  summarise(n_participantes = n_distinct(IMOVEL)) %>% 
  arrange(IMOVEL, RENDA_IND) %>%  # Ordenar os dados
  print()


ggplot(imovel_renda, aes(x = RENDA_IND, y = n_participantes, fill = IMOVEL)) +
  geom_col() +
  #labs(x =  "IMOVEL", y = "Número de Participantes")+
#  scale_fill_brewer(
 #   palette = "Reds",
  #  na.value = "gray70"#,
  #  name = "Renda individual",
  #  labels = c("Até 1 SM", "1 a 2 SM", "2 a 4 SM", "4 a 9/ 10 a 19 SM")
  #) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")


## BAIRRO ####
dados_grafico <- infs2 %>%
  filter(!is.na(BAIRRO), !is.na(media_m2)) %>%
  group_by(BAIRRO) %>%
  summarise(
    media_m2 = first(media_m2),  # Mantém o valor original do m²
    n_participantes = n_distinct(PARTICIPANTE)  # Conta participantes únicos
  ) %>%
  mutate(
    BAIRRO = fct_reorder(BAIRRO, media_m2),  # Ordena por valor do m²
    label = ifelse(n_participantes == 1,
                   paste(n_participantes, "participante"),
                   paste(n_participantes, "participantes"))  ) %>% 
  print()

# Criar o gráfico com labels# Criar o gráfico com labprint()els
ggplot(dados_grafico, aes(x = BAIRRO, y = media_m2)) +
  geom_col(fill = "#FCAE91", width = 0.7) +
  # Texto branco dentro da barra (40% da largura)
  geom_text(aes(label = label, y = media_m2 * 0.4), 
            color = "white", 
            size = 3,
            fontface = "bold") +
  coord_flip() +  # Barras horizontais
  scale_y_continuous(
    limits = c(0, max(dados_grafico$media_m2) * 1.1),  # 10% de espaço extra
    expand = c(0, 0),  # Remove espaços desnecessários
    labels = dollar_format(prefix = "R$ ")  # Formato monetário
  ) +
  labs(x = NULL, y = "Valor médio do m²"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),  # Remove linhas horizontais
    axis.text.y = element_text(size = 7),  # Tamanho do texto dos bairros
    plot.title = element_text(face = "bold", size = 14)
  )


### Bairo região ####
tab.BAIRO.REGIAO <- infs2 %>%
  filter(!is.na(BAIRRO_REGIAO2)) %>%
  distinct(PARTICIPANTE, BAIRRO_REGIAO2) %>% 
  count(BAIRRO_REGIAO2) %>% 
  print()

ggplot(tab.BAIRO.REGIAO, aes(x = BAIRRO_REGIAO2, y = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Região",
      y = "Número de Participantes")+
  geom_text(aes(label = paste0("(", n, ")")), vjust = -0.2, size = 3.5) +
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        legend.position = "none")



### Renda por bairro ####
renda_bairro <- infs2 %>% 
  filter(!is.na(RENDA_IND), !is.na(BAIRRO)) %>% 
  group_by(BAIRRO, RENDA_IND, media_m2) %>% 
  summarise(n = n(),
            n_participantes = n_distinct(PARTICIPANTE), .groups = 'drop') %>%
  mutate(
    BAIRRO = fct_reorder(BAIRRO, media_m2),
    RENDA_IND = factor(RENDA_IND),  # Mantém as categorias de renda como fator
    # Ordena por valor do m²
    label = ifelse(n_participantes == 1,
                   paste(n_participantes, "participante"),
                   paste(n_participantes, "participantes"))  ) %>% 
  arrange(media_m2, BAIRRO, RENDA_IND) %>% 
  print()

renda_bairro %>%
  ggplot(aes(x = RENDA_IND, y = BAIRRO, size = n_participantes, color = RENDA_IND)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(3, 12)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 50, hjust = 1),
    legend.position = "right",
    legend.box.spacing = unit(0.005, "cm")
  ) +
  labs(x = "Bairro", y = "Renda individual", size = "Nº de pessoas")


### Renda por REGIAO ####
renda_regiao <- infs2 %>% 
  filter(!is.na(RENDA_IND), !is.na(BAIRRO_REGIAO2)) %>% 
  group_by(RENDA_IND, BAIRRO_REGIAO2) %>%
  count(RENDA_IND) %>% 
  print()

renda_regiao %>%
  ggplot(aes(x = BAIRRO.REGIAO, y = RENDA_IND, size = n, color = RENDA_IND)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(3, 12)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 50, hjust = 1),
    legend.position = "right",
    legend.box.spacing = unit(0.005, "cm")
  ) +
  labs(x = "Bairro", y = "Renda individual", size = "Nº de pessoas")

### Renda fam por REGIAO ####
renda_fam_regiao <- infs2 %>% 
  filter(!is.na(RENDA_FAM), !is.na(BAIRRO_REGIAO2)) %>% 
  group_by(BAIRRO_REGIAO2, RENDA_FAM) %>%
  count(RENDA_FAM) %>% 
  print()

renda_fam_regiao %>%
  ggplot(aes(x = BAIRRO_REGIAO2, y = RENDA_FAM, size = n, color = RENDA_FAM)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(3, 12)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 50, hjust = 1),
    legend.position = "right",
    legend.box.spacing = unit(0.005, "cm")
  ) +
  labs(x = "Bairro", y = "Renda Familiar", size = "Nº de pessoas")





### n de comodos por M4 ####
infs2 %>% 
  filter(!is.na(NCOMODOS), !is.na(media_m2)) %>%
  ggplot(aes(x = NCOMODOS, y = media_m2)) +
  geom_jitter(width = 0.2, alpha = 0.9, color = "black") +
  stat_summary(fun = mean, geom = "point", color = "red", size = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", size = 1) +
  labs(x = "Número de NCOMODOS",
       y = "Preço médio do m² (R$)"
  ) +
  theme_minimal()


# OCUPACAO ####
### ocupacao pai ####
ocupacao_pai <- dados2 %>% 
  distinct(PARTICIPANTE, INDICE_OCUPACAO_PAI) %>%  #garante 1 linha por participante
  count(INDICE_OCUPACAO_PAI) %>% 
  arrange(INDICE_OCUPACAO_PAI) %>% 
  mutate(
    categoria = case_when(
      INDICE_OCUPACAO_PAI == 0 ~ "0. Desempregado/sem renda",
      INDICE_OCUPACAO_PAI == 1 ~ "1. Trabalhador braçal s/ treinamento",
      INDICE_OCUPACAO_PAI == 2 ~ "2. Trabalhador braçal c/ treinamento",
      INDICE_OCUPACAO_PAI == 3 ~ "3. Funções admin./atend. ao público",
      INDICE_OCUPACAO_PAI == 4 ~ "5. Microempres./ger. baixo escalão",
      INDICE_OCUPACAO_PAI == 5 ~ "6. Profissionais especializados/liberais",
      INDICE_OCUPACAO_PAI == 6 ~ "7. Peq. Emp./ger. alto escalão",
      TRUE ~ as.character(INDICE_OCUPACAO_PAI)
    ),
    categoria = factor(categoria, levels = unique(categoria))) %>% 
  print()


ocupacao_pai %>% 
  ggplot(aes(x = INDICE_OCUPACAO_PAI, y = n, label = n, fill = categoria)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Índice de Ocupação",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25)
        
        
  )

### ocupacao mae ####
ocupacao_mae <- dados2 %>% 
  distinct(PARTICIPANTE, INDICE_OCUPACAO_MAE) %>%  #garante 1 linha por participante
  count(INDICE_OCUPACAO_MAE) %>% 
  arrange(INDICE_OCUPACAO_MAE) %>% 
  mutate(
    categoria = case_when(
      INDICE_OCUPACAO_MAE == 0 ~ "0. Desempregado/sem renda",
      INDICE_OCUPACAO_MAE == 1 ~ "1. Trabalhador braçal s/ treinamento",
      INDICE_OCUPACAO_MAE == 2 ~ "2. Trabalhador braçal c/ treinamento",
      INDICE_OCUPACAO_MAE == 3 ~ "3. Funções admin./atend. ao público",
      INDICE_OCUPACAO_MAE == 4 ~ "4. Profissionais da educação",
      INDICE_OCUPACAO_MAE == 5 ~ "5. Microempres./ger. baixo escalão",
      INDICE_OCUPACAO_MAE == 6 ~ "6. Profissionais especializados/liberais",
      INDICE_OCUPACAO_MAE == 7 ~ "7. Peq. Emp./ger. alto escalão",
      TRUE ~ as.character(INDICE_OCUPACAO_MAE)
    ),
    categoria = factor(categoria, levels = unique(categoria))) %>% 
  print()


ocupacao_mae %>% 
  ggplot(aes(x = INDICE_OCUPACAO_MAE, y = n, label = n, fill = categoria)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Índice de Ocupação da Mãe",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25)
        
        
  )



## Ocupação outro cargo ####
ocupacao_outro_cargo <- dados2 %>% 
  distinct(PARTICIPANTE, INDICE_OUTRO_CARGO) %>%  #garante 1 linha por participante
  count(INDICE_OUTRO_CARGO) %>% 
  arrange(INDICE_OUTRO_CARGO) %>% 
  print()

ocupacao_outro_cargo %>% 
  ggplot(aes(x = factor(INDICE_OUTRO_CARGO), y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Ambição outro cargo",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        legend.position = "none")


## Ocupação dos sonhos ####
ocupacao_sonhos <- dados2 %>% 
  distinct(PARTICIPANTE, INDICE_OCUPACAO_SONHOS) %>%  #garante 1 linha por participante
  count(INDICE_OCUPACAO_SONHOS) %>% 
  arrange(INDICE_OCUPACAO_SONHOS) %>% 
  print()

ocupacao_sonhos %>% 
  ggplot(aes(x = factor(INDICE_OCUPACAO_SONHOS), y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Índice Trabalho dos Sonhos",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        
        legend.position = "none")


# ocupação x ocupação outro cargo
infs2 %>%
  count(INDICE_OCUPACAO, INDICE_OUTRO_CARGO, name = "n_participantes") %>% 
  ggplot(aes(x = INDICE_OCUPACAO, y = INDICE_OUTRO_CARGO)) +
  geom_density_2d_filled(contour_var = "ndensity", alpha = 0.8) +
  scale_fill_viridis_d(option = "C") +
  # labs(    x = "Índice de Ocupação",
  #   y = "Índice de Ocupação dos Sonhos",
  #   fill = "Densidade"
  #) +
  theme_minimal()


infs2 %>%
  count(INDICE_OCUPACAO, INDICE_OCUPACAO_SONHOS, name = "n_participantes") %>% 
  ggplot(aes(x = INDICE_OCUPACAO, y = INDICE_OCUPACAO_SONHOS)) +
  geom_density_2d_filled(contour_var = "ndensity", alpha = 0.8) +
  scale_fill_viridis_d(option = "C") +
  # labs(    x = "Índice de Ocupação",
  #   y = "Índice de Ocupação dos Sonhos",
  #   fill = "Densidade"
  #) +
  theme_minimal()



## Locomoção e Distância ####
locomocao <- dados2 %>% 
  distinct(PARTICIPANTE, OCUPACAO_LOCOMOCAO2) %>%  #garante 1 linha por participante
  count(OCUPACAO_LOCOMOCAO2) %>% 
  arrange(OCUPACAO_LOCOMOCAO2) %>% 
  print()

locomocao %>% 
  ggplot(aes(x = factor(OCUPACAO_LOCOMOCAO2), y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Forma de Locomoção para o trabalho",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), legend.position = "none")

distancia <- infs2%>% 
  distinct(PARTICIPANTE, OCUPACAO_DIST) %>%  #garante 1 linha por participante
  count(OCUPACAO_DIST) %>% 
  print()

distancia %>% 
  ggplot(aes(x = OCUPACAO_DIST, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
 # labs(x = "Forma de Locomoção para o trabalho",
  #     y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        legend.position = "none")


## renda e distância ####
infs2 %>%
  count(RENDA_IND, OCUPACAO_DIST, name = "n_participantes") %>% 
  ggplot(aes(x = OCUPACAO_DIST, y = RENDA_IND)) +
  geom_point()


# ESCOLARIDADE ####
escolaridade <- dados2 %>% 
  distinct(PARTICIPANTE, ESCOLARIDADE2) %>%  #garante 1 linha por participante
  count(ESCOLARIDADE2) %>% 
  arrange(ESCOLARIDADE2) %>% 
  print()

escolaridade %>% 
  ggplot(aes(x = factor(ESCOLARIDADE2), y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "ESCOLARIDADE",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        legend.position = "none")




##Escolaridade do participante X escolaridade dos pais ####

### pai ####
table(infs2$ESCOLARIDADE2, infs2$ESCOLA_PAI2)
tabela_escolaridade <- infs2 %>%
  count(ESCOLARIDADE2, ESCOLA_PAI2, PARTICIPANTE) %>%
  group_by(ESCOLARIDADE2, ESCOLA_PAI2) %>%
  summarise(n_participantes = n_distinct(PARTICIPANTE)) %>% 
  mutate(
    ESCOLARIDADE2 = factor(ESCOLARIDADE2,
                          levels = c("fund", "medio", "superior"),
                          ordered = TRUE),
    ESCOLA_PAI2 = factor(ESCOLA_PAI2,
                        levels = c("analfabeto", "fund", "medio.superior"),
                        ordered = TRUE)) %>%
  arrange(ESCOLARIDADE2, ESCOLA_PAI2) %>%  # Ordenar os dados
  print()


ggplot(tabela_escolaridade, aes(x = ESCOLARIDADE2, y = n_participantes, fill = ESCOLA_PAI2)) +
  geom_col() +
  scale_fill_brewer(
    palette = "Reds",
    na.value = "gray70",
    name = "Escolaridade do Pai",
    labels = c("Analfabeto", "Fundamental", "Médio/Superior", "Não informado")
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
                     legend.position = "right")


### mae ####
table(infs2$ESCOLARIDADE2, infs2$ESCOLA_MAE2)

tabela_escolaridade_mae <- infs2 %>%
  count(ESCOLARIDADE2, ESCOLA_MAE2, PARTICIPANTE) %>%
  group_by(ESCOLARIDADE2, ESCOLA_MAE2) %>%
  summarise(n_participantes = n_distinct(PARTICIPANTE)) %>% 
  mutate(
    ESCOLARIDADE2 = factor(ESCOLARIDADE2,
                          levels = c("fund", "medio", "superior"),
                          labels = c("Fund", "Médio", "Superior/Pós-grad"),
                          ordered = TRUE),
    ESCOLA_MAE2 = factor(ESCOLA_MAE2,
                        levels = c("analfabeto", "fund", "medio.superior"),
                        labels = c("Analfabeta", "Fund", "Médio/Superior"),
                        ordered = TRUE)
  ) %>%
  arrange(ESCOLARIDADE2, ESCOLA_MAE2) %>%  # Ordenar os dados
  print()


ggplot(tabela_escolaridade_mae, aes(x = ESCOLARIDADE2, y = n_participantes, fill = ESCOLA_MAE2)) +
  geom_col() +
  scale_fill_brewer(
    palette = "Reds",
    na.value = "gray70",
    name = "Escolaridade do Mãe",
    labels = c("Analfabeta", "Fundamental", "Médio/Superior", "Não informado")
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")



# RENDA ####
## individual ####
renda_individual <- dados2 %>% 
  distinct(PARTICIPANTE, RENDA_IND) %>%  #garante 1 linha por participante
  mutate(RENDA_IND = fct_explicit_na(RENDA_IND, "Não informado")) %>%  # Transforma NA em categoria
  count(RENDA_IND) %>% 
  arrange(RENDA_IND) %>% 
  print()

renda_individual %>% 
  ggplot(aes(x = factor(RENDA_IND), y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Renda Individual",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        legend.position = "none")

# RENDA ####
renda_familiar <- dados2 %>% 
  distinct(PARTICIPANTE, RENDA_FAM) %>%  #garante 1 linha por participante
  mutate(RENDA_FAM = fct_explicit_na(RENDA_FAM, "Não informado")) %>%  # Transforma NA em categoria
  count(RENDA_FAM) %>% 
  arrange(RENDA_FAM) %>% 
  print()

renda_familiar %>% 
  ggplot(aes(x = factor(RENDA_FAM), y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FCAE91") +
  labs(x = "Renda Individual",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        legend.position = "none")

### Renda X Escolaridade ####
table(infs2$ESCOLARIDADE2, infs2$RENDA_IND)
renda_escolaridade <- infs2 %>%
  count(ESCOLARIDADE2, RENDA_IND, PARTICIPANTE) %>%
  group_by(ESCOLARIDADE2, RENDA_IND) %>%
  summarise(n_participantes = n_distinct(PARTICIPANTE)) %>% 
  mutate(
    ESCOLARIDADE2 = factor(ESCOLARIDADE2,
                          levels = c("fund", "medio", "superior"),
                          labels = c("Fund.", "Médio", "Ensino Superior"), ordered = TRUE)) %>% 
  arrange(ESCOLARIDADE2, RENDA_IND) %>%  # Ordenar os dados
  print()


ggplot(renda_escolaridade, aes(x = ESCOLARIDADE2, y = n_participantes, fill = RENDA_IND)) +
  geom_col() +
  labs(x =  "Escolaridade", y = "Número de Participantes")+
  scale_fill_brewer(
    palette = "Reds",
    na.value = "gray70",
    name = "Renda individual",
    labels = c("Até 1 SM", "1 a 2 SM", "2 a 4 SM", "4 a 9/ 10 a 19 SM")
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")



### Renda X Ocupação ####
table(infs2$INDICE_OCUPACAO, infs2$RENDA_IND)

ggplot(infs2, aes(x = INDICE_OCUPACAO, fill = RENDA_IND)) +
  geom_bar(position = "fill")
#  labs(y = "Proporção", fill = "Escolaridade da Mãe")





# M2 - preço do m2 do bairro ####

## Comodos ####
comodos_banheiros <- infs2 %>% 
  filter(!is.na(NBANHEIROS)) %>% 
  count(NBANHEIROS) %>% 
  print()

comodos_quartos <- infs2 %>% 
  filter(!is.na(NQUARTOS)) %>% 
  count(NQUARTOS) %>% 
  print()

## M2 por comodos ####
#A linha vermelha mostra a média do preço do m² para cada quantidade de banheiros.
infs2 %>% 
  filter(!is.na(NBANHEIROS), !is.na(media_m2)) %>%
  ggplot(aes(x = NBANHEIROS, y = media_m2)) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "#2b8cbe") +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", size = 1) +
  labs(x = "Número de banheiros",
       y = "Preço médio do m² (R$)"
  ) +
  theme_minimal()



infs2 %>% 
  filter(!is.na(NQUARTOS), !is.na(media_m2)) %>%
  ggplot(aes(x = NQUARTOS, y = media_m2)) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "#2b8cbe") +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", size = 1) +
  labs(x = "Número de quartos",
       y = "Preço médio do m² (R$)"
  ) +
  theme_minimal()



# LAZER ####

lazer <- infs2 %>% 
  filter(!is.na(LAZER_CARACTERISTICA)) %>%
  group_by(LAZER_CARACTERISTICA) %>%
  summarise(n_participantes = n_distinct(PARTICIPANTE), .groups = 'drop') %>%
  print()


lazer %>% 
  ggplot(aes(x = LAZER_CARACTERISTICA, y= n_participantes)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


lazer_renda_ind <- infs2 %>% 
  filter(!is.na(LAZER_CARACTERISTICA), !is.na(RENDA_IND)) %>%
  group_by(LAZER_CARACTERISTICA, RENDA_IND) %>%
  summarise(n = n(),
            n_participantes = n_distinct(PARTICIPANTE), .groups = 'drop') %>%
  print()


lazer_renda_ind %>% 
  ggplot(aes(x = LAZER_CARACTERISTICA, y= RENDA_IND, color = RENDA_IND, size = n_participantes)) +
  geom_point(alpha = 0.8) +
  scale_size_continuous(
    range = c(2, 8),  # Tamanho mínimo e máximo das bolhas
    name = "Nº de Participantes"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  ) 
  #guides(color = guide_legend(override.aes = list(size = 5)))  # Tamanho uniforme na legenda


## Viagem ####

viagem <- infs2 %>% 
  filter(!is.na(VIAGEM_LUGAR)) %>%
  group_by(VIAGEM_LUGAR) %>%
  summarise(n_participantes = n_distinct(PARTICIPANTE), .groups = 'drop') %>%
  print()


viagem %>% 
  ggplot(aes(x = VIAGEM_LUGAR, y= n_participantes)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## viagem por renda ####
viagem_renda <- infs2 %>% 
  filter(!is.na(VIAGEM_LUGAR), !is.na(RENDA_IND)) %>%
  group_by(VIAGEM_LUGAR, RENDA_IND) %>%
  summarise(n = n(),
            n_participantes = n_distinct(PARTICIPANTE), .groups = 'drop') %>%
  print()


viagem_renda %>% 
  ggplot(aes(x = VIAGEM_LUGAR, y= RENDA_IND, color = RENDA_IND, size = n_participantes)) +
  geom_point(alpha = 0.8) +
  scale_size_continuous(
    range = c(2, 8),  # Tamanho mínimo e máximo das bolhas
    name = "Nº de Participantes"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  ) 
#guides(color = guide_legend(override.aes = list(size = 5)))  # Tamanho uniforme na legenda



## Infância ####
infancia <- infs2 %>% 
  filter(!is.na(INFANCIA_MEMORIA), !is.na(RENDA_IND)) %>%
  group_by(INFANCIA_MEMORIA, RENDA_IND) %>% 
  count(INFANCIA_MEMORIA) %>% 
  print()


infancia %>% 
  ggplot(aes(x = INFANCIA_MEMORIA, y = RENDA_IND, color = INFANCIA_MEMORIA)) +
  geom_point(size = 4, alpha = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


infancia <- infs2 %>% 
  filter(!is.na(INFANCIA_MEMORIA), !is.na(RENDA_IND)) %>%
  group_by(INFANCIA_MEMORIA, RENDA_IND) %>%
  summarise(n = n(),
            n_participantes = n_distinct(PARTICIPANTE), .groups = 'drop') %>%
  print()


infancia %>% 
  ggplot(aes(x = INFANCIA_MEMORIA, y= RENDA_IND, color = RENDA_IND, size = n_participantes)) +
  geom_point(alpha = 0.8) +
  scale_size_continuous(
    range = c(2, 8),  # Tamanho mínimo e máximo das bolhas
    name = "Nº de Participantes"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  ) 



# INDICE SOCIO ####


## PARAMETROS OBJETIVOS ####
#OUSHIRO: (RENDA + OCUPAÇÃO + ESCOLARIDADE + MEDIA DA ESCOLARIDADE DOS PAIS)/4

#POLI: (PRECO M2 + COMODOS + RENDA + OCUPAÇÃO + ESCOLARIDADE + MEDIA DA ESCOLARIDADE DOS PAIS)/6

## PARAMETROS OBJETIVOS + HABITOS DE CONSUMO ####

#POLI: (PRECO M2 + VIAGEM + INFÂNCIA + LAZER+ COMODOS + RENDA + OCUPAÇÃO + ESCOLARIDADE + MEDIA DA ESCOLARIDADE DOS PAIS)/10


#escala

(x*5)/8

(x*5)/n