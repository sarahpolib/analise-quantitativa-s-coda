#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%# PERFIL DOS PARTICIPANTES #%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 15/07/2025 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% V1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
                      

## Ocupação ####

ocupacao <- dados2 %>% 
  distinct(PARTICIPANTE, INDICE_OCUPACAO) %>%  #garante 1 linha por participante
  count(INDICE_OCUPACAO) %>% 
  arrange(INDICE_OCUPACAO) %>% 
  print()



ocupacao %>% 
  ggplot(aes(x = factor(INDICE_OCUPACAO), y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FC9272") +
  labs(
    x = "Índice de Ocupação",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9),   # tamanho do título eixo Y
        legend.position = "none")



## Ocupação outro cargo ####

ocupacao_outro_cargo <- dados2 %>% 
  distinct(PARTICIPANTE, INDICE_OUTRO_CARGO) %>%  #garante 1 linha por participante
  count(INDICE_OUTRO_CARGO) %>% 
  arrange(INDICE_OUTRO_CARGO) %>% 
  print()

ocupacao_outro_cargo %>% 
  ggplot(aes(x = factor(INDICE_OUTRO_CARGO), y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FC9272") +
  labs(x = "Ambição outro cargo",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9),   # tamanho do título eixo Y
        legend.position = "none")


## Ocupação dos sonhos ####
ocupacao_sonhos <- dados2 %>% 
  distinct(PARTICIPANTE, INDICE_OCUPACAO_SONHOS) %>%  #garante 1 linha por participante
  count(INDICE_OCUPACAO_SONHOS) %>% 
  arrange(INDICE_OCUPACAO_SONHOS) %>% 
  print()

ocupacao_sonhos %>% 
  ggplot(aes(x = factor(INDICE_OCUPACAO_SONHOS), y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FC9272") +
  labs(x = "Índice Trabalho dos Sonhos",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9),   # tamanho do título eixo Y
        legend.position = "none")




## Locomoção e Distância ####
locomocao <- dados2 %>% 
  distinct(PARTICIPANTE, OCUPACAO_LOCOMOCAO) %>%  #garante 1 linha por participante
  count(OCUPACAO_LOCOMOCAO) %>% 
  arrange(OCUPACAO_LOCOMOCAO) %>% 
  print()

locomocao %>% 
  ggplot(aes(x = factor(OCUPACAO_LOCOMOCAO), y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FC9272") +
  labs(x = "Forma de Locomoção para o trabalho",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9),   # tamanho do título eixo Y
        legend.position = "none")


## Escolaridade ####
escolaridade <- dados2 %>% 
  distinct(PARTICIPANTE, ESCOLARIDADE) %>%  #garante 1 linha por participante
  count(ESCOLARIDADE) %>% 
  arrange(ESCOLARIDADE) %>% 
  print()

escolaridade %>% 
  ggplot(aes(x = factor(ESCOLARIDADE), y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FC9272") +
  labs(x = "ESCOLARIDADE",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9),   # tamanho do título eixo Y
        legend.position = "none")

## Escolaridade dos pais ####
escolaridade_pai <- dados2 %>% 
  distinct(PARTICIPANTE, ESCOLA_PAI2) %>%  #garante 1 linha por participante
  count(ESCOLA_PAI2) %>% 
  arrange(ESCOLA_PAI2) %>% 
  print()

escolaridade_pai %>% 
  ggplot(aes(x = factor(ESCOLA_PAI2), y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FC9272") +
  labs(x = "ESCOLARIDADE PAI",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9),   # tamanho do título eixo Y
        legend.position = "none")


escolaridade_mae <- dados2 %>% 
  distinct(PARTICIPANTE, ESCOLA_MAE2) %>%  #garante 1 linha por participante
  count(ESCOLA_MAE2) %>% 
  arrange(ESCOLA_MAE2) %>% 
  print()

escolaridade_mae %>% 
  ggplot(aes(x = factor(ESCOLA_MAE2), y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FC9272") +
  labs(x = "ESCOLARIDADE Mãe",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9),   # tamanho do título eixo Y
        legend.position = "none")


table(infs2$ESCOLARIDADE2, infs2$ESCOLA_PAI2)

ggplot(infs2, aes(x = ESCOLARIDADE, fill = ESCOLA_PAI)) +
  geom_bar(position = "fill") +
  labs(y = "Proporção", fill = "Escolaridade do Pai")

table(infs2$ESCOLARIDADE2, infs2$ESCOLA_MAE2)

ggplot(infs2, aes(x = ESCOLARIDADE2, fill = ESCOLA_MAE)) +
  geom_bar(position = "fill") +
  labs(y = "Proporção", fill = "Escolaridade da Mãe")

## Renda ####
renda_individual <- dados2 %>% 
  distinct(PARTICIPANTE, RENDA_IND) %>%  #garante 1 linha por participante
  count(RENDA_IND) %>% 
  arrange(RENDA_IND) %>% 
  print()

renda_individual %>% 
  ggplot(aes(x = factor(RENDA_IND), y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FC9272") +
  labs(x = "Renda Individual",
       y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9),   # tamanho do título eixo Y
        legend.position = "none")

### Renda X Escolaridade ####

table(infs2$ESCOLARIDADE2, infs2$RENDA_IND)

ggplot(infs2, aes(x = ESCOLARIDADE2, fill = RENDA_IND)) +
  geom_bar(position = "fill") +
  labs(y = "Proporção", fill = "Escolaridade da Mãe")


table(infs2$ESCOLARIDADE2, infs2$RENDA_FAM)

ggplot(infs2, aes(x = ESCOLARIDADE2, fill = RENDA_FAM)) +
  geom_bar(position = "fill") +
  labs(y = "Proporção", fill = "Escolaridade da Mãe")


### Renda X Ocupação ####

table(infs2$INDICE_OCUPACAO, infs2$RENDA_IND)

ggplot(infs2, aes(x = INDICE_OCUPACAO, fill = RENDA_IND)) +
  geom_bar(position = "fill")
#  labs(y = "Proporção", fill = "Escolaridade da Mãe")



## Preço do m2 do bairro ####
table(infs2$media_m2, infs2$RENDA_IND)

ggplot(infs2, aes(x = RENDA_IND, y = media_m2)) +
  geom_bar(stat = "identity")


table(infs2$NBANHEIROS, infs2$media_m2)

ggplot(infs2, aes(x = media_m2, y = NBANHEIROS)) +
  geom_point()

table(infs2$media_m2, infs2$NBANHEIROS)

ggplot(infs2, aes(x = NBANHEIROS, y = media_m2)) +
  geom_bar(stat = "identity")



media_m2_bairro %>% 
  ggplot(aes(x = BAIRRO, y = media_m2)) +
  geom_bar(stat = "identity", color = "white", fill = "#FC9272") +
  #labs(x = "Renda Individual",
  #y = "Número de Participantes")+
  #geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9),   # tamanho do título eixo Y
        legend.position = "none")+
  coord_flip()  # deixa os nomes dos bairros legíveis


dados2 %>% 
ggplot(aes(x = NBANHEIROS, y = BAIRRO, fill = media_m2)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_text(aes(label = scales::percent(media_m2, accuracy = 0.1)), size = 4) +
  labs(
    x = "Escolaridade do Pai",
    y = "Escolaridade do Participante",
    fill = "Proporção"
  ) +
  theme_minimal()




## Renda por bairro ####
renda_bairro <- infs2 %>% 
  filter(!is.na(RENDA_IND), !is.na(BAIRRO)) %>% 
  group_by(BAIRRO, RENDA_IND) %>% 
  summarise(n = n())%>% 
  print()

renda_bairro %>% 
  ggplot(aes(x = BAIRRO, y = RENDA_IND, color = RENDA_IND)) +
  geom_point(size = 4, alpha = 0.7) +
  #labs(
  #   title = "Número de participantes por faixa de renda e bairro",
  #   x = "Bairro",
  #   y = "Número de participantes",
  #   color = "Faixa de renda"
  # ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


dados2 %>%
  filter(!is.na(RENDA_IND), !is.na(BAIRRO)) %>% 
  distinct(PARTICIPANTE, BAIRRO, RENDA_IND) %>%
  count(BAIRRO, RENDA_IND, name = "n_participantes") %>% 
  ggplot(aes(x = RENDA_IND, y = BAIRRO, size = n_participantes, color = RENDA_IND)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(3, 12)) +
  theme_minimal() +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Bairro", y = "Renda individual", size = "Nº de pessoas")


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


## Lazer e renda individual ####
infs2$LAZER_CARACTERISTICA

lazer <- infs2 %>% 
  filter(!is.na(LAZER_CARACTERISTICA), !is.na(RENDA_IND)) %>%
  group_by(LAZER_CARACTERISTICA, RENDA_IND) %>% 
  count(LAZER_CARACTERISTICA) %>% 
  print()


lazer %>% 
  ggplot(aes(x = LAZER_CARACTERISTICA, y = RENDA_IND, color = LAZER_CARACTERISTICA)) +
  geom_point(size = 4, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Número de pessoas por tipo de lazer e faixa de renda",
    x = "Faixa de Renda Individual",
    y = "Contagem",
    color = "Lazer"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Viagem ####


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

## Amigos ####



