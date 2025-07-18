#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%# PERFIL DOS PARTICIPANTES #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 15/07/2025 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% V1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
                      

# BAIRRO ####

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
  geom_col(fill = "#FC9272", width = 0.7) +
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


## Renda por bairro ####
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

## n de comodos por M4 ####

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
ocupacao <- dados2 %>% 
  distinct(PARTICIPANTE, INDICE_OCUPACAO) %>%  #garante 1 linha por participante
  count(INDICE_OCUPACAO) %>% 
  arrange(INDICE_OCUPACAO) %>% 
  mutate(
    categoria = case_when(
      INDICE_OCUPACAO == 1 ~ "1 Desempregado/sem renda",
      INDICE_OCUPACAO == 2 ~ "2. Trabalhador braçal s/ treinamento",
      INDICE_OCUPACAO == 3 ~ "3. Trabalhador braçal c/ treinamento",
      INDICE_OCUPACAO == 4 ~ "4. Funções admin./atend. ao público",
      INDICE_OCUPACAO == 5 ~ "5. Profissionais da educação",
      INDICE_OCUPACAO == 6 ~ "6. Microempres./ger. baixo escalão",
      INDICE_OCUPACAO == 7 ~ "7. Profissionais liberais",
      INDICE_OCUPACAO == 8 ~ "8. Peq. Emp./ger. alto escalão",
      TRUE ~ as.character(INDICE_OCUPACAO)
    ),
    categoria = factor(categoria, levels = unique(categoria))) %>% 
  print()



ocupacao %>% 
  ggplot(aes(x = INDICE_OCUPACAO, y = n, label = n, fill = categoria)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Índice de Ocupação",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9),   # tamanho do título eixo Y
        )



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

distancia <- infs2%>% 
  distinct(PARTICIPANTE, OCUPACAO_DIST) %>%  #garante 1 linha por participante
  count(OCUPACAO_DIST) %>% 
  print()

distancia %>% 
  ggplot(aes(x = OCUPACAO_DIST, y = n, label = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FC9272") +
 # labs(x = "Forma de Locomoção para o trabalho",
  #     y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.2, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9),   # tamanho do título eixo Y
        legend.position = "none")


## renda e distância ####
infs2 %>%
  count(RENDA_IND, OCUPACAO_DIST, name = "n_participantes") %>% 
  ggplot(aes(x = OCUPACAO_DIST, y = RENDA_IND)) +
  geom_point()


# ESCOLARIDADE ####
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



##Escolaridade do participante X escolaridade dos pais ####

### pai ####
table(infs2$ESCOLARIDADE2, infs2$ESCOLA_PAI2)
tabela_escolaridade <- infs2 %>%
  count(ESCOLARIDADE, ESCOLA_PAI, PARTICIPANTE) %>%
  group_by(ESCOLARIDADE, ESCOLA_PAI) %>%
  summarise(n_participantes = n_distinct(PARTICIPANTE)) %>% 
  mutate(
    ESCOLARIDADE = factor(ESCOLARIDADE,
                          levels = c("fund1", "fund2", "medio", "superior", "posgrad"),
                          ordered = TRUE),
    ESCOLA_PAI = factor(ESCOLA_PAI,
                        levels = c("analfabeto", "fund1", "fund2", "medio", "superior", "posgrad"),
                        ordered = TRUE)) %>%
  arrange(ESCOLARIDADE, ESCOLA_PAI) %>%  # Ordenar os dados
  print()


ggplot(tabela_escolaridade, aes(x = ESCOLARIDADE, y = n_participantes, fill = ESCOLA_PAI)) +
  geom_col() +
  scale_fill_brewer(
    palette = "Reds",
    na.value = "gray70",
    name = "Escolaridade do Pai",
    labels = c("Analfabeto", "Fundamental I", "Fundamental II", "Médio", "Superior", "Pós-graduação", "Não informado")
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
                     legend.position = "right")


### mae ####
table(infs2$ESCOLARIDADE2, infs2$ESCOLA_MAE2)

tabela_escolaridade_mae <- infs2 %>%
  count(ESCOLARIDADE, ESCOLA_MAE, PARTICIPANTE) %>%
  group_by(ESCOLARIDADE, ESCOLA_MAE) %>%
  summarise(n_participantes = n_distinct(PARTICIPANTE)) %>% 
  mutate(
    ESCOLARIDADE = factor(ESCOLARIDADE,
                          levels = c("fund1", "fund2", "medio", "superior", "posgrad"),
                          labels = c("Fund. I", "Fund. II", "Médio", "Superior", "Pós-grad"),
                          ordered = TRUE),
    ESCOLA_MAE = factor(ESCOLA_MAE,
                        levels = c("analfabeto", "fund1", "medio", "superior", "posgrad"),
                        labels = c("Analfabeta", "Fund. I", "Médio", "Superior", "Pós-grad"),
                        ordered = TRUE)
  ) %>%
  arrange(ESCOLARIDADE, ESCOLA_MAE) %>%  # Ordenar os dados
  print()


ggplot(tabela_escolaridade_mae, aes(x = ESCOLARIDADE, y = n_participantes, fill = ESCOLA_MAE)) +
  geom_col() +
  scale_fill_brewer(
    palette = "Reds",
    na.value = "gray70",
    name = "Escolaridade do Mãe",
    labels = c("Analfabeto", "Fundamental I", "Médio", "Superior", "Pós-graduação", "Não informado")
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")



# RENDA ####
renda_individual <- dados2 %>% 
  distinct(PARTICIPANTE, RENDA_IND) %>%  #garante 1 linha por participante
  mutate(RENDA_IND = fct_explicit_na(RENDA_IND, "Não informado")) %>%  # Transforma NA em categoria
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
renda_escolaridade <- infs2 %>%
  count(ESCOLARIDADE, RENDA_IND, PARTICIPANTE) %>%
  group_by(ESCOLARIDADE, RENDA_IND) %>%
  summarise(n_participantes = n_distinct(PARTICIPANTE)) %>% 
  mutate(
    ESCOLARIDADE = factor(ESCOLARIDADE,
                          levels = c("fund1", "fund2", "medio", "superior", "posgrad"),
                          labels = c("Fund. I", "Fund. II", "Médio", "Graduação", "Pós-graduação"), ordered = TRUE)) %>% 
  arrange(ESCOLARIDADE, RENDA_IND) %>%  # Ordenar os dados
  print()


ggplot(renda_escolaridade, aes(x = ESCOLARIDADE, y = n_participantes, fill = RENDA_IND)) +
  geom_col() +
  labs(x =  "Escolaridade", y = "Número de Participantes")+
  scale_fill_brewer(
    palette = "Reds",
    na.value = "gray70",
    name = "Renda individual",
    labels = c("até 1 SM", "1 a 2 SM", "2 a 4 SM", "4 a 9 SM", "10 a 19 SM")
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