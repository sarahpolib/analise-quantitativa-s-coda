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

escolaridade_participante %>% 
  ggplot(aes(x = ESCOLARIDADE2, y = n, label = n, fill = ESCOLARIDADE2)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Escolaridade",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
        )
  


## OCUPAÇÃO ####
ocupacao <- infs2 %>% 
  distinct(PARTICIPANTE, INDICE_OCUPACAO) %>%  #garante 1 linha por participante
  count(INDICE_OCUPACAO) %>% 
  arrange(INDICE_OCUPACAO) %>% 
  mutate(
    categoria = case_when(
      INDICE_OCUPACAO == 0 ~ "0. Desempregado/sem renda",
      INDICE_OCUPACAO == 1 ~ "1. Trabalhador braçal s/ treinamento",
      INDICE_OCUPACAO == 2 ~ "2. Trabalhador braçal c/ treinamento",
      INDICE_OCUPACAO == 3 ~ "3. Funções admin./atend. ao público",
      INDICE_OCUPACAO == 4 ~ "4. Microempres./ger. baixo escalão",
      INDICE_OCUPACAO == 5 ~ "5. Profissionais especializados/liberais",
      INDICE_OCUPACAO == 6 ~ "6. Peq. Emp./ger. alto escalão",
      TRUE ~ as.character(INDICE_OCUPACAO)
    ),
    categoria = factor(categoria, levels = unique(categoria))) %>% 
  print()


ocupacao %>%
  ggplot(aes(x = INDICE_OCUPACAO, y = n)) +
  geom_line(linewidth = 1.2, color = "black") +
  geom_point(size = 3, color = "black") +
  scale_x_continuous(breaks = ocupacao$INDICE_OCUPACAO) +
  labs(
    x = "Índice de ocupação",
    y = "Número de participantes",
    title = "Distribuição contínua do índice de ocupação"
  ) +
  theme_minimal()


## OUTRO CARGO ####
outrocargo_participante <- infs2 %>% 
  distinct(PARTICIPANTE, INDICE_OUTRO_CARGO) %>%  #garante 1 linha por participante
  count(INDICE_OUTRO_CARGO) %>%
  mutate(INDICE_OUTRO_CARGO = fct_explicit_na(INDICE_OUTRO_CARGO, "Não informado")) %>%  # Transforma NA em categoria
  print()

outrocargo_participante %>%
  ggplot(aes(x = INDICE_OUTRO_CARGO, y = n, label = n, fill = INDICE_OUTRO_CARGO)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Outro Cargo",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
 #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )

## OCUPAÇÃO DOS SONHOS ####
ocupacaosonhos_participante <- infs2 %>% 
  distinct(PARTICIPANTE, INDICE_OCUPACAO_SONHOS) %>%  #garante 1 linha por participante
  count(INDICE_OCUPACAO_SONHOS) %>%
  mutate(INDICE_OCUPACAO_SONHOS = fct_explicit_na(INDICE_OCUPACAO_SONHOS, "Não informado")) %>%  # Transforma NA em categoria
  print()

ocupacaosonhos_participante %>%
  ggplot(aes(x = INDICE_OCUPACAO_SONHOS, y = n, label = n, fill = INDICE_OCUPACAO_SONHOS)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Cargo dos sonhos",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )

## LOCOMOÇÃO ####
locomocao_participante <- infs2 %>% 
  distinct(PARTICIPANTE, OCUPACAO_LOCOMOCAO2) %>%  #garante 1 linha por participante
  count(OCUPACAO_LOCOMOCAO2) %>%
  mutate(OCUPACAO_LOCOMOCAO2 = fct_explicit_na(OCUPACAO_LOCOMOCAO2, "Não informado")) %>%  # Transforma NA em categoria
  print()

locomocao_participante %>%
  ggplot(aes(x = OCUPACAO_LOCOMOCAO2, y = n, label = n, fill = OCUPACAO_LOCOMOCAO2)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Locomoção",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )



## MEGA SENA ####
megasena_participante <- infs2 %>% 
  distinct(PARTICIPANTE, MEGA_SENA2) %>%  #garante 1 linha por participante
  count(MEGA_SENA2) %>%
  mutate(MEGA_SENA2 = fct_explicit_na(MEGA_SENA2, "Não informado")) %>%  # Transforma NA em categoria
  print()

megasena_participante %>%
  ggplot(aes(x = MEGA_SENA2, y = n, label = n, fill = MEGA_SENA2)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Mega Sena",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )


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
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )



## LAZER ####
lazer_participante <- infs2 %>% 
  distinct(PARTICIPANTE, LAZER_CARACTERISTICA) %>%  #garante 1 linha por participante
  count(LAZER_CARACTERISTICA) %>%
  mutate(LAZER_CARACTERISTICA = fct_explicit_na(LAZER_CARACTERISTICA, "Não informado")) %>%  # Transforma NA em categoria
  print()

lazer_participante %>%
  ggplot(aes(x = LAZER_CARACTERISTICA, y = n, label = n, fill = LAZER_CARACTERISTICA)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Lazer",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )



## LAZER CAMPINAS ####
lazercampinas_participante <- infs2 %>% 
  distinct(PARTICIPANTE, LAZER_CAMPINAS_CARACTERISTICA) %>%  #garante 1 linha por participante
  count(LAZER_CAMPINAS_CARACTERISTICA) %>%
  mutate(LAZER_CAMPINAS_CARACTERISTICA = fct_explicit_na(LAZER_CAMPINAS_CARACTERISTICA, "Não informado")) %>%  # Transforma NA em categoria
  print()

lazercampinas_participante %>%
  ggplot(aes(x = LAZER_CAMPINAS_CARACTERISTICA, y = n, label = n, fill = LAZER_CAMPINAS_CARACTERISTICA)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Lazer em Campinas",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )


## VIAGEM ####
viagem_participante <- infs2 %>% 
  distinct(PARTICIPANTE, VIAGEM) %>%  #garante 1 linha por participante
  count(VIAGEM) %>%
  mutate(VIAGEM = fct_explicit_na(VIAGEM, "Não informado")) %>%  # Transforma NA em categoria
  print()

viagem_participante %>%
  ggplot(aes(x = VIAGEM, y = n, label = n, fill = VIAGEM)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Viagem",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )


## VIAGEM LUGAR ####
viagemlugar_participante <- infs2 %>% 
  distinct(PARTICIPANTE, VIAGEM_LUGAR) %>%  #garante 1 linha por participante
  count(VIAGEM_LUGAR) %>%
  mutate(VIAGEM_LUGAR = fct_explicit_na(VIAGEM_LUGAR, "Não informado")) %>%  # Transforma NA em categoria
  print()

viagemlugar_participante %>%
  ggplot(aes(x = VIAGEM_LUGAR, y = n, label = n, fill = VIAGEM_LUGAR)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Viagem Lugar",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )


## VIAGEM VONTADE ####
viagemvontade_participante <- infs2 %>% 
  distinct(PARTICIPANTE, LAZER_VIAGEM_VONTADE2) %>%  #garante 1 linha por participante
  count(LAZER_VIAGEM_VONTADE2) %>%
  mutate(LAZER_VIAGEM_VONTADE2 = fct_explicit_na(LAZER_VIAGEM_VONTADE2, "Não informado")) %>%  # Transforma NA em categoria
  print()

viagemvontade_participante %>%
  ggplot(aes(x = LAZER_VIAGEM_VONTADE2, y = n, label = n, fill = LAZER_VIAGEM_VONTADE2)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Viagem Vontade",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )



## REGIÃO ####
regiao_participante <- infs2 %>% 
  distinct(PARTICIPANTE, BAIRRO_REGIAO2) %>%  #garante 1 linha por participante
  count(BAIRRO_REGIAO2) %>%
  mutate(BAIRRO_REGIAO2 = fct_explicit_na(BAIRRO_REGIAO2, "Não informado")) %>%  # Transforma NA em categoria
  print()

regiao_participante %>%
  ggplot(aes(x = BAIRRO_REGIAO2, y = n, label = n, fill = BAIRRO_REGIAO2)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Região",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )


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
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
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
    x = "Região",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )


## NQUARTOS ####
quartos_participante <- infs2 %>% 
  distinct(PARTICIPANTE, NQUARTOS) %>%  #garante 1 linha por participante
  count(NQUARTOS) %>%
  mutate(NQUARTOS = fct_explicit_na(NQUARTOS, "Não informado")) %>%  # Transforma NA em categoria
  print()

quartos_participante %>%
  ggplot(aes(x = NQUARTOS, y = n, label = n, fill = NQUARTOS)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Quartos",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )


## NBANHEIROS ####
banheiros_participante <- infs2 %>% 
  distinct(PARTICIPANTE, NBANHEIROS) %>%  #garante 1 linha por participante
  count(NBANHEIROS) %>%
  mutate(NBANHEIROS = fct_explicit_na(NBANHEIROS, "Não informado")) %>%  # Transforma NA em categoria
  print()

banheiros_participante %>%
  ggplot(aes(x = NBANHEIROS, y = n, label = n, fill = NBANHEIROS)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Banheiros",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )



## NBANHEIROS ####
comodos_participante <- infs2 %>% 
  distinct(PARTICIPANTE, NCOMODOS) %>%  #garante 1 linha por participante
  count(NCOMODOS) %>%
  mutate(NCOMODOS = fct_explicit_na(NCOMODOS, "Não informado")) %>%  # Transforma NA em categoria
  print()

comodos_participante %>%
  ggplot(aes(x = NCOMODOS, y = n, label = n, fill = NCOMODOS)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Comodos",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )

## DENSIDADE_HABITACAO ####
densidade_participante <- infs2 %>%
  distinct(PARTICIPANTE, DENSIDADE_HABITACAO) %>%
  count(DENSIDADE_HABITACAO)

ggplot(densidade_participante, aes(x = DENSIDADE_HABITACAO, y = n)) +
  geom_line(color = "#3182bd", size = 1) +
  geom_point(color = "#08519c", size = 2) +
  labs(
    x = "Densidade habitacional",
    y = "Número de participantes",
    title = "Distribuição da densidade habitacional"
  ) +
  theme_minimal(base_size = 13)

## RENDA_IND ####
rendaind_participante <- infs2 %>% 
  distinct(PARTICIPANTE, RENDA_IND) %>%  #garante 1 linha por participante
  count(RENDA_IND) %>%
  mutate(RENDA_IND = fct_explicit_na(RENDA_IND, "Não informado")) %>%  # Transforma NA em categoria
  print()

rendaind_participante %>%
  ggplot(aes(x = RENDA_IND, y = n, label = n, fill = RENDA_IND)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Renda Individual",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )


## RENDA_FAM ####
rendafam_participante <- infs2 %>% 
  distinct(PARTICIPANTE, RENDA_FAM) %>%  #garante 1 linha por participante
  count(RENDA_FAM) %>%
  mutate(RENDA_FAM = fct_explicit_na(RENDA_FAM, "Não informado")) %>%  # Transforma NA em categoria
  print()

rendafam_participante %>%
  ggplot(aes(x = RENDA_FAM, y = n, label = n, fill = RENDA_FAM)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Renda Familiar",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )


## ESCOLA_PAI2 ####
escolapai_participante <- infs2 %>% 
  distinct(PARTICIPANTE, ESCOLA_PAI2) %>%  #garante 1 linha por participante
  count(ESCOLA_PAI2) %>%
  mutate(ESCOLA_PAI2 = fct_explicit_na(ESCOLA_PAI2, "Não informado")) %>%  # Transforma NA em categoria
  print()

escolapai_participante %>%
  ggplot(aes(x = ESCOLA_PAI2, y = n, label = n, fill = ESCOLA_PAI2)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "escola do Pai",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )


## ESCOLA_MAE2 ####
escolamae_participante <- infs2 %>% 
  distinct(PARTICIPANTE, ESCOLA_MAE2) %>%  #garante 1 linha por participante
  count(ESCOLA_MAE2) %>%
  mutate(ESCOLA_MAE2 = fct_explicit_na(ESCOLA_MAE2, "Não informado")) %>%  # Transforma NA em categoria
  print()

escolamae_participante %>%
  ggplot(aes(x = ESCOLA_MAE2, y = n, label = n, fill = ESCOLA_MAE2)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    x = "Escola da Mãe",
    y = "Número de Participantes")+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5), panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25), axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9)   # tamanho do título eixo Y
  )


## INDICE_OCUPACAO_PAI ####
ocupacaopai_participante <- infs2 %>%
  distinct(PARTICIPANTE, INDICE_OCUPACAO_PAI) %>%
  count(INDICE_OCUPACAO_PAI)

ggplot(ocupacaopai_participante, aes(x = INDICE_OCUPACAO_PAI, y = n)) +
  geom_line(color = "#3182bd", size = 1) +
  geom_point(color = "#08519c", size = 2) +
  labs(
    x = "Ocupação Pai",
    y = "Número de participantes",
    title = "Distribuição da densidade habitacional"
  ) +
  theme_minimal(base_size = 13)


## INDICE_OCUPACAO_MAE ####
ocupacaomae_participante <- infs2 %>%
  distinct(PARTICIPANTE, INDICE_OCUPACAO_MAE) %>%
  count(INDICE_OCUPACAO_MAE)

ggplot(ocupacaomae_participante, aes(x = INDICE_OCUPACAO_MAE, y = n)) +
  geom_line(color = "#3182bd", size = 1) +
  geom_point(color = "#08519c", size = 2) +
  labs(
    x = "Ocupação Mãe",
    y = "Número de participantes",
    title = "Distribuição da densidade habitacional"
  ) +
  theme_minimal(base_size = 13)




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


### Bairo região ####
tab.BAIRO.REGIAO <- infs2 %>%
  filter(!is.na(BAIRRO_REGIAO2)) %>%
  distinct(PARTICIPANTE, BAIRRO_REGIAO2) %>% 
  count(BAIRRO_REGIAO2) %>% 
  print()

ggplot(tab.BAIRO.REGIAO, aes(x = BAIRRO_REGIAO2, y = n)) +
  geom_bar(stat = "identity", color = "white", fill = "#FC9272") +
  labs(x = "Região",
      y = "Número de Participantes")+
  geom_text(aes(label = paste0("(", n, ")")), vjust = -0.2, size = 3.5) +
  theme_minimal()+
  theme(panel.grid.major = element_line(color = alpha("gray70", 0.2), linewidth = 0.5),
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9),   # tamanho do título eixo Y
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
        panel.grid.minor = element_line(color = alpha("gray85", 0.1), linewidth = 0.25),
        axis.title.x = element_text(size = 9),  # tamanho do título eixo X
        axis.title.y = element_text(size = 9),   # tamanho do título eixo Y
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
  distinct(PARTICIPANTE, OCUPACAO_LOCOMOCAO2) %>%  #garante 1 linha por participante
  count(OCUPACAO_LOCOMOCAO2) %>% 
  arrange(OCUPACAO_LOCOMOCAO2) %>% 
  print()

locomocao %>% 
  ggplot(aes(x = factor(OCUPACAO_LOCOMOCAO2), y = n, label = n)) +
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
  distinct(PARTICIPANTE, ESCOLARIDADE2) %>%  #garante 1 linha por participante
  count(ESCOLARIDADE2) %>% 
  arrange(ESCOLARIDADE2) %>% 
  print()

escolaridade %>% 
  ggplot(aes(x = factor(ESCOLARIDADE2), y = n, label = n)) +
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

# RENDA ####
renda_familiar <- dados2 %>% 
  distinct(PARTICIPANTE, RENDA_FAM) %>%  #garante 1 linha por participante
  mutate(RENDA_FAM = fct_explicit_na(RENDA_FAM, "Não informado")) %>%  # Transforma NA em categoria
  count(RENDA_FAM) %>% 
  arrange(RENDA_FAM) %>% 
  print()

renda_familiar %>% 
  ggplot(aes(x = factor(RENDA_FAM), y = n, label = n)) +
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