#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%% Comparações entre os três processos %%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# FEATURE SELECTION - LASSO ####
# Função pra extrair e arrumar
coef_to_df <- function(coef_obj, nome){
  as.data.frame(as.matrix(coef_obj)) %>%
    rownames_to_column("VARIAVEL") %>%
    rename(COEF = 2) %>%
    filter(VARIAVEL != "(Intercept)") %>%  # 🔹 ignora intercepto
    mutate(MODELO = nome)
}


AP_df <- coef_to_df(coef(lasso_AP, s = "lambda.min"), "PALATALIZACAO")
S0_df <- coef_to_df(coef(lasso_S0, s = "lambda.min"), "APAGAMENTO")
HAP_df  <- coef_to_df(coef(lasso_HAP, s = "lambda.min"), "ASPIRACAO")

# garantir a ordem original das variáveis conforme aparecem nos dados combinados
# ordem_variaveis <- coef_3processos %>%
#   distinct(VARIAVEL) %>%
#   pull(VARIAVEL)

coef_3processos <- coef_3processos %>%
  mutate(VARIAVEL = factor(VARIAVEL, levels = c(
    "INDICE_ESCOL_MAE_norm",
    "INDICE_ESCOL_PAI_norm",
    "INDICE_ESCOL3_norm",
    "INDICE_OCUPACAO_norm",
    "INDICE_OCUPACAO_SONHOS2_norm",
    "INDICE_RENDA_IND_norm",
    "INDICE_INFANCIA_norm",
    "INDICE_LAZER_norm",
    "INDICE_LAZER_CAMPINAS_norm",
    "INDICE_MEGA_norm",
    "INDICE_VIAGEM_norm",
    "INDICE_VIAGEM_LUGAR_norm",
    "INDICE_VIAGEM_VONTADE_norm"
  )))

coef_3processos <- bind_rows(AP_df, S0_df, HAP_df) %>% 
  complete(VARIAVEL, MODELO, fill = list(COEF = 0)) %>%
  mutate(
    MODELO = factor(MODELO, levels = c("PALATALIZACAO", "APAGAMENTO", "ASPIRACAO")),
    VARIAVEL = factor(VARIAVEL, levels = c(
      "INDICE_ESCOL_MAE_norm",
      "INDICE_ESCOL_PAI_norm",
      "INDICE_ESCOL3_norm",
      "INDICE_OCUPACAO_norm",
      "INDICE_OCUPACAO_SONHOS2_norm",
      "INDICE_RENDA_IND_norm",
      "INDICE_INFANCIA_norm",
      "INDICE_LAZER_norm",
      "INDICE_LAZER_CAMPINAS_norm",
      "INDICE_MEGA_norm",
      "INDICE_VIAGEM_norm",
      "INDICE_VIAGEM_LUGAR_norm",
      "INDICE_VIAGEM_VONTADE_norm"
    ))
  )


# gráfico
ggplot(coef_3processos, aes(x = VARIAVEL, y = COEF, fill = MODELO)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_vline(
    xintercept = seq(1.5, length(unique(coef_3processos$VARIAVEL)) - 0.5, 1),
    color = "grey80", linewidth = 0.4
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "PALATALIZACAO" = "#3366CC",
      "APAGAMENTO" = "#DC3912",
      "ASPIRACAO" = "#FF9900"
    ),
    breaks = c("PALATALIZACAO", "APAGAMENTO", "ASPIRACAO")
  ) +
  theme_light() +
  theme(
    panel.grid = element_blank(),    # remove TODAS as linhas de fundo
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )



# gráfico lollipop alinhado
ggplot(coef_3processos, aes(x = VARIAVEL, y = COEF, color = MODELO)) +
  # linha do lollipop
  geom_segment(
    aes(x = as.numeric(VARIAVEL) + 
          (as.numeric(factor(MODELO, levels = c("PALATALIZACAO", "APAGAMENTO", "ASPIRACAO"))) - 2) * 0.2, 
        xend = as.numeric(VARIAVEL) + 
          (as.numeric(factor(MODELO, levels = c("PALATALIZACAO", "APAGAMENTO", "ASPIRACAO"))) - 2) * 0.2, 
        y = 0, yend = COEF),
    linewidth = 0.9
  ) +
  # ponto do lollipop
  geom_point(
    aes(x = as.numeric(VARIAVEL) + 
          (as.numeric(factor(MODELO, levels = c("PALATALIZACAO", "APAGAMENTO", "ASPIRACAO"))) - 2) * 0.2), shape = 16, size = 2 ) +
  geom_vline(
    xintercept = seq(1.5, length(unique(coef_3processos$VARIAVEL)) - 0.5, 1),
    color = "grey85", linewidth = 0.4
  ) +
  coord_flip() +
  scale_color_manual(
    values = c(
      "PALATALIZACAO" = "#3366CC",
      "APAGAMENTO" = "#DC3912",
      "ASPIRACAO" = "#FF9900"
    )
  ) +
  scale_x_continuous(
    breaks = 1:length(unique(coef_3processos$VARIAVEL)),
    labels = unique(coef_3processos$VARIAVEL)
  ) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )




## Pai #############################################
escolaridade_pai_3processos <- bind_rows(
  AP.prop_ESCOLA_PAI2 %>% filter(VD == "P", !is.na(ESCOLA_PAI2)) %>% mutate(variavel = "P"),
  S0.prop_ESCOLA_PAI2 %>% filter(VD == "0", !is.na(ESCOLA_PAI2)) %>% mutate(variavel = "0"), 
  HAP.prop_ESCOLA_PAI2 %>% filter(VD == "H", !is.na(ESCOLA_PAI2)) %>% mutate(variavel = "H")) %>% 
  mutate(variavel = factor(variavel, levels = c("P", "0", "H")))

# Verificando se os dados contêm "P"
table(escolaridade_pai_3processos$variavel)


png("C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa/graficos/escolaridade_pai_3processos.png", width = 6.5, height = 4.5, units = "in", res = 300)
grafico.escolaridade.pai <- ggplot(escolaridade_pai_3processos, aes(x = ESCOLA_PAI2, y = prop * 100, group = variavel, color = variavel)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = label), vjust = -0.5, size = 3) +
  scale_x_discrete(labels = c(
      "analfabeto" = "Analfabeto",
      "fund" = "Ens. Fund.",
      "medio" = "Ens. Médio",
      "superior" = "Ens. Superior"))+
  scale_y_continuous(
    limits = c(0, NA),  # Define mínimo como 0 e máximo automático
    expand = expansion(mult = c(0, 0.2)),  # Adiciona 20% de espaço no topo
    labels = scales::percent_format(scale = 1)
  ) +
  scale_color_discrete(
    labels = c("P" = "Palatalização", 
               "0" = "Apagamento",
               "H" = "Aspiração"))+
  labs(
    x = "Escolaridade do Pai",
    y = "Proporção da variante (%)",
    color = "Variável Dependente"
  ) +
  theme_minimal()+
theme(
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.text.x = element_text(angle = 20, hjust = 1)
)
dev.off()


## Mae ####
escolaridade_mae_3processos <- bind_rows(
  AP.prop_ESCOLA_MAE2 %>% filter(VD == "P", !is.na(ESCOLA_MAE2)) %>% mutate(variavel = "P"),
  S0.prop_ESCOLA_MAE2 %>% filter(VD == "0", !is.na(ESCOLA_MAE2)) %>% mutate(variavel = "0"), 
  HAP.prop_ESCOLA_MAE2 %>% filter(VD == "H", !is.na(ESCOLA_MAE2)) %>% mutate(variavel = "H")) %>% 
  mutate(variavel = factor(variavel, levels = c("P", "0", "H")))


grafico.escolaridade.mae <- ggplot(escolaridade_mae_3processos, aes(x = ESCOLA_MAE2, y = prop * 100, group = variavel, color = variavel)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = label), vjust = -0.5, size = 3) +
  scale_x_discrete(labels = c(
    "analfabeto" = "Analfabeto",
    "fund" = "Ens. Fund.",
    "medio" = "Ens. Médio",
    "superior" = "Ens. Superior"))+
  scale_y_continuous(
    limits = c(0, NA),  # Define mínimo como 0 e máximo automático
    expand = expansion(mult = c(0, 0.2)),  # Adiciona 20% de espaço no topo
    labels = scales::percent_format(scale = 1)
  ) +
  scale_color_discrete(
    labels = c("P" = "Palatalização", 
               "0" = "Apagamento",
               "H" = "Aspiração"))+
   labs(
    x = "Escolaridade da Mãe",
    y = "Proporção da variante (%)",
    color = "Variável Dependente"
  ) +
  theme_minimal()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )


png("C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa/graficos/escolaridades.png", width = 9, height = 4.5, units = "in", res = 300)
(grafico.escolaridade | grafico.escolaridade.pai | grafico.escolaridade.mae) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
dev.off()



# OCUPAÇÃO ####
## Participante ####

ocupacao_3processos <- bind_rows(
  AP.prop_INDICE_OCUPACAO %>% filter(VD == "P") %>% mutate(variavel = "P"),
  S0.prop_INDICE_OCUPACAO %>% filter(VD == "0") %>% mutate(variavel = "0"), 
  HAP.prop_INDICE_OCUPACAO %>% filter(VD == "H") %>% mutate(variavel = "H")) %>% 
  mutate(variavel = factor(variavel, levels = c("P", "0", "H")))

# Verificando se os dados contêm "P"
table(ocupacao_3processos$variavel)

png("C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa/graficos/ocupacao_3processos.png", width = 6.5, height = 4.5, units = "in", res = 300)
grafico.ocupacao <- ggplot(ocupacao_3processos, aes(x = INDICE_OCUPACAO, y = prop * 100, group = variavel, color = variavel)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = label), vjust = -0.5, size = 3) +
   scale_y_continuous(
    limits = c(0, NA),  # Define mínimo como 0 e máximo automático
    expand = expansion(mult = c(0, 0.2)),  # Adiciona 20% de espaço no topo
    labels = scales::percent_format(scale = 1)
  ) +
  scale_x_continuous(limits = c(0, 8))+
  scale_color_discrete(
    labels = c("P" = "Palatalização",
               "0" = "Apagamento",
               "H" = "Aspiração"))+
  labs(
    x = "Ocupação",
    y = "Proporção da variante (%)",
    color = "Variável Dependente"
  ) +
  theme_minimal()
dev.off()
grafico.ocupacao



## Pai ####
ocupacao_pai_3processos <- bind_rows(
  AP.prop_INDICE_OCUPACAO_PAI %>% filter(VD == "P", !is.na(INDICE_OCUPACAO_PAI)) %>% mutate(variavel = "P"),
  S0.prop_INDICE_OCUPACAO_PAI %>% filter(VD == "0", !is.na(INDICE_OCUPACAO_PAI)) %>% mutate(variavel = "0"), 
  HAP.prop_INDICE_OCUPACAO_PAI %>% filter(VD == "H", !is.na(INDICE_OCUPACAO_PAI)) %>% mutate(variavel = "H")) %>% 
  mutate(variavel = factor(variavel, levels = c("P", "0", "H")))

# Verificando se os dados contêm "P"
table(ocupacao_pai_3processos$variavel)


png("C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa/graficos/ocupacao_pai_3processos.png", width = 6.5, height = 4.5, units = "in", res = 300)
grafico.ocupacao.pai <- ggplot(ocupacao_pai_3processos, aes(x = INDICE_OCUPACAO_PAI, y = prop * 100, group = variavel, color = variavel)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = label), vjust = -0.5, size = 3) +
  scale_y_continuous(
    limits = c(0, NA),  # Define mínimo como 0 e máximo automático
    expand = expansion(mult = c(0, 0.2)),  # Adiciona 20% de espaço no topo
    labels = scales::percent_format(scale = 1)
  ) +
  scale_x_continuous(limits = c(0, 8))+
  scale_color_discrete(
    labels = c("P" = "Palatalização", 
               "0" = "Apagamento",
               "H" = "Aspiração"))+
  labs(
    x = "Ocupação do Pai",
    y = "Proporção da variante (%)",
    color = "Variável Dependente"
  ) +
  theme_minimal()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
  )
dev.off()
grafico.ocupacao.pai


## Mae ####
ocupacao_mae_3processos <- bind_rows(
  AP.prop_INDICE_OCUPACAO_MAE %>% filter(VD == "P", !is.na(INDICE_OCUPACAO_MAE)) %>% mutate(variavel = "P"),
  S0.prop_INDICE_OCUPACAO_MAE %>% filter(VD == "0", !is.na(INDICE_OCUPACAO_MAE)) %>% mutate(variavel = "0"), 
  HAP.prop_INDICE_OCUPACAO_MAE %>% filter(VD == "H", !is.na(INDICE_OCUPACAO_MAE)) %>% mutate(variavel = "H")) %>% 
  mutate(variavel = factor(variavel, levels = c("P", "0", "H")))


grafico.ocupacao.mae <- ggplot(ocupacao_mae_3processos, aes(x = INDICE_OCUPACAO_MAE, y = prop * 100, group = variavel, color = variavel)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = label), vjust = -0.5, size = 3) +
  scale_y_continuous(
    limits = c(0, NA),  # Define mínimo como 0 e máximo automático
    expand = expansion(mult = c(0, 0.2)),  # Adiciona 20% de espaço no topo
    labels = scales::percent_format(scale = 1)
  ) +
  scale_x_continuous(limits = c(0, 8))+
  scale_color_discrete(
    labels = c("P" = "Palatalização", 
               "0" = "Apagamento",
               "H" = "Aspiração"))+
  labs(
    x = "Ocupação da Mãe",
    y = "Proporção da variante (%)",
    color = "Variável Dependente"
  ) +
  theme_minimal()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())
grafico.ocupacao.mae


png("C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa/graficos/ocupacoes.png", width = 9, height = 4.5, units = "in", res = 300)
(grafico.ocupacao | grafico.ocupacao.pai | grafico.ocupacao.mae) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
dev.off()


# RENDA  ####
## Individual ####

renda_3processos <- bind_rows(
  AP.prop_LAZER_CAMPINAS_CARACTERISTICA %>% filter(VD == "P", !is.na(LAZER_CAMPINAS_CARACTERISTICA)) %>% mutate(variavel = "P"),
  S0.prop_LAZER_CAMPINAS_CARACTERISTICA %>% filter(VD == "0", !is.na(LAZER_CAMPINAS_CARACTERISTICA)) %>% mutate(variavel = "0"), 
  HAP.prop_LAZER_CAMPINAS_CARACTERISTICA %>% filter(VD == "H", !is.na(LAZER_CAMPINAS_CARACTERISTICA)) %>% mutate(variavel = "H")) %>% 
  mutate(variavel = factor(variavel, levels = c("P", "0", "H")))

# Verificando se os dados contêm "P"
table(renda_3processos$variavel)

png("C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa/graficos/renda_3processos.png", width = 6.5, height = 4.5, units = "in", res = 300)
grafico.renda <- ggplot(renda_3processos, aes(x = LAZER_CAMPINAS_CARACTERISTICA, y = prop * 100, group = variavel, color = variavel)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = label), vjust = -0.5, size = 3) +
  scale_y_continuous(
    limits = c(0, NA),  # Define mínimo como 0 e máximo automático
    expand = expansion(mult = c(0, 0.2)),  # Adiciona 20% de espaço no topo
    labels = scales::percent_format(scale = 1)
  ) +
  scale_color_discrete(
    labels = c("P" = "Palatalização",
               "0" = "Apagamento",
               "H" = "Aspiração"))+
  labs(
    x = "Renda",
    y = "Proporção da variante (%)",
    color = "Variável \nDependente"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
dev.off()
grafico.renda

## Familiar ####
renda_fam_3processos <- bind_rows(
  AP.prop_RENDA_FAM %>% filter(VD == "P", !is.na(RENDA_FAM)) %>% mutate(variavel = "P"),
  S0.prop_RENDA_FAM %>% filter(VD == "0", !is.na(RENDA_FAM)) %>% mutate(variavel = "0"), 
  HAP.prop_RENDA_FAM %>% filter(VD == "H", !is.na(RENDA_FAM)) %>% mutate(variavel = "H")) %>% 
  mutate(variavel = factor(variavel, levels = c("P", "0", "H")))

# Verificando se os dados contêm "P"
table(renda_fam_3processos$variavel)

png("C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa/graficos/renda_fam_3processos.png", width = 6.5, height = 4.5, units = "in", res = 300)
grafico.renda_fam <- ggplot(renda_fam_3processos, aes(x = RENDA_FAM, y = prop * 100, group = variavel, color = variavel)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = label), vjust = -0.5, size = 3) +
  scale_y_continuous(
    limits = c(0, NA),  # Define mínimo como 0 e máximo automático
    expand = expansion(mult = c(0, 0.2)),  # Adiciona 20% de espaço no topo
    labels = scales::percent_format(scale = 1)
  ) +
  scale_color_discrete(
    labels = c("P" = "Palatalização",
               "0" = "Apagamento",
               "H" = "Aspiração"))+
  labs(
    x = "Renda Familiar",
    y = "Proporção da variante (%)",
    color = "Variável \nDependente"
  ) +
  theme_minimal()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1))
dev.off()
grafico.renda_fam


png("C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa/graficos/rendas.png", width = 9, height = 4.5, units = "in", res = 300)
(grafico.renda | grafico.renda_fam) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
dev.off()

# LAZER  ####
## Oq faz no tempo livre ####

lazer_3processos <- bind_rows(
  AP.prop_LAZER_CARACTERISTICA %>% filter(VD == "P", !is.na(LAZER_CARACTERISTICA)) %>% mutate(variavel = "P"),
  S0.prop_LAZER_CARACTERISTICA %>% filter(VD == "0", !is.na(LAZER_CARACTERISTICA)) %>% mutate(variavel = "0"), 
  HAP.prop_LAZER_CARACTERISTICA %>% filter(VD == "H", !is.na(LAZER_CARACTERISTICA)) %>% mutate(variavel = "H")) %>% 
  mutate(variavel = factor(variavel, levels = c("P", "0", "H")))

# Verificando se os dados contêm "P"
table(lazer_3processos$variavel)

png("C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa/graficos/lazer_3processos.png", width = 6.5, height = 4.5, units = "in", res = 300)
grafico.lazer <- ggplot(lazer_3processos, aes(x = LAZER_CARACTERISTICA, y = prop * 100, group = variavel, color = variavel)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = label), vjust = -0.5, size = 3) +
  scale_y_continuous(
    limits = c(0, NA),  # Define mínimo como 0 e máximo automático
    expand = expansion(mult = c(0, 0.2)),  # Adiciona 20% de espaço no topo
    labels = scales::percent_format(scale = 1)
  ) +
  scale_x_discrete(labels = c("ambos" = "Ambos", "custo" = "Custo", "nao.sai" = "Não sai", "sem.custo" =  "Sem custo"))+
  scale_color_discrete(
    labels = c("P" = "Palatalização",
               "0" = "Apagamento",
               "H" = "Aspiração"))+
  labs(
    x = "Lazer",
    y = "Proporção da variante (%)",
    color = "Variável \nDependente"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
dev.off()
grafico.lazer

## Lazer em Campinas ####
lazer_campinas_3processos <- bind_rows(
  AP.prop_LAZER_CAMPINAS_CARACTERISTICA %>% filter(VD == "P", !is.na(LAZER_CAMPINAS_CARACTERISTICA)) %>% mutate(variavel = "P"),
  S0.prop_LAZER_CAMPINAS_CARACTERISTICA %>% filter(VD == "0", !is.na(LAZER_CAMPINAS_CARACTERISTICA)) %>% mutate(variavel = "0"), 
  HAP.prop_LAZER_CAMPINAS_CARACTERISTICA %>% filter(VD == "H", !is.na(LAZER_CAMPINAS_CARACTERISTICA)) %>% mutate(variavel = "H")) %>% 
  mutate(variavel = factor(variavel, levels = c("P", "0", "H")))

# Verificando se os dados contêm "P"
table(lazer_campinas_3processos$variavel)

png("C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa/graficos/lazer_campinas_3processos.png", width = 6.5, height = 4.5, units = "in", res = 300)
grafico.lazer_campinas <- ggplot(lazer_campinas_3processos, aes(x = LAZER_CAMPINAS_CARACTERISTICA, y = prop * 100, group = variavel, color = variavel)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = label), vjust = -0.5, size = 3) +
  scale_y_continuous(
    limits = c(0, NA),  # Define mínimo como 0 e máximo automático
    expand = expansion(mult = c(0, 0.2)),  # Adiciona 20% de espaço no topo
    labels = scales::percent_format(scale = 1)
  ) +
  scale_x_discrete(labels = c("ambos" = "Ambos", "custo" = "Custo","nao" = "Não tem", "nao.sai" = "Não sai", "sem.custo" =  "Sem custo"))+
  scale_color_discrete(
    labels = c("P" = "Palatalização",
               "0" = "Apagamento",
               "H" = "Aspiração"))+
  labs(
    x = "Lazer em Campinas",
    y = "Proporção da variante (%)",
    color = "Variável \nDependente"
  ) +
  theme_minimal()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1))
dev.off()
grafico.lazer_campinas



## Infancia ####

infancia_3processos <- bind_rows(
  AP.prop_INFANCIA_MEMORIA %>%
    as.data.frame() %>%
    filter(VD == "P", !is.na(INFANCIA_MEMORIA)) %>%
    mutate(variavel = "P"),
  
  S0.prop_INFANCIA_MEMORIA %>%
    as.data.frame() %>%
    filter(VD == "0", !is.na(INFANCIA_MEMORIA)) %>%
    mutate(variavel = "0"),
  
  HAP.prop_INFANCIA_MEMORIA %>%
    as.data.frame() %>%
    filter(VD == "H", !is.na(INFANCIA_MEMORIA)) %>%
    mutate(variavel = "H")
) %>%
  mutate(variavel = factor(variavel, levels = c("P", "0", "H")))

# Verificando se os dados contêm "P"
table(infancia_3processos$variavel)


png("C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa/graficos/infancia_3processos.png", width = 6.5, height = 4.5, units = "in", res = 300)
grafico.infancia <- ggplot(infancia_3processos, aes(x = INFANCIA_MEMORIA, y = prop * 100, group = variavel, color = variavel)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = label), vjust = -0.5, size = 3) +
  scale_y_continuous(
    limits = c(0, NA),  # Define mínimo como 0 e máximo automático
    expand = expansion(mult = c(0, 0.2)),  # Adiciona 20% de espaço no topo
    labels = scales::percent_format(scale = 1)
  ) +
  #scale_x_discrete(labels = c("ambos" = "Ambos", "custo" = "Custo", "nao.sai" = "Não sai", "sem.custo" =  "Sem custo"))+
  scale_color_discrete(
    labels = c("P" = "Palatalização",
               "0" = "Apagamento",
               "H" = "Aspiração"))+
  labs(
    x = "Memória de Infância",
    y = "Proporção da variante (%)",
    color = "Variável \nDependente"
  ) +
  theme_minimal()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1))
dev.off()
grafico.infancia

png("C:/Users/sarah/Downloads/analiseSclasse/analise-quantitativa/graficos/lazeres.png", width = 9, height = 4.5, units = "in", res = 300)
(grafico.lazer | grafico.lazer_campinas | grafico.infancia) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
dev.off()
