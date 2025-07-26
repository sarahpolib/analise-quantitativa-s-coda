#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%% Comparações entre os três processos %%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#ESCOLARIDADE PAI ####
escolaridade_pai_3processos <- bind_rows(
  AP.prop_ESCOLA_PAI2 %>% filter(VD == "P") %>% mutate(variavel = "P"),
  S0.prop_ESCOLA_PAI2 %>% filter(VD == "0") %>% mutate(variavel = "0"), 
  HAP.prop_ESCOLA_PAI2 %>% filter(VD == "H") %>% mutate(variavel = "H")) %>% 
  mutate(variavel = factor(variavel, levels = c("P", "0", "H")))

# Verificando se os dados contêm "P"
table(escolaridade_pai_3processos$variavel)


ggplot(escolaridade_pai_3processos, aes(x = ESCOLA_PAI2, y = prop * 100, group = variavel, color = variavel)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = label), vjust = -0.5, size = 3) +
  scale_y_continuous(
    limits = c(0, NA),  # Define mínimo como 0 e máximo automático
    expand = expansion(mult = c(0, 0.2)),  # Adiciona 20% de espaço no topo
    labels = scales::percent_format(scale = 1)
  ) +
  labs(
    x = "Escolaridade do Pai",
    y = "Proporção da variante(%)",
    color = "Variável Dependente"
  ) +
  theme_minimal()



#ESCOLARIDADE MAE ####
AP <- AP.prop_ESCOLA_MAE2 %>% 
  filter(VD == "P") %>% 
  mutate(variavel = "P")


S0 <-  S0.prop_ESCOLA_MAE2 %>% 
  filter(VD == "0") %>% 
  mutate(variavel = "0")


HAP <- HAP.prop_ESCOLA_MAE2 %>% 
  filter(VD == "H") %>% 
  mutate(variavel = "H")


escoalridade_mae_3processos <- bind_rows(AP, S0, HAP,) %>% 
  mutate(variavel = factor(variavel, levels = c("P", "0", "H"))) 



ggplot(escoalridade_mae_3processos, aes(x = ESCOLA_MAE2, y = prop * 100, group = variavel, color = variavel)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = label), vjust = -0.5, size = 3) +
  # scale_y_continuous(
  #   limits = c(0, 10),  # ajuste se necessário
  #   labels = function(x) paste0(x, "%")
  # ) +
   labs(
    x = "Escolaridade da Mãe",
    y = "Proporção da variante P (%)",
    color = "Variável Dependente"
  ) +
  theme_minimal()
