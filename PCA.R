#PCA ANALISE

colunas_norm_AP <- names(dados_AP)[grepl("_norm$", names(dados_AP))]

# criar um data frame só com as colunas normalizadas
dados_pca_AP <- dados_AP %>% select(all_of(VD, colunas_norm_AP))

dados_pca_AP_clean <- dados_pca_AP %>% na.omit()

pca_AP <- prcomp(dados_pca_AP_clean, center = TRUE, scale. = TRUE)  # centro e escala

summary(pca)          # variância explicada por cada componente
pca$rotation           # cargas (contribuição das variáveis)
pca$x                  # coordenadas dos indivíduos


View(dados_AP
     )


colnames(dados_AP)
