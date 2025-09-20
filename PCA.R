#PCA ANALISE

escalas_clean <- na.omit(escalas)

pca_res <- prcomp(escalas_clean, center = TRUE, scale. = TRUE)

summary(pca_res)          # variância explicada por cada componente
pca$rotation           # cargas (contribuição das variáveis)
pca$x                  # coordenadas dos indivíduos


colnames(dados_AP)
