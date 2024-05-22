#' @title Manejo de Dados de Parcelas Florestais
#'
#' @description Roteiro de aula prática da disciplina LCB-0217 do
#'   curso de Ciências Biológicas da ESALQ/USP de Piracicaba, SP,
#'   Brasil
#' 
#' @author Renato A. Ferreira de Lima \email{raflima@usp.br}
#' 
#' @details
#' Dados originais de entrada não são os originais e foram manipulados
#' manualmente para auxiliar na atividade prática (não use os dados em
#' outro contexto)
#' 
#'
# INSTALANDO OS PACOTES NECESSÁRIOS ---------------------------------

if (!requireNamespace("remotes")) install.packages("remotes")

if (!requireNamespace("flora")) remotes::install_github("gustavobio/flora")

if (!requireNamespace("vegan")) install.packages("vegan")

if (!requireNamespace("BIOMASS")) install.packages("BIOMASS")

if (!requireNamespace("readxl")) install.packages("readxl")

if (!requireNamespace("writexl")) install.packages("writexl")


# LENDO OS DADOS ----------------------------------------------------
## Definindo o nome do arquivo e o caminho certo até os dados
minha_pasta <- "dados/dados-brutos"
nome_arquivo <- "dados_piracaia.xlsx"
meu_caminho <- file.path(minha_pasta, nome_arquivo)

## Verificando se a pasta e o arquivo estão no seu computador. Se não,
# a pasta é criada e o arquivo é baixado do repositório online
if (!dir.exists(minha_pasta))
  dir.create(minha_pasta, recursive = TRUE)

if (!nome_arquivo %in% list.files(minha_pasta)) {
  url <- "https://github.com/LimaRAF/LCB0217/tree/master/dados/dados-brutos"
  download.file(paste0(url, "/", nome_arquivo),
                destfile = meu_caminho)
}

## Lendo o arquivo
parcelas <- as.data.frame(readxl::read_excel(meu_caminho, sheet = 1))
head(parcelas, n = 3)

descricao <- as.data.frame(readxl::read_excel(meu_caminho, sheet = 2))
head(descricao, n = 3)



# INSPECIONANDO/EDITANDO OS DADOS ---------------------------------

## Quantas parcelas?
nrow(descricao)
length(unique(parcelas$Parcela))

## Quantos hectares amostrados? Valores em hectares
(esforco <- sum(descricao$Area_ha))

## Quantos indivíduos? Ou seja, de números de linhas na planilha?
(Ntotal <- nrow(parcelas))

## Qual o menor e maior perímetros? Tudo ok? Valores em centímetros
min(parcelas$PAP1)
max(parcelas$PAP1) # Parece haver um erro de digitação! 15 virou 15000!
summary(parcelas$PAP1)

## Substituição de PAP muito alto (>10 m)
paps_para_substituir <- parcelas$PAP1 > 1000
parcelas$PAP1[paps_para_substituir] <- 
  parcelas$PAP1[paps_para_substituir]/1000

## Qual a menor e maior altura? Tudo ok? Valores em metros
range(parcelas$AlturaTotal_m)


# NOMES DAS ESPÉCIES ----------------------------------------------
## Quantas espécies?
todas_especies <- unique(parcelas$Especie)
length(todas_especies)

## Quais espécies? Tudo ok???
sort(todas_especies)

## Checando os nomes das espécies usando o pacote 'flora'
checagem_nomes <- flora::get.taxa(parcelas$Especie,
                                  suggestion.distance = 0.85)

## Reposição de sinônimos e nomes com problemas de grafia
ids_para_substituir <- !is.na(checagem_nomes$search.str) &
                        checagem_nomes$search.str != checagem_nomes$original.search 

### inspecionando as diferenças
unique(cbind(parcelas$Especie[ids_para_substituir], 
             checagem_nomes$search.str[ids_para_substituir]))

### reposição dos nomes
parcelas$EspecieCorrigida <- parcelas$Especie
parcelas$EspecieCorrigida[ids_para_substituir] <- 
  checagem_nomes$search.str[ids_para_substituir]

## Criando as colunas de familia
parcelas$Familia <- checagem_nomes$family

## Repondo outros nomes
ids_para_substituir <- checagem_nomes$notes %in% 
                        c("not found", "check no accepted name")

### inspecionando as diferenças
unique(cbind(parcelas$Especie[ids_para_substituir], 
             checagem_nomes$search.str[ids_para_substituir]))

### resolvendo caso a caso (olhar na Flora e Funga do Brasil Online se necessário)
ids_para_subs <- parcelas$Especie %in% "Maytenus robusta"
parcelas$EspecieCorrigida[ids_para_subs] <- "Monteverdia gonoclada"
parcelas$Familia[ids_para_subs] <- "Celastraceae"

ids_para_subs <- parcelas$Especie %in% "Eugenia sp"
parcelas$EspecieCorrigida[ids_para_subs] <- "Eugenia sp."
parcelas$Familia[ids_para_subs] <- "Myrtaceae"

ids_para_subs <- parcelas$Especie %in% "Myrcia sp2"
parcelas$EspecieCorrigida[ids_para_subs] <- "Myrcia sp.2"
parcelas$Familia[ids_para_subs] <- "Myrtaceae"

## Criando a coluna de gênero
parcelas$Genero <- sapply(strsplit(parcelas$EspecieCorrigida, " "), head, 1)

## Quantas espécies após as correções?
todas_especies_corr <- unique(parcelas$EspecieCorrigida)
length(todas_especies)
(S <- length(todas_especies_corr))

## Quantas familias e generos
length(unique(parcelas$Familia))
length(unique(parcelas$Genero))



# RESUMOS POR ÁRVORE ----------------------------------------------

## Diametros por fuste em centimetros (o número máximo de fustes nos dados é 7)
fuste_max <- 7
paps <- paste0("PAP", 1:fuste_max)
daps <- paste0("DAP", 1:fuste_max)
for(i in seq_along(daps)) {
  parcelas[[daps[i]]] <- (parcelas[[paps[i]]] / pi)
}
head(parcelas, 3)

## Calcule a área basal para cada fuste de cada árvore (em m2)
areas_basais <- paste0("AB", 1:fuste_max)
for(i in seq_along(daps)) {
  parcelas[[areas_basais[i]]] <- pi * (parcelas[[daps[i]]]/200) ^ 2
}
head(parcelas, 3)

## Calcule a área basal para cada árvore (em cm2 e m2)
parcelas$AB_total_m2 <- apply(parcelas[, areas_basais], 
                           MARGIN = 1, 
                           FUN = sum,
                           na.rm = TRUE)
head(parcelas, 3)

## Calcule o DAP máximo
parcelas$DAP_max <- apply(parcelas[, daps], 
                              MARGIN = 1, 
                              FUN = max,
                              na.rm = TRUE)


## Calcule o Volume total em m3 - equação de Schumacher-Hall (1933)
b0 <- -8.889; b1 <- 1.881; b2 <- 0.875 ; ff = 0.87 # doi: 10.1093/forsci/fxaa032, DBH > 10
parcelas$Volume_m3 <- ff * 
                      (exp( b0 + b1 * log(parcelas$DAP_max) +  
                       b2 * log(parcelas$AlturaTotal_m)))
head(parcelas, 3)

## Obtendo a densidade da madeira de uma base de dados global 
wsg <- BIOMASS::getWoodDensity(
                          genus = parcelas$Genero,
                          species = parcelas$EspecieCorrigida,
                          family = parcelas$Familia, 
                          region = "SouthAmericaTrop")
parcelas$DensidadeMadeira <- wsg$meanWD

## Calculando a biomassa acima do solo 
parcelas$AGB_mg <- 
  BIOMASS::computeAGB(D = parcelas$DAP_max,
                      WD = parcelas$DensidadeMadeira,
                      H = parcelas$AlturaTotal_m)
head(parcelas, 3)

# RESUMOS POR PARCELA ---------------------------------------------

## Adicionando um contador do número de indivíduos por linha (só para facilitar os cálculos)
parcelas$N <- 1L

## Resultados da floresta: densidade, área basal, volume e biomassa
resultados <- aggregate(cbind(N, AB_total_m2, Volume_m3, AGB_mg) ~ Parcela,
                        data = parcelas, FUN = sum, na.rm = TRUE)

## Resultados da floresta: altura total média
resultados$Altura_media_m <- aggregate(parcelas$AlturaTotal_m,
                                       list(parcelas$Parcela),
                        data = parcelas, FUN = mean, na.rm = TRUE)$x

## Resultados da floresta por hectare: densidade, área basal, volume e biomassa
## AVISO: a ordem das parcelas nos objetos 'resultados_por_ha' e
## 'descricao' tem que ser a mesma!!!
resultados_por_ha <- resultados[,2:5]/descricao$Area_ha
names(resultados_por_ha) <- c("Densidade_N_ha", "Dominancia_m2_ha", 
                              "Volume_m3_ha", "AGB_mg_ha")
resultados <- cbind.data.frame(resultados, resultados_por_ha)


## Riqueza e diversidade por parcela
### preparando os dados
contagem_por_parcela <- xtabs(N ~ Parcela + EspecieCorrigida, parcelas)

### riqueza de espécies
resultados$S <- vegan::specnumber(contagem_por_parcela)

### índice de Shannon
resultados$H <- vegan::diversity(contagem_por_parcela, index = "shannon")

### índice de Simpson
resultados$D <- vegan::diversity(contagem_por_parcela, index = "simpson")

### inverso do Índice de Simpson
resultados$invD <- vegan::diversity(contagem_por_parcela, index = "invsimpson")

### equabilidade de Pielou
resultados$J <- resultados$H/log(resultados$S)

### alpha de Fisher
resultados$alpha <- vegan::fisher.alpha(contagem_por_parcela)

### rarefação da riqueza para numeros menores de indivíduos por parcela (p.ex.: 5, 10 e 15)
### NOTA: valores de N_rar acima do mínimo das parcelas retornam os resultados com um aviso!
N_rar <- c(5, 10, 15)
S_rar <- as.data.frame(vegan::rarefy(contagem_por_parcela, N_rar))
names(S_rar) <- paste0("S_", names(S_rar))
resultados <- cbind.data.frame(resultados, S_rar)
head(resultados, 3)
 

# RESUMOS GERAIS ----------------------------------------------

## Calculando a densidade por hectare da floresta toda (individuos/ha)
(DA <- Ntotal/esforco)

## Calculando a area basal por hectare da floresta toda (m2/ha)
AB <- sum(parcelas$AB_total_m2)
(DoA <- AB/esforco)

## Calculando a biomassa acima do solo por hectare da floresta toda (Mg ou Ton/ha)
AGB <- sum(parcelas$AGB_mg)
(AGB_ha <- AGB/esforco)

## Calculando a biomassa acima do solo por hectare da floresta toda (Mg ou Ton/ha)
Vol <- sum(parcelas$Volume_m3)
(Vol_ha <- Vol/esforco)

## Índices de riqueza, diversidade e equabilidade

### preparando os dados
contagem <- xtabs(N ~ EspecieCorrigida, parcelas)

### índice de Shannon
(H <- vegan::diversity(contagem))

### índice de Simpson
D <- vegan::diversity(contagem, index = "simpson")

### inverso do Índice de Simpson
(invD <- vegan::diversity(contagem, index = "invsimpson"))

### equabilidade de Pielou
(J <- H/log(length(todas_especies_corr)))

### alpha de Fisher
(alpha <- vegan::fisher.alpha(contagem))

### rarefação da riqueza para numeros menores de indivíduos (p.ex.: 100, 250 e 500)
N_rar <- c(50, 100, 250, 500, 750)
(S_rarefeito <- vegan::rarefy(contagem, N_rar))

### visualizando a curva de rarefação
valores_N <- seq(0,875,5)
plot(vegan::rarefy(contagem, valores_N)[1,] ~ valores_N)
abline(h = S, lty = 2)


# SALVANDO OS RESULTADOS ------------------------------------------

## Adicionando os totais à tabela de resultados por parcela
totais <- data.frame("1-24", Ntotal, AB, Vol, AGB, NA, DA, DoA, Vol_ha,
                     AGB_ha, S, H, D, invD, J, alpha, NA, NA, NA)
names(totais) <- names(resultados)
resultados_finais <- rbind.data.frame(resultados, totais)

## Definindo o nome do arquivo e o caminho certo para salvar os dados
nome_arquivo <- "resultados.xlsx"
minha_pasta <- "dados/dados-processados"
meu_caminho <- file.path(minha_pasta, nome_arquivo)

## Verificando se a pasta existe. Se não, a pasta é criada.
if (!dir.exists(minha_pasta))
  dir.create(minha_pasta, recursive = TRUE)

## Salvando
writexl::write_xlsx(resultados_finais, meu_caminho, format_headers = FALSE)


# BONUS: COMPARANDO AMBIENTES -------------------------------------

## Obtendo as descricoes de cada parcela
resultados1 <- merge(resultados, descricao, by = "Parcela")

## Análise visual das diferenças
par(mfrow = c(2,2), mar = c(3.5,4,1,1), las = 1, mgp = c(2,0.6,0))
boxplot(N ~ Ambiente, data = resultados1)
boxplot(AGB_mg ~ Ambiente, data = resultados1)
boxplot(S ~ Ambiente, data = resultados1)
boxplot(H ~ Ambiente, data = resultados1)

## Definindo o nome do arquivo e o caminho certo para salvar a figura
nome_arquivo <- "boxplot.jpg"
minha_pasta <- "figuras/"
meu_caminho <- file.path(minha_pasta, nome_arquivo)

## Verificando se a pasta existe. Se não, a pasta é criada.
if (!dir.exists(minha_pasta))
  dir.create(minha_pasta)

## Salvando a figura
dev.print(jpeg, file = meu_caminho, 
          width = 1400, height = 1000, res = 200)

## Limpando a área de trabalho
rm(list = ls())
