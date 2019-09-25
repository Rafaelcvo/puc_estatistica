# Exercicio 1.
# Importando bibliotecas.
library(readxl)
library(psych)
library(GPArotation)

# Leitura do arquivo.
home <- read_excel("Atividade Avaliativa 3 - Home.xlsx")

# Removendo a coluna ID.
home <- home[2:8]

parallel <- fa.parallel(home, fm = 'pa', fa = 'fa')

# Usando tres variaveis.
tresfatores <- fa(home, nfactors = 3, rotate = "oblimin", fm = "pa")
tresfatores

print(tresfatores$loadings, cutoff = 0.3)

fa.diagram(tresfatores)

#------------------------------------------------------------------------#
# Exercicio 2
# Importando as bibliotecas.
library(cluster)

# Lendo o arquivo
crime <- read_excel("Atividade Avaliativa 3 - Crimes.xlsx")

hc <- hclust(dist(crime[,-1]), method = 'average')
plot(hc)

# Separando em tres grupos.
clusterCut <- cutree(hc, 3)
rect.hclust(hc, k=3, border="red")


# Gerando um grafico em 2D.
clusplot(crime[,-1], clusterCut, 
         main='Represetação Gráfica 2D - Solução com 3 Clusters',
         color=TRUE, shade=TRUE, labels=2,
         lines=0)

# Criando grupos.
crime$Grupos <- clusterCut
Grupo_ordenado <- crime[order(crime$Grupos),]
somente_grupo <- subset(Grupo_ordenado,select = c(STATE, Grupos))
View(somente_grupo)
View(Grupo_ordenado)

#---------------------------------------------------------------------#

# Exercicio 3
# Importando bibliotecas.
attach(lobos)
require(MASS)
library(klaR)

# Lendo o arquivo.
lobos <- read_excel("Atividade Avaliativa 3 - Lobos.xlsx")

ajuste <- lda(Grupo ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9)
ajuste

plot(ajuste, col = as.integer(Grupo))

table(Grupo, Predito = predict(ajuste, lobos[,1:9])$class)
mean(Grupo == predict(ajuste, lobos[,1:9])$class)

partimat(Grupo ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9, method="lda")

plot(ajuste, dimen = 1, type = "b")

ajuste2 <- qda(Grupo ~ Nota_tecnica + Historico)
ajuste2
table(Grupo, Predito = predict(ajuste2, Candidatos[,3:4])$class)
mean(Grupo == predict(ajuste2, Candidatos[,3:4])$class)

partimat(Grupo ~ Nota_tecnica + Historico, method="qda")














