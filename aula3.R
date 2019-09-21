install.packages('psych')
install.packages('GPArotation')


library(psych)
library(GPArotation)
library(readxl)

# Analise factorial

EFA <- read_excel("EFA.xlsx")
View(efa)

table(is.na(efa))

parallel <- fa.parallel(EFA, fm = 'pa', fa = 'fa')

tresfatores <- fa(EFA, nfactors = 3, rotate = "oblimin", fm="pa")
tresfatores

print(tresfatores$loadings, cutoff = .3)

quatrofatores <- fa(EFA, nfactors = 4, rotate = "oblimin", fm="pa")
quatrofatores
print(quatrofatores$loadings, cutoff = .3)


quatrofatores <- fa(EFA, nfactors = 4, rotate = "oblimin", fm="pa")
quatrofatores

fa.diagram(quatrofatores)

# Analise Clusters

Proteina <- read_excel("Protein.xlsx")
View(Proteina)

#install.packages("cluster")
library(cluster)

hc <- hclust(dist(Proteina[,-1]), method = 'average')
plot(hc)

clusterCut <- cutree(hc, 3)
rect.hclust(hc, k=3, border="red")


clusplot(Proteina[,-1], clusterCut, 
         main='Represetação Gráfica 2D - Solução com 3 Clusters',
         color=TRUE, shade=TRUE, labels=2,
         lines=0)

Proteina$Grupos <- clusterCut
Grupo_ordenado <- Proteina[order(Proteina$Grupos),]
somente_grupo <- subset(Grupo_ordenado,select = c(Country, Grupos))
View(somente_grupo)
View(Grupo_ordenado)

# Analise discriminante
Candidatos <- read_excel("Candidatos.xlsx")
View(Candidatos)

ds_candidatos <- Candidatos[,-1]
View(ds_candidatos)
ds_candidatos$Grupo <- factor(ds_candidatos$Grupo, levels = c(1, 2, 3),
                              labels = c("Aprovado", "Espera", "Reprovado"))
View(ds_candidatos)

install.packages("klaR")
attach(ds_candidatos)
require(MASS)
library(klaR)
ajuste <- lda(Grupo ~ Nota_tecnica + Historico)
ajuste

plot(ajuste, col = as.integer(Grupo))

table(Grupo, Predito = predict(ajuste, Candidatos[,3:4])$class)
mean(Grupo == predict(ajuste, Candidatos[,3:4])$class)

partimat(Grupo ~ Nota_tecnica + Historico, method="lda")

plot(ajuste, dimen = 1, type = "b")

ajuste2 <- qda(Grupo ~ Nota_tecnica + Historico)
ajuste2
table(Grupo, Predito = predict(ajuste2, Candidatos[,3:4])$class)
mean(Grupo == predict(ajuste2, Candidatos[,3:4])$class)

partimat(Grupo ~ Nota_tecnica + Historico, method="qda")

















