# Regressao logistica em R com glm.
# https://www.datacamp.com/community/tutorials/logistic-regression-R
# Nesta seção, você estudará um exemplo de regressão logística binária ISLR, 
# que abordará com o pacote, que fornecerá o conjunto de dados, e a glm()função, 
# que geralmente é usada para ajustar modelos lineares generalizados, 
# será usado para ajustar o modelo de regressão logística.

# Carregando os dados
#install.packages("ISLR")

require(ISLR)
View(Smarket)
names(Smarket)
head(Smarket)
summary(Smarket)

par(mfrow = c(1,8))
for (i in 1:8) {
  hist(Smarket[,i], main = names(Smarket)[i])
}

for (i in 1:8) {
  boxplot(Smarket[,1], main=names(Smarket)[i])
}

# Obtendo os dados ausentes.
#install.packages("Amelia")
#install.packages("mlbench")
library(Amelia)
library(mlbench)
missmap(Smarket, col = c("blue", "red"), legend = FALSE)

library(corrplot)

par(mfrow = c(1,1))
correlations <- cor(Smarket[,1:8])
corrplot(correlations, method = "circle")

pairs(Smarket, col = Smarket$Direction)

# Distribuição de densidade.
library(ggplot2)
library(caret)

x <- Smarket[,1:8]
y <- Smarket[,9]

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot = "density", scales=scales)

# Modelo de regressao logistica de construção
# O glm ira ajustar modelos lineares generalizados.
# Logistic Regression

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + 
                 Volume, data = Smarket, family = binomial)

# Informando sobre o ajuste
summary(glm.fit)

glm.probs <- predict(glm.fit, type = "response")
glm.probs[1:5]

# Prevendo se o mercado vai subir(up) ou descer(down) com limiar de 0,5
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")

# Anexando ao datasets
attach(Smarket)
table(glm.pred, Direction)

# Calculando a median
mean(glm.pred == Direction)

# Criando amostra de treino e teste
train = Year < 2005
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = Smarket,
               family = binomial,
               subset = train)
glm.probs <- predict(glm.fit, newdata = Smarket[!train,], type = "response")
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")

# Criando um nova variavel resposta
Direction.2005 <- Smarket$Direction[!train]
table(glm.pred, Direction.2005) 
# Este modelo acima se saiu pior que o anterios.

# Resolução do sobreajuste
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3, data = Smarket, 
              family = binomial, subset = train)
glm.probs = predict(glm.fit, newdata = Smarket[!train,], type = "response")
glm.pred = ifelse(glm.probs > 0.5, "Up", "Down")
table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005)

summary(glm.fit)

















