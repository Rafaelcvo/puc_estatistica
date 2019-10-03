# Obtenha um modelo preditivo usando como variável resposta o fato de o bebê ter ou não baixo peso.
# lowbwt - Baixo peso ao nascer: 1-Low(baixo) / 0-Normal -Variavel dependente
# https://www.datacamp.com/community/tutorials/logistic-regression-R

# Importando as bibliotecas
library(readxl)
library(Amelia)
library(mlbench)
library(corrplot)
library(MASS)
library(pscl)

# Importando o dataset.
df <- read_excel("Atividade Avaliativa 2 - birthweight.xlsx")

# Verificando os dados com summary
summary(df)

# Plotando os dados
plot(df)

# Verificando a existencia de dados ausentes
missmap(df, col = c("red", "blue"))

# Criando uma matrix de dispersano
pairs(df, col = df$lowbwt)

# Calculando a correlação entre as variaveis
correlations <- cor(df[,1:14])
corrplot(correlations, method = "circle")

# Ajustando o modelo.
attach(df)
ajuste <- glm(lowbwt ~ headcirumference + length + Birthweight + Gestation + smoker +
              motherage + mnocig + mheight + mppwt + fage + fedyrs + fnocig + fheight, 
              family = binomial)

summary(ajuste)

# Selecao do melhor modelo.
stepAIC(ajuste, direction = "both")

# Ajustando um novo modelo
ajuste2 <- glm(formula = lowbwt ~ Birthweight, family = binomial)
summary(ajuste2)

# Calculando os odds
exp(cbind(coef(ajuste2), confint.default(ajuste2)))

# Teste chi-quadrado
pchisq(ajuste2$deviance, ajuste2$df.residual, lower.tail = F)
pchisq(ajuste2$null.deviance - ajuste2$deviance,
       ajuste2$df.null - ajuste2$df.residual, lower.tail = F)

# Estimando o coeficiente de determinação
pR2(ajuste2)
