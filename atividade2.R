# Obtenha um modelo preditivo usando como variável resposta o fato de o bebê ter ou não baixo peso.
# lowbwt - Baixo peso ao nascer: 1-Low(baixo) / 0-Normal -Variavel dependente
# https://www.datacamp.com/community/tutorials/logistic-regression-R

# Importando o dataset.
library(readxl)
df <- read_excel("Atividade Avaliativa 2 - birthweight.xlsx")

# Ajustando o modelo.
attach(df)
ajuste <- glm(lowbwt ~ headcirumference + length + Birthweight + Gestation + smoker +
              motherage + mnocig + mheight + mppwt + fage + fedyrs + fnocig + fheight, family = binomial)

summary(ajuste)

# Selecao do melhor model.
library(MASS)
stepAIC(ajuste, direction = "both")

library(ggplot2)
ggplot(data = df) +
  geom_point(mapping = aes(x = lowbwt, y = Birthweight))







