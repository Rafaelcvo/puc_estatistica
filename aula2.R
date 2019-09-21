library(readxl)
Experiencia_Tarefa <- read_excel("Experiencia_Tarefa.xlsx")
View(Experiencia_Tarefa)

attach(Experiencia_Tarefa) 
ajuste <- glm(Tarefa_concluidda ~ Experiencia, family = binomial) 
summary(ajuste)

anova(ajuste, test = 'Chisq')
require(MASS)
exp(cbind(coef(ajuste), confint.default(ajuste)))

plot(Experiencia_Tarefa$Experiencia, Experiencia_Tarefa$Tarefa_concluidda)
y_chapeu <- data.frame(
  X = Experiencia_Tarefa$Experiencia,
  y_chapeu = ajuste$fitted.values)
y_chapeu <- y_chapeu[order(y_chapeu$X),]
lines(y_chapeu, col = 2)
y_chapeu
par(mfrow=c(2,2))
plot(ajuste)

y_chapeu <- data.frame(X = Experiencia_Tarefa$Experiencia, y_chapeu =
                         ajuste$fitted.values)
y_chapeu$conclusão <- ifelse(y_chapeu$y_chapeu > .5, 1, 0)
y_chapeu$y <- Tarefa_concluidda
table(y_chapeu[,3:4])
mean(y_chapeu$conclusão == y_chapeu$y)

# Regressao logistica composta
library(readxl)
Lasagna_Triers <- read_excel("Lasagna Triers.xlsx")
View(Lasagna_Triers)

attach(Lasagna_Triers)
Lasagna_Triers$Compra <- ifelse(Lasagna_Triers$`Have Tried`=="Yes", 1, 0)
ajuste <- glm(Compra ~ Age + Weight + Income + `Pay Type` +
                `Car Value` + `CC Debt` + Gender + `Live Alone` +
                `Dwell Type` + `Mall Trips` + Nbhd, family = binomial)

summary(ajuste)

library(MASS)
stepAIC(ajuste, direction = "both")

ajuste2 <- glm(Compra ~ Age +
                 `Pay Type` +
                 Gender +
                 `Live Alone` +
                 `Mall Trips` +
                 Nbhd, family = binomial)
summary(ajuste2)

ajuste3 <- glm(Compra ~ Age +
                 `Pay Type` +
                 `Live Alone` +
                 `Mall Trips` +
                 Nbhd, family = binomial)
summary(ajuste3)

require(MASS)
exp(cbind(coef(ajuste2), confint.default(ajuste2)))

#Teste Chi-quadrado do Deviance
pchisq(ajuste2$deviance, ajuste2$df.residual, lower.tail = F) 

#Teste Chi-quadrado da Regressão
pchisq(ajuste2$null.deviance - ajuste2$deviance,
       ajuste2$df.null - ajuste$df.residual, lower.tail = F) 

library(carData)
vif(ajuste2)

install.packages('pscl')
library(pscl)
pR2(ajuste2)

par(mfrow=c(2,2))
plot(ajuste2)

coef2 <- data.frame(coef(ajuste2))
p_hat <- (exp(coef2[1,] + coef2[2,]*20 + coef2[3,] + coef2[4,] +
                coef2[5,] + coef2[6,]*10 + coef2[8,]))/
  (1+exp(coef2[1,] + coef2[2,]*20 + coef2[3,] + coef2[4,] +
           coef2[5,] + coef2[6,]*10 + coef2[8,]))
p_hat

coef2 <- data.frame(coef(ajuste2))
p_hat <- (exp(coef2[1,] + coef2[2,]*20 + coef2[3,] + 
                coef2[5,] + coef2[6,]*10 + coef2[8,]))/
  (1+exp(coef2[1,] + coef2[2,]*20 + coef2[3,] + 
           coef2[5,] + coef2[6,]*10 + coef2[8,]))
p_hat













