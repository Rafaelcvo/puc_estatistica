# Import das bibiotecas
library(readxl)
library(tseries)
library(ggplot2)
require(forecast)
library(urca)
setwd("~/git/puc_estatistica")
?ts

# Importando os dados
bebidas <- read_excel("Atividade Avaliativa 4 - BEBIDA.xlsx")
bebida_ts <- ts(bebidas, start = c(1985,1), end = c(2000,7),frequency = 12)
start(bebida_ts)
end(bebida_ts)

# Plotando os graficos
plot(bebida_ts, type = "l", col = 2, xlab='Ano', main = "Produção mensal de bebidas")
boxplot(bebida_ts~cycle(bebida_ts))
plot(bebida_ts, main = "Padrao")
plot(log(bebida_ts), main = "Log", col = 2)
grid(col = 'darkgrey', lwd = 1)

# Gerando a autocorrelação
acf(bebida_ts) #funçao ACF - Autocorrelação
pacf(bebida_ts)
  
# Avnaçando a serie
#ts.plot(lag(bebida_ts, -35))
#abline(v = 2000, lty = 2)

p = diff(bebida_ts)
print(bebida_ts)
plot(p)
acf(p)
pacf(p)

adf.test(bebida_ts, alternative="stationary", k=0)
adf.test(bebida_ts, alternative="explosive", k=0)

acf(diff(bebida_ts))
pacf(diff(bebida_ts))

acf(log(bebida_ts))
pacf(log(bebida_ts))

ajuste <- arima(bebida_ts, order = c(0, 0, 1))
hist(ajuste$residuals)
tsdiag(ajuste)
Box.test(bebida_ts, type = "Ljung")
ajuste$aic

ajuste2 <- arima(bebida_ts, order = c(0, 1, 1))
ajuste2$aic
BIC(ajuste, ajuste2)

ajuste3 <- arima(bebida_ts, order = c(1, 0, 1))
ajuste3$aic
BIC(ajuste, ajuste2, ajuste3)

pred <- predict(ajuste,n.ahead = 24)
ts.plot(bebida_ts, pred$pred,lty = c(1,3))  
pred2 <- predict(ajuste2,n.ahead = 24)
ts.plot(bebida_ts, pred2$pred,lty = c(1,3))  
pred3 <- predict(ajuste3,n.ahead = 24)
ts.plot(bebida_ts, pred3$pred,lty = c(1,3))  

# Criando a regressao
b <- ordered(cycle(bebida_ts))
bebida_regr <- lm(bebida_ts~b)
summary(bebida_regr)

# Verificando se a serie é estacionaria
summary(lm(diff(bebida_ts)~lag(bebida_ts, -1)[-length(bebida_ts)] - 1))

# utiliando o pacote urca
summary(ur.df(bebida_ts, type = 'none', lags = 0))













