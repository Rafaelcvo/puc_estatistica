data("AirPassengers")
View(AirPassengers)

plot(AirPassengers)

boxplot(AirPassengers~cycle(AirPassengers))

plot(log(AirPassengers))

par(mfrow = c(1,2))
acf(log(AirPassengers))
pacf(log(AirPassengers))

plot(diff(log(AirPassengers)))
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))

#install.packages("tseries")
library(tseries)
adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)

ajuste <- arima(log(AirPassengers), c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
pred <- predict(ajuste, n.ahead = 10*12)

par(mfrow = c(1,2))
ts.plot(AirPassengers, exp(pred$pred), lty = c(1,3))  
ts.plot(AirPassengers, exp(pred$pred), log = "y", lty = c(1,3))

# Retornos Petrobras
# http://www.portalaction.com.br/series-temporais/11-estacionariedade

library(readxl)

retorno_ts <- ts(Retornos.Petro)

plot(retorno_ts)

par(mfrow = c(1,2))
acf(retorno_ts)
pacf(retorno_ts)

adf.test(retorno_ts, alternative="stationary", k=0) #se de maior que 5% ela é explosive
adf.test(retorno_ts, alternative="explosive", k=0) #se der maior que 5% ela é stationary


acf(diff(retorno_ts))
pacf(diff(retorno_ts))

plot(exp(retorno_ts))
plot(retorno_ts)

acf(log(retorno_ts))
pacf(log(retorno_ts))

ajuste <- arima(retorno_ts, order = c(0, 0, 1))
hist(ajuste$residuals)
tsdiag(ajuste)
Box.test(retorno_ts, type = "Ljung")
ajuste$aic

ajuste2 <- arima(retorno_ts, order = c(0, 1, 1))
ajuste2$aic
BIC(ajuste, ajuste2)

ajuste3 <- arima(retorno_ts, order = c(1, 0, 1))
ajuste3$aic
BIC(ajuste, ajuste2, ajuste3)

par(mfrow = c(1,1))
pred <- predict(ajuste, n.ahead = 25)
ts.plot(retorno_ts, pred$pred,lty = c(1,3))  
pred2 <- predict(ajuste2, n.ahead = 25)
ts.plot(retorno_ts, pred2$pred,lty = c(1,3))  
pred3 <- predict(ajuste3, n.ahead = 25)
ts.plot(retorno_ts, pred3$pred,lty = c(1,3))  


