library(readxl)

bebidas <- read_excel("Atividade Avaliativa 4 - BEBIDA.xlsx")
bebida_ts <- ts(bebidas, )

acf(bebida_ts)
pacf(bebida_ts)

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

pred <- predict(ajuste,n.ahead = 180)
ts.plot(bebida_ts, pred$pred,lty = c(1,3))  
pred2 <- predict(ajuste2,n.ahead = 180)
ts.plot(bebida_ts, pred2$pred,lty = c(1,3))  
pred3 <- predict(ajuste3,n.ahead = 180)
ts.plot(bebida_ts, pred3$pred,lty = c(1,3))  
