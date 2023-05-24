vale3 = read.csv2(file = "C:\\Users\\Eduardo\\Documents\\vale3.csv")
plot.ts(vale3$�ltimo)
stemp = ts(vale3, start = c(2011), end = c(2021), frequency = 12)
stemp
plot(stemp)
�ltimovale3 = stemp[,2]
plot(�ltimovale3)

#Transforma��o Box-Cox

lambda <- BoxCox.lambda(�ltimovale3)
vale3_bc <- BoxCox(�ltimovale3,lambda = lambda)
ggtsdisplay(vale3_bc)

#Diferenciacao Raiz Unitaria

ndiffs(vale3_bc)
vale3_diff <- diff(vale3_bc,1)
ggtsdisplay(vale3_diff)

#Ajustando ARIMA

vale3_treino <- ts( �ltimovale3[1:100],frequency = 12)
vale3_validacao <- ts(�ltimovale3[101:130], frequency = 12)
fit <- Arima( y = vale3_treino, order = c(1,1,0), seasonal = c(1,1,0), lambda = TRUE)
summary(fit)
coeftest(fit)

plot(vale3_treino)
lines(fit$fitted,col = 'blue')
accuracy(vale3_treino , fit$fitted)

#Realizando Previsao

predi <- forecast(fit, h = 30)
plot(predi)
plot(as.numeric(vale3_validacao), type = 'l')
lines (as.numeric(predi$mean), col = 'blue')
accuracy(as.numeric(vale3_validacao) , as.numeric(predi$mean))

#Residuos

tsdiag(fit)
qqnorm(fit$residuals)