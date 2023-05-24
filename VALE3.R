vale3 = read.csv2(file = "C:\\Users\\Eduardo\\Documents\\vale3.csv")
plot.ts(vale3$Último)
stemp = ts(vale3, start = c(2011), end = c(2021), frequency = 12)
stemp
plot(stemp)
Últimovale3 = stemp[,2]
plot(Últimovale3)

#Transformação Box-Cox

lambda <- BoxCox.lambda(Últimovale3)
vale3_bc <- BoxCox(Últimovale3,lambda = lambda)
ggtsdisplay(vale3_bc)

#Diferenciacao Raiz Unitaria

ndiffs(vale3_bc)
vale3_diff <- diff(vale3_bc,1)
ggtsdisplay(vale3_diff)

#Ajustando ARIMA

vale3_treino <- ts( Últimovale3[1:100],frequency = 12)
vale3_validacao <- ts(Últimovale3[101:130], frequency = 12)
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