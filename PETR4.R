petr4 = read.csv2(file = "C:\\Users\\Eduardo\\Documents\\PETR4.csv")
plot.ts(petr4$Ãsltimo)
stemp = ts(petr4, start = c(2011), end = c(2021), frequency = 12)
stemp
plot(stemp)
Ãsltimopetr4 = stemp[,2]
plot(Ãsltimopetr4)

#Transformacao Box-Cox

lambda <- BoxCox.lambda(Ãsltimopetr4)
petr4_bc <- BoxCox(Ãsltimopetr4, lambda = lambda)
ggtsdisplay(petr4_bc)

#Diferenciacao Raiz Unitaria

ndiffs(petr4_bc)
petr4_diff <- diff(petr4_bc,1)
ggtsdisplay(petr4_diff)

#Ajustando ARIMA

petr4_treino <- ts(Ãsltimopetr4[1:100],frequency = 12)
petr4_validacao <-ts(Ãsltimopetr4[101:121], frequency = 12)
fit <- Arima( y = petr4_treino, order = c(1,1,0), seasonal = c(1,1,0),lambda = TRUE)
summary(fit)
coeftest(fit)
plot(petr4_treino)
lines(fit$fitted,col = 'blue')
accuracy(petr4_treino , fit$fitted)

#Realizando Previsao

predi <- forecast(fit, h = 21)
plot(predi)
plot(as.numeric(petr4_validacao), type = 'l')
lines(as.numeric(predi$mean), col = 'blue')
accuracy(as.numeric(petr4_validacao) , as.numeric(predi$mean))

#Residuos
tsdiag(fit)
qqnorm(fit$residuals)