MRFG3 = read.csv2(file = "C:\\Users\\Eduardo\\Downloads\\MRFG3.csv")
stemp = ts(MRFG3, start = c(2010), end = c(2021), frequency = 12)
stemp
plot(stemp)
ultimomrfg3 = stemp[,2]
plot(ultimomrfg3)

#Transformacao Box-Cox

lambda <- BoxCox.lambda(ultimomrfg3)
mrfg3_bc <- BoxCox(ultimomrfg3, lambda = lambda)
ggtsdisplay(mrfg3_bc)

#Diferenciacao Raiz Unitaria

ndiffs(mrfg3_bc)
mrfg3_diff <- diff(mrfg3_bc,1)
ggtsdisplay(mrfg3_diff)

#Ajustando ARIMA

mrfg3_treino <- ts(ultimomrfg3[1:100], frequency = 12)
mrfg3_validacao <- ts(ultimomrfg3[101:133], frequency = 12)
fit <- Arima( y = mrfg3_treino, order = c(1,1,0), seasonal = c(1,1,0), lambda = TRUE)
summary(fit)
plot(mrfg3_treino)
lines(fit$fitted,col = 'blue')
accuracy(mrfg3_treino , fit$fitted)

#Realizando Previsao

predi <- forecast(fit, h = 33)
plot(predi)
plot(as.numeric(mrfg3_validacao), type = 'l')
lines(as.numeric(predi$mean), col = 'blue')
accuracy(as.numeric(mrfg3_validacao), as.numeric(predi$mean))

#Residuos

tsdiag(fit)
qqnorm(fit$residuals)