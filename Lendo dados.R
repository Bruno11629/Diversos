#setup
library(RSQLite)
library(dygraphs)
library(bcp)
setwd("C:/Users/Bruno/Desktop/710810")

#le o banco
con <- dbConnect(RSQLite::SQLite(), "bd.sql") #conecta com banco de dados
DB20 <- dbGetQuery(con, "SELECT * FROM dados WHERE Chr = 20") #Extrai o cromossomo 
DB20$Position = as.integer(DB20$Position)
DB20<- DB20[order(DB20$Position),] #ordena

#separa o banco
i=seq(from = 4,to=209,by=2)
ratio= DB20[,i]

#converge para numerico
ratio= apply(ratio,2,as.numeric)
ratio= cbind(DB20$Position,ratio)
ratio= as.data.frame(ratio)
names(ratio)= c("Position",names(ratio[2:ncol(ratio)]))
ratio= as.data.frame(ratio)

#Visualisação 
par(mfrow=c(2,1))
plot(ratio[,1], ratio[,2],ylab="",xlab="Posição",main="Cromossomo completo")
plot(ratio[1:2000,1], ratio[1:2000,2],ylab="",xlab="Posição",main="2000 posições")

#visualisação como série temporal
tsratio= as.ts(ratio[,1])
dyRangeSelector(dygraph(tsratio))

#analise descritiva para série temporal
acf(tsratio)
pacf(tsratio)

#metodos não paramétricos
#visualização
y=ratio[,35]
plot(y, type='l')

dyRangeSelector(dygraph(as.ts(y)))

#lowess

fit = lowess(1:1000, y,f= 0.01)
lines(fit, col=2)
e= y- fit$y
plot(e, type = "l")

#spline
fit2=  smooth.spline(y,spar = .01)
lines(fit2, col="blue")
e= y- fit2$y
plot(e, type = "l")

#bcp
y=ratio[2000,37]
fit3= bcp(y, p0= 0.001)
plot(fit3)

y1=ratio[13000:17000,37]
plot(y1,type = "l")
lines(fit3$posterior.mean[13000:17000],col=2)



