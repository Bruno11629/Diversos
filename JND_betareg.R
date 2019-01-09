#Preparando o banco de dados
dados= read.csv("JND.csv",sep = ";",dec= ",")
names(dados)= c("Ancora","ID","Reverberancia","Prop")
dados= as.data.frame(dados)
dados$Reverberancia= as.factor(dados$Reverberancia)
dados$Ancora = as.factor(dados$Ancora)in
dados9= dados[which(dados$Ancora=="9"),]
dados1= dados[which(dados$Ancora=="1"),]
dados9$Prop[which(dados9$Prop==1.0)]=0.99999999
dados9$Prop[which(dados9$Prop==0.0)]=0.00000001
dados1$Prop[which(dados1$Prop==1.0)]=0.9999
dados1$Prop[which(dados1$Prop==0.0)]=0.0001

#Setup

library(betareg)

#betareg
#Ancora 9
fit9=betareg(Prop~Reverberancia,data= dados9)
Predicao9= as.data.frame(seq(from=1,to = 9,by=1))
names(Predicao9)= "Reverberancia"
Predicao9$Reverberancia = as.factor(Predicao9$Reverberancia)
Predicao9$Pred = predict(fit9,newdata= Predicao9)
plot(Predicao9)
summary(fit9)

#Ancora 1
fit1=betareg(Prop~Reverberancia,data= dados1)
Predicao1= as.data.frame(seq(from=1,to = 9,by=1))
names(Predicao1)= "Reverberancia"
Predicao1$Reverberancia = as.factor(Predicao1$Reverberancia)
Predicao1$Pred = predict(fit1,newdata= Predicao1)
plot(Predicao1)
summary(fit1)


db1 = dados9[which(dados9$Reverberancia==1),]
db2 = dados9[which(dados9$Reverberancia==2),]
db3 = dados9[which(dados9$Reverberancia==3),]
db4 = dados9[which(dados9$Reverberancia==4),]
db5 = dados9[which(dados9$Reverberancia==5),]
db6 = dados9[which(dados9$Reverberancia==6),]
db7 = dados9[which(dados9$Reverberancia==7),]
db8 = dados9[which(dados9$Reverberancia==8),]
db9 = dados9[which(dados9$Reverberancia==9),]

db1$Reverberancia = 9
db2$Reverberancia = 8
db3$Reverberancia = 7
db4$Reverberancia = 6
db5$Reverberancia = 5
db6$Reverberancia = 4
db7$Reverberancia = 3
db8$Reverberancia = 2
db9$Reverberancia = 1



#db1$Valor=155.95
#db2$Valor=142.86
#db3$Valor=113.51
#db4$Valor=108.6
#db5$Valor=57.6
#db6$Valor=49.36
#db7$Valor=45.21
#db8$Valor=16.94
#db9$Valor=12.24

db=rbind(db1,db2,db3,db4,db5,db6,db7,db8,db9)
db$Reverberancia= as.factor(db$Reverberancia)
fit=betareg(Prop~ Reverberancia,data= db)
Predicao= as.data.frame(seq(from=1,to = 9,by=1))
names(Predicao)= "Reverberancia"
Predicao$Reverberancia = as.factor(Predicao$Reverberancia)
Predicao$Pred = predict(fit,newdata= Predicao)
plot(Predicao, type = "l")
summary(fit)

#bootstrap Fadel
predGrupo = function(fit0){
  coefs = coef(fit0)[-10]
  alogit = function(x) 1/(1+exp(-x))
  coefs = c(coefs[1], coefs[1]+coefs[-1])
  names(coefs) = paste0("Reverberancia", 1:9)
  alogit(coefs)
}

ajusteCompleto = function(dat){
  fitAjuste = try(betareg(Prop~ Reverberancia,data= dat))
  if (is(fitAjuste, "try-error"))
    return(NULL)
  predGrupo(fitAjuste)
}

runBoot = function(nothing) {
  ids = sample(10:29, 20, rep=TRUE)
  tbl = table(ids)
  lst = vector('list', 20)
  for (i in 1:20){
    lst[[i]] = subset(db, ID %in% names(tbl))
    tbl = tbl-1
    tbl = tbl[tbl > 0]
    if (length(tbl) < 1)
      break
  }
  mydb = do.call(rbind, lst)
  ajusteCompleto(mydb)
}

mat = do.call(cbind, lapply(1:500, runBoot))

a = apply(mat, 1, quantile, c(0.025, 0.975))
