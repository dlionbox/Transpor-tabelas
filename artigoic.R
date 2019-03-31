#install.packages("reshape2")
library(readr)
library(tidyr)
library(stringr)

path<-"../dadosartigo"

#Mortes:
mortesfem00<-read_delim("death_20_00_females.txt",delim = "     ")
mortesfem10<-read_delim("death_20_10_females.txt",delim = "     ")
mortesmasc00<-read_delim("death_20_00_males.txt",delim = "     ")
mortesmasc10<-read_delim("death_20_10_males.txt",delim = "     ")

#População:
popfem00<-read_delim("pop_20_00_females.txt",delim = "     ")
popfem10<-read_delim("pop_20_10_females.txt",delim = "     ")
popmasc00<-read_delim("pop_20_00_males.txt",delim = "     ")
popmasc10<-read_delim("pop_20_10_males.txt",delim = "     ")

#Para ler todos os csvs do diretório em uma única lista:
fi<-list.files(path,pattern = "*.csv")
dat<-lapply(fi,read_csv2)

#Separar a lista em vários data.frames:
dat<-lapply(dat, function(x) x[!(names(x) %in% c("0 a 6 dias","7 a 27 dias","28 a 364 dias","Menor 1 ano (ign)","Idade ignorada","Total"))])
dat<-lapply(dat, function(x) {x$Municipio<-substring(x$Municipio,first = 1,last = 6)
                              return(x)})

nomes<-c("homens1999","homens2001","homens2009","homens2011","mulheres1999","mulheres2001","mulheres2009","mulheres2011")
for(i in 1:length(nomes)){
  assign(nomes[i],dat[[i]])
}

#rm(dat,fi,nomes,i,path)

#Transpondo os bancos:

#Homens 1999:
b<-data.frame(t(homens1999))
b$names <- rownames(b)

c<-b%>%gather(key = "codigo",value = "mortes",1:5488)
  
#Homens 2001:
b2<-data.frame(t(homens2001))
b2$names <- rownames(b2)

c2<-b2%>%gather(key = "codigo",value = "mortes",1:5576)

#Homens 2009:
b3<-data.frame(t(homens2009))
b3$names <- rownames(b3)

c3<-b3%>%gather(key = "codigo",value = "mortes",1:5586)

#Homens 2011:
b4<-data.frame(t(homens2011))
b4$names <- rownames(b4)

c4<-b4%>%gather(key = "codigo",value = "mortes",1:5586)

#Mulheres 1999:
m<-data.frame(t(mulheres1999))
m$names <- rownames(m)

w<-m%>%gather(key = "codigo",value = "mortes",1:5447)

#Mulheres 2001:
m2<-data.frame(t(mulheres2001))
m2$names <- rownames(m2)

w2<-m2%>%gather(key = "codigo",value = "mortes",1:5549)

#mulheres 2009:
m3<-data.frame(t(mulheres2009))
m3$names <- rownames(m3)

w3<-m3%>%gather(key = "codigo",value = "mortes",1:5574)

#mulheres 2011:
m4<-data.frame(t(mulheres2011))
m4$names <- rownames(m4)

w4<-m4%>%gather(key = "codigo",value = "mortes",1:5581)

#rm(homens1999,homens2001,homens2009,homens2011,mulheres1999,mulheres2001,mulheres2009,mulheres2011,a,dat)
#rm(m,m2,m3,m4,b,b2,b3,b4)

#Arrumar os códigos de municípios:
#Homens:
aux<-c%>%filter(names=="Municipio")%>%select(-names)
c<-left_join(c,aux,by=c("codigo"="codigo"))
c<-c%>%filter(names!="Municipio")%>%select(-codigo)

aux2<-c2%>%filter(names=="Municipio")%>%select(-names)
c2<-left_join(c2,aux2,by=c("codigo"="codigo"))
c2<-c2%>%filter(names!="Municipio")%>%select(-codigo)

aux3<-c3%>%filter(names=="Municipio")%>%select(-names)
c3<-left_join(c3,aux3,by=c("codigo"="codigo"))
c3<-c3%>%filter(names!="Municipio")%>%select(-codigo)

aux4<-c4%>%filter(names=="Municipio")%>%select(-names)
c4<-left_join(c4,aux4,by=c("codigo"="codigo"))
c4<-c4%>%filter(names!="Municipio")%>%select(-codigo)

#Mulheres:
auxm<-w%>%filter(names=="Municipio")%>%select(-names)
w<-left_join(w,auxm,by=c("codigo"="codigo"))
w<-w%>%filter(names!="Municipio")%>%select(-codigo)

auxm2<-w2%>%filter(names=="Municipio")%>%select(-names)
w2<-left_join(w2,auxm2,by=c("codigo"="codigo"))
w2<-w2%>%filter(names!="Municipio")%>%select(-codigo)

auxm3<-w3%>%filter(names=="Municipio")%>%select(-names)
w3<-left_join(w3,auxm3,by=c("codigo"="codigo"))
w3<-w3%>%filter(names!="Municipio")%>%select(-codigo)

auxm4<-w4%>%filter(names=="Municipio")%>%select(-names)
w4<-left_join(w4,auxm4,by=c("codigo"="codigo"))
w4<-w4%>%filter(names!="Municipio")%>%select(-codigo)

#rm(aux,aux2,aux3,aux4,auxm,auxm2,auxm3,auxm4)

#Arrumar os códigos de idades:
#Homens:
a<-c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)
b<-rep(a,times=nrow(c)/length(a))
c<-cbind(c,b)

b<-rep(a,times=nrow(c2)/length(a))
c2<-cbind(c2,b)

b<-rep(a,times=nrow(c3)/length(a))
c3<-cbind(c3,b)

b<-rep(a,times=nrow(c4)/length(a))
c4<-cbind(c4,b)

#Mulheres:
b<-rep(a,times=nrow(w)/length(a))
w<-cbind(w,b)

b<-rep(a,times=nrow(w2)/length(a))
w2<-cbind(w2,b)

b<-rep(a,times=nrow(w3)/length(a))
w3<-cbind(w3,b)

b<-rep(a,times=nrow(w4)/length(a))
w4<-cbind(w4,b)

#rm(a,b)

#Colocando em ordem as variáveis e arrumando os nomes:
#Homens:
c<-c%>%select(Muni=mortes.y,Age=b,Death=mortes.x)
c2<-c2%>%select(Muni=mortes.y,Age=b,Death=mortes.x)
c3<-c3%>%select(Muni=mortes.y,Age=b,Death=mortes.x)
c4<-c4%>%select(Muni=mortes.y,Age=b,Death=mortes.x)

#Mulheres:
w<-w%>%select(Muni=mortes.y,Age=b,Death=mortes.x)
w2<-w2%>%select(Muni=mortes.y,Age=b,Death=mortes.x)
w3<-w3%>%select(Muni=mortes.y,Age=b,Death=mortes.x)
w4<-w4%>%select(Muni=mortes.y,Age=b,Death=mortes.x)

#Exportando as bases:
write.csv(c,"death_19_99_males.csv")
write.csv(c2,"death_20_01_males.csv")
write.csv(c3,"death_20_09_males.csv")
write.csv(c4,"death_20_11_males.csv")

write.csv(w,"death_19_99_females.csv")
write.csv(w2,"death_20_01_females.csv")
write.csv(w3,"death_20_09_females.csv")
write.csv(w4,"death_20_11_females.csv")

