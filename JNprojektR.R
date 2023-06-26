
library(stringr)
library(dummies)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(class)
library(corrplot)
library(PerformanceAnalytics)
library(scatterplot3d)


setwd("D:/IBwP/pRojekt") # sciezka do folderu roboczego
inb_1 = read.csv("dane_inbody.csv",sep=",",dec=".",stringsAsFactors = FALSE)
ant_1 = read.csv("dane_antropo_13.csv",sep=",",dec=".",stringsAsFactors = FALSE)

# Czyszczenie dane_antropo_13.csv:
names(ant_1)
ant_1=ant_1[,-2]

ant_1[,1]=factor(ant_1[,1])
ant_1[ant_1==0]=NA
ant_1[ant_1==""]=NA

ant_1$Wysokosc[ant_1$Wysokosc==17.2]=172 # wys. 17.2 -> 172
ant_1$WysokoscSiedzeniowa[ant_1$WysokoscSiedzeniowa==175.5]=75.5 # wys. siedzeniowa 175.5 -> 75.5

# kolejno: obw. uda, szer. barkow, szer. bioder, fald na rameniu, fald pod lopatka, fald na brzuchu, cis. skurczowe, cis. rozkurczowe, tetno
kolout=c(10,12,13,16,17,18,19,20,21)
pairs(~.,data = ant_1[,kolout])

# usuwanie odstajacych (tetno krwi)
gran_g=3*quantile(ant_1$TetnoKrwi,0.99,na.rm = T)
gran_d=0.3*quantile(ant_1$TetnoKrwi,0.01,na.rm = T)

boxplot(ant_1$TetnoKrwi,range = 1.5)
z=0.05
zakres=qnorm(1-z/2)

b=boxplot(ant_1$TetnoKrwi,range = zakres)

for(i in kolout)
{
  odst=boxplot(ant_1[,i],range=zakres,plot=F)$out
  ant_1[is.element(ant_1[,i],odst),i]=NA
}
pairs(~.,data = ant_1[,kolout])

write.table(ant_1,"dane_antropo_clear.csv",sep=",",dec = ".",na="",row.names = F)

# Czyszczenie dane_inbody.csv:
inb_1=inb_1[,-c(2,3,122:222)]

while("inb_1" %in% search())
{
  detach(inb_1)
}
attach(inb_1)

wiek_pop=which(Age>21) # 34. wiersz
inb_1$id[wiek_pop] # id 142
inb_1$DateOfBirth[34]="16.11.1999"
inb_1$Age[34]=15.8


impedancja=grep("Impedance",names(inb_1),fixed = T) # wektor indeksow
inb_1=inb_1[,-impedancja]
kol_0=c(75:77) 
kol_0_clear=setdiff(c(1:ncol(inb_1)),kol_0)

inb_1[,kol_0_clear][inb_1[,kol_0_clear]<=0]=NA
inb_1[,4]=factor(inb_1[,4])
inb_1[,82]=factor(inb_1[,82])

write.table(inb_1,"dane_inbody_clear.csv",sep=",",dec=".",na="",row.names = F)

inb_C=read.csv("dane_inbody_clear.csv",sep=",",dec=".",stringsAsFactors = FALSE)
ant_C=read.csv("dane_antropo_clear.csv",sep=",",dec=".",stringsAsFactors = FALSE)

id=sort(union(ant_C$id,inb_C$id))

# liczenie wierszy i kolumn
lkolumn_ant_C=ncol(ant_C)-1
lkolumn_inb_C=ncol(inb_C)-1
lwierszy=length(id)
lkolumn=lkolumn_ant_C*9+lkolumn_inb_C*9+1
matr1=matrix(NA,nrow = lwierszy,ncol = lkolumn,dimnames = list(ID=id,
                                                Zmienne=c("ID",paste(rep(names(ant_C)[-1],each=9),rep(1:9,times=lkolumn_ant_C)),
                                                          paste(rep(names(inb_C)[-1],each=9),rep(1:9,times=lkolumn_inb_C)))))

matr1=as.data.frame(matr1)

# laczenie obu macierzy
for(j in 1:nrow(ant_C))
{
  wiersz = which(id==ant_C[j,1])
  data=ant_C[j,2]
  if(str_detect(data,"-09-")|str_detect(data,".09.")|str_detect(data,"-10-")|str_detect(data,".10."))
  {
    pomiar.kandydaci=c(1,3,5,7,9)
  } else
  {
    pomiar.kandydaci=c(2,4,6,8)
  }
  
  if(str_detect(data,"2018"))
  {
    pomiar=intersect(pomiar.kandydaci,c(6,7))
  }
  if(str_detect(data,"2019"))
  {
    pomiar=intersect(pomiar.kandydaci,c(8,9))
  }
  if(str_detect(data,"2016"))
  {
    pomiar=intersect(pomiar.kandydaci,c(2,3))
  }
  if(str_detect(data,"2017"))
  {
    pomiar=intersect(pomiar.kandydaci,c(4,5))
  }
  if(str_detect(data,"2015"))
  {
    pomiar=1
  }
  
  
  for (k in 1:lkolumn_ant_C) 
  {
    matr1[wiersz,(k-1)*9+pomiar+1]=ant_C[j,k+1]
  }
  
  
}

for(j in 1:nrow(inb_C))
{
  wiersz = which(id==inb_C[j,1])
  data=strsplit(inb_C[j,5],split = ". ")[[1]][1]
  if(str_detect(data,"-09-")|str_detect(data,".09.")|str_detect(data,"-10-")|str_detect(data,".10."))
  {
    pomiar.kandydaci=c(1,3,5,7,9)
  } else
  {
    pomiar.kandydaci=c(2,4,6,8)
  }
  if(str_detect(data,"2018"))
  {
    pomiar=intersect(pomiar.kandydaci,c(6,7))
  }
  if(str_detect(data,"2019"))
  {
    pomiar=intersect(pomiar.kandydaci,c(8,9))
  }
  if(str_detect(data,"2016"))
  {
    pomiar=intersect(pomiar.kandydaci,c(2,3))
  }
  if(str_detect(data,"2017"))
  {
    pomiar=intersect(pomiar.kandydaci,c(4,5))
  }
  if(str_detect(data,"2015"))
  {
    pomiar=1
  }
  
  
  for (k in 1:lkolumn_inb_C) 
  {
    matr1[wiersz,(k-1+lkolumn_ant_C)*9+pomiar+1]=inb_C[j,k+1]
  }
  
  
}

matr1[,1]=row.names(matr1)
Plec=DataUrodz=rep(NA,times=nrow(matr1))

for (wiersz in c(1:nrow(matr1))) 
{
  for(kol in 182:190)
  {
    if(!is.na(matr1[wiersz,kol]) & matr1[wiersz,kol]!="")
    {
      DataUrodz[wiersz]=matr1[wiersz,kol]
    }
  }
  
  for(kol in 200:208)
  {
    if(!is.na(matr1[wiersz,kol]) & matr1[wiersz,kol]!="")
    {
      Plec[wiersz]=matr1[wiersz,kol]    }
  }
}

ID=matr1$ID

clr_1=c(2:10,182:190,200:217)
matr1=matr1[,-clr_1]

matr1=cbind(ID,DataUrodz,Plec,matr1[,c(2:ncol(matr1))])
faktor_1=c(1,3,868:876)
for (i in faktor_1)
{
  matr1[,i]=factor(matr1[,i])
}

write.table(matr1,"dane_scalone_clear.csv",sep = ",",dec = ".",na="",row.names=F)

oba_1=read.csv("dane_scalone_clear.csv",sep=",",dec=".",stringsAsFactors = FALSE)

oba_stack=data.frame(ID=rep(oba_1$ID,each=9))
oba_stack=cbind(oba_stack,DataUrodz=rep(oba_1$DataUrodz,each=9))
oba_stack=cbind(oba_stack,Plec=rep(oba_1$Plec,each=9))

nazwy=names(oba_1)[seq(from=4,by=9,to=ncol(oba_1))]
for (i in 1:length(nazwy)) 
{
  nazwy[i]=strsplit(nazwy[i],split = .1)[[1]]  
}

matr2=matrix(NA,nrow = nrow(oba_stack),ncol=length(nazwy))
colnames(matr2)=nazwy
licznik=1

for (i in seq(from=4,by=9,to=ncol(oba_1))) 
{
  for (pom in 1:9) 
  {
    matr2[seq(from=pom,by=9,to=nrow(matr2)),licznik]=oba_1[,(i+pom-1)]
  }
  licznik=licznik+1
}

oba_stack=cbind(oba_stack,as.data.frame(matr2))

numery=c(4:99,101:123)
for(i in numery)
{
  oba_stack[,i]=as.numeric(oba_stack[,i])
}

write.table(oba_stack,"dane_scalone_stack.csv",sep = ",",dec = ".",na="",row.names=F)

# wyizolowanie niektorych kolumn do dalszej pracy
needed_columns_1=c(1, 24:29, 36:44)
needed_1=oba_stack[,needed_columns_1]

# mineraly<10
mn=11
mnrls=data.frame()
for(u in c(1:nrow(needed_1)))
{
  if(!is.na(needed_1[u, mn])) # kolumny niepuste
  {
    if(needed_1[u, mn] < 10)  # wyszukanie wartosci <10
    {
      mnrls=rbind(mnrls, needed_1[u, mn]) # przepisanie wartosci
    }
    else
    {
      mnrls=rbind(mnrls, NA)  # zamiana >10 na NA
    }
  }
  else
  {
    mnrls = rbind(mnrls, NA)
  }
}
needed_1=cbind(needed_1,mnrls)  # dodanie kolumny z przepisanymi wynikami
names(needed_1)[17]="Minerals2" # zmiana nazwy kolumny

# lower/upper range dla wagi
for(w in c(1:nrow(needed_1)))
{
  if(!is.na(needed_1[w, 3]) & !is.na(needed_1[w, 4]))
  {
    if(needed_1[w, 3] != "" & needed_1[w, 4] != "")
    {
      if(needed_1[w, 3] > needed_1[w,4])
      {
        wlu = needed_1[w, 3]
        needed_1[w, 3] = needed_1[w, 4]
        needed_1[w, 4] = wlu
      }
    }
  }
}

# lower/upper range dla wody
for(w in c(1:nrow(needed_1)))
{
  if(!is.na(needed_1[w, 6]) & !is.na(needed_1[w, 7]))
  {
    if(needed_1[w, 6] != "" & needed_1[w, 7] != "")
    {
      if(needed_1[w, 6] > needed_1[w, 7])
      {
        clu = needed_1[w, 6]
        needed_1[w, 6] = needed_1[w, 7]
        needed_1[w, 7] = clu
      }
    }
  }
}

# lower/upper range dla protein
for(w in c(1:nrow(needed_1)))
{
  if(!is.na(needed_1[w, 9]) & !is.na(needed_1[w, 10]))
  {
    if(needed_1[w, 9] != "" & needed_1[w, 10] != "")
    {
      if(needed_1[w, 9] > needed_1[w,10])
      {
        plu = needed_1[w, 9]
        needed_1[w, 9] = needed_1[w, 10]
        needed_1[w, 10] = plu
      }
    }
  }
}

# lower/upper range dla mineralow
for(w in c(1:nrow(needed_1)))
{
  if(!is.na(needed_1[w, 12]) & !is.na(needed_1[w, 13]))
  {
    if(needed_1[w, 12] != "" & needed_1[w, 13] != "")
    {
      if(needed_1[w, 12] > needed_1[w,13])
      {
        mlu = needed_1[w, 12]
        needed_1[w, 12] = needed_1[w, 13]
        needed_1[w, 13] = mlu
      }
    }
  }
}

# lower/upper range dla BFM
for(w in c(1:nrow(needed_1)))
{
  if(!is.na(needed_1[w, 15]) & !is.na(needed_1[w, 16]))
  {
    if(needed_1[w, 15] != "" & needed_1[w, 16] != "")
    {
      if(needed_1[w, 15] > needed_1[w,16])  # sprawdzenie, czy wartosc lower > upper
      {
        flu = needed_1[w, 15]               #
        needed_1[w, 15] = needed_1[w, 16]   # zamiana miejscami, aby wartosc wieksza byla
        needed_1[w, 16] = flu               # w kolumnie upper range
      }
    }
  }
}

# BFM=waga-(tbw+proteiny+mineraly)
nowe_BFM = data.frame()
wg=2
wd=5
pr=8
mnr=17
for(u in c(1:nrow(needed_1)))
{
  # sprawdzenie, czy potrzebne komorki sa niepuste
  if(!is.na(needed_1[u, wg]) & !is.na(needed_1[u, wd]) & !is.na(needed_1[u, pr]) & !is.na(needed_1[u, mnr]))
  {
    if(needed_1[u, wg] != "" & needed_1[u, wd] != "" & needed_1[u, pr] != "" & needed_1[u, mnr] != "")
    {
      # obliczenie nowego BFM ze wzoru, jesli wszystkie potrzebne skladniki wystepuja 
      nowe_BFM = rbind(nowe_BFM, needed_1[u, wg]-(needed_1[u, wd]+needed_1[u, pr]+needed_1[u, mnr]))
    }
    else
    {
      # wpisanie NA, jesli ktorys z potrzebnych skladnikow nie wystepuje
      nowe_BFM = rbind(nowe_BFM, NA)
    }
  }
  else
  {
    nowe_BFM = rbind(nowe_BFM, NA)
  }
}
needed_1=cbind(needed_1,nowe_BFM) # dopisanie kolumny z obliczonym BFM
names(needed_1)[18]="New_BFM"   # zmiana nazwy kolumny

# wykresy
colors=c("red", "blue")
puec=as.factor(oba_stack$Plec)

# waga-tbw
w3=plot(needed_1$TBW_TotalBodyWater_.1, needed_1$Weight.1, main = "Wykres: waga a TBW",
        xlab = "TBW", ylab = "Waga",
        pch = 20, col = colors[puec])
abline(lm(needed_1$Weight.1 ~ needed_1$TBW_TotalBodyWater_.1, data = needed_1), col = "black")
legend("topleft", legend = levels(puec),
       col =  colors, pch = 15)

# waga-proteiny
w4=plot(needed_1$Protein.1, needed_1$Weight.1, main = "Wykres: waga a proteiny",
        xlab = "Proteiny", ylab = "Waga",
        pch = 20, col = colors[puec])
abline(lm(needed_1$Weight.1 ~ needed_1$Protein.1, data = needed_1), col = "black")
legend("topleft", legend = levels(puec),
       col =  colors, pch = 15)

# waga-mineraly
w5=plot(needed_1$Minerals2, needed_1$Weight.1, main = "Wykres: waga a mineraly",
        xlab = "Mineraly", ylab = "Waga",
        pch = 20, col = colors[puec])
abline(lm(needed_1$Weight.1 ~ needed_1$Minerals2, data = needed_1), col = "black")
legend("bottomright", legend = levels(puec),
       col =  colors, pch = 15)

# waga-bfm
w6=plot(needed_1$New_BFM, needed_1$Weight.1, main = "Wykres: waga a BFM",
        xlab = "BFM", ylab = "Waga",
        pch = 20, col = colors[puec])
abline(lm(needed_1$Weight.1 ~ needed_1$New_BFM, data = needed_1), col = "black")
legend("topleft", legend = levels(puec),
       col =  colors, pch = 15)

# zaleznosc od plci
w2 = scatterplot3d(oba_stack$FaldNaBrzuchu.1, oba_stack$FaldNaRamieniu.1, oba_stack$FaldPodLopatka.1, pch = 20, color = colors[puec],
                   grid = TRUE, box = FALSE, xlab = "Fald na brzuchu", 
                   ylab = "Fald na ramieniu", zlab = "Fald pod lopatka", main="Wykres zaleznosci od plci")
legend("bottomright", legend = levels(puec),
       col =  colors, pch = 15)

# faldy a waga i BFM
wyk_kor_1=c(17:19)
wyk_kor_2=c(2, 18)
w1_p=cor(x=oba_stack[,wyk_kor_1], y=needed_1[,wyk_kor_2], use="pairwise.complete.obs",method = "pearson")
corrplot(w1_p, method = "ellipse")
corrplot(w1_p, method = "number")
w1_s=cor(x=oba_stack[,wyk_kor_1], y=needed_1[,wyk_kor_2], use="pairwise.complete.obs",method = "spearman")
corrplot(w1_s, method = "ellipse")
corrplot(w1_s, method = "number")

# 
dclas_kol=c(3:4, 17:19, 23:24)
dclas=oba_stack[,dclas_kol]
dclas[,1] = factor(dclas[,1])
dclas=dummy.data.frame(dclas)
dclas=dclas[,-1]
names(dclas)[1]="Plec"

dreg=cbind(dclas,nowe_BFM) # +BFM
names(dreg)[8]="BFM"

sr_BFM=rep(NA,nrow(needed_1)) 
sr_BFM[(needed_1[,18] < needed_1[,15]) | (needed_1[,18] > needed_1[,16])]=0
sr_BFM[(needed_1[,18] > needed_1[,15]) & (needed_1[,18] < needed_1[,16])]=1
sr_BFM=factor(sr_BFM)

dclas=cbind(dclas,sr_BFM)

write.table(dclas,"dane_dclas.csv",sep = ";",dec = ",",na="",row.names=F)
write.table(dreg,"dane_dreg.csv",sep = ";",dec = ",",na="",row.names=F)


ocena<<-function(macierz_bledow, nazwa_klasyfikatora = "brak nazwy")
{
  #      macierz    |TP FP|
  #                 |FN TN|
  tp = macierz_bledow[1,1]
  fp = macierz_bledow[1,2]
  fn = macierz_bledow[2,1]
  tn = macierz_bledow[2,2]

  acc = (tp + tn)/(tp + fp + tn + fn)
  sen = tp/(tp + fn)
  spe = tn/(tn + fp)
  pre = tp/(tp + fp)
  f1 = 2*pre*sen/(pre + sen)
  jakosc = c(acc, sen, spe, pre, f1)
  nazwy = c("dokladnosc", "czulosc", "specyficznosc", "precyzja", "f1")

  while(names(dev.cur()) != "null device") dev.off()
  png(paste(nazwa_klasyfikatora, ".png", sep = ""), width = 1000, height = 800)
  barplot(jakosc, main = nazwa_klasyfikatora, names = nazwy, ylim = c(0,1))
  dev.off()
  jakosc.ramka = data.frame(acc, sen, spe, pre, f1)
  return(jakosc.ramka)
}

df_on=subset(dclas,!is.na(dclas$sr_BFM))

complete.cases(df_on)
df_on=df_on[complete.cases(df_on),]

split=sample.split(df_on$sr_BFM,SplitRatio = 0.8)
train_set=subset(df_on,split==TRUE)
test_set=subset(df_on,split==FALSE)

train_X=train_set[,-ncol(df_on)]
test_X=test_set[,-ncol(df_on)]
train_Y=train_set[,ncol(df_on)]
test_Y=test_set[,ncol(df_on)]

p=5
train_X_s=scale(train_X)
test_X_s=scale(test_X)

# decission tree
dec_tree=rpart(formula=sr_BFM~.,data = train_set,method = "class",control = rpart.control(maxdepth = 5))
rpart.plot(dec_tree,box.palette = "RdBu",digit=-2)
sdt_pred=predict(dec_tree,test_set,type="class")
wynik_1=table(test_set$sr_BFM,sdt_pred)
wynik_sdt=ocena(wynik_1,"Simple decission tree (depth 5)")
wynik_sdt

# random forrest
ran_forr=randomForest(formula=sr_BFM~.,data=train_set, method="class", ntree=100)
rf_pred=predict(ran_forr,test_set)
wynik_2=table(test_set$sr_BFM,rf_pred)
wynik_rf=ocena(wynik_2,"Random forrest")
wynik_rf

# KNN
knn_pred=knn(train_X_s,test_X_s,train_Y,k=p)
wynik_3=table(knn_pred,test_Y)
wynik_knn=ocena(wynik_3,"KNN")
wynik_knn

