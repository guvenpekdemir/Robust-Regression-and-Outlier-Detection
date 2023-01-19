
# Basit Doðrusal Regresyon


set.seed(01)

x <- rnorm(10)
x <- c(x, c(2,1,10,15))
y <- rnorm(10)
y <- c(y, c(4,3,5,6))

df <- data.frame(x,y)

plot(y~x, data=df)

#Y'nin qqplotu 
qqnorm(df$y, pch = 1, frame = FALSE)
qqline(df$y, col = "blue")

model <- lm(y ~ x) # Model Anlamlý.

summary(model)
fitted(model) 
coefficients(model)
abline(model)

# Artýklaýn Normallik testi

#H0: Artýklar normal daðýlmaktadýr.
#H1: Artýklar normal daðýlmamaktadýr.

shapiro.test(model$residuals) #H0 rededilemez. Artýklar normal daðýlmaktadýr.

#install.packages("lmtest")

#Varyans homojenliði testi

#H0: Artýklar arasýnda varyans homojenliði vardýr.
#H1: Artýklar arasýnda varyans homojenliði yoktur.

library(lmtest)
bptest(y~x) #H0 hipotezi reddedilemez. Artýklar arasýnda varyans homojenliði vardýr.


# Model Artýklarý
r <- model$residuals

r

par(mfrow=c(2,2))
plot(model)

#Artýk deðer analizi

#Standartlaþtýrýlmýþ Artýklar

standartres <- rstandard(model)

standartres

#Student Türü
library(MASS)

stud_resid <- studres(model)

stud_resid

#install.packages("qpcR")


#Press Artýklarý

library(qpcR)

prres <- PRESS(model, verbose = TRUE)

press_artýk <- prres$residuals

press_artýk

#R-Student Türü Artýk
rstudenttt_artýk <- rstudent(model)

rstudenttt_artýk


finaldf <- cbind(df[c('y', 'x')], standartres,stud_resid,press_artýk,rstudenttt_artýk)

# Mutlak deðerce ;
# Standartlaþtýrýlmýþ artýklar için d > 3  ise aykýrý olabilir.
# Student türü artýklar için r > 2 ise potansiyel aykýrý deðerdir.
# Press artýklarý için hii 1 e yaklaþtýkça press artýklarý büyür.
# R-Student türü artýklar için t > 2 ise gözlem potansiyel aykýrý deðer olabilir.

finaldf

# Artýklara Bakýldýðýnda 4,11,12. gözlemlerin artýk deðerlerininin fazla olduðu,
# tüm artýklar türünde olmasada diðerlerinde görülmektedir. 

# Bu gözlemler çýkartýlýp tekardan basit doðrusal regresyon analizi yapýlabilir.


artýklý_arýnmýs_df <- finaldf[-c(4,11,12),]
arýnmýs_df <- df[-c(4,11,12),]

# SON Model

son_model <- lm(y ~ x, data = arýnmýs_df)

summary(son_model)
coefficients(son_model)
fitted(son_model)

# Pi

for(i in 1:11){
  pi <- (i-1/2)/11
  print(pi)
}

Pý <- c(0.04545455, 0.1363636, 0.2272727, 0.3181818, 0.4090909, 0.5, 0.5909091, 0.6818182, 0.7727273,
        0.8636364,0.9545455)



tstandartres <- sort(artýklý_arýnmýs_df$standartres)
tstud_resid <- sort(artýklý_arýnmýs_df$stud_resid)
tpress <- sort(artýklý_arýnmýs_df$press_artýk)
trstudent <- sort(artýklý_arýnmýs_df$rstudenttt_artýk)


# ARTIKLARIN P-P Grafiði 

plot(tstandartres, Pý)
plot(tstud_resid, Pý)
plot(tpress, Pý)
plot(trstudent, Pý)


# ARTIKLARIN y þapka deðerine göre grafiði


summary(son_model)
coefficients(son_model)
yhat <- fitted(son_model)

plot(tstandartres, yhat)
plot(tstud_resid, yhat)
plot(tpress, yhat)
plot(trstudent, yhat)

# COOK D Distance

library(car)

cookd <-  cooks.distance(model =son_model) 


# DFFITS

dffits <- as.data.frame(dffits(son_model))

dffits

dffits_esik_deger <- 2 * sqrt(1/11)


#Sýralanmýþ dffits gözlemler 

dffits[order(-dffits['dffits(son_model)']), ]

#Ýlk 2 gözlemin 0,6'tan büyük bir DFFITS deðerine sahip olduðunu görebiliriz; bu, modelde oldukça etkili
#olup olmadýklarýný belirlemek için bu gözlemleri daha yakýndan araþtýrmak isteyebileceðimiz anlamýna gelir.



plot(dffits(son_model), type = 'h')
abline(h = dffits_esik_deger, lty = 2)
abline(h = -dffits_esik_deger, lty = 2)

# X ekseni, veri kümesindeki her gözlemin indeksini görüntüler ve y deðeri,
# her gözlem için karþýlýk gelen DFFITS deðerini görüntüler.


# DFBETAS


dfbetas <- as.data.frame(dfbetas(son_model))

dfbetas

n <- nrow(arýnmýs_df)

dfbetas_esik <- 2/sqrt(n)



#DFBETAS esik cizgisi ile gösterimi

par(mfrow=c(2,1))
plot(dfbetas$x, type='h')
abline(h = dfbetas_esik, lty = 2)
abline(h = -dfbetas_esik, lty = 2)

# Çizimden 2 gözlemin 0,60  eþiðinin mutlak deðerini aþtýðýný görebiliriz.

