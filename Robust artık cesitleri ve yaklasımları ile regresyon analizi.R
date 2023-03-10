
# Basit Do?rusal Regresyon


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

model <- lm(y ~ x) # Model Anlaml?.

summary(model)
fitted(model) 
coefficients(model)
abline(model)

# Art?kla?n Normallik testi

#H0: Art?klar normal da??lmaktad?r.
#H1: Art?klar normal da??lmamaktad?r.

shapiro.test(model$residuals) #H0 rededilemez. Art?klar normal da??lmaktad?r.

#install.packages("lmtest")

#Varyans homojenli?i testi

#H0: Art?klar aras?nda varyans homojenli?i vard?r.
#H1: Art?klar aras?nda varyans homojenli?i yoktur.

library(lmtest)
bptest(y~x) #H0 hipotezi reddedilemez. Art?klar aras?nda varyans homojenli?i vard?r.


# Model Art?klar?
r <- model$residuals

r

par(mfrow=c(2,2))
plot(model)

#Art?k de?er analizi

#Standartla?t?r?lm?? Art?klar

standartres <- rstandard(model)

standartres

#Student T?r?
library(MASS)

stud_resid <- studres(model)

stud_resid

#install.packages("qpcR")


#Press Art?klar?

library(qpcR)

prres <- PRESS(model, verbose = TRUE)

press_art?k <- prres$residuals

press_art?k

#R-Student T?r? Art?k
rstudenttt_art?k <- rstudent(model)

rstudenttt_art?k


finaldf <- cbind(df[c('y', 'x')], standartres,stud_resid,press_art?k,rstudenttt_art?k)

# Mutlak de?erce ;
# Standartla?t?r?lm?? art?klar i?in d > 3  ise ayk?r? olabilir.
# Student t?r? art?klar i?in r > 2 ise potansiyel ayk?r? de?erdir.
# Press art?klar? i?in hii 1 e yakla?t?k?a press art?klar? b?y?r.
# R-Student t?r? art?klar i?in t > 2 ise g?zlem potansiyel ayk?r? de?er olabilir.

finaldf

# Art?klara Bak?ld???nda 4,11,12. g?zlemlerin art?k de?erlerininin fazla oldu?u,
# t?m art?klar t?r?nde olmasada di?erlerinde g?r?lmektedir. 

# Bu g?zlemler ??kart?l?p tekardan basit do?rusal regresyon analizi yap?labilir.


art?kl?_ar?nm?s_df <- finaldf[-c(4,11,12),]
ar?nm?s_df <- df[-c(4,11,12),]

# SON Model

son_model <- lm(y ~ x, data = ar?nm?s_df)

summary(son_model)
coefficients(son_model)
fitted(son_model)

# Pi

for(i in 1:11){
  pi <- (i-1/2)/11
  print(pi)
}

P? <- c(0.04545455, 0.1363636, 0.2272727, 0.3181818, 0.4090909, 0.5, 0.5909091, 0.6818182, 0.7727273,
        0.8636364,0.9545455)



tstandartres <- sort(art?kl?_ar?nm?s_df$standartres)
tstud_resid <- sort(art?kl?_ar?nm?s_df$stud_resid)
tpress <- sort(art?kl?_ar?nm?s_df$press_art?k)
trstudent <- sort(art?kl?_ar?nm?s_df$rstudenttt_art?k)


# ARTIKLARIN P-P Grafi?i 

plot(tstandartres, P?)
plot(tstud_resid, P?)
plot(tpress, P?)
plot(trstudent, P?)


# ARTIKLARIN y ?apka de?erine g?re grafi?i


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


#S?ralanm?? dffits g?zlemler 

dffits[order(-dffits['dffits(son_model)']), ]

#?lk 2 g?zlemin 0,6'tan b?y?k bir DFFITS de?erine sahip oldu?unu g?rebiliriz; bu, modelde olduk?a etkili
#olup olmad?klar?n? belirlemek i?in bu g?zlemleri daha yak?ndan ara?t?rmak isteyebilece?imiz anlam?na gelir.



plot(dffits(son_model), type = 'h')
abline(h = dffits_esik_deger, lty = 2)
abline(h = -dffits_esik_deger, lty = 2)

# X ekseni, veri k?mesindeki her g?zlemin indeksini g?r?nt?ler ve y de?eri,
# her g?zlem i?in kar??l?k gelen DFFITS de?erini g?r?nt?ler.


# DFBETAS


dfbetas <- as.data.frame(dfbetas(son_model))

dfbetas

n <- nrow(ar?nm?s_df)

dfbetas_esik <- 2/sqrt(n)



#DFBETAS esik cizgisi ile g?sterimi

par(mfrow=c(2,1))
plot(dfbetas$x, type='h')
abline(h = dfbetas_esik, lty = 2)
abline(h = -dfbetas_esik, lty = 2)

# ?izimden 2 g?zlemin 0,60  e?i?inin mutlak de?erini a?t???n? g?rebiliriz.

