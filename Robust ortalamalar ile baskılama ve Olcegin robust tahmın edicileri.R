
library(ggplot2)
library(tidyverse)
library(lattice)
library(DescTools) # Winsorized
library(Lmoments) # TL Mean icin
library(rt.test)
library(lmomco)
library(asbio)
library(stats)
library(robustbase)
library(lmomnor)



set.seed(54)
x <- rnorm(100)
summary(x)
x <- c(x, c(-5,-6,-3,3,4,5,6))
boxplot(x)
boxplot.stats(x)$out


df <- data.frame(x)

#Daðýlým

plot(df$x)
densityplot(df$x)

## Ortalamalar
ortalamadf <- df

ýndex <- which(ortalamadf$x %in% boxplot.stats(ortalamadf$x)$out)

ýndex

mean(ortalamadf$x) # Ortalama

ortalamadf[ýndex, ] <- mean(ortalamadf$x) 

ortalamadf

densityplot(ortalamadf$x, xlab = "Ortalama ile Baskýlama")

# MEDYAN

newdf <- sort(df$x)

sýralýdf<- data.frame(newdf)

median(sýralýdf$newdf) #Medyan

medyandf <- sýralýdf 

ýndexmed <- which(medyandf$newdf %in% boxplot.stats(medyandf$newdf)$out)

medyandf[ýndexmed, ] <- median(medyandf$newdf)


densityplot(medyandf$newdf,xlab = "Medyan ile Baskýlama")

#TRIMMED Mean

mean(sýralýdf$newdf, trim = 0.05) #Trimmed mean n=107*0.05 = 5.35 6 aþaðý ve yukardan budayacak.


trýmmdf <- sýralýdf

ýndextrým <- which(trýmmdf$newdf %in% boxplot.stats(trýmmdf$newdf)$out) #Ayný deðerler zaten

trýmmdf[ýndextrým, ] <- mean(trýmmdf$newdf, trim = 0.05)

densityplot(trýmmdf$newdf, xlab="TRIMMED Mean")

## Winsorized Ortalama
set.seed(54)
winsorizedf <- Winsorize(sýralýdf$newdf) 
# Kuyruklardaki negatif uç deðerlerini "-2.13" gözlem deðerine pozitif kýsmý "1.873" gözlem deðerine atadý.
wýnsodf <- as.data.frame(winsorizedf)

densityplot(wýnsodf$winsorizedf, xlab="Winsorized Mean")

## TRÝMMED L Tipi Ortalama

#TLdf<-newdf

#lmomnor(para = TLdf)
#t1lmoments(newdf,rmax = 4) # Bunu pek anlamadým hocam. 

#Hodges-Lehman
HLdf <- newdf

HL.estimate(HLdf) 

ýndexHL <- which(HLdf %in% boxplot.stats(HLdf)$out)

HLdf[ýndexHL] <- HL.estimate(HLdf)

densityplot(HLdf, xlab = "Hodges-Lehman Tahmin Edicisi R Tipi")

# M Tahmin Edicileri 
# Huber M Tahmini
HMdf <- newdf


huber.mu(HMdf)
huber.mu(HMdf, c = 1.28, iter = 20, conv = 1e-07)

HMdf[ýndexHL] <- huber.mu(newdf)

densityplot(HLdf, xlab = "Huber M Tahmin Edicisi")


#Ölçeðin Saðlam Tahmin Edicileri


mad(newdf, center = median(newdf), constant = 1.4826, na.rm = FALSE,
    low = FALSE, high = FALSE)

#Sn

Sn(newdf, constant = 1.1926)

#Qn


Qn(newdf, constant = NULL)








