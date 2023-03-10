
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

#Da??l?m

plot(df$x)
densityplot(df$x)

## Ortalamalar
ortalamadf <- df

?ndex <- which(ortalamadf$x %in% boxplot.stats(ortalamadf$x)$out)

?ndex

mean(ortalamadf$x) # Ortalama

ortalamadf[?ndex, ] <- mean(ortalamadf$x) 

ortalamadf

densityplot(ortalamadf$x, xlab = "Ortalama ile Bask?lama")

# MEDYAN

newdf <- sort(df$x)

s?ral?df<- data.frame(newdf)

median(s?ral?df$newdf) #Medyan

medyandf <- s?ral?df 

?ndexmed <- which(medyandf$newdf %in% boxplot.stats(medyandf$newdf)$out)

medyandf[?ndexmed, ] <- median(medyandf$newdf)


densityplot(medyandf$newdf,xlab = "Medyan ile Bask?lama")

#TRIMMED Mean

mean(s?ral?df$newdf, trim = 0.05) #Trimmed mean n=107*0.05 = 5.35 6 a?a?? ve yukardan budayacak.


tr?mmdf <- s?ral?df

?ndextr?m <- which(tr?mmdf$newdf %in% boxplot.stats(tr?mmdf$newdf)$out) #Ayn? de?erler zaten

tr?mmdf[?ndextr?m, ] <- mean(tr?mmdf$newdf, trim = 0.05)

densityplot(tr?mmdf$newdf, xlab="TRIMMED Mean")

## Winsorized Ortalama
set.seed(54)
winsorizedf <- Winsorize(s?ral?df$newdf) 
# Kuyruklardaki negatif u? de?erlerini "-2.13" g?zlem de?erine pozitif k?sm? "1.873" g?zlem de?erine atad?.
w?nsodf <- as.data.frame(winsorizedf)

densityplot(w?nsodf$winsorizedf, xlab="Winsorized Mean")

## TR?MMED L Tipi Ortalama

#TLdf<-newdf

#lmomnor(para = TLdf)
#t1lmoments(newdf,rmax = 4) # Bunu pek anlamad?m hocam. 

#Hodges-Lehman
HLdf <- newdf

HL.estimate(HLdf) 

?ndexHL <- which(HLdf %in% boxplot.stats(HLdf)$out)

HLdf[?ndexHL] <- HL.estimate(HLdf)

densityplot(HLdf, xlab = "Hodges-Lehman Tahmin Edicisi R Tipi")

# M Tahmin Edicileri 
# Huber M Tahmini
HMdf <- newdf


huber.mu(HMdf)
huber.mu(HMdf, c = 1.28, iter = 20, conv = 1e-07)

HMdf[?ndexHL] <- huber.mu(newdf)

densityplot(HLdf, xlab = "Huber M Tahmin Edicisi")


#?l?e?in Sa?lam Tahmin Edicileri


mad(newdf, center = median(newdf), constant = 1.4826, na.rm = FALSE,
    low = FALSE, high = FALSE)

#Sn

Sn(newdf, constant = 1.1926)

#Qn


Qn(newdf, constant = NULL)








