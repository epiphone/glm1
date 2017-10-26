# Harjoitustyo 2

# id:           tutkimushenkilön id
# age:          tutkittavan ikä
# skin:         0 = normaali iho, 1 = helposti palava iho
# gender:       0 = nainen, 1 = mies
# exposure:     aikaisempien ihosyopien lukumaara
# cancer:       1 = jos potilaalla havaitaan uusi ihosyöpä, 0 muuten (vastemuuttuja)
# trt:          0 = plasebo, 1 = 50mg beetakaroteenia / paiva
rm(list = ls())
skincan <- read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/skincan.txt', header = TRUE)
attach(skincan)
cexposure <- exposure - mean(exposure)
cage <- age - mean(age)
boxplot(exposure ~ cancer)
plot(jitter(gender), jitter(cancer))
boxplot(exposure)
table(gender)[2]

table(cancer, gender)
table(cancer, ifelse(age > mean(age),1, 0))
table(cancer, ifelse(exposure > mean(exposure),1, 0))
table(cancer, skin)

m0 <- glm(cancer ~ (cage + skin + gender + cexposure + trt)^2, family = binomial)
summary(m0)

# m1 <- glm(cancer ~ cage + skin + gender + cexposure + trt, family = binomial)
summary(m1)
summary(glm(cancer ~ cage:cexposure + cexposure + skin + gender + trt, family = binomial))

m2 <- glm(cancer ~ gender + skin + trt + cage*cexposure, family = binomial)
summary(m2)

m3 <- glm(cancer ~ cage*cexposure + skin + gender + trt, family = binomial)
summary(m3)

m4 <- glm(cancer ~ cage*cexposure + skin + gender, family = binomial)
summary(m4)

m5 <- glm(cancer ~ cage:cexposure + cexposure + skin + gender, family = binomial("logit")) # paras
summary(m5)

# m6 <- glm(cancer ~ cage*cexposure + skin + gender + trt, family = binomial)
summary(m6)

# interaktiotermin merkitsevyys devianssin avulla:
anova(m5, m6)
d <- m5$deviance - m6$deviance
1 - pchisq(d, 2)
# prediktorien cage ja trt jattaminen malliin alentaa devianssia (4.70) mutta pchisq-testin p=0.095 joten jatetaan pois mallista

# tulkintaa
m <- m5
b <- coef(m)
invlogit <- function (x) { exp(x) / (1 + exp(x)) }
b
exp(b[2])
summary(m)
invlogit(b[1]) # syovan tn naisille kun normaali iho ja exposure=mean(exposure) ja age=mean(age) 0.09
invlogit(b[1] + b[3]) # syovan tn naisille kun palava iho ja exposure=mean(exposure) ja age=mean(age) on 0.13
invlogit(b[1] + b[4]) # syovan tn miehille kun normaali iho ja exposure=mean(exposure) ja age=mean(age) on 0.17
invlogit(b[1] + b[3] + b[4]) # syovan tn miehille kun palava iho ja exposure=mean(exposure) ja age=mean(age) on 0.23


exp(b[1]) # vedonlyontisuhde, eli ihosyovan odds on 0.10-kertainen siihen ettei ihosyopaa kun exposure=mean(exposure)
exp(b[2]) # kun exposure eroaa yhdella, on korkeamman ryhmalla 1.19-kertainen odds kun ika on mean(age)
exp(b[3]) # helposti palavan ihon odds 1.46-kertainen normaaliin ihoon verrattuna
exp(b[4]) # miesten odds on 2.06-kertainen naisiin verrattuna

# kuvaajat
summary(m)
par(mfrow=c(1,1))
plot(residuals(m, type='pearson'))
plot(residuals(m, type='deviance'))
plot(m$linear.predictors, residuals(m, type='pearson'), xlab = 'Lineaarinen prediktori', ylab= 'Pearson-jäännökset')
plot(m$linear.predictors, residuals(m, type='deviance'))

pred <- m$linear.predictors
freq <- table(cancer, pred)
prob <- freq[2,] / (freq[1,] + freq[2,])
preds <- sort(unique(pred))
plot(preds, prob, xlab = 'Lineaarinen prediktori', ylab= 'Uuden ihosyövän havaitsemisen tn')
lines(preds, invlogit(preds), col = 'red')

# olettaen age=mean(age):
par(mfrow=c(1,2), oma = c(5,4,0,0) + 0.1, mar=c(1, 1, 1, 1) + 1)
freqs <- table(cancer[gender==0], exposure[gender==0], skin[gender==0])
freqSkin0 <- freqs[,,1]
freqSkin1 <- freqs[,,2]
probSkin0 <- freqSkin0[2,] / (freqSkin0[2,] + freqSkin0[1,])
probSkin1 <- freqSkin1[2,] / (freqSkin1[2,] + freqSkin1[1,])
exps <- sort(unique(exposure[gender==0]))
plot(exps, probSkin0, main = 'Naiset', xlab=NA, ylab=NA)
points(exps, probSkin1, col = 'red')
curve(invlogit(b[1] + b[2]*(x-mean(exposure))),add=T)
curve(invlogit(b[1] + b[2]*(x-mean(exposure)) + b[3]),add=T, col = 'red')

freqs <- table(cancer[gender==1], exposure[gender==1], skin[gender==1])
freqSkin0 <- freqs[,,1]
freqSkin1 <- freqs[,,2]
probSkin0 <- freqSkin0[2,] / (freqSkin0[2,] + freqSkin0[1,])
probSkin1 <- freqSkin1[2,] / (freqSkin1[2,] + freqSkin1[1,])
exps <- sort(unique(exposure[gender==1]))
plot(exps, probSkin0, main = 'Miehet', xlab=NA, ylab=NA)
points(exps, probSkin1, col = 'red')
curve(invlogit(b[1] + b[2]*(x-mean(exposure)) + b[4]),add=T)
curve(invlogit(b[1] + b[2]*(x-mean(exposure)) + b[3] + b[4]),add=T, col = 'red')

title(xlab='Aikaisempien ihosyöpien lkm', ylab= 'Uuden ihosyövän havaitsemisen tn', outer = TRUE, line = 1)
