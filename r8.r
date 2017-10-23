yoruotsi <- read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/yoruotsi.txt', header = TRUE)
yoruotsi$laudatur <- ifelse(yoruotsi$arvosana == 6, 1, 0)
yoruotsi$clka <- lka - mean(lka)
attach(yoruotsi)
invlogit <- function (x) { exp(x) / (1 + exp(x)) }

glm1 <- glm(laudatur ~ clka, family = binomial(link='logit'))
summary(glm1)
b <- coef(glm1)

# malli = invlogit(-2.03 + (lka - mean(lka)) * 2.61)
# b0:n tulkinta: laudaturin tn, kun lka == mean(lka): invlogit(b0)
# invlogit(-2.03 + (9 - mean(lka)) * 2.61)

lk <- sort(unique(lka))
lk2 <- lk + 1
sf <- invlogit(b[1] + (lk2 - mean(lka)) * b[2]) - invlogit(b[1] + (lk - mean(lka)) * b[2])
sf
plot(sf ~ lk)

linpred <- b[1] + b[2] * (lk - mean(lka))
phat1 <- invlogit(linpred)
linpred2 <- b[1] + b[2] * (lk2 - mean(lka))

par(mfrow=c(1,2))
plot(I(phat2 - phat1) ~ lk)
plot(I(phat2 / phat1) ~ lk)


lk <- sort(unique(lka))
lk2 <- lk + 0.2
linpred <- b[1] + b[2] * (lk - mean(lka))
phat1 <- invlogit(linpred)
linpred2 <- b[1] + b[2] * (lk2 - mean(lka))
par(mfrow=c(1,2))
plot(lk, phat2 - phat1)
plot(lk, phat2 / phat1)

# b0 tulkinta vedonlyontisuhteen avulla
exp(b[1]) # vedonlyontisuhde = 0.13, eli laudaturin saamisen tn on vain 13% sen saamatta jaamisen tn:sta

# b1 tulkinta ristitulosuhteiden avulla
exp(b[2]) # kun verrataan oppilaita joilla clka eroaa yhden numeron, ryhmassa jossa clka on suurempi odds on 13.6-kertainen
exp(b[2] * 0.2) # kun verrataan oppilaita joilla clka eroaa kaksi kymmenysta, ryhmassa jossa clka on suurempi odds on 1.7-kertainen


head(yoruotsi)
glm1 <- glm(laudatur ~ mies + clka, family = binomial(link='logit'))
summary(glm1)
b <- coef(glm1)
invlogit(b[1]) # naisilla joilla clka = 0, ennustettu laudaturin tn on 0.14
b[2]/4 # ero miehissa ja naisilla on korkeintaan 0.18
b[3]/4 # ero opiskelijoilla joiden clka eroaa yhden numeron on korkeintaan 0.64
exp(b[2]) # miesten vedonlyontisuhde jaettuna naisten vedonlyontisuhteella on 0.49 kun sama clka
exp(b[3]) # kun verrataan naisia joilla clka eroaa yhden numeron, suuremman clka:n ryhman odds on 12.7-kertainen

par(mfrow=c(1,1))
sff <- invlogit(b[1] + b[3] * (lk - mean(lka)))
sfm <- invlogit(b[1] + b[2] + b[3] * (lk - mean(lka)))
plot(lk, sff, col = 'red')
points(lk, sfm, col = 'blue')

plot(laudatur ~ lka, xlab = "lka", ylab = "laudaturin todennäköisyys")
lines(lk, sff, col = 'red')
lines(lk, sfm, col = 'blue')

glm1 <- glm(laudatur ~ clka + mies + koulu, family = binomial(link='logit'))
summary(glm1)
b <- coef(glm1)

invlogit(b[1]) # koulun A naisilla kun clka = 0 ennustettu laudaturin tn on 0.14
exp(b[3]) # kun verrataan miehia ja naisia samasta koulusta kun clka = 0, miehilla odds on naisiin verrattuna 0.45-kertainen
