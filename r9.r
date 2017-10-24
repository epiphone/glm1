# 1a)
yoruotsi <- read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/yoruotsi.txt', header = TRUE)
attach(yoruotsi)
ml <- as.numeric(arvosana > 4)
clka <- lka - mean(lka)
invlogit <- function (x) { exp(x) / (1 + exp(x)) }

# 1b)
glm1 <- glm(ml ~ mies * clka, family = binomial(link='logit'))
summary(glm1)
b <- coef(glm1)
invlogit(b[1]) # vah. M:n tn naisille kun clka=0 on 0.43
exp(b[1]) # vedonlyontisuhde, eli vah. M:n odds on 0.74-kertainen M:aa alemman arvosanan saamiseen

# 1c)
exp(b[2]) # M:n odds miehilla naisiin verrattuna 0.57-kertainen kun clka=0
exp(b[3]) # kun clka eroaa yhdella, on korkeamman clka:n naisilla M:n odds 11.29-kertainen alemman clka:n naisiin verrattuna
exp(b[3]*0.2) # kun verrataan naisia joiden clka eroaa kaksi kymmenysta, ryhmassa jossa clka on suurempi M:n odds on 1.6-kertainen
exp(b[3]) * exp(b[4]) # kun verrataan miehia joiden clka eroaa yhden numeron, ryhmassa jossa clka on suurempi M:n odds on 7.54-kertainen
exp(0.2*b[3]) * exp(0.2*b[4]) # kun verrataan miehia joiden clka eroaa kaksi kymmenysta, ryhmassa jossa clka on suurempi M:n odds on 1.5-kertainen

# 1d)
clks <- sort(unique(clka))
plot(clks, invlogit(b[1] + clks*b[3]), col = 'red')
points(clks, invlogit(b[1] + b[2] + clks*b[3] + clks*b[4]), col = 'blue')
b2 <- coef(glm(ml ~ mies + clka, family = binomial(link='logit')))
plot(clks, invlogit(b2[1] + clks*b2[3]), col = 'red')
points(clks, invlogit(b2[1] + b2[2] + clks*b2[3]), col = 'blue')


# 2a)
koulu <- relevel(koulu, ref = 'B')
glm2 <- glm(ml ~ mies + koulu * clka, family = binomial)
summary(glm2)
b <- coef(glm2)

# 2b)
invlogit(b[1]) # ennustettu M:n tn kun clka=0, koulu=B ja sukupuoli nainen
exp(b[1]) # M:n vedonlyontikerroin on 0.45-kertainen M:aa alemman arvosanan saamiseen naiselle koulussa B kun clka=0

# 2c)
exp(b[2]) # miesten odds M:lle on 0.56-kertainen naisiin verrattuna, kun sama koulu ja clka=0
b[2] / 4 # miesten tn saada M on korkeintaan 0.14 pienempi kuin naisten tn kun clka=0 ja koulu on sama

# 2d)
exp(b[3:5])

# 2e)
invlogit(b[1]) # M:n tn ennuste koulun B naisille kun clka=0 on 0.31
invlogit(b[1] + b[2]) # M:n tn ennuste koulun B miehille kun clka=0 on 0.20
invlogit(b[1] + b[3]) # M:n tn ennuste koulun A naisille kun clka=0 on 0.44
invlogit(b[1] + b[2] + b[3]) # M:n tn ennuste koulun A miehille kun clka=0 on 0.30
# jne...

# 2f)
par(mfrow=c(2,2))
plot(lka, invlogit(b[1] + b[3] + b[6]*clka + b[7]*clka), col = 'red', ylab = 'M:n tn ennuste', main = 'koulu A')
points(lka, invlogit(b[1] + b[2] + b[3] + b[6]*clka + b[7]*clka), col = 'blue')
curve(0.5 + 0*x, add = TRUE)

plot(lka, invlogit(b[1] + b[6]*clka), col = 'red', ylab = 'M:n tn ennuste', main = 'koulu B')
points(lka, invlogit(b[1] + b[2] + b[6]*clka), col = 'blue')
curve(0.5 + 0*x, add = TRUE)

plot(lka, invlogit(b[1] + b[4] + b[6]*clka + b[8]*clka), col = 'red', ylab = 'M:n tn ennuste', main = 'koulu C')
points(lka, invlogit(b[1] + b[2] + b[4] + b[6]*clka + b[8]*clka), col = 'blue')
curve(0.5 + 0*x, add = TRUE)

plot(lka, invlogit(b[1] + b[5] + b[6]*clka + b[9]*clka), col = 'red', ylab = 'M:n tn ennuste', main = 'koulu D')
points(lka, invlogit(b[1] + b[2] + b[5] + b[6]*clka + b[9]*clka), col = 'blue')
curve(0.5 + 0*x, add = TRUE)

# 2g)
