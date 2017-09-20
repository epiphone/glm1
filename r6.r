# Luento 6
# 20.9.2017

# 1)

yoruotsi <- read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/yoruotsi.txt', header = TRUE)
yoruotsi$kouluA <- as.numeric(yoruotsi$koulu == 'A')
yoruotsi$kouluB <- as.numeric(yoruotsi$koulu == 'B')
yoruotsi$kouluC <- as.numeric(yoruotsi$koulu == 'C')
yoruotsi$kouluD <- as.numeric(yoruotsi$koulu == 'D')

attach(yoruotsi)
plot(ruotsi.pist ~ koulu)

fitted <- lm(ruotsi.pist ~ kouluA + kouluB + kouluC + kouluD)
fitted # regressiokerroin kouluD == NA
abline(fitted, col = 'blue')

fitted <- lm(ruotsi.pist ~ kouluA + kouluB + kouluC)
fitted # intercept = kouluD keskim. pistemaara, kouluA...C = poikkeamat siita

fitted <- lm(ruotsi.pist ~ koulu)
fitted # referenssimuuttujana toimii kouluA
abline(fitted, col = 'green')

fitted <- lm(ruotsi.pist ~ relevel(koulu, ref = 'B'))
fitted
abline(fitted, col = 'red')


# 2)

summary(fitted) # eniten eroavat koulut A ja C, ero kouluun D on vahemman selkea.

# F-testi:
b <- coef(fitted)[2:4] # X <- model.matrix(fitted); solve(t(X) %*% X) %*% t(X) %*% ruotsi.pist
S <- vcov(fitted)[2:4, 2:4]
r <- 3
F <- t(b) %*% solve(S) %*% b / r

p <- 3
n <- length(ruotsi.pist)
1 - pf(F, r, n - p - 1) # p-arvo


# 3)

# lisataan sukupuoli:
fitted <- lm(ruotsi.pist ~ koulu + mies)
summary(fitted)
intercept <- coef(fitted)[1]
meansF <- c(coef(fitted)[1], intercept + coef(fitted)[2], intercept + coef(fitted)[3], intercept + coef(fitted)[4])
plot(meansF, type = 'l', ylim = c(180, 250))
lines(meansF + coef(fitted)[5])

# interaktio:
fitted <- lm(ruotsi.pist ~ koulu * mies)
b <- coef(fitted)
meansF <- c(0, b[2:4]) + b[1]
meansM <- meansF + c(0, b[6:8]) + b[5]
plot(meansF, type = 'l', ylim = c(190, 240))
lines(meansM)

# F-testi:
fitted <- lm(ruotsi.pist ~ relevel(koulu, ref = 'B') * mies)
summary(fitted)
b <- coef(fitted)[6:8]
S <- vcov(fitted)[6:8, 6:8]
r <- 3
F <- t(b) %*% solve(S) %*% b / r
F

p <- 7
n <- length(ruotsi.pist)
1 - pf(F, r, n - p - 1) # p-arvo
