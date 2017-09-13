# 1

yoruotsi <- read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/yoruotsi.txt', header=TRUE)
yoruotsi <- yoruotsi[yoruotsi$koulu == 'A' | yoruotsi$koulu == 'B',]
yoruotsi$kouluA <- ifelse(yoruotsi$koulu == 'A', 1, 0)
attach(yoruotsi)


# 2 

fitted <- lm(ruotsi.pist ~ kouluA + lka)
summary(fitted)
X <- model.matrix(fitted)

# pns-estimaatit:
bhat <- solve(t(X) %*% X) %*% t(X) %*% ruotsi.pist

# sovitteet:
yhat <- X %*% bhat


# jäännökset (y - yhat erotuksia, vrt. regressiosuora ja pisteet):
e <- ruotsi.pist - yhat

# jäännösvarianssin estimaatti
n <- length(ruotsi.pist)
p <- 2
s2 <- t(e) %*% e / (n - p - 1)
s <- sqrt(s2)[1, 1]

# regressiokertoimien kovarianssimatriisin estimaatti ja regressiokertoimien keskivirheet:
covs <- s^2 * solve(t(X) %*% X)
stdErr <- sqrt(diag(covs))
covs

# regressiokertoimien kouluA ja lka 95% luottamusvälit:
kouluAErrMargin <- c(bhat[2] - 2 * stdErr[2], bhat[2] + 2 * stdErr[2])
kouluAErrMargin

lkaErrMargin <- c(bhat[3] - 2 * stdErr[3], bhat[3] + 2 * stdErr[3])
lkaErrMargin

# luottamusvalit t-jakaumalla:
tc <- qt(0.975, n - p - 1)
tc
kouluAErrMarginT <- c(bhat[2] - tc * stdErr[2], bhat[2] + tc * stdErr[2])
lkaErrMarginT <- c(bhat[3] - tc * stdErr[3], bhat[3] + tc * stdErr[3])

# testataan nollahypoteesiä H0 : B1 = 0, kun vastahypoteesinä on H1 : B1 != 0:
summary(fitted)
bhat[2] / stdErr[2] # t-testisuure kouluA

bhat[3] / stdErr[3] # t-testisuure lka

# mallin selitysaste:
y <- ruotsi.pist
R2 <- 1 - t(e) %*% e / t(y - mean(y)) %*% (y - mean(y))