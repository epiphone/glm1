# TILA311 Demo 2
# Aleksi Pekkala (alvianpe@student.jyu.fi)
# 17.9.2017
# Tehtävät 1-8


# 1)

iq <- read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/iq.txt', header = TRUE)
iq <- iq[,c('kid_score', 'mom_hs', 'mom_age')]


# 2)

fitted <- lm(iq$kid_score ~ iq$mom_age)
plot(iq$kid_score ~ jitter(iq$mom_age))
abline(fitted, col = 'blue')


# 3)

plot(fitted$fitted.values, fitted$residuals)


# 4)

summary(fitted)
# Prediktorin mom_age regressiokerroin on noin 0.7, eli kid_score kasvaa 0.7 kun
# mom_age kasvaa yhden yksikön. 

# Parasta ikää ei voida suositella, koska prediktorin regressiokerroin on lähellä nollaa.
# Lisäksi mallin selitysaste R^2 on matala (~0.008)


# 5) 

fitted <- lm(iq$kid_score ~ iq$mom_age + iq$mom_hs)
summary(fitted)
# Prediktorin mom_hs regressiokerroin on noin 11.3, eli se näyttää vaikuttavan jyrkemmin
# arvoon kid_score.


# 6)

coefs <- summary(fitted)$coefficients
n <- length(iq$kid_score)
tc <- qt(0.975, n-2-1)
momAgeErrMargin <- c(coefs[2, 1] - tc * coefs[2, 2], coefs[2, 1] + tc * coefs[2, 2])
momHSErrMargin <- c(coefs[3, 1] - tc * coefs[3, 2], coefs[3, 1] + tc * coefs[3, 2])
# Regressiokerroin mom_age ei luottamusvälien perusteella poikkea merkittävästi nollasta.


# 7)

fitted <- lm(kid_score ~ mom_age * mom_hs, data = iq)
iqHS <- iq[iq$mom_hs == 1,]
iqNoHS <- iq[iq$mom_hs == 0,]
fittedHS <- lm(kid_score ~ mom_age * mom_hs, data = iqHS)
fittedNoHS <- lm(kid_score ~ mom_age * mom_hs, data = iqNoHS)
coef(fittedHS)
coef(fittedNoHS)
# Prediktorin mom_age regressiokerroin muuttuu negatiiviseksi kun mom_hs on 0, eli
# lukiota käymättömien äitien ikä vaikuttaa negatiivisesti lasten testituloksiin.


# 8)

plot(iq$kid_score ~ jitter(iq$mom_age))
plot(iqHS$kid_score ~ iqHS$mom_age)
abline(fittedHS, col = 'blue')
abline(fittedNoHS, col = 'red')
