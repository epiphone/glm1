# Luento 5
# 19.8.2017


# 1)

fev <- read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/fev.txt', header = TRUE)
fev$fev1 <- fev$fev1 * 30.8
plot(fev$fev1 ~ fev$height)

mheight <- fev$height - mean(fev$height)
fitted <- lm(fev$fev1 ~ mheight)
fitted
coefs <- coef(fitted)
coefs

plot(fev$fev1 ~ mheight)
abline(fitted, col = 'blue')

plot(fitted$residuals ~ fitted$fitted.values) # ilmenee systemaattinen kuvio, joten mallin olettamukset eivat ole voimassa

fitted <- lm(fev$fev1 ~ mheight + I(mheight^2))

# jäännökset sovitettuja arvoja vastaan:
plot(fitted$residuals ~ fitted$fitted.values)

# jäännökset pituutta vastaan:
plot(fitted$residuals ~ fev$height)

# jäännösten boxplotit sukupuolittain:
fIndex <- which(fev$gender == '0')
mIndex <- which(fev$gender == '1')
boxplot(fitted$residuals[fIndex], fitted$residuals[mIndex])

# vasteet sovitettuja arvoja vastaan:
plot(fev$fev1 ~ fitted$fitted.values)

# logaritmi:
fev$logfev1 <- log(fev$fev1)
fitted <- lm(fev$logfev1 ~ mheight)
plot(fitted$residuals ~ fitted$fitted.values)

# g)
fev2 <- fev[fev$fev1 != min(fev$fev1),]
mheight2 <- fev2$height - mean(fev2$height)
fitted2 <- lm(fev2$fev1 ~ mheight2)
plot(fitted2$residuals ~ fitted2$fitted.values)

fitted2log <- lm(fev2$logfev1 ~ mheight2)
plot(fitted2log$residuals ~ fitted2log$fitted.values)

# h)
qqnorm(fitted2log$residuals)

# i)
fitted3 <- lm(fev2$logfev1 ~ mheight2 + fev2$gender)
plot(fitted3$residuals ~ fitted3$fitted.values)
summary(fitted3)


# 2)

mean(fev2$logfev1)
mean(fev2$fev1)
