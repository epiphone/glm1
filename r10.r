# 1a)
yoruotsi <- read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/yoruotsi.txt', header = TRUE)
attach(yoruotsi)
ml <- as.numeric(arvosana > 4)
clka <- lka - mean(lka)
invlogit <- function (x) { exp(x) / (1 + exp(x)) }

# 1b)
glm1 <- glm(ml ~ mies + clka, family = binomial)
summary(glm1)
b <- coef(glm1)
sd <- sqrt(diag(vcov(glm1)))
b[2:3] - 1.96 * sd[2:3] # alarajat
b[2:3] + 1.96 * sd[2:3] # ylarajat
confint(glm1) # ala-ja ylarajat confint-funktiolla

# 1c)
exp(b[2:3]) # vedonlyontisuhteet
exp(b[2:3] - 1.96 * sd[2:3]) # alarajat
exp(b[2:3] + 1.96 * sd[2:3]) # ylarajat
exp(confint.default(glm1)) # rajat confint-funktiolla

# 1d)
glm2 <- glm(ml ~ mies * clka, family = binomial)
summary(glm2)
b <- coef(glm2)
sd <- sqrt(diag(vcov(glm2)))

# clka:n OR luottamusvaleineen naisilla:
exp(b[3])
c(exp(b[3] - 1.96 * sd[3]), exp(b[3] + 1.96 * sd[3]))

# clka:n OR luottamusvaleineen miehilla:
nainen <- 1 - mies
glm3 <- glm(ml ~ nainen * clka, family = binomial)
b2 <- coef(glm3)
sd2 <- sqrt(diag(vcov(glm3)))
exp(b2[3])
c(exp(b2[3] - 1.96 * sd2[3]), exp(b2[3] + 1.96 * sd2[3]))

# 1e)
exp(b[3]*0.2)
c(exp(b[3]*0.2 - 1.96 * sd[3]*0.2), exp(b[3]*0.2 + 1.96 * sd[3]*0.2))

# 1f)
summary(glm1)
summary(glm2) # interaktiotermin P > 0.1, ei merkitseva
d <- glm1$deviance - glm2$deviance # devianssien ero
1 - pchisq(d, 1)

# 1g) interaktiotermin merkitsevyys Q-testin avulla:
glm4 <- glm(ml ~ mies + koulu * clka, family = binomial)
b <- coef(glm4)[7:9]
S <- vcov(glm4)[7:9,7:9]
Q <- t(b) %*% solve(S) %*% b
1 - pchisq(Q, 3) # < 0.005, koulu:clka-interaktio on merkitseva

# 1h) interaktiotermin merkitsevyys devianssin avulla:
glm5 <- glm(ml ~ mies + koulu + clka, family = binomial)
d <- glm5$deviance - glm4$deviance
1 - pchisq(d, 3) # < 0.005, on merkitseva


# 2a)
par(mfrow=c(1,1))
glm6 <- glm(ml ~ clka, family = binomial)
b <- coef(glm6)
freq <- table(ml, lka)
prob <- freq[2,] / (freq[2,] + freq[1,])
lk <- sort(unique(lka))
plot(lk, prob)
curve(invlogit(b[1]+b[2]*(x-mean(lka))),add=T, col = 'red')

# 2b)
glm7 <- glm(ml ~ clka + mies + koulu, family = binomial)
pred <- glm7$linear.predictors

freq <- table(ml, pred)
prob <- freq[2,] / (freq[2,] + freq[1,])
pred <- sort(unique(pred))
plot(pred, prob, ylab='Tn', xlab='lin.pred')
lines(pred, invlogit(pred))
plot(residuals(glm7, type='deviance'), glm7$linear.predictors)