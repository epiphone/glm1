# 1)
iq <- read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/iq.txt', header = TRUE)
iq$iq_large <- ifelse(iq$kid_score > 100, 1, 0)
iq$iq100 <- iq$mom_iq - 100
iq$mom_work <- as.factor(iq$mom_work)
attach(iq)

# 2)
glm1 <- glm(iq_large ~ mom_hs + iq100 * mom_work, family = binomial)
glm2 <- glm(iq_large ~ mom_hs + iq100 + mom_work, family = binomial)
d <- glm2$deviance - glm1$deviance # devianssien ero
1 - pchisq(d, 3) # ~0.09, ei tilastollisesti merkitseva

# 3)
glm3 <- glm(iq_large ~ mom_hs + iq100 + mom_work, family = binomial)
d <- glm(iq_large ~ mom_hs + iq100, family = binomial)$deviance - glm3$deviance
1 - pchisq(d, 3) # < 0.05, on merkitseva

# 4)
b <- coef(glm3)[4:6]
S <- vcov(glm3)[4:6,4:6]
Q <- t(b) %*% solve(S) %*% b
1 - pchisq(Q, 3) # < 0.05, luokitteleva mom_work-muuttuja on Q-testin perusteella merkitseva

# 5)
anova(glm3, glm(iq_large ~ mom_hs + iq100, family = binomial)) # palauttaa deviansseja erotuksineen

# 6)
b <- coef(glm3)
sd <- sqrt(diag(vcov(glm3)))
exp(b[2]) # odds lukion kayneille aideille on 1.7-kertainen muihin aiteihin verrattuna, kun iq100=0 ja tyostatus on sama
c(exp(b[2] - 1.96*sd[2]), exp(b[2] + 1.96*sd[2]))
exp(b[3]) # kun vertaillaan ryhmia joiden iq eroaa yhdella pisteella, ylemman ryhman odds on 1.05-kertainen kun tyo- ja lukiostatus on sama
c(exp(b[3] - 1.96*sd[3]), exp(b[3] + 1.96*sd[3]))
exp(b)
exp(confint(glm3))

# 7-8)
par(mfrow=c(2,2))
invlogit <- function (x) { exp(x) / (1 + exp(x)) }
iqs <- sort(unique(mom_iq))
b
freq <- table(iq_large[mom_work==1], mom_iq[mom_work==1])
prob <- freq[2,] / (freq[2,] + freq[1,])
iqs <- sort(unique(mom_iq[mom_work==1]))
plot(iqs, prob, main = 'Work 1')
curve(invlogit(b[1]+b[2] + b[3]*(x-100)),add=T, col = 'green')
curve(invlogit(b[1]+b[3]*(x-100)),add=T)

freq <- table(iq_large[mom_work==2], mom_iq[mom_work==2])
prob <- freq[2,] / (freq[2,] + freq[1,])
iqs <- sort(unique(mom_iq[mom_work==2]))
plot(iqs, prob, main = 'Work 2')
curve(invlogit(b[1]+b[2] + b[3]*(x-100) + b[4]),add=T, col = 'green')
curve(invlogit(b[1]+b[3]*(x-100) + b[4]),add=T)

freq <- table(iq_large[mom_work==3], mom_iq[mom_work==3])
prob <- freq[2,] / (freq[2,] + freq[1,])
iqs <- sort(unique(mom_iq[mom_work==3]))
plot(iqs, prob, main = 'Work 3')
curve(invlogit(b[1]+b[2] + b[3]*(x-100) + b[5]),add=T, col = 'green')
curve(invlogit(b[1]+b[3]*(x-100) + b[5]),add=T)

freq <- table(iq_large[mom_work==4], mom_iq[mom_work==4])
prob <- freq[2,] / (freq[2,] + freq[1,])
iqs <- sort(unique(mom_iq[mom_work==4]))
plot(iqs, prob, main = 'Work 4')
curve(invlogit(b[1]+b[2] + b[3]*(x-100) + b[6]),add=T, col = 'green')
curve(invlogit(b[1]+b[3]*(x-100) + b[6]),add=T)

# 9)
par(mfrow=c(1,1))
plot(residuals(glm3, type='pearson'))
plot(residuals(glm3, type='deviance'))
plot(glm3$linear.predictors, residuals(glm3, type='pearson'))
plot(glm3$linear.predictors, residuals(glm3, type='deviance'))
# Randomized quantile residuals/PIT-residuals (Dunn-Smyth, 1996)
library(mvabund)
fit <- manyglm(ml ~ clka + mies + koulu, family=binomial(), data=yoruotsi)
plot(fit$fitted, fit$PIT.residuals)

# 10)
pred <- glm3$linear.predictors
freq <- table(iq_large, pred)
prob <- freq[2,] / (freq[1,] + freq[2,])
preds <- sort(unique(pred))
plot(preds, prob)
lines(preds, invlogit(preds))

# 11)
b <- coef(glm3)
confint(glm3)

probs <- fitted(glm3)
b <- coef(glm3)
sim.logit <- function(probs)
{
  y <- rbinom(length(probs), probs, size=1)
  out <- glm(y ~ mom_hs + iq100 + factor(mom_work), family = binomial)
  coefs <- summary(out)$coefficients
  c(coefs[3,1],coefs[3,2])
}
b.sim <- replicate(1000,sim.logit(probs))
z <- (b.sim[1,]-b[3])/(b.sim[2,])
z <- sort(z)
sd <- sqrt(diag(vcov(malli)))
#95% lv:
c(b[3]-z[975]*sd[3], b[3]-z[25]*sd[3]) #0.03435954 0.06655808

