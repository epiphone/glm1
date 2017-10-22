# 1)

yoruotsi <- read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/yoruotsi.txt', header = TRUE)
yoruotsi$laudatur <- ifelse(yoruotsi$arvosana == 6, 1, 0)
attach(yoruotsi)

boxplot(yoruotsi$lka ~ arvosana)

plot(laudatur ~ lka)
lm1 <- lm(laudatur ~ lka)
abline(lm1)
summary(lm1) # regressiokertoimilla ei jarkevaa tulkintaa

frek <- table(laudatur, lka)
frek
lk <- sort(unique(lka))
sf <- frek[2,] / (frek[1,] + frek[2,])
plot(sf ~ lk)

lukiot.1 <- glm(laudatur ~ lka, family = binomial(link='logit'))
summary(lukiot.1)
exp(-24.3850 + mean(lka) * 2.6114) / (1 + exp(-24.3850 + mean(lka) * 2.6114)) # Laudaturin todennakoisyys kun lka == mean(lka)

new.data <- data.frame(lka=lk)
pred <- predict(lukiot.1, new.data, type='response')

lines(lk, pred, type='l', lty=2)


# 2)

yoruotsi$clka <- lka - mean(lka)
attach(yoruotsi)
# logit(pi) = b0 + b1*clka  =>  pi = logit^-1(b0 + b1*clka) = exp(b0 + b1*clka) / (1 + exp(b0 + b1*clka))
lukiot.2 <- glm(laudatur ~ clka, family = binomial(link='logit'))
summary(lukiot.2)

# pi = exp(-2.203 + 2.61*clka) / (1 + exp(-2.203 + 2.61*clka))

invlogit <- function (x) { exp(x) / (1 + exp(x)) }

invlogit(-2.203) # laudaturin tod.nak., kun lka on aineiston keskiarvo
invlogit(-2.203 + (8 - mean(lka)) * 2.61) - invlogit(-2.203 + (7 - mean(lka)) * 2.61) # laudaturin tod.nak. ero 8 ja 7:n oppilailla
invlogit(-2.203 + (9 - mean(lka)) * 2.61) - invlogit(-2.203 + (8 - mean(lka)) * 2.61) # laudaturin tod.nak. ero 9 ja 8:n oppilailla
invlogit(-2.203 + (10 - mean(lka)) * 2.61) - invlogit(-2.203 + (9 - mean(lka)) * 2.61) # laudaturin tod.nak. ero 10 ja 9:n oppilailla

preds <- invlogit(-2.203 + (lk - mean(lka)) * 2.61)
plot(preds ~ lk)
