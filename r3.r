# 1

yoruotsi <- read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/yoruotsi.txt', header=TRUE)
yoruotsi <- yoruotsi[yoruotsi$koulu == 'A' | yoruotsi$koulu == 'B',]
yoruotsi$kouluA <- ifelse(yoruotsi$koulu == 'A', 1, 0)
attach(yoruotsi)


# 2

fitted <- lm(ruotsi.pist ~ kouluA + lka)
fitted # regressiokertoimet ovat kouluA = 8.8, lka = 43.3
b3 <- coef(fitted)
plot(lka, ruotsi.pist)
abline(b3[1], b3[3], col = 'blue')
abline(b3[1] + b3[2], b3[3], col = 'red')

clka <- lka - mean(lka)
fitted <- lm(ruotsi.pist ~ kouluA + clka)
b3 <- coef(fitted)
b3
plot(lka, ruotsi.pist)
curve(b3[1] + b3[3] * (x - mean(lka)), from = 7, to = 10, add = TRUE, col = 'blue')
curve(b3[1] + b3[2] + b3[3] * (x - mean(lka)), from = 7, to = 10, add = TRUE, col = 'red')


# 3

fitted <- lm(yoruotsi$ruotsi.pist ~ kouluA * clka)
fitted
plot(lka, ruotsi.pist)
abline(lm(ruotsi.pist ~ lka, data = yoruotsi[kouluA == 0,]), col = 'blue')
abline(lm(ruotsi.pist ~ lka, data = yoruotsi[kouluA == 1,]), col = 'red')
