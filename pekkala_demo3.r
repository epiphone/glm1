# TILA311 Demo 3
# Aleksi Pekkala (alvianpe@student.jyu.fi)
# 22.9.2017
# Tehtävät 1-10


# 1)

yoruotsi <- read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/yoruotsi.txt', header = TRUE)
yoruotsi$clka <- yoruotsi$lka - mean(yoruotsi$lka)
attach(yoruotsi)


# 2)

fitted <- lm(ruotsi.pist ~ (mies + koulu + clka)^2)
summary(fitted)
# prediktorin mies * clka t-testin p > 0.05 => ei merkitseva


# 3)

# F-testi prediktorille mies:koulu:
b <- coef(fitted)[7:9]
S <- vcov(fitted)[7:9, 7:9]
r <- 3
F <- t(b) %*% solve(S) %*% b / r
F
p <- length(coef(fitted)) - 1
n <- length(ruotsi.pist)
1 - pf(F, r, n - p - 1) # p-arvo > 0.05 => mies:koulu ei ole merkitseva

# F-testi prediktorille koulu:clka:
b <- coef(fitted)[11:13]
S <- vcov(fitted)[11:13, 11:13]
r <- 3
F <- t(b) %*% solve(S) %*% b / r
F
p <- length(coef(fitted)) - 1
n <- length(ruotsi.pist)
1 - pf(F, r, n - p - 1) # p-arvo < 0.05 => koulu:clka on merkitseva


# 4)

fitted <- lm(ruotsi.pist ~ mies + koulu * clka)
summary(fitted)


# 5)

# kouluA = 226.890 - 10.996*mies + 38.289*clka
# kouluB = 226.890 - 10.996*mies - 8.772 + 38.289*clka + 5.160*clka
# kouluC = 226.890 - 10.996*mies + 7.771 + 38.289*clka - 3.261*clka
# kouluD = 226.890 - 10.996*mies + 1.309 + 38.289*clka - 17.267*clka
#
# koulujen B, C ja D erot koulun A oppilaiden odotettuun ruotsin pistemaaraan ovat -8.7, 7.8 ja 1.3.
# koulun vaikutus ruotsin pistemaaraan on vahvin koulussa B.
# lukuaineiden keskiarvon vaikutus ruotsin pistemaaraan on vahvin koulussa D.


# 6)

b <- coef(fitted)
lkaM <- mean(lka)

graphics.off()
par(mfrow=c(2,2), oma = c(5,4,0,0) + 0.1, mar=c(1, 1, 1, 1) + 1)
yoruotsi$cols <- ifelse(mies == 0, 'red', 'black')
attach(yoruotsi)

plot(ruotsi.pist ~ lka, col = cols, data = yoruotsi[yoruotsi$koulu == 'A',], xlim = c(6.5, 10), ylim = c(120, 300), main = 'Lukio A')
curve(b[1] + b[2] + b[6] * (x - lkaM), add=T)
curve(b[1] + b[6] * (x - lkaM), add=T, col = 'red')

plot(ruotsi.pist ~ lka, col = cols, data = yoruotsi[yoruotsi$koulu == 'B',], xlim = c(6.5, 10), ylim = c(120, 300), main = 'Lukio B')
curve(b[1] + b[2] + b[3] + b[6] * (x - lkaM) + b[7] * (x - lkaM), add=T)
curve(b[1] + b[3] + b[6] * (x - lkaM)  + b[7] * (x - lkaM), add=T, col = 'red')

plot(ruotsi.pist ~ lka, col = cols, data = yoruotsi[yoruotsi$koulu == 'C',], xlim = c(6.5, 10), ylim = c(120, 300), main = 'Lukio C')
curve(b[1] + b[2] + b[4] + b[6] * (x - lkaM) + b[8] * (x - lkaM), add=T)
curve(b[1] + b[4] + b[6] * (x - lkaM)  + b[8] * (x - lkaM), add=T, col = 'red')

plot(ruotsi.pist ~ lka, col = cols, data = yoruotsi[yoruotsi$koulu == 'D',], xlim = c(6.5, 10), ylim = c(120, 300), main = 'Lukio D')
curve(b[1] + b[2] + b[5] + b[6] * (x - lkaM) + b[9] * (x - lkaM), add=T)
curve(b[1] + b[5] + b[6] * (x - lkaM)  + b[9] * (x - lkaM), add=T, col = 'red')

title(xlab = 'Peruskoulun lukuaineiden keskiarvo', ylab = 'Pistemäärä ruotsin YO-kokeessa', outer = TRUE, line = 3)


# 7)

yoruotsi$ruotsi8 <- ruotsi - 8
attach(yoruotsi)
ruotsi8


# 8)

fitted <- lm(ruotsi.pist ~ mies + koulu + ruotsi8)
summary(fitted)
boxplot(ruotsi.pist ~ ruotsi8)
# ero odotetuissa ruotsin pistemaarissa naisten ja miesten valilla on -3.544
# ero odotetuissa ruotsin pistemaarissa on 22.543 + (ruotsin arvosana - 8)


# 9)

fitted <- lm(ruotsi.pist ~ koulu + mies * ruotsi8)
summary(fitted)
# miehet: 213.925 - 5.411*kouluB + 9.613*kouluC + 5.674*kouluD - 3.035 + 23.913*ruotsi8 - 3.195*ruotsi8
# naiset: 213.925 - 5.411*kouluB + 9.613*kouluC + 5.674*kouluD + 23.913*ruotsi8
# ero odotetuissa miesten ja naisten pistemaarissa on -3.035 - 3.195*(ruotsin arvosana - 8)


# 10)

# interaktiotermi mies:ruotsi8 on syyta jattaa pois, koska t-testin p-arvo on > 0.05.