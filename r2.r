# 1

yoruotsi<-read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/yoruotsi.txt', header=TRUE)

names(yoruotsi)
yoruotsi[1:10,]
yoruotsi[1:10,]$ruotsi.pist


# 2

index <- which(yoruotsi$koulu == 'A')
yoruotsi <- yoruotsi[index,]
summary(yoruotsi)
colnames(yoruotsi)[5] <- 'Apist'

ApistMedian <- median(yoruotsi$Apist)
lower <- yoruotsi[which(yoruotsi$Apist < ApistMedian),]
upper <- yoruotsi[which(yoruotsi$Apist >= ApistMedian),]
combined <- rbind(lower, upper)


# 3

yoruotsi<-read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/yoruotsi.txt', header=TRUE, stringsAsFactors = FALSE)
abOnly <- yoruotsi[yoruotsi$koulu == 'A' | yoruotsi$koulu == 'B',]
abOnly$koulu <- as.factor(abOnly$koulu)
hist(abOnly$ruotsi.pist)
boxplot(abOnly$ruotsi.pist ~ abOnly$koulu)

aOnly <- yoruotsi[yoruotsi$koulu == 'A',]
bOnly <- yoruotsi[yoruotsi$koulu == 'B',]
mean(aOnly$ruotsi.pist)
var(aOnly$ruotsi.pist)
mean(bOnly$ruotsi.pist)
var(bOnly$ruotsi.pist)

abOnly$kouluA <- ifelse(abOnly$koulu == 'A', 1, 0) # as.numeric

attach(abOnly)

# lineaarinen regressiomalli:
yoruotsi.lm <- lm(ruotsi.pist ~ kouluA)
yoruotsi.lm

# kuvaaja:
plot(jitter(kouluA), ruotsi.pist)
abline(yoruotsi.lm)


# 4

plot(abOnly$lka, abOnly$ruotsi.pist, xlab='keskiarvo', ylab='ruotsi arvosana')
cor(abOnly$ruotsi.pist, abOnly$lka)

boxplot(abOnly$ruotsi.pist ~ abOnly$koulu, xlab='koulu')
boxplot(abOnly$ruotsi.pist ~ abOnly$mies, xlab='sukupuoli')
boxplot(abOnly$lka ~ abOnly$koulu, xlab='koulu', ylab='keskiarvo')
boxplot(abOnly$lka ~ abOnly$mies, xlab='sukupuoli', ylab='keskiarvo')

# d-e)
abOnly.lm <- lm(abOnly$ruotsi.pist ~ abOnly$lka)
abOnly.lm

# f-g) keskitetaan lka
lkaCentered <- abOnly$lka - mean(abOnly$lka)
abOnly.lm2 <- lm(abOnly$ruotsi.pist ~ lkaCentered)
# abOnly.lm2 <- lm(abOnly$ruotsi.pist ~ I(abOnly$lka - mean(abOnly$lka)))
abOnly.lm2 # regressiokerroin/kulmakerroin b1 = 43.7

plot(lkaCentered, abOnly$ruotsi.pist)
abline(abOnly.lm2)

# h)
lka8 <- yoruotsi[yoruotsi$lka == 8,]
abOnly.lm3 <- lm(abOnly$ruotsi.pist ~ abOnly$lka, subset = lka8)
abOnly.lm3
plot(lka8$lka, lka8$ruotsi.pist)
abline(lka8.lm)
