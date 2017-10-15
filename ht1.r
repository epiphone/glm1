# TILA311 HT1
# Aleksi Pekkala (alvianpe@student.jyu.fi)
# 14.10.2017


pisa <- read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/pisadata.txt', header = TRUE)
pisa$cmatem <- pisa$matem - mean(pisa$matem)
pisa$topmatem <- factor(ifelse(pisa$matem > mean(pisa$matem), 1, 0))
pisa$motiv <- factor(pisa$motiv)
pisa$ita <- factor(ifelse(pisa$koulualue == 'Ita-Suomi', 1, 0))
head(pisa)
str(pisa)
attach(pisa)

# sukup HISEI SES koulusij koulualue motiv matem aidink
summary(lm(mpist ~ .*., data = pisa))
plot(HISEI ~ SES)

# final:
m <- lm(mpist ~ HISEI + SES + ita + sukup + matem + aidink)
summary(m)
plot(m$residuals ~ m$fitted.values, xlab='Sovite', ylab='Jäännös') # ilmenee systemaattinen kuvio, joten mallin olettamukset eivat ole voimassa
qqplot(m$residuals, m$fitted.values, xlab = 'Teoreettiset kvantiilit', ylab = 'Otosten kvantiilit')

mean(mpist)
length(unique(id))
plot(jitter(matem), mpist)
hist(matem)
boxplot(mpist ~ koulualue, xlab = 'Koulun lääni', ylab = 'PISA-kokeen matematiikan pistemäärä')

# Histogrammi matematiikan viim. arvosanasta sukupuolittain:
barplot(
  table(sukup, matem),
  beside = TRUE,
  col = c('blue', 'red'),
  xlab = 'Matematiikan viimeisin arvosana',
  ylab = 'Oppilaiden lukumäärä',
)
legend("topright",legend = c("pojat", "tytöt"), fill = c("blue", "red"))
nrow(pisa[motiv == 0,])
pie(table(sukup))
# Histogrammi mpist:
hist(mpist, xlab = 'PISA-kokeen matematiikan pistemäärä', ylab = 'Oppilaiden lukumäärä', main = '', col = 'gray')
# Histogrammi motivaatiosta sukupuolittain:
mpistg <- as.factor(cut(mpist, seq(300, 800, 100)))
table(sukup, mpistg)
barplot(
  table(motiv, mpistg),
  beside = TRUE,
  col = c('blue', 'red'),
  xlab = 'PISA-kokeen matematiikan pistemäärä',
  ylab = 'Oppilaiden lukumäärä'
)
legend("topright",legend = c("ei motivoitunut", "motivoitunut"), fill = c("blue", "red"))

plot(mpist ~ jitter(matem), col = ifelse(sukup == 'tytto', 'red', 'blue'))
abline(lm(mpist ~ matem, data = pisa[sukup == 'tytto',]), col = 'red')
abline(lm(mpist ~ matem, data = pisa[sukup == 'poika',]), col = 'blue')

m2 <- lm(mpist ~ sukup + SES + motiv + matem + aidink)
summary(m2)
m1 <- lm(mpist ~ motiv + koulualue + SES + matem + sukup + aidink)
summary(m1)

m <- lm(mpist ~ koulualue)
summary(m)
boxplot(mpist ~ sukup)
boxplot(mpist ~ poika)
plot(m$fitted.values, m$residuals)

abline(m)

abline(m1)
plot(m1)
