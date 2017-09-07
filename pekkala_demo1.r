# TILA311 Demo 1
# Aleksi Pekkala (alvianpe@student.jyu.fi)
# 7.9.2017

# 1)

iq <- read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/iq.txt', header = TRUE)


# 2)

iq <- iq[,c('kid_score', 'mom_hs')]


# 3)

plot(iq$kid_score)
iq <- iq[iq$kid_score > min(iq$kid_score),]


# 4)

scoresHS <- iq[iq$mom_hs == 1,]$kid_score
scoresNoHS <- iq[iq$mom_hs == 0,]$kid_score
boxplot(scoresHS, scoresNoHS)
mean(scoresHS)
mean(scoresNoHS)
t.test(scoresNoHS, scoresHS) # p < 0.05


# 5)

iq.lm <- lm(iq$kid_score ~ iq$mom_hs)
iq.lm # b1 = 78.17, b2 = 11.15
plot(jitter(iq$mom_hs), iq$kid_score)
abline(iq.lm)

mean(scoresNoHS) # 78.17391
mean(scoresHS) - mean(scoresNoHS) # 11.14574


# 6)

mean(iq$mom_hs) * 11.14574
momHSCentered <- iq$mom_hs - mean(iq$mom_hs)
iq.lm <- lm(iq$kid_score ~ momHSCentered)
iq.lm # b1 = 86.95, b2 = 11.15
# mallin vakiotermi oli ennen keskittämistä mean(scoresNoHS), keskittämisen jälkeen
# mean(scoresNoHS) + mean(iq$mom_hs) * (mean(scoresHS) - mean(scoresNoHS))


# 7)

iq <- read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/iq.txt', header = TRUE)
iq <- iq[iq$kid_score > min(iq$kid_score),]
iq <- iq[,c('kid_score', 'mom_iq')]


# 8)

plot(iq$kid_score, iq$mom_iq)
cor(iq$kid_score, iq$mom_iq) # ~ 0.45, eli korreloi positiivisesti


# 9)

iq.lm <- lm(iq$kid_score ~ iq$mom_iq)
iq.lm

# sellaisten lasten testipistemäärien keskiarvo, joiden äidin älykkyysosamäärä on 100:
mean(iq[round(iq$mom_iq) == 100,]$kid_score)

# Mikä on keski-määräinen ero lasten pistemäärissä,
# kun äitien älykkyysosamäärät eroavat 20 pisteellä?
# ???
