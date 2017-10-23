# 1)
iq <- read.table('http://users.jyu.fi/~slahola/files/ylm1_datoja/iq.txt', header = TRUE)
iq <- iq[,c('mom_hs', 'mom_age', 'mom_iq')]
iq$iq100 <- iq$mom_iq - 100
attach(iq)


# 2)
glm1 <- glm(mom_hs ~ iq100, family = binomial(link='logit'))
summary(glm1)
b <- coef(glm1)
# 1.49 + 0.05*iq100
# exp(1.49 + 0.05*iq100) / (1 + exp(1.49 + 0.05*iq100))

# phs = aidin lukion kaymisen tn
# logistinen malli: logit(phs) = B0 + B1*iq100 => phs = exp(B0 + B1*iq100) / (1 + exp(B0 + B1*iq100))


# 3)
invlogit <- function (x) { exp(x) / (1 + exp(x)) }
invlogit(b[1]) # ennustettu lukion kaymisen tn kun iq100 = 0 on 0.82


# 4)
# laskennallisesti:
b[2] * (-10/4)

# graafisesti:
iqs <- sort(unique(iq100))
pred1 <- invlogit(b[1] + iqs * b[2])
pred2 <- invlogit(b[1] + (iqs + 10) * b[2])
plot(iqs, pred2 - pred1)
# ero enintaan 0.14


# 5)
# f(x) on lukion kaymisen tn kun mom_iq = x
# f(x + 10) - f(x) =
invlogit(b[1] + (75 - 100)*b[2]) - invlogit(b[1] + (85-100)*b[2]) # -0.14, kun x = 75
invlogit(b[1] + (100 - 100)*b[2]) - invlogit(b[1] + (110-100)*b[2]) # -0.07, kun x = 100
invlogit(b[1] + (125 - 100)*b[2]) - invlogit(b[1] + (135-100)*b[2]) # -0.02, kun x = 125


# 6)
iqs <- seq(70, 130)
preds <- invlogit(b[1] + (iqs-90)*b[2]) - invlogit(b[1] + (iqs-100)*b[2])
plot(iqs, preds)


# 7)
b
exp(b[1]) # vedonlyontisuhde = 4.4, eli lukion kaymisen tn on 4.4-kertainen kaymattomyyteen verrattuna kun iq = 100
exp(b[2]) # kun verrataan aiteja joiden iq eroaa yhden numeron, ryhmassa jossa iq on suurempi vedonlyontisuhde (odds) on 1.06-kertainen (ts. 6% suurempi)
exp(b[2]*10) # kun verrataan aiteja joiden iq eroaa kymmenella, ryhmassa jossa iq on suurempi odds on 1.8-kertainen


# 8)
age23 <- mom_age - 23
glm2 <- glm(mom_hs ~ iq100 + age23, family = binomial(link='logit'))
summary(glm2)
b <- coef(glm2)
invlogit(b[1]) # ennustettu tn lukion kaymiselle kun mom_age=23 ja iq=100 on 0.84


# 9)
10*b[2]/4 # samanikaisille aideille joiden iq:t eroavat kymmenella pisteella, suurin ero lukion kaymisen tn:ssa on 0.15
b[3] / 4 # saman iq:n aideille joilla vuosi ikaeroa, suurin ero lukion kaymisen tn:ssa on 0.06


# 10)
exp(b[1]) # kun mom_age=23 ja mom_iq=100, tn lukion kaymiselle on 5.3-kertainen lukion kaymattomyydelle verrattuna
exp(b[2]) # kun mom_age=23 ja iq eroaa yhdella pisteella, tn lukion kaymiselle on 1.06-kertainen kaymattomyyteen verrattuna
exp(b[3]) # kun mom_iq=100 ja ika eroaa vuodella, tn lukion kaymiselle on 1.25-kertainen kaymattomyyteen verrattuna
