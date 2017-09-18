# Luento 5

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

