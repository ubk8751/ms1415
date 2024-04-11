library(gamlss) # LGAclaims dataset
library(Rcmdr)
library(stats)
library(forecast)
# Count data set
# fit Poisson model
LGAclaims
head(LGAclaims)
# number of third party claims,
# in a twelve month period between 1984-1986
# in each of 176 geographical areas
# (local government areas)
# in New South Wales, Australia
summary(LGAclaims)
sum(is.na(LGAclaims)) # checking if the data set
# has NA
# no NA in the data set
fit1 <- glm(LGAclaims$Claims ~ LGAclaims$Pop_density +
    LGAclaims$KI + LGAclaims$Accidents +
    LGAclaims$Population + LGAclaims$L_Population +
    LGAclaims$L_Accidents + LGAclaims$L_KI +
    LGAclaims$L_Popdensity, family = poisson)
summary(fit1)
influence.measures(fit1)
plot(fit1)
plot(fit1$residuals)
remove <- c(13, 34, 35)
fit2 <- glm(LGAclaims$Claims[-remove] ~
    LGAclaims$Pop_density[-remove] +
    LGAclaims$KI[-remove] +
    LGAclaims$Accidents[-remove] +
    LGAclaims$Population[-remove] +
    LGAclaims$L_Population[-remove] +
    LGAclaims$L_Accidents[-remove] +
    LGAclaims$L_KI[-remove] +
    LGAclaims$L_Popdensity[-remove], family = poisson)
plot(fit2$residuals)
plot(fit2)
influence.measures(fit2)
# fitting a Gaussian-based model
fit3 <- glm(LGAclaims$Claims ~ LGAclaims$Pop_density +
    LGAclaims$KI +
    LGAclaims$Accidents + LGAclaims$Population +
    LGAclaims$L_Population +
    LGAclaims$L_Accidents + LGAclaims$L_KI +
    LGAclaims$L_Popdensity, family = gaussian)
summary(fit3)
plot(fit3$residuals)
summary(stepwise(fit3))
fit4 <- glm(LGAclaims$Claims ~ LGAclaims$Accidents +
    LGAclaims$L_Accidents + LGAclaims$L_KI +
    LGAclaims$L_Popdensity, family = gaussian)
summary(fit4)
plot(fit4$residuals)
plot(fit4)
accuracy(fit1)
accuracy(fit2)
accuracy(fit3)
accuracy(fit4)
