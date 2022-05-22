


source("./modelSelection.R")

######### Simulations with Robust Standard Errors########

## Simulation Data
#Select the needed columns to na.omit()


attach(allData)
simData <- data.frame(anycoupatt_fin,reshuffleatt_fin,rechangeatt_fin,
                      gwf_legislature,personalism,gwf_leadermil,
                      ll_rgdpe_PW,lag_growth_rgdpe_PW ,ll_pop_PW, 
                      postcold,country,cowcode,year)
detach(allData)
simData <- na.omit(simData)


## Create country dummies for fixed effects
countries <- as.vector(as.character(unique(sort(simData$cowcode))))
Countrydummies <- matrix(0,nrow(simData),length(countries))
colnames(Countrydummies) <- countries 

for (i in 1:nrow(simData)) {
  
  for (n in 1:length(countries)) {
    
    if (simData$country[i] == countries[n]) {
      
      Countrydummies[i,countries[n]] <- 1
      
    }
  }
}

## Create Year Dummies For Fixed Effects
years <- as.vector(as.character(unique(sort(simData$year))))

Yeardummies <- matrix(0,nrow(simData),length(years))
colnames(Yeardummies) <- years

for (i in 1:nrow(simData)) {
  
  for (n in 1:length(years)) {
    
    if (simData$year[i] == years[n]) {
      
      Yeardummies[i,years[n]] <- 1
    }
  }
}

## Simulation data
simData <- data.frame(simData,Countrydummies,Yeardummies) 

## Main Models Penalized Logit with Fixed Effects

mainShuf <- brglm(reshuffleatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                  + ll_rgdpe_PW + lag_growth_rgdpe_PW + 
                    ll_pop_PW+ postcold + factor(cowcode)+factor(year), family=binomial(link="logit"),
                  method = "brglm.fit", p1 = T, data = simData)

##robust_se_mainShuf  <- sqrt(diag(vcovHC(mainShuf, type = "HC0")))


mainRech <- brglm(rechangeatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                  + ll_rgdpe_PW + lag_growth_rgdpe_PW + ll_pop_PW + postcold 
                  + factor(cowcode)+factor(year), family=binomial(link="logit"),
                  method = "brglm.fit", p1 = T, data = simData)

##robust_se_mainRech  <- sqrt(diag(vcovHC(mainRech, type = "HC0")))

## Within country means and standard deviations
means_personalism <- 
  aggregate(simData$personalism,
            by = list(simData$country),
            FUN = mean)
names(means_personalism) <- c("country","mean")
simData$mean_personalism <- means_personalism$mean[match(simData$country, means_personalism$country)]
simData$demeaned_personalism <- simData$personalism - simData$mean_personalism

mean_personalism <- mean(simData$personalism)
personalism_high <- mean_personalism + sd(simData$demeaned_personalism)
personalism_low <- mean_personalism - sd(simData$demeaned_personalism)

## Simulation For Reshuffling Coups
## First Difference
## Legislative - No Legislative

## independent variables
## Drop 2010 in year dummies
iv <- as.matrix(cbind(1,simData$gwf_legislature,simData$personalism,simData$gwf_leadermil,
                      simData$ll_rgdpe_PW,
                      simData$lag_growth_rgdpe_PW, simData$ll_pop_PW, simData$postcold,
                      simData[,15:(13 + ncol(Countrydummies))],simData[,(15+ncol(Countrydummies)):177],
                      simData$gwf_legislature*simData$personalism))

dv <- as.vector(simData$reshuffleatt_fin)

## Drop 2010* from coefficients and varcov
nsim <- 1000
coefficients <- mainShuf$coefficients
coefficients <- c(coefficients[1:170],coefficients[172])
## Robust Standard Errors
varcov <- vcovHC(mainShuf, type = "HC0")


## Scenario for Level of Personalism from 0 to 1
persoScenario <- seq(0,1,0.01)

## No legislation
casesperso <- array(NA, c(dim(iv), length(persoScenario)))

casesperso[, ,] <- iv

# No legislature
for (i in 1:length(persoScenario)) {
  casesperso[, 2, i] <- 0
}
# persoScenario
for (i in 1:length(persoScenario)) {
  casesperso[, 3, i] <- persoScenario[i]
}
# No legislature*persenalism
for (i in 1:length(persoScenario)) {
  casesperso[, 171, i] <- persoScenario[i]*0
}


valperso_Shuf <- matrix(NA, nrow = nsim, ncol = length(persoScenario))


for(i in 1:length(persoScenario)) {
  OVAperso <- sim_function(
    coefs = coefficients, 
    vcov = varcov,
    response_function = response_function,
    stochastic_component = stochastic_component,
    scenario = casesperso[,,i],
    predicted_values = F
  )
  
  tmp_valperso <- apply(OVAperso$ev, 1, mean)
  valperso_Shuf[, i] <- tmp_valperso
}


## With legislation
casesperso1 <- array(NA, c(dim(iv), length(persoScenario)))

casesperso1[, ,] <- iv

# Legislature
for (i in 1:length(persoScenario)) {
  casesperso1[, 2, i] <- 1
}
#persoScenario
for (i in 1:length(persoScenario)) {
  casesperso1[, 3, i] <- persoScenario[i]
}
# Legislature*personalism
for (i in 1:length(persoScenario)) {
  casesperso1[, 171, i] <- persoScenario[i]*1
}


## With Legislation
valperso1_Shuf <- matrix(NA, nrow = nsim, ncol = length(persoScenario))

for(i in 1:length(persoScenario)) {
  OVAperso1 <- sim_function(
    coefs = coefficients, 
    vcov = varcov,
    response_function = response_function,
    stochastic_component = stochastic_component,
    scenario = casesperso1[,,i],
    predicted_values = F
  )
  
  tmp_valperso1 <- apply(OVAperso1$ev, 1, mean)
  valperso1_Shuf[, i] <- tmp_valperso1
}

## Effect of Personalism Change when THere is a Legislature

dt1 <- data.frame(t(apply(valperso1_Shuf, 2, quantile, c(0.975, 0.025))))
dt1$mean <- apply(valperso1_Shuf, 2, mean)
names(dt1)[1:2] <- c("upper", "lower")


## Effect of Personalism Change when THere is No Legislature

dt2 <- data.frame(t(apply(valperso_Shuf, 2, quantile, c(0.975, 0.025))))
dt2$mean <- apply(valperso_Shuf, 2, mean)
names(dt2)[1:2] <- c("upper", "lower")

pdf(file = "Reshuffling_coups.pdf",width=10, height=5)
par(mfrow=c(1,2))
plot(
  persoScenario,
  dt1$mean,
  type = "n",
  ylim = c(0,1),
  ylab = "Predicted Probability of Reshuffling Coups",
  xlab = "Level of Personalism",
  main = "Effect of Personalism Change \n on Reshuffling Coups \n (Legislature)",
  cex.main = 0.8,
  bty = "n",
  las = 1
)

polygon(
  c(rev(persoScenario), persoScenario),
  c(rev(dt1$upper), dt1$lower),
  col = adjustcolor("navyblue", alpha = 0.5),
  border = NA
)

abline(h = 0,lty = "dashed")
abline(v = mean_personalism ,lty = "dashed")
abline(v = personalism_high,lty = "dashed", col= "red")
abline(v = personalism_low,lty = "dashed", col= "darkgreen")
lines(persoScenario, dt1$mean, lwd = 1)
lines(persoScenario, dt1$lower, lty = "dashed", col = "gray20")
lines(persoScenario, dt1$upper, lty = "dashed", col = "gray20")
par(new=TRUE)
hist(simData[which(simData$gwf_legislature == 1),]$personalism,axes=FALSE ,
     xlab="", ylab = "", main = "", border="lightblue", 
     col = adjustcolor("lightblue", alpha = 0.10) )


plot(
  persoScenario,
  dt2$mean,
  type = "n",
  ylim = c(0,1),
  ylab = "Predicted Probability of Reshuffling Coups",
  xlab = "Level of Personalism",
  main = "Effect of Personalism Change \n on Reshuffling Coups \n (No Legislature)",
  cex.main = 0.8,
  bty = "n",
  las = 1
)

polygon(
  c(rev(persoScenario), persoScenario),
  c(rev(dt2$upper), dt2$lower),
  col = adjustcolor("navyblue", alpha = 0.5),
  border = NA
)

abline(h = 0,lty = "dashed")
abline(v = mean_personalism,lty = "dashed")
abline(v = personalism_high,lty = "dashed", col= "red")
abline(v = personalism_low,lty = "dashed", col= "darkgreen")
lines(persoScenario, dt2$mean, lwd = 1)
lines(persoScenario, dt2$lower, lty = "dashed", col = "gray20")
lines(persoScenario, dt2$upper, lty = "dashed", col = "gray20")
par(new=TRUE)
hist(simData[which(simData$gwf_legislature == 0),]$personalism,axes=FALSE ,
     xlab="", ylab = "", main = "", border="lightblue", 
     col = adjustcolor("lightblue", alpha = 0.10) )
dev.off()



## First Difference With Legislation - No Legislation

fd1 <- valperso1_Shuf - valperso_Shuf
dt3 <- data.frame(t(apply(fd1, 2, quantile, c(0.975, 0.025))))
dt3$mean <- apply(fd1, 2, mean)
names(dt3)[1:2] <- c("upper", "lower")

pdf(file = "FD_reshuffling_coups.pdf")
plot(
  persoScenario,
  dt3$mean,
  type = "n",
  ylim = c(-0.30, 0.30),
  ylab = "First Difference of Predicted Probabilities",
  xlab = "Level of Personalism",
  main = "First Difference of Probability of Reshuffling Coup \n
          (Legislature Exist - No Legislature)",
  cex.main = 0.8,
  bty = "n",
  las = 1
)

polygon(
  c(rev(persoScenario), persoScenario),
  c(rev(dt3$upper), dt3$lower),
  col = adjustcolor("navyblue", alpha = 0.5),
  border = NA
)

abline(h = 0,lty = "dashed")
lines(persoScenario, dt3$mean, lwd = 1)
lines(persoScenario, dt3$lower, lty = "dashed", col = "gray20")
lines(persoScenario, dt3$upper, lty = "dashed", col = "gray20")
par(new=TRUE)
hist(simData$personalism,axes=FALSE , xlab="", ylab = "", main = "", border="lightblue", 
     col = adjustcolor("lightblue", alpha = 0.10) )
dev.off()


## In Lowest level of personalism
mean(fd1[,1])
quantile(fd1[,1],c(0.975, 0.025))

## First differences in case of a one within country standard deviation change in personalism
FDLegisShuf <- valperso1_Shuf[,26] - valperso1_Shuf[,44]
FDLegisShuf2 <- valperso1_Shuf[,44] - valperso1_Shuf[,61]


mean(FDLegisShuf)
mean(FDLegisShuf2)

quantile(FDLegisShuf,c(0.975, 0.025))
quantile(FDLegisShuf2,c(0.975, 0.025))


FDNoLegisShuf <- valperso_Shuf[,26] - valperso_Shuf[,44]
FDNoLegisShuf2 <- valperso_Shuf[,44] - valperso_Shuf[,61]

mean(FDNoLegisShuf)
mean(FDNoLegisShuf2)

quantile(FDNoLegisShuf,c(0.975, 0.025))
quantile(FDNoLegisShuf2,c(0.975, 0.025))


###################################################################

## Simulation For Regime-Change Coups
## First Difference
## Legislative - No Legislative

## Drop 2010* from coefficients and varcov
nsim <- 1000
coefficients <- mainRech$coefficients
coefficients <- c(coefficients[1:170],coefficients[172])
## Robust Standard Errors
varcov <- vcovHC(mainRech, type = "HC0")


## Scenario for Level of Personalism from 0 to 1
persoScenario <- seq(0,1,0.01)

## No legislation
casesperso <- array(NA, c(dim(iv), length(persoScenario)))

casesperso[, ,] <- iv

# No legislature
for (i in 1:length(persoScenario)) {
  casesperso[, 2, i] <- 0
}
# persoScenario
for (i in 1:length(persoScenario)) {
  casesperso[, 3, i] <- persoScenario[i]
}
# No legislature*persenalism
for (i in 1:length(persoScenario)) {
  casesperso[, 171, i] <- persoScenario[i]*0
}


valperso_Rech <- matrix(NA, nrow = nsim, ncol = length(persoScenario))


for(i in 1:length(persoScenario)) {
  OVAperso <- sim_function(
    coefs = coefficients, 
    vcov = varcov,
    response_function = response_function,
    stochastic_component = stochastic_component,
    scenario = casesperso[,,i],
    predicted_values = F
  )
  
  tmp_valperso <- apply(OVAperso$ev, 1, mean)
  valperso_Rech[, i] <- tmp_valperso
}


## With legislation
casesperso1 <- array(NA, c(dim(iv), length(persoScenario)))

casesperso1[, ,] <- iv

# Legislature
for (i in 1:length(persoScenario)) {
  casesperso1[, 2, i] <- 1
}
#persoScenario
for (i in 1:length(persoScenario)) {
  casesperso1[, 3, i] <- persoScenario[i]
}
# Legislature*personalism
for (i in 1:length(persoScenario)) {
  casesperso1[, 171, i] <- persoScenario[i]*1
}


## With Legislation
valperso1_Rech <- matrix(NA, nrow = nsim, ncol = length(persoScenario))

for(i in 1:length(persoScenario)) {
  OVAperso1 <- sim_function(
    coefs = coefficients, 
    vcov = varcov,
    response_function = response_function,
    stochastic_component = stochastic_component,
    scenario = casesperso1[,,i],
    predicted_values = F
  )
  
  tmp_valperso1 <- apply(OVAperso1$ev, 1, mean)
  valperso1_Rech[, i] <- tmp_valperso1
}

## Effect of Personalism Change on Regime Change Coups when There is a Legislature exist

dt4 <- data.frame(t(apply(valperso1_Rech, 2, quantile, c(0.975, 0.025))))
dt4$mean <- apply(valperso1_Rech, 2, mean)
names(dt4)[1:2] <- c("upper", "lower")


## Effect of Personalism Change on Regime Change Coups when There is No Legislature

dt5 <- data.frame(t(apply(valperso_Rech, 2, quantile, c(0.975, 0.025))))
dt5$mean <- apply(valperso_Rech, 2, mean)
names(dt5)[1:2] <- c("upper", "lower")


pdf(file = "Rechange_coups.pdf",width=10, height=5)
par(mfrow=c(1,2))
plot(
  persoScenario,
  dt4$mean,
  type = "n",
  ylim = c(0,1),
  ylab = "Predicted Probability of Regime-Change Coups",
  xlab = "Level of Personalism",
  main = "Effect of Personalism Change \n on Regime-Change Coups \n (Legislature)",
  cex.main = 0.8,
  bty = "n",
  las = 1
)

polygon(
  c(rev(persoScenario), persoScenario),
  c(rev(dt4$upper), dt4$lower),
  col = adjustcolor("navyblue", alpha = 0.5),
  border = NA
)

abline(h = 0,lty = "dashed")
abline(v = mean_personalism,lty = "dashed")
abline(v = personalism_high,lty = "dashed", col= "red")
abline(v = personalism_low,lty = "dashed", col= "darkgreen")
lines(persoScenario, dt4$mean, lwd = 1)
lines(persoScenario, dt4$lower, lty = "dashed", col = "gray20")
lines(persoScenario, dt4$upper, lty = "dashed", col = "gray20")
par(new=TRUE)
hist(simData[which(simData$gwf_legislature == 1),]$personalism,axes=FALSE ,
     xlab="", ylab = "", main = "", border="lightblue", 
     col = adjustcolor("lightblue", alpha = 0.10) )



plot(
  persoScenario,
  dt5$mean,
  type = "n",
  ylim = c(0,1),
  ylab = "Predicted Probability of Regime-Change Coups",
  xlab = "Level of Personalism",
  main = "Effect of Personalism Change \n on Regime-Change Coups \n (No Legislature)",
  cex.main = 0.8,
  bty = "n",
  las = 1
)

polygon(
  c(rev(persoScenario), persoScenario),
  c(rev(dt5$upper), dt5$lower),
  col = adjustcolor("navyblue", alpha = 0.5),
  border = NA
)

abline(h = 0,lty = "dashed")
abline(v = mean_personalism,lty = "dashed")
abline(v = personalism_high,lty = "dashed", col= "red")
abline(v = personalism_low,lty = "dashed", col= "darkgreen")
lines(persoScenario, dt5$mean, lwd = 1)
lines(persoScenario, dt5$lower, lty = "dashed", col = "gray20")
lines(persoScenario, dt5$upper, lty = "dashed", col = "gray20")
par(new=TRUE)
hist(simData[which(simData$gwf_legislature == 0),]$personalism,axes=FALSE ,
     xlab="", ylab = "", main = "", border="lightblue", 
     col = adjustcolor("lightblue", alpha = 0.10) )
dev.off()



## First Difference With Legislation - No Legislation

fd2 <- valperso1_Rech - valperso_Rech
dt6 <- data.frame(t(apply(fd2, 2, quantile, c(0.975, 0.025))))
dt6$mean <- apply(fd2, 2, mean)
names(dt6)[1:2] <- c("upper", "lower")

pdf(file = "FD_regimechange_coups.pdf")
plot(
  persoScenario,
  dt6$mean,
  type = "n",
  ylim = c(-0.30, 0.30),
  ylab = "First Difference of Predicted Probabilities",
  xlab = "Level of Personalism",
  main = "First Difference of Probability of Regime-Change Coups \n
          (Legislature Exist - No Legislature)",
  cex.main = 0.8,
  bty = "n",
  las = 1
)

polygon(
  c(rev(persoScenario), persoScenario),
  c(rev(dt6$upper), dt6$lower),
  col = adjustcolor("navyblue", alpha = 0.5),
  border = NA
)

abline(h = 0,lty = "dashed")
lines(persoScenario, dt6$mean, lwd = 1)
lines(persoScenario, dt6$lower, lty = "dashed", col = "gray20")
lines(persoScenario, dt6$upper, lty = "dashed", col = "gray20")
par(new=TRUE)
hist(simData$personalism,axes=FALSE , xlab="", ylab = "", main = "", border="lightblue", 
     col = adjustcolor("lightblue", alpha = 0.10) )
dev.off()

## First differences in case of a one within country standard deviation change in personalism
FDLegisRech <- valperso1_Rech[,26] - valperso1_Rech[,44]
FDLegisRech2 <- valperso1_Rech[,44] - valperso1_Rech[,61]

mean(FDLegisRech)
mean(FDLegisRech2)

quantile(FDLegisRech,c(0.975, 0.025))
quantile(FDLegisRech2,c(0.975, 0.025))


FDNoLegisRech <- valperso_Rech[,26] - valperso_Rech[,44]
FDNoLegisRech2 <- valperso_Rech[,44] - valperso_Rech[,61]

mean(FDNoLegisRech)
mean(FDNoLegisRech2)

quantile(FDNoLegisRech,c(0.975, 0.025))
quantile(FDNoLegisRech2,c(0.975, 0.025))

