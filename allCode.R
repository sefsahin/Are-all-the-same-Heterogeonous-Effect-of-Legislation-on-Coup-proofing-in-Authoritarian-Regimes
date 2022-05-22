

## Some needed packages and libraries
rm(list=ls())

p_needed <- c("knitr", "MASS", "separationplot", "ggplot2", "reshape2", "ggthemes","brglm","readr","haven",
              "MASS","lmtest","sandwich","stargazer","lme4","tidyverse","Zelig","sjstats","countrycode","dplyr","tidyr",
              "stringr","plyr","heatmapFit","plotROC","pROC","gridExtra")

packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
sapply(p_needed, require, character.only = TRUE)


## Some needed functions for future use. 

## Function for summary table. 

summaryTable <- function (x) {
  
  var <- na.omit(x)
  obs <- length(var)
  mean <- mean(var)
  std <- sd(var)
  min <- min(var)
  max <- max(var)
  median <- median(var)
  
  return(round(c(obs,mean,std,min,max,median),3))
}

## A function to get the mean of personalism level of each group in grouped data.

getMean <- function (x) {mean(x[,"personalism"])}



## Functions for simulations. 
response_function <- function(x) {
  1 / (1 + exp(-x))
}
stochastic_component <- function(ndraws, p) {
  rbinom(n = ndraws, size = 1, prob = p)
}
sim_function <-
  function(seed = 17,
           nsim = 1000,
           coefs,
           vcov,
           scenario,
           response_function,
           predicted_values = F,
           stochastic_component) {
    if (is.null(dim(scenario))) {
      stop("The scenario needs to be in a matrix.")
    }
    if (length(coefs) != ncol(scenario)) {
      stop("The scenario and the parameter vector don’t fit.")
    }
    set.seed(seed)
    # Set up the sampling distribution
    S <- mvrnorm(nsim, coefs, vcov)
    # Calculate mu and p
    # The linear component
    mu <- S %*% t(scenario)
    # The response function
    ev <- response_function(mu)
    if (predicted_values) {
      pv <-
        array(stochastic_component(ndraws = prod(dim(ev)),
                                   p = ev), dim = dim(ev))
      return(list(ev = ev, pv = pv))
    }
    return(list(ev = ev))
  }


## Load Data
# Assuming the data is in the same directory with the script.

allData <- read_csv("./replicationData.csv")

## Summary Table 

SumTable <- rbind(summaryTable(allData$anycoupatt_fin),
                  summaryTable(allData$rechangeatt_fin),
                  summaryTable(allData$reshuffleatt_fin),
                  summaryTable(allData$gwf_legislature),
                  summaryTable(allData$personalism),
                  summaryTable(allData$gwf_leadermil),
                  summaryTable(allData$ll_rgdpe_PW),
                  summaryTable(allData$lag_growth_rgdpe_PW),
                  summaryTable(allData$ll_pop_PW),
                  summaryTable(allData$postcold))

## row and column names for the summary table.

colnames(SumTable) <- c("Obs.","Mean","Std.Dev.","Min","Max","Median")
rownames(SumTable) <- c("Any coup attempt",
                        "Reshuffling coup attempt",
                        "Regime-change coup attempt",
                        "Legislature",
                        "Personalism",
                        "Military leader",
                        "ln(GDP/capita)",
                        "Economic Growth",
                        "ln(Population)",
                        "Post-Cold War")


## You can find the summary table in the Appendix Table 1 
# or comment out the name below to see it in R.

## Show Table
SumTable

## Appendix A1
## For Latex
## kable(SumTable,digits = 3, caption = "Summary Statistics", booktabs = T, format = "latex")

### Personalism Through Years, Figure 1 Main Article. 

fig1Data <- ddply(allData, .(region,year),getMean)

colnames(fig1Data) <- c("Regions","Years","Personalism")

ggplot(data=fig1Data, aes(x=Years, y=Personalism, color=Regions)) +
  geom_line() + labs(title = 'Mean Personalism Levels Through Years') + xlab("Years") +
  ylab("Mean Level of Personalism") + theme_minimal()  + theme(legend.position="bottom")

ggsave("./Figure2(Personalism).png", width = 20, height = 10, units = "cm")

### Reshuffle and Regime-Change Coups Through Years


fig2Data <- allData[which(allData$reshuffleatt_fin >0),c("year","personalism","reshuffleatt_fin","gwf_legislature")]

fig2Data$Legislature <- as.factor(fig2Data$gwf_legislature)


Shuffle <- ggplot(data=fig2Data, aes(x=year, y=personalism, color=Legislature)) +
  geom_point(shape=15) + geom_hline(yintercept=0.42, linetype = "dashed") + theme_minimal() + 
  ylab("Level of Personalism") +
  scale_colour_manual(labels = c("No Legislature", "Legislature"), values = c("#F8766D","#619CFF")) +
  ggtitle("Reshuffling Coups Through Years") +
  theme(legend.position="bottom")

##ggsave("./Figure2(Reshuffle-Coups).png", width = 20, height = 10, units = "cm")



## Regime-Change Coups Through Years
fig3Data <- allData[which(allData$rechangeatt_fin >0),c("year","personalism","rechangeatt_fin","gwf_legislature")]

fig3Data$Legislature <- as.factor(fig3Data$gwf_legislature)

ReChange <- ggplot(data=fig3Data, aes(x=year, y=personalism, color=Legislature)) +
  geom_point(shape=17) + geom_hline(yintercept=0.42, linetype = "dashed") + theme_minimal() + 
  ylab("Level of Personalism") +
  scale_colour_manual(labels = c("No Legislature", "Legislature"), values = c("#F8766D","#619CFF")) +
  ggtitle("Regime-Change Coups Through Years") +
  theme(legend.position="bottom")

##ggsave("./Figure3(Regime-ChangeCoups).png", width = 20, height = 10, units = "cm")

pdf("./Figure1(Coups).pdf",width=8, height=5)
grid.arrange(Shuffle,ReChange, nrow = 2)
dev.off()


## Some Percentages

# Percentage of Regime Change and Reshuffling coups against all coups
nrow(fig2Data[,"reshuffleatt_fin"])/(nrow(fig2Data[,"reshuffleatt_fin"])+nrow(fig3Data[,"rechangeatt_fin"]))*100
nrow(fig3Data[,"rechangeatt_fin"])/(nrow(fig2Data[,"reshuffleatt_fin"])+nrow(fig3Data[,"rechangeatt_fin"]))*100

# Percentage of country years with Legislative
nrow(allData[which(allData$gwf_legislature == 1),"gwf_legislature"])/nrow(allData[,"gwf_legislature"])*100

# Percentage of Reshuffling Coups that occur in a country year that has lower than median personalism level
nrow(fig2Data[which(fig2Data$personalism <= 0.42),
              "reshuffleatt_fin"])/nrow(fig2Data[,"reshuffleatt_fin"])*100

# Percentage of Reshuffling Coups that occur in a country year that has a Legislature
nrow(fig2Data[which(fig2Data$gwf_legislature == 1),
              "reshuffleatt_fin"])/nrow(fig2Data[,"reshuffleatt_fin"])*100

# Share of Regime Change Coups that occur in a country year that has lower than median personalism level
nrow(fig3Data[which(fig3Data$personalism <= 0.42),
              "rechangeatt_fin"])/nrow(fig3Data[,"rechangeatt_fin"])*100

# Share of Regime Coups that occur in a country year that has lower than median personalism level 
# and has a Legislature
nrow(fig3Data[which(fig3Data$personalism <= 0.42 & fig3Data$gwf_legislature == 1),
              "rechangeatt_fin"])/nrow(fig3Data[which(fig3Data$personalism <= 0.42),
                                                "rechangeatt_fin"])*100
##### Models #####

## First baseline models
## All are logit models.
## Main variables + Control variables 
## No Fixed effects
## Include Cubic times past since the last coup/type of coup. 


## All coups
## Without differentiating between coup types.

## Without Personalism
modelAll <-glm(anycoupatt_fin ~ gwf_legislature + gwf_leadermil 
               + ll_rgdpe_PW + lag_growth_rgdpe_PW + ll_pop_PW 
               + postcold+anyattyrs+anyattyrs2+anyattyrs3, data=allData, family = binomial(link='logit'))

##summary(modelAll)

## With Personalism and Interaction term between legislature and Personalism
modelAll_perso <-glm(anycoupatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                     + ll_rgdpe_PW +lag_growth_rgdpe_PW + ll_pop_PW 
                     + postcold+anyattyrs+anyattyrs2+anyattyrs3, data=allData, family = binomial(link='logit'))

##summary(modelAll_perso)

## Likelihood Test
## let's check whether addition of interaction improves model fit or not
L1 <- logLik(modelAll) # Log Likelihood of model 1  -- restricted
L2 <- logLik(modelAll_perso) # Log Likelihood of model 2 --- unrestricted(full)
LRT <- -2 * L1 + 2 * L2 # converges to chi^2 distribution
# Reject H0 if ... What is our H0 here?

LRT > qchisq(0.95, df = 2)
## The model fit is improved with the interaction term.



## Reshuffling Coups
## Differentiating between coup types.

modelShuf <-glm(reshuffleatt_fin ~ gwf_legislature + gwf_leadermil 
                + ll_rgdpe_PW + lag_growth_rgdpe_PW + 
                  ll_pop_PW+ postcold + shuffattyrs + shuffattyrs2 
                + shuffattyrs3, data=allData, family = binomial(link='logit'))

##summary(modelShuf)

modelShuf_perso <-glm(reshuffleatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                      + ll_rgdpe_PW + lag_growth_rgdpe_PW + 
                        ll_pop_PW+ postcold + shuffattyrs + shuffattyrs2 
                      + shuffattyrs3, data=allData, family = binomial(link='logit'))

##summary(modelShuf_perso)

## Likelihood Test
## let's check whether addition of interaction improves model fit or not
L1 <- logLik(modelShuf) # Log Likelihood of model 1  -- restricted
L2 <- logLik(modelShuf_perso) # Log Likelihood of model 2 --- unrestricted(full)
LRT <- -2 * L1 + 2 * L2 # converges to chi^2 distribution
# Reject H0 if ... What is our H0 here?

LRT > qchisq(0.95, df = 2)
## The model fit is improved with the interaction term.



## Regime-Change Coups
## Differentiating between coup types.


modelRech <-glm(rechangeatt_fin ~ gwf_legislature + gwf_leadermil 
                + ll_rgdpe_PW +lag_growth_rgdpe_PW + ll_pop_PW + 
                  postcold + rechattyrs + rechattyrs2 + rechattyrs3, data=allData,
                family = binomial(link='logit'))

##summary(modelRech)

modelRech_perso <-glm(rechangeatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                      + ll_rgdpe_PW + lag_growth_rgdpe_PW + ll_pop_PW + postcold 
                      + rechattyrs + rechattyrs2 + rechattyrs3, data=allData, family = binomial(link='logit'))


##summary(modelRech_perso)

## Likelihood Test
## let's check whether addition of interaction improves model fit or not
L1 <- logLik(modelShuf) # Log Likelihood of model 1  -- restricted
L2 <- logLik(modelShuf_perso) # Log Likelihood of model 2 --- unrestricted(full)
LRT <- -2 * L1 + 2 * L2 # converges to chi^2 distribution
# Reject H0 if ... What is our H0 here?

LRT > qchisq(0.95, df = 2)
## The model fit is improved with the interaction term.


##### Main Models #####

## Now Main Models
## All are penalized logit models.
## Main variables + Control variables + Fixed effects
## Cook et al. 2020 (Fixed effects in rare events data: a penalized maximum likelihood solution)


## All coups
## Without differentiating between coup types.

## Without Personalism
modelAll_pen <-brglm(anycoupatt_fin ~ gwf_legislature + gwf_leadermil 
                     + ll_rgdpe_PW + lag_growth_rgdpe_PW + ll_pop_PW 
                     + postcold+factor(cowcode)+factor(year), family=binomial(link="logit"),
                     method = "brglm.fit", p1 = T, data = allData)

##summary(modelAll_pen)

## With Personalism and Interaction term between legislature and Personalism
modelAll_perso_pen <-brglm(anycoupatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                           + ll_rgdpe_PW +lag_growth_rgdpe_PW + ll_pop_PW 
                           + postcold + factor(cowcode)+factor(year), family=binomial(link="logit"),
                           method = "brglm.fit", p1 = T, data = allData)

##summary(modelAll_perso_pen)

## Likelihood Test
## let's check whether addition of interaction improves model fit or not
L1 <- logLik(modelAll) # Log Likelihood of model 1  -- restricted
L2 <- logLik(modelAll_perso) # Log Likelihood of model 2 --- unrestricted(full)
LRT <- -2 * L1 + 2 * L2 # converges to chi^2 distribution
# Reject H0 if ... What is our H0 here?

LRT > qchisq(0.95, df = 2)
## The model fit is improved with the interaction term.



## Reshuffling Coups
## Differentiating between coup types.

modelShuf_pen <-brglm(reshuffleatt_fin ~ gwf_legislature + gwf_leadermil 
                      + ll_rgdpe_PW + lag_growth_rgdpe_PW + 
                        ll_pop_PW+ postcold + factor(cowcode)+factor(year), family=binomial(link="logit"),
                      method = "brglm.fit", p1 = T, data = allData)

##summary(modelShuf_pen)

modelShuf_perso_pen <-brglm(reshuffleatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                            + ll_rgdpe_PW + lag_growth_rgdpe_PW + 
                              ll_pop_PW+ postcold + factor(cowcode)+factor(year), family=binomial(link="logit"),
                            method = "brglm.fit", p1 = T, data = allData)

##summary(modelShuf_perso_pen)

## Likelihood Test
## let's check whether addition of interaction improves model fit or not
L1 <- logLik(modelShuf) # Log Likelihood of model 1  -- restricted
L2 <- logLik(modelShuf_perso) # Log Likelihood of model 2 --- unrestricted(full)
LRT <- -2 * L1 + 2 * L2 # converges to chi^2 distribution
# Reject H0 if ... What is our H0 here?

LRT > qchisq(0.95, df = 2)
## The model fit is improved with the interaction term.



## Regime-Change Coups
## Differentiating between coup types.

modelRech_pen <-brglm(rechangeatt_fin ~ gwf_legislature + gwf_leadermil 
                      + ll_rgdpe_PW +lag_growth_rgdpe_PW + ll_pop_PW + 
                        postcold + factor(cowcode)+factor(year), family=binomial(link="logit"),
                      method = "brglm.fit", p1 = T, data = allData)

##summary(modelRech_pen)

modelRech_perso_pen <-brglm(rechangeatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                            + ll_rgdpe_PW + lag_growth_rgdpe_PW + ll_pop_PW + postcold 
                            + factor(cowcode)+factor(year), family=binomial(link="logit"),
                            method = "brglm.fit", p1 = T, data = allData)


##summary(modelRech_perso_pen)

## Likelihood Test
## let's check whether addition of interaction improves model fit or not
L1 <- logLik(modelShuf) # Log Likelihood of model 1  -- restricted
L2 <- logLik(modelShuf_perso) # Log Likelihood of model 2 --- unrestricted(full)
LRT <- -2 * L1 + 2 * L2 # converges to chi^2 distribution
# Reject H0 if ... What is our H0 here?

LRT > qchisq(0.95, df = 2)
## The model fit is improved with the interaction term.


## Further Comparison Between Models
## Baseline models vs Main Models (Penalized Logit with Fixed Effects)


## Reshuffling Coups

## Seperation Plots

## modelShuf
separationplot(pred=modelShuf$fitted.values, actual=modelShuf$y,
               line=TRUE, show.expected=TRUE, heading="Reshuffling Coups without Personalism and Fixed Effects")

## modelShuf_perso
separationplot(pred=modelShuf_perso$fitted.values, actual=modelShuf_perso$y,
               line=TRUE, show.expected=TRUE, heading="Reshuffling Coups with Personalism without Fixed Effects")

## modelShuf_pen
separationplot(pred=modelShuf_pen$fitted.values, actual=modelShuf_pen$y,
               line=TRUE, show.expected=TRUE, heading="Reshuffling Coups without Personalism with Fixed Effects")

## modelShuf_perso_pen
separationplot(pred=modelShuf_perso_pen$fitted.values, actual=modelShuf_perso_pen$y,
               line=TRUE, show.expected=TRUE, heading="Reshuffling Coups with Personalism and Fixed Effects")


## Appendix A15 
pdf("./shufSepPlots.pdf")
par(mfrow=c(4,1))
separationplot(pred=modelShuf$fitted.values, actual=modelShuf$y,
               line=TRUE, show.expected=TRUE, heading="Reshuffling Coups without Personalism and Fixed Effects")
separationplot(pred=modelShuf_perso$fitted.values, actual=modelShuf_perso$y,
               line=TRUE, show.expected=TRUE, heading="Reshuffling Coups with Personalism without Fixed Effects")
separationplot(pred=modelShuf_pen$fitted.values, actual=modelShuf_pen$y,
               line=TRUE, show.expected=TRUE, heading="Reshuffling Coups without Personalism with Fixed Effects")
separationplot(pred=modelShuf_perso_pen$fitted.values, actual=modelShuf_perso_pen$y,
               line=TRUE, show.expected=TRUE, heading="Reshuffling Coups with Personalism and Fixed Effects")
dev.off()


## ROC Plots 

shufRocdata <- data.frame(coups= modelShuf$y, 
                          modelShuf = modelShuf$fitted.values, 
                          modelShuf_perso = modelShuf_perso$fitted.values,
                          Shuf_pen = modelShuf_pen$fitted.values,
                          Shuf_perso_pen = modelShuf_perso_pen$fitted.values)
longtestShuf <- melt_roc(shufRocdata, "coups", c("modelShuf", "modelShuf_perso", "Shuf_pen","Shuf_perso_pen"))


## Appendix A12
pdf("reshuffleROC.pdf")
ggplot(longtestShuf, aes(d = D, m = M, color = name)) + geom_roc() + style_roc()
dev.off()

## Heatmaps

## modelShuf
pred <- predict(modelShuf, type="response")
heatmap.fit(modelShuf$y, pred, reps=1000)

## modelShuf_perso
pred2 <- predict(modelShuf_perso, type="response")
heatmap.fit(modelShuf_perso$y, pred2, reps=1000)

## modelShuf_pen
pred3 <- predict(modelShuf_pen, type="response")
heatmap.fit(modelShuf_pen$y, pred3, reps=1000)

## modelShuf_perso_pen
pred4 <- predict(modelShuf_perso_pen, type="response")
heatmap.fit(modelShuf_perso_pen$y, pred4, reps=1000)


## Appendix A13
pdf("reshuffleHeatmaps1.pdf")
heatmap.fit(modelShuf$y, pred, reps=1000)
dev.off()

pdf("reshuffleHeatmaps2.pdf")
heatmap.fit(modelShuf_perso$y, pred2, reps=1000)
dev.off()

pdf("reshuffleHeatmaps3.pdf")
heatmap.fit(modelShuf_pen$y, pred3, reps=1000)
dev.off()

pdf("reshuffleHeatmaps4.pdf")
heatmap.fit(modelShuf_perso_pen$y, pred4, reps=1000)
dev.off()



##############################################

## Regime-Change Coups

## Seperation Plots

## modelRech
separationplot(pred=modelRech$fitted.values, actual=modelRech$y,
               line=TRUE, show.expected=TRUE, heading="Regime-Change Coups without Personalism and Fixed Effects")

## modelRech_perso
separationplot(pred=modelRech_perso$fitted.values, actual=modelRech_perso$y,
               line=TRUE, show.expected=TRUE, heading="Regime-Change Coups with Personalism without Fixed Effects")

## modelRech_pen
separationplot(pred=modelRech_pen$fitted.values, actual=modelRech_pen$y,
               line=TRUE, show.expected=TRUE, heading="Regime-Change Coups without Personalism with Fixed Effects")

## modelRech_perso_pen
separationplot(pred=modelRech_perso_pen$fitted.values, actual=modelRech_perso_pen$y,
               line=TRUE, show.expected=TRUE, heading="Regime-Change Coups with Personalism and Fixed Effects")


## Appendix A16 
pdf("./RechSepPlots.pdf")
par(mfrow=c(4,1))
separationplot(pred=modelRech$fitted.values, actual=modelRech$y,
               line=TRUE, show.expected=TRUE, heading="Regime-Change Coups without Personalism and Fixed Effects")
separationplot(pred=modelRech_perso$fitted.values, actual=modelRech_perso$y,
               line=TRUE, show.expected=TRUE, heading="Regime-Change Coups with Personalism without Fixed Effects")
separationplot(pred=modelRech_pen$fitted.values, actual=modelRech_pen$y,
               line=TRUE, show.expected=TRUE, heading="Regime-Change Coups without Personalism with Fixed Effects")
separationplot(pred=modelRech_perso_pen$fitted.values, actual=modelRech_perso_pen$y,
               line=TRUE, show.expected=TRUE, heading="Regime-Change Coups with Personalism and Fixed Effects")

dev.off()


## ROC Plots 

RechRocdata <- data.frame(coups= modelRech$y, 
                          modelRech = modelRech$fitted.values, 
                          modelRech_perso = modelRech_perso$fitted.values,
                          Rech_pen = modelRech_pen$fitted.values,
                          Rech_perso_pen = modelRech_perso_pen$fitted.values)
longtestRech <- melt_roc(RechRocdata, "coups", c("modelRech", "modelRech_perso", "Rech_pen","Rech_perso_pen"))

## Appendix A12
pdf("RechangeROC.pdf")
ggplot(longtestRech, aes(d = D, m = M, color = name)) + geom_roc() + style_roc()
dev.off()

## Heatmaps

## modelRech
pred <- predict(modelRech, type="response")
heatmap.fit(modelRech$y, pred, reps=1000)

## modelRech_perso
pred2 <- predict(modelRech_perso, type="response")
heatmap.fit(modelRech_perso$y, pred2, reps=1000)

## modelRech_pen
pred3 <- predict(modelRech_pen, type="response")
heatmap.fit(modelRech_pen$y, pred3, reps=1000)

## modelRech_perso_pen
pred4 <- predict(modelRech_perso_pen, type="response")
heatmap.fit(modelRech_perso_pen$y, pred4, reps=1000)


## Appendix A14
pdf("reRechfleHeatmaps1.pdf")
heatmap.fit(modelRech$y, pred, reps=1000)
dev.off()

pdf("reRechfleHeatmaps2.pdf")
heatmap.fit(modelRech_perso$y, pred2, reps=1000)
dev.off()

pdf("reRechfleHeatmaps3.pdf")
heatmap.fit(modelRech_pen$y, pred3, reps=1000)
dev.off()

pdf("reRechfleHeatmaps4.pdf")
heatmap.fit(modelRech_perso_pen$y, pred4, reps=1000)
dev.off()


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


## Robustness Checks

## Coefficient Table for Main Models (Penalized Logit with Fixed Effects) / Appendix A2

modelAll_pen <-brglm(anycoupatt_fin ~ gwf_legislature + gwf_leadermil 
                     + ll_rgdpe_PW + lag_growth_rgdpe_PW + ll_pop_PW 
                     + postcold+factor(cowcode)+factor(year), family=binomial(link="logit"),
                     method = "brglm.fit", p1 = T, data = allData)

modelAll_perso_pen <-brglm(anycoupatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                           + ll_rgdpe_PW +lag_growth_rgdpe_PW + ll_pop_PW 
                           + postcold + factor(cowcode)+factor(year), family=binomial(link="logit"),
                           method = "brglm.fit", p1 = T, data = allData)

modelShuf_pen <-brglm(reshuffleatt_fin ~ gwf_legislature + gwf_leadermil 
                      + ll_rgdpe_PW + lag_growth_rgdpe_PW + 
                        ll_pop_PW+ postcold + factor(cowcode)+factor(year), family=binomial(link="logit"),
                      method = "brglm.fit", p1 = T, data = allData)

modelShuf_perso_pen <- brglm(reshuffleatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                             + ll_rgdpe_PW + lag_growth_rgdpe_PW + 
                               ll_pop_PW+ postcold + factor(cowcode)+factor(year), family=binomial(link="logit"),
                             method = "brglm.fit", p1 = T, data = allData)


modelRech_pen <-brglm(rechangeatt_fin ~ gwf_legislature + gwf_leadermil 
                      + ll_rgdpe_PW +lag_growth_rgdpe_PW + ll_pop_PW + 
                        postcold + factor(cowcode)+factor(year), family=binomial(link="logit"),
                      method = "brglm.fit", p1 = T, data = allData)


modelRech_perso_pen <-brglm(rechangeatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                            + ll_rgdpe_PW + lag_growth_rgdpe_PW + ll_pop_PW + postcold 
                            + factor(cowcode)+factor(year), family=binomial(link="logit"),
                            method = "brglm.fit", p1 = T, data = allData)

stargazer(modelAll_pen,modelAll_perso_pen,modelShuf_pen,modelShuf_perso_pen,modelRech_pen,modelRech_perso_pen)

## Coefficient Table for Main Models with Robust Standard Errors(Penalized Logit with Fixed Effects) / Appendix A3

modelShuf_perso_pen_robust <- sqrt(diag(vcovHC(modelShuf_perso_pen, type = "HC0")))
modelRech_perso_pen_robust <- sqrt(diag(vcovHC(modelRech_perso_pen, type = "HC0")))
## Robust standard errors (change by hand from above calculation)
stargazer(modelShuf_perso_pen,modelRech_perso_pen)

## Coefficient Table for Baseline Models (Logit Models without Fixed Effects) / Appendix A4

modelAll <-glm(anycoupatt_fin ~ gwf_legislature + gwf_leadermil 
               + ll_rgdpe_PW + lag_growth_rgdpe_PW + ll_pop_PW 
               + postcold+anyattyrs+anyattyrs2+anyattyrs3, data=allData, family = binomial(link='logit'))

modelAll_perso <-glm(anycoupatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                     + ll_rgdpe_PW +lag_growth_rgdpe_PW + ll_pop_PW 
                     + postcold+anyattyrs+anyattyrs2+anyattyrs3, data=allData, family = binomial(link='logit'))

modelShuf <-glm(reshuffleatt_fin ~ gwf_legislature + gwf_leadermil 
                + ll_rgdpe_PW + lag_growth_rgdpe_PW + 
                  ll_pop_PW+ postcold + shuffattyrs + shuffattyrs2 
                + shuffattyrs3, data=allData, family = binomial(link='logit'))

modelShuf_perso <-glm(reshuffleatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                      + ll_rgdpe_PW + lag_growth_rgdpe_PW + 
                        ll_pop_PW+ postcold + shuffattyrs + shuffattyrs2 
                      + shuffattyrs3, data=allData, family = binomial(link='logit'))

modelRech <-glm(rechangeatt_fin ~ gwf_legislature + gwf_leadermil 
                + ll_rgdpe_PW +lag_growth_rgdpe_PW + ll_pop_PW + 
                  postcold + rechattyrs + rechattyrs2 + rechattyrs3, data=allData,
                family = binomial(link='logit'))

modelRech_perso <-glm(rechangeatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                      + ll_rgdpe_PW + lag_growth_rgdpe_PW + ll_pop_PW + postcold 
                      + rechattyrs + rechattyrs2 + rechattyrs3, data=allData, family = binomial(link='logit'))

stargazer(modelAll,modelAll_perso,modelShuf,modelShuf_perso,modelRech,modelRech_perso)

## Coefficient Table for Other Model Specifications (Different Model Specifications) 

## Mixed level model Appendix A5
modelShuf_mixed  <- glmer(reshuffleatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                          + ll_rgdpe_PW + lag_growth_rgdpe_PW + 
                            ll_pop_PW+ postcold + shuffattyrs + shuffattyrs2 
                          + shuffattyrs3+(1 | country),
                          data=allData, family=binomial, nAGQ = 0)

modelRech_mixed  <- glmer(rechangeatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                          + ll_rgdpe_PW + lag_growth_rgdpe_PW + ll_pop_PW + postcold 
                          + rechattyrs + rechattyrs2 + rechattyrs3 +(1 | country),
                          data=allData, family=binomial, nAGQ = 0)

## Rare Event Logit Models Appendix A6
modelShuf_rare <- zelig(reshuffleatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                        + ll_rgdpe_PW + lag_growth_rgdpe_PW + 
                          ll_pop_PW+ postcold,
                        data = simData, model = "relogit", 
                        tau = sum(simData$reshuffleatt_fin)/length(simData$reshuffleatt_fin))

modelRech_rare <- zelig(rechangeatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                        + ll_rgdpe_PW + lag_growth_rgdpe_PW + 
                          ll_pop_PW+ postcold,
                        data = simData, model = "relogit", 
                        tau = sum(simData$rechangeatt_fin)/length(simData$rechangeatt_fin))

stargazer(modelShuf_mixed ,modelRech_mixed)
stargazer(from_zelig_model(modelShuf_rare),from_zelig_model(modelRech_rare))

## Coefficient Table for Models with No Controls / Appendix A7

modelShuf_perso_pen_nc <- brglm(reshuffleatt_fin ~ gwf_legislature*personalism 
                                , family=binomial(link="logit"),
                                method = "brglm.fit", p1 = T, data = allData)


modelRech_perso_pen_nc <- brglm(rechangeatt_fin ~ gwf_legislature*personalism  
                                , family=binomial(link="logit"),
                                method = "brglm.fit", p1 = T, data = allData)

stargazer(modelShuf_perso_pen_nc , modelRech_perso_pen_nc)


## Coefficient Table for Models with Coup Success as Dependent Variable / Appendix A8

modelShuf_success <-brglm(reshufflesucc_fin ~ gwf_legislature*personalism + gwf_leadermil 
                          + ll_rgdpe_PW + lag_growth_rgdpe_PW + 
                            ll_pop_PW+ postcold + factor(cowcode)+factor(year), family=binomial(link="logit"),
                          method = "brglm.fit", p1 = T, data = allData)


modelRech_success <-brglm(allData$rechangesucc_fin ~ gwf_legislature*personalism + gwf_leadermil 
                          + ll_rgdpe_PW + lag_growth_rgdpe_PW + ll_pop_PW + postcold 
                          + factor(cowcode)+factor(year), family=binomial(link="logit"),
                          method = "brglm.fit", p1 = T, data = allData)

stargazer(modelShuf_success,modelRech_success)


## Coefficient Table for Main models when Leaders First Two Years Dropped / Appendix A9

model3years_Shuf <- brglm(reshuffleatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                          + ll_rgdpe_PW + lag_growth_rgdpe_PW + 
                            ll_pop_PW+ postcold + factor(cowcode)+factor(year), family=binomial(link="logit"),
                          method = "brglm.fit", p1 = T,
                          data = allData[which(allData$leader_duration >= 3),])

model3years_Rech <-  brglm(rechangeatt_fin ~ gwf_legislature*personalism + gwf_leadermil 
                           + ll_rgdpe_PW + lag_growth_rgdpe_PW 
                           + ll_pop_PW + postcold 
                           + factor(cowcode) + factor(year), family=binomial(link="logit"),
                           method = "brglm.fit", p1 = T,
                           data = allData[which(allData$leader_duration >= 3),])

stargazer(model3years_Shuf,model3years_Rech)


## Coefficient Table for Cross Validation of the results (with Penalized Logit Model) / Appendix A10

## For Reshuffle Coups with Robust Standard Errors
X_Shuf <- data.frame(cbind(allData$reshuffleatt_fin,allData$gwf_legislature,allData$personalism,
                           allData$gwf_leadermil,allData$ll_rgdpe_PW,allData$lag_growth_rgdpe_PW,
                           allData$ll_pop_PW,allData$postcold,allData$cowcode,allData$year))


set.seed(17)
m <- 10
beta_res <- matrix(NA, nrow = m, ncol = ncol(X_Shuf)-1)
se_res <- matrix(NA, nrow = m, ncol = ncol(X_Shuf)-1)

startvals <- rep(0,ncol(X_Shuf))

for (i in 1:m) {
  
  X_Shuf_0 <- X_Shuf[which(X_Shuf[,1] == 0),]
  X_Shuf_1 <- X_Shuf[which(X_Shuf[,1] == 1),]
  
  sel0 <- sample(1:nrow(X_Shuf_0), floor(2 / 3 * nrow(X_Shuf_0)))
  sel1 <- sample(1:nrow(X_Shuf_1), floor(2 / 3 * nrow(X_Shuf_1)))
  
  X_rs <- rbind(X_Shuf_0[sel0,],X_Shuf_1[sel1,])
  
  
  model <-brglm(X_rs[,1] ~ X_rs[,2]*X_rs[,3]
                +  X_rs[,4] +X_rs[,5] + X_rs[,6]
                + X_rs[,7] + X_rs[,8] + +factor(X_rs[,9]) + factor(X_rs[,10]), 
                family=binomial(link="logit"), method = "brglm.fit", p1 = T, data = X_rs)
  
  
  beta_res[i,] <- c(model$coefficients[1:8],model$coefficients[length(model$coefficients)])
  se_res[i, ] <- c(sqrt(diag(vcovHC(model, type = "HC0")))[1:8],
                   sqrt(diag(vcovHC(model, type = "HC0")))[length(sqrt(diag(vcovHC(model, type = "HC0"))))])
}


q <- apply(beta_res, 2, mean, na.rm = T)
seq <- apply(se_res ^ 2, 2, mean, na.rm = T)
sq <-
  apply(sweep(beta_res, 2, 
              apply(beta_res, 2, mean, 
                    na.rm = T)) ^ 2 / (m - 1), 2, 
        sum, na.rm = T) * (1 + 1 / m)

q_se <- sqrt(seq + sq)

results_Shuf <- cbind(c(modelShuf_perso_pen$coefficients[1:8],
                        modelShuf_perso_pen$coefficients[172]),
                      c(sqrt(diag(vcovHC(modelShuf_perso_pen, type = "HC0")))[1:8],
                        sqrt(diag(vcovHC(modelShuf_perso_pen, type = "HC0")))[171])
                      ,q, q_se)

rownames(results_Shuf) <- c("Intercept", "Legislature","Personalism","Military Leader","GDPpc","GDP Growth",
                            "Population","Post Cold War","Legislature:Personalism")
colnames(results_Shuf) <- c("Estimate", "Std. Error",
                            "CrossVal Comb ES","CrosVal Comb SE")

## show results
round(results_Shuf,3)

##kable(results_Shuf,digits = 3, caption = "Cross Validation Results For Reshuffling Coups",
#booktabs = T, format = "latex")

## For Regime-Change Coups with Robust Standard Errors Appendix A11

X_Rech <- data.frame(cbind(allData$rechangeatt_fin,allData$gwf_legislature,allData$personalism,
                           allData$gwf_leadermil,allData$ll_rgdpe_PW,allData$lag_growth_rgdpe_PW,
                           allData$ll_pop_PW,allData$postcold,allData$cowcode,allData$year))


set.seed(17)
m <- 10
beta_res <- matrix(NA, nrow = m, ncol = ncol(X_Rech)-1)
se_res <- matrix(NA, nrow = m, ncol = ncol(X_Rech)-1)

startvals <- rep(0,ncol(X_Rech))

for (i in 1:m) {
  
  X_Rech_0 <- X_Rech[which(X_Rech[,1] == 0),]
  X_Rech_1 <- X_Rech[which(X_Rech[,1] == 1),]
  
  sel0 <- sample(1:nrow(X_Rech_0), floor(2 / 3 * nrow(X_Rech_0)))
  sel1 <- sample(1:nrow(X_Rech_1), floor(2 / 3 * nrow(X_Rech_1)))
  
  X_rc <- rbind(X_Rech_0[sel0,],X_Rech_1[sel1,])
  
  
  model <-brglm(X_rc[,1] ~ X_rc[,2]*X_rc[,3]
                +  X_rc[,4] +X_rc[,5] + X_rc[,6]
                + X_rc[,7] + X_rc[,8] + +factor(X_rc[,9]) + factor(X_rc[,10]), 
                family=binomial(link="logit"), method = "brglm.fit", p1 = T, data = X_rc)
  
  
  beta_res[i,] <- c(model$coefficients[1:8],model$coefficients[length(model$coefficients)])
  se_res[i, ] <- c(sqrt(diag(vcovHC(model, type = "HC0")))[1:8],
                   sqrt(diag(vcovHC(model, type = "HC0")))[length(sqrt(diag(vcovHC(model, type = "HC0"))))])
}


q <- apply(beta_res, 2, mean, na.rm = T)
seq <- apply(se_res ^ 2, 2, mean, na.rm = T)
sq <-
  apply(sweep(beta_res, 2, 
              apply(beta_res, 2, mean, 
                    na.rm = T)) ^ 2 / (m - 1), 2, 
        sum, na.rm = T) * (1 + 1 / m)

q_se <- sqrt(seq + sq)

results_Rech <- cbind(c(modelRech_perso_pen$coefficients[1:8],
                        modelRech_perso_pen$coefficients[172]),
                      c(sqrt(diag(vcovHC(modelRech_perso_pen, type = "HC0")))[1:8],
                        sqrt(diag(vcovHC(modelRech_perso_pen, type = "HC0")))[171])
                      ,q, q_se)

rownames(results_Rech) <- c("Intercept", "Legislature","Personalism","Military Leader","GDPpc","GDP Growth",
                            "Population","Post Cold War","Legislature:Personalism")
colnames(results_Rech) <- c("Estimate", "Std. Error",
                            "CrossVal Comb ES","CrossVal Comb SE")

## show results
round(results_Rech,3)
##kable(results_Rech,digits = 3, caption = "Cross Validation Results For Regime-Change Coups",
#booktabs = T, format = "latex")







