


source("./Data.R")

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




