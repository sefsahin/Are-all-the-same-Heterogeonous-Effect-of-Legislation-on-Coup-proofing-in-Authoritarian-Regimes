


source("./Simulations.R")


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





