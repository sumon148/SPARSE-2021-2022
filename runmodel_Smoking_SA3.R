# Based on Survey design based standard error

runmodel_smoking_SA3 <- function(model_id, parallel=TRUE, cl=NULL, seed=1, burnin=250, n.iter=1000, thin=1, n.chain=3, plot.trace=NULL,
                              predX, bias.correction=FALSE, save.full.pred=FALSE) {
  
  dat$yr <- 2018

  tr <- c("log", "sqrt","lg_ratio")[c(grepl("log", model_id, fixed=TRUE), grepl("sqrt", model_id, fixed=TRUE) , grepl("lg_ratio", model_id, fixed=FALSE))]
  sub <- sub_pred_dist  # same as sub_nr
  if (length(tr)) {
    backtr_fun <- switch(tr,
                         log = if (bias.correction) {
                           bc.factor <- exp(0.5*dat$log_se[sub]^2)
                           bc.factor[is.na(bc.factor)] <- 1
                           function(x) exp(x)*bc.factor
                         }  else {
                           function(x) exp(x)
                         },
                         sqrt = if (bias.correction) {
                           bc.factor <- dat$sqrt_se[sub]^2
                           bc.factor[is.na(bc.factor)] <- 0
                           function(x) (x^2 + bc.factor)
                         } else {
                           function(x) x^2
                         },
                         lg_ratio = if (bias.correction) {
                           bc.factor <- exp(0.5*dat$log_ratio_se[sub]^2)
                           bc.factor[is.na(bc.factor)] <- 1
                           function(x) exp(x)*bc.factor / (1 + exp(x)*bc.factor)
                         }  else {
                           function(x) exp(x)/(1+exp(x))
                         }
    )
  }
  
  if (parallel) {
    if (is.null(cl)) {
      cl <- setup_cluster(3)
      on.exit(stop_cluster(cl))
    }
    plot.trace <- NULL
  } else {
    cl <- NULL
  }
  
  sim <- MCMCsim(sampler, store.all=TRUE, burnin=burnin, n.iter=n.iter, thin=thin, n.chain=n.chain, cl=cl, plot.trace=plot.trace)
  #sim <- MCMCsim(sampler, store.all=TRUE, cl=cl, verbose = T)
  
  (summ <- summary(sim))
  
  (DIC <- compute_DIC(sim))
  (WAIC <- compute_WAIC(sim, diagnostic=TRUE))
  
  LOO <- loo(sim)

  # backtransforms
  if (length(tr)) {
    lp <- transform_dc(sim$linpred, fun = backtr_fun)
  } else {
    lp <- sim$linpred
  }
  summ$lp <- summary(lp)
  
  rownames(summ$lp) <- paste(dat$SA3_2016[sub])
  
  lp.95p.CI <- t(apply(as.matrix(lp),2,function(x)quantile(x,c(0.025,0.975))))
  rownames(lp.95p.CI) <- paste(dat$SA3_2016[sub])
  summ$lp.95p.CI <- lp.95p.CI
  
  # overall total
  
  N.tot <- c(t(tapply(dat$N, list(dat$yr), sum)))
  M <- aggrMatrix(paste(dat$yr[sub], sep=":"), w=dat$N[sub], mean=FALSE, facnames=TRUE)
  lpM <- transform_dc(lp, fun=function(x) crossprod_mv(M, x))
  summ$lp_tot <- summary(lpM)  # time-series
  rownames(summ$lp_tot) <- colnames(M)
  summ$lp_tot[, c("Mean", "SD", "MCSE", "q0.05", "q0.5", "q0.95")] <- (1/N.tot) * summ$lp_tot[, c("Mean", "SD", "MCSE", "q0.05", "q0.5", "q0.95")]
  
  lp_NATIONAL.95p.CI <- t(apply(as.matrix(lpM),2,function(x)quantile(x,c(0.0275,0.975))))
  rownames(lp_NATIONAL.95p.CI) <- colnames(M)
  lp_NATIONAL.95p.CI <- (1/N.tot) * lp_NATIONAL.95p.CI
  summ$lp_NATIONAL.95p.CI <- lp_NATIONAL.95p.CI
  
  # totals by State ----------------------------------------#
  
  N.tot <- c(t(tapply(dat$N[sub], list(dat$State[sub]), sum)))
  M <- aggrMatrix(dat$State[sub], w=dat$N[sub], mean=FALSE, facnames=TRUE)
  lpM <- transform_dc(lp, fun=function(x) crossprod_mv(M, x))
  summ$lp_State <- summary(lpM)  # time-series motive
  rownames(summ$lp_State) <- colnames(M)
  summ$lp_State[, c("Mean", "SD", "MCSE", "q0.05", "q0.5", "q0.95")] <- (1/N.tot) * summ$lp_State[, c("Mean", "SD", "MCSE", "q0.05", "q0.5", "q0.95")]

  lp_STATE.95p.CI <- t(apply(as.matrix(lpM),2,function(x)quantile(x,c(0.0275,0.975))))
  rownames(lp_STATE.95p.CI) <- colnames(M)
  lp_STATE.95p.CI <- (1/N.tot) * lp_STATE.95p.CI
  summ$lp_STATE.95p.CI <- lp_STATE.95p.CI
  
  # totals by SA4 ----------------------------------------#
  
  N.tot <- c(t(tapply(dat$N[sub], list(dat$SA4_2016[sub]), sum)))
  M <- aggrMatrix(dat$SA4_2016[sub], w=dat$N[sub], mean=FALSE, facnames=TRUE)
  lpM <- transform_dc(lp, fun=function(x) crossprod_mv(M, x))
  summ$lp_SA4 <- summary(lpM)  # time-series motive
  rownames(summ$lp_SA4) <- colnames(M)
  summ$lp_SA4[, c("Mean", "SD", "MCSE", "q0.05", "q0.5", "q0.95")] <- (1/N.tot) * summ$lp_SA4[, c("Mean", "SD", "MCSE", "q0.05", "q0.5", "q0.95")]

  lp_SA4.95p.CI <- t(apply(as.matrix(lpM),2,function(x)quantile(x,c(0.0275,0.975))))
  rownames(lp_SA4.95p.CI) <- colnames(M)
  lp_SA4.95p.CI <- (1/N.tot) * lp_SA4.95p.CI
  summ$lp_SA4.95p.CI <- lp_SA4.95p.CI
  
  # totals by SA3 ----------------------------------------#
  
  N.tot <- c(t(tapply(dat$N[sub], list(dat$SA3_2016[sub]), sum)))
  M <- aggrMatrix(dat$SA3_2016[sub], w=dat$N[sub], mean=FALSE, facnames=TRUE)
  lpM <- transform_dc(lp, fun=function(x) crossprod_mv(M, x))
  summ$lp_SA3 <- summary(lpM)  # time-series motive
  rownames(summ$lp_SA3) <- colnames(M)
  summ$lp_SA3[, c("Mean", "SD", "MCSE", "q0.05", "q0.5", "q0.95")] <- (1/N.tot) * summ$lp_SA3[, c("Mean", "SD", "MCSE", "q0.05", "q0.5", "q0.95")]
  
  lp_SA3.95p.CI <- t(apply(as.matrix(lpM),2,function(x)quantile(x,c(0.0275,0.975))))
  rownames(lp_SA3.95p.CI) <- colnames(M)
  lp_SA3.95p.CI <- (1/N.tot) * lp_SA3.95p.CI
  summ$lp_SA3.95p.CI <- lp_SA3.95p.CI
  

  
  rmarkdown::render("model_Output_Smoking_SA3.R",quiet = TRUE)
  file.rename("model_Output_Smoking_SA3.html", paste0("modeloutput_", model_id, ".html"))
  
  save(model_id, model, model.V, summ, DIC, WAIC, LOO, file=paste0("model_", model_id, ".RData"))
  if (save.full.pred) {
    lp_dist <- lp
    save(lp_dist,  file=paste0(model_id, "_sim_predictions.RData")) 
  }
  
  list(model_id=model_id, model=model, model.V=model.V, sim=sim, sampler=sampler, summ=summ, DIC=DIC, WAIC=WAIC,LOO=LOO)
}
