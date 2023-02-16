#' ---
#' title: Model output
#' output:
#'    html_document:
#'      toc: true
#' ---

#+ echo=FALSE, message=FALSE

options(width=130)  # width of output
library(ggplot2)

smoke_national_2016 <- read.csv("smoke_nat.csv",header = T)[,-1]
smoke_state_2016 <- read.csv("smoke_state.csv",header = T)[,-1]
smoke_state_SA4_2016 <- read.csv("smoke_state_SA4_2016.csv",header = T)[,-1]
smoke_state_SA3_2016 <- read.csv("smoke_state_SA3_2016.csv",header = T)


#' # model
model_id
model

survey.est <- FALSE
#' ## modelmaten

DIC
unlist(WAIC[c("WAIC1", "p_WAIC1", "WAIC2", "p_WAIC2")])
LOO
#' ## MCMC summaries, sorted by decreasing Rhat
subset <- !(grepl("_xi_", names(summ)) | grepl("_Q_", names(summ)) | grepl("_Qraw_", names(summ)) | names(summ) %in% c("Q_", "res_"))

#+ echo=FALSE, dpi=200, out.width=1000

print(summ[subset], sort="R_hat", tail=TRUE)

#print(summ[subset],  tail=TRUE) #sort="R_hat",

#' ### Fixed effects
summ$beta[,c(1,3,9)]


#' ### National Level Estimate
#+ echo=FALSE, dpi=200, out.width=1000
if (!is.null(summ$lp_tot)) {
d.1 <- setDT(dat[, c("yr", "State", "SA4_2016", "SA3_2016" , "mean", "se", "n","N")])

# d.1$N.missing <- d.1$N
# d.1$N.missing[is.na(d.1$mean) | is.na(d.1$se) ] <- NA
# # d.1$N.missing[is.na(d.1$mean) | is.na(d.1$se) | d.1$mean==0] <- NA
# df.1 <- d.1[, .(est.DIR=sum(N.missing*mean,na.rm=TRUE)/sum(N.missing,na.rm=TRUE), se.DIR=sqrt(sum((N.missing * se)^2,na.rm=TRUE))/sum(N.missing,na.rm=TRUE),N=sum(N,na.rm=TRUE)), by=.(yr)]

d.1$n.missing <- d.1$n
d.1$n.missing[is.na(d.1$mean) | is.na(d.1$se) | d.1$n==0] <- NA
df.1 <- d.1[, .(est.DIR=sum(n.missing*mean,na.rm=TRUE)/sum(n.missing,na.rm=TRUE), se.DIR=sqrt(sum((n.missing * se)^2,na.rm=TRUE))/sum(n.missing,na.rm=TRUE)), by=.(yr)]

df.1$ll <- df.1$est.DIR - 2*df.1$se.DIR
df.1$ul <- df.1$est.DIR + 2*df.1$se.DIR

m <- match(df.1[, paste(yr, sep=":")], rownames(summ$lp_tot))
df.1[, M_est_FH := summ$lp_tot[m, "Mean"]]
df.1[, M_se_FH := summ$lp_tot[m, "SD"]]
df.1[, bias_FH := M_est_FH - est.DIR]
df.1[, M_est_FH_ll := summ$lp_NATIONAL.95p.CI[,1]]
df.1[, M_se_FH_ul := summ$lp_NATIONAL.95p.CI[,2]]
}
df.1

summary(df.1$bias_FH)

#' ### State Level Estimate
#+ echo=FALSE, dpi=200, out.width=1000
if (!is.null(summ$lp_State)) {
  d.1 <- setDT(dat[, c("yr", "State", "SA4_2016", "SA3_2016" , "mean", "se", "n","N")])

#   d.1$N.missing <- d.1$N
#   d.1$N.missing[is.na(d.1$mean) | is.na(d.1$se) ] <- NA
# #  d.1$N.missing[is.na(d.1$mean) | is.na(d.1$se) | d.1$n<10] <- NA
#   df <- d.1[, .(est.DIR=sum(N.missing*mean,na.rm=TRUE)/sum(N.missing,na.rm=TRUE), se.DIR=sqrt(sum((N.missing * se)^2,na.rm=TRUE))/sum(N.missing,na.rm=TRUE),N=sum(N,na.rm=TRUE)), by=.(State)]
#   df <- df[order(df$State),]

  d.1$n.missing <- d.1$n
  d.1$n.missing[is.na(d.1$mean) | is.na(d.1$se) | d.1$n==0] <- NA
  df <- d.1[, .(est.DIR=sum(n.missing*mean,na.rm=TRUE)/sum(n.missing,na.rm=TRUE), se.DIR=sqrt(sum((n.missing * se)^2,na.rm=TRUE))/sum(n.missing,na.rm=TRUE),N=sum(N,na.rm=TRUE)), by=.(State)]
  
  # d.NT <-d.1[d.1$State=="NT",]
  # sum(d.NT$mean*d.NT$n,na.rm=T)/sum(d.NT$n,na.rm=T)
  # sum(d.NT$mean*d.NT$N.missing,na.rm=T)/sum(d.NT$N.missing,na.rm=T)
  # 
  # d.WA <-d.1[d.1$State=="WA",]
  # sum(d.WA$mean*d.WA$n,na.rm=T)/sum(d.WA$n,na.rm=T)
  # sum(d.WA$mean*d.WA$N.missing,na.rm=T)/sum(d.WA$N.missing,na.rm=T)
  
  if(survey.est){
    smoke_state_2016$state <- factor(smoke_state_2016$state,levels=levels(df$State))
    smoke_state_2016 <- smoke_state_2016[order(smoke_state_2016$state),]
   # df$est.DIR <- smoke_state_2016$mean
   # df$se.DIR <- smoke_state_2016$se
  } 
  
  df$ll <- df$est.DIR - 2*df$se.DIR
  df$ul <- df$est.DIR + 2*df$se.DIR
  df$ll[df$ll<0] <- 0
  m <- match(df[, paste(State, sep=":")], rownames(summ$lp_State))
  df[, M_est_FH := summ$lp_State[m, "Mean"]]
  df[, M_se_FH := summ$lp_State[m, "SD"]]
  df[, bias_FH := M_est_FH - est.DIR]
  df[, M_est_FH_ll := summ$lp_STATE.95p.CI[,1]]
  df[, M_est_FH_ul := summ$lp_STATE.95p.CI[,2]]
  
#  Numra <- sum(df$N*df$est.DIR,na.rm=TRUE)/sum(df$N,na.rm=TRUE)
  Numra <- 0.138
  Denom <- sum(df$N*df$M_est_FH,na.rm=TRUE)/sum(df$N,na.rm=TRUE)
  df[, M_est_BFH := M_est_FH*Numra/Denom]
#  df[, M_se_BFH := sqrt(M_se_FH^2+(M_est_BFH-M_est_FH)^2)]
  df[, M_se_BFH := M_se_FH]
}
df

summary(df$bias_FH)

pd <- position_dodge(0.5) # move them .05 to the left and right

if (!is.null(summ$lp_State)) {
#  plotsub <- df$year >= 1994
  p <- ggplot(data=df, mapping=aes(x=State, y=est.DIR))  # est is direct estimate
  #p <- p + geom_line(size=0.6)
  p <- p + geom_point(shape=1) + xlab("State") +
    ylab("Smoking Prevalence") 
  p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),width=0.5)
  p <- p + geom_point(mapping=aes(x=State, y=M_est_FH),width=0.5,shape=2,col="green3",position=pd)
#  p <- p + geom_errorbar(aes(ymin=M_est_FH - 2*M_se_FH,ymax=M_est_FH + 2*M_se_FH),width=0.5,col="green3",position=pd)
  p <- p + geom_errorbar(aes(ymin=M_est_FH_ll,ymax=M_est_FH_ul),width=0.5,col="green3",position=pd)
  p <- p + geom_point(mapping=aes(x=State, y=M_est_BFH),width=0.5,shape=4,col="red",position=pd)
  p <- p + geom_errorbar(aes(ymin=M_est_BFH - 2*M_se_BFH,ymax=M_est_BFH + 2*M_se_BFH),width=0.5,col="red",position=pd)
  p <- p + coord_flip()
  p <- p + ggtitle("State level Smoking Prevalence")
  suppressWarnings(
    plot(p)
  )
}


## PPC Density plot State --------------#

# N.tot <- c(t(tapply(dat$N[sub], list(dat$State[sub]), sum)))
# M <- aggrMatrix(dat$State[sub], w=dat$N[sub], mean=FALSE, facnames=TRUE)
# lpM <- transform_dc(lp, fun=function(x) crossprod_mv(M, x))
# lp_State <- transform_dc(lpM,fun=function(x) x/N.tot)
# 
# y_State <- df$est.DIR 
# y_rep_State <- as.array(lp_State)
# ppc_dens_overlay(y_State,y_rep_State[,1,])+ggtitle("State Level Prevalence") #+xlim(0,1)


#' ### SA4 Level Estimate
#+ echo=FALSE, dpi=200, out.width=1000

if (!is.null(summ$lp_SA4)) {
  d.1 <- setDT(dat[, c("yr", "State", "SA4_2016", "SA3_2016" , "mean", "se", "n","N")])
  
  # d.1$N.missing <- d.1$N
  # d.1$N.missing[is.na(d.1$mean) | is.na(d.1$se)] <- NA
  # df <- d.1[, .(est.DIR=sum(N.missing*mean,na.rm=TRUE)/sum(N.missing,na.rm=TRUE), se.DIR=sqrt(sum((N.missing * se)^2,na.rm=TRUE))/sum(N.missing,na.rm=TRUE),N=sum(N,na.rm=TRUE),State=unique(State)), by=.(SA4_2016)]
  # # df.n <- d.1[, .(est.DIR=sum(n*mean,na.rm=TRUE)/sum(n,na.rm=TRUE), se.DIR=sqrt(sum((n * se)^2,na.rm=TRUE))/sum(n,na.rm=TRUE),N=sum(N,na.rm=TRUE),State=unique(State)), by=.(SA4_2016)]
  # df <- df[order(df$SA4_2016),]
  
  
  d.1$n.missing <- d.1$n
  d.1$n.missing[is.na(d.1$mean) | is.na(d.1$se) | d.1$n==0] <- NA
  df <- d.1[, .(est.DIR=sum(n.missing*mean,na.rm=TRUE)/sum(n.missing,na.rm=TRUE), se.DIR=sqrt(sum((n.missing * se)^2,na.rm=TRUE))/sum(n.missing,na.rm=TRUE),N=sum(N,na.rm=TRUE),n=sum(n,na.rm=TRUE),State=unique(State)), by=.(SA4_2016)]
  df <- df[order(df$SA4_2016),]
  
  
  if(survey.est){
  smoke_state_SA4_2016$sa416 <- factor(smoke_state_SA4_2016$sa416,levels=levels(df$SA4_2016))
  smoke_state_SA4_2016 <- smoke_state_SA4_2016[order(smoke_state_SA4_2016$sa416),]
  range_est <- range(smoke_state_SA4_2016$mean,df$est.DIR)
  plot(smoke_state_SA4_2016$sa416,smoke_state_SA4_2016$mean,ylim=range_est)
  lines(df$SA4_2016,df$est.DIR,typ="p",col=2)
  range_se <- range(smoke_state_SA4_2016$se,df$se.DIR)
  plot(smoke_state_SA4_2016$sa416,smoke_state_SA4_2016$se,ylim=range_se)
  lines(df$SA4_2016,df$se,typ="p",col=2)
  # df$est.DIR <- smoke_state_SA4_2016$mean
  # df$se.DIR <- smoke_state_SA4_2016$se
  }
  
  plot(df$SA4_2016,df$est.DIR)
#  lines(smoke_state_SA4_2016$sa416,smoke_state_SA4_2016$mean,typ="p",col=2)
  plot(df$SA4_2016,df$se.DIR)
#  lines(smoke_state_SA4_2016$sa416,smoke_state_SA4_2016$se,typ="p",col=2)
  
  df$ll <- df$est.DIR - 2*df$se.DIR
  df$ul <- df$est.DIR + 2*df$se.DIR
  df$ll[df$ll<0] <- 0
  df$ul[df$ul>1] <- 0.99
  m <- match(df[, paste(SA4_2016, sep=":")], rownames(summ$lp_SA4))
  df[, M_est_FH := summ$lp_SA4[m, "Mean"]]
  df[, M_se_FH := summ$lp_SA4[m, "SD"]]
  df[, bias_FH := M_est_FH - est.DIR]
  df[, M_est_FH_ll := summ$lp_SA4.95p.CI[,1]]
  df[, M_est_FH_ul := summ$lp_SA4.95p.CI[,2]]
  
#  Numra <- sum(df$N*df$est.DIR,na.rm=TRUE)/sum(df$N,na.rm=TRUE)
  Numra <- 0.138
  Denom <- sum(df$N*df$M_est_FH,na.rm=TRUE)/sum(df$N,na.rm=TRUE)
  df[, M_est_BFH := M_est_FH*Numra/Denom]
#  df[, M_se_BFH := sqrt(M_se_FH^2+(M_est_BFH-M_est_FH)^2)]
  df[, M_se_BFH := M_se_FH]
  
}
df

summary(df$bias_FH)

tapply(df$bias_FH,df$State,function(x) summary(x))

plot(df$est.DIR,df$M_est_FH,xlim=range(df$est.DIR,df$M_est_FH,na.rm=T),ylim=range(df$est.DIR,df$M_est_FH,na.rm=T),xlab="DIR",ylab="SAE",main="Smoking Prevalence: SA4")
lines(df$est.DIR,df$M_est_BFH,typ="p",col=2)
legend("topleft",legend=c("FH","BFH"),pch=c(1,1),col=c(1,2))
abline(0,1)

plot(df$se.DIR,df$M_se_FH,xlim=range(df$se.DIR,df$M_se_FH,na.rm=T),ylim=range(df$se.DIR,df$M_se_FH,na.rm=T),xlab="DIR",ylab="SAE",main="SE: SA4")
lines(df$se.DIR,df$M_se_BFH,typ="p",col=2)
legend("topleft",legend=c("FH","BFH"),pch=c(1,1),col=c(1,2))
abline(0,1)

plot(df$se.DIR/df$est.DIR,df$M_se_FH/df$M_est_FH,xlim=range(df$se.DIR/df$est.DIR,df$M_se_FH/df$M_est_FH,na.rm=T),ylim=range(df$se.DIR/df$est.DIR,df$M_se_FH/df$M_est_FH,na.rm=T),xlab="DIR",ylab="SAE",main="CV: SA4")
lines(df$se.DIR/df$est.DIR,df$M_se_BFH/df$M_est_BFH,typ="p",col=2)
legend("topleft",legend=c("FH","BFH"),pch=c(1,1),col=c(1,2))
abline(0,1)

## PPC Density plot SA4 --------------#

# N.tot <- c(t(tapply(dat$N[sub], list(dat$SA4_2016[sub]), sum)))
# M <- aggrMatrix(dat$SA4_2016[sub], w=dat$N[sub], mean=FALSE, facnames=TRUE)
# lpM <- transform_dc(lp, fun=function(x) crossprod_mv(M, x))
# lp_SA4 <- transform_dc(lpM,fun=function(x) x/N.tot)
# 
# y_SA4 <- df$est.DIR 
# y_rep_SA4 <- as.array(lp_SA4)
# ppc_dens_overlay(y_SA4,y_rep_SA4[,1,])+ggtitle("SA4 Level Prevalence") # +xlim(0,1)

pd <- position_dodge(0.5) # move them .05 to the left and right

d <- unique(df$State)

for (i in 1:length(d)) {
  #  plotsub <- df$year >= 1994
  p <- ggplot(data=df[df$State==levels(df$State)[i],], mapping=aes(x=SA4_2016, y=est.DIR))  # est is direct estimate
  #p <- p + geom_line(size=0.6)
  p <- p + geom_point(shape=1) + xlab("SA4") +
    ylab("Smoking Prevalence") 
  # p <- p + geom_errorbar(aes(ymin=est.DIR - 2*se.DIR,ymax=est.DIR + 2*se.DIR),width=0.5)
  p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),width=0.5)
  p <- p + geom_point(mapping=aes(x=SA4_2016, y=M_est_FH),shape=2,col="green3",position=pd)
#  p <- p + geom_errorbar(aes(ymin=M_est_FH - 2*M_se_FH,ymax=M_est_FH + 2*M_se_FH),width=0.5,col="green3",position=pd)
  p <- p + geom_errorbar(aes(ymin=M_est_FH_ll,ymax=M_est_FH_ul),width=0.5,col="green3",position=pd)
  p <- p + geom_point(mapping=aes(x=SA4_2016, y=M_est_BFH),width=0.5,shape=4,col="red",position=pd)
  p <- p + geom_errorbar(aes(ymin=M_est_BFH - 2*M_se_BFH,ymax=M_est_BFH + 2*M_se_BFH),width=0.5,col="red",position=pd)
  p <- p + coord_flip()
  p <- p + ggtitle(paste0("SA4 level Smoking Prevalence: ",d[i]))
  suppressWarnings(
    plot(p)
  )
}

#' ### SA3 Level Estimate
#+ echo=FALSE, dpi=200, out.width=1000
# Add classification of Remoteness area
# dat$RA_NAME_2016

if (!is.null(summ$lp_SA3)) {
  d.1 <- setDT(dat[, c("yr", "State", "SA4_2016", "SA3_2016","RA_NAME_2016", "mean", "se", "n","N")])
  df <- d.1[, .(est.DIR=mean, se.DIR=se,SA4_2016=unique(SA4_2016),RA_NAME_2016=unique(RA_NAME_2016),N=sum(N,na.rm=TRUE),n=sum(n,na.rm=TRUE),State=unique(State)), by=.(SA3_2016)]
  #df$RA_NAME_2016 <- as.factor(df$RA_NAME_2016)
  df$RA_NAME_2016 <- factor(df$RA_NAME_2016,levels = c("Inner Regional Australia","Major Cities of Australia","Outer Regional Australia","Remote Australia","Very Remote Australia"))
  
  
  if(survey.est){
    smoke_state_SA3_2016$sa316 <- factor(smoke_state_SA3_2016$sa316,levels=levels(df$SA3_2016))
    smoke_state_SA3_2016 <- smoke_state_SA3_2016[order(smoke_state_SA3_2016$sa316),]
    range_est <- range(smoke_state_SA3_2016$mean,df$est.DIR,na.rm=T)
    plot(smoke_state_SA3_2016$sa316,smoke_state_SA3_2016$mean,ylim=range_est)
    lines(df$SA3_2016,df$est.DIR,typ="p",col=2)
    range_se <- range(smoke_state_SA3_2016$se,df$se.DIR)
    plot(smoke_state_SA3_2016$sa316,smoke_state_SA3_2016$se,ylim=range_se)
    lines(df$SA3_2016,df$se,typ="p",col=2)
  }
  

  df$ll <- df$est.DIR - 2*df$se.DIR
  df$ul <- df$est.DIR + 2*df$se.DIR
  df$ll[df$ll<0] <- 0
  df$ul[df$ul>1] <- 0.99
  m <- match(df[, paste(SA3_2016, sep=":")], rownames(summ$lp_SA3))
  df[, M_est_FH := summ$lp_SA3[m, "Mean"]]
  df[, M_se_FH := summ$lp_SA3[m, "SD"]]
  df[, bias_FH := M_est_FH - est.DIR]
  df[, M_est_FH_ll := summ$lp_SA3.95p.CI[,1]]
  df[, M_est_FH_ul := summ$lp_SA3.95p.CI[,2]]
  
#  Numra <- sum(df$N*df$est.DIR,na.rm=TRUE)/sum(df$N,na.rm=TRUE)
  Numra <- 0.138
  Denom <- sum(df$N*df$M_est_FH,na.rm=TRUE)/sum(df$N,na.rm=TRUE)
  df[, M_est_BFH := M_est_FH*Numra/Denom]
#  df[, M_se_BFH := sqrt(M_se_FH^2+(M_est_BFH-M_est_FH)^2)]
  df[, M_se_BFH := M_se_FH]
}
df

summary(df$bias_FH)

tapply(df$bias_FH,df$State,function(x) summary(x))

plot(df$est.DIR,df$M_est_FH,xlim=range(df$est.DIR,df$M_est_FH,na.rm=T),ylim=range(df$est.DIR,df$M_est_FH,na.rm=T),xlab="DIR",ylab="SAE",main="Smoking Prevalence: SA3")
lines(df$est.DIR,df$M_est_BFH,typ="p",col=2)
legend("topleft",legend=c("FH","BFH"),pch=c(1,1),col=c(1,2))
abline(0,1)

plot(df$se.DIR,df$M_se_FH,xlim=range(df$se.DIR,df$M_se_FH,na.rm=T),ylim=range(df$se.DIR,df$M_se_FH,na.rm=T),xlab="DIR",ylab="SAE",main="SE: SA3")
lines(df$se.DIR,df$M_se_BFH,typ="p",col=2)
legend("topleft",legend=c("FH","BFH"),pch=c(1,1),col=c(1,2))
abline(0,1)

plot(df$se.DIR/df$est.DIR,df$M_se_FH/df$M_est_FH,xlim=range(df$se.DIR/df$est.DIR,df$M_se_FH/df$M_est_FH,na.rm=T),ylim=range(df$se.DIR/df$est.DIR,df$M_se_FH/df$M_est_FH,na.rm=T),xlab="DIR",ylab="SAE",main="CV: SA3")
lines(df$se.DIR/df$est.DIR,df$M_se_BFH/df$M_est_BFH,typ="p",col=2)
legend("topleft",legend=c("FH","BFH"),pch=c(1,1),col=c(1,2))
abline(0,1)


## PPC Density plot SA3 --------------#

# N.tot <- c(t(tapply(dat$N[sub], list(dat$SA3_2016[sub]), sum)))
# M <- aggrMatrix(dat$SA3_2016[sub], w=dat$N[sub], mean=FALSE, facnames=TRUE)
# lpM <- transform_dc(lp, fun=function(x) crossprod_mv(M, x))
# lp_SA3 <- transform_dc(lpM,fun=function(x) x/N.tot)
# 
# y_SA3 <- df$est.DIR[sub_dist_with_zero] 
# y_rep_SA3 <- as.array(lp_SA3)[,,sub_dist_with_zero]
# ppc_dens_overlay(y_SA3,y_rep_SA3[,1,])+ggtitle("SA3 Level Prevalence") # +xlim(0,1)


pd <- position_dodge(0.5) # move them .05 to the left and right

d <- unique(df$State)

for (i in 1:length(d)) {
  #  plotsub <- df$year >= 1994
  p <- ggplot(data=df[df$State==levels(df$State)[i],], mapping=aes(x=SA3_2016, y=est.DIR))  # est is direct estimate
  #p <- p + geom_line(size=0.6)
  p <- p + geom_point(shape=1) + xlab("SA3") +
    ylab("Smoking Prevalence") 
  # p <- p + geom_errorbar(aes(ymin=est.DIR - 2*se.DIR,ymax=est.DIR + 2*se.DIR),width=0.5)
  p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),width=0.5)
  p <- p + geom_point(mapping=aes(x=SA3_2016, y=M_est_FH),shape=2,col="green3",position=pd)
  #  p <- p + geom_errorbar(aes(ymin=M_est_FH - 2*M_se_FH,ymax=M_est_FH + 2*M_se_FH),width=0.5,col="green3",position=pd)
  p <- p + geom_errorbar(aes(ymin=M_est_FH_ll,ymax=M_est_FH_ul),width=0.5,col="green3",position=pd)
  p <- p + geom_point(mapping=aes(x=SA3_2016, y=M_est_BFH),width=0.5,shape=4,col="red",position=pd)
  p <- p + geom_errorbar(aes(ymin=M_est_BFH - 2*M_se_BFH,ymax=M_est_BFH + 2*M_se_BFH),width=0.5,col="red",position=pd)
  p <- p + coord_flip()
  p <- p + ggtitle(paste0("SA3 level Smoking Prevalence: ",d[i]))
  suppressWarnings(
    plot(p)
  )
}



#' ### SA3 Level Estimate: State-by-RA
#+ echo=FALSE, dpi=200, out.width=1000

d <- levels(df$State)
e <- levels(df$RA_NAME_2016)

for (i in 1:length(d)) {
  for(j in 1:length(e)){
  #  plotsub <- df$year >= 1994
  p <- ggplot(data=df[df$State==levels(df$State)[i] & df$RA_NAME_2016==levels(df$RA_NAME_2016)[j],], mapping=aes(x=SA3_2016, y=est.DIR))  # est is direct estimate
  #p <- p + geom_line(size=0.6)
  p <- p + geom_point(shape=1) + xlab("SA3") +
    ylab("Smoking Prevalence") 
  # p <- p + geom_errorbar(aes(ymin=est.DIR - 2*se.DIR,ymax=est.DIR + 2*se.DIR),width=0.5)
  p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),width=0.5)
  p <- p + geom_point(mapping=aes(x=SA3_2016, y=M_est_FH),shape=2,col="green3",position=pd)
  #  p <- p + geom_errorbar(aes(ymin=M_est_FH - 2*M_se_FH,ymax=M_est_FH + 2*M_se_FH),width=0.5,col="green3",position=pd)
  p <- p + geom_errorbar(aes(ymin=M_est_FH_ll,ymax=M_est_FH_ul),width=0.5,col="green3",position=pd)
  p <- p + geom_point(mapping=aes(x=SA3_2016, y=M_est_BFH),width=0.5,shape=4,col="red",position=pd)
  p <- p + geom_errorbar(aes(ymin=M_est_BFH - 2*M_se_BFH,ymax=M_est_BFH + 2*M_se_BFH),width=0.5,col="red",position=pd)
  p <- p + coord_flip()
  p <- p + ggtitle(paste0("SA3 level Smoking Prevalence: ",d[i]," & ",e[j]))
  suppressWarnings(
    plot(p)
  )
}
}


#' ### Model diagnostic
#+ echo=FALSE, message=FALSE, progress=FALSE, dpi=200, out.width=1000
## Model diagnostics: LOO PIT and Posterior Predictive check (Density Plot)

# # LOO PIT: SQUARED TRansformed Scale
#   loo.M1 <- loo(sim,r_eff=TRUE,save_psis=TRUE)
#   psis.loo.M1 <- loo.M1$psis_object
#   lw.loo.M1 <- weights(psis.loo.M1)
#   y_observed <- dat$sqrt_mean[sub_dist_with_zero]
#   y_rep <- predict(sim)
#   y_rep_array <- as.matrix(y_rep)
#   (P1.M1 <- ppc_loo_pit_overlay(y_observed,y_rep_array,lw=lw.loo.M1) )
#   (P2.M1 <- ppc_loo_pit_qq(y_observed,y_rep_array,lw=lw.loo.M1) )
#   (P3.M1 <- ppc_loo_pit_qq(y_observed,y_rep_array,lw=lw.loo.M1,compare="normal") )
# 
# # # Density plot: Original Scale -----------------#
# # # Do it later : Takes time.
# #  y <- dat$mod_mean
# #  y_rep_transformed <- as.array(lp)
# 
# # # Density plot: Transformed Scale -----------------#
#   y <- dat$sqrt_mean
#   y_rep_transformed <- as.array(sim$linpred)
# 
# # 
#  (p1 <- ppc_dens_overlay(y[sub_dist_with_zero],y_rep_transformed[,1,sub_dist_with_zero]) + xlim(0,1) + ggtitle("SA3 Level Prevalence"))
# # #(p2 <- ppc_dens_overlay(y,y_rep_transformed[,2,]) + xlim(0,1) + ggtitle("SA3 Level Prevalence"))
# # #(p3 <- ppc_dens_overlay(y,y_rep_transformed[,3,]) + xlim(0,1) + ggtitle("SA3 Level Prevalence"))
# # 
# # #p1
#   (p1 <- ppc_dens_overlay(y[sub_dist],y_rep_transformed[,1,sub_dist]) + xlim(0,1) + ggtitle("SA3 Level Prevalence"))
# # 
#  sub_NSW <- dat$State=="NSW" & !is.na(dat$mean) &  (dat$mean>0)
#  sub_VIC <- dat$State=="VIC" &  !is.na(dat$mean) &  (dat$mean>0)
#  sub_QLD <- dat$State=="QLD" &  !is.na(dat$mean) &  (dat$mean>0)
#  sub_SA <- dat$State=="SA" &  !is.na(dat$mean) &  (dat$mean>0)
#  sub_WA <- dat$State=="WA" &  !is.na(dat$mean) &  (dat$mean>0)
#  sub_TAS <- dat$State=="TAS" &  !is.na(dat$mean) &  (dat$mean>0)
#  sub_NT <- dat$State=="NT" &  !is.na(dat$mean) &  (dat$mean>0)
#  sub_ACT <- dat$State=="ACT" &  !is.na(dat$mean) &  (dat$mean>0)
# # 
#  ppc_dens_overlay(y[sub_NSW],y_rep_transformed[,1,sub_NSW])  + ggtitle("SA3 Level Prevalence: NSW")
#  ppc_dens_overlay(y[sub_VIC],y_rep_transformed[,1,sub_VIC])  + ggtitle("SA3 Level Prevalence: VIC")
#  ppc_dens_overlay(y[sub_QLD],y_rep_transformed[,1,sub_QLD])  + ggtitle("SA3 Level Prevalence: QLD")
#  ppc_dens_overlay(y[sub_SA],y_rep_transformed[,1,sub_SA])  + ggtitle("SA3 Level Prevalence: SA")
#  ppc_dens_overlay(y[sub_WA],y_rep_transformed[,1,sub_WA])  + ggtitle("SA3 Level Prevalence: WA")
#  ppc_dens_overlay(y[sub_TAS],y_rep_transformed[,1,sub_TAS])  + ggtitle("SA3 Level Prevalence: TAS")
#  ppc_dens_overlay(y[sub_NT],y_rep_transformed[,1,sub_NT])  + ggtitle("SA3 Level Prevalence: NT")
#  ppc_dens_overlay(y[sub_ACT],y_rep_transformed[,1,sub_ACT])  + ggtitle("SA3 Level Prevalence: ACT")
# # 
# # 
#  sub_Major <- dat$RA_NAME_2016=="Major Cities of Australia" & !is.na(dat$mean) &  (dat$mean>0)
#  sub_Inner_Outer <- dat$RA_NAME_2016%in%c("Inner Regional Australia","Outer Regional Australia") &  !is.na(dat$mean) &  (dat$mean>0)
#  sub_Remote <- dat$RA_NAME_2016%in%c("Remote Australia","Very Remote Australia") &  !is.na(dat$mean) &  (dat$mean>0)
# # 
#  ppc_dens_overlay(y[sub_Major],y_rep_transformed[,1,sub_Major])  + ggtitle("SA3 Level Prevalence: Major Cities")
#  ppc_dens_overlay(y[sub_Inner_Outer],y_rep_transformed[,1,sub_Inner_Outer])  + ggtitle("SA3 Level Prevalence: Inner and outer")
#  ppc_dens_overlay(y[sub_Remote],y_rep_transformed[,1,sub_Remote])  + ggtitle("SA3 Level Prevalence: Remote")
# 

# Residual Diagnostics
dat$resid[sub_dist_with_zero] <- (unlist(get_means(sim,"e_")))
# dat$stu_resid[sub_dist_with_zero] <- (unlist(get_means(sim,"e_")))/dat$se[sub_dist_with_zero]
# dat$stu_resid_main[sub_dist_with_zero] <- (unlist(get_means(sim,"e_")))/dat$se_main[sub_dist_with_zero]


par(mfrow=c(1,2))
x2 <- seq(min(dat$resid[sub_dist]),max(dat$resid[sub_dist]),length=40)
fun <-dnorm(x2,mean=mean(dat$resid[sub_dist]),sd=sd(dat$resid[sub_dist]))
hist(dat$resid[sub_dist],main="Distribution of Raw Residuals",prob=TRUE,ylim=c(0,max(fun)+1),breaks = 30)
lines(x2,fun,col=2,lwd=2)
qqnorm(dat$resid[sub_dist],main="QQ Plot: Raw Residuals")
qqline(dat$resid[sub_dist])

# x2 <- seq(min(dat$stu_resid[sub_dist]),max(dat$stu_resid[sub_dist]),length=40)
# fun <-dnorm(x2,mean=mean(dat$stu_resid[sub_dist]),sd=sd(dat$stu_resid[sub_dist]))
# hist(dat$stu_resid[sub_dist],main="Distribution of Studentized Residuals",prob=TRUE,ylim=c(0,max(fun)))
# lines(x2,fun,col=2,lwd=2)
# qqnorm(dat$stu_resid[sub_dist],main="QQ Plot: Studentized Residuals")
# qqline(dat$stu_resid[sub_dist])
# 
# x2 <- seq(min(dat$stu_resid_main[sub_dist]),max(dat$stu_resid_main[sub_dist]),length=40)
# fun <-dnorm(x2,mean=mean(dat$stu_resid_main[sub_dist]),sd=sd(dat$stu_resid_main[sub_dist]))
# hist(dat$stu_resid_main[sub_dist],main="Distribution of Studentized Residuals",prob=TRUE,ylim=c(0,max(fun)),breaks = 30)
# lines(x2,fun,col=2,lwd=2)
# qqnorm(dat$stu_resid_main[sub_dist],main="QQ Plot: Studentized Residuals")
# qqline(dat$stu_resid_main[sub_dist])

#' ## Model diagnostic by Covariates

par(mfrow=c(3,3))
plot(dat$IRSD[sub_dist],dat$resid[sub_dist],main="Residuals VS IRSD",xlab="IRSD",ylab="Residual")
abline(h=0,col=2)
#plot(dat$IRSAD[sub_dist],dat$resid[sub_dist],main="Residuals VS IRSD",xlab="IRSD",ylab="Residual")
#abline(h=0,col=2)
#plot(dat$P_Age_18_24_SA4[sub_dist],dat$resid[sub_dist],main="Residuals VS P_Age_18_24_SA4",xlab="P_Age_18_24_SA4",ylab="Residual")
#plot(dat$Age_18_24[sub_dist],dat$resid[sub_dist],main="Residuals VS P_Age_18_24_SA4",xlab="P_Age_18_24_SA4",ylab="Residual")
#abline(h=0,col=2)
#plot(dat$P_Age_25_29_SA4[sub_dist],dat$resid[sub_dist],main="Residuals VS P_Age_25_29_SA4",xlab="P_Age_25_29_SA4",ylab="Residual")
#plot(dat$Age_25_29[sub_dist],dat$resid[sub_dist],main="Residuals VS P_Age_25_29_SA4",xlab="P_Age_25_29_SA4",ylab="Residual")
#abline(h=0,col=2)
#plot(dat$P_Edu_Bachelor_SA4[sub_dist],dat$resid[sub_dist],main="Residuals VS P_Edu_Bachelor_SA4",xlab="P_Edu_Bachelor_SA4",ylab="Residual")
plot(dat$Bachelor_and_Higher_Education[sub_dist],dat$resid[sub_dist],main="Residuals VS P_Edu_Bachelor",xlab="P_Edu_Bachelor_SA4",ylab="Residual")
abline(h=0,col=2)
plot(dat$Bachelor_and_Higher_Education_SA4[sub_dist],dat$resid[sub_dist],main="Residuals VS P_Edu_Bachelor_SA4",xlab="P_Edu_Bachelor_SA4",ylab="Residual")
abline(h=0,col=2)
plot(dat$Certificate_III_IV[sub_dist],dat$resid[sub_dist],main="Residuals VS Certificate_III_IV",xlab="P_Edu_Bachelor_SA4",ylab="Residual")
abline(h=0,col=2)
plot(dat$Certificate_III_IV_SA4[sub_dist],dat$resid[sub_dist],main="Residuals VS Certificate_III_IV_SA4",xlab="P_Edu_Bachelor_SA4",ylab="Residual")
abline(h=0,col=2)
plot(dat$Indigenous[sub_dist],dat$resid[sub_dist],main="Residuals VS P_Indigenous",xlab="P_Indigenous_SA4",ylab="Residual")
abline(h=0,col=2)
plot(dat$Indigenous_SA4[sub_dist],dat$resid[sub_dist],main="Residuals VS P_Indigenous_SA4",xlab="P_Indigenous_SA4",ylab="Residual")
abline(h=0,col=2)

#'######## ## Model diagnostic by State

par(mfrow=c(1,1))
ggplot(data=dat[sub_dist,], mapping=aes(x=n, y=resid)) + geom_point(shape=1) + 
  xlab("n") + ylab("Residuals") +geom_hline(yintercept = 0) +facet_wrap(~State)

ggplot(data=dat[sub_dist,], mapping=aes(x=IRSD, y=resid)) + geom_point(shape=1) + 
  xlab("IRSD") + ylab("Residuals") +geom_hline(yintercept = 0) +facet_wrap(~State)

ggplot(data=dat[sub_dist,], mapping=aes(x=IRSAD, y=resid)) + geom_point(shape=1) + 
  xlab("IRSAD") + ylab("Residuals") +geom_hline(yintercept = 0) +facet_wrap(~State)

#ggplot(data=dat[sub_dist,], mapping=aes(x=P_Age_18_24_SA4, y=resid)) + geom_point(shape=1) + 
#  xlab("P_Age_18_24_SA4") + ylab("Residuals") +geom_hline(yintercept = 0) +facet_wrap(~State)
# ggplot(data=dat[sub_dist,], mapping=aes(x=P_Age_25_29_SA4, y=resid)) + geom_point(shape=1) + 
#   xlab("P_Age_25_29_SA4") + ylab("Residuals") +geom_hline(yintercept = 0) +facet_wrap(~State)

ggplot(data=dat[sub_dist,], mapping=aes(x=Bachelor_and_Higher_Education, y=resid)) + geom_point(shape=1) + 
  xlab("Bachelor_and_Higher_Education") + ylab("Residuals") +geom_hline(yintercept = 0) +facet_wrap(~State)

ggplot(data=dat[sub_dist,], mapping=aes(x=Bachelor_and_Higher_Education_SA4, y=resid)) + geom_point(shape=1) + 
  xlab("Bachelor_and_Higher_Education_SA4") + ylab("Residuals") +geom_hline(yintercept = 0) +facet_wrap(~State)

ggplot(data=dat[sub_dist,], mapping=aes(x=Certificate_III_IV, y=resid)) + geom_point(shape=1) + 
  xlab("Certificate_III_IV") + ylab("Residuals") +geom_hline(yintercept = 0) +facet_wrap(~State)

ggplot(data=dat[sub_dist,], mapping=aes(x=Certificate_III_IV_SA4, y=resid)) + geom_point(shape=1) + 
  xlab("Certificate_III_IV_SA4") + ylab("Residuals") +geom_hline(yintercept = 0) +facet_wrap(~State)

ggplot(data=dat[sub_dist,], mapping=aes(x=Indigenous, y=resid)) + geom_point(shape=1) + 
  xlab("Indigenous") + ylab("Residuals") +geom_hline(yintercept = 0) +facet_wrap(~State)

ggplot(data=dat[sub_dist,], mapping=aes(x=Indigenous_SA4, y=resid)) + geom_point(shape=1) + 
  xlab("Indigenous_SA4") + ylab("Residuals") +geom_hline(yintercept = 0) +facet_wrap(~State)



# # ### Posterior Probability P-value (PPP) Check
# #+ echo=FALSE, message=FALSE, progress=FALSE, dpi=200, out.width=1000
# 
# # At SA3 Level
# dat$pred_sqrt_se_NA <- dat$pred_sqrt_se
# dat$pred_sqrt_se_NA[!sub_dist] <- NA
# 
# 
# # At SA3 Level
# pred.sa3 <- predict(sim,fun. = function(x,p) x^2+dat$pred_sqrt_se^2,ppcheck = TRUE)
# pred.sa3.alt <- predict(sim,fun. = function(x,p) x^2+dat$pred_sqrt_se_NA^2,ppcheck = TRUE)
# # At SA4 Level
# WT <- dat$N
# SA4_2016 <- dat$SA4_2016
# pred.mean.sa4 <- predict(sim,fun. = function(x,p) tapply((x^2+dat$pred_sqrt_se_NA^2)*dat$N,dat$SA4_2016,sum,na.rm=T)/tapply(dat$N,dat$SA4_2016,sum,na.rm=T),ppcheck = TRUE)
# pred.mean.sa4 <- predict(sim,fun. = function(x,p) tapply((x^2+dat$pred_sqrt_se_NA^2)*WT,SA4_2016,sum,na.rm=T)/tapply(WT,SA4_2016,sum,na.rm=T),ppcheck = TRUE)
# # At State Level
# pred.mean.state <- predict(sim,fun. = function(x,p) tapply((x^2+dat$pred_sqrt_se_NA^2)*dat$N,dat$State,sum,na.rm=T)/tapply(dat$N,dat$State,sum,na.rm=T),ppcheck = TRUE)
# 
# # ## PPP Check at SA3
# 
# summary(attr(pred.sa3, "ppp")[sub_dist])
# hist(attr(pred.sa3, "ppp")[sub_dist])
# 
# # ## PPP Check at SA4
# summary(attr(pred.mean.sa4,"ppp"))
# PPP.SA4 <- data.frame(SA4=as.numeric(rownames(attr(pred.mean.sa4,"ppp"))),ppp=attr(pred.mean.sa4,"ppp"))
# colnames(PPP.SA4) <- c("SA4","PPP")
# plot(PPP.SA4$SA4,PPP.SA4$PPP)
# 
# # ## PPP Check at State
# summary(attr(pred.mean.state,"ppp"))
# PPP.State<- data.frame(State=(rownames(attr(pred.mean.state,"ppp"))),ppp=attr(pred.mean.state,"ppp"))
# colnames(PPP.State) <- c("State","PPP")
# PPP.State
