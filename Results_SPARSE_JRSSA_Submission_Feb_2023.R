# Results from ABS DataLab Output clearance------------------------------#

# Library
library(mcmcsae)
library(Matrix)
library(dplyr)
library(foreign)
library(haven)
library(data.table)
library(sp)
library(ggplot2)
library(ggpubr)
library(survey)
library(spdep)
library(sf)
library(tidyverse)
library(rgdal)
library(dotwhisker)
library(xtable)
library(readxl)
library(reshape2)
library(ggpmisc)
cl <- setup_cluster(3)
library(GIGrvg)
parallel::clusterEvalQ(cl, library(GIGrvg))

# GVF Based Plots : Figure 1 and Supplementary Figure 1 --------------------

data.old <- read.csv("C:/Users/u1107832/OneDrive - Australian National University/ABS DATA LAB OUTPUT CLEARANCE/ABS DataLab Output 20220504/dat_SA3_WME_Suppressed_safe.csv",header=T)
data.old <- read.csv("dat_SA3_WME_Suppressed_safe.csv",header=T)
table(data.old$n)
# NA 9, Suppressed 23, and partially Suppressed 15

sub_suppressed <- !is.na(data.old$n) & data.old$n!=9999 & data.old$n!=99999

data.old$mod_mean <- NA
data.old$mod_mean[sub_suppressed] <- data.old$mean[sub_suppressed]+0.00001
data.old$log_mean[sub_suppressed] <- log(data.old$mod_mean[sub_suppressed])
data.old$se_log_mean[sub_suppressed] <- sqrt( (1/(data.old$mod_mean[sub_suppressed])^2) * (data.old$se[sub_suppressed])^2 )
data.old$se_sqrt_mean[sub_suppressed] <- (data.old$se[sub_suppressed])/(2*sqrt(data.old$mod_mean[sub_suppressed]))

plot(data.old$n[sub_suppressed],data.old$se[sub_suppressed])
lines(data.old$n[sub_suppressed],data.old$pred_se[sub_suppressed],typ="p",col=2)
plot(data.old$n[sub_suppressed],data.old$se_sqrt_mean[sub_suppressed])
lines(data.old$n[sub_suppressed],data.old$pred_sqrt_se[sub_suppressed],typ="p",col=2)

summary.statistics <- rbind(
n=summary(data.old$n[sub_suppressed]),
mean=summary(data.old$mean[sub_suppressed]),
se=summary(data.old$se[sub_suppressed]),
pred_se=summary(data.old$pred_se[sub_suppressed]),
sqrt_se=summary(data.old$se_sqrt_mean[sub_suppressed]),
pred_sqrt_se=summary(data.old$pred_sqrt_se[sub_suppressed])
)

xtable(summary.statistics)

data.se <- data.old[sub_suppressed,][,c("n","mean","log_mean","sqrt_mean","se","se_log_mean","se_sqrt_mean","pred_se","pred_sqrt_se")]

cor.test(data.se$mean[data.se$mean>0.001],data.se$se[data.se$mean>0.001])
cor.test(data.se$sqrt_mean[data.se$sqrt_mean>0.001],data.se$se_sqrt_mean[data.se$sqrt_mean>0.001])
cor.test(data.se$log_mean[data.se$log_mean>-6],data.se$se_log_mean[data.se$log_mean>-6])


p1 <- ggplot(data.se,aes(x=mean,y=se))+geom_point()+ggtitle("Estimate vs. SE: Original Scale")+xlab("Estimate")+ylab("SE")
p2 <- ggplot(data.se,aes(x=n,y=se))+geom_point()+ggtitle("Sample size vs. SE")+xlab("Sample Size")+ylab("SE")
p3 <- ggplot(data.se,aes(x=sqrt_mean,y=se_sqrt_mean))+geom_point()+ggtitle("Estimate vs. SE: SQRT Scale")+xlab("Estimate")+ylab("SE")
p4 <- ggplot(data.se,aes(x=log_mean,y=se_log_mean))+geom_point()+ggtitle("Estimate vs. SE: Log Scale")+xlab("Estimate")+ylab("SE")

ggarrange(p1,p2,p3,p4,nrow=2,ncol=2)
ggsave("Estimate_vs_SE.pdf",device = "pdf",width = 8,height = 8)



data.se.1 <- melt(data.se,id.vars = c("n","mean"),measure.vars = c("se","pred_se"),variable.name = "SE",value.name = "se")
data.se.1$SE <- factor(data.se.1$SE,labels = c("Raw","GVF"))
data.se.2 <- melt(data.se,id.vars = c("n","sqrt_mean"),measure.vars = c("se_sqrt_mean","pred_sqrt_se"),variable.name = "SE",value.name = "se")
data.se.2$SE <- factor(data.se.2$SE,labels = c("Raw","GVF"))
p1 <- ggplot(data.se.1,aes(x=mean,y=se,color=SE))+geom_point()+ggtitle("Estimate vs. SE: Original Scale")+xlab("Estimate")+ylab("SE")
p2 <- ggplot(data.se.2,aes(x=sqrt_mean,y=se,color=SE))+geom_point()+ggtitle("Estimate vs. SE: SQRT Scale")+xlab("Estimate")+ylab("SE")
p3 <- ggplot(data.se.1,aes(x=n,y=se,color=SE))+geom_point()+ggtitle("Sample Size vs. SE: Original Scale")+xlab("Sample Size")+ylab("SE")
p4 <- ggplot(data.se.2,aes(x=n,y=se,color=SE))+geom_point()+ggtitle("Sample Size vs. SE: SQRT Scale")+xlab("Sample Size")+ylab("SE")
ggarrange(p1,p2,p3,p4,nrow=2,ncol=2,common.legend = TRUE)
ggsave("Relationship between estimate and SE.pdf",device = "pdf",width = 8,height = 8)

data.old$RA <- data.old$RA_NAME_2016
data.old$RA <- factor(data.old$RA,labels = c("Inner","Major Cities","Outer","Remote","Very Remote"))
data.old$RA <- factor(data.old$RA,levels = c("Major Cities","Inner","Outer","Remote","Very Remote"))

p1 <- ggplot(data = data.old[sub_suppressed,], aes(x=IRSD,y=mean,size=n)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point(aes(colour = RA,alpha=0.4,size=n))+ylab("Smoking prevalence")+
  guides(alpha = "none") 


p2 <- ggplot(data = data.old[sub_suppressed,], aes(x=IRSAD,y=mean,size=n)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point(aes(colour = RA,alpha=0.4,size=n))+ylab("Smoking prevalence")+
  guides(alpha = "none") 

p3 <- ggplot(data = data.old[sub_suppressed,], aes(x=IEO,y=mean,size=n)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point(aes(colour = RA,alpha=0.4,size=n))+ylab("Smoking prevalence")+
  guides(alpha = "none") 

p4 <- ggplot(data = data.old[sub_suppressed,], aes(x=IER,y=mean,size=n)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point(aes(colour = RA,alpha=0.4,size=n))+ylab("Smoking prevalence")+
  guides(alpha = "none") 


ggarrange(p1,p2,p3,p4,nrow=2,ncol=2,common.legend = TRUE)
ggsave("Relationship between SEIFA and Smoking.pdf",device = "pdf",width = 8,height = 8)




# Load Results : Excel file -------------------

National_Level_Estimates <- read_excel("National_Level_Estimates_Model_Type_1.xls")
State_Level_Estimates <- read_excel("State_Level_Estimates_Model_Type_1.xls")
SA4_Level_Estimates <- read_excel("SA4_Level_Estimates_Model_Type_1.xls")
SA3_Level_Estimates <- read_excel("SA3_Level_Estimates_Model_Type_1_Suppressed.xls")
SEIFA_SA4 <- read.csv("SEIFA_SA4.csv",header=T)
SEIFA_SA4 <- SEIFA_SA4[SEIFA_SA4$SA4%in%SA4_Level_Estimates$SA4_2016,]
SEIFA_SA4 <- SEIFA_SA4[order(SEIFA_SA4$SA4),]
SA4_Level_Estimates <- merge(SA4_Level_Estimates,SEIFA_SA4[,-1],by.x="SA4_2016",by.y="SA4",all.x=T)


SA3_domains_NA <- is.na(SA3_Level_Estimates$est.DIR)
SA3_domains_Suppressed <- !is.na(SA3_Level_Estimates$est.DIR) & (SA3_Level_Estimates$est.DIR==9999)
sub_dist_SA3_domains <- !is.na(SA3_Level_Estimates$est.DIR) & (SA3_Level_Estimates$est.DIR>0) & (SA3_Level_Estimates$est.DIR!=9999)

State_Level_Estimates$State <- factor(State_Level_Estimates$State,levels=c("NSW","VIC","QLD","SA","WA","TAS","NT","ACT"))
SA4_Level_Estimates$State <- factor(SA4_Level_Estimates$State,levels=c("NSW","VIC","QLD","SA","WA","TAS","NT","ACT"))
SA3_Level_Estimates$State <- factor(SA3_Level_Estimates$State,levels=c("NSW","VIC","QLD","SA","WA","TAS","NT","ACT"))

SA3_Level_Estimates$RA <- SA3_Level_Estimates$RA_NAME_2016
SA3_Level_Estimates$RA <- factor(SA3_Level_Estimates$RA,labels = c("Inner Regional","Major Cities","Outer Regional","Remote","Very Remote"))
SA3_Level_Estimates$RA <- factor(SA3_Level_Estimates$RA,levels = c("Major Cities","Inner Regional","Outer Regional","Remote","Very Remote"))

SA4_Level_Estimates$SA4_2016 <- as.factor(SA4_Level_Estimates$SA4_2016)
SA4_Level_Estimates$SA4_Name <- as.factor(SA4_Level_Estimates$SA4_NAME16)

SA3_Level_Estimates$SA4_2016 <- as.factor(SA3_Level_Estimates$SA4_2016)
SA3_Level_Estimates$SA4_Name <- as.factor(SA3_Level_Estimates$SA4_NAME16)
SA3_Level_Estimates$SA3_2016 <- as.factor(SA3_Level_Estimates$SA3_2016)
SA3_Level_Estimates$SA3_Name <- as.factor(SA3_Level_Estimates$SA3_NAME16)

# RB, ARB, RRSE, CR, CV, CV Ratio 
State_Level_Estimates$M2_RB_FH <- (State_Level_Estimates$M2_bias_FH/State_Level_Estimates$est.DIR)
State_Level_Estimates$M7_RB_FH <- (State_Level_Estimates$M7_bias_FH/State_Level_Estimates$est.DIR)
State_Level_Estimates$M11_RB_FH <- (State_Level_Estimates$M11_bias_FH/State_Level_Estimates$est.DIR)
State_Level_Estimates$M8_RB_FH <- (State_Level_Estimates$M8_bias_FH/State_Level_Estimates$est.DIR)
State_Level_Estimates$M9_RB_FH <- (State_Level_Estimates$M9_bias_FH/State_Level_Estimates$est.DIR)
State_Level_Estimates$M10_RB_FH <- (State_Level_Estimates$M10_bias_FH/State_Level_Estimates$est.DIR)

State_Level_Estimates$M2_RAB_FH <- abs(State_Level_Estimates$M2_bias_FH/State_Level_Estimates$est.DIR)
State_Level_Estimates$M7_RAB_FH <- abs(State_Level_Estimates$M7_bias_FH/State_Level_Estimates$est.DIR)
State_Level_Estimates$M8_RAB_FH <- abs(State_Level_Estimates$M8_bias_FH/State_Level_Estimates$est.DIR)
State_Level_Estimates$M9_RAB_FH <- abs(State_Level_Estimates$M9_bias_FH/State_Level_Estimates$est.DIR)
State_Level_Estimates$M10_RAB_FH <- abs(State_Level_Estimates$M10_bias_FH/State_Level_Estimates$est.DIR)
State_Level_Estimates$M11_RAB_FH <- abs(State_Level_Estimates$M11_bias_FH/State_Level_Estimates$est.DIR)

State_Level_Estimates$M2_RRSE_FH <- (State_Level_Estimates$se.DIR-State_Level_Estimates$M2_se_FH)/State_Level_Estimates$se.DIR*100
State_Level_Estimates$M7_RRSE_FH <- (State_Level_Estimates$se.DIR-State_Level_Estimates$M7_se_FH)/State_Level_Estimates$se.DIR*100
State_Level_Estimates$M8_RRSE_FH <- (State_Level_Estimates$se.DIR-State_Level_Estimates$M8_se_FH)/State_Level_Estimates$se.DIR*100
State_Level_Estimates$M9_RRSE_FH <- (State_Level_Estimates$se.DIR-State_Level_Estimates$M9_se_FH)/State_Level_Estimates$se.DIR*100
State_Level_Estimates$M11_RRSE_FH <- (State_Level_Estimates$se.DIR-State_Level_Estimates$M11_se_FH)/State_Level_Estimates$se.DIR*100
State_Level_Estimates$M10_RRSE_FH <- (State_Level_Estimates$se.DIR-State_Level_Estimates$M10_se_FH)/State_Level_Estimates$se.DIR*100

State_Level_Estimates$M2_CR_FH <- c (State_Level_Estimates$est.DIR > State_Level_Estimates$M2_est_FH_ll & State_Level_Estimates$est.DIR < State_Level_Estimates$M2_est_FH_ul)
State_Level_Estimates$M7_CR_FH <- c (State_Level_Estimates$est.DIR > State_Level_Estimates$M7_est_FH_ll & State_Level_Estimates$est.DIR < State_Level_Estimates$M7_est_FH_ul)
State_Level_Estimates$M8_CR_FH <- c (State_Level_Estimates$est.DIR > State_Level_Estimates$M8_est_FH_ll & State_Level_Estimates$est.DIR < State_Level_Estimates$M8_est_FH_ul)
State_Level_Estimates$M9_CR_FH <- c (State_Level_Estimates$est.DIR > State_Level_Estimates$M9_est_FH_ll & State_Level_Estimates$est.DIR < State_Level_Estimates$M9_est_FH_ul)
State_Level_Estimates$M11_CR_FH <- c (State_Level_Estimates$est.DIR > State_Level_Estimates$M11_est_FH_ll & State_Level_Estimates$est.DIR < State_Level_Estimates$M11_est_FH_ul)
State_Level_Estimates$M10_CR_FH <- c (State_Level_Estimates$est.DIR > State_Level_Estimates$M10_est_FH_ll & State_Level_Estimates$est.DIR < State_Level_Estimates$M10_est_FH_ul)

State_Level_Estimates$CV_DIR <- (State_Level_Estimates$se.DIR/State_Level_Estimates$est.DIR)*100
State_Level_Estimates$M7_CV_FH <- (State_Level_Estimates$M7_se_FH/State_Level_Estimates$M7_est_FH)*100
State_Level_Estimates$M2_CV_FH <- (State_Level_Estimates$M2_se_FH/State_Level_Estimates$M2_est_FH)*100
State_Level_Estimates$M8_CV_FH <- (State_Level_Estimates$M8_se_FH/State_Level_Estimates$M8_est_FH)*100
State_Level_Estimates$M9_CV_FH <- (State_Level_Estimates$M9_se_FH/State_Level_Estimates$M9_est_FH)*100
State_Level_Estimates$M10_CV_FH <- (State_Level_Estimates$M10_se_FH/State_Level_Estimates$M10_est_FH)*100
State_Level_Estimates$M11_CV_FH <- (State_Level_Estimates$M11_se_FH/State_Level_Estimates$M11_est_FH)*100

State_Level_Estimates$M7_CV_Ratio <- (State_Level_Estimates$M7_CV_FH/State_Level_Estimates$CV_DIR)*100
State_Level_Estimates$M2_CV_Ratio <- (State_Level_Estimates$M2_CV_FH/State_Level_Estimates$CV_DIR)*100
State_Level_Estimates$M8_CV_Ratio <- (State_Level_Estimates$M8_CV_FH/State_Level_Estimates$CV_DIR)*100
State_Level_Estimates$M9_CV_Ratio <- (State_Level_Estimates$M9_CV_FH/State_Level_Estimates$CV_DIR)*100
State_Level_Estimates$M10_CV_Ratio <- (State_Level_Estimates$M10_CV_FH/State_Level_Estimates$CV_DIR)*100
State_Level_Estimates$M11_CV_Ratio <- (State_Level_Estimates$M11_CV_FH/State_Level_Estimates$CV_DIR)*100


SA4_Level_Estimates$M2_RB_FH <- (SA4_Level_Estimates$M2_bias_FH/SA4_Level_Estimates$est.DIR)
SA4_Level_Estimates$M7_RB_FH <- (SA4_Level_Estimates$M7_bias_FH/SA4_Level_Estimates$est.DIR)
SA4_Level_Estimates$M8_RB_FH <- (SA4_Level_Estimates$M8_bias_FH/SA4_Level_Estimates$est.DIR)
SA4_Level_Estimates$M9_RB_FH <- (SA4_Level_Estimates$M9_bias_FH/SA4_Level_Estimates$est.DIR)
SA4_Level_Estimates$M11_RB_FH <- (SA4_Level_Estimates$M11_bias_FH/SA4_Level_Estimates$est.DIR)
SA4_Level_Estimates$M10_RB_FH <- (SA4_Level_Estimates$M10_bias_FH/SA4_Level_Estimates$est.DIR)

SA4_Level_Estimates$M2_RAB_FH <- abs(SA4_Level_Estimates$M2_bias_FH/SA4_Level_Estimates$est.DIR)
SA4_Level_Estimates$M7_RAB_FH <- abs(SA4_Level_Estimates$M7_bias_FH/SA4_Level_Estimates$est.DIR)
SA4_Level_Estimates$M8_RAB_FH <- abs(SA4_Level_Estimates$M8_bias_FH/SA4_Level_Estimates$est.DIR)
SA4_Level_Estimates$M9_RAB_FH <- abs(SA4_Level_Estimates$M9_bias_FH/SA4_Level_Estimates$est.DIR)
SA4_Level_Estimates$M11_RAB_FH <- abs(SA4_Level_Estimates$M11_bias_FH/SA4_Level_Estimates$est.DIR)
SA4_Level_Estimates$M10_RAB_FH <- abs(SA4_Level_Estimates$M10_bias_FH/SA4_Level_Estimates$est.DIR)

SA4_Level_Estimates$M2_RRSE_FH <- (SA4_Level_Estimates$se.DIR-SA4_Level_Estimates$M2_se_FH)/SA4_Level_Estimates$se.DIR*100
SA4_Level_Estimates$M7_RRSE_FH <- (SA4_Level_Estimates$se.DIR-SA4_Level_Estimates$M7_se_FH)/SA4_Level_Estimates$se.DIR*100
SA4_Level_Estimates$M8_RRSE_FH <- (SA4_Level_Estimates$se.DIR-SA4_Level_Estimates$M8_se_FH)/SA4_Level_Estimates$se.DIR*100
SA4_Level_Estimates$M9_RRSE_FH <- (SA4_Level_Estimates$se.DIR-SA4_Level_Estimates$M9_se_FH)/SA4_Level_Estimates$se.DIR*100
SA4_Level_Estimates$M11_RRSE_FH <- (SA4_Level_Estimates$se.DIR-SA4_Level_Estimates$M11_se_FH)/SA4_Level_Estimates$se.DIR*100
SA4_Level_Estimates$M10_RRSE_FH <- (SA4_Level_Estimates$se.DIR-SA4_Level_Estimates$M10_se_FH)/SA4_Level_Estimates$se.DIR*100

SA4_Level_Estimates$M2_CR_FH <- c (SA4_Level_Estimates$est.DIR > SA4_Level_Estimates$M2_est_FH_ll & SA4_Level_Estimates$est.DIR < SA4_Level_Estimates$M2_est_FH_ul)
SA4_Level_Estimates$M7_CR_FH <- c (SA4_Level_Estimates$est.DIR > SA4_Level_Estimates$M7_est_FH_ll & SA4_Level_Estimates$est.DIR < SA4_Level_Estimates$M7_est_FH_ul)
SA4_Level_Estimates$M8_CR_FH <- c (SA4_Level_Estimates$est.DIR > SA4_Level_Estimates$M8_est_FH_ll & SA4_Level_Estimates$est.DIR < SA4_Level_Estimates$M8_est_FH_ul)
SA4_Level_Estimates$M9_CR_FH <- c (SA4_Level_Estimates$est.DIR > SA4_Level_Estimates$M9_est_FH_ll & SA4_Level_Estimates$est.DIR < SA4_Level_Estimates$M9_est_FH_ul)
SA4_Level_Estimates$M11_CR_FH <- c (SA4_Level_Estimates$est.DIR > SA4_Level_Estimates$M11_est_FH_ll & SA4_Level_Estimates$est.DIR < SA4_Level_Estimates$M11_est_FH_ul)
SA4_Level_Estimates$M10_CR_FH <- c (SA4_Level_Estimates$est.DIR > SA4_Level_Estimates$M10_est_FH_ll & SA4_Level_Estimates$est.DIR < SA4_Level_Estimates$M10_est_FH_ul)

SA4_Level_Estimates$CV_DIR <- (SA4_Level_Estimates$se.DIR/SA4_Level_Estimates$est.DIR)*100
SA4_Level_Estimates$M7_CV_FH <- (SA4_Level_Estimates$M7_se_FH/SA4_Level_Estimates$M7_est_FH)*100
SA4_Level_Estimates$M2_CV_FH <- (SA4_Level_Estimates$M2_se_FH/SA4_Level_Estimates$M2_est_FH)*100
SA4_Level_Estimates$M8_CV_FH <- (SA4_Level_Estimates$M8_se_FH/SA4_Level_Estimates$M8_est_FH)*100
SA4_Level_Estimates$M9_CV_FH <- (SA4_Level_Estimates$M9_se_FH/SA4_Level_Estimates$M9_est_FH)*100
SA4_Level_Estimates$M10_CV_FH <- (SA4_Level_Estimates$M10_se_FH/SA4_Level_Estimates$M10_est_FH)*100
SA4_Level_Estimates$M11_CV_FH <- (SA4_Level_Estimates$M11_se_FH/SA4_Level_Estimates$M11_est_FH)*100

SA4_Level_Estimates$M7_CV_Ratio <- (SA4_Level_Estimates$M7_CV_FH/SA4_Level_Estimates$CV_DIR)*100
SA4_Level_Estimates$M2_CV_Ratio <- (SA4_Level_Estimates$M2_CV_FH/SA4_Level_Estimates$CV_DIR)*100
SA4_Level_Estimates$M8_CV_Ratio <- (SA4_Level_Estimates$M8_CV_FH/SA4_Level_Estimates$CV_DIR)*100
SA4_Level_Estimates$M9_CV_Ratio <- (SA4_Level_Estimates$M9_CV_FH/SA4_Level_Estimates$CV_DIR)*100
SA4_Level_Estimates$M10_CV_Ratio <- (SA4_Level_Estimates$M10_CV_FH/SA4_Level_Estimates$CV_DIR)*100
SA4_Level_Estimates$M11_CV_Ratio <- (SA4_Level_Estimates$M11_CV_FH/SA4_Level_Estimates$CV_DIR)*100


SA3_Level_Estimates$M2_RB_FH <- (SA3_Level_Estimates$M2_bias_FH/SA3_Level_Estimates$est.DIR)
SA3_Level_Estimates$M7_RB_FH <- (SA3_Level_Estimates$M7_bias_FH/SA3_Level_Estimates$est.DIR)
SA3_Level_Estimates$M8_RB_FH <- (SA3_Level_Estimates$M8_bias_FH/SA3_Level_Estimates$est.DIR)
SA3_Level_Estimates$M9_RB_FH <- (SA3_Level_Estimates$M9_bias_FH/SA3_Level_Estimates$est.DIR)
SA3_Level_Estimates$M11_RB_FH <- (SA3_Level_Estimates$M11_bias_FH/SA3_Level_Estimates$est.DIR)
SA3_Level_Estimates$M10_RB_FH <- (SA3_Level_Estimates$M10_bias_FH/SA3_Level_Estimates$est.DIR)

SA3_Level_Estimates$M2_RAB_FH <- abs(SA3_Level_Estimates$M2_bias_FH/SA3_Level_Estimates$est.DIR)
SA3_Level_Estimates$M7_RAB_FH <- abs(SA3_Level_Estimates$M7_bias_FH/SA3_Level_Estimates$est.DIR)
SA3_Level_Estimates$M8_RAB_FH <- abs(SA3_Level_Estimates$M8_bias_FH/SA3_Level_Estimates$est.DIR)
SA3_Level_Estimates$M9_RAB_FH <- abs(SA3_Level_Estimates$M9_bias_FH/SA3_Level_Estimates$est.DIR)
SA3_Level_Estimates$M11_RAB_FH <- abs(SA3_Level_Estimates$M11_bias_FH/SA3_Level_Estimates$est.DIR)
SA3_Level_Estimates$M10_RAB_FH <- abs(SA3_Level_Estimates$M10_bias_FH/SA3_Level_Estimates$est.DIR)

SA3_Level_Estimates$M2_RRSE_FH <- (SA3_Level_Estimates$se.DIR-SA3_Level_Estimates$M2_se_FH)/SA3_Level_Estimates$se.DIR*100
SA3_Level_Estimates$M7_RRSE_FH <- (SA3_Level_Estimates$se.DIR-SA3_Level_Estimates$M7_se_FH)/SA3_Level_Estimates$se.DIR*100
SA3_Level_Estimates$M8_RRSE_FH <- (SA3_Level_Estimates$se.DIR-SA3_Level_Estimates$M8_se_FH)/SA3_Level_Estimates$se.DIR*100
SA3_Level_Estimates$M9_RRSE_FH <- (SA3_Level_Estimates$se.DIR-SA3_Level_Estimates$M9_se_FH)/SA3_Level_Estimates$se.DIR*100
SA3_Level_Estimates$M11_RRSE_FH <- (SA3_Level_Estimates$se.DIR-SA3_Level_Estimates$M11_se_FH)/SA3_Level_Estimates$se.DIR*100
SA3_Level_Estimates$M10_RRSE_FH <- (SA3_Level_Estimates$se.DIR-SA3_Level_Estimates$M10_se_FH)/SA3_Level_Estimates$se.DIR*100


SA3_Level_Estimates$M2_CR_FH <- c (SA3_Level_Estimates$est.DIR > SA3_Level_Estimates$M2_est_FH_ll & SA3_Level_Estimates$est.DIR < SA3_Level_Estimates$M2_est_FH_ul)
SA3_Level_Estimates$M7_CR_FH <- c (SA3_Level_Estimates$est.DIR > SA3_Level_Estimates$M7_est_FH_ll & SA3_Level_Estimates$est.DIR < SA3_Level_Estimates$M7_est_FH_ul)
SA3_Level_Estimates$M8_CR_FH <- c (SA3_Level_Estimates$est.DIR > SA3_Level_Estimates$M8_est_FH_ll & SA3_Level_Estimates$est.DIR < SA3_Level_Estimates$M8_est_FH_ul)
SA3_Level_Estimates$M9_CR_FH <- c (SA3_Level_Estimates$est.DIR > SA3_Level_Estimates$M9_est_FH_ll & SA3_Level_Estimates$est.DIR < SA3_Level_Estimates$M9_est_FH_ul)
SA3_Level_Estimates$M11_CR_FH <- c (SA3_Level_Estimates$est.DIR > SA3_Level_Estimates$M11_est_FH_ll & SA3_Level_Estimates$est.DIR < SA3_Level_Estimates$M11_est_FH_ul)
SA3_Level_Estimates$M10_CR_FH <- c (SA3_Level_Estimates$est.DIR > SA3_Level_Estimates$M10_est_FH_ll & SA3_Level_Estimates$est.DIR < SA3_Level_Estimates$M10_est_FH_ul)

SA3_Level_Estimates$CV_DIR <- (SA3_Level_Estimates$se.DIR/SA3_Level_Estimates$est.DIR)*100
SA3_Level_Estimates$M7_CV_FH <- (SA3_Level_Estimates$M7_se_FH/SA3_Level_Estimates$M7_est_FH)*100
SA3_Level_Estimates$M2_CV_FH <- (SA3_Level_Estimates$M2_se_FH/SA3_Level_Estimates$M2_est_FH)*100
SA3_Level_Estimates$M8_CV_FH <- (SA3_Level_Estimates$M8_se_FH/SA3_Level_Estimates$M8_est_FH)*100
SA3_Level_Estimates$M9_CV_FH <- (SA3_Level_Estimates$M9_se_FH/SA3_Level_Estimates$M9_est_FH)*100
SA3_Level_Estimates$M10_CV_FH <- (SA3_Level_Estimates$M10_se_FH/SA3_Level_Estimates$M10_est_FH)*100
SA3_Level_Estimates$M11_CV_FH <- (SA3_Level_Estimates$M11_se_FH/SA3_Level_Estimates$M11_est_FH)*100

SA3_Level_Estimates$M7_CV_Ratio <- (SA3_Level_Estimates$M7_CV_FH/SA3_Level_Estimates$CV_DIR)*100
SA3_Level_Estimates$M2_CV_Ratio <- (SA3_Level_Estimates$M2_CV_FH/SA3_Level_Estimates$CV_DIR)*100
SA3_Level_Estimates$M8_CV_Ratio <- (SA3_Level_Estimates$M8_CV_FH/SA3_Level_Estimates$CV_DIR)*100
SA3_Level_Estimates$M9_CV_Ratio <- (SA3_Level_Estimates$M9_CV_FH/SA3_Level_Estimates$CV_DIR)*100
SA3_Level_Estimates$M10_CV_Ratio <- (SA3_Level_Estimates$M10_CV_FH/SA3_Level_Estimates$CV_DIR)*100
SA3_Level_Estimates$M11_CV_Ratio <- (SA3_Level_Estimates$M11_CV_FH/SA3_Level_Estimates$CV_DIR)*100


save(SA3_Level_Estimates,file="SA3_Level_Estimates.Rdata")
save(SA4_Level_Estimates,file="SA4_Level_Estimates.Rdata")
save(State_Level_Estimates,file="State_Level_Estimates.Rdata")

# Auxiliary Variable Selection plots ------------------------
load("SA3_Level_Estimates.Rdata")

# Relationship with Auxiliary Covariates 

sub_suppressed <- !is.na(SA3_Level_Estimates$n) & SA3_Level_Estimates$n!=9999

SA3_Level_Estimates$RA <- SA3_Level_Estimates$RA_NAME_2016
SA3_Level_Estimates$RA <- factor(SA3_Level_Estimates$RA,labels = c("Inner","Major Cities","Outer","Remote","Very Remote"))
SA3_Level_Estimates$RA <- factor(SA3_Level_Estimates$RA,levels = c("Major Cities","Inner","Outer","Remote","Very Remote"))

p1 <- SA3_Level_Estimates[sub_suppressed,] %>%
  ggplot( aes(x=RA, y=est.DIR, fill=RA,alpha=0.4)) +
  guides(alpha = "none") +
  geom_boxplot() +
  ggtitle("Distribution of smoking prevalence by RA") +
  xlab("Remoteness Area")+ylab("Smoking prevalence")


p2 <- SA3_Level_Estimates %>%
  ggplot( aes(x=RA, y=Indigenous_SA3, fill=RA,alpha=0.40)) +
  guides(alpha = "none") +
  geom_boxplot() +
  ggtitle("Distribution of the proportion of Indigenous adults by RA") +
  xlab("Remoteness Area")+ylab("Proportion of Indigenous adults")

ggarrange(p1,p2,nrow=2,ncol=1,common.legend = TRUE)
ggsave("Relationship between RA and Smoking.pdf",device = "pdf",width = 8,height = 8)


p3 <- ggplot(data = SA3_Level_Estimates, aes(x=sqrt(Indigenous_SA3),y=IRSD,size=N,alpha=0.40)) +
  guides(alpha = "none") +
  geom_point(aes(colour = RA))+ylab("IRSD")+xlab("sqrt(Proportion of Indigenous adults)")+
  ggtitle("IRSD and Proportion of Indigenous adults by RA and number of adult population")
p4 <- ggplot(data = SA3_Level_Estimates[sub_suppressed,], aes(x=sqrt(Indigenous_SA3),y=IRSD,size=n,alpha=0.40)) +
  guides(alpha = "none") + 
  geom_point(aes(colour = RA))+ylab("IRSD")+xlab("sqrt(Proportion of Indigenous adults)")+
  ggtitle("IRSD and Proportion of Indigenous adults by RA and number of sampled adult population")
SA3_Level_Estimates$Prevalence <- SA3_Level_Estimates$est.DIR
p5 <- ggplot(data = SA3_Level_Estimates[sub_suppressed,], aes(x=sqrt(Indigenous_SA3),y=IRSD,size=Prevalence,alpha=0.40)) +
  guides(alpha = "none") +
  geom_point(aes(colour = RA))+ylab("IRSD")+xlab("sqrt(Proportion of Indigenous adults)")+
  ggtitle("IRSD and Proportion of Indigenous adults by RA and smoking prevalence")

ggarrange(p3,p4,p5,nrow=3,ncol=1,common.legend = FALSE)

ggsave("Relationship between IRSD, Indigenous and RA.pdf",device = "pdf",width = 8,height = 12)


p1 <- ggplot(data = SA3_Level_Estimates[sub_suppressed,], aes(x=Bachelor_and_Higher_Education_SA3,y=est.DIR,size=n)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point(aes(colour = RA,alpha=0.4,size=n))+ylab("Smoking prevalence")+xlab("Bachelor or higher education")+
  guides(alpha = "none")

p2 <- ggplot(data = SA3_Level_Estimates[sub_suppressed,], aes(x=Certificate_III_IV_SA3,y=est.DIR,size=n)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point(aes(colour = RA,alpha=0.4,size=n))+ylab("Smoking prevalence")+xlab("Certificate III/IV education")+
  guides(alpha = "none")

p3 <- ggplot(data = SA3_Level_Estimates[sub_suppressed,], aes(x=Diploma_SA3,y=est.DIR,size=n)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point(aes(colour = RA,alpha=0.4,size=n))+ylab("Smoking prevalence")+xlab("Diploma")+
  guides(alpha = "none")


ggarrange(p1,p2,p3,nrow=2,ncol=2,common.legend = TRUE)
ggsave("Relationship between Education and Smoking.pdf",device = "pdf",width = 8,height = 8)


p4 <- ggplot(data = SA3_Level_Estimates[sub_suppressed,], aes(x=sqrt(Male_SA3),y=est.DIR,size=n)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point(aes(colour = RA,alpha=0.4,size=n))+ylab("Smoking prevalence")+xlab("SQRT(Proportion of male adults)")+
  guides(alpha = "none")

p5 <- ggplot(data = SA3_Level_Estimates[sub_suppressed,], aes(x=sqrt(Australian_SA3),y=est.DIR,size=n)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point(aes(colour = RA,alpha=0.4,size=n))+ylab("Smoking prevalence")+xlab("SQRT(Proportion of Australian adults)")+
  guides(alpha = "none")

p6 <- ggplot(data = SA3_Level_Estimates[sub_suppressed,], aes(x=sqrt(Indigenous_SA3),y=est.DIR,size=n)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point(aes(colour = RA,alpha=0.4,size=n))+ylab("Smoking prevalence")+xlab("SQRT(Proportion of Indigenous adults)")+
  guides(alpha = "none")


ggarrange(p4,p5,p6,nrow=2,ncol=2,common.legend = TRUE)
ggsave("Relationship between Social Characteristics and Smoking.pdf",device = "pdf",width = 8,height = 8)

p7 <- ggplot(data = SA3_Level_Estimates[sub_suppressed,], aes(x=Employed_SA3,y=est.DIR,size=n)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point(aes(colour = RA,alpha=0.4,size=n))+ylab("Smoking prevalence")+xlab("Proportion of employed adults")

p8 <- ggplot(data = SA3_Level_Estimates[sub_suppressed,], aes(x=Unemployed_SA3,y=est.DIR,size=n)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point(aes(colour = RA,alpha=0.4,size=n))+ylab("Smoking prevalence")+xlab("Proportion of unemployed adults")

ggarrange(p7,p8,nrow=2,ncol=1,common.legend = TRUE)
ggsave("Relationship between Employment and Smoking.pdf",device = "pdf",width = 6,height = 8)


# Results directly from ABS Datalab 

dat.nat <- read_excel("National_Level_Estimate_Model_Wirh_Without_State.xlsx")
dat.nat$Model <- as.factor(dat.nat$Model)
dat.nat$Model <- factor(dat.nat$Model,labels=c("Bachelor","Bachelor+State","Certificate","Certificate+State","DIR"))
dat.nat$Model <- factor(dat.nat$Model,levels=c("DIR","Bachelor","Bachelor+State","Certificate","Certificate+State"))

pd <- position_dodge(0.5)
p <- ggplot(data=dat.nat,mapping = aes(x=yr,y=Estimate,color=Model,group=Model))
p <- p + geom_point(shape=16,position=pd,size=2)+xlab("Nation")+ylab("Smoking Prevalence")+ggtitle("National Level Estimate")+scale_x_discrete(labels = NULL, breaks = NULL) 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position = pd,width=0.5,size=1)
p <- p + coord_flip() 
p1 <- p
ggsave("dat.nat.with.without.sate.pdf",p,device = "pdf",width = 8,height = 8)

dat.state <- read_excel("State_Level_Estimate_Model_Wirh_Without_State.xlsx")
dat.state$Model <- as.factor(dat.state$Model)
dat.state$Model <- factor(dat.state$Model,labels=c("Bachelor","Bachelor+State","Certificate","Certificate+State","DIR"))
dat.state$Model <- factor(dat.state$Model,levels=c("DIR","Bachelor","Bachelor+State","Certificate","Certificate+State"))
dat.state$State <- factor(dat.state$State, levels = dat.state$State[dat.state$Model=="DIR"][order(dat.state$Estimate[dat.state$Model=="DIR"])])

pd <- position_dodge(0.5)
p <- ggplot(data=dat.state,mapping = aes(x=State,y=Estimate,color=Model,group=Model))
p <- p + geom_point(shape=16,position=pd,size=2)+xlab("State")+ylab("Smoking Prevalence")+ggtitle("State Level Estimate")
p <- p + geom_errorbar(aes(ymin=Estimate-2*se,ymax=Estimate+2*se),position = pd,width=0.5,size=1)
p <- p + coord_flip()
p2 <- p

ggsave("dat.state.with.without.sate.pdf",p,device = "pdf",width = 8,height = 8)

ggarrange(p1,p2,nrow = 2,ncol=1,heights = c(1,3),common.legend = TRUE,legend = "right")

ggsave("Nat.state.with.without.sate.pdf",device = "pdf",width = 8,height = 10)


# Information Criteria and # Regression Parameters -------------------
IC_Value_Model <- read_excel("IC_Value_Model_Type_1.xls")

xtable(IC_Value_Model)


Regression_Coefficient_Model <- read_excel("Regression_Coefficient_Model_Type_1.xls")
Regression_Coefficient_Model[9,c(2,5,8,11,14,17,20,23,26,29)] <- Regression_Coefficient_Model[9,c(2,5,8,11,14,17,20,23,26,29)]*1000
Regression_Coefficient_Model[11,c(2,5,8,11,14,17,20,23,26,29)] <- Regression_Coefficient_Model[11,c(2,5,8,11,14,17,20,23,26,29)]*100
Regression_Coefficient_Model[12,c(2,5,8,11,14,17,20,23,26,29)] <- Regression_Coefficient_Model[12,c(2,5,8,11,14,17,20,23,26,29)]*100
Regression_Coefficient_Model[13,c(2,5,8,11,14,17,20,23,26,29)] <- Regression_Coefficient_Model[13,c(2,5,8,11,14,17,20,23,26,29)]*100
Regression_Coefficient_Model[16,c(2,5,8,11,14,17,20,23,26,29)] <- Regression_Coefficient_Model[16,c(2,5,8,11,14,17,20,23,26,29)]*100
Regression_Coefficient_Model[17,c(2,5,8,11,14,17,20,23,26,29)] <- Regression_Coefficient_Model[17,c(2,5,8,11,14,17,20,23,26,29)]*100

xtable(Regression_Coefficient_Model)

xtable(Regression_Coefficient_Model[,c(1:16)])
xtable(Regression_Coefficient_Model[,c(1,17:31)])


Regression_Coefficient_Model$Mean_M1
Regression_Coefficient_Model$t_M1
Regression_Coefficient_Model$Mean_M2
Regression_Coefficient_Model$t_M2

RR <- Regression_Coefficient_Model[,c(1:2,4,5,7,8,10,11,13,14,16,17,19,20,22,23,25,26,28,29,31)]

RR[9,c(2:21)] <- RR[9,c(2:21)]*1000

xtable(RR)

# Model 11
Regression_Coefficient_Model[9,c(32:33)] <- Regression_Coefficient_Model[9,c(32:33)]*1000
Regression_Coefficient_Model[11,c(32:33)] <- Regression_Coefficient_Model[11,c(32:33)]*100
Regression_Coefficient_Model[12,c(32:33)] <- Regression_Coefficient_Model[12,c(32:33)]*100
Regression_Coefficient_Model[13,c(32:33)] <- Regression_Coefficient_Model[13,c(32:33)]*100
Regression_Coefficient_Model[16,c(32:33)] <- Regression_Coefficient_Model[16,c(32:33)]*100
Regression_Coefficient_Model[17,c(32:33)] <- Regression_Coefficient_Model[17,c(32:33)]*100

xtable(Regression_Coefficient_Model[,c(32:34)])

# Variance Parameters
Variance_Parameters_Model <- read_excel("Variance_Parameters_Model_Type_1.xls")
t(Variance_Parameters_Model)


# Model Diagnostics ---------------------------------

# RB, ARB, RRSE, CR and CV Ratio ------- : State#

Performance_Mesures <- function(Est,se,ll,ul,M_Est,M_se,M_ll,M_ul){
  RB <- (M_Est - Est)/Est*100
  ARB <- abs(M_Est - Est)/Est*100
  RRSE <- (se - M_se)/se*100
  CV_Ratio <- (M_se/M_Est)/(se/Est)*100
  CR <- as.numeric(c(Est<M_ul & Est>M_ll))
  data.frame(RB=RB,ARB=ARB,RRSE=RRSE,CV_Ratio=CV_Ratio,CR=CR)
}

# State, SA4, SA3 level Calculation -------------------#


State_Level_PM_M1 <- Performance_Mesures(Est=State_Level_Estimates$est.DIR,se=State_Level_Estimates$se.DIR,
                                         ll=State_Level_Estimates$ll,ul=State_Level_Estimates$ul,
                                         M_Est=State_Level_Estimates$M1_est_FH,M_se=State_Level_Estimates$M1_se_FH,
                                         M_ll=State_Level_Estimates$M1_est_FH_ll,M_ul=State_Level_Estimates$M1_est_FH_ul)
State_Level_PM_M2 <- Performance_Mesures(Est=State_Level_Estimates$est.DIR,se=State_Level_Estimates$se.DIR,
                                         ll=State_Level_Estimates$ll,ul=State_Level_Estimates$ul,
                                         M_Est=State_Level_Estimates$M2_est_FH,M_se=State_Level_Estimates$M2_se_FH,
                                         M_ll=State_Level_Estimates$M2_est_FH_ll,M_ul=State_Level_Estimates$M2_est_FH_ul)
State_Level_PM_M3 <- Performance_Mesures(Est=State_Level_Estimates$est.DIR,se=State_Level_Estimates$se.DIR,
                                         ll=State_Level_Estimates$ll,ul=State_Level_Estimates$ul,
                                         M_Est=State_Level_Estimates$M3_est_FH,M_se=State_Level_Estimates$M3_se_FH,
                                         M_ll=State_Level_Estimates$M3_est_FH_ll,M_ul=State_Level_Estimates$M3_est_FH_ul)

State_Level_PM_M4 <- Performance_Mesures(Est=State_Level_Estimates$est.DIR,se=State_Level_Estimates$se.DIR,
                                         ll=State_Level_Estimates$ll,ul=State_Level_Estimates$ul,
                                         M_Est=State_Level_Estimates$M4_est_FH,M_se=State_Level_Estimates$M4_se_FH,
                                         M_ll=State_Level_Estimates$M4_est_FH_ll,M_ul=State_Level_Estimates$M4_est_FH_ul)

State_Level_PM_M5 <- Performance_Mesures(Est=State_Level_Estimates$est.DIR,se=State_Level_Estimates$se.DIR,
                                         ll=State_Level_Estimates$ll,ul=State_Level_Estimates$ul,
                                         M_Est=State_Level_Estimates$M5_est_FH,M_se=State_Level_Estimates$M5_se_FH,
                                         M_ll=State_Level_Estimates$M5_est_FH_ll,M_ul=State_Level_Estimates$M5_est_FH_ul)

State_Level_PM_M6 <- Performance_Mesures(Est=State_Level_Estimates$est.DIR,se=State_Level_Estimates$se.DIR,
                                         ll=State_Level_Estimates$ll,ul=State_Level_Estimates$ul,
                                         M_Est=State_Level_Estimates$M6_est_FH,M_se=State_Level_Estimates$M6_se_FH,
                                         M_ll=State_Level_Estimates$M6_est_FH_ll,M_ul=State_Level_Estimates$M6_est_FH_ul)

State_Level_PM_M7 <- Performance_Mesures(Est=State_Level_Estimates$est.DIR,se=State_Level_Estimates$se.DIR,
                                         ll=State_Level_Estimates$ll,ul=State_Level_Estimates$ul,
                                         M_Est=State_Level_Estimates$M7_est_FH,M_se=State_Level_Estimates$M7_se_FH,
                                         M_ll=State_Level_Estimates$M7_est_FH_ll,M_ul=State_Level_Estimates$M7_est_FH_ul)

State_Level_PM_M8 <- Performance_Mesures(Est=State_Level_Estimates$est.DIR,se=State_Level_Estimates$se.DIR,
                                         ll=State_Level_Estimates$ll,ul=State_Level_Estimates$ul,
                                         M_Est=State_Level_Estimates$M8_est_FH,M_se=State_Level_Estimates$M8_se_FH,
                                         M_ll=State_Level_Estimates$M8_est_FH_ll,M_ul=State_Level_Estimates$M8_est_FH_ul)

State_Level_PM_M9 <- Performance_Mesures(Est=State_Level_Estimates$est.DIR,se=State_Level_Estimates$se.DIR,
                                         ll=State_Level_Estimates$ll,ul=State_Level_Estimates$ul,
                                         M_Est=State_Level_Estimates$M9_est_FH,M_se=State_Level_Estimates$M9_se_FH,
                                         M_ll=State_Level_Estimates$M9_est_FH_ll,M_ul=State_Level_Estimates$M9_est_FH_ul)

State_Level_PM_M10 <- Performance_Mesures(Est=State_Level_Estimates$est.DIR,se=State_Level_Estimates$se.DIR,
                                          ll=State_Level_Estimates$ll,ul=State_Level_Estimates$ul,
                                          M_Est=State_Level_Estimates$M10_est_FH,M_se=State_Level_Estimates$M10_se_FH,
                                          M_ll=State_Level_Estimates$M10_est_FH_ll,M_ul=State_Level_Estimates$M10_est_FH_ul)

State_Level_PM_M11 <- Performance_Mesures(Est=State_Level_Estimates$est.DIR,se=State_Level_Estimates$se.DIR,
                                          ll=State_Level_Estimates$ll,ul=State_Level_Estimates$ul,
                                          M_Est=State_Level_Estimates$M11_est_FH,M_se=State_Level_Estimates$M11_se_FH,
                                          M_ll=State_Level_Estimates$M11_est_FH_ll,M_ul=State_Level_Estimates$M11_est_FH_ul)


SA4_Level_PM_M1 <- Performance_Mesures(Est=SA4_Level_Estimates$est.DIR,se=SA4_Level_Estimates$se.DIR,
                                         ll=SA4_Level_Estimates$ll,ul=SA4_Level_Estimates$ul,
                                         M_Est=SA4_Level_Estimates$M1_est_FH,M_se=SA4_Level_Estimates$M1_se_FH,
                                         M_ll=SA4_Level_Estimates$M1_est_FH_ll,M_ul=SA4_Level_Estimates$M1_est_FH_ul)
SA4_Level_PM_M2 <- Performance_Mesures(Est=SA4_Level_Estimates$est.DIR,se=SA4_Level_Estimates$se.DIR,
                                         ll=SA4_Level_Estimates$ll,ul=SA4_Level_Estimates$ul,
                                         M_Est=SA4_Level_Estimates$M2_est_FH,M_se=SA4_Level_Estimates$M2_se_FH,
                                         M_ll=SA4_Level_Estimates$M2_est_FH_ll,M_ul=SA4_Level_Estimates$M2_est_FH_ul)
SA4_Level_PM_M3 <- Performance_Mesures(Est=SA4_Level_Estimates$est.DIR,se=SA4_Level_Estimates$se.DIR,
                                         ll=SA4_Level_Estimates$ll,ul=SA4_Level_Estimates$ul,
                                         M_Est=SA4_Level_Estimates$M3_est_FH,M_se=SA4_Level_Estimates$M3_se_FH,
                                         M_ll=SA4_Level_Estimates$M3_est_FH_ll,M_ul=SA4_Level_Estimates$M3_est_FH_ul)

SA4_Level_PM_M4 <- Performance_Mesures(Est=SA4_Level_Estimates$est.DIR,se=SA4_Level_Estimates$se.DIR,
                                         ll=SA4_Level_Estimates$ll,ul=SA4_Level_Estimates$ul,
                                         M_Est=SA4_Level_Estimates$M4_est_FH,M_se=SA4_Level_Estimates$M4_se_FH,
                                         M_ll=SA4_Level_Estimates$M4_est_FH_ll,M_ul=SA4_Level_Estimates$M4_est_FH_ul)

SA4_Level_PM_M5 <- Performance_Mesures(Est=SA4_Level_Estimates$est.DIR,se=SA4_Level_Estimates$se.DIR,
                                         ll=SA4_Level_Estimates$ll,ul=SA4_Level_Estimates$ul,
                                         M_Est=SA4_Level_Estimates$M5_est_FH,M_se=SA4_Level_Estimates$M5_se_FH,
                                         M_ll=SA4_Level_Estimates$M5_est_FH_ll,M_ul=SA4_Level_Estimates$M5_est_FH_ul)

SA4_Level_PM_M6 <- Performance_Mesures(Est=SA4_Level_Estimates$est.DIR,se=SA4_Level_Estimates$se.DIR,
                                         ll=SA4_Level_Estimates$ll,ul=SA4_Level_Estimates$ul,
                                         M_Est=SA4_Level_Estimates$M6_est_FH,M_se=SA4_Level_Estimates$M6_se_FH,
                                         M_ll=SA4_Level_Estimates$M6_est_FH_ll,M_ul=SA4_Level_Estimates$M6_est_FH_ul)

SA4_Level_PM_M7 <- Performance_Mesures(Est=SA4_Level_Estimates$est.DIR,se=SA4_Level_Estimates$se.DIR,
                                         ll=SA4_Level_Estimates$ll,ul=SA4_Level_Estimates$ul,
                                         M_Est=SA4_Level_Estimates$M7_est_FH,M_se=SA4_Level_Estimates$M7_se_FH,
                                         M_ll=SA4_Level_Estimates$M7_est_FH_ll,M_ul=SA4_Level_Estimates$M7_est_FH_ul)

SA4_Level_PM_M8 <- Performance_Mesures(Est=SA4_Level_Estimates$est.DIR,se=SA4_Level_Estimates$se.DIR,
                                         ll=SA4_Level_Estimates$ll,ul=SA4_Level_Estimates$ul,
                                         M_Est=SA4_Level_Estimates$M8_est_FH,M_se=SA4_Level_Estimates$M8_se_FH,
                                         M_ll=SA4_Level_Estimates$M8_est_FH_ll,M_ul=SA4_Level_Estimates$M8_est_FH_ul)

SA4_Level_PM_M9 <- Performance_Mesures(Est=SA4_Level_Estimates$est.DIR,se=SA4_Level_Estimates$se.DIR,
                                         ll=SA4_Level_Estimates$ll,ul=SA4_Level_Estimates$ul,
                                         M_Est=SA4_Level_Estimates$M9_est_FH,M_se=SA4_Level_Estimates$M9_se_FH,
                                         M_ll=SA4_Level_Estimates$M9_est_FH_ll,M_ul=SA4_Level_Estimates$M9_est_FH_ul)

SA4_Level_PM_M10 <- Performance_Mesures(Est=SA4_Level_Estimates$est.DIR,se=SA4_Level_Estimates$se.DIR,
                                          ll=SA4_Level_Estimates$ll,ul=SA4_Level_Estimates$ul,
                                          M_Est=SA4_Level_Estimates$M10_est_FH,M_se=SA4_Level_Estimates$M10_se_FH,
                                          M_ll=SA4_Level_Estimates$M10_est_FH_ll,M_ul=SA4_Level_Estimates$M10_est_FH_ul)

SA4_Level_PM_M11 <- Performance_Mesures(Est=SA4_Level_Estimates$est.DIR,se=SA4_Level_Estimates$se.DIR,
                                          ll=SA4_Level_Estimates$ll,ul=SA4_Level_Estimates$ul,
                                          M_Est=SA4_Level_Estimates$M11_est_FH,M_se=SA4_Level_Estimates$M11_se_FH,
                                          M_ll=SA4_Level_Estimates$M11_est_FH_ll,M_ul=SA4_Level_Estimates$M11_est_FH_ul)


SA3_Level_Estimates_Suppressed <- SA3_Level_Estimates[sub_dist_SA3_domains,]

SA3_Level_PM_M1 <- Performance_Mesures(Est=SA3_Level_Estimates_Suppressed$est.DIR,se=SA3_Level_Estimates_Suppressed$se.DIR,
                                       ll=SA3_Level_Estimates_Suppressed$ll,ul=SA3_Level_Estimates_Suppressed$ul,
                                       M_Est=SA3_Level_Estimates_Suppressed$M1_est_FH,M_se=SA3_Level_Estimates_Suppressed$M1_se_FH,
                                       M_ll=SA3_Level_Estimates_Suppressed$M1_est_FH_ll,M_ul=SA3_Level_Estimates_Suppressed$M1_est_FH_ul)
SA3_Level_PM_M2 <- Performance_Mesures(Est=SA3_Level_Estimates_Suppressed$est.DIR,se=SA3_Level_Estimates_Suppressed$se.DIR,
                                       ll=SA3_Level_Estimates_Suppressed$ll,ul=SA3_Level_Estimates_Suppressed$ul,
                                       M_Est=SA3_Level_Estimates_Suppressed$M2_est_FH,M_se=SA3_Level_Estimates_Suppressed$M2_se_FH,
                                       M_ll=SA3_Level_Estimates_Suppressed$M2_est_FH_ll,M_ul=SA3_Level_Estimates_Suppressed$M2_est_FH_ul)
SA3_Level_PM_M3 <- Performance_Mesures(Est=SA3_Level_Estimates_Suppressed$est.DIR,se=SA3_Level_Estimates_Suppressed$se.DIR,
                                       ll=SA3_Level_Estimates_Suppressed$ll,ul=SA3_Level_Estimates_Suppressed$ul,
                                       M_Est=SA3_Level_Estimates_Suppressed$M3_est_FH,M_se=SA3_Level_Estimates_Suppressed$M3_se_FH,
                                       M_ll=SA3_Level_Estimates_Suppressed$M3_est_FH_ll,M_ul=SA3_Level_Estimates_Suppressed$M3_est_FH_ul)
SA3_Level_PM_M4 <- Performance_Mesures(Est=SA3_Level_Estimates_Suppressed$est.DIR,se=SA3_Level_Estimates_Suppressed$se.DIR,
                                       ll=SA3_Level_Estimates_Suppressed$ll,ul=SA3_Level_Estimates_Suppressed$ul,
                                       M_Est=SA3_Level_Estimates_Suppressed$M4_est_FH,M_se=SA3_Level_Estimates_Suppressed$M4_se_FH,
                                       M_ll=SA3_Level_Estimates_Suppressed$M4_est_FH_ll,M_ul=SA3_Level_Estimates_Suppressed$M4_est_FH_ul)
SA3_Level_PM_M5 <- Performance_Mesures(Est=SA3_Level_Estimates_Suppressed$est.DIR,se=SA3_Level_Estimates_Suppressed$se.DIR,
                                       ll=SA3_Level_Estimates_Suppressed$ll,ul=SA3_Level_Estimates_Suppressed$ul,
                                       M_Est=SA3_Level_Estimates_Suppressed$M5_est_FH,M_se=SA3_Level_Estimates_Suppressed$M5_se_FH,
                                       M_ll=SA3_Level_Estimates_Suppressed$M5_est_FH_ll,M_ul=SA3_Level_Estimates_Suppressed$M5_est_FH_ul)
SA3_Level_PM_M6 <- Performance_Mesures(Est=SA3_Level_Estimates_Suppressed$est.DIR,se=SA3_Level_Estimates_Suppressed$se.DIR,
                                       ll=SA3_Level_Estimates_Suppressed$ll,ul=SA3_Level_Estimates_Suppressed$ul,
                                       M_Est=SA3_Level_Estimates_Suppressed$M6_est_FH,M_se=SA3_Level_Estimates_Suppressed$M6_se_FH,
                                       M_ll=SA3_Level_Estimates_Suppressed$M6_est_FH_ll,M_ul=SA3_Level_Estimates_Suppressed$M6_est_FH_ul)
SA3_Level_PM_M7 <- Performance_Mesures(Est=SA3_Level_Estimates_Suppressed$est.DIR,se=SA3_Level_Estimates_Suppressed$se.DIR,
                                       ll=SA3_Level_Estimates_Suppressed$ll,ul=SA3_Level_Estimates_Suppressed$ul,
                                       M_Est=SA3_Level_Estimates_Suppressed$M7_est_FH,M_se=SA3_Level_Estimates_Suppressed$M7_se_FH,
                                       M_ll=SA3_Level_Estimates_Suppressed$M7_est_FH_ll,M_ul=SA3_Level_Estimates_Suppressed$M7_est_FH_ul)
SA3_Level_PM_M8 <- Performance_Mesures(Est=SA3_Level_Estimates_Suppressed$est.DIR,se=SA3_Level_Estimates_Suppressed$se.DIR,
                                       ll=SA3_Level_Estimates_Suppressed$ll,ul=SA3_Level_Estimates_Suppressed$ul,
                                       M_Est=SA3_Level_Estimates_Suppressed$M8_est_FH,M_se=SA3_Level_Estimates_Suppressed$M8_se_FH,
                                       M_ll=SA3_Level_Estimates_Suppressed$M8_est_FH_ll,M_ul=SA3_Level_Estimates_Suppressed$M8_est_FH_ul)
SA3_Level_PM_M9 <- Performance_Mesures(Est=SA3_Level_Estimates_Suppressed$est.DIR,se=SA3_Level_Estimates_Suppressed$se.DIR,
                                       ll=SA3_Level_Estimates_Suppressed$ll,ul=SA3_Level_Estimates_Suppressed$ul,
                                       M_Est=SA3_Level_Estimates_Suppressed$M9_est_FH,M_se=SA3_Level_Estimates_Suppressed$M9_se_FH,
                                       M_ll=SA3_Level_Estimates_Suppressed$M9_est_FH_ll,M_ul=SA3_Level_Estimates_Suppressed$M9_est_FH_ul)
SA3_Level_PM_M10 <- Performance_Mesures(Est=SA3_Level_Estimates_Suppressed$est.DIR,se=SA3_Level_Estimates_Suppressed$se.DIR,
                                       ll=SA3_Level_Estimates_Suppressed$ll,ul=SA3_Level_Estimates_Suppressed$ul,
                                       M_Est=SA3_Level_Estimates_Suppressed$M10_est_FH,M_se=SA3_Level_Estimates_Suppressed$M10_se_FH,
                                       M_ll=SA3_Level_Estimates_Suppressed$M10_est_FH_ll,M_ul=SA3_Level_Estimates_Suppressed$M10_est_FH_ul)
SA3_Level_PM_M11 <- Performance_Mesures(Est=SA3_Level_Estimates_Suppressed$est.DIR,se=SA3_Level_Estimates_Suppressed$se.DIR,
                                       ll=SA3_Level_Estimates_Suppressed$ll,ul=SA3_Level_Estimates_Suppressed$ul,
                                       M_Est=SA3_Level_Estimates_Suppressed$M11_est_FH,M_se=SA3_Level_Estimates_Suppressed$M11_se_FH,
                                       M_ll=SA3_Level_Estimates_Suppressed$M11_est_FH_ll,M_ul=SA3_Level_Estimates_Suppressed$M11_est_FH_ul)

# Summary Statistics of RB ------------------------------#

State_RB <- rbind(summary(State_Level_PM_M2$RB),
      summary(State_Level_PM_M8$RB),
      summary(State_Level_PM_M11$RB),
      summary(State_Level_PM_M7$RB),
      summary(State_Level_PM_M9$RB),
      summary(State_Level_PM_M10$RB)
)

SA4_RB <- rbind(summary(SA4_Level_PM_M2$RB),
summary(SA4_Level_PM_M8$RB),
summary(SA4_Level_PM_M11$RB),
summary(SA4_Level_PM_M7$RB),
summary(SA4_Level_PM_M9$RB),
summary(SA4_Level_PM_M10$RB)
)

SA3_RB <- rbind(summary(SA3_Level_PM_M2$RB),
      summary(SA3_Level_PM_M8$RB),
      summary(SA3_Level_PM_M11$RB),
      summary(SA3_Level_PM_M7$RB),
      summary(SA3_Level_PM_M9$RB),
      summary(SA3_Level_PM_M10$RB)
)

RB_State_SA4_SA3 <- rbind(State_RB,SA4_RB,SA3_RB)
rownames(RB_State_SA4_SA3) <- rep(c("M_A","M_B","M_C","M_D","M_E","M_F"),3)

xtable(RB_State_SA4_SA3)

# SA3 with all domains

SA3_RB_ALL_Domains <- read_excel("Summary_RB_SA3_Model_Type_1.xls")

xtable(SA3_RB_ALL_Domains)


# Summary Statistics of ARB ------------------------------#



State_ARB <- rbind(summary(State_Level_PM_M2$ARB),
                  summary(State_Level_PM_M8$ARB),
                  summary(State_Level_PM_M11$ARB),
                  summary(State_Level_PM_M7$ARB),
                  summary(State_Level_PM_M9$ARB),
                  summary(State_Level_PM_M10$ARB)
)

SA4_ARB <- rbind(summary(SA4_Level_PM_M2$ARB),
                summary(SA4_Level_PM_M8$ARB),
                summary(SA4_Level_PM_M11$ARB),
                summary(SA4_Level_PM_M7$ARB),
                summary(SA4_Level_PM_M9$ARB),
                summary(SA4_Level_PM_M10$ARB)
)

SA3_ARB <- rbind(summary(SA3_Level_PM_M2$ARB),
                summary(SA3_Level_PM_M8$ARB),
                summary(SA3_Level_PM_M11$ARB),
                summary(SA3_Level_PM_M7$ARB),
                summary(SA3_Level_PM_M9$ARB),
                summary(SA3_Level_PM_M10$ARB)
)

ARB_State_SA4_SA3 <- rbind(State_ARB,SA4_ARB,SA3_ARB)
rownames(ARB_State_SA4_SA3) <- rep(c("M_A","M_B","M_C","M_D","M_E","M_F"),3)

xtable(ARB_State_SA4_SA3)

# SA3 with all domains

SA3_ARB_ALL_Domains <- read_excel("Summary_ARB_SA3_Model_Type_1.xls")

xtable(SA3_ARB_ALL_Domains)


# Summary Statistics of RRSE ------------------------------#



State_RRSE <- rbind(summary(State_Level_PM_M2$RRSE),
                   summary(State_Level_PM_M8$RRSE),
                   summary(State_Level_PM_M11$RRSE),
                   summary(State_Level_PM_M7$RRSE),
                   summary(State_Level_PM_M9$RRSE),
                   summary(State_Level_PM_M10$RRSE)
)

SA4_RRSE <- rbind(summary(SA4_Level_PM_M2$RRSE),
                 summary(SA4_Level_PM_M8$RRSE),
                 summary(SA4_Level_PM_M11$RRSE),
                 summary(SA4_Level_PM_M7$RRSE),
                 summary(SA4_Level_PM_M9$RRSE),
                 summary(SA4_Level_PM_M10$RRSE)
)

SA3_RRSE <- rbind(summary(SA3_Level_PM_M2$RRSE),
                 summary(SA3_Level_PM_M8$RRSE),
                 summary(SA3_Level_PM_M11$RRSE),
                 summary(SA3_Level_PM_M7$RRSE),
                 summary(SA3_Level_PM_M9$RRSE),
                 summary(SA3_Level_PM_M10$RRSE)
)

RRSE_State_SA4_SA3 <- rbind(State_RRSE,SA4_RRSE,SA3_RRSE)
rownames(RRSE_State_SA4_SA3) <- rep(c("M_A","M_B","M_C","M_D","M_E","M_F"),3)

xtable(RRSE_State_SA4_SA3)

# SA3 with all domains

SA3_RRSE_ALL_Domains <- read_excel("Summary_RRSE_SA3_Model_Type_1.xls")

xtable(SA3_RRSE_ALL_Domains)


# Summary statistics of CV Ratio ------------------------------#

State_CV_Ratio <- rbind(summary(State_Level_PM_M2$CV_Ratio),
                    summary(State_Level_PM_M8$CV_Ratio),
                    summary(State_Level_PM_M11$CV_Ratio),
                    summary(State_Level_PM_M7$CV_Ratio),
                    summary(State_Level_PM_M9$CV_Ratio),
                    summary(State_Level_PM_M10$CV_Ratio)
)

SA4_CV_Ratio <- rbind(summary(SA4_Level_PM_M2$CV_Ratio),
                  summary(SA4_Level_PM_M8$CV_Ratio),
                  summary(SA4_Level_PM_M11$CV_Ratio),
                  summary(SA4_Level_PM_M7$CV_Ratio),
                  summary(SA4_Level_PM_M9$CV_Ratio),
                  summary(SA4_Level_PM_M10$CV_Ratio)
)

SA3_CV_Ratio <- rbind(summary(SA3_Level_PM_M2$CV_Ratio),
                  summary(SA3_Level_PM_M8$CV_Ratio),
                  summary(SA3_Level_PM_M11$CV_Ratio),
                  summary(SA3_Level_PM_M7$CV_Ratio),
                  summary(SA3_Level_PM_M9$CV_Ratio),
                  summary(SA3_Level_PM_M10$CV_Ratio)
)

CV_Ratio_State_SA4_SA3 <- rbind(State_CV_Ratio,SA4_CV_Ratio,SA3_CV_Ratio)
rownames(CV_Ratio_State_SA4_SA3) <- rep(c("M_A","M_B","M_C","M_D","M_E","M_F"),3)

xtable(CV_Ratio_State_SA4_SA3)

# Summary statistics of CV  ------------------------------#



State_CV <- rbind(summary(State_Level_Estimates$CV_DIR),
                        summary(State_Level_Estimates$M2_CV_FH),
                        summary(State_Level_Estimates$M8_CV_FH),
                        summary(State_Level_Estimates$M11_CV_FH),
                        summary(State_Level_Estimates$M7_CV_FH),
                        summary(State_Level_Estimates$M9_CV_FH),
                        summary(State_Level_Estimates$M10_CV_FH)
)

SA4_CV <- rbind(summary(SA4_Level_Estimates$CV_DIR),
                  summary(SA4_Level_Estimates$M2_CV_FH),
                  summary(SA4_Level_Estimates$M8_CV_FH),
                  summary(SA4_Level_Estimates$M11_CV_FH),
                summary(SA4_Level_Estimates$M7_CV_FH),
                summary(SA4_Level_Estimates$M9_CV_FH),
                  summary(SA4_Level_Estimates$M10_CV_FH)
)


SA3_CV <- rbind(summary(SA3_Level_Estimates$CV_DIR[sub_dist_SA3_domains]),
                summary(SA3_Level_Estimates$M2_CV_FH),
                summary(SA3_Level_Estimates$M8_CV_FH),
                summary(SA3_Level_Estimates$M11_CV_FH),
                summary(SA3_Level_Estimates$M7_CV_FH),
                summary(SA3_Level_Estimates$M9_CV_FH),
                summary(SA3_Level_Estimates$M10_CV_FH)
)


CV_State_SA4_SA3 <- rbind(State_CV,SA4_CV,SA3_CV)
rownames(CV_State_SA4_SA3) <- rep(c("DIR","M_A","M_B","M_C","M_D","M_E","M_F"),3)

xtable(CV_State_SA4_SA3)


# Coverage Rate ------------------------------#

# SA3 level 

sub_dist_SA3_domains <- !is.na(SA3_Level_Estimates$est.DIR) & (SA3_Level_Estimates$est.DIR!=9999) & (SA3_Level_Estimates$est.DIR>0) & !is.na(SA3_Level_Estimates$se.DIR) & (SA3_Level_Estimates$se.DIR!=9999)
sub_dist_SA3_domains_small_n <- !is.na(SA3_Level_Estimates$est.DIR) & (SA3_Level_Estimates$est.DIR!=9999) & (SA3_Level_Estimates$est.DIR>0) & SA3_Level_Estimates$n<=50
sub_dist_SA3_domains_large_n <- !is.na(SA3_Level_Estimates$est.DIR) & (SA3_Level_Estimates$est.DIR!=9999) & (SA3_Level_Estimates$est.DIR>0) & SA3_Level_Estimates$n>50

CR_SA3 <-
  t(
    rbind(c(mean(SA3_Level_Estimates$M2_CR_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M2_CR_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M2_CR_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M8_CR_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M8_CR_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M8_CR_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M7_CR_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M7_CR_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M7_CR_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M9_CR_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M9_CR_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M9_CR_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M11_CR_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M11_CR_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M11_CR_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M10_CR_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M10_CR_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M10_CR_FH[sub_dist_SA3_domains_large_n]))
    )
  )


# SA4 level

CR_SA4 <-
  t(
    rbind(c(mean(SA4_Level_Estimates$M2_CR_FH),mean(SA4_Level_Estimates$M2_CR_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M2_CR_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M8_CR_FH),mean(SA4_Level_Estimates$M8_CR_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M8_CR_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M11_CR_FH),mean(SA4_Level_Estimates$M11_CR_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M11_CR_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M7_CR_FH),mean(SA4_Level_Estimates$M7_CR_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M7_CR_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M9_CR_FH),mean(SA4_Level_Estimates$M9_CR_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M9_CR_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M10_CR_FH),mean(SA4_Level_Estimates$M10_CR_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M10_CR_FH[SA4_Level_Estimates$n > 148]))
    )
  )

CR_State <-
  t(
    rbind(c(mean(State_Level_Estimates$M2_CR_FH)),
          c(mean(State_Level_Estimates$M8_CR_FH)),
          c(mean(State_Level_Estimates$M11_CR_FH)),
          c(mean(State_Level_Estimates$M7_CR_FH)),
          c(mean(State_Level_Estimates$M9_CR_FH)),
          c(mean(State_Level_Estimates$M10_CR_FH))
    )
  )

CR_SA3_SA4 <- rbind(CR_SA3,CR_SA4,CR_State)*100

rownames(CR_SA3_SA4) <- c(rep(c("Overall","n=<Median","n>Median"),2),"Overall")
colnames(CR_SA3_SA4) <- (c("M2","M8","M11","M7","M9","M10"))
xtable(CR_SA3_SA4,digits = 2)


# Coverage by State ----------------------------#


CR_SA3_SA4_by_State <- 

100*rbind (  

rbind(
tapply(SA3_Level_Estimates$M2_CR_FH[sub_dist_SA3_domains],SA3_Level_Estimates$State[sub_dist_SA3_domains],mean),
tapply(SA3_Level_Estimates$M8_CR_FH[sub_dist_SA3_domains],SA3_Level_Estimates$State[sub_dist_SA3_domains],mean),
tapply(SA3_Level_Estimates$M11_CR_FH[sub_dist_SA3_domains],SA3_Level_Estimates$State[sub_dist_SA3_domains],mean),
tapply(SA3_Level_Estimates$M7_CR_FH[sub_dist_SA3_domains],SA3_Level_Estimates$State[sub_dist_SA3_domains],mean),
tapply(SA3_Level_Estimates$M9_CR_FH[sub_dist_SA3_domains],SA3_Level_Estimates$State[sub_dist_SA3_domains],mean),
tapply(SA3_Level_Estimates$M10_CR_FH[sub_dist_SA3_domains],SA3_Level_Estimates$State[sub_dist_SA3_domains],mean)
),


rbind(
  tapply(SA3_Level_Estimates$M2_CR_FH[sub_dist_SA3_domains_small_n],SA3_Level_Estimates$State[sub_dist_SA3_domains_small_n],mean),
  tapply(SA3_Level_Estimates$M8_CR_FH[sub_dist_SA3_domains_small_n],SA3_Level_Estimates$State[sub_dist_SA3_domains_small_n],mean),
  tapply(SA3_Level_Estimates$M11_CR_FH[sub_dist_SA3_domains_small_n],SA3_Level_Estimates$State[sub_dist_SA3_domains_small_n],mean),
  tapply(SA3_Level_Estimates$M7_CR_FH[sub_dist_SA3_domains_small_n],SA3_Level_Estimates$State[sub_dist_SA3_domains_small_n],mean),
  tapply(SA3_Level_Estimates$M9_CR_FH[sub_dist_SA3_domains_small_n],SA3_Level_Estimates$State[sub_dist_SA3_domains_small_n],mean),
  tapply(SA3_Level_Estimates$M10_CR_FH[sub_dist_SA3_domains_small_n],SA3_Level_Estimates$State[sub_dist_SA3_domains_small_n],mean)
),


rbind(
  tapply(SA3_Level_Estimates$M2_CR_FH[sub_dist_SA3_domains_large_n],SA3_Level_Estimates$State[sub_dist_SA3_domains_large_n],mean),
  tapply(SA3_Level_Estimates$M8_CR_FH[sub_dist_SA3_domains_large_n],SA3_Level_Estimates$State[sub_dist_SA3_domains_large_n],mean),
  tapply(SA3_Level_Estimates$M11_CR_FH[sub_dist_SA3_domains_large_n],SA3_Level_Estimates$State[sub_dist_SA3_domains_large_n],mean),
  tapply(SA3_Level_Estimates$M7_CR_FH[sub_dist_SA3_domains_large_n],SA3_Level_Estimates$State[sub_dist_SA3_domains_large_n],mean),
  tapply(SA3_Level_Estimates$M9_CR_FH[sub_dist_SA3_domains_large_n],SA3_Level_Estimates$State[sub_dist_SA3_domains_large_n],mean),
  tapply(SA3_Level_Estimates$M10_CR_FH[sub_dist_SA3_domains_large_n],SA3_Level_Estimates$State[sub_dist_SA3_domains_large_n],mean)
),


rbind(
  tapply(SA4_Level_Estimates$M2_CR_FH,SA4_Level_Estimates$State,mean),
  tapply(SA4_Level_Estimates$M8_CR_FH,SA4_Level_Estimates$State,mean),
  tapply(SA4_Level_Estimates$M11_CR_FH,SA4_Level_Estimates$State,mean),
  tapply(SA4_Level_Estimates$M7_CR_FH,SA4_Level_Estimates$State,mean),
  tapply(SA4_Level_Estimates$M9_CR_FH,SA4_Level_Estimates$State,mean),
  tapply(SA4_Level_Estimates$M10_CR_FH,SA4_Level_Estimates$State,mean)
),


rbind(
  tapply(SA4_Level_Estimates$M2_CR_FH[SA4_Level_Estimates$n <= 148],SA4_Level_Estimates$State[SA4_Level_Estimates$n <= 148],mean),
  tapply(SA4_Level_Estimates$M8_CR_FH[SA4_Level_Estimates$n <= 148],SA4_Level_Estimates$State[SA4_Level_Estimates$n <= 148],mean),
  tapply(SA4_Level_Estimates$M11_CR_FH[SA4_Level_Estimates$n <= 148],SA4_Level_Estimates$State[SA4_Level_Estimates$n <= 148],mean),
  tapply(SA4_Level_Estimates$M7_CR_FH[SA4_Level_Estimates$n <= 148],SA4_Level_Estimates$State[SA4_Level_Estimates$n <= 148],mean),
  tapply(SA4_Level_Estimates$M9_CR_FH[SA4_Level_Estimates$n <= 148],SA4_Level_Estimates$State[SA4_Level_Estimates$n <= 148],mean),
  tapply(SA4_Level_Estimates$M10_CR_FH[SA4_Level_Estimates$n <= 148],SA4_Level_Estimates$State[SA4_Level_Estimates$n <= 148],mean)
),


rbind(
  tapply(SA4_Level_Estimates$M2_CR_FH[SA4_Level_Estimates$n > 148],SA4_Level_Estimates$State[SA4_Level_Estimates$n > 148],mean),
  tapply(SA4_Level_Estimates$M8_CR_FH[SA4_Level_Estimates$n > 148],SA4_Level_Estimates$State[SA4_Level_Estimates$n > 148],mean),
  tapply(SA4_Level_Estimates$M11_CR_FH[SA4_Level_Estimates$n > 148],SA4_Level_Estimates$State[SA4_Level_Estimates$n > 148],mean),
  tapply(SA4_Level_Estimates$M7_CR_FH[SA4_Level_Estimates$n > 148],SA4_Level_Estimates$State[SA4_Level_Estimates$n > 148],mean),
  tapply(SA4_Level_Estimates$M9_CR_FH[SA4_Level_Estimates$n > 148],SA4_Level_Estimates$State[SA4_Level_Estimates$n > 148],mean),
  tapply(SA4_Level_Estimates$M10_CR_FH[SA4_Level_Estimates$n > 148],SA4_Level_Estimates$State[SA4_Level_Estimates$n > 148],mean)
)

)

rownames(CR_SA3_SA4_by_State) <- rep(c("M_A","M_B","M_C","M_D","M_E","M_F"),6)

xtable(CR_SA3_SA4_by_State)

Coverage_Rate_State_SA4_SA3_Model_Type_1 <- read_excel("Coverage_Rate_State_SA4_SA3_Model_Type_1.xls")
Coverage_Rate_State_SA4_SA3_Model_Type_1$State

xtable(Coverage_Rate_State_SA4_SA3_Model_Type_1[c(2,8,11,7,9,10),c(2:4)]*100)


# RB, ARB and RRSE by the Sample size --------------------#

RB_SA4 <-
  t(
    rbind(c(mean(SA4_Level_Estimates$M2_RB_FH),mean(SA4_Level_Estimates$M2_RB_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M2_RB_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M8_RB_FH),mean(SA4_Level_Estimates$M8_RB_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M8_RB_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M11_RB_FH),mean(SA4_Level_Estimates$M11_RB_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M11_RB_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M7_RB_FH),mean(SA4_Level_Estimates$M7_RB_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M7_RB_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M9_RB_FH),mean(SA4_Level_Estimates$M9_RB_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M9_RB_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M10_RB_FH),mean(SA4_Level_Estimates$M10_RB_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M10_RB_FH[SA4_Level_Estimates$n > 148]))
    )
  )

xtable(RB_SA4*100)

ARB_SA4 <-
  t(
    rbind(c(mean(SA4_Level_Estimates$M2_RAB_FH),mean(SA4_Level_Estimates$M2_RAB_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M2_RAB_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M8_RAB_FH),mean(SA4_Level_Estimates$M8_RAB_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M8_RAB_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M11_RAB_FH),mean(SA4_Level_Estimates$M11_RAB_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M11_RAB_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M7_RAB_FH),mean(SA4_Level_Estimates$M7_RAB_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M7_RAB_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M9_RAB_FH),mean(SA4_Level_Estimates$M9_RAB_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M9_RAB_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M10_RAB_FH),mean(SA4_Level_Estimates$M10_RAB_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M10_RAB_FH[SA4_Level_Estimates$n > 148]))
    )
  )

xtable(ARB_SA4*100)

RRSE_SA4 <-
  t(
    rbind(c(mean(SA4_Level_Estimates$M2_RRSE_FH),mean(SA4_Level_Estimates$M2_RRSE_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M2_RRSE_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M8_RRSE_FH),mean(SA4_Level_Estimates$M8_RRSE_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M8_RRSE_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M11_RRSE_FH),mean(SA4_Level_Estimates$M11_RRSE_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M11_RRSE_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M7_RRSE_FH),mean(SA4_Level_Estimates$M7_RRSE_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M7_RRSE_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M9_RRSE_FH),mean(SA4_Level_Estimates$M9_RRSE_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M9_RRSE_FH[SA4_Level_Estimates$n > 148])),
          c(mean(SA4_Level_Estimates$M10_RRSE_FH),mean(SA4_Level_Estimates$M10_RRSE_FH[SA4_Level_Estimates$n <= 148]),mean(SA4_Level_Estimates$M10_RRSE_FH[SA4_Level_Estimates$n > 148]))
    )
  )

xtable(RRSE_SA4)


RB_SA3 <-
  t(
    rbind(c(mean(SA3_Level_Estimates$M2_RB_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M2_RB_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M2_RB_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M8_RB_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M8_RB_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M8_RB_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M7_RB_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M7_RB_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M7_RB_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M9_RB_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M9_RB_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M9_RB_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M11_RB_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M11_RB_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M11_RB_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M10_RB_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M10_RB_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M10_RB_FH[sub_dist_SA3_domains_large_n]))
    )
  )


xtable(RB_SA3*100)

ARB_SA3 <-
  t(
    rbind(c(mean(SA3_Level_Estimates$M2_RAB_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M2_RAB_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M2_RAB_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M8_RAB_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M8_RAB_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M8_RAB_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M7_RAB_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M7_RAB_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M7_RAB_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M9_RAB_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M9_RAB_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M9_RAB_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M11_RAB_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M11_RAB_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M11_RAB_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M10_RAB_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M10_RAB_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M10_RAB_FH[sub_dist_SA3_domains_large_n]))
    )
  )


xtable(ARB_SA3*100)


RRSE_SA3 <-
  t(
    rbind(c(mean(SA3_Level_Estimates$M2_RRSE_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M2_RRSE_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M2_RRSE_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M8_RRSE_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M8_RRSE_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M8_RRSE_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M7_RRSE_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M7_RRSE_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M7_RRSE_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M9_RRSE_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M9_RRSE_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M9_RRSE_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M11_RRSE_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M11_RRSE_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M11_RRSE_FH[sub_dist_SA3_domains_large_n])),
          c(mean(SA3_Level_Estimates$M10_RRSE_FH[sub_dist_SA3_domains]),mean(SA3_Level_Estimates$M10_RRSE_FH[sub_dist_SA3_domains_small_n]),mean(SA3_Level_Estimates$M10_RRSE_FH[sub_dist_SA3_domains_large_n]))
    )
  )


xtable(RRSE_SA3)


# Boxplot of Bias ---------------#

data.sa4.bias <- melt(SA4_Level_Estimates,id.vars = "State",measure.vars = c("M2_bias_FH","M8_bias_FH","M11_bias_FH","M7_bias_FH","M9_bias_FH","M10_bias_FH"),variable.name = "Model",value.name = "Bias")
data.sa3.bias <- melt(SA3_Level_Estimates_Suppressed,id.vars = "State",measure.vars = c("M2_bias_FH","M8_bias_FH","M11_bias_FH","M7_bias_FH","M9_bias_FH","M10_bias_FH"),variable.name = "Model",value.name = "Bias")
ggplot(data.sa4.bias,mapping=aes(x=Model,y=Bias))+geom_boxplot()+facet_wrap(~State)
ggplot(data.sa3.bias,mapping=aes(x=Model,y=Bias))+geom_boxplot()+facet_wrap(~State)
ggplot(data.sa3.bias,mapping=aes(x=Model,y=Bias,fill=State))+geom_boxplot()


p1 <- ggplot(data.sa3.bias,mapping=aes(x=State,y=Bias,fill=Model))+geom_boxplot()+geom_hline(yintercept = 0)+ggtitle("Bias by Model and State: SA3 level")

p1 <- p1 + scale_fill_manual(name = "Model", values = c("green3", "skyblue","orange","lightgreen","lightblue","red"), 
                             labels = expression(M[A], M[B],  M[C], M[D], M[E],  M[F])) 

p2 <- ggplot(data.sa4.bias,mapping=aes(x=State,y=Bias,fill=Model))+geom_boxplot()+geom_hline(yintercept = 0)+ggtitle("Bias by Model and State: SA4 level")

p2 <- p2 + scale_fill_manual(name = "Model", values = c("green3", "skyblue","orange","lightgreen","lightblue","red"), 
                           labels = expression(M[A], M[B],  M[C], M[D], M[E],  M[F])) 


p3 <- ggplot(data.sa4.bias,mapping=aes(x=State,y=Bias,fill=Model))+geom_boxplot(outlier.shape = NA)+geom_hline(yintercept = 0)+ggtitle("Bias by Model and State: SA4 level")+ylim(-0.12,0.1)

p3 <- p3 + scale_fill_manual(name = "Model", values = c("green3", "skyblue","orange","lightgreen","lightblue","red"), 
                           labels = expression(M[A], M[B],  M[C], M[D], M[E],  M[F])) 

ggarrange(p1,p2,p3,nrow=3,ncol=1,common.legend = TRUE)

ggsave("Bias_SA3_SA4_Mode_by_State",device = "pdf",width = 8,height = 12)


SA3_Level_Estimates_Suppressed$RA_NAME_2016_m2 <- SA3_Level_Estimates_Suppressed$RA_NAME_2016
SA3_Level_Estimates_Suppressed$RA_NAME_2016_m2[SA3_Level_Estimates_Suppressed$RA_NAME_2016_m2=="Very Remote Australia"] <- "Remote Australia"
SA3_Level_Estimates_Suppressed$RA_NAME_2016_m2 <- factor(SA3_Level_Estimates_Suppressed$RA_NAME_2016_m2,labels = c("Inner Regional","Major Cities","Outer Regional","Remote & Very Remote"))
SA3_Level_Estimates_Suppressed$RA_NAME_2016_m2 <- factor(SA3_Level_Estimates_Suppressed$RA_NAME_2016_m2,levels = c("Major Cities","Inner Regional","Outer Regional","Remote & Very Remote"))
data.sa3.bias.RA <- melt(SA3_Level_Estimates_Suppressed,id.vars = "RA_NAME_2016_m2",measure.vars = c("M2_bias_FH","M8_bias_FH","M11_bias_FH","M7_bias_FH","M9_bias_FH","M10_bias_FH"),variable.name = "Model",value.name = "Bias")

ggplot(data.sa3.bias.RA,mapping=aes(x=Model,y=Bias))+geom_boxplot()+facet_wrap(~RA_NAME_2016_m2)
ggplot(data.sa3.bias.RA,mapping=aes(x=Model,y=Bias,fill=RA_NAME_2016_m2))+geom_boxplot()


p4 <- ggplot(data.sa3.bias.RA,mapping=aes(x=RA_NAME_2016_m2,y=Bias,fill=Model))+geom_boxplot()+geom_hline(yintercept = 0)+ggtitle("Bias by Model and RA: SA3 level")+xlab("RA")

p4 <- p4 + scale_fill_manual(name = "Model", values = c("green3", "skyblue","orange","lightgreen","lightblue","red"), 
                             labels = expression(M[A], M[B],  M[C], M[D], M[E],  M[F])) 

ggarrange(p2,p3,p1,p4,nrow=2,ncol=2,common.legend = TRUE)

ggsave("Bias_SA3_SA4_Mode_by_State_RA.pdf",device = "pdf",width = 8,height = 12)


# Model Comparison ------------------#

# Compare SE between Model with RA_Name_2016_m (Model-1) and Model with RA_Name_2016_m2 (Model-2)
range_se <- range(State_Level_Estimates$M1_se_FH,State_Level_Estimates$M2_se_FH)
plot(State_Level_Estimates$M1_se_FH,State_Level_Estimates$M2_se_FH,xlab="M1",ylab="M2",xlim=range_se,ylim=range_se,main="SE: M1 vs M2")
abline(0,1)
range_bias_est <- range(State_Level_Estimates$M1_bias_FH,State_Level_Estimates$M2_bias_FH)
plot(State_Level_Estimates$M1_bias_FH,State_Level_Estimates$M2_bias_FH,xlab="M1",ylab="M2",xlim=range_bias_est,ylim=range_bias_est,main="Bias: M1 vs M2")
abline(0,1)

range_se <- range(SA4_Level_Estimates$M1_se_FH,SA4_Level_Estimates$M2_se_FH)
plot(SA4_Level_Estimates$M1_se_FH,SA4_Level_Estimates$M2_se_FH,xlab="M1",ylab="M2",xlim=range_se,ylim=range_se,main="SE: M1 vs M2")
abline(0,1)
range_bias_est <- range(SA4_Level_Estimates$M1_bias_FH,SA4_Level_Estimates$M2_bias_FH)
plot(SA4_Level_Estimates$M1_bias_FH,SA4_Level_Estimates$M2_bias_FH,xlab="M1",ylab="M2",xlim=range_bias_est,ylim=range_bias_est,main="Bias: M1 vs M2")
abline(0,1)



range_se <- range(SA3_Level_Estimates$M1_se_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M2_se_FH[sub_dist_SA3_domains])
plot(SA3_Level_Estimates$M1_se_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M2_se_FH[sub_dist_SA3_domains],xlab="M1",ylab="M2",xlim=range_se,ylim=range_se,main="SE: M1 vs M2")
abline(0,1)
range_bias_est <- range(SA3_Level_Estimates$M1_bias_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M2_bias_FH[sub_dist_SA3_domains],na.rm = TRUE)
plot(SA3_Level_Estimates$M1_bias_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M2_bias_FH[sub_dist_SA3_domains],xlab="M1",ylab="M2",xlim=range_bias_est,ylim=range_bias_est,main="Bias: M1 vs M2")
abline(0,1)

# Remoteness of SA3 domains -------------#

table(SA3_Level_Estimates$RA_NAME_2016)
# Inner Regional Australia Major Cities of Australia  Outer Regional Australia          Remote Australia 
# 81                       190                        47                         8 
# Very Remote Australia 
# 8 

table(SA3_Level_Estimates$RA_NAME_2016[SA3_domains_NA])
# Inner Regional Australia Outer Regional Australia    Very Remote Australia 
#       2                        1                        6 

table(SA3_Level_Estimates$RA_NAME_2016[SA3_domains_Suppressed])

SA3_Level_Estimates$RA_NAME_2016_m <- SA3_Level_Estimates$RA_NAME_2016
SA3_Level_Estimates$RA_NAME_2016_m[SA3_Level_Estimates$RA_NAME_2016=="Remote Australia"] <- "Remote & Very Remote"
SA3_Level_Estimates$RA_NAME_2016_m[SA3_Level_Estimates$RA_NAME_2016=="Very Remote Australia"] <- "Remote & Very Remote"

table(SA3_Level_Estimates$RA_NAME_2016_m)
table(SA3_Level_Estimates$RA_NAME_2016_m[sub_dist_SA3_domains])

SA3_Level_Estimates$RA_NAME_2016_m2 <- SA3_Level_Estimates$RA_NAME_2016_m
SA3_Level_Estimates$RA_NAME_2016_m2[SA3_Level_Estimates$RA_NAME_2016=="Remote & Very Remote"] <- "Outer, Remote & Very Remote"
SA3_Level_Estimates$RA_NAME_2016_m2[SA3_Level_Estimates$RA_NAME_2016=="Outer Regional Australia"] <- "Outer, Remote & Very Remote"

table(SA3_Level_Estimates$RA_NAME_2016_m[SA3_domains_Suppressed])

SA3_Level_Estimates$RA <- SA3_Level_Estimates$RA_NAME_2016

p1 <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M1_bias_FH<9999 & !is.na(SA3_Level_Estimates$M1_bias_FH),],aes(x=M1_bias_FH,y=M2_bias_FH,color=RA))+
  geom_point()+xlab("M1")+ylab("M2")+ggtitle("Bias by RA: M1 vs M2")+geom_abline(slope=1,intercept = 0)

p2 <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M1_se_FH<9999 & !is.na(SA3_Level_Estimates$M1_se_FH),],aes(x=M1_se_FH,y=M2_se_FH,color=RA))+
  geom_point()+xlab("M1")+ylab("M2")+ggtitle("SE by RA: M1 vs M2")+geom_abline(slope=1,intercept = 0)

p3 <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M1_bias_FH<9999 & !is.na(SA3_Level_Estimates$M1_bias_FH),],aes(x=M1_bias_FH,y=M2_bias_FH,color=State))+
  geom_point()+xlab("M1")+ylab("M2")+ggtitle("Bias by State: M1 vs M2")+geom_abline(slope=1,intercept = 0)

p4 <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M1_se_FH<9999 & !is.na(SA3_Level_Estimates$M1_se_FH),],aes(x=M1_se_FH,y=M2_se_FH,color=State))+
  geom_point()+xlab("M1")+ylab("M2")+ggtitle("SE by State: M1 vs M2")+geom_abline(slope=1,intercept = 0)


xl <- expression(M[A1])
yl <- expression(M[A2])
title <- expression(Bias ~ by ~ RA ~ M[A1] ~ vs. ~ M[A2])

p1 <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M1_bias_FH<9999 & !is.na(SA3_Level_Estimates$M1_bias_FH),],aes(x=M1_bias_FH,y=M2_bias_FH,color=RA))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)

xl <- expression(M[A1])
yl <- expression(M[A2])
title <- expression(SE ~ by ~ RA ~ M[A1] ~ vs. ~ M[A2])

p2 <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M1_se_FH<9999 & !is.na(SA3_Level_Estimates$M1_se_FH),],aes(x=M1_se_FH,y=M2_se_FH,color=RA))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)


xl <- expression(M[A1])
yl <- expression(M[A2])
title <- expression(Bias ~ by ~ State ~ M[A1] ~ vs. ~ M[A2])

p3 <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M1_bias_FH<9999 & !is.na(SA3_Level_Estimates$M1_bias_FH),],aes(x=M1_bias_FH,y=M2_bias_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)

xl <- expression(M[A1])
yl <- expression(M[A2])
title <- expression(SE ~ by ~ State ~ M[A1] ~ vs. ~ M[A2])

p4 <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M1_se_FH<9999 & !is.na(SA3_Level_Estimates$M1_se_FH),],aes(x=M1_se_FH,y=M2_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)

library(ggpubr)

p5 <- ggarrange(p1,p2,nrow=2,ncol=1,legend = "right",common.legend = TRUE)

ggsave("Bias_SE_RA_M1_M2.pdf",p5,device = "pdf",width = 8,height = 8)

p6 <- ggarrange(p3,p4,nrow=2,ncol=1,legend = "right",common.legend = TRUE)
ggsave("Bias_SE_State_M1_M2.pdf",p6,device = "pdf",width = 8,height = 8)


xl <- expression(M[A1])
yl <- expression(M[A2])
title <- expression(Bias ~ by ~ State ~ M[A1] ~ vs. ~ M[A2])

p1 <- ggplot(SA4_Level_Estimates,aes(x=M1_bias_FH,y=M2_bias_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)

xl <- expression(M[A1])
yl <- expression(M[A2])
title <- expression(SE ~ by ~ State ~ M[A1] ~ vs. ~ M[A2])
p2 <- ggplot(SA4_Level_Estimates,aes(x=M1_se_FH,y=M2_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)

p3 <- ggarrange(p1,p2,nrow=2,ncol=1,legend = "right",common.legend = TRUE)
ggsave("SA4_Level_Bias_SE_State_M1_M2.pdf",p3,device = "pdf",width = 8,height = 8)

# Which SA4 domains have higher bias and SE

SA4_Level_Estimates$M1_bias_FH[SA4_Level_Estimates$State=="NT"]
SA4_Level_Estimates$SA4_NAME16[SA4_Level_Estimates$State=="NT" & SA4_Level_Estimates$M1_bias_FH>0.07] 
# "Northern Territory - Outback"

SA4_Level_Estimates$M1_se_FH[SA4_Level_Estimates$State=="NT"]
SA4_Level_Estimates$SA4_NAME16[SA4_Level_Estimates$State=="NT" & SA4_Level_Estimates$M1_se_FH>0.03] 
# "Northern Territory - Outback"

SA4_Level_Estimates$M1_bias_FH[SA4_Level_Estimates$State=="WA"]
SA4_Level_Estimates$SA4_NAME16[SA4_Level_Estimates$State=="WA" & SA4_Level_Estimates$M1_bias_FH< -0.2] 
# Western Australia - Outback (North)
SA4_Level_Estimates$M1_se_FH[SA4_Level_Estimates$State=="WA"]
SA4_Level_Estimates$SA4_NAME16[SA4_Level_Estimates$State=="WA" & SA4_Level_Estimates$M1_se_FH> 0.03] 
# "Western Australia - Outback (North)"

SA4_Level_Estimates$M1_se_FH[SA4_Level_Estimates$State=="SA"]
SA4_Level_Estimates$SA4_NAME16[SA4_Level_Estimates$State=="SA" & SA4_Level_Estimates$M1_se_FH> 0.02] 
# "South Australia - Outback"

SA4_Level_Estimates$M1_se_FH[SA4_Level_Estimates$State=="QLD"]
SA4_Level_Estimates$SA4_NAME16[SA4_Level_Estimates$State=="QLD" & SA4_Level_Estimates$M1_se_FH> 0.02] 
# "Queensland - Outback"

SA4_Level_Estimates$M1_se_FH[SA4_Level_Estimates$State=="TAS"]
SA4_Level_Estimates$SA4_NAME16[SA4_Level_Estimates$State=="TAS" & SA4_Level_Estimates$M1_se_FH> 0.02] 
# "Queensland - Outback"

# Model Comparison -------- Indigenous Variable
# Model-2, Model-4 and Model-6

cor.test(SA3_Level_Estimates$est.DIR,SA3_Level_Estimates$Indigenous_SA3)
cor.test(SA3_Level_Estimates$est.DIR,sqrt(SA3_Level_Estimates$Indigenous_SA3))
cor.test(SA4_Level_Estimates$est.DIR,SA4_Level_Estimates$Indigenous_SA4)
cor.test(SA4_Level_Estimates$est.DIR,sqrt(SA4_Level_Estimates$Indigenous_SA4))

cor.test(sqrt(SA3_Level_Estimates$est.DIR),SA3_Level_Estimates$Indigenous_SA3)
cor.test(sqrt(SA3_Level_Estimates$est.DIR),sqrt(SA3_Level_Estimates$Indigenous_SA3))


# Using Models with Indigenous_SA3

p1 <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$est.DIR<9999 & !is.na(SA3_Level_Estimates$est.DIR),],aes(y=est.DIR,x=sqrt(Indigenous_SA3),color=RA))+
  geom_point()+ylab("Smoking prevalence")+xlab("sqrt(Indigenous Proportion)")+ggtitle("Smoking prevalence and Indigenity: SA3") # + geom_smooth(method='lm')
p2 <- ggplot(SA4_Level_Estimates,aes(y=est.DIR,x=sqrt(Indigenous_SA4),color=State))+
  geom_point()+ylab("Smoking prevalence")+xlab("sqrt(Indigenous Proportion)")+ggtitle("Smoking prevalence and Indigenity: SA4") # + geom_smooth(method='lm')
p3 <- ggarrange(p1,p2,nrow=2,ncol=1,legend = "right")

ggsave("Prevalence_Indigenous_SA3_SA4.pdf",p3,device = "pdf",width = 8,height = 8)


xl <- expression(M[A2])
yl <- expression(M[A4])
title <- expression(Bias ~ by ~ RA ~ M[A2] ~ vs. ~ M[A4])

p1 <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M2_bias_FH<9999 & !is.na(SA3_Level_Estimates$M2_bias_FH),],aes(x=M2_bias_FH,y=M4_bias_FH,color=RA))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)

xl <- expression(M[A2])
yl <- expression(M[A4])
title <- expression(SE ~ by ~ RA ~ M[A2] ~ vs. ~ M[A4])

p2 <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M2_se_FH<9999 & !is.na(SA3_Level_Estimates$M2_se_FH),],aes(x=M2_se_FH,y=M4_se_FH,color=RA))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)

xl <- expression(M[A2])
yl <- expression(M[A4])
title <- expression(Bias ~ by ~ State ~ M[A2] ~ vs. ~ M[A4])

p3 <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M2_bias_FH<9999 & !is.na(SA3_Level_Estimates$M2_bias_FH),],aes(x=M2_bias_FH,y=M4_bias_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)

xl <- expression(M[A2])
yl <- expression(M[A4])
title <- expression(SE ~ by ~ State ~ M[A2] ~ vs. ~ M[A4])

p4 <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M2_se_FH<9999 & !is.na(SA3_Level_Estimates$M2_se_FH),],aes(x=M2_se_FH,y=M4_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)


p5 <- ggarrange(p1,p2,nrow=2,ncol=1,legend = "right",common.legend = TRUE)

ggsave("Bias_SE_RA_M2_M4.pdf",p5,device = "pdf",width = 8,height = 8)

p6 <- ggarrange(p3,p4,nrow=2,ncol=1,legend = "right",common.legend = TRUE)
ggsave("Bias_SE_State_M2_M4.pdf",p6,device = "pdf",width = 8,height = 8)

# Using Models with Indigenous_SA4


xl <- expression(M[A2])
yl <- expression(M[A6])
title <- expression(Bias ~ by ~ RA ~ M[A2] ~ vs. ~ M[A6])

p1 <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M2_bias_FH<9999 & !is.na(SA3_Level_Estimates$M2_bias_FH),],aes(x=M2_bias_FH,y=M6_bias_FH,color=RA))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)

xl <- expression(M[A2])
yl <- expression(M[A6])
title <- expression(SE ~ by ~ RA ~ M[A2] ~ vs. ~ M[A6])

p2 <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M2_se_FH<9999 & !is.na(SA3_Level_Estimates$M2_se_FH),],aes(x=M2_se_FH,y=M6_se_FH,color=RA))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)

xl <- expression(M[A2])
yl <- expression(M[A6])
title <- expression(Bias ~ by ~ State ~ M[A2] ~ vs. ~ M[A6])

p3 <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M2_bias_FH<9999 & !is.na(SA3_Level_Estimates$M2_bias_FH),],aes(x=M2_bias_FH,y=M6_bias_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)

xl <- expression(M[A2])
yl <- expression(M[A6])
title <- expression(SE ~ by ~ State ~ M[A2] ~ vs. ~ M[A6])

p4 <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M2_se_FH<9999 & !is.na(SA3_Level_Estimates$M2_se_FH),],aes(x=M2_se_FH,y=M6_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)


p5 <- ggarrange(p1,p2,nrow=2,ncol=1,legend = "right",common.legend = TRUE)

ggsave("Bias_SE_RA_M2_M6.pdf",p5,device = "pdf",width = 8,height = 8)

p6 <- ggarrange(p3,p4,nrow=2,ncol=1,legend = "right",common.legend = TRUE)
ggsave("Bias_SE_State_M2_M6.pdf",p6,device = "pdf",width = 8,height = 8)


# Model Comparison -------- Random effects 

# Model-2, Model-7, Model-8, Model-9, Model-11, and Model-10





# Look at Relative Bias ------------------------#

xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(RB ~ by ~ RA ~ M[A] ~ vs. ~ M[D])

p1 <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M2_RAB_FH,y=M7_RAB_FH,color=RA_NAME_2016))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title) + geom_abline(slope=1,intercept = 0)

xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(SE ~ by ~ RA ~ M[A] ~ vs. ~ M[D])

p2 <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M2_se_FH,y=M7_se_FH,color=RA_NAME_2016))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)

xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(Bias ~ by ~ State ~ M[A] ~ vs. ~ M[D])

p3 <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M2_bias_FH,y=M7_bias_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)

xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(SE ~ by ~ State ~ M[A] ~ vs. ~ M[D])

p4 <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M2_se_FH<9999 & !is.na(SA3_Level_Estimates$M2_se_FH),],aes(x=M2_se_FH,y=M7_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)



xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(RB ~ by ~ State ~ M[A] ~ vs. ~ M[D])

p5_a <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M2_RB_FH,y=M7_RB_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+
  geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M2_RB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M7_RB_FH[sub_dist_SA3_domains]))+
  ylim(range(SA3_Level_Estimates$M2_RB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M7_RB_FH[sub_dist_SA3_domains]))

xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(ARB ~ by ~ State ~ M[A] ~ vs. ~ M[D])

p5_b <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M2_RAB_FH,y=M7_RAB_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+
  geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M2_RAB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M7_RAB_FH[sub_dist_SA3_domains]))+
  ylim(range(SA3_Level_Estimates$M2_RAB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M7_RAB_FH[sub_dist_SA3_domains]))

xl <- expression(M[B])
yl <- expression(M[E])
title <- expression(RB ~ by ~ State ~ M[B] ~ vs. ~ M[E])

p6_a <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M8_RB_FH,y=M9_RB_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+
  geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M8_RB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M9_RB_FH[sub_dist_SA3_domains]))+
  ylim(range(SA3_Level_Estimates$M8_RB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M9_RB_FH[sub_dist_SA3_domains]))

xl <- expression(M[B])
yl <- expression(M[E])
title <- expression(ARB ~ by ~ State ~ M[B] ~ vs. ~ M[E])

p6_b <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M8_RAB_FH,y=M9_RAB_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+
  geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M8_RAB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M9_RAB_FH[sub_dist_SA3_domains]))+
  ylim(range(SA3_Level_Estimates$M8_RAB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M9_RAB_FH[sub_dist_SA3_domains]))

xl <- expression(M[C])
yl <- expression(M[F])
title <- expression(RB ~ by ~ State ~ M[C] ~ vs. ~ M[F])

p7_a <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M11_RB_FH,y=M10_RB_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+
  geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M11_RB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M10_RB_FH[sub_dist_SA3_domains]))+
  ylim(range(SA3_Level_Estimates$M11_RB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M10_RB_FH[sub_dist_SA3_domains]))

xl <- expression(M[C])
yl <- expression(M[F])
title <- expression(ARB ~ by ~ State ~ M[C] ~ vs. ~ M[F])

p7_b <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M11_RAB_FH,y=M10_RAB_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+
  geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M11_RAB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M10_RAB_FH[sub_dist_SA3_domains]))+
  ylim(range(SA3_Level_Estimates$M11_RAB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M10_RAB_FH[sub_dist_SA3_domains]))

p <- ggarrange(p5_a,p5_b,p6_a,p6_b,p7_a,p7_b,nrow = 3, ncol = 2,common.legend = TRUE)

ggsave("RB_ARB_SA3.pdf",p,device = "pdf",width = 8,height = 12)

# By remoteness
SA3_Level_Estimates$RA <- as.factor(SA3_Level_Estimates$RA_NAME_2016)
SA3_Level_Estimates$RA <- factor(SA3_Level_Estimates$RA,labels = c("Inner Regional","Major Cities","Outer Regional","Remote","Very Remote"))
SA3_Level_Estimates$RA <- factor(SA3_Level_Estimates$RA,levels=c("Major Cities","Inner Regional","Outer Regional","Remote","Very Remote"))

xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(RB ~ by ~ RA ~ M[A] ~ vs. ~ M[D])

p5_a <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M2_RB_FH,y=M7_RB_FH,color=RA))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+
  geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M2_RB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M7_RB_FH[sub_dist_SA3_domains]))+
  ylim(range(SA3_Level_Estimates$M2_RB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M7_RB_FH[sub_dist_SA3_domains]))

xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(ARB ~ by ~ RA ~ M[A] ~ vs. ~ M[D])

p5_b <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M2_RAB_FH,y=M7_RAB_FH,color=RA))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+
  geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M2_RAB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M7_RAB_FH[sub_dist_SA3_domains]))+
  ylim(range(SA3_Level_Estimates$M2_RAB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M7_RAB_FH[sub_dist_SA3_domains]))

xl <- expression(M[B])
yl <- expression(M[E])
title <- expression(RB ~ by ~ RA ~ M[B] ~ vs. ~ M[E])

p6_a <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M8_RB_FH,y=M9_RB_FH,color=RA))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+
  geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M8_RB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M9_RB_FH[sub_dist_SA3_domains]))+
  ylim(range(SA3_Level_Estimates$M8_RB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M9_RB_FH[sub_dist_SA3_domains]))

xl <- expression(M[B])
yl <- expression(M[E])
title <- expression(ARB ~ by ~ RA ~ M[B] ~ vs. ~ M[E])

p6_b <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M8_RAB_FH,y=M9_RAB_FH,color=RA))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+
  geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M8_RAB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M9_RAB_FH[sub_dist_SA3_domains]))+
  ylim(range(SA3_Level_Estimates$M8_RAB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M9_RAB_FH[sub_dist_SA3_domains]))

xl <- expression(M[C])
yl <- expression(M[F])
title <- expression(RB ~ by ~ RA ~ M[C] ~ vs. ~ M[F])

p7_a <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M11_RB_FH,y=M10_RB_FH,color=RA))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+
  geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M11_RB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M10_RB_FH[sub_dist_SA3_domains]))+
  ylim(range(SA3_Level_Estimates$M11_RB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M10_RB_FH[sub_dist_SA3_domains]))

xl <- expression(M[C])
yl <- expression(M[F])
title <- expression(ARB ~ by ~ RA ~ M[C] ~ vs. ~ M[F])

p7_b <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M11_RAB_FH,y=M10_RAB_FH,color=RA))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+
  geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M11_RAB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M10_RAB_FH[sub_dist_SA3_domains]))+
  ylim(range(SA3_Level_Estimates$M11_RAB_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M10_RAB_FH[sub_dist_SA3_domains]))

p <- ggarrange(p5_a,p5_b,p6_a,p6_b,p7_a,p7_b,nrow = 3, ncol = 2,common.legend = TRUE)

ggsave("RB_ARB_SA3_RA.pdf",p,device = "pdf",width = 8,height = 12)


# SA4 level plots 

xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(RB ~ by ~ State ~ M[A] ~ vs. ~ M[D])

p5_a <- ggplot(SA4_Level_Estimates,aes(x=M2_RB_FH,y=M7_RB_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+
  geom_abline(slope=1,intercept = 0)+
  xlim(range(SA4_Level_Estimates$M2_RB_FH,SA4_Level_Estimates$M7_RB_FH))+
  ylim(range(SA4_Level_Estimates$M2_RB_FH,SA4_Level_Estimates$M7_RB_FH))

xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(ARB ~ by ~ State ~ M[A] ~ vs. ~ M[D])

p5_b <- ggplot(SA4_Level_Estimates,aes(x=M2_RAB_FH,y=M7_RAB_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+
  geom_abline(slope=1,intercept = 0)+
  xlim(range(SA4_Level_Estimates$M2_RAB_FH,SA4_Level_Estimates$M7_RAB_FH))+
  ylim(range(SA4_Level_Estimates$M2_RAB_FH,SA4_Level_Estimates$M7_RAB_FH))

xl <- expression(M[B])
yl <- expression(M[E])
title <- expression(RB ~ by ~ State ~ M[B] ~ vs. ~ M[E])

p6_a <- ggplot(SA4_Level_Estimates,aes(x=M8_RB_FH,y=M9_RB_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+
  geom_abline(slope=1,intercept = 0)+
  xlim(range(SA4_Level_Estimates$M8_RB_FH,SA4_Level_Estimates$M9_RB_FH))+
  ylim(range(SA4_Level_Estimates$M8_RB_FH,SA4_Level_Estimates$M9_RB_FH))

xl <- expression(M[B])
yl <- expression(M[E])
title <- expression(ARB ~ by ~ State ~ M[B] ~ vs. ~ M[E])

p6_b <- ggplot(SA4_Level_Estimates,aes(x=M8_RAB_FH,y=M9_RAB_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+
  geom_abline(slope=1,intercept = 0)+
  xlim(range(SA4_Level_Estimates$M8_RAB_FH,SA4_Level_Estimates$M9_RAB_FH))+
  ylim(range(SA4_Level_Estimates$M8_RAB_FH,SA4_Level_Estimates$M9_RAB_FH))


xl <- expression(M[C])
yl <- expression(M[F])
title <- expression(RB ~ by ~ State ~ M[C] ~ vs. ~ M[F])

p7_a <- ggplot(SA4_Level_Estimates,aes(x=M11_RB_FH,y=M10_RB_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+
  geom_abline(slope=1,intercept = 0)+
  xlim(range(SA4_Level_Estimates$M11_RB_FH,SA4_Level_Estimates$M10_RB_FH))+
  ylim(range(SA4_Level_Estimates$M11_RB_FH,SA4_Level_Estimates$M10_RB_FH))

xl <- expression(M[C])
yl <- expression(M[F])
title <- expression(ARB ~ by ~ State ~ M[C] ~ vs. ~ M[F])

p7_b <- ggplot(SA4_Level_Estimates,aes(x=M11_RAB_FH,y=M10_RAB_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+
  geom_abline(slope=1,intercept = 0)+
  xlim(range(SA4_Level_Estimates$M11_RAB_FH,SA4_Level_Estimates$M10_RAB_FH))+
  ylim(range(SA4_Level_Estimates$M11_RAB_FH,SA4_Level_Estimates$M10_RAB_FH))

p <- ggarrange(p5_a,p5_b,p6_a,p6_b,p7_a,p7_b,nrow = 3, ncol = 2,common.legend = TRUE)

ggsave("RB_ARB_SA4.pdf",p,device = "pdf",width = 8,height = 12)


# RB for M10 against M7 and M9 

p5_a <- ggplot(SA4_Level_Estimates,aes(x=M10_RB_FH,y=M7_RB_FH,color=State))+
  geom_point()+xlab("M10")+ylab("M7")+ggtitle("RB by State: M10 vs M7")+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA4_Level_Estimates$M10_RB_FH,SA4_Level_Estimates$M7_RB_FH))+
  ylim(range(SA4_Level_Estimates$M10_RB_FH,SA4_Level_Estimates$M7_RB_FH))

p5_b <- ggplot(SA4_Level_Estimates,aes(x=M10_RAB_FH,y=M7_RAB_FH,color=State))+
  geom_point()+xlab("M10")+ylab("M7")+ggtitle("ARB by State: M10 vs M7")+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA4_Level_Estimates$M10_RAB_FH,SA4_Level_Estimates$M7_RAB_FH))+
  ylim(range(SA4_Level_Estimates$M10_RAB_FH,SA4_Level_Estimates$M7_RAB_FH))


p6_a <- ggplot(SA4_Level_Estimates,aes(x=M10_RB_FH,y=M9_RB_FH,color=State))+
  geom_point()+xlab("M10")+ylab("M9")+ggtitle("RB by State: M10 vs M9")+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA4_Level_Estimates$M10_RB_FH,SA4_Level_Estimates$M9_RB_FH))+
  ylim(range(SA4_Level_Estimates$M10_RB_FH,SA4_Level_Estimates$M9_RB_FH))

p6_b <- ggplot(SA4_Level_Estimates,aes(x=M10_RAB_FH,y=M9_RAB_FH,color=State))+
  geom_point()+xlab("M10")+ylab("M9")+ggtitle("ARB by State: M10 vs M9")+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA4_Level_Estimates$M10_RAB_FH,SA4_Level_Estimates$M9_RAB_FH))+
  ylim(range(SA4_Level_Estimates$M10_RAB_FH,SA4_Level_Estimates$M9_RAB_FH))

p <- ggarrange(p5_a,p5_b,p6_a,p6_b,nrow = 2, ncol = 2,common.legend = TRUE)

ggsave("RB_ARB_SA4_M10.pdf",p,device = "pdf",width = 8,height = 8)


p5_a <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M10_bias_FH<9999 & !is.na(SA3_Level_Estimates$M10_bias_FH),],aes(x=M10_RB_FH,y=M7_RB_FH,color=State))+
  geom_point()+xlab("M10")+ylab("M7")+ggtitle("RB by State: M10 vs M7")+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M10_RB_FH,SA3_Level_Estimates$M7_RB_FH))+
  ylim(range(SA3_Level_Estimates$M10_RB_FH,SA3_Level_Estimates$M7_RB_FH))

p5_b <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M10_bias_FH<9999 & !is.na(SA3_Level_Estimates$M10_bias_FH),],aes(x=M10_RAB_FH,y=M7_RAB_FH,color=State))+
  geom_point()+xlab("M10")+ylab("M7")+ggtitle("ARB by State: M10 vs M7")+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M10_RAB_FH,SA3_Level_Estimates$M7_RAB_FH))+
  ylim(range(SA3_Level_Estimates$M10_RAB_FH,SA3_Level_Estimates$M7_RAB_FH))


p6_a <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M10_bias_FH<9999 & !is.na(SA3_Level_Estimates$M10_bias_FH),],aes(x=M10_RB_FH,y=M9_RB_FH,color=State))+
  geom_point()+xlab("M10")+ylab("M9")+ggtitle("RB by State: M10 vs M9")+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M10_RB_FH,SA3_Level_Estimates$M9_RB_FH))+
  ylim(range(SA3_Level_Estimates$M10_RB_FH,SA3_Level_Estimates$M9_RB_FH))

p6_b <- ggplot(SA3_Level_Estimates[SA3_Level_Estimates$M10_bias_FH<9999 & !is.na(SA3_Level_Estimates$M10_bias_FH),],aes(x=M10_RAB_FH,y=M9_RAB_FH,color=State))+
  geom_point()+xlab("M10")+ylab("M9")+ggtitle("ARB by State: M10 vs M9")+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M10_RAB_FH,SA3_Level_Estimates$M9_RAB_FH))+
  ylim(range(SA3_Level_Estimates$M10_RAB_FH,SA3_Level_Estimates$M9_RAB_FH))

p <- ggarrange(p5_a,p5_b,p6_a,p6_b,nrow = 2, ncol = 2,common.legend = TRUE)

ggsave("RB_ARB_SA3_M10.pdf",p,device = "pdf",width = 8,height = 8)



# State level Performance 


p5_a <- ggplot(State_Level_Estimates,aes(x=M10_RB_FH,y=M7_RB_FH,color=State))+
  geom_point()+xlab("M10")+ylab("M7")+ggtitle("RB by State: M10 vs M7")+geom_abline(slope=1,intercept = 0)+
  xlim(range(State_Level_Estimates$M10_RB_FH,State_Level_Estimates$M7_RB_FH))+
  ylim(range(State_Level_Estimates$M10_RB_FH,State_Level_Estimates$M7_RB_FH))

p5_b <- ggplot(State_Level_Estimates,aes(x=M10_RAB_FH,y=M7_RAB_FH,color=State))+
  geom_point()+xlab("M10")+ylab("M7")+ggtitle("ARB by State: M10 vs M7")+geom_abline(slope=1,intercept = 0)+
  xlim(range(State_Level_Estimates$M10_RAB_FH,State_Level_Estimates$M7_RAB_FH))+
  ylim(range(State_Level_Estimates$M10_RAB_FH,State_Level_Estimates$M7_RAB_FH))

p6_a <- ggplot(State_Level_Estimates,aes(x=M10_RB_FH,y=M9_RB_FH,color=State))+
  geom_point()+xlab("M10")+ylab("M9")+ggtitle("RB by State: M10 vs M9")+geom_abline(slope=1,intercept = 0)+
  xlim(range(State_Level_Estimates$M10_RB_FH,State_Level_Estimates$M9_RB_FH))+
  ylim(range(State_Level_Estimates$M10_RB_FH,State_Level_Estimates$M9_RB_FH))

p6_b <- ggplot(State_Level_Estimates,aes(x=M10_RAB_FH,y=M9_RAB_FH,color=State))+
  geom_point()+xlab("M10")+ylab("M9")+ggtitle("ARB by State: M10 vs M9")+geom_abline(slope=1,intercept = 0)+
  xlim(range(State_Level_Estimates$M10_RAB_FH,State_Level_Estimates$M9_RAB_FH))+
  ylim(range(State_Level_Estimates$M10_RAB_FH,State_Level_Estimates$M9_RAB_FH))

# Calculate Mean RB, and Mean RAB 

RB_RAB_State_SA4_SA3 <- rbind(
  c(
  mean(State_Level_Estimates$M2_RB_FH),
  mean(State_Level_Estimates$M8_RB_FH),
  mean(State_Level_Estimates$M11_RB_FH),
  mean(State_Level_Estimates$M7_RB_FH),
  mean(State_Level_Estimates$M9_RB_FH),
  mean(State_Level_Estimates$M10_RB_FH)
  )*100,

c(
  mean(SA4_Level_Estimates$M2_RB_FH),
  mean(SA4_Level_Estimates$M8_RB_FH),
  mean(SA4_Level_Estimates$M11_RB_FH),
  mean(SA4_Level_Estimates$M7_RB_FH),
  mean(SA4_Level_Estimates$M9_RB_FH),
  mean(SA4_Level_Estimates$M10_RB_FH)
)*100,

c(
  mean(SA3_Level_Estimates$M2_RB_FH[sub_dist_SA3_domains][is.finite(SA3_Level_Estimates$M2_RB_FH[sub_dist_SA3_domains])]),
  mean(SA3_Level_Estimates$M8_RB_FH[sub_dist_SA3_domains][is.finite(SA3_Level_Estimates$M8_RB_FH[sub_dist_SA3_domains])]),
  mean(SA3_Level_Estimates$M11_RB_FH[sub_dist_SA3_domains][is.finite(SA3_Level_Estimates$M11_RB_FH[sub_dist_SA3_domains])]),
  mean(SA3_Level_Estimates$M7_RB_FH[sub_dist_SA3_domains][is.finite(SA3_Level_Estimates$M7_RB_FH[sub_dist_SA3_domains])]),
  mean(SA3_Level_Estimates$M9_RB_FH[sub_dist_SA3_domains][is.finite(SA3_Level_Estimates$M9_RB_FH[sub_dist_SA3_domains])]),
  mean(SA3_Level_Estimates$M10_RB_FH[sub_dist_SA3_domains][is.finite(SA3_Level_Estimates$M10_RB_FH[sub_dist_SA3_domains])])
)*100,



c(
  mean(State_Level_Estimates$M2_RAB_FH),
  mean(State_Level_Estimates$M8_RAB_FH),
  mean(State_Level_Estimates$M11_RAB_FH),
  mean(State_Level_Estimates$M7_RAB_FH),
  mean(State_Level_Estimates$M9_RAB_FH),
  mean(State_Level_Estimates$M10_RAB_FH)
)*100,

c(
  mean(SA4_Level_Estimates$M2_RAB_FH),
  mean(SA4_Level_Estimates$M8_RAB_FH),
  mean(SA4_Level_Estimates$M11_RAB_FH),
  mean(SA4_Level_Estimates$M7_RAB_FH),
  mean(SA4_Level_Estimates$M9_RAB_FH),
  mean(SA4_Level_Estimates$M10_RAB_FH)
)*100,

c(
  mean(SA3_Level_Estimates$M2_RAB_FH[sub_dist_SA3_domains][is.finite(SA3_Level_Estimates$M2_RAB_FH[sub_dist_SA3_domains])]),
  mean(SA3_Level_Estimates$M8_RAB_FH[sub_dist_SA3_domains][is.finite(SA3_Level_Estimates$M8_RAB_FH[sub_dist_SA3_domains])]),
  mean(SA3_Level_Estimates$M11_RAB_FH[sub_dist_SA3_domains][is.finite(SA3_Level_Estimates$M11_RAB_FH[sub_dist_SA3_domains])]),
  mean(SA3_Level_Estimates$M7_RAB_FH[sub_dist_SA3_domains][is.finite(SA3_Level_Estimates$M7_RAB_FH[sub_dist_SA3_domains])]),
  mean(SA3_Level_Estimates$M9_RAB_FH[sub_dist_SA3_domains][is.finite(SA3_Level_Estimates$M9_RAB_FH[sub_dist_SA3_domains])]),
  mean(SA3_Level_Estimates$M10_RAB_FH[sub_dist_SA3_domains][is.finite(SA3_Level_Estimates$M10_RAB_FH[sub_dist_SA3_domains])])
)*100
)

rownames(RB_RAB_State_SA4_SA3) <- rep(c("State","SA4","SA3"),2)
colnames(RB_RAB_State_SA4_SA3) <- c("M2","M8","M11","M7","M9","M10")

xtable(RB_RAB_State_SA4_SA3,digits = 2)


# Standard error and RRSE and CV Ratio -----------------------#

# SA3 level

myTheme <- theme(legend.text = element_text(size = 8), 
                 legend.title = element_text(size = 8), 
                 legend.key.size = unit(1, 'cm'))


xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(SE ~ by ~ RA ~ M[A] ~ vs. ~ M[D])

p1_a <- ggplot(SA3_Level_Estimates,aes(x=M2_se_FH,y=M7_se_FH,color=RA))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M7_se_FH,SA3_Level_Estimates$M2_se_FH))+
  ylim(range(SA3_Level_Estimates$M7_se_FH,SA3_Level_Estimates$M2_se_FH))+myTheme


xl <- expression(M[B])
yl <- expression(M[D])
title <- expression(SE ~ by ~ RA ~ M[B] ~ vs. ~ M[D])

p1_b <- ggplot(SA3_Level_Estimates,aes(x=M8_se_FH,y=M9_se_FH,color=RA))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M8_se_FH,SA3_Level_Estimates$M9_se_FH))+
  ylim(range(SA3_Level_Estimates$M8_se_FH,SA3_Level_Estimates$M9_se_FH))+myTheme


xl <- expression(M[C])
yl <- expression(M[F])
title <- expression(SE ~ by ~ RA ~ M[C] ~ vs. ~ M[F])

p1_c <- ggplot(SA3_Level_Estimates,aes(x=M11_se_FH,y=M10_se_FH,color=RA))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M11_se_FH,SA3_Level_Estimates$M10_se_FH))+
  ylim(range(SA3_Level_Estimates$M11_se_FH,SA3_Level_Estimates$M10_se_FH))+myTheme


xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(RRSE ~ by ~ RA ~ M[A] ~ vs. ~ M[D])

p2_a <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M2_RRSE_FH,y=M7_RRSE_FH,color=RA))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M2_RRSE_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M7_RRSE_FH[sub_dist_SA3_domains]))+
  ylim(range(SA3_Level_Estimates$M2_RRSE_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M7_RRSE_FH[sub_dist_SA3_domains]))+myTheme

xl <- expression(M[B])
yl <- expression(M[E])
title <- expression(RRSE ~ by ~ RA ~ M[B] ~ vs. ~ M[E])

p2_b <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M8_RRSE_FH,y=M9_RRSE_FH,color=RA))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M8_RRSE_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M9_RRSE_FH[sub_dist_SA3_domains]))+
  ylim(range(SA3_Level_Estimates$M8_RRSE_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M9_RRSE_FH[sub_dist_SA3_domains]))+myTheme

xl <- expression(M[C])
yl <- expression(M[F])
title <- expression(RRSE ~ by ~ RA ~ M[C] ~ vs. ~ M[F])

p2_c <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M11_RRSE_FH,y=M10_RRSE_FH,color=RA))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M10_RRSE_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M11_RRSE_FH[sub_dist_SA3_domains]))+
  ylim(range(SA3_Level_Estimates$M10_RRSE_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M11_RRSE_FH[sub_dist_SA3_domains]))+myTheme


p <- ggarrange(p1_a,p2_a,p1_b,p2_b,p1_c,p2_c,nrow = 3, ncol = 2,common.legend = TRUE)

ggsave("SE_RRSE_SA3_RA.pdf",p,device = "pdf",width = 8,height = 12)


xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(SE ~ by ~ State ~ M[A] ~ vs. ~ M[D])

p1_a <- ggplot(SA3_Level_Estimates,aes(x=M2_se_FH,y=M7_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M7_se_FH,SA3_Level_Estimates$M2_se_FH))+
  ylim(range(SA3_Level_Estimates$M7_se_FH,SA3_Level_Estimates$M2_se_FH))+myTheme


xl <- expression(M[B])
yl <- expression(M[D])
title <- expression(SE ~ by ~ State ~ M[B] ~ vs. ~ M[D])

p1_b <- ggplot(SA3_Level_Estimates,aes(x=M8_se_FH,y=M9_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M8_se_FH,SA3_Level_Estimates$M9_se_FH))+
  ylim(range(SA3_Level_Estimates$M8_se_FH,SA3_Level_Estimates$M9_se_FH))+myTheme


xl <- expression(M[C])
yl <- expression(M[F])
title <- expression(SE ~ by ~ State ~ M[C] ~ vs. ~ M[F])

p1_c <- ggplot(SA3_Level_Estimates,aes(x=M11_se_FH,y=M10_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M11_se_FH,SA3_Level_Estimates$M10_se_FH))+
  ylim(range(SA3_Level_Estimates$M11_se_FH,SA3_Level_Estimates$M10_se_FH))+myTheme


xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(RRSE ~ by ~ State ~ M[A] ~ vs. ~ M[D])

p2_a <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M2_RRSE_FH,y=M7_RRSE_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M2_RRSE_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M7_RRSE_FH[sub_dist_SA3_domains]))+
  ylim(range(SA3_Level_Estimates$M2_RRSE_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M7_RRSE_FH[sub_dist_SA3_domains]))+myTheme

xl <- expression(M[B])
yl <- expression(M[E])
title <- expression(RRSE ~ by ~ State ~ M[B] ~ vs. ~ M[E])

p2_b <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M8_RRSE_FH,y=M9_RRSE_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M8_RRSE_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M9_RRSE_FH[sub_dist_SA3_domains]))+
  ylim(range(SA3_Level_Estimates$M8_RRSE_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M9_RRSE_FH[sub_dist_SA3_domains]))+myTheme

xl <- expression(M[C])
yl <- expression(M[F])
title <- expression(RRSE ~ by ~ State ~ M[C] ~ vs. ~ M[F])

p2_c <- ggplot(SA3_Level_Estimates[sub_dist_SA3_domains,],aes(x=M11_RRSE_FH,y=M10_RRSE_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M10_RRSE_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M11_RRSE_FH[sub_dist_SA3_domains]))+
  ylim(range(SA3_Level_Estimates$M10_RRSE_FH[sub_dist_SA3_domains],SA3_Level_Estimates$M11_RRSE_FH[sub_dist_SA3_domains]))+myTheme


p <- ggarrange(p1_a,p2_a,p1_b,p2_b,p1_c,p2_c,nrow = 3, ncol = 2,common.legend = TRUE)

ggsave("SE_RRSE_SA3_State.pdf",p,device = "pdf",width = 8,height = 12)



# SA4 level

xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(SE ~ by ~ State ~ M[A] ~ vs. ~ M[D])

x_range <- range(SA4_Level_Estimates$M2_se_FH,SA4_Level_Estimates$M7_se_FH,
            SA4_Level_Estimates$M8_se_FH,SA4_Level_Estimates$M9_se_FH,
            SA4_Level_Estimates$M11_se_FH,SA4_Level_Estimates$M10_se_FH)

p1_a <- ggplot(SA4_Level_Estimates,aes(x=M2_se_FH,y=M7_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(x_range)+
  ylim(x_range)+myTheme


xl <- expression(M[B])
yl <- expression(M[D])
title <- expression(SE ~ by ~ State ~ M[B] ~ vs. ~ M[D])

p1_b <- ggplot(SA4_Level_Estimates,aes(x=M8_se_FH,y=M9_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(x_range)+
  ylim(x_range)+myTheme


xl <- expression(M[C])
yl <- expression(M[F])
title <- expression(SE ~ by ~ State ~ M[C] ~ vs. ~ M[F])

p1_c <- ggplot(SA4_Level_Estimates,aes(x=M11_se_FH,y=M10_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(x_range)+
  ylim(x_range)+myTheme


xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(RRSE ~ by ~ State ~ M[A] ~ vs. ~ M[D])

x_range <- range(SA4_Level_Estimates$M2_RRSE_FH,SA4_Level_Estimates$M7_RRSE_FH,
                 SA4_Level_Estimates$M8_RRSE_FH,SA4_Level_Estimates$M9_RRSE_FH,
                 SA4_Level_Estimates$M11_RRSE_FH,SA4_Level_Estimates$M10_RRSE_FH)

p2_a <- ggplot(SA4_Level_Estimates,aes(x=M2_RRSE_FH,y=M7_RRSE_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(x_range)+
  ylim(x_range)+myTheme

xl <- expression(M[B])
yl <- expression(M[E])
title <- expression(RRSE ~ by ~ State ~ M[B] ~ vs. ~ M[E])

p2_b <- ggplot(SA4_Level_Estimates,aes(x=M8_RRSE_FH,y=M9_RRSE_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(x_range)+
  ylim(x_range)+myTheme

xl <- expression(M[C])
yl <- expression(M[F])
title <- expression(RRSE ~ by ~ State ~ M[C] ~ vs. ~ M[F])

p2_c <- ggplot(SA4_Level_Estimates,aes(x=M11_RRSE_FH,y=M10_RRSE_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(x_range)+
  ylim(x_range)+myTheme


p <- ggarrange(p1_a,p2_a,p1_b,p2_b,p1_c,p2_c,nrow = 3, ncol = 2,common.legend = TRUE)

ggsave("SE_RRSE_SA4_State.pdf",p,device = "pdf",width = 8,height = 12)


# Combine SA3 and SA4 level SE ---------------#

xl <- expression(M[A])
yl <- expression(M[B])
title <- expression(SE ~ by ~ State ~ M[A] ~ vs. ~ M[B] ~ at ~ SA3 ~Level)

p1_a <- ggplot(SA3_Level_Estimates,aes(x=M2_se_FH,y=M8_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M8_se_FH,SA3_Level_Estimates$M2_se_FH))+
  ylim(range(SA3_Level_Estimates$M8_se_FH,SA3_Level_Estimates$M2_se_FH))+myTheme

xl <- expression(M[A])
yl <- expression(M[B])
title <- expression(SE ~ by ~ State ~ M[A] ~ vs. ~ M[B] ~ at ~ SA4 ~Level)

p1_b <- ggplot(SA4_Level_Estimates,aes(x=M2_se_FH,y=M8_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA4_Level_Estimates$M8_se_FH,SA4_Level_Estimates$M2_se_FH))+
  ylim(range(SA4_Level_Estimates$M8_se_FH,SA4_Level_Estimates$M2_se_FH))+myTheme

xl <- expression(M[D])
yl <- expression(M[E])
title <- expression(SE ~ by ~ State ~ M[D] ~ vs. ~ M[E] ~ at ~ SA3 ~Level)

p2_a <- ggplot(SA3_Level_Estimates,aes(x=M7_se_FH,y=M9_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M7_se_FH,SA3_Level_Estimates$M9_se_FH))+
  ylim(range(SA3_Level_Estimates$M7_se_FH,SA3_Level_Estimates$M9_se_FH))+myTheme

xl <- expression(M[A])
yl <- expression(M[B])
title <- expression(SE ~ by ~ State ~ M[D] ~ vs. ~ M[E] ~ at ~ SA4 ~Level)

p2_b <- ggplot(SA4_Level_Estimates,aes(x=M7_se_FH,y=M9_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA4_Level_Estimates$M7_se_FH,SA4_Level_Estimates$M9_se_FH))+
  ylim(range(SA4_Level_Estimates$M7_se_FH,SA4_Level_Estimates$M9_se_FH))+myTheme

xl <- expression(M[C])
yl <- expression(M[F])
title <- expression(SE ~ by ~ State ~ M[C] ~ vs. ~ M[F] ~ at ~ SA3 ~Level)

p3_a <- ggplot(SA3_Level_Estimates,aes(x=M11_se_FH,y=M10_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA3_Level_Estimates$M10_se_FH,SA3_Level_Estimates$M11_se_FH))+
  ylim(range(SA3_Level_Estimates$M10_se_FH,SA3_Level_Estimates$M11_se_FH))+myTheme

xl <- expression(M[C])
yl <- expression(M[F])
title <- expression(SE ~ by ~ State ~ M[C] ~ vs. ~ M[F] ~ at ~ SA4 ~Level)

p3_b <- ggplot(SA4_Level_Estimates,aes(x=M11_se_FH,y=M10_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(SA4_Level_Estimates$M10_se_FH,SA4_Level_Estimates$M11_se_FH))+
  ylim(range(SA4_Level_Estimates$M10_se_FH,SA4_Level_Estimates$M11_se_FH))+myTheme


p <- ggarrange(p1_a,p1_b,p2_a,p2_b,p3_a,p3_b,nrow = 3, ncol = 2,common.legend = TRUE)

ggsave("SE_RRSE_SA4_State_alt.pdf",p,device = "pdf",width = 8,height = 12)

# State level


xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(SE ~ by ~ State ~ M[A] ~ vs. ~ M[D])

p1_a <- ggplot(State_Level_Estimates,aes(x=M2_se_FH,y=M7_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(State_Level_Estimates$M7_se_FH,State_Level_Estimates$M2_se_FH))+
  ylim(range(State_Level_Estimates$M7_se_FH,State_Level_Estimates$M2_se_FH))+myTheme


xl <- expression(M[B])
yl <- expression(M[D])
title <- expression(SE ~ by ~ State ~ M[B] ~ vs. ~ M[E])

p1_b <- ggplot(State_Level_Estimates,aes(x=M8_se_FH,y=M9_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(State_Level_Estimates$M8_se_FH,State_Level_Estimates$M9_se_FH))+
  ylim(range(State_Level_Estimates$M8_se_FH,State_Level_Estimates$M9_se_FH))+myTheme


xl <- expression(M[C])
yl <- expression(M[F])
title <- expression(SE ~ by ~ State ~ M[C] ~ vs. ~ M[F])

p1_c <- ggplot(State_Level_Estimates,aes(x=M11_se_FH,y=M10_se_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(State_Level_Estimates$M11_se_FH,State_Level_Estimates$M10_se_FH))+
  ylim(range(State_Level_Estimates$M11_se_FH,State_Level_Estimates$M10_se_FH))+myTheme


xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(RRSE ~ by ~ State ~ M[A] ~ vs. ~ M[D])

p2_a <- ggplot(State_Level_Estimates,aes(x=M2_RRSE_FH,y=M7_RRSE_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(State_Level_Estimates$M2_RRSE_FH,State_Level_Estimates$M7_RRSE_FH))+
  ylim(range(State_Level_Estimates$M2_RRSE_FH,State_Level_Estimates$M7_RRSE_FH))+myTheme

xl <- expression(M[B])
yl <- expression(M[E])
title <- expression(RRSE ~ by ~ State ~ M[B] ~ vs. ~ M[E])

p2_b <- ggplot(State_Level_Estimates,aes(x=M8_RRSE_FH,y=M9_RRSE_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(State_Level_Estimates$M8_RRSE_FH,State_Level_Estimates$M9_RRSE_FH))+
  ylim(range(State_Level_Estimates$M8_RRSE_FH,State_Level_Estimates$M9_RRSE_FH))+myTheme

xl <- expression(M[C])
yl <- expression(M[F])
title <- expression(RRSE ~ by ~ State ~ M[C] ~ vs. ~ M[F])

p2_c <- ggplot(State_Level_Estimates,aes(x=M11_RRSE_FH,y=M10_RRSE_FH,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(State_Level_Estimates$M10_RRSE_FH,State_Level_Estimates$M11_RRSE_FH))+
  ylim(range(State_Level_Estimates$M10_RRSE_FH,State_Level_Estimates$M11_RRSE_FH))+myTheme


xl <- expression(M[A])
yl <- expression(M[D])
title <- expression(CV ~ Ratio ~ by ~ State ~ M[A] ~ vs. ~ M[D])

p3_a <- ggplot(State_Level_Estimates,aes(x=M2_CV_Ratio,y=M7_CV_Ratio,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(State_Level_Estimates$M2_CV_Ratio,State_Level_Estimates$M7_CV_Ratio))+
  ylim(range(State_Level_Estimates$M2_CV_Ratio,State_Level_Estimates$M7_CV_Ratio))+myTheme

xl <- expression(M[B])
yl <- expression(M[E])
title <- expression(CV ~ Ratio ~ State ~ M[B] ~ vs. ~ M[E])

p3_b <- ggplot(State_Level_Estimates,aes(x=M8_CV_Ratio,y=M9_CV_Ratio,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(State_Level_Estimates$M8_CV_Ratio,State_Level_Estimates$M9_CV_Ratio))+
  ylim(range(State_Level_Estimates$M8_CV_Ratio,State_Level_Estimates$M9_CV_Ratio))+myTheme

xl <- expression(M[C])
yl <- expression(M[F])
title <- expression(CV ~ Ratio ~ State ~ M[C] ~ vs. ~ M[F])

p3_c <- ggplot(State_Level_Estimates,aes(x=M11_CV_Ratio,y=M10_CV_Ratio,color=State))+
  geom_point()+xlab(xl)+ylab(yl)+ggtitle(title)+geom_abline(slope=1,intercept = 0)+
  xlim(range(State_Level_Estimates$M10_CV_Ratio,State_Level_Estimates$M11_CV_Ratio))+
  ylim(range(State_Level_Estimates$M10_CV_Ratio,State_Level_Estimates$M11_CV_Ratio))+myTheme


p <- ggarrange(p1_a,p2_a,p3_a,p1_b,p2_b,p3_b,p1_c,p2_c,p3_c,nrow = 3, ncol = 3,common.legend = TRUE)

ggsave("SE_RRSE_CV_State.pdf",p,device = "pdf",width = 10,height = 12)


# Generate error-bar plot Figures by State, SA4 and SA3 ----------------------------
library(reshape2)


load("State_Level_Estimates.Rdata")
load("SA4_Level_Estimates.Rdata")
load("SA3_Level_Estimates.Rdata")


# National Level -----------#

df_National_est <- melt(National_Level_Estimates,id.vars = c("yr"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_National_est$Model <- factor(df_National_est$Model,labels=c("M10","M11","M2","DIR"))

df_National_se <- melt(National_Level_Estimates,id.vars = c("yr"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_National_se$Model <- factor(df_National_se$Model,labels=c("M10","M11","M2","DIR"))

df_National_ll <- melt(National_Level_Estimates,id.vars = c("yr"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_National_ll$Model <- factor(df_National_ll$Model,labels=c("M10","M11","M2","DIR"))

df_National_ul <- melt(National_Level_Estimates,id.vars = c("yr"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_National_ul$Model <- factor(df_National_ul$Model,labels=c("M10","M11","M2","DIR"))


df_National <- merge(df_National_est,df_National_se,by=c("yr","Model"))
df_National <- merge(df_National,df_National_ll,by=c("yr","Model"))
df_National <- merge(df_National,df_National_ul,by=c("yr","Model"))


pd <- position_dodge(0.5) # move them .05 to the left and right

df_National$National <- "National"

df_National$National <- factor(df_National$National, levels = df_National$National[df_National$Model=="DIR"][order(df_National$Estimate[df_National$Model=="DIR"])])


# State level

df_state_est <- melt(State_Level_Estimates,id.vars = c("State"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_state_est$Model <- factor(df_state_est$Model,labels=c("M10","M11","M2","DIR"))

df_state_se <- melt(State_Level_Estimates,id.vars = c("State"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_state_se$Model <- factor(df_state_se$Model,labels=c("M10","M11","M2","DIR"))

df_state_ll <- melt(State_Level_Estimates,id.vars = c("State"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_state_ll$Model <- factor(df_state_ll$Model,labels=c("M10","M11","M2","DIR"))

df_state_ul <- melt(State_Level_Estimates,id.vars = c("State"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_state_ul$Model <- factor(df_state_ul$Model,labels=c("M10","M11","M2","DIR"))


df_state <- merge(df_state_est,df_state_se,by=c("State","Model"))
df_state <- merge(df_state,df_state_ll,by=c("State","Model"))
df_state <- merge(df_state,df_state_ul,by=c("State","Model"))

pd <- position_dodge(0.5) # move them .05 to the left and right

df_state$State <- factor(df_state$State, levels = df_state$State[df_state$Model=="DIR"][order(df_state$Estimate[df_state$Model=="DIR"])])

p <- ggplot(data=df_state, mapping=aes(x=State, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("State") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
#p <- p + geom_errorbar(aes(ymin=Estimate-1.96*se,ymax=Estimate+1.96*se),position=pd, width=.2, size=1)
# p <- p + scale_color_manual(values=c("black","red", "blue", "green"))
#p <- p + scale_color_brewer()
# p <- p + scale_color_manual(labels = c(expression(M_A),expression(M_C),expression(M_F),"DIR"))
p <- p + scale_y_continuous(breaks=c(0.10, 0.15,0.20, 0.25),limits =c(0.08,0.27))
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                    labels = expression(M[F], M[C],  M[A], DIR)) 

p <- p + coord_flip()

p2 <- p

name <- paste0("State_Estimate")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 6)

min(df_state$ll) # 0.08061428
max(df_state$ul) # 0.2641699


p <- ggplot(data=df_National, mapping=aes(x=National, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("National") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)+scale_x_discrete(labels = NULL, breaks = NULL) 
#p <- p + geom_errorbar(aes(ymin=Estimate-1.96*se,ymax=Estimate+1.96*se),position=pd, width=.2, size=1)
# p <- p + scale_color_manual(values=c("black","red", "blue", "green"))
#p <- p + scale_color_brewer()
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + scale_y_continuous(breaks=c(0.10, 0.15,0.20, 0.25),limits =c(0.08,0.27))
p <- p + coord_flip()
p1<-p
name <- paste0("National_Estimate")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 6, height = 4)



ggarrange(p1,p2,nrow=2,ncol=1,heights = c(1,3),legend = "right",common.legend = TRUE)
name <- paste0("Nation_State_Estimate")  
fname <- paste0(name, ".pdf")
ggsave(fname, units = "in",width = 8, height = 8)


# Try to combine with the State level

names(df_National)
names(df_state)

df_National$State <- as.character(df_National$National)
df_state$State <- as.character(df_state$State)

df_National_State <- rbind(df_National[,c("Model",    "Estimate", "se",       "ll",       "ul","State")],df_state[,c("Model",    "Estimate", "se",       "ll",       "ul","State")])


df_National_State$State <- factor(df_National_State$State, levels = df_National_State$State[df_National_State$Model=="DIR"][order(df_National_State$Estimate[df_National_State$Model=="DIR"])])

p <- ggplot(data=df_National_State, mapping=aes(x=State, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("State / Country") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
#p <- p + geom_errorbar(aes(ymin=Estimate-1.96*se,ymax=Estimate+1.96*se),position=pd, width=.2, size=1)
# p <- p + scale_color_manual(values=c("black","red", "blue", "green"))
#p <- p + scale_color_brewer()
# p <- p + scale_color_manual(labels = c(expression(M_A),expression(M_C),expression(M_F),"DIR"))
p <- p + scale_y_continuous(breaks=c(0.10, 0.15,0.20, 0.25),limits =c(0.08,0.27))
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 

p <- p + coord_flip()

name <- paste0("Nation_State_Estimate_a")  
fname <- paste0(name, ".pdf")
ggsave(fname, units = "in",width = 8, height = 8)


# SA4 level -------------#

# NSW

df_SA4_est <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="NSW",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA4_est$Model <- factor(df_SA4_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_se <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="NSW",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA4_se$Model <- factor(df_SA4_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ll <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="NSW",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA4_ll$Model <- factor(df_SA4_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ul <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="NSW",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA4_ul$Model <- factor(df_SA4_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA4 <- merge(df_SA4_est,df_SA4_se,by=c("SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ll,by=c("SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ul,by=c("SA4_NAME16","Model"))



pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA4$SA4 <- factor(df_SA4$SA4_NAME16, levels = df_SA4$SA4_NAME16[df_SA4$Model=="DIR"][order(df_SA4$Estimate[df_SA4$Model=="DIR"])])

p <- ggplot(data=df_SA4, mapping=aes(x=SA4, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA4") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA4_Estimate_","NSW")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 12)

# VIC

df_SA4_est <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="VIC",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA4_est$Model <- factor(df_SA4_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_se <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="VIC",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA4_se$Model <- factor(df_SA4_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ll <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="VIC",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA4_ll$Model <- factor(df_SA4_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ul <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="VIC",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA4_ul$Model <- factor(df_SA4_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA4 <- merge(df_SA4_est,df_SA4_se,by=c("SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ll,by=c("SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ul,by=c("SA4_NAME16","Model"))



pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA4$SA4 <- factor(df_SA4$SA4_NAME16, levels = df_SA4$SA4_NAME16[df_SA4$Model=="DIR"][order(df_SA4$Estimate[df_SA4$Model=="DIR"])])

p <- ggplot(data=df_SA4, mapping=aes(x=SA4, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA4") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA4_Estimate_","VIC")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 10)

# QLD

df_SA4_est <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="QLD",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA4_est$Model <- factor(df_SA4_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_se <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="QLD",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA4_se$Model <- factor(df_SA4_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ll <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="QLD",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA4_ll$Model <- factor(df_SA4_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ul <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="QLD",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA4_ul$Model <- factor(df_SA4_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA4 <- merge(df_SA4_est,df_SA4_se,by=c("SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ll,by=c("SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ul,by=c("SA4_NAME16","Model"))



pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA4$SA4 <- factor(df_SA4$SA4_NAME16, levels = df_SA4$SA4_NAME16[df_SA4$Model=="DIR"][order(df_SA4$Estimate[df_SA4$Model=="DIR"])])

p <- ggplot(data=df_SA4, mapping=aes(x=SA4, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA4") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA4_Estimate_","QLD")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 8)


# SA

df_SA4_est <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="SA",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA4_est$Model <- factor(df_SA4_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_se <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="SA",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA4_se$Model <- factor(df_SA4_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ll <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="SA",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA4_ll$Model <- factor(df_SA4_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ul <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="SA",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA4_ul$Model <- factor(df_SA4_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA4 <- merge(df_SA4_est,df_SA4_se,by=c("SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ll,by=c("SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ul,by=c("SA4_NAME16","Model"))



pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA4$SA4 <- factor(df_SA4$SA4_NAME16, levels = df_SA4$SA4_NAME16[df_SA4$Model=="DIR"][order(df_SA4$Estimate[df_SA4$Model=="DIR"])])

p <- ggplot(data=df_SA4, mapping=aes(x=SA4, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA4") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA4_Estimate_","SA")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 6)


# WA

df_SA4_est <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="WA",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA4_est$Model <- factor(df_SA4_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_se <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="WA",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA4_se$Model <- factor(df_SA4_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ll <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="WA",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA4_ll$Model <- factor(df_SA4_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ul <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State=="WA",],id.vars = c("SA4_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA4_ul$Model <- factor(df_SA4_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA4 <- merge(df_SA4_est,df_SA4_se,by=c("SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ll,by=c("SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ul,by=c("SA4_NAME16","Model"))



pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA4$SA4 <- factor(df_SA4$SA4_NAME16, levels = df_SA4$SA4_NAME16[df_SA4$Model=="DIR"][order(df_SA4$Estimate[df_SA4$Model=="DIR"])])

p <- ggplot(data=df_SA4, mapping=aes(x=SA4, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA4") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA4_Estimate_","WA")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 6)


# ACT, NT, TAS

df_SA4_est <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State%in%c("ACT","NT","TAS"),],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA4_est$Model <- factor(df_SA4_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_se <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State%in%c("ACT","NT","TAS"),],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA4_se$Model <- factor(df_SA4_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ll <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State%in%c("ACT","NT","TAS"),],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA4_ll$Model <- factor(df_SA4_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ul <- melt(SA4_Level_Estimates[SA4_Level_Estimates$State%in%c("ACT","NT","TAS"),],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA4_ul$Model <- factor(df_SA4_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA4 <- merge(df_SA4_est,df_SA4_se,by=c("State","SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ll,by=c("State","SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ul,by=c("State","SA4_NAME16","Model"))



pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA4$SA4 <- paste(df_SA4$State,df_SA4$SA4_NAME16,sep = ":")

p <- ggplot(data=df_SA4, mapping=aes(x=SA4, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA4") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA4_Estimate_","TAS_NT_ACT")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 6)


# SA4 Level estimate based on proportion of Indigenous adults ---------------------------------#

plot(SA4_Level_Estimates$State,SA4_Level_Estimates$Indigenous_SA4)
plot(SA4_Level_Estimates$State[SA4_Level_Estimates$Indigenous_SA4>0.05],SA4_Level_Estimates$Indigenous_SA4[SA4_Level_Estimates$Indigenous_SA4>0.05])

df_SA4_est <- melt(SA4_Level_Estimates[SA4_Level_Estimates$Indigenous_SA4>=0.05,],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA4_est$Model <- factor(df_SA4_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_se <- melt(SA4_Level_Estimates[SA4_Level_Estimates$Indigenous_SA4>=0.05,],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA4_se$Model <- factor(df_SA4_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ll <- melt(SA4_Level_Estimates[SA4_Level_Estimates$Indigenous_SA4>=0.05,],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA4_ll$Model <- factor(df_SA4_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ul <- melt(SA4_Level_Estimates[SA4_Level_Estimates$Indigenous_SA4>=0.05,],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA4_ul$Model <- factor(df_SA4_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA4 <- merge(df_SA4_est,df_SA4_se,by=c("State","SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ll,by=c("State","SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ul,by=c("State","SA4_NAME16","Model"))



pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA4$SA4 <- paste(df_SA4$State,df_SA4$SA4_NAME16,sep = ":")
df_SA4$SA4 <- factor(df_SA4$SA4, levels = (df_SA4$SA4[df_SA4$Model=="DIR"][order(df_SA4$Estimate[df_SA4$Model=="DIR"])]))

p <- ggplot(data=df_SA4, mapping=aes(x=SA4, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA4") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA4_Estimate_","Indigenous 5 pct")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 10)

# SA4 Level domains with Outer, Remote and very remote areas ----------------------------------# 

SA4_Higher_Indigenous <- unique(SA4_Level_Estimates$SA4_NAME16[SA4_Level_Estimates$Indigenous_SA4>0.05])
SA4_Remote_Areas <- unique(SA3_Level_Estimates$SA4_NAME16[SA3_Level_Estimates$RA_NAME_2016%in%levels(as.factor(SA3_Level_Estimates$RA_NAME_2016))[4:5]])
SA4_Outer_Areas <- unique(SA3_Level_Estimates$SA4_NAME16[SA3_Level_Estimates$RA_NAME_2016%in%levels(as.factor(SA3_Level_Estimates$RA_NAME_2016))[3]])

SA4_Outer <- SA4_Outer_Areas[SA4_Outer_Areas%in%SA4_Higher_Indigenous==FALSE]

summary(SA4_Level_Estimates$n[SA4_Level_Estimates$SA4_NAME16%in%SA4_Outer])

cbind(SA4_Level_Estimates$SA4_NAME16[SA4_Level_Estimates$SA4_NAME16%in%SA4_Outer],
      SA4_Level_Estimates$n[SA4_Level_Estimates$SA4_NAME16%in%SA4_Outer],
      SA4_Level_Estimates$Bachelor_and_Higher_Education_SA4[SA4_Level_Estimates$SA4_NAME16%in%SA4_Outer],
      SA4_Level_Estimates$Indigenous_SA4[SA4_Level_Estimates$SA4_NAME16%in%SA4_Outer])


(SA3_Level_Estimates$IRSD[SA3_Level_Estimates$SA4_NAME16%in%SA4_Outer])
SA3_Level_Estimates$SA4_NAME16[SA3_Level_Estimates$SA4_NAME16%in%SA4_Outer]

df_SA4_est <- melt(SA4_Level_Estimates[SA4_Level_Estimates$SA4_NAME16%in%SA4_Outer,],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA4_est$Model <- factor(df_SA4_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_se <- melt(SA4_Level_Estimates[SA4_Level_Estimates$SA4_NAME16%in%SA4_Outer,],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA4_se$Model <- factor(df_SA4_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ll <- melt(SA4_Level_Estimates[SA4_Level_Estimates$SA4_NAME16%in%SA4_Outer,],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA4_ll$Model <- factor(df_SA4_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ul <- melt(SA4_Level_Estimates[SA4_Level_Estimates$SA4_NAME16%in%SA4_Outer,],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA4_ul$Model <- factor(df_SA4_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA4 <- merge(df_SA4_est,df_SA4_se,by=c("State","SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ll,by=c("State","SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ul,by=c("State","SA4_NAME16","Model"))



pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA4$SA4 <- paste(df_SA4$State,df_SA4$SA4_NAME16,sep = ":")
df_SA4$SA4 <- factor(df_SA4$SA4, levels = (df_SA4$SA4[df_SA4$Model=="DIR"][order(df_SA4$Estimate[df_SA4$Model=="DIR"])]))

p <- ggplot(data=df_SA4, mapping=aes(x=SA4, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA4") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA4_Estimate_","Outer_Region")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 10)



# SA4 domains where SE and CV are very close though extra random effects are added --------#

plot(SA4_Level_Estimates$M2_RRSE_FH,SA4_Level_Estimates$M10_RRSE_FH)
abline(0,1)
plot(SA4_Level_Estimates$M11_RRSE_FH,SA4_Level_Estimates$M10_RRSE_FH)
abline(0,1)
plot(SA4_Level_Estimates$M9_RRSE_FH,SA4_Level_Estimates$M10_RRSE_FH)
abline(0,1)
plot(SA4_Level_Estimates$M8_RRSE_FH,SA4_Level_Estimates$M10_RRSE_FH)
abline(0,1)


sum(SA4_Level_Estimates$M2_CR_FH) # 45
sum(SA4_Level_Estimates$M11_CR_FH) # 54
sum(SA4_Level_Estimates$M8_CR_FH) # 47
sum(SA4_Level_Estimates$M9_CR_FH) # 55
sum(SA4_Level_Estimates$M10_CR_FH) # 58

# Now show these 13 Domains where significant difference is be observed 

SA4_CR_Diff_1 <- SA4_Level_Estimates$SA4_NAME16[SA4_Level_Estimates$M2_CR_FH!=SA4_Level_Estimates$M10_CR_FH]
SA4_CR_Diff_2 <- SA4_Level_Estimates$SA4_NAME16[SA4_Level_Estimates$M11_CR_FH!=SA4_Level_Estimates$M10_CR_FH]
SA4_CR_Diff_3 <- SA4_Level_Estimates$SA4_NAME16[SA4_Level_Estimates$M8_CR_FH!=SA4_Level_Estimates$M10_CR_FH]
SA4_CR_Diff_4 <- SA4_Level_Estimates$SA4_NAME16[SA4_Level_Estimates$M9_CR_FH!=SA4_Level_Estimates$M10_CR_FH]

SA4_CR_Diff <- unique(c(SA4_CR_Diff_1,SA4_CR_Diff_2,SA4_CR_Diff_3,SA4_CR_Diff_4))


SA4_Level_Estimates$n[SA4_Level_Estimates$M2_CR_FH!=SA4_Level_Estimates$M10_CR_FH]
SA4_Level_Estimates$Indigenous_SA4[SA4_Level_Estimates$M2_CR_FH!=SA4_Level_Estimates$M10_CR_FH]

SA4_Level_Estimates$n[SA4_Level_Estimates$M11_CR_FH!=SA4_Level_Estimates$M10_CR_FH]
SA4_Level_Estimates$n[SA4_Level_Estimates$M9_CR_FH!=SA4_Level_Estimates$M10_CR_FH]

df_SA4_est <- melt(SA4_Level_Estimates[SA4_Level_Estimates$SA4_NAME16%in%SA4_CR_Diff,],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA4_est$Model <- factor(df_SA4_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_se <- melt(SA4_Level_Estimates[SA4_Level_Estimates$SA4_NAME16%in%SA4_CR_Diff,],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA4_se$Model <- factor(df_SA4_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ll <- melt(SA4_Level_Estimates[SA4_Level_Estimates$SA4_NAME16%in%SA4_CR_Diff,],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA4_ll$Model <- factor(df_SA4_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ul <- melt(SA4_Level_Estimates[SA4_Level_Estimates$SA4_NAME16%in%SA4_CR_Diff,],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA4_ul$Model <- factor(df_SA4_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA4 <- merge(df_SA4_est,df_SA4_se,by=c("State","SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ll,by=c("State","SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ul,by=c("State","SA4_NAME16","Model"))



pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA4$SA4 <- paste(df_SA4$State,df_SA4$SA4_NAME16,sep = ":")
df_SA4$SA4 <- factor(df_SA4$SA4, levels = (df_SA4$SA4[df_SA4$Model=="DIR"][order(df_SA4$Estimate[df_SA4$Model=="DIR"])]))

p <- ggplot(data=df_SA4, mapping=aes(x=SA4, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA4") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA4_Estimate_","Covered_SA4_Region")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 10)

# SA4 domain smoking prevalence : Uncovered by the Model-based estimator 

# SA4 Domain which are not covered by Model M_F  --------------#

SA4_CR_Diff_5 <- SA4_Level_Estimates$SA4_NAME16[SA4_Level_Estimates$M10_CR_FH==FALSE]

df_SA4_est <- melt(SA4_Level_Estimates[SA4_Level_Estimates$SA4_NAME16%in%SA4_CR_Diff_5,],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA4_est$Model <- factor(df_SA4_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_se <- melt(SA4_Level_Estimates[SA4_Level_Estimates$SA4_NAME16%in%SA4_CR_Diff_5,],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA4_se$Model <- factor(df_SA4_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ll <- melt(SA4_Level_Estimates[SA4_Level_Estimates$SA4_NAME16%in%SA4_CR_Diff_5,],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA4_ll$Model <- factor(df_SA4_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA4_ul <- melt(SA4_Level_Estimates[SA4_Level_Estimates$SA4_NAME16%in%SA4_CR_Diff_5,],id.vars = c("State","SA4_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA4_ul$Model <- factor(df_SA4_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA4 <- merge(df_SA4_est,df_SA4_se,by=c("State","SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ll,by=c("State","SA4_NAME16","Model"))
df_SA4 <- merge(df_SA4,df_SA4_ul,by=c("State","SA4_NAME16","Model"))



pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA4$SA4 <- paste(df_SA4$State,df_SA4$SA4_NAME16,sep = ":")
df_SA4$SA4 <- factor(df_SA4$SA4, levels = (df_SA4$SA4[df_SA4$Model=="DIR"][order(df_SA4$Estimate[df_SA4$Model=="DIR"])]))

p <- ggplot(data=df_SA4, mapping=aes(x=SA4, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA4") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA4_Estimate_","Uncovered_SA4_Region")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 10)



df_SA4_est <- SA4_Level_Estimates[SA4_Level_Estimates$SA4_NAME16%in%SA4_CR_Diff_5,]
df_SA4_est$RANK_SA4_Edu <- rank(df_SA4_est$Bachelor_and_Higher_Education_SA4)
df_SA4_est$RANK_SA4_IND <- rank(df_SA4_est$Indigenous_SA4)

df_SA4_est_melt <- melt(df_SA4_est,id.vars = c("State","SA4_NAME16","Indigenous_SA4","Bachelor_and_Higher_Education_SA4","n","RANK_SA4_Edu","RANK_SA4_IND"),measure.vars = c("M10_est_FH","est.DIR"),variable.name = "Model",value.name = "Prevalence")
#df_SA4_est_melt$Model <- factor(df_SA4_est_melt$Model,levels=c("M_F","DIR"))  
df_SA4_est_melt$Indigenous <- df_SA4_est_melt$Indigenous_SA4
p <- ggplot(data=df_SA4_est_melt, mapping=aes(x=(RANK_SA4_Edu), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=n,shape=State,colour = Model)) + xlab("Rank by higher educated adults") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                             labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p1 <- p

p <- ggplot(data=df_SA4_est_melt, mapping=aes(x=(RANK_SA4_IND), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=Indigenous,shape=State,colour = Model)) + xlab("Rank by Indigenous adults") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p2 <- p

ggarrange(p1,p2,nrow = 2,ncol=1,legend = "right")

name <- paste0("SA4_Estimate_","Uncovered_SA4_Region_Rank")  
fname <- paste0(name, ".pdf")
ggsave(fname, units = "in",width = 8, height = 10)

# SA3 level smoking prevalence -----------------------------#

SA3_Level_Estimates$est.DIR[is.na(SA3_Level_Estimates$est.DIR)] <- NA
SA3_Level_Estimates$est.DIR[(SA3_Level_Estimates$est.DIR==9999)] <- NA
SA3_Level_Estimates$est.DIR[is.na(SA3_Level_Estimates$se.DIR)] <- NA
SA3_Level_Estimates$se.DIR[is.na(SA3_Level_Estimates$est.DIR)] <- NA
SA3_Level_Estimates$ll[is.na(SA3_Level_Estimates$est.DIR)] <- NA
SA3_Level_Estimates$ul[is.na(SA3_Level_Estimates$est.DIR)] <- NA


# NSW -------------#

df_SA3_est <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="NSW",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA3_est$Model <- factor(df_SA3_est$Model,labels=c("M10","M11","M2","DIR"))
#df_SA3_est$Model <- factor(df_SA3_est$Model,levels=c("DIR","M2","M11","M10"))

df_SA3_se <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="NSW",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA3_se$Model <- factor(df_SA3_se$Model,labels=c("M10","M11","M2","DIR"))
#df_SA3_se$Model <- factor(df_SA3_se$Model,levels=c("DIR","M2","M11","M10"))

df_SA3_ll <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="NSW",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA3_ll$Model <- factor(df_SA3_ll$Model,labels=c("M10","M11","M2","DIR"))
#df_SA3_ll$Model <- factor(df_SA3_ll$Model,levels=c("DIR","M2","M11","M10"))

df_SA3_ul <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="NSW",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA3_ul$Model <- factor(df_SA3_ul$Model,labels=c("M10","M11","M2","DIR"))
df_SA3_ul$Model <- factor(df_SA3_ul$Model,levels=c("DIR","M2","M11","M10"))

df_SA3 <- merge(df_SA3_est,df_SA3_se,by=c("SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ll,by=c("SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ul,by=c("SA3_NAME16","Model"))


df_SA3$Estimate[is.na(df_SA3$Estimate)] <- NA
df_SA3$Estimate[(df_SA3$Estimate==9999)] <- NA
df_SA3$se[is.na(df_SA3$Estimate)] <- NA
df_SA3$ll[is.na(df_SA3$Estimate)] <- NA
df_SA3$ul[is.na(df_SA3$Estimate)] <- NA

df_SA3_1 <- df_SA3[c(1:120),]
df_SA3_2 <- df_SA3[c(121:240),]
df_SA3_3 <- df_SA3[c(241:360),]


pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA3_1$SA3 <- factor(df_SA3_1$SA3_NAME16, levels = df_SA3_1$SA3_NAME16[df_SA3_1$Model=="M10"][order(df_SA3_1$Estimate[df_SA3_1$Model=="M10"])])

p <- ggplot(data=df_SA3_1, mapping=aes(x=SA3, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA3") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = (c("red", "green3", "skyblue","purple")), 
                             labels = (expression(M[F], M[C],  M[A], DIR))) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()

name <- paste0("SA3_Estimate_","NSW_1")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 12)


df_SA3_2$SA3 <- factor(df_SA3_2$SA3_NAME16, levels = df_SA3_2$SA3_NAME16[df_SA3_2$Model=="M10"][order(df_SA3_2$Estimate[df_SA3_2$Model=="M10"])])

p <- ggplot(data=df_SA3_2, mapping=aes(x=SA3, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA3") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA3_Estimate_","NSW_2")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 12)


df_SA3_3$SA3 <- factor(df_SA3_3$SA3_NAME16, levels = df_SA3_3$SA3_NAME16[df_SA3_3$Model=="M10"][order(df_SA3_3$Estimate[df_SA3_3$Model=="M10"])])

p <- ggplot(data=df_SA3_3, mapping=aes(x=SA3, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA3") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA3_Estimate_","NSW_3")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 12)


# VIC -------------#

df_SA3_est <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="VIC",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA3_est$Model <- factor(df_SA3_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_se <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="VIC",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA3_se$Model <- factor(df_SA3_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ll <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="VIC",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA3_ll$Model <- factor(df_SA3_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ul <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="VIC",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA3_ul$Model <- factor(df_SA3_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA3 <- merge(df_SA3_est,df_SA3_se,by=c("SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ll,by=c("SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ul,by=c("SA3_NAME16","Model"))


df_SA3$Estimate[is.na(df_SA3$Estimate)] <- NA
df_SA3$Estimate[(df_SA3$Estimate==9999)] <- NA
df_SA3$se[is.na(df_SA3$Estimate)] <- NA
df_SA3$ll[is.na(df_SA3$Estimate)] <- NA
df_SA3$ul[is.na(df_SA3$Estimate)] <- NA

df_SA3_1 <- df_SA3[c(1:132),]
df_SA3_2 <- df_SA3[c(133:264),]


pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA3_1$SA3 <- factor(df_SA3_1$SA3_NAME16, levels = df_SA3_1$SA3_NAME16[df_SA3_1$Model=="M10"][order(df_SA3_1$Estimate[df_SA3_1$Model=="M10"])])

p <- ggplot(data=df_SA3_1, mapping=aes(x=SA3, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA3") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA3_Estimate_","VIC_1")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 12)


df_SA3_2$SA3 <- factor(df_SA3_2$SA3_NAME16, levels = df_SA3_2$SA3_NAME16[df_SA3_2$Model=="M10"][order(df_SA3_2$Estimate[df_SA3_2$Model=="M10"])])

p <- ggplot(data=df_SA3_2, mapping=aes(x=SA3, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA3") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA3_Estimate_","VIC_2")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 12)


# QLD -------------#

df_SA3_est <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="QLD",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA3_est$Model <- factor(df_SA3_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_se <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="QLD",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA3_se$Model <- factor(df_SA3_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ll <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="QLD",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA3_ll$Model <- factor(df_SA3_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ul <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="QLD",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA3_ul$Model <- factor(df_SA3_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA3 <- merge(df_SA3_est,df_SA3_se,by=c("SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ll,by=c("SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ul,by=c("SA3_NAME16","Model"))


df_SA3$Estimate[is.na(df_SA3$Estimate)] <- NA
df_SA3$Estimate[(df_SA3$Estimate==9999)] <- NA
df_SA3$se[is.na(df_SA3$Estimate)] <- NA
df_SA3$ll[is.na(df_SA3$Estimate)] <- NA
df_SA3$ul[is.na(df_SA3$Estimate)] <- NA

df_SA3_1 <- df_SA3[c(1:112),]
df_SA3_2 <- df_SA3[c(113:224),]
df_SA3_3 <- df_SA3[c(225:328),]


pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA3_1$SA3 <- factor(df_SA3_1$SA3_NAME16, levels = df_SA3_1$SA3_NAME16[df_SA3_1$Model=="M10"][order(df_SA3_1$Estimate[df_SA3_1$Model=="M10"])])

p <- ggplot(data=df_SA3_1, mapping=aes(x=SA3, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA3") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA3_Estimate_","QLD_1")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 12)


df_SA3_2$SA3 <- factor(df_SA3_2$SA3_NAME16, levels = df_SA3_2$SA3_NAME16[df_SA3_2$Model=="M10"][order(df_SA3_2$Estimate[df_SA3_2$Model=="M10"])])

p <- ggplot(data=df_SA3_2, mapping=aes(x=SA3, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA3") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA3_Estimate_","QLD_2")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 12)


df_SA3_3$SA3 <- factor(df_SA3_3$SA3_NAME16, levels = df_SA3_3$SA3_NAME16[df_SA3_3$Model=="M10"][order(df_SA3_3$Estimate[df_SA3_3$Model=="M10"])])

p <- ggplot(data=df_SA3_3, mapping=aes(x=SA3, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA3") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA3_Estimate_","QLD_3")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 12)


# SA -------------#

df_SA3_est <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="SA",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA3_est$Model <- factor(df_SA3_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_se <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="SA",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA3_se$Model <- factor(df_SA3_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ll <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="SA",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA3_ll$Model <- factor(df_SA3_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ul <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="SA",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA3_ul$Model <- factor(df_SA3_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA3 <- merge(df_SA3_est,df_SA3_se,by=c("SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ll,by=c("SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ul,by=c("SA3_NAME16","Model"))


df_SA3$Estimate[is.na(df_SA3$Estimate)] <- NA
df_SA3$Estimate[(df_SA3$Estimate==9999)] <- NA
df_SA3$se[is.na(df_SA3$Estimate)] <- NA
df_SA3$ll[is.na(df_SA3$Estimate)] <- NA
df_SA3$ul[is.na(df_SA3$Estimate)] <- NA


pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA3$SA3 <- factor(df_SA3$SA3_NAME16, levels = df_SA3$SA3_NAME16[df_SA3$Model=="M10"][order(df_SA3$Estimate[df_SA3$Model=="M10"])])

p <- ggplot(data=df_SA3, mapping=aes(x=SA3, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA3") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA3_Estimate_","SA")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 12)

# WA -------------#

df_SA3_est <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="WA",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA3_est$Model <- factor(df_SA3_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_se <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="WA",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA3_se$Model <- factor(df_SA3_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ll <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="WA",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA3_ll$Model <- factor(df_SA3_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ul <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="WA",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA3_ul$Model <- factor(df_SA3_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA3 <- merge(df_SA3_est,df_SA3_se,by=c("SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ll,by=c("SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ul,by=c("SA3_NAME16","Model"))


df_SA3$Estimate[is.na(df_SA3$Estimate)] <- NA
df_SA3$Estimate[(df_SA3$Estimate==9999)] <- NA
df_SA3$se[is.na(df_SA3$Estimate)] <- NA
df_SA3$ll[is.na(df_SA3$Estimate)] <- NA
df_SA3$ul[is.na(df_SA3$Estimate)] <- NA


pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA3$SA3 <- factor(df_SA3$SA3_NAME16, levels = df_SA3$SA3_NAME16[df_SA3$Model=="M10"][order(df_SA3$Estimate[df_SA3$Model=="M10"])])

p <- ggplot(data=df_SA3, mapping=aes(x=SA3, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA3") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA3_Estimate_","WA")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 12)


# TAS -------------#

df_SA3_est <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="TAS",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA3_est$Model <- factor(df_SA3_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_se <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="TAS",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA3_se$Model <- factor(df_SA3_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ll <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="TAS",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA3_ll$Model <- factor(df_SA3_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ul <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="TAS",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA3_ul$Model <- factor(df_SA3_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA3 <- merge(df_SA3_est,df_SA3_se,by=c("SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ll,by=c("SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ul,by=c("SA3_NAME16","Model"))


df_SA3$Estimate[is.na(df_SA3$Estimate)] <- NA
df_SA3$Estimate[(df_SA3$Estimate==9999)] <- NA
df_SA3$se[is.na(df_SA3$Estimate)] <- NA
df_SA3$ll[is.na(df_SA3$Estimate)] <- NA
df_SA3$ul[is.na(df_SA3$Estimate)] <- NA


pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA3$SA3 <- factor(df_SA3$SA3_NAME16, levels = df_SA3$SA3_NAME16[df_SA3$Model=="M10"][order(df_SA3$Estimate[df_SA3$Model=="M10"])])

p <- ggplot(data=df_SA3, mapping=aes(x=SA3, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA3") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA3_Estimate_","TAS")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 7)


# NT -------------#

df_SA3_est <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="NT",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA3_est$Model <- factor(df_SA3_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_se <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="NT",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA3_se$Model <- factor(df_SA3_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ll <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="NT",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA3_ll$Model <- factor(df_SA3_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ul <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="NT",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA3_ul$Model <- factor(df_SA3_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA3 <- merge(df_SA3_est,df_SA3_se,by=c("SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ll,by=c("SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ul,by=c("SA3_NAME16","Model"))


df_SA3$Estimate[is.na(df_SA3$Estimate)] <- NA
df_SA3$Estimate[(df_SA3$Estimate==9999)] <- NA
df_SA3$se[is.na(df_SA3$Estimate)] <- NA
df_SA3$ll[is.na(df_SA3$Estimate)] <- NA
df_SA3$ul[is.na(df_SA3$Estimate)] <- NA


pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA3$SA3 <- factor(df_SA3$SA3_NAME16, levels = df_SA3$SA3_NAME16[df_SA3$Model=="M10"][order(df_SA3$Estimate[df_SA3$Model=="M10"])])

p <- ggplot(data=df_SA3, mapping=aes(x=SA3, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA3") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA3_Estimate_","NT")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 6)


# ACT -------------#

df_SA3_est <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="ACT",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA3_est$Model <- factor(df_SA3_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_se <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="ACT",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA3_se$Model <- factor(df_SA3_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ll <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="ACT",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA3_ll$Model <- factor(df_SA3_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ul <- melt(SA3_Level_Estimates[SA3_Level_Estimates$State=="ACT",],id.vars = c("SA3_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA3_ul$Model <- factor(df_SA3_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA3 <- merge(df_SA3_est,df_SA3_se,by=c("SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ll,by=c("SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ul,by=c("SA3_NAME16","Model"))


df_SA3$Estimate[is.na(df_SA3$Estimate)] <- NA
df_SA3$Estimate[(df_SA3$Estimate==9999)] <- NA
df_SA3$se[is.na(df_SA3$Estimate)] <- NA
df_SA3$ll[is.na(df_SA3$Estimate)] <- NA
df_SA3$ul[is.na(df_SA3$Estimate)] <- NA


pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA3$SA3 <- factor(df_SA3$SA3_NAME16, levels = df_SA3$SA3_NAME16[df_SA3$Model=="M10"][order(df_SA3$Estimate[df_SA3$Model=="M10"])])

p <- ggplot(data=df_SA3, mapping=aes(x=SA3, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA3") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA3_Estimate_","ACT")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 6)


# SA3 Domains in Remote and Very Remote Areas & 10\% Indigenous Population --------------------------------#

theme_axis <- theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
                    axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
                    axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                    axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"))

# SA3_Remote_Areas <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$RA_NAME_2016%in%levels(as.factor(SA3_Level_Estimates$RA_NAME_2016))[c(4,5)]]
SA3_Remote_Indigenous_Areas <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$RA_NAME_2016%in%levels(as.factor(SA3_Level_Estimates$RA_NAME_2016))[c(3,4,5)] & SA3_Level_Estimates$Indigenous_SA3>=0.10]
# SA3_Remote_Indigenous_Areas <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$RA_NAME_2016%in%levels(as.factor(SA3_Level_Estimates$RA_NAME_2016))[c(1,2,3,4,5)] & SA3_Level_Estimates$Indigenous_SA3>=0.10]

summary(SA3_Level_Estimates$n[SA3_Level_Estimates$SA3_NAME16%in%SA3_Remote_Indigenous_Areas])

cbind(SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$SA3_NAME16%in%SA3_Remote_Indigenous_Areas],
      SA3_Level_Estimates$n[SA3_Level_Estimates$SA3_NAME16%in%SA3_Remote_Indigenous_Areas],
      SA3_Level_Estimates$Bachelor_and_Higher_Education_SA3[SA3_Level_Estimates$SA3_NAME16%in%SA3_Remote_Indigenous_Areas],
      SA3_Level_Estimates$Indigenous_SA3[SA3_Level_Estimates$SA3_NAME16%in%SA3_Remote_Indigenous_Areas])


(SA3_Level_Estimates$IRSD[SA3_Level_Estimates$SA3_NAME16%in%SA3_Remote_Indigenous_Areas])
SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$SA3_NAME16%in%SA3_Remote_Indigenous_Areas]

df_SA3_est <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_Remote_Indigenous_Areas,],id.vars = c("State","RA","SA3_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA3_est$Model <- factor(df_SA3_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_se <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_Remote_Indigenous_Areas,],id.vars = c("State","RA","SA3_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA3_se$Model <- factor(df_SA3_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ll <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_Remote_Indigenous_Areas,],id.vars = c("State","RA","SA3_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA3_ll$Model <- factor(df_SA3_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ul <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_Remote_Indigenous_Areas,],id.vars = c("State","RA","SA3_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA3_ul$Model <- factor(df_SA3_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA3 <- merge(df_SA3_est,df_SA3_se,by=c("State","RA","SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ll,by=c("State","RA","SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ul,by=c("State","RA","SA3_NAME16","Model"))



pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA3$SA3 <- paste(df_SA3$State,df_SA3$RA,df_SA3$SA3_NAME16,sep = ":")
#df_SA3$SA3 <- factor(df_SA3$SA3, levels = (df_SA3$SA3[df_SA3$Model=="DIR"][order(df_SA3$Estimate[df_SA3$Model=="DIR"])]))

p <- ggplot(data=df_SA3, mapping=aes(x=SA3, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA3") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")+theme_axis
p <- p + coord_flip()


name <- paste0("SA3_Estimate_","Remote_Indigenous_Region")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 10)


# The SA3 Outer domains with 8\% or more Indigenous population in Other Region -----------------#

SA3_Other_Indigenous_Areas <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$RA_NAME_2016%in%levels(as.factor(SA3_Level_Estimates$RA_NAME_2016))[c(3)] & SA3_Level_Estimates$Indigenous_SA3>=0.1]
length(SA3_Other_Indigenous_Areas)
length(SA3_Other_Indigenous_Areas)
length(SA3_Other_Indigenous_Areas)

SA3_Remote_Indigenous_Areas%in%SA3_Other_Indigenous_Areas

unique(SA3_Other_Indigenous_Areas,SA3_Other_Indigenous_Areas)

df_SA3_est <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_Other_Indigenous_Areas,],id.vars = c("State","SA3_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA3_est$Model <- factor(df_SA3_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_se <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_Other_Indigenous_Areas,],id.vars = c("State","SA3_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA3_se$Model <- factor(df_SA3_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ll <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_Other_Indigenous_Areas,],id.vars = c("State","SA3_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA3_ll$Model <- factor(df_SA3_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ul <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_Other_Indigenous_Areas,],id.vars = c("State","SA3_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA3_ul$Model <- factor(df_SA3_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA3 <- merge(df_SA3_est,df_SA3_se,by=c("State","SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ll,by=c("State","SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ul,by=c("State","SA3_NAME16","Model"))



pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA3$SA3 <- paste(df_SA3$State,df_SA3$SA3_NAME16,sep = ":")
df_SA3$SA3 <- factor(df_SA3$SA3, levels = (df_SA3$SA3[df_SA3$Model=="DIR"][order(df_SA3$Estimate[df_SA3$Model=="DIR"])]))

p <- ggplot(data=df_SA3, mapping=aes(x=SA3, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA3") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + coord_flip()


name <- paste0("SA3_Estimate_","Outer_Indigenous_Region")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 10)


# SA3 Domains where CR issue among Models --------------------------------#

# Now show these 13 Domains where significant difference is be observed 

SA3_CR_Diff_1 <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$M2_CR_FH!=SA3_Level_Estimates$M10_CR_FH]
SA3_CR_Diff_2 <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$M11_CR_FH!=SA3_Level_Estimates$M10_CR_FH]
SA3_CR_Diff_3 <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$M8_CR_FH!=SA3_Level_Estimates$M10_CR_FH]
SA3_CR_Diff_4 <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$M9_CR_FH!=SA3_Level_Estimates$M10_CR_FH]



apply(as.matrix(table(SA3_Level_Estimates$State[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff],SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff])),1,sum)

SA3_CR_Diff_NSW_VIC <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff & SA3_Level_Estimates$State%in%c("NSW","VIC")]
SA3_CR_Diff_Others <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff & SA3_Level_Estimates$State%in%c("QLD","SA","WA","TAS")]

df_SA3_est <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff_NSW_VIC,],id.vars = c("State","SA3_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA3_est$Model <- factor(df_SA3_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_se <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff_NSW_VIC,],id.vars = c("State","SA3_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA3_se$Model <- factor(df_SA3_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ll <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff_NSW_VIC,],id.vars = c("State","SA3_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA3_ll$Model <- factor(df_SA3_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ul <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff_NSW_VIC,],id.vars = c("State","SA3_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA3_ul$Model <- factor(df_SA3_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA3 <- merge(df_SA3_est,df_SA3_se,by=c("State","SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ll,by=c("State","SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ul,by=c("State","SA3_NAME16","Model"))

df_SA3_est <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff_Others,],id.vars = c("State","SA3_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA3_est$Model <- factor(df_SA3_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_se <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff_Others,],id.vars = c("State","SA3_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA3_se$Model <- factor(df_SA3_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ll <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff_Others,],id.vars = c("State","SA3_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA3_ll$Model <- factor(df_SA3_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ul <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff_Others,],id.vars = c("State","SA3_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA3_ul$Model <- factor(df_SA3_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA3 <- merge(df_SA3_est,df_SA3_se,by=c("State","SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ll,by=c("State","SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ul,by=c("State","SA3_NAME16","Model"))



pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA3$SA3 <- paste(df_SA3$State,df_SA3$SA3_NAME16,sep = ":")
df_SA3$SA3 <- factor(df_SA3$SA3, levels = (df_SA3$SA3[df_SA3$Model=="DIR"][order(df_SA3$Estimate[df_SA3$Model=="DIR"])]))

p <- ggplot(data=df_SA3, mapping=aes(x=SA3, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA3") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p <- p + coord_flip()


name <- paste0("SA3_Estimate_","Covered_SA3_Region_B")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 10)


# SA3 Domains where CR issue among Models M2, M10, M11--------------------------------#


SA3_CR_Diff_1 <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$M2_CR_FH!=SA3_Level_Estimates$M10_CR_FH]
SA3_CR_Diff_2 <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$M11_CR_FH!=SA3_Level_Estimates$M10_CR_FH]
# SA3_CR_Diff_3 <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$M8_CR_FH!=SA3_Level_Estimates$M10_CR_FH]
# SA3_CR_Diff_4 <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$M9_CR_FH!=SA3_Level_Estimates$M10_CR_FH]

# SA3_CR_Diff <- unique(c(SA3_CR_Diff_1,SA3_CR_Diff_2,SA3_CR_Diff_3,SA3_CR_Diff_4))
SA3_CR_Diff <- unique(c(SA3_CR_Diff_1,SA3_CR_Diff_2))

SA3_CR_Diff_Major <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff & SA3_Level_Estimates$RA=="Major Cities"]
SA3_CR_Diff_Inner <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff & SA3_Level_Estimates$RA=="Inner Regional"]
SA3_CR_Diff_Outer <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff & SA3_Level_Estimates$RA=="Outer Regional"]
SA3_CR_Diff_Remote <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff & SA3_Level_Estimates$RA=="Remote"]
SA3_CR_Diff_Very_Remote <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff & SA3_Level_Estimates$RA=="Very Remote"]

SA3_CR_Diff_Major_Inner <- c(SA3_CR_Diff_Major,SA3_CR_Diff_Inner,SA3_CR_Diff_Outer,SA3_CR_Diff_Remote,SA3_CR_Diff_Very_Remote)

df_SA3_est <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff_Major_Inner,],id.vars = c("State","RA","SA3_NAME16"),measure.vars = c("M10_est_FH","M11_est_FH","M2_est_FH","est.DIR"),variable.name = "Model",value.name = "Estimate")
df_SA3_est$Model <- factor(df_SA3_est$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_se <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff_Major_Inner,],id.vars = c("State","RA","SA3_NAME16"),measure.vars = c("M10_se_FH","M11_se_FH","M2_se_FH","se.DIR"),variable.name = "Model",value.name = "se")
df_SA3_se$Model <- factor(df_SA3_se$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ll <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff_Major_Inner,],id.vars = c("State","RA","SA3_NAME16"),measure.vars = c("M10_est_FH_ll","M11_est_FH_ll","M2_est_FH_ll","ll"),variable.name = "Model",value.name = "ll")
df_SA3_ll$Model <- factor(df_SA3_ll$Model,labels=c("M10","M11","M2","DIR"))

df_SA3_ul <- melt(SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff_Major_Inner,],id.vars = c("State","RA","SA3_NAME16"),measure.vars = c("M10_est_FH_ul","M11_est_FH_ul","M2_est_FH_ul","ul"),variable.name = "Model",value.name = "ul")
df_SA3_ul$Model <- factor(df_SA3_ul$Model,labels=c("M10","M11","M2","DIR"))

df_SA3 <- merge(df_SA3_est,df_SA3_se,by=c("State","RA","SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ll,by=c("State","RA","SA3_NAME16","Model"))
df_SA3 <- merge(df_SA3,df_SA3_ul,by=c("State","RA","SA3_NAME16","Model"))


theme_axis <- theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
                    axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
                    axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                    axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"))


pd <- position_dodge(0.75) # move them .05 to the left and right

df_SA3$SA3 <- paste(df_SA3$State,df_SA3$RA,df_SA3$SA3_NAME16,sep = ":")
# df_SA3$SA3 <- factor(df_SA3$SA3, levels = (df_SA3$SA3[df_SA3$Model=="DIR"][order(df_SA3$Estimate[df_SA3$Model=="DIR"])]))

p <- ggplot(data=df_SA3, mapping=aes(x=SA3, y=Estimate,color=Model,group=Model))  # est is direct estimate
p <- p + geom_point(shape=16,position=pd, size=2) + xlab("SA3") +  ylab("Smoking Prevalence") 
p <- p + geom_errorbar(aes(ymin=ll,ymax=ul),position=pd, width=.5, size=1)
p <- p + scale_colour_manual(name = "Model", values = c("red", "green3", "skyblue","purple"), 
                             labels = expression(M[F], M[C],  M[A], DIR)) 

p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")+theme_axis
p <- p + coord_flip()


name <- paste0("SA3_Estimate_","Covered_SA3_Region_State_RA")  
fname <- paste0(name, ".pdf")
ggsave(fname, p,units = "in",width = 8, height = 12)






# Domains which are uncovered by M_F model by State -------------------------#


# SA3 Domain which are not covered by Model M_F : NSW --------------#

SA3_CR_Diff_5 <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$State=="NSW" & SA3_Level_Estimates$est.DIR < 1 & SA3_Level_Estimates$M10_CR_FH==FALSE]


df_SA3_est <- SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff_5,]
df_SA3_est$RANK_SA3_IRSD <- rank(df_SA3_est$IRSD)
df_SA3_est$RANK_SA3_IND <- rank(df_SA3_est$Indigenous_SA3)
df_SA3_est$RANK_SA3_Edu <- rank(df_SA3_est$Bachelor_and_Higher_Education_SA3)

df_SA3_est_melt <- melt(df_SA3_est,id.vars = c("State","SA3_NAME16","Indigenous_SA3","IRSD","Bachelor_and_Higher_Education_SA3","n","RANK_SA3_Edu","RANK_SA3_IND","RANK_SA3_IRSD"),measure.vars = c("M10_est_FH","est.DIR"),variable.name = "Model",value.name = "Prevalence")
#df_SA3_est_melt$Model <- factor(df_SA3_est_melt$Model,levels=c("M_F","DIR"))  
df_SA3_est_melt$Indigenous <- df_SA3_est_melt$Indigenous_SA3
p <- ggplot(data=df_SA3_est_melt, mapping=aes(x=(RANK_SA3_Edu), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=n,shape=State,colour = Model)) + xlab("Rank by higher educated adults") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p1 <- p

p <- ggplot(data=df_SA3_est_melt, mapping=aes(x=(RANK_SA3_IND), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=Indigenous,shape=State,colour = Model)) + xlab("Rank by Indigenous adults") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p2 <- p

p <- ggplot(data=df_SA3_est_melt, mapping=aes(x=(RANK_SA3_IRSD), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=IRSD,shape=State,colour = Model)) + xlab("Rank by IRSD") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p3 <- p

ggarrange(p1,p2,p3,nrow = 3,ncol=1,legend = "right")

name <- paste0("SA3_Estimate_","Uncovered_SA3_Region_NSW_Rank_1")  
fname <- paste0(name, ".pdf")
ggsave(fname, units = "in",width = 8, height = 12)


# SA3 Domain which are not covered by Model M_F : VIC --------------#

SA3_CR_Diff_6 <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$State=="VIC" & SA3_Level_Estimates$est.DIR < 1 & SA3_Level_Estimates$M10_CR_FH==FALSE]


df_SA3_est <- SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff_6,]
df_SA3_est$RANK_SA3_IRSD <- rank(df_SA3_est$IRSD)
df_SA3_est$RANK_SA3_IND <- rank(df_SA3_est$Indigenous_SA3)
df_SA3_est$RANK_SA3_Edu <- rank(df_SA3_est$Bachelor_and_Higher_Education_SA3)

df_SA3_est_melt <- melt(df_SA3_est,id.vars = c("State","SA3_NAME16","Indigenous_SA3","IRSD","Bachelor_and_Higher_Education_SA3","n","RANK_SA3_Edu","RANK_SA3_IND","RANK_SA3_IRSD"),measure.vars = c("M10_est_FH","est.DIR"),variable.name = "Model",value.name = "Prevalence")
#df_SA3_est_melt$Model <- factor(df_SA3_est_melt$Model,levels=c("M_F","DIR"))  
df_SA3_est_melt$Indigenous <- df_SA3_est_melt$Indigenous_SA3
p <- ggplot(data=df_SA3_est_melt, mapping=aes(x=(RANK_SA3_Edu), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=n,shape=State,colour = Model)) + xlab("Rank by higher educated adults") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p1 <- p

p <- ggplot(data=df_SA3_est_melt, mapping=aes(x=(RANK_SA3_IND), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=Indigenous,shape=State,colour = Model)) + xlab("Rank by Indigenous adults") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p2 <- p

p <- ggplot(data=df_SA3_est_melt, mapping=aes(x=(RANK_SA3_IRSD), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=IRSD,shape=State,colour = Model)) + xlab("Rank by IRSD") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p3 <- p

ggarrange(p1,p2,p3,nrow = 3,ncol=1,legend = "right")

name <- paste0("SA3_Estimate_","Uncovered_SA3_Region_VIC_Rank_1")  
fname <- paste0(name, ".pdf")
ggsave(fname, units = "in",width = 8, height = 12)



# SA3 Domain which are not covered by Model M_F : QLD --------------#

SA3_CR_Diff_7 <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$State=="QLD" & SA3_Level_Estimates$est.DIR < 1 & SA3_Level_Estimates$M10_CR_FH==FALSE]


df_SA3_est <- SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff_7,]
df_SA3_est$RANK_SA3_IRSD <- rank(df_SA3_est$IRSD)
df_SA3_est$RANK_SA3_IND <- rank(df_SA3_est$Indigenous_SA3)
df_SA3_est$RANK_SA3_Edu <- rank(df_SA3_est$Bachelor_and_Higher_Education_SA3)

df_SA3_est_melt <- melt(df_SA3_est,id.vars = c("State","SA3_NAME16","Indigenous_SA3","IRSD","Bachelor_and_Higher_Education_SA3","n","RANK_SA3_Edu","RANK_SA3_IND","RANK_SA3_IRSD"),measure.vars = c("M10_est_FH","est.DIR"),variable.name = "Model",value.name = "Prevalence")
#df_SA3_est_melt$Model <- factor(df_SA3_est_melt$Model,levels=c("M_F","DIR"))  
df_SA3_est_melt$Indigenous <- df_SA3_est_melt$Indigenous_SA3
p <- ggplot(data=df_SA3_est_melt, mapping=aes(x=(RANK_SA3_Edu), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=n,shape=State,colour = Model)) + xlab("Rank by higher educated adults") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p1 <- p

p <- ggplot(data=df_SA3_est_melt, mapping=aes(x=(RANK_SA3_IND), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=Indigenous,shape=State,colour = Model)) + xlab("Rank by Indigenous adults") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p2 <- p

p <- ggplot(data=df_SA3_est_melt, mapping=aes(x=(RANK_SA3_IRSD), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=IRSD,shape=State,colour = Model)) + xlab("Rank by IRSD") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p3 <- p

ggarrange(p1,p2,p3,nrow = 3,ncol=1,legend = "right")

name <- paste0("SA3_Estimate_","Uncovered_SA3_Region_QLD_Rank_1")  
fname <- paste0(name, ".pdf")
ggsave(fname, units = "in",width = 8, height = 12)

# SA3 Domain which are not covered by Model M_F : SA & WA --------------#

SA3_CR_Diff_7 <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$State%in%c("SA","WA") & SA3_Level_Estimates$est.DIR < 1 & SA3_Level_Estimates$M10_CR_FH==FALSE]


df_SA3_est <- SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff_7,]
df_SA3_est$RANK_SA3_IRSD <- rank(df_SA3_est$IRSD)
df_SA3_est$RANK_SA3_IND <- rank(df_SA3_est$Indigenous_SA3)
df_SA3_est$RANK_SA3_Edu <- rank(df_SA3_est$Bachelor_and_Higher_Education_SA3)

df_SA3_est_melt <- melt(df_SA3_est,id.vars = c("State","SA3_NAME16","Indigenous_SA3","IRSD","Bachelor_and_Higher_Education_SA3","n","RANK_SA3_Edu","RANK_SA3_IND","RANK_SA3_IRSD"),measure.vars = c("M10_est_FH","est.DIR"),variable.name = "Model",value.name = "Prevalence")
#df_SA3_est_melt$Model <- factor(df_SA3_est_melt$Model,levels=c("M_F","DIR"))  
df_SA3_est_melt$Indigenous <- df_SA3_est_melt$Indigenous_SA3
p <- ggplot(data=df_SA3_est_melt, mapping=aes(x=(RANK_SA3_Edu), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=n,shape=State,colour = Model)) + xlab("Rank by higher educated adults") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p1 <- p

p <- ggplot(data=df_SA3_est_melt, mapping=aes(x=(RANK_SA3_IND), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=Indigenous,shape=State,colour = Model)) + xlab("Rank by Indigenous adults") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p2 <- p

p <- ggplot(data=df_SA3_est_melt, mapping=aes(x=(RANK_SA3_IRSD), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=IRSD,shape=State,colour = Model)) + xlab("Rank by IRSD") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p3 <- p

ggarrange(p1,p2,p3,nrow = 3,ncol=1,legend = "right")

name <- paste0("SA3_Estimate_","Uncovered_SA3_Region_SA_WA_Rank_1")  
fname <- paste0(name, ".pdf")
ggsave(fname, units = "in",width = 8, height = 12)


# SA3 Domain which are not covered by Model M_F : ACT, NT & TAS --------------#

SA3_CR_Diff_7 <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$State%in%c("TAS","NT","ACT") & SA3_Level_Estimates$est.DIR < 1 & SA3_Level_Estimates$M10_CR_FH==FALSE]


df_SA3_est <- SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff_7,]
df_SA3_est$RANK_SA3_IRSD <- rank(df_SA3_est$IRSD)
df_SA3_est$RANK_SA3_IND <- rank(df_SA3_est$Indigenous_SA3)
df_SA3_est$RANK_SA3_Edu <- rank(df_SA3_est$Bachelor_and_Higher_Education_SA3)

df_SA3_est_melt <- melt(df_SA3_est,id.vars = c("State","SA3_NAME16","Indigenous_SA3","IRSD","Bachelor_and_Higher_Education_SA3","n","RANK_SA3_Edu","RANK_SA3_IND","RANK_SA3_IRSD"),measure.vars = c("M10_est_FH","est.DIR"),variable.name = "Model",value.name = "Prevalence")
#df_SA3_est_melt$Model <- factor(df_SA3_est_melt$Model,levels=c("M_F","DIR"))  
df_SA3_est_melt$Indigenous <- df_SA3_est_melt$Indigenous_SA3
p <- ggplot(data=df_SA3_est_melt, mapping=aes(x=(RANK_SA3_Edu), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=n,shape=State,colour = Model)) + xlab("Rank by higher educated adults") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p1 <- p

p <- ggplot(data=df_SA3_est_melt, mapping=aes(x=(RANK_SA3_IND), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=Indigenous,shape=State,colour = Model)) + xlab("Rank by Indigenous adults") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p2 <- p

p <- ggplot(data=df_SA3_est_melt, mapping=aes(x=(RANK_SA3_IRSD), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=IRSD,shape=State,colour = Model)) + xlab("Rank by IRSD") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p3 <- p

ggarrange(p1,p2,p3,nrow = 3,ncol=1,legend = "right")

name <- paste0("SA3_Estimate_","Uncovered_SA3_Region_ACT_NT_TAS_Rank_1")  
fname <- paste0(name, ".pdf")
ggsave(fname, units = "in",width = 8, height = 12)

# Combined Graph ---------------------------------------------------------# 

SA3_CR_Diff_8 <- SA3_Level_Estimates$SA3_NAME16[SA3_Level_Estimates$State%in%c("NSW","VIC","QLD") & SA3_Level_Estimates$est.DIR < 1 & SA3_Level_Estimates$M10_CR_FH==FALSE]


df_SA3_est <- SA3_Level_Estimates[SA3_Level_Estimates$SA3_NAME16%in%SA3_CR_Diff_8,]
df_SA3_est$RANK_SA3_IRSD <- rank(df_SA3_est$IRSD)
df_SA3_est$RANK_SA3_IND <- rank(df_SA3_est$Indigenous_SA3)
df_SA3_est$RANK_SA3_Edu <- rank(df_SA3_est$Bachelor_and_Higher_Education_SA3)

df_SA3_est_melt <- melt(df_SA3_est,id.vars = c("State","SA3_NAME16","Indigenous_SA3","IRSD","Bachelor_and_Higher_Education_SA3","n","RANK_SA3_Edu","RANK_SA3_IND","RANK_SA3_IRSD"),measure.vars = c("M10_est_FH","est.DIR"),variable.name = "Model",value.name = "Prevalence")
#df_SA3_est_melt$Model <- factor(df_SA3_est_melt$Model,levels=c("M_F","DIR"))  
df_SA3_est_melt$Indigenous <- df_SA3_est_melt$Indigenous_SA3
p <- ggplot(data=df_SA3_est_melt, mapping=aes(x=(RANK_SA3_Edu), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=n,shape=State,colour = Model)) + xlab("Rank by higher educated adults") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p1 <- p

p <- ggplot(data=df_SA3_est_melt, mapping=aes(x=(RANK_SA3_IND), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=Indigenous,shape=State,colour = Model)) + xlab("Rank by Indigenous adults") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p2 <- p

p <- ggplot(data=df_SA3_est_melt, mapping=aes(x=(RANK_SA3_IRSD), y=Prevalence,group=Model,alpha=0.40))  # est is direct estimate
p <- p + geom_point(aes(size=IRSD,shape=State,colour = Model)) + xlab("Rank by IRSD") +  ylab("Smoking Prevalence") 
p <- p + guides(alpha = "none")
p <- p + scale_color_manual(name = "Model", values = c("red", "green3"), 
                            labels = expression(M[F], DIR)) 
p <- p + geom_hline(yintercept = 0.14,linetype = "dotted")
p3 <- p

ggarrange(p1,p2,p3,nrow = 3,ncol=1,legend = "right")

name <- paste0("SA3_Estimate_","Uncovered_SA3_Region_NSW_VIC_QLD_Rank_1")  
fname <- paste0(name, ".pdf")
ggsave(fname, units = "in",width = 8, height = 12)


# Results for JRSSA Main Paper -----------------------------------------------

# Regression coefficients 

# Regression Parameters
Regression_Coefficient_Model <- read_excel("Regression_Coefficient_Model_Type_1.xls")

# FH Model : M_A
Coef_M2<-Regression_Coefficient_Model[c(1:10,16,17),c("Names","Mean_M2","SE_M2","t_M2")]
# Extended FH Model : M_B
Coef_M7<-Regression_Coefficient_Model[c(1:10,16,17),c("Mean_M7","SE_M7","t_M7")]
# Extended Spatial FH Model : M_C
Coef_M10<-Regression_Coefficient_Model[c(1:10,16,17),c("Mean_M10","SE_M10","t_M10")]

# BYM model : ME - coefficient will come from next Output Clearance
Coef_M9<-Regression_Coefficient_Model[c(1:10,16,17),c("Mean_M9","SE_M9","t_M9")]
# Coef_M11<-Regression_Coefficient_Model[c(1:10,16,17),c("Names","Mean_M11","SE_M11","t_M11")]
# ICAR Model : M_D
Coef_M8<-Regression_Coefficient_Model[c(1:10,16,17),c("Mean_M8","SE_M8","t_M8")]

Coef_Table <- cbind(Coef_M2,Coef_M7,Coef_M10,Coef_M9,Coef_M8)
Coef_Table[,c(2,3,5,6,8,9,11,12,14,15)] <- Coef_Table[,c(2,3,5,6,8,9,11,12,14,15)]*100

# Variance Parameters 

# Variance Parameters
Variance_Parameters_Model <- read_excel("Variance_Parameters_Model_Type_1.xls")
Variance_Parameters_Model <- as.data.frame(Variance_Parameters_Model)
Var_M2<-Variance_Parameters_Model[,c("Names","Mean_M2","SE_M2","t_M2")]
Var_M7<-Variance_Parameters_Model[,c("Mean_M7","SE_M7","t_M7")]
Var_M10<-Variance_Parameters_Model[,c("Mean_M10","SE_M10","t_M10")]
Var_M9<-Variance_Parameters_Model[,c("Mean_M9","SE_M9","t_M9")]
Var_M8<-Variance_Parameters_Model[,c("Mean_M8","SE_M8","t_M8")]

# Information Criteria

xtable(IC_Value_Model[c(2,7,10,9,8),c("DIC","WAIC2","LOOIC","ELPD")]
)

# RRSE calculation ------------------------------------

State_Level_Estimates$RRSE_M2 <- (State_Level_Estimates$se.DIR-State_Level_Estimates$M2_se_FH)/State_Level_Estimates$se.DIR
State_Level_Estimates$RRSE_M7 <- (State_Level_Estimates$se.DIR-State_Level_Estimates$M7_se_FH)/State_Level_Estimates$se.DIR
State_Level_Estimates$RRSE_M8 <- (State_Level_Estimates$se.DIR-State_Level_Estimates$M8_se_FH)/State_Level_Estimates$se.DIR
State_Level_Estimates$RRSE_M9 <- (State_Level_Estimates$se.DIR-State_Level_Estimates$M9_se_FH)/State_Level_Estimates$se.DIR
State_Level_Estimates$RRSE_M10 <- (State_Level_Estimates$se.DIR-State_Level_Estimates$M10_se_FH)/State_Level_Estimates$se.DIR

State_RRSE_Mean <- c(mean(State_Level_Estimates$RRSE_M2),
                     mean(State_Level_Estimates$RRSE_M8),
                     mean(State_Level_Estimates$RRSE_M7),
                     mean(State_Level_Estimates$RRSE_M9),
                     mean(State_Level_Estimates$RRSE_M10))*100

boxplot(cbind(M2=State_Level_Estimates$RRSE_M2,
              M8=State_Level_Estimates$RRSE_M8,
              M7=State_Level_Estimates$RRSE_M7,
              M9=State_Level_Estimates$RRSE_M9,
              M10=State_Level_Estimates$RRSE_M10))


cbind(se=State_Level_Estimates$se.DIR,
      M2=State_Level_Estimates$M2_se_FH,
      M8=State_Level_Estimates$M7_se_FH,
      M7=State_Level_Estimates$M8_se_FH,
      M9=State_Level_Estimates$M9_se_FH,
      M10=State_Level_Estimates$M10_se_FH)


SA4_Level_Estimates$RRSE_M2 <- (SA4_Level_Estimates$se.DIR-SA4_Level_Estimates$M2_se_FH)/SA4_Level_Estimates$se.DIR
SA4_Level_Estimates$RRSE_M7 <- (SA4_Level_Estimates$se.DIR-SA4_Level_Estimates$M7_se_FH)/SA4_Level_Estimates$se.DIR
SA4_Level_Estimates$RRSE_M8 <- (SA4_Level_Estimates$se.DIR-SA4_Level_Estimates$M8_se_FH)/SA4_Level_Estimates$se.DIR
SA4_Level_Estimates$RRSE_M9 <- (SA4_Level_Estimates$se.DIR-SA4_Level_Estimates$M9_se_FH)/SA4_Level_Estimates$se.DIR
SA4_Level_Estimates$RRSE_M10 <- (SA4_Level_Estimates$se.DIR-SA4_Level_Estimates$M10_se_FH)/SA4_Level_Estimates$se.DIR

SA4_RRSE_Mean <- c(mean(SA4_Level_Estimates$RRSE_M2),
                     mean(SA4_Level_Estimates$RRSE_M8),
                   mean(SA4_Level_Estimates$RRSE_M7),mean(SA4_Level_Estimates$RRSE_M9),
                   mean(SA4_Level_Estimates$RRSE_M10))*100

boxplot(cbind(M2=SA4_Level_Estimates$RRSE_M2,
              M8=SA4_Level_Estimates$RRSE_M8,
              M7=SA4_Level_Estimates$RRSE_M7,
              M9=SA4_Level_Estimates$RRSE_M9,
              M10=SA4_Level_Estimates$RRSE_M10))


SA3_Level_Estimates$RRSE_M2 <- (SA3_Level_Estimates$se.DIR-SA3_Level_Estimates$M2_se_FH)/SA3_Level_Estimates$se.DIR
SA3_Level_Estimates$RRSE_M7 <- (SA3_Level_Estimates$se.DIR-SA3_Level_Estimates$M7_se_FH)/SA3_Level_Estimates$se.DIR
SA3_Level_Estimates$RRSE_M8 <- (SA3_Level_Estimates$se.DIR-SA3_Level_Estimates$M8_se_FH)/SA3_Level_Estimates$se.DIR
SA3_Level_Estimates$RRSE_M9 <- (SA3_Level_Estimates$se.DIR-SA3_Level_Estimates$M9_se_FH)/SA3_Level_Estimates$se.DIR
SA3_Level_Estimates$RRSE_M10 <- (SA3_Level_Estimates$se.DIR-SA3_Level_Estimates$M10_se_FH)/SA3_Level_Estimates$se.DIR

SA3_RRSE_Mean <- c(mean(SA3_Level_Estimates$RRSE_M2[sub_dist_SA3_domains],na.rm=T),
                   mean(SA3_Level_Estimates$RRSE_M8[sub_dist_SA3_domains],na.rm=T),
                   mean(SA3_Level_Estimates$RRSE_M7[sub_dist_SA3_domains],na.rm=T),
                   mean(SA3_Level_Estimates$RRSE_M9[sub_dist_SA3_domains],na.rm=T),
                   mean(SA3_Level_Estimates$RRSE_M10[sub_dist_SA3_domains],na.rm=T))*100

boxplot(cbind(M2=SA3_Level_Estimates$RRSE_M2,
              M8=SA3_Level_Estimates$RRSE_M8,
              M7=SA3_Level_Estimates$RRSE_M7,
              M9=SA3_Level_Estimates$RRSE_M9,
              M10=SA3_Level_Estimates$RRSE_M10))


RRSE_State_SA4_SA3 <- rbind(State_RRSE_Mean,SA4_RRSE_Mean,SA3_RRSE_Mean)

xtable(RRSE_State_SA4_SA3)


# Distribution of SA3 level Indigenous adults ---------------

plot(SA3_Level_Estimates$State,SA3_Level_Estimates$Indigenous_SA3)
table(SA3_Level_Estimates$RA_NAME_2016,SA3_Level_Estimates$State)
SA3_Level_Estimates$Indigenous_SA3[SA3_Level_Estimates$Indigenous_SA3>0.10]

apply(as.matrix(table(SA3_Level_Estimates$State[SA3_Level_Estimates$Indigenous_SA3>0.10],SA3_Level_Estimates$Indigenous_SA3[SA3_Level_Estimates$Indigenous_SA3>0.10])),1,sum)
      

# Generate Maps --------------------------------
# SA3 Level
load("SA3_Level_Estimates.Rdata")

AUS_SA3 <- readOGR("V:/u1107832/Australian National Health Survey/1270055001_sa3_2016_aust_shape/SA3_2016_AUST.shp")
AUS_SA3 <- readOGR("V:/Australian National Health Survey/1270055001_sa3_2016_aust_shape/SA3_2016_AUST.shp")
AUS_SA3_MAIN<-AUS_SA3[AUS_SA3$SA3_CODE16%in%SA3_Level_Estimates$SA3_2016,]
#plot(AUS_SA3_MAIN)

AUS_SA3_MAIN <- AUS_SA3_MAIN[order(AUS_SA3_MAIN$SA3_CODE16),]
SA3_Level_Estimates <- SA3_Level_Estimates[order(SA3_Level_Estimates$SA3_2016),]
SA3_Level_Estimates$SA3_NAME16
# SA3_Level_Estimates$SA4_NAME16 <- AUS_SA3_MAIN$SA4_NAME16

# Combine DIR and SAE Estimates together
AUS_SA3_MAIN_DIR <- AUS_SA3_MAIN
AUS_SA3_MAIN_SAE <- AUS_SA3_MAIN
AUS_SA3_MAIN_DIR$Prevalence <- SA3_Level_Estimates$est.DIR
AUS_SA3_MAIN_DIR$se <- SA3_Level_Estimates$se.DIR
AUS_SA3_MAIN_DIR$Estimator <- "Direct"

AUS_SA3_MAIN_SAE$Prevalence <- SA3_Level_Estimates$M10_est_FH
AUS_SA3_MAIN_SAE$se <- SA3_Level_Estimates$M10_se_FH
AUS_SA3_MAIN_SAE$Estimator <- "Model-based"

AUS_SA3_MAIN_DIR_SAE <- rbind(AUS_SA3_MAIN_DIR,AUS_SA3_MAIN_SAE)
AUS_SA3_MAIN_DIR_SAE_sf <- st_as_sf(AUS_SA3_MAIN_DIR_SAE)

summary(SA3_Level_Estimates$Population_SA4)
summary(SA3_Level_Estimates$N)

AUS_SA3_MAIN$est.DIR <- SA3_Level_Estimates$est.DIR
AUS_SA3_MAIN$se.DIR <- SA3_Level_Estimates$se.DIR
AUS_SA3_MAIN$est.SAE <- SA3_Level_Estimates$M10_est_FH
AUS_SA3_MAIN$se.SAE <- SA3_Level_Estimates$M10_se_FH

AUS_SA3_MAIN$N <- SA3_Level_Estimates$N


AUS_SA3_MAIN$Prevalence <- SA3_Level_Estimates$M10_est_FH
AUS_SA3_MAIN$se <- SA3_Level_Estimates$M10_se_FH

AUS_SA3_MAIN$IRSD <- SA3_Level_Estimates$IRSD
AUS_SA3_MAIN$RA <- SA3_Level_Estimates$RA_NAME_2016
AUS_SA3_MAIN$Indigenous <- SA3_Level_Estimates$Indigenous_SA3
AUS_SA3_MAIN$Bachelor_and_Higher_Education <- SA3_Level_Estimates$Bachelor_and_Higher_Education_SA3


AUS_SA3_MAIN_sf <- st_as_sf(AUS_SA3_MAIN)




# SA4 Level
load("SA4_Level_Estimates.Rdata")

AUS_SA4 <- readOGR("V:/u1107832/Australian National Health Survey/1270055001_sa4_2016_aust_shape/SA4_2016_AUST.shp")
AUS_SA4 <- readOGR("V:/Australian National Health Survey/1270055001_sa4_2016_aust_shape/SA4_2016_AUST.shp")
AUS_SA4_MAIN<-AUS_SA4[AUS_SA4$SA4_CODE16%in%unique(SA4_Level_Estimates$SA4_2016),]


# Combine DIR and SAE Estimates together
AUS_SA4_MAIN_DIR <- AUS_SA4_MAIN
AUS_SA4_MAIN_SAE <- AUS_SA4_MAIN
AUS_SA4_MAIN_DIR$Prevalence <- SA4_Level_Estimates$est.DIR
AUS_SA4_MAIN_DIR$se <- SA4_Level_Estimates$se.DIR
AUS_SA4_MAIN_DIR$Estimator <- "Direct"

AUS_SA4_MAIN_SAE$Prevalence <- SA4_Level_Estimates$M10_est_FH
AUS_SA4_MAIN_SAE$se <- SA4_Level_Estimates$M10_se_FH
AUS_SA4_MAIN_SAE$Estimator <- "Model-based"

AUS_SA4_MAIN_DIR_SAE <- rbind(AUS_SA4_MAIN_DIR,AUS_SA4_MAIN_SAE)
AUS_SA4_MAIN_DIR_SAE_sf <- st_as_sf(AUS_SA4_MAIN_DIR_SAE)

AUS_SA4_MAIN$est.DIR <- SA4_Level_Estimates$est.DIR
AUS_SA4_MAIN$se.DIR <- SA4_Level_Estimates$se.DIR
AUS_SA4_MAIN$est.SAE <- SA4_Level_Estimates$M10_est_FH
AUS_SA4_MAIN$se.SAE <- SA4_Level_Estimates$M10_se_FH


AUS_SA4_MAIN$IRSD <- SA4_Level_Estimates$IRSD
AUS_SA4_MAIN$Indigenous <- SA4_Level_Estimates$Indigenous_SA4
AUS_SA4_MAIN$Bachelor_and_Higher_Education <- SA4_Level_Estimates$Bachelor_and_Higher_Education_SA4



AUS_SA4_MAIN_sf <- st_as_sf(AUS_SA4_MAIN)


# State Level

load("State_Level_Estimates.Rdata")

AUS_STE <- readOGR("V:/u1107832/Australian National Health Survey/1270055001_ste_2016_aust_shape/STE_2016_AUST.shp")
AUS_STE <- readOGR("V:/Australian National Health Survey/1270055001_ste_2016_aust_shape/STE_2016_AUST.shp")
dim(AUS_STE)
AUS_STE <- AUS_STE[AUS_STE$STE_NAME16!="Other Territories",] # c("New South Wales","Victoria","Queensland","South Australia","Western Australia","Tasmania",) 
AUS_STE$State <- as.factor(AUS_STE$STE_NAME16)
AUS_STE$State <- factor(AUS_STE$State,labels = c("ACT","NSW","NT","QLD","SA","TAS","VIC","WA"))
AUS_STE$State <- factor(AUS_STE$State,levels = c("NSW","QLD","VIC","SA","WA","TAS","NT","ACT"))

AUS_STE_MAIN<-AUS_STE


AUS_STE_MAIN$est.DIR <- State_Level_Estimates$est.DIR
AUS_STE_MAIN$se.DIR <- State_Level_Estimates$se.DIR
AUS_STE_MAIN$est.SAE <- State_Level_Estimates$M10_est_FH
AUS_STE_MAIN$se.SAE <- State_Level_Estimates$M10_se_FH

AUS_STE_MAIN_sf <- st_as_sf(AUS_STE_MAIN)




# Using ggplot and sf Package -------------------------------

thmplot <- theme(
  panel.background = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  plot.margin = margin(0.1, 1, 1, 1),
  plot.title = element_text(size = 16, face = "bold"),
  legend.text=element_text(size=10,face = "bold"),
  legend.title=element_text(size=14,face = "bold"),
  strip.text.x = element_text(size=14,face = "bold"),
  strip.text.y = element_text(size=14,face = "bold"))


AUS_SA3_MAIN_DIR_SAE_sf$Prevalence[AUS_SA3_MAIN_DIR_SAE_sf$Prevalence>1] <- NA

# Consider only Inland in SA3 Plot

p2<-ggplot(data = AUS_SA3_MAIN_DIR_SAE_sf) +
  geom_sf(aes(fill = Prevalence,geometry=geometry),) +
  scale_fill_viridis_c(option = "plasma")+
  # geom_text(aes(label = NAME_2,x = coordinates(District.Bangladesh)[,1], y = coordinates(District.Bangladesh)[,2]),size=1.5, fontface = "bold")+
  xlab("")+ylab("")+thmplot+ggtitle("SA3 Level Stunting: 2017-18")+facet_wrap(~Estimator,nrow = 2)# +theme(legend.position = c(0.003,0.20))
# geom_text(aes(label = sfile2$NAME_2,x = coordinates(BD.level.2)[,1], y = coordinates(BD.level.2)[,2]),size=1.5, fontface = "bold")
ggsave("SA3_Level_Smoking_Map.png",p2, units="in", width=8, height=12, device="png", dpi="print")




# Using Class Interval -----------------------------------------------------# 

library(RColorBrewer)
library(classInt)
library(colorspace)




AUS_SA3_MAIN_DIR_SAE_sf <- AUS_SA3_MAIN_DIR_SAE_sf[AUS_SA3_MAIN_DIR_SAE_sf$SA3_NAME16!="Lord Howe Island",]

# number of bins (Classess)
nclr <- 8 
# brewer.pal makes the color palettes from ColorBrewer available as R palettes.
plotclr <- brewer.pal(nclr, "Oranges") 
plotvar <-round(as.vector(AUS_SA3_MAIN_DIR_SAE_sf$Prevalence*100),4)
summary(plotvar)


# Define your class 
class <- classIntervals(plotvar,
                        nclr,
                        style = "fixed",
                        fixedBreaks = c(0.0,5,8,11,14,17,20,25,60))
colcode <- findColours(class,plotclr)


thmplot <- theme(
  panel.background = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  legend.position = c(0.003,0.10),
  plot.margin = margin(0.1, 1, 1, 1))

thmplot <- theme(
  panel.background = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  plot.margin = margin(0.05, 1, 1, 1),
  plot.title = element_text(size = 12, face = "bold"),
  legend.text=element_text(size=8,face = "bold"),
  legend.title=element_text(size=10,face = "bold"),
  strip.text.x = element_text(size=14,face = "bold"),
  strip.text.y = element_text(size=14,face = "bold"),
  legend.position = c(0.5,0.80),
  legend.key.size = unit(0.5, 'cm'), #change legend key size
  legend.key.height = unit(0.5, 'cm'), #change legend key height
  legend.key.width = unit(0.5, 'cm'), #change legend key width
)


# "#FFF5EB" "#FEE6CE" "#FDD0A2" "#FDAE6B" "#FD8D3C" "#F16913" "#D94801" "#8C2D04"


ggplot() + geom_sf(data = AUS_SA3_MAIN_DIR_SAE_sf, mapping  = aes(fill = colcode), col = "black")+
  scale_colour_manual(name = '%Smoking', aesthetics = c("colour", "fill"), values = c("#8C2D04",
                                                                                       "#D94801",
                                                                                       "#F16913",
                                                                                       "#FD8D3C",
                                                                                       "#FDAE6B",
                                                                                       "#FDD0A2",
                                                                                       "#FEE6CE",
                                                                                       "#FFF5EB"),
                      labels = c("[25,60]","[20,25)","[17,20)","[14,17)","[11,14)","[8,11)","[5,8)","[0.0,5)"))+
  #  geom_text(aes(label = sfile2$NAME_2,x = coordinates(District.Bangladesh)[,1], y = coordinates(District.Bangladesh)[,2]),size=2.5, fontface = "plain")+
  xlab("")+ylab("")+thmplot+ggtitle("SA3 Level Smoking: 2017-2018")+facet_grid(~Estimator)

ggsave("SA3_Level_Smoking_Map_Alt.png",units="in", width=12, height=8, device="png", dpi="print")


# Map of SE --------------------#

AUS_SA3_MAIN_DIR_SAE_sf$se[AUS_SA3_MAIN_DIR_SAE_sf$se>100] <- NA
# number of bins (Classess)
nclr <- 8 
# brewer.pal makes the color palettes from ColorBrewer available as R palettes.
plotclr <- brewer.pal(nclr, "Oranges") 
plotvar <-round(as.vector(AUS_SA3_MAIN_DIR_SAE_sf$se*100),4)
summary(plotvar)


# Define your class 
class <- classIntervals(plotvar,
                        nclr,
                        style = "fixed",
                        fixedBreaks = c(1.1,1.5,1.8,2.0,2.5,4.5,6.0,7.6,18.0))
colcode <- findColours(class,plotclr)


thmplot <- theme(
  panel.background = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  legend.position = c(0.003,0.10),
  plot.margin = margin(0.1, 1, 1, 1))

thmplot <- theme(
  panel.background = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  plot.margin = margin(0.05, 1, 1, 1),
  plot.title = element_text(size = 12, face = "bold"),
  legend.text=element_text(size=8,face = "bold"),
  legend.title=element_text(size=10,face = "bold"),
  strip.text.x = element_text(size=14,face = "bold"),
  strip.text.y = element_text(size=14,face = "bold"),
  legend.position = c(0.5,0.80),
  legend.key.size = unit(0.5, 'cm'), #change legend key size
  legend.key.height = unit(0.5, 'cm'), #change legend key height
  legend.key.width = unit(0.5, 'cm'), #change legend key width
)


# "#FFF5EB" "#FEE6CE" "#FDD0A2" "#FDAE6B" "#FD8D3C" "#F16913" "#D94801" "#8C2D04"


ggplot() + geom_sf(data = AUS_SA3_MAIN_DIR_SAE_sf, mapping  = aes(fill = colcode), col = "black")+
  scale_colour_manual(name = '%SE', aesthetics = c("colour", "fill"), values = c("#8C2D04",
                                                                                      "#D94801",
                                                                                      "#F16913",
                                                                                      "#FD8D3C",
                                                                                      "#FDAE6B",
                                                                                      "#FDD0A2",
                                                                                      "#FEE6CE",
                                                                                      "#FFF5EB"),
                      labels = c("[7.6,18.0]","[6.0,7.6)","[4.5,6.0)","[2.5,4.5)","[2.0,2.5)","[1.8,2.0)","[1.5,1.8)","[1.1,1.5)"))+
  #  geom_text(aes(label = sfile2$NAME_2,x = coordinates(District.Bangladesh)[,1], y = coordinates(District.Bangladesh)[,2]),size=2.5, fontface = "plain")+
  xlab("")+ylab("")+thmplot+ggtitle("SE of SA3 Level Smoking: 2017-2018")+facet_grid(~Estimator)

ggsave("SA3_Level_Smoking_SE_Map_Alt.png",units="in", width=12, height=8, device="png", dpi="print")
# 1.1,1.5,1.8,2.0,2.5,4.5,6.0,7.6,18.0

# SA4 Level Map  ----------------#



# number of bins (Classess)
nclr <- 8 
# brewer.pal makes the color palettes from ColorBrewer available as R palettes.
plotclr <- brewer.pal(nclr, "Oranges") 
plotvar <-round(as.vector(AUS_SA4_MAIN_DIR_SAE_sf$Prevalence*100),4)
summary(plotvar)


# Define your class 
class <- classIntervals(plotvar,
                        nclr,
                        style = "fixed",
                        fixedBreaks = c(4,8,10,12,14,16,20,25,50))
colcode <- findColours(class,plotclr)



thmplot <- theme(
  panel.background = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  plot.margin = margin(0.05, 1, 1, 1),
  plot.title = element_text(size = 12, face = "bold"),
  legend.text=element_text(size=8,face = "bold"),
  legend.title=element_text(size=10,face = "bold"),
  strip.text.x = element_text(size=14,face = "bold"),
  strip.text.y = element_text(size=14,face = "bold"),
  legend.position = c(0.5,0.80),
  legend.key.size = unit(0.5, 'cm'), #change legend key size
  legend.key.height = unit(0.5, 'cm'), #change legend key height
  legend.key.width = unit(0.5, 'cm'), #change legend key width
)


# "#FFF5EB" "#FEE6CE" "#FDD0A2" "#FDAE6B" "#FD8D3C" "#F16913" "#D94801" "#8C2D04"


ggplot() + geom_sf(data = AUS_SA4_MAIN_DIR_SAE_sf, mapping  = aes(fill = colcode), col = "black")+
  scale_colour_manual(name = '%Smoking', aesthetics = c("colour", "fill"), values = c("#8C2D04",
                                                                                      "#D94801",
                                                                                      "#F16913",
                                                                                      "#FD8D3C",
                                                                                      "#FDAE6B",
                                                                                      "#FDD0A2",
                                                                                      "#FEE6CE",
                                                                                      "#FFF5EB"),
                      labels = c("[25,50]","[20,25)","[16,20)","[14,16)","[12,14)","[10,12)","[8,10)","[4,8)"))+
  #  geom_text(aes(label = sfile2$NAME_2,x = coordinates(District.Bangladesh)[,1], y = coordinates(District.Bangladesh)[,2]),size=2.5, fontface = "plain")+
  xlab("")+ylab("")+thmplot+ggtitle("SA4 Level Smoking: 2017-2018")+facet_grid(~Estimator)

ggsave("SA4_Level_Smoking_Map_Alt.png",units="in", width=12, height=8, device="png", dpi="print")

# Map of SE --------------------#

# number of bins (Classess)
nclr <- 8 
# brewer.pal makes the color palettes from ColorBrewer available as R palettes.
plotclr <- brewer.pal(nclr, "Oranges") 
plotvar <-round(as.vector(AUS_SA4_MAIN_DIR_SAE_sf$se*100),4)
summary(plotvar)


# Define your class 
class <- classIntervals(plotvar,
                        nclr,
                        style = "fixed",
                        fixedBreaks = c(0.9,1.3,1.5,1.8,2.1,2.7,3.6,4.3,12.9))
colcode <- findColours(class,plotclr)



thmplot <- theme(
  panel.background = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  plot.margin = margin(0.05, 1, 1, 1),
  plot.title = element_text(size = 12, face = "bold"),
  legend.text=element_text(size=8,face = "bold"),
  legend.title=element_text(size=10,face = "bold"),
  strip.text.x = element_text(size=14,face = "bold"),
  strip.text.y = element_text(size=14,face = "bold"),
  legend.position = c(0.5,0.80),
  legend.key.size = unit(0.5, 'cm'), #change legend key size
  legend.key.height = unit(0.5, 'cm'), #change legend key height
  legend.key.width = unit(0.5, 'cm'), #change legend key width
)


# "#FFF5EB" "#FEE6CE" "#FDD0A2" "#FDAE6B" "#FD8D3C" "#F16913" "#D94801" "#8C2D04"


ggplot() + geom_sf(data = AUS_SA4_MAIN_DIR_SAE_sf, mapping  = aes(fill = colcode), col = "black")+
  scale_colour_manual(name = '%SE', aesthetics = c("colour", "fill"), values = c("#8C2D04",
                                                                                      "#D94801",
                                                                                      "#F16913",
                                                                                      "#FD8D3C",
                                                                                      "#FDAE6B",
                                                                                      "#FDD0A2",
                                                                                      "#FEE6CE",
                                                                                      "#FFF5EB"),
                      labels = c("[4.3,12.9]","[3.6,4.3)","[2.7,3.6)","[2.1,2.7)","[1.8,2.1)","[1.5,1.8)","[1.3,1.5)","[0.9,1.3)"))+
  #  geom_text(aes(label = sfile2$NAME_2,x = coordinates(District.Bangladesh)[,1], y = coordinates(District.Bangladesh)[,2]),size=2.5, fontface = "plain")+
  xlab("")+ylab("")+thmplot+ggtitle("SE of SA4 Level Smoking: 2017-2018")+facet_grid(~Estimator)
# 0.9,1.3,1.5,1.8,2.1,2.7,3.6,4.3,12.9
ggsave("SA4_Level_Smoking_SE_Map_Alt.png",units="in", width=12, height=8, device="png", dpi="print")



# Using tmap : Interactive map ------------------------------

library(tmap)
library(sf)


tmap_mode("view")
SA3_tmap_Smoking <- tm_shape(AUS_SA3_MAIN_sf) +
  tm_polygons(c("Prevalence")) +
  tm_text("SA3_NAME16", size = 1.2) + 
  tm_layout(legend.text.size = 0.8, legend.title.size = 0.8, frame = FALSE, legend.outside = TRUE,
            legend.show = TRUE, title = "Smoking Prevalence: SA3 Level")
tmap_save(SA3_tmap_Smoking, filename = "SA3_tmap_Smoking_Prevalence.html")


tmap_mode("plot")
tm_shape(AUS_SA3_MAIN_sf) +
  tm_polygons(c("IRSD", "Prevalence")) +
  tm_facets(sync = TRUE, ncol = 2)
tmap_save(filename = "SA3_tmap_IRSD_Smoking.png")

AUS_SA3_MAIN_sf$RA <- factor(AUS_SA3_MAIN_sf$RA,levels = levels(as.factor(AUS_SA3_MAIN_sf$RA))[c(2,1,3:5)])
tm_shape(AUS_SA3_MAIN_sf) +
  tm_polygons(c("RA", "Prevalence")) +
  tm_facets(sync = TRUE, ncol = 2)
tmap_save(filename = "SA3_tmap_Remoteness_Smoking.png")

tm_shape(AUS_SA3_MAIN_sf) +
  tm_polygons(c("Indigenous", "Prevalence")) +
  tm_facets(sync = TRUE, ncol = 2)
tmap_save(filename = "SA3_tmap_Indigenous_Smoking.png")

AUS_SA3_MAIN_sf$Education <- AUS_SA3_MAIN_sf$Bachelor_and_Higher_Education
tm_shape(AUS_SA3_MAIN_sf) +
  tm_polygons(c("Education", "Prevalence")) +
  tm_facets(sync = TRUE, ncol = 2)
tmap_save(filename = "SA3_tmap_Higher_Education_Smoking.png")


# SA4 Domain having SA3 outside of inland

AUS_SA3_MAIN_DIR_SAE_sf <- AUS_SA3_MAIN_DIR_SAE_sf[AUS_SA3_MAIN_DIR_SAE_sf$SA3_NAME16!="Lord Howe Island",]

AUS_SA3_MAIN$SA4_NAME16[AUS_SA3_MAIN$SA3_NAME16=="Lord Howe Island"]
AUS_SA3_MAIN$SA3_NAME16[AUS_SA3_MAIN$SA4_NAME16=="Mid North Coast"]

AUS_SA4_MAIN$Prevalence <- SA4_Level_Estimates$M10_est_FH
AUS_SA4_MAIN$se <- SA4_Level_Estimates$M10_se_FH

SA4_Domian_List <- (cbind(AUS_SA4_MAIN$STE_NAME16,AUS_SA4_MAIN$STE_CODE16,AUS_SA4_MAIN$SA4_CODE16,AUS_SA4_MAIN$SA4_NAME16))

print(xtable(SA4_Domian_List), include.rownames=FALSE)


AUS_SA3_MAIN_NSW <- AUS_SA3_MAIN[AUS_SA3_MAIN$STE_CODE16==1,]
SA3_Domian_List <- (cbind(AUS_SA3_MAIN_NSW$STE_NAME16,AUS_SA3_MAIN_NSW$STE_CODE16,AUS_SA3_MAIN_NSW$SA4_CODE16,AUS_SA3_MAIN_NSW$SA3_CODE16,AUS_SA3_MAIN_NSW$SA3_NAME16))

print(xtable(SA3_Domian_List), include.rownames=FALSE)

AUS_SA3_MAIN_VIC <- AUS_SA3_MAIN[AUS_SA3_MAIN$STE_CODE16==2,]
SA3_Domian_List <- (cbind(AUS_SA3_MAIN_VIC$STE_NAME16,AUS_SA3_MAIN_VIC$STE_CODE16,AUS_SA3_MAIN_VIC$SA4_CODE16,AUS_SA3_MAIN_VIC$SA3_CODE16,AUS_SA3_MAIN_VIC$SA3_NAME16))

print(xtable(SA3_Domian_List), include.rownames=FALSE)

AUS_SA3_MAIN_QLD <- AUS_SA3_MAIN[AUS_SA3_MAIN$STE_CODE16==3,]
SA3_Domian_List <- (cbind(AUS_SA3_MAIN_QLD$STE_NAME16,AUS_SA3_MAIN_QLD$STE_CODE16,AUS_SA3_MAIN_QLD$SA4_CODE16,AUS_SA3_MAIN_QLD$SA3_CODE16,AUS_SA3_MAIN_QLD$SA3_NAME16))

print(xtable(SA3_Domian_List), include.rownames=FALSE)


AUS_SA3_MAIN_Others <- AUS_SA3_MAIN[AUS_SA3_MAIN$STE_CODE16%in%c(4:8),]
SA3_Domian_List <- (cbind(AUS_SA3_MAIN_Others$STE_NAME16,AUS_SA3_MAIN_Others$STE_CODE16,AUS_SA3_MAIN_Others$SA4_CODE16,AUS_SA3_MAIN_Others$SA3_CODE16,AUS_SA3_MAIN_Others$SA3_NAME16))

print(xtable(SA3_Domian_List), include.rownames=FALSE)



AUS_SA4_MAIN_sf <- st_as_sf(AUS_SA4_MAIN)

AUS_SA4_MAIN_sf <- AUS_SA4_MAIN_sf[AUS_SA4_MAIN_sf$SA4_NAME16!="Mid North Coast",]

tmap_mode("view")
SA4_tmap_Smoking <- tm_shape(AUS_SA4_MAIN_sf) +
  tm_polygons(c("Prevalence")) +
  tm_text("SA4_NAME16", size = 1.2) + 
  tm_layout(legend.text.size = 0.8, legend.title.size = 0.8, frame = FALSE, legend.outside = TRUE,
            legend.show = TRUE, title = "Smoking Prevalence: SA4 Level")
tmap_save(SA4_tmap_Smoking, filename = "SA4_tmap_Smoking_Prevalence.html")

tmap_mode("plot")
tm_shape(AUS_SA4_MAIN_sf) +
  tm_polygons(c("IRSD", "Prevalence")) +
  tm_facets(sync = TRUE, ncol = 2)
tmap_save(filename = "SA4_tmap_IRSD_Smoking.png")

tm_shape(AUS_SA4_MAIN_sf) +
  tm_polygons(c("Indigenous", "Prevalence")) +
  tm_facets(sync = TRUE, ncol = 2)
tmap_save(filename = "SA4_tmap_Indigenous_Smoking.png")

AUS_SA4_MAIN_sf$Education <- AUS_SA4_MAIN_sf$Bachelor_and_Higher_Education
tm_shape(AUS_SA4_MAIN_sf) +
  tm_polygons(c("Education", "Prevalence")) +
  tm_facets(sync = TRUE, ncol = 2)
tmap_save(filename = "SA4_tmap_Higher_Education_Smoking.png")


# Map : NSW & ACT
tmap_mode("plot")
SA4_tmap_Smoking <- tm_shape(AUS_SA4_MAIN_sf[AUS_SA4_MAIN_sf$STE_NAME16%in%c("Australian Capital Territory","New South Wales"),]) +
  tm_polygons(c("Prevalence")) +
  tm_text("SA4_CODE16", size = 0.5) + 
  tm_layout(legend.text.size = 0.5, legend.title.size = 0.5, frame = FALSE, legend.outside = F,
            legend.show = TRUE)
tmap_save(SA4_tmap_Smoking, filename = "SA4_tmap_Smoking_Prevalence_NSW_ACT.png")

# Map : vIC
tmap_mode("plot")
SA4_tmap_Smoking <- tm_shape(AUS_SA4_MAIN_sf[AUS_SA4_MAIN_sf$STE_NAME16%in%c("Victoria"),]) +
  tm_polygons(c("Prevalence")) +
  tm_text("SA4_CODE16", size = 0.4) + 
  tm_layout(legend.text.size = 0.5, legend.title.size = 0.5, frame = FALSE, legend.outside = F,
            legend.show = TRUE)
tmap_save(SA4_tmap_Smoking, filename = "SA4_tmap_Smoking_Prevalence_VIC.png")

# Map : QLD
tmap_mode("plot")
SA4_tmap_Smoking <- tm_shape(AUS_SA4_MAIN_sf[AUS_SA4_MAIN_sf$STE_NAME16%in%c("Queensland"),]) +
  tm_polygons(c("Prevalence")) +
  tm_text("SA4_CODE16", size = 0.4) + 
  tm_layout(legend.text.size = 0.5, legend.title.size = 0.5, frame = FALSE, legend.outside = F,
            legend.show = TRUE)
tmap_save(SA4_tmap_Smoking, filename = "SA4_tmap_Smoking_Prevalence_QLD.png")


# Map : SA
tmap_mode("plot")
SA4_tmap_Smoking <- tm_shape(AUS_SA4_MAIN_sf[AUS_SA4_MAIN_sf$STE_NAME16%in%c("South Australia"),]) +
  tm_polygons(c("Prevalence")) +
  tm_text("SA4_CODE16", size = 0.5) + 
  tm_layout(legend.text.size = 0.5, legend.title.size = 0.5, frame = FALSE, legend.outside = F,
            legend.show = TRUE)
tmap_save(SA4_tmap_Smoking, filename = "SA4_tmap_Smoking_Prevalence_SA.png",width = 8,height =12,units = "in")


# Map : WA
tmap_mode("plot")
SA4_tmap_Smoking <- tm_shape(AUS_SA4_MAIN_sf[AUS_SA4_MAIN_sf$STE_NAME16%in%c("Western Australia"),]) +
  tm_polygons(c("Prevalence")) +
  tm_text("SA4_CODE16", size = 0.5) + 
  tm_layout(legend.text.size = 0.5, legend.title.size = 0.5, frame = FALSE, legend.outside = F,
            legend.show = TRUE)
tmap_save(SA4_tmap_Smoking, filename = "SA4_tmap_Smoking_Prevalence_WA.png",width = 6.5,height =7.5,units = "in",dpi = 400)

# Map : NT
tmap_mode("plot")
SA4_tmap_Smoking <- tm_shape(AUS_SA4_MAIN_sf[AUS_SA4_MAIN_sf$STE_NAME16%in%c("Northern Territory"),]) +
  tm_polygons(c("Prevalence")) +
  tm_text("SA4_CODE16", size = 0.5) + 
  tm_layout(legend.text.size = 0.5, legend.title.size = 0.5, frame = FALSE, legend.outside = F,
            legend.show = TRUE)
tmap_save(SA4_tmap_Smoking, filename = "SA4_tmap_Smoking_Prevalence_NT.png")


# Map : TAS
tmap_mode("plot")
SA4_tmap_Smoking <- tm_shape(AUS_SA4_MAIN_sf[AUS_SA4_MAIN_sf$STE_NAME16%in%c("Tasmania"),]) +
  tm_polygons(c("Prevalence")) +
  tm_text("SA4_CODE16", size = 0.5) + 
  tm_layout(legend.text.size = 0.5, legend.title.size = 0.5, frame = FALSE, legend.outside = F,
            legend.show = TRUE)
tmap_save(SA4_tmap_Smoking, filename = "SA4_tmap_Smoking_Prevalence_TAS.png")



# SA3 Level Maps 

# Tasmania 
AUS_SA3_MAIN_sf_TAS <- AUS_SA3_MAIN_sf[AUS_SA3_MAIN_sf$STE_CODE16==6,]
AUS_SA3_MAIN_sf_TAS$SA3_CODE <- as.numeric(substr(AUS_SA3_MAIN_sf_TAS$SA3_CODE16,2,5))
SA3_tmap_Smoking <- tm_shape(AUS_SA3_MAIN_sf_TAS) +
  tm_polygons(c("Prevalence")) +
  tm_text("SA3_CODE", size = 0.5) + 
  tm_layout(legend.text.size = 0.5, legend.title.size = 0.5, frame = FALSE, legend.outside = F,
            legend.show = TRUE)
tmap_save(SA3_tmap_Smoking, filename = "SA3_tmap_Smoking_Prevalence_TAS.png")

# ACT 
AUS_SA3_MAIN_sf_ACT <- AUS_SA3_MAIN_sf[AUS_SA3_MAIN_sf$STE_CODE16==8,]
AUS_SA3_MAIN_sf_ACT$SA3_CODE <- as.numeric(substr(AUS_SA3_MAIN_sf_ACT$SA3_CODE16,2,5))
SA3_tmap_Smoking <- tm_shape(AUS_SA3_MAIN_sf_ACT) +
  tm_polygons(c("Prevalence")) +
  tm_text("SA3_CODE", size = 0.5) + 
  tm_layout(legend.text.size = 0.5, legend.title.size = 0.5, frame = FALSE, legend.outside = F,
            legend.show = TRUE)
tmap_save(SA3_tmap_Smoking, filename = "SA3_tmap_Smoking_Prevalence_ACT.png")

# NT 
AUS_SA3_MAIN_sf_NT <- AUS_SA3_MAIN_sf[AUS_SA3_MAIN_sf$STE_CODE16==7,]
AUS_SA3_MAIN_sf_NT$SA3_CODE <- as.numeric(substr(AUS_SA3_MAIN_sf_NT$SA3_CODE16,2,5))
SA3_tmap_Smoking <- tm_shape(AUS_SA3_MAIN_sf_NT) +
  tm_polygons(c("Prevalence")) +
  tm_text("SA3_CODE", size = 0.5) + 
  tm_layout(legend.text.size = 0.5, legend.title.size = 0.5, frame = FALSE, legend.outside = F,
            legend.show = TRUE)
tmap_save(SA3_tmap_Smoking, filename = "SA3_tmap_Smoking_Prevalence_NT.png")


# SA 
AUS_SA3_MAIN_sf_SA <- AUS_SA3_MAIN_sf[AUS_SA3_MAIN_sf$STE_CODE16==4,]
AUS_SA3_MAIN_sf_SA$SA3_CODE <- as.numeric(substr(AUS_SA3_MAIN_sf_SA$SA3_CODE16,2,5))
SA3_tmap_Smoking <- tm_shape(AUS_SA3_MAIN_sf_SA) +
  tm_polygons(c("Prevalence")) +
  tm_text("SA3_CODE", size = 0.5) + 
  tm_layout(legend.text.size = 0.5, legend.title.size = 0.5, frame = FALSE, legend.outside = F,
            legend.show = TRUE)
tmap_save(SA3_tmap_Smoking, filename = "SA3_tmap_Smoking_Prevalence_SA.png")


# WA 
AUS_SA3_MAIN_sf_WA <- AUS_SA3_MAIN_sf[AUS_SA3_MAIN_sf$STE_CODE16==5,]
AUS_SA3_MAIN_sf_WA$SA3_CODE <- as.numeric(substr(AUS_SA3_MAIN_sf_WA$SA3_CODE16,2,5))
SA3_tmap_Smoking <- tm_shape(AUS_SA3_MAIN_sf_WA) +
  tm_polygons(c("Prevalence")) +
  tm_text("SA3_CODE", size = 0.5) + 
  tm_layout(legend.text.size = 0.5, legend.title.size = 0.5, frame = FALSE, legend.outside = F,
            legend.show = TRUE)
tmap_save(SA3_tmap_Smoking, filename = "SA3_tmap_Smoking_Prevalence_WA.png")





# QLD
AUS_SA3_MAIN_sf_QLD <- AUS_SA3_MAIN_sf[AUS_SA3_MAIN_sf$STE_CODE16==3,]
AUS_SA3_MAIN_sf_QLD$SA3_CODE <- as.numeric(substr(AUS_SA3_MAIN_sf_QLD$SA3_CODE16,2,5))
SA3_tmap_Smoking <- tm_shape(AUS_SA3_MAIN_sf_QLD) +
  tm_polygons(c("Prevalence")) +
  tm_text("SA3_CODE", size = 0.5) + 
  tm_layout(legend.text.size = 0.5, legend.title.size = 0.5, frame = FALSE, legend.outside = F,
            legend.show = TRUE)
tmap_save(SA3_tmap_Smoking, filename = "SA3_tmap_Smoking_Prevalence_QLD.png")

# VIC
AUS_SA3_MAIN_sf_VIC <- AUS_SA3_MAIN_sf[AUS_SA3_MAIN_sf$STE_CODE16==2,]
AUS_SA3_MAIN_sf_VIC$SA3_CODE <- as.numeric(substr(AUS_SA3_MAIN_sf_VIC$SA3_CODE16,2,5))
SA3_tmap_Smoking <- tm_shape(AUS_SA3_MAIN_sf_VIC) +
  tm_polygons(c("Prevalence")) +
  tm_text("SA3_CODE", size = 0.5) + 
  tm_layout(legend.text.size = 0.5, legend.title.size = 0.5, frame = FALSE, legend.outside = F,
            legend.show = TRUE)
tmap_save(SA3_tmap_Smoking, filename = "SA3_tmap_Smoking_Prevalence_VIC.png")


# NSW
AUS_SA3_MAIN_sf_NSW <- AUS_SA3_MAIN_sf[AUS_SA3_MAIN_sf$STE_CODE16==1,]
AUS_SA3_MAIN_sf_NSW$SA3_CODE <- as.numeric(substr(AUS_SA3_MAIN_sf_NSW$SA3_CODE16,2,5))
SA3_tmap_Smoking <- tm_shape(AUS_SA3_MAIN_sf_NSW[AUS_SA3_MAIN_sf_NSW$SA3_CODE!=803,]) +
  tm_polygons(c("Prevalence")) +
  tm_text("SA3_CODE", size = 0.5) + 
  tm_layout(legend.text.size = 0.5, legend.title.size = 0.5, frame = FALSE, legend.outside = F,
            legend.show = TRUE)
tmap_save(SA3_tmap_Smoking, filename = "SA3_tmap_Smoking_Prevalence_NSW.png")

# Bubble plot of population
tmap_mode("plot")
tm1 <- tm_shape(AUS_STE_MAIN_sf) + tm_polygons("AREASQKM16", convert2density = TRUE)
tm2 <- tm_shape(AUS_STE_MAIN_sf) + tm_bubbles(size = "AREASQKM16")

tm3 <- tm_shape(AUS_SA4_MAIN_sf) + tm_polygons("est.SAE", convert2density = F)
tm4 <- tm_shape(AUS_SA4_MAIN_sf) + tm_bubbles(size = "est.SAE")

tm5 <- tm_shape(AUS_SA3_MAIN_sf) + tm_polygons("est.SAE", convert2density = F)
tm6 <- tm_shape(AUS_SA3_MAIN_sf) + tm_bubbles(size = "est.SAE")
tmap_arrange(tm5, tm6)

tm5 <- tm_shape(AUS_SA3_MAIN_sf) + tm_polygons("N", convert2density = T)
tm6 <- tm_shape(AUS_SA3_MAIN_sf) + tm_bubbles(size = "N")
tmap_arrange(tm5, tm6)



