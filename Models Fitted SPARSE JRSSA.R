# Final Models: SPARSE JRSSA Paper
# Using mcmcsae Package

# Required Source File: "runmodel_Smoking_SA3.R" and "model_Output_Smoking_SA3.R"
source("runmodel_Smoking_SA3.R")

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
library(rgdal)
library(dotwhisker)
library(rmarkdown)
library(reshape2)
cl <- setup_cluster(3)
library(GIGrvg)
library(bayesplot)
library(loo)
parallel::clusterEvalQ(cl, library(GIGrvg))
library(xlsx)

# The Data File used in ABS DataLAb
load("dat_SP.Rdata")

# SA3 Level Shape File: Available fron ABS
# https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.001July%202016?OpenDocument

AUS_SA3 <- readOGR("1270055001_sa3_2016_aust_shape/SA3_2016_AUST.shp")
AUS_SA3_MAIN<-AUS_SA3[AUS_SA3$SA3_CODE16%in%dat$SA3_2016_ID,]

AUS_SA3_MAIN <- AUS_SA3_MAIN[order(AUS_SA3_MAIN$SA3_CODE16),]
dat <- dat[order(dat$SA3_2016_ID),]
dat$SA4_NAME16 <- AUS_SA3_MAIN$SA4_NAME16

# SA4 Level Shape File: Available fron ABS
AUS_SA4 <- readOGR("1270055001_sa4_2016_aust_shape/SA4_2016_AUST.shp")
AUS_SA4_MAIN<-AUS_SA4[AUS_SA4$SA4_CODE16%in%unique(dat$SA4_2016),]


# Run Hierarchical Bayes Models -------------------------------#

# Model M_A in the Manuscript

model_id <- "sqrt_model_M_A"
model <- sqrt_mean ~ reg(~ 1 + State + IRSD +  Bachelor_and_Higher_Education + RA_NAME_2016_m2,remove.redundant = "TRUE" , name="beta") +
  gen(formula=~1, factor = ~SA3_2016,  name="r_WN_SA3")  
predX <- computeDesignMatrix(model,data=dat)
model.V <- NULL
sampler <- create_sampler(model,formula.V =model.V, sigma.fixed=TRUE, Q0=1/dat$pred_sqrt_se[sub_dist_with_zero]^2, linpred=predX, data=dat[sub_dist_with_zero,], block = TRUE) # linpred="fitted"

dat$sqrt_se <- NA
dat$sqrt_se[sub_dist] <-dat$pred_sqrt_se[sub_dist]
out <- runmodel_smoking_SA3(model_id, parallel=TRUE, cl=cl, seed=1, burnin=1000, n.iter=10000, thin=5,predX=predX, bias.correction=TRUE, save.full.pred=FALSE)

# Model M_A in the Manuscript

model_id <- "sqrt_model_M_A"
model <- sqrt_mean ~ reg(~ 1 + State + IRSD +  Bachelor_and_Higher_Education + RA_NAME_2016_m2,remove.redundant = "TRUE" , name="beta") +
  gen(formula=~1, factor = ~SA3_2016,  name="r_WN_SA3")  
predX <- computeDesignMatrix(model,data=dat)
model.V <- NULL
sampler <- create_sampler(model,formula.V =model.V, sigma.fixed=TRUE, Q0=1/dat$pred_sqrt_se[sub_dist_with_zero]^2, linpred=predX, data=dat[sub_dist_with_zero,], block = TRUE) # linpred="fitted"

dat$sqrt_se <- NA
dat$sqrt_se[sub_dist] <-dat$pred_sqrt_se[sub_dist]
out <- runmodel_smoking_SA3(model_id, parallel=TRUE, cl=cl, seed=1, burnin=1000, n.iter=10000, thin=5,predX=predX, bias.correction=TRUE, save.full.pred=FALSE)

# Model M_B in the Manuscript

model_id <- "sqrt_model_M_B"
model <- sqrt_mean ~ reg(~ 1 + State + IRSD +  Bachelor_and_Higher_Education + RA_NAME_2016_m2,remove.redundant = "TRUE" , name="beta") +
  gen(formula=~1, factor = ~spatial(SA3_2016, poly.df=AUS_SA3_MAIN,derive.constraints=TRUE),name="spatial_SA3")
  
predX <- computeDesignMatrix(model,data=dat)
model.V <- NULL
sampler <- create_sampler(model,formula.V =model.V, sigma.fixed=TRUE, Q0=1/dat$pred_sqrt_se[sub_dist_with_zero]^2, linpred=predX, data=dat[sub_dist_with_zero,], block = TRUE) # linpred="fitted"

dat$sqrt_se <- NA
dat$sqrt_se[sub_dist] <-dat$pred_sqrt_se[sub_dist]
out <- runmodel_smoking_SA3(model_id, parallel=TRUE, cl=cl, seed=1, burnin=1000, n.iter=10000, thin=5,predX=predX, bias.correction=TRUE, save.full.pred=FALSE)

# Model M_C
model_id <- "sqrt_model_M_C"
model <- sqrt_mean ~ reg(~ 1 + State + IRSD +  Bachelor_and_Higher_Education + RA_NAME_2016_m2,remove.redundant = "TRUE" , name="beta") +
  gen(formula=~1, factor = ~spatial(SA3_2016, poly.df=AUS_SA3_MAIN,derive.constraints=TRUE),name="spatial_SA3") +
  gen(formula=~1, factor = ~SA3_2016,  name="r_WN_SA3")  

predX <- computeDesignMatrix(model,data=dat)
model.V <- NULL
sampler <- create_sampler(model,formula.V =model.V, sigma.fixed=TRUE, Q0=1/dat$pred_sqrt_se[sub_dist_with_zero]^2, linpred=predX, data=dat[sub_dist_with_zero,], block = TRUE) # linpred="fitted"

dat$sqrt_se <- NA
dat$sqrt_se[sub_dist] <-dat$pred_sqrt_se[sub_dist]
out <- runmodel_smoking_SA3(model_id, parallel=TRUE, cl=cl, seed=1, burnin=1000, n.iter=10000, thin=5,predX=predX, bias.correction=TRUE, save.full.pred=FALSE)

# Model M_D in the Manuscript

model_id <- "sqrt_model_M_D"
model <- sqrt_mean ~ reg(~ 1 + State + IRSD +  Bachelor_and_Higher_Education + RA_NAME_2016_m2,remove.redundant = "TRUE" , name="beta") +
  gen(formula=~1, factor = ~SA4_2016,  name="r_WN_SA4")+
  gen(formula=~1, factor = ~SA3_2016,  name="r_WN_SA3")  
predX <- computeDesignMatrix(model,data=dat)
model.V <- NULL
sampler <- create_sampler(model,formula.V =model.V, sigma.fixed=TRUE, Q0=1/dat$pred_sqrt_se[sub_dist_with_zero]^2, linpred=predX, data=dat[sub_dist_with_zero,], block = TRUE) # linpred="fitted"

# Model M_E in the Manuscript

model_id <- "sqrt_model_M_E"
model <- sqrt_mean ~ reg(~ 1 + State + IRSD +  Bachelor_and_Higher_Education + RA_NAME_2016_m2,remove.redundant = "TRUE" , name="beta") +
  gen(formula=~1, factor = ~SA4_2016,  name="r_WN_SA4")+
  gen(formula=~1, factor = ~spatial(SA3_2016, poly.df=AUS_SA3_MAIN,derive.constraints=TRUE),name="spatial_SA3")

predX <- computeDesignMatrix(model,data=dat)
model.V <- NULL
sampler <- create_sampler(model,formula.V =model.V, sigma.fixed=TRUE, Q0=1/dat$pred_sqrt_se[sub_dist_with_zero]^2, linpred=predX, data=dat[sub_dist_with_zero,], block = TRUE) # linpred="fitted"

dat$sqrt_se <- NA
dat$sqrt_se[sub_dist] <-dat$pred_sqrt_se[sub_dist]
out <- runmodel_smoking_SA3(model_id, parallel=TRUE, cl=cl, seed=1, burnin=1000, n.iter=10000, thin=5,predX=predX, bias.correction=TRUE, save.full.pred=FALSE)

# Model M_F in the Manuscript
model_id <- "sqrt_model_M_F"
model <- sqrt_mean ~ reg(~ 1 + State + IRSD +  Bachelor_and_Higher_Education + RA_NAME_2016_m2,remove.redundant = "TRUE" , name="beta") +
  gen(formula=~1, factor = ~spatial(SA3_2016, poly.df=AUS_SA3_MAIN,derive.constraints=TRUE),name="spatial_SA3") +
  gen(formula=~1, factor = ~SA4_2016,  name="r_WN_SA4")+
  gen(formula=~1, factor = ~SA3_2016,  name="r_WN_SA3")  

predX <- computeDesignMatrix(model,data=dat)
model.V <- NULL
sampler <- create_sampler(model,formula.V =model.V, sigma.fixed=TRUE, Q0=1/dat$pred_sqrt_se[sub_dist_with_zero]^2, linpred=predX, data=dat[sub_dist_with_zero,], block = TRUE) # linpred="fitted"

dat$sqrt_se <- NA
dat$sqrt_se[sub_dist] <-dat$pred_sqrt_se[sub_dist]
out <- runmodel_smoking_SA3(model_id, parallel=TRUE, cl=cl, seed=1, burnin=1000, n.iter=10000, thin=5,predX=predX, bias.correction=TRUE, save.full.pred=FALSE)


