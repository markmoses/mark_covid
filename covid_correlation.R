library(data.table)
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)

source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
source("/ihme/cc_resources/libraries/current/r/get_ids.R")
source("/ihme/cc_resources/libraries/current/r/get_outputs.R")
source("/ihme/cc_resources/libraries/current/r/get_cause_metadata.R")
source('/ihme/cc_resources/libraries/current/r/get_covariate_estimates.R')
source('/ihme/cc_resources/libraries/current/r/get_population.R')

#

# covariates from erin
locs <- get_location_metadata(22)
pop <- get_population(location_id = locs[level==3,location_id],
                      age_group_id = 22,
                      sex_id=3,
                      year_id=2021,
                      decomp_step = "iterative")
# get ifr
ifr <- fread("/snfs1/temp/ehulland/COVID-19/VCCCM/ifr_agestd_by_location_period_58_rsoren_20210212.csv")
# get cum infection
df_all <- fread("/snfs1/temp/ehulland/COVID-19/VCCCM/df_outcome_bylocday_v2021_03_10.09.csv")
df_all[is.na(daily_cases_mean), daily_cases_mean:= 0]
df_all <- df_all[date< ymd("2021-03-08"), sum(daily_cases_mean), by =location_id]
df_all <- merge(df_all, pop,
                by = "location_id")
df_all[, cum_infection:= V1/population]

df_all <- merge(df_all[,.(location_id, cum_infection)],
                ifr[period==3,.(location_id,period_ifr_age_adjusted)])
covs <- fread( "/snfs1/temp/ehulland/COVID-19/VCCCM/final_df_covariates_scaled.csv")
covs[, period_ifr_age_adjusted:= NULL]

jee <- c("jee_readyscore_prevent", "jee_readyscore_detect", "jee_readyscore_respond", "jee_readyscore_other")
jee <- melt(covs[, c("location_id", jee), with=F],
     id.vars = "location_id")

jee <- jee[, psych::geometric.mean(value), by =location_id]
setnames(jee, "V1", "jee_geo_mean")
covs <- merge(covs, jee)



# get the
the_dir<-"/share/resource_tracking/forecasting/the/20201006_V17_subnats_bkp/summary_files/"
the<-fread(paste0(the_dir,'THEpc_scenario_stats.csv'))
the<-the[scenario==0 & year==2019,]
the<-merge(the, locs, by.x="iso3", by.y="ihme_loc_id")
the<-the[,.(location_id, mean)]
setnames(the, c('mean'), c('the_mean'))
the[, the_mean:=lapply(.SD, function(x) scale(log(x))), .SDcols="the_mean"]

gini<-fread("/ihme/scratch/users/rsoren/data/covid-crosscountry/df_political_and_social_correlates.csv")
gini[, gini:= lapply(.SD, function(x) scale(log(x))), .SDcols="gini"]
gini <- gini[,.(gini,location_id)]
gini <- gini[!is.na(location_id)]

diabetes <- get_covariate_estimates(covariate_id = 29,
                                    year_id = 2019, sex_id = 2,
                                    location_id = unique(df_all[,location_id]),
                                    decomp_step = "iterative")
setnames(diabetes, "mean_value", "diabetes")
diabetes[, diabetes:= lapply(.SD, function(x) scale(log(x))), .SDcols="diabetes"]


df_all <- merge(df_all,
                diabetes[,.(location_id, diabetes)],
                by ="location_id")


df_all <- merge(df_all,
                covs,
                by ="location_id", all.x=T)

df_all <- merge(df_all,
                the,
                by ="location_id", all.x=T)
df_all <- merge(df_all,
                gini,
                by ="location_id", all.x=T)




cols <- c("jee_readyscore_overall", "jee_readyscore_prevent", "jee_readyscore_detect",
          "jee_readyscore_respond", "jee_readyscore_other","jee_geo_mean","ghsi_0_overall","HAQI_mean",
         "uhc_19",  "BMI","smoke_mean", "GDP_mean", "the_mean", "gini", "diabetes")


label <- c("JEE Overall", "JEE Prevention", "JEE Detection",
           "JEE Response", "JEE Other", "JEE Geometric Mean",
           "GHSI","IHME Healthcare Access and Quality Index",
           "IHME UHC Index", "Average BMI", "Smoking Prevalence",
           "GDP per capita", "THE per capita", "Gini", "Diabetes Age-standardized Prevalence")

df_all <- df_all[cum_infection!=0]
df_all[, cum_infection_scale:= scale(log(cum_infection))]
df_all[, ifr_scale:= scale(log(period_ifr_age_adjusted))]

# 
# m3 <- rpart(
#   formula = ifr_scale ~ .,
#   data    = df_all[, c("ifr_scale", cols), with=F],
#   method  = "anova", 
#   control = list(minsplit = 10, maxdepth = 12, xval = 10)
# )






pdf("/homes/mwm6/random/covid_datav5.pdf", width = 12, height = 6)

for(i in 1:length(cols)){
  print(i)
  setnames(df_all, cols[i],"x_var")
  p1 <- ggplot(data=df_all, aes(y = cum_infection_scale,
                                x = x_var)) + geom_point()
  p1 <- p1 + geom_smooth(method='lm',se=F)
  mod <- lm(cum_infection_scale ~ x_var, data= df_all)
  est <- summary(mod)$coefficients[2,"Estimate"]
  p_val <- summary(mod)$coefficients[2,"Pr(>|t|)"]
  p_val <- round(p_val,3)
  if(p_val==0) p_val <- "p-val < 0.001"
  if(is.numeric(p_val)) p_val <- paste0("p-val = ", round(p_val, 3))
  p1 <- p1 + theme_classic()
  p1 <- p1 + ggtitle(paste0("Cumulative Infections per capita\nCoefficient = ", round(est,3), "; ", p_val))
  
  p1 <- p1 + xlab(label[i]) + ylab("Cumulative infections")
  p1 <- p1 + theme(axis.text = element_text(size=16),
              axis.title = element_text(size=18),
              plot.title = element_text(size=20))
  
  
  p2 <- ggplot(data=df_all, aes(y = ifr_scale,
                                x = x_var)) + geom_point()
  p2 <- p2 + geom_smooth(method='lm',se=F)
  mod <- lm(ifr_scale ~ x_var, data= df_all)
  est <- summary(mod)$coefficients[2,"Estimate"]
  p_val <- summary(mod)$coefficients[2,"Pr(>|t|)"]
  p_val <- round(p_val,3)
  if(p_val==0) p_val <- "p-val < 0.001"
  if(is.numeric(p_val)) p_val <- paste0("p-val = ", p_val)
  p2 <- p2 + theme_classic()
  p2 <- p2 + ggtitle(paste0("Age-standardized Infection Fatality rate\nCoefficient = ", round(est,3), "; ", p_val))
  
  p2 <- p2 + xlab(label[i]) + ylab("Age-standardized Infection Fatality rate")
  p2 <- p2 + theme(axis.text = element_text(size=16),
                   axis.title = element_text(size=18),
                   plot.title = element_text(size=20))
  
  pall <- gridExtra::grid.arrange(p1, p2,
                                nrow =1,
                               top = grid::textGrob(label[i],gp=grid::gpar(fontsize=24,font=1)))

  setnames(df_all, "x_var", cols[i])
}

dev.off()
