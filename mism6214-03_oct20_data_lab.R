### set working environment
# add packages to the list as needed
pkgs <- list("glmnet", "reticulate", "stringr", "rstudioapi", "data.table", "parallel", "minpack.lm", "doParallel",
             "foreach", "pROC", "gplots", "pwr", "dplyr", "caret", "sm", "ggplot2", "scales", "reshape2", "Hmisc",
             "bayesAB", "gridExtra", "plotly", "flux", "RColorBrewer", "plm", "xts", "pdp", "vip", "ranger", "vioplot",
             "randomForest", "haven")

# install packages in list
#lapply(pkgs, install.packages, character.only = T)

# load packages in list
lapply(pkgs, require, character.only = T)

# set wd
setwd("/Users/j.runge/Downloads")

##### Experimentation #####
### read in data
#data <- read_dta("android_xsection_treated_cleaned.dta")
data <- read.csv("android_xsection_treated_cleaned.csv", header = TRUE)

### store data as csv
#write.csv(data, file="android_xsection_treated_cleaned.csv")

### some data formatting
data$treatment <- as.factor(data$treatment)
data$treatment <- relevel(data$treatment, ref='after0days')

### simple plot of key outcomes by treatment
by_treatment <- data %>%
  group_by(treatment) %>%
  summarise(n = n(),
            # the bug was here: the calculated variable needs to be named differently from 
            # the main variable in the dataframe; i now added "_mean" behind the calculated vars
            conversion_mean = mean(conversion),
            conversion_se = sd(conversion)/sqrt(length(conversion)),
            purchases_mean = mean(purchases),
            purchases_se = sd(purchases)/sqrt(length(purchases)),
            revenue_mean = mean(revenue),
            revenue_se = sd(revenue)/sqrt(length(revenue)),
            sessions_mean = mean(sessions),
            sessions_se = sd(sessions)/sqrt(length(sessions))
  )

btm <- melt(by_treatment[,c('treatment','conversion_mean','purchases_mean','revenue_mean','sessions_mean')],id.vars = 1)
btm$variable <- substr(btm$variable,1,7)

btm_se <- melt(by_treatment[,c('treatment','conversion_se','purchases_se','revenue_se','sessions_se')],id.vars = 1)
btm_se$variable <- substr(btm_se$variable,1,7)

btm_all <- merge(x = btm, y = btm_se, by = c("treatment", "variable"), all.x = TRUE)

revenue_ci_plot <- ggplot(data=filter(btm_all,btm_all$variable=="revenue"), aes(x=treatment, y = value.x)) +
  geom_bar(stat = "identity", position = "dodge", colour="grey", fill="azure1") +
  geom_errorbar(aes(ymin = value.x - 1.96*value.y, ymax = value.x + 1.96*value.y), width = .2, colour="coral4", position = position_dodge(.9)) +
  xlab("Treatment") +
  ylab("Revenue")

conversion_ci_plot <- ggplot(data=filter(btm_all,btm_all$variable=="convers"), aes(x=treatment, y = value.x)) +
  geom_bar(stat = "identity", position = "dodge", colour="grey", fill="beige") +
  geom_errorbar(aes(ymin = value.x - 1.96*value.y, ymax = value.x + 1.96*value.y), width = .2, colour="coral4", position = position_dodge(.9)) +
  xlab("Treatment") +
  ylab("Conversion")

purchases_ci_plot <- ggplot(data=filter(btm_all,btm_all$variable=="purchas"), aes(x=treatment, y = value.x)) +
  geom_bar(stat = "identity", position = "dodge", colour="grey", fill="darkolivegreen1") +
  geom_errorbar(aes(ymin = value.x - 1.96*value.y, ymax = value.x + 1.96*value.y), width = .2, colour="coral4", position = position_dodge(.9)) +
  xlab("Treatment") +
  ylab("Purchases")

sessions_ci_plot <- ggplot(data=filter(btm_all,btm_all$variable=="session"), aes(x=treatment, y = value.x)) +
  geom_bar(stat = "identity", position = "dodge", colour="grey", fill="chocolate") +
  geom_errorbar(aes(ymin = value.x - 1.96*value.y, ymax = value.x + 1.96*value.y), width = .2, colour="coral4", position = position_dodge(.9)) +
  xlab("Treatment") +
  ylab("Sessions") +
  ylim(170,190)

### simple treatment effects via regression
conv_te_lm <- train(conversion ~ treatment
                      ,
                      data = data,
                      method="lm")
summary(conv_te_lm)

purch_te_lm <- train(purchases ~ treatment
                   ,
                   data = data,
                   method="lm")
summary(purch_te_lm)

rev_te_lm <- train(revenue ~ treatment
                   ,
                   data = data,
                   method="lm")
summary(rev_te_lm)

sesh_te_lm <- train(sessions ~ treatment
                   ,
                   data = data,
                   method="lm")
summary(sesh_te_lm)

### treatment effect heterogeneity
# other variables
rev_dpi <- train(revenue ~ dpi
                        ,
                        data = data,
                        method="lm")
summary(rev_dpi)

rev_ram <- train(revenue ~ device_ram
                        ,
                        data = data,
                        method="lm")
summary(rev_ram)

rev_high_value <- train(revenue ~ high_value
                        ,
                        data = data,
                        method="lm")
summary(rev_high_value)

rev_other_vars <- train(revenue ~ dpi + high_value + device_ram
                     ,
                     data = data,
                     method="lm")
summary(rev_other_vars)

# treatment effect heterogeneity for revenue in dpi
rev_teh_dpi <- train(revenue ~ treatment * dpi
                     ,
                     data = data,
                     method="lm")
summary(rev_teh_dpi)

# treatment effect heterogeneity for revenue in median split on ram
rev_teh_hv <- train(revenue ~ treatment * high_value
                     ,
                     data = data,
                     method="lm")
summary(rev_teh_hv)

# treatment effect heterogeneity for revenue in device ram
rev_teh_ram <- train(revenue ~ treatment * device_ram
                     ,
                     data = data,
                     method="lm")
summary(rev_teh_ram)
