### set working environment
# add packages to the list as needed
pkgs <- list("glmnet", "reticulate", "stringr", "rstudioapi", "data.table", "parallel", "minpack.lm", "doParallel",
             "foreach", "pROC", "gplots", "pwr", "dplyr", "caret", "sm", "ggplot2", "scales", "reshape2", "Hmisc",
             "bayesAB", "gridExtra", "plotly", "flux", "RColorBrewer", "plm", "xts", "pdp", "vip", "ranger", "vioplot",
             "randomForest")

# install packages in list
#lapply(pkgs, install.packages, character.only = T)

# load packages in list
lapply(pkgs, require, character.only = T)

# set wd
setwd("/Users/j.runge/Downloads")

##### CLV Prediction #####
### read in data
#write.csv(data, file="combined.csv")
data <- read.csv("d14_d30_data_v3.csv", header = TRUE)

### some data formatting
data$d30_spend <- as.numeric(data$d30_spend)
data$d14_spend <- as.numeric(data$d14_spend)
data$d7_spend <- as.numeric(data$d7_spend)
data$d3_spend <- as.numeric(data$d3_spend)
data$d1_spend <- as.numeric(data$d1_spend)
data$count_p_1 <- as.numeric(data$count_p_1)
data$max_p_2 <- as.numeric(data$max_p_2)
data$p_4 <- as.numeric(data$p_4)
data$p_5 <- as.numeric(data$p_5)
data$sum_p_6 <- as.numeric(data$sum_p_6)
data$count_p_7 <- as.numeric(data$count_p_7)
data$count_p_8 <- as.numeric(data$count_p_8)
data$count_p_9 <- as.numeric(data$count_p_9)
data$count_p_10 <- as.numeric(data$count_p_10)
data$count_p_11 <- as.numeric(data$count_p_11)
data$count_p_12 <- as.numeric(data$count_p_12)
data$count_p_13 <- as.numeric(data$count_p_13)
data$len_p_14 <- as.numeric(data$len_p_14)
data$count_p_15 <- as.numeric(data$count_p_15)
data$count_p_19 <- as.numeric(data$count_p_19)
data$count_p_20 <- as.numeric(data$count_p_20)

### correlation heatmap
# select variables used in analysis for heatmap
cor_vars <- c("d30_spend","d14_spend","d7_spend","d3_spend","d1_spend",
              "count_p_1","max_p_2","p_4","p_5","sum_p_6","count_p_7",
              "count_p_8","count_p_9","count_p_10","count_p_11","count_p_12",
              "count_p_13","len_p_14","count_p_15","count_p_19","count_p_20")

cor_data <- data[cor_vars]

# create correlation matrix
# fill all missings with 0 to allow for correlation calculation
cor_data[is.na(cor_data) == TRUE] <- 0
cormat <- round(cor(cor_data),2)

# helper function to get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)

melted_cormat <- melt(upper_tri, na.rm = TRUE)

# label levels of factors to make heatmap nice to read
#levels(melted_cormat$Var1)[levels(melted_cormat$Var1)=="years_since_first_spend"] <- "Years active on platform"
#levels(melted_cormat$Var2)[levels(melted_cormat$Var2)=="years_since_first_spend"] <- "Years active on platform"

cor_heat <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()


### fit random forest
# select x vars
x1_vars <- c("d14_spend","d7_spend","d3_spend","d1_spend",
             "count_p_1","max_p_2","p_4","p_5","sum_p_6","count_p_7",
             "count_p_8","count_p_9","count_p_10","count_p_11","count_p_12",
             "count_p_13","len_p_14","count_p_15","count_p_19","count_p_20")
x2_vars <- c("count_p_1","max_p_2","p_4","p_5","sum_p_6","count_p_7",
             "count_p_8","count_p_9","count_p_10","count_p_11","count_p_12",
             "count_p_13","len_p_14","count_p_15","count_p_19","count_p_20")

# replace missings with 0
data[is.na(data) == TRUE] <- 0

# set seed and specify cross-validation
set.seed(825)
cv_fold <- trainControl(method = "cv",
                        number = 5)

# set number of variables to randomly sample at each split
mtry <- sqrt(ncol(data))

# train random forests
rf_x1 <- train(as.formula(paste("d30_spend", paste(x1_vars, collapse = " + "), sep = " ~ ")), 
               data = data,
               method = "ranger",
               #method = "rpart",
               trControl = cv_fold,
               metric = "RMSE",
               importance = "impurity",
               tuneLength=50)
rf_x2 <- train(as.formula(paste("d30_spend", paste(x2_vars, collapse = " + "), sep = " ~ ")), 
               data = data, 
               method = "ranger",
               #method = "rpart",
               trControl = cv_fold,
               metric = "RMSE",
               importance = "impurity",
               tuneLength=50)

print(rf_x1)
print(rf_x2)
plot(rf_x1)
plot(rf_x2)
rf_x1$results
rf_x2$results

# for decision tree
#print(varImp((rf_x1)))
#plot(varImp((rf_x1)))

### look at variable importance
rf_x1_var_imp <- vip(rf_x1$finalModel,
                     #bar = FALSE,
                     size = 1.5,
                     num_features = 15) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

rf_x2_var_imp <- vip(rf_x2$finalModel,
                     #bar = FALSE,
                     size = 1.5,
                     num_features = 15) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

vip_compare <- grid.arrange(rf_x1_var_imp,
                            rf_x2_var_imp,
                            nrow = 1,
                            ncol = 2)

### generate partial dependency plots with main x variable
rf_x1_par <- partial(rf_x1,
                     pred.var = c("d14_spend"),
                     chull = TRUE)
rf_x1_plot <- ggplot(rf_x1_par, aes(x=d14_spend, y=yhat)) + 
  geom_line() +
  geom_smooth(method="auto") +
  #xlim(0,.5) +
  #ylim(0,10) +
  xlab("D14 Spend") +
  ylab("D30 Spend") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

rf_x2_par <- partial(rf_x2,
                     pred.var = c("count_p_15"),
                     chull = TRUE)
rf_x2_plot <- ggplot(rf_x2_par, aes(x=count_p_15, y=yhat)) + 
  geom_line() +
  geom_smooth(method="auto") +
  #xlim(0,.5) +
  #ylim(0,10) +
  xlab("Count P15") +
  ylab("D30 Spend") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

### generate partial dependency plots with two main x variables
rf_x1_par2 <- partial(rf_x1,
                      pred.var = c("d14_spend","d7_spend"),
                      chull = TRUE)
rf_x1_plot2 <- ggplot(rf_x1_par2, aes(x=d14_spend, y=d7_spend, z=yhat)) +
  geom_tile(aes(fill=yhat)) +
  scale_fill_gradientn(colours=rev(brewer.pal(9,"YlGnBu")), name = "D30 Spend", labels = NULL) +
  stat_contour(bins=10,aes(x=Number_experiments, y=Experimentation_adoption, z=yhat), color="black", size=0.2) +
  #xlim(0,.5) +
  #ylim(0,10) +
  xlab("D14 Spend") +
  ylab("D7 Spend") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

rf_x2_par2 <- partial(rf_x2,
                      pred.var = c("count_p_15","count_p_9"),
                      chull = TRUE)
rf_x2_plot2 <- ggplot(rf_x2_par2, aes(x=count_p_15, y=count_p_9, z=yhat)) +
  geom_tile(aes(fill=yhat)) +
  scale_fill_gradientn(colours=rev(brewer.pal(9,"YlGnBu")), name = "D30 Spend", labels = NULL) +
  stat_contour(bins=10,aes(x=Number_experiments, y=Experimentation_adoption, z=yhat), color="black", size=0.2) +
  #xlim(0,.5) +
  #ylim(0,10) +
  xlab("Count P15") +
  ylab("Count P9") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


##### MMM #####
### read in data
#write.csv(data, file="combined.csv")
data_mmm <- read.csv("data_mmm.csv", header = TRUE)

### some data formatting
data_mmm$date <- as.Date(data_mmm$date, format='%Y-%m-%d')
data_mmm$year <- substr(data_mmm$date, 1, 4)
data_mmm$year <- as.factor(data_mmm$year)

# add key variables
data_mmm$total_spend <- data_mmm$facebook_newsfeed_spend
                        + data_mmm$youtube_brand_spend
                        + data_mmm$search_spend
                        + data_mmm$youtube_performance_spend
                        + data_mmm$newspaper_spend
                        + data_mmm$tv_spend
# add logarithm for key vars
data_mmm$log_sales <- log(data_mmm$sales + 1)
data_mmm$log_total_spend <- log(data_mmm$total_spend + 1)
data_mmm$log_facebook_newsfeed_spend <- log(data_mmm$facebook_newsfeed_spend + 1)
data_mmm$log_youtube_brand_spend <- log(data_mmm$youtube_brand_spend + 1)
data_mmm$log_search_spend <- log(data_mmm$search_spend + 1)
data_mmm$log_youtube_performance_spend <- log(data_mmm$youtube_performance_spend + 1)
data_mmm$log_newspaper_spend <- log(data_mmm$newspaper_spend + 1)
data_mmm$log_tv_spend <- log(data_mmm$tv_spend + 1)


### some plotting
# spend and sales associations
sales_spend <- ggplot(data_mmm, aes(x=total_spend, y=sales, color=year)) + 
  geom_point()+
  geom_smooth() +
  #geom_text(x = 7.8, y = 9.5, label = lm_eqn(df), parse = TRUE) +
  xlab("Total ad spend") +
  ylab("Total sales") +
  #ylim(0,100000) +
  labs(title = "Sales & Spend by Year") 

log_sales_spend <- ggplot(data_mmm, aes(x=log_total_spend, y=log_sales, color=year)) + 
  geom_point()+
  geom_smooth() +
  #geom_text(x = 7.8, y = 9.5, label = lm_eqn(df), parse = TRUE) +
  xlab("Total ad spend (log)") +
  ylab("Total sales (log)") +
  #ylim(0,100000) +
  labs(title = "Sales & Spend by Year (log)") 

# daily spend over time
daily_spend_over_time <- ggplot(data_mmm, aes(x=date, y=c(log_facebook_newsfeed_spend,
                                                          log_youtube_brand_spend,
                                                          log_search_spend,
                                                          log_youtube_performance_spend,
                                                          log_newspaper_spend,
                                                          log_tv_spend))) + 
  #geom_point()+
  #geom_smooth() +
  #geom_text(x = 7.8, y = 9.5, label = lm_eqn(df), parse = TRUE) +
  xlab("Date") +
  ylab("Ad Spend") +
  labs(title = "Spend over time for key channels")

daily_spend_over_time <- ggplot(data_mmm, aes(x=date, y=log_facebook_newsfeed_spend)) + 
  geom_point()+
  geom_smooth() +
  #geom_text(x = 7.8, y = 9.5, label = lm_eqn(df), parse = TRUE) +
  xlab("Date") +
  ylab("Ad Spend") +
  labs(title = "Spend over time for key channels")

### play with some simple regression models
lm1 <- train(sales ~ total_spend
             ,
             data = data_mmm,
             #trControl=cross_train,
             method="lm")
summary(lm1)

lm2 <- train(sales ~ facebook_newsfeed_spend
             + youtube_brand_spend
             + search_spend
             + youtube_performance_spend
             + newspaper_spend
             + tv_spend
             ,
             data = data_mmm,
             #trControl=cross_train,
             method="lm")
summary(lm2)


lm1_log <- train(log_sales ~ log_total_spend
                    ,
                    data = data_mmm,
                    #trControl=cross_train,
                    method="lm")
summary(lm1_log)

lm2_log <- train(log_sales ~ log_facebook_newsfeed_spend
             + log_youtube_brand_spend
             + log_search_spend
             + log_youtube_performance_spend
             + log_newspaper_spend
             + log_tv_spend
             ,
             data = data_mmm,
             #trControl=cross_train,
             method="lm")
summary(lm2_log)


lm1_log_2022 <- train(log_sales ~ log_total_spend
                 ,
                 data = filter(data_mmm,data_mmm$year==2022),
                 #trControl=cross_train,
                 method="lm")
summary(lm1_log_2022)

lm2_log_2022 <- train(log_sales ~ log_facebook_newsfeed_spend
                 + log_youtube_brand_spend
                 + log_search_spend
                 + log_youtube_performance_spend
                 + log_newspaper_spend
                 + log_tv_spend
                 ,
                 data = filter(data_mmm,data_mmm$year==2022),
                 #trControl=cross_train,
                 method="lm")
summary(lm2_log_2022)

### look at lagged spend-sales correlations
lagged_corr_alltime_1_40 <- ccf(spend_ts <- ts(data_mmm$total_spend), sales_ts <- ts(data_mmm$sales), lag=40,
                                main = "Lagged correlations between past spend and current sales (All-time)",
                                xlim = c(-40,-1))

lagged_corr_alltime_11_16 <- ccf(spend_ts <- ts(data_mmm$total_spend), sales_ts <- ts(data_mmm$sales), lag=16,
                                 main = "Lagged correlations between past spend and current sales (All-time)",
                                 xlim = c(-16,-11))

log_lagged_corr_alltime_1_40 <- ccf(ts(data_mmm$log_total_spend), ts(data_mmm$log_sales), lag=40,
                                    main = "Lagged correlations between past spend and current sales (All-time)",
                                    xlim = c(-40,-1))

lagged_corr_alltime_11_16 <- ccf(ts(data_mmm$log_total_spend), ts(data_mmm$log_sales), lag=16,
                                 main = "Lagged correlations between past spend and current sales (All-time)",
                                 xlim = c(-16,-11))

