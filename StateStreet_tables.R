# ThiS R file is only a part of the whole codings. It only includes the main part of the tables replication.

### Table 2 #####
library(dplyr)
library(mefa4)
#library(raster)
library(mltools)
library(lme4)
library(pkr)

setwd("~/Desktop/StateStreetNew")
score <- read.csv('section3/data_table3.csv') #import quality score data
csho <- read.csv('section1/1_rawDatasets/SP500_Safety1.csv') # import common shares outstanding

#sort stocks by quality scores
csho <- csho %>% dplyr::select(GVKEY, fyear, csho)
table2 <- merge(score, csho, by=c('GVKEY', 'fyear'))
table2$me <- table2$prcc_f * table2$csho
table2 <- table2 %>% group_by(fyear) %>% mutate(rank=rank(z_quality)) %>% arrange(fyear,z_quality)

# create portfolio function
create_portfolio <- function(dataset, by='me', num_subsets=10, portfolio_initio='P') {
  dataset$portfolio <- NULL # new "portfolio column to record portfolio
  count_entry <- 1 # row index for the n-th entry
  dataset <- dataset %>% group_by(fyear) %>% mutate(rank=rank(z_quality)) %>% arrange(fyear,z_quality) %>% mutate(cum_me=cumsum(!!as.name(by)))
  uniqueYear <- as.vector(unique(dataset$fyear))
  head(dataset)
  for (year in uniqueYear) {
    count_subsetEntry <- 1 # row index for the n-th entry in subset
    count_portfolio <- 1 # initalize portfolio index: 1 means P1
    sum_portfolio <- 0 # initalize cumulative market value 
    subset <- dataset %>% filter(fyear == year)
    subset_length <- length(subset$GVKEY)
    mkt_cap <- sum(subset$me)
    portfolio_cap <- 1/num_subsets*mkt_cap
    for (i in c(1:subset_length)) {
      sum_portfolio <- sum_portfolio + subset[i, by]
      if (sum_portfolio > count_portfolio*portfolio_cap) {
        count_portfolio <- count_portfolio + 1 # if cumulative market value > portfolio boundary, then move on to the next portfolio
      }
      dataset[count_entry, 'portfolio'] <- paste0(portfolio_initio, count_portfolio)
      count_entry <- count_entry + 1
      count_subsetEntry <- count_subsetEntry + 1
    }
  }
  dataset <- as.data.frame(dataset)
  dataset
}

# keep useful columns 
table2 <- create_portfolio(dataset=table2, by='me', num_subsets=10, portfolio_initio='P')
table2 <- table2 %>% select(GVKEY, fyear, z_quality, z_profit, z_growth, z_safety, z_payout, me, portfolio)

# calculate weights
table2 <- table2 %>% group_by(fyear, portfolio) %>% mutate(me_portfolio=sum(me)) 
attach(table2)
table2$weight <- me/me_portfolio
table2$weighted_quality <- weight*z_quality
table2$weighted_profit <- weight*z_profit
table2$weighted_growth <- weight*z_growth
table2$weighted_safety <- weight*z_safety
table2$weighted_payout <- weight*z_payout

table2_summarize <- table2 %>% group_by(fyear, portfolio) %>% 
  summarise(port_quality=sum(weighted_quality), port_profit=sum(weighted_profit), port_growth=sum(weighted_growth),
            port_safety=sum(weighted_safety), port_payout=sum(weighted_payout))

# form table 2
uniqueYear <- sort(as.vector(unique(table2_summarize$fyear)))

quality0 <- table2_summarize %>% filter(fyear==uniqueYear[1]) %>% select(portfolio, port_quality)
quality1 <- table2_summarize %>% filter(fyear==uniqueYear[1+1]) %>% select(portfolio, port_quality)
quality3 <- table2_summarize %>% filter(fyear==uniqueYear[1+3]) %>% select(portfolio, port_quality)
quality5 <- table2_summarize %>% filter(fyear==uniqueYear[1+5]) %>% select(portfolio, port_quality)
factor10 <- table2_summarize %>% filter(fyear==uniqueYear[1+10]) %>% 
  select(portfolio, port_quality, port_profit, port_growth, port_safety, port_payout)

# final modifications
table2_output <- cbind(quality0, quality1, quality3, quality5, factor10)
table2_output <- table2_output %>% select(portfolio, port_quality, port_quality1, port_quality2, port_quality3, port_quality4,
                                                port_profit, port_growth, port_safety, port_payout)
table2_output <- t(table2_output)[-c(1,2),]
colnames(table2_output) <- c('P1', 'P10', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7', 'P8', 'P9')
rownames(table2_output) <- c('Quality(t)', 'Quality(t+1Y)', 'Quality(t+3Y)', 'Quality(t+5Y)','Quality(t+10Y)',
                             'Profit(t+10Y)', 'Growth(t+10Y)', 'Safety(t+10Y)', 'Payout(t+10Y)')
table2_output <- as.data.frame(table2_output) # convert to dataframe so as to perform 'select' reorder in the next line
table2_output <- table2_output %>% select(P1,P2, P3, P4, P5, P6, P7, P8, P9,P10)
table2_output$P1 <- as.numeric(levels(table2_output$P1))[table2_output$P1] # convert factor to numerics
str(table2_output$P1)
table2_output$P10 <- as.numeric(levels(table2_output$P10))[table2_output$P10]
table2_output <- table2_output %>% mutate(diff=P10-P1)
colnames(table2_output) <- c('P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7', 'P8', 'P9', 'P10', 'P10-P1')
write.csv(table2_output,'data_table2.csv')


### Table 3 #####
# Zhenyu Chen, zhenyuchen@brandeis.edu
# Apr 16, 2020

library(dplyr)
library(mefa4)

setwd("~/Desktop/StateStreetNew/section3")
data <- read.csv('data_table3.csv')

# calculate return from last year
attach(data)
data$retLag <- lag(prcc_f/lag(prcc_f))
delete_year <- unique(data$fyear)[1:2]
data <- data %>% filter(fyear %notin% delete_year)

# regression 3.1 (table 3, panel A, US sample)
reg3.1.1 <- lm(z_mb ~ z_quality)
summary(reg3.1.1)

reg3.1.2<- lm(z_mb ~ z_quality + z_me + retLag)
summary(reg3.1.2)


# regression 3.2 (table 3, panel B, US sample)
reg3.2.1 <- lm(z_mb ~ z_profit)
summary(reg3.2.1)

reg3.2.2 <- lm(z_mb ~ z_growth)
summary(reg3.2.2)

reg3.2.3 <- lm(z_mb ~ z_safety)
summary(reg3.2.3)

reg3.2.4 <- lm(z_mb ~ z_payout)
summary(reg3.2.4)

reg3.2.5 <- lm(z_mb ~ z_profit + z_growth + z_safety + z_payout + z_me + retLag)
summary(reg3.2.5)


### Table 4 ###
library(dplyr)

# load data
setwd('~/Desktop/StateStreetNew/section4')
data <- read.csv('2_data_table4.csv')

# portfolio names, P1 to P10, H-L 
uniquePort <- c('P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7', 'P8', 'P9', 'P10', 'H-L')

table <- as.data.frame(matrix(NA, nrow=12, ncol=11))
colnames(table) <- uniquePort

count <- 1
for (p in uniquePort) {
  subdata <- data %>% filter(portfolio==p)
  
  # 1. excess return
  table[1,count] <- mean(subdata$excess_ret)
  
  # 2. excess return significance
  
  # 3. CAPM alpha
  lm_capm <- lm(data=subdata, excess_ret ~ mkt)
  table[3,count] <- coef(lm_capm)[1]
  
  # 4. CAPM alpha significance
  table[4,count] <- summary(lm_capm)$coefficients[1,4]
  
  # 5. 3-factor alpha
  lm_3factor <- lm(data=subdata, excess_ret ~ mkt+smb+hml)
  table[5,count] <- coef(lm_3factor)[1]
  
  # 6. 3-factor alpha significance
  table[6,count] <- summary(lm_3factor)$coefficients[1,4]
  
  # 7. 4-factor alpha
  lm_4factor <- lm(data=subdata, excess_ret ~ mkt+smb+hml+umd)
  table[7,count] <- coef(lm_4factor)[1]
  
  # 8. 4-factor alpha significance
  table[8,count] <- summary(lm_4factor)$coefficients[1,4]
  
  # 9. beta
  table[9,count] <- coef(lm_capm)[2]
  
  # 10. sharpe ratio
  table[10,count] <- mean(subdata$excess_ret)/sd(subdata$excess_ret)
  
  # 11. information ratio
  table[11,count] <- coef(lm_4factor)[1]/sd(lm_4factor$residuals)
  
  # 12. adjusted r2
  table[12,count] <-summary(lm_4factor)$adj.r.squared 
  
  count <- count + 1
}

item <- c('Excess return', 'Excess return p-value', 'CAPM alpha', 'CAPM alpha p-value',
          '3-factor alpha', '3-factor alpha p-value', '4-factor alpha', '4-factor alpha p-value',
          'Beta', 'Sharpe ratio', 'Information ratio', 'Adjusted R2')
item <- as.data.frame(item)
table <- cbind(item, table)

write.csv(table, '3_table4.csv', row.names=FALSE)



### Table 5 #####
# Zhenyu Chen, zhenyuchen@brandeis.edu
# Apr 26, 2020

library(dplyr)
library(tidyr)
library(mltools)

## load data
setwd("~/Desktop/StateStreetNew")
data <- read.csv('section3/data_table3.csv')
mkt <- read.csv('section4/1_mkt_excessret.csv')
sh <- read.csv('section4/1_smb_hml.csv')
umd <- read.csv('section4/1_umd.csv')
csho <- read.csv('section1/1_rawDatasets/SP500_Safety1.csv')
rf <- read.csv('section1/1_rawDatasets/SP500_Safety3.csv')

## data preparation
# mkt: retrive MKT factor from MKT
mkt <- mkt %>% dplyr::select(fyear, MKT) 
mkt <- mkt[!duplicated(mkt$fyear), ]

# normal return: calculate normal return and save to ret
data_table5 <- data %>% arrange(GVKEY, fyear)
data_table5$ret <- data_table5$prcc_f/lag(data_table5$prcc_f) - 1
data_table5 <- data_table5 %>% filter(fyear != sort(unique(data_table5$fyear))[1]) # remove data for 1st year (2000)

# risk free: change the date format and extract year
rf$date <- as.Date(as.character(rf$caldt),"%Y%m%d")
rf$year <- as.numeric(format(rf$date, "%Y"))

# risk free: get annual average risk free returns
rf <- rf %>% group_by(year) %>% mutate(avg_rf=mean(b10ret*12)) #annualize
rf <- rf %>% distinct(year, .keep_all = TRUE)

# risk free: merge return data with rf data
rf <- rf %>% dplyr::select(year, avg_rf)
data_table5 <- merge(data_table5, rf, by.x=c('fyear'), by.y=c('year'), name=NULL) # here z_mb becomes 0, but this variable is not useful in this code

# excess return: calculate excess returns
data_table5$ret.excess <- data_table5$ret - data_table5$avg_rf

# me: size of a stock = price * common shares outstanding
csho <- csho %>% dplyr::select(fyear, GVKEY, csho)
data_table5 <- merge(data_table5, csho, by=c('fyear', 'GVKEY'))
data_table5$me <- data_table5$prcc_f*data_table5$csho
data_table5 <- data_table5 %>% group_by(fyear) %>% mutate(me.median=median(me))

## calculate abnormal return
# merge data_table5 with 4 factors to calculate abnormal return
data_table5 <- merge(data_table5, mkt, by='fyear')
data_table5 <- merge(data_table5, sh, by='fyear')
data_table5 <- merge(data_table5, umd, by='fyear')

# extra coding challenge: many companies do not have sufficient information for certain years
# solution: calculate abnormal return by return - betas*factors 
# first, get betas
uniqueCompany <- as.vector(unique(data_table5$GVKEY))
betaMatrix <-  matrix(NA, nrow=length(uniqueCompany), ncol=6)
count = 1
for (c in uniqueCompany) {
  subdata <- data_table5 %>% filter(GVKEY == c) %>% arrange(fyear)
  # 4-factor regression
  lm <- lm(data=subdata, ret.excess ~ MKT+smb+hml+umd)
  # save results
  betaMatrix[count, 1] <- c
  betaMatrix[count, 2] <- lm$coefficients[1]
  betaMatrix[count, 3] <- lm$coefficients[2]
  betaMatrix[count, 4] <- lm$coefficients[3]
  betaMatrix[count, 5] <- lm$coefficients[4]
  betaMatrix[count, 6] <- lm$coefficients[5]
  count <- count + 1
}
colnames(betaMatrix) <- c('GVKEY', 'alpha', 'beta_mkt', 'beta_smb', 'beta_hml', 'beta_umd')

# merge them to data_table5
data_table5 <- merge(data_table5, betaMatrix, by='GVKEY')

# finally, calculate abnormal return
data_table5$ret.abnormal <- data_table5$ret.excess - (data_table5$beta_mkt*data_table5$MKT +  data_table5$beta_smb*data_table5$smb + 
                                                        data_table5$beta_hml*data_table5$hml + data_table5$beta_umd*data_table5$umd)

data_table5 <- data_table5 %>% 
  group_by(fyear) %>%
  mutate(size_rank=rank(me), junk_threshold=max(size_rank)*0.2, quality_threshold=max(size_rank)*0.8) %>%
  drop_na() %>%
  arrange(fyear, me)


## all data needed for table 5 are now all ready

## creating table 5
# function 1 (step 1): sort_portfolio
# for each year, categorize stocks into 6 portfolios, by assigned column names (quality/profit/growth, etc) 
sort_portfolio <- function (by='z_quality') {
  # first sort by size and then desired variable (e.g. z_quality)
  stocks_small <- data_table5 %>% group_by(fyear) %>% filter(size_rank <= junk_threshold) %>% mutate(rank=rank(!!as.name(by))) #use the input name as the sorting criteria
  stocks_big <- data_table5 %>% group_by(fyear) %>% filter(size_rank >= quality_threshold) %>% mutate(rank=rank(!!as.name(by))) 
  #then, for each size, sort 
  stocks_small[, "portfolio"] <- bin_data(stocks_small$rank, bins=3, binType = "explicit")
  stocks_big[, "portfolio"] <- bin_data(stocks_big$rank, bins=3, binType = "explicit")
  #set each portfolio into factor
  stocks_small$portfolio<-as.factor(stocks_small$portfolio)
  stocks_big$portfolio<-as.factor(stocks_big$portfolio)
  #then rename
  levels(stocks_small$portfolio)<-c("S1","S2","S3")
  levels(stocks_big$portfolio)<-c("B1","B2","B3")
  sorted_data <- rbind(stocks_small, stocks_big)
  # keep variables for table 5
  sorted_data  <- sorted_data  %>% 
    dplyr::select(fyear, GVKEY, ret.excess, ret.abnormal, portfolio) %>%
    arrange(fyear, portfolio)
  # function return
  sorted_data
}

# function 2 (step 2): portfolio return
# calculate returns and abnormal returns for each portfolio and for each year
portfolio_return <- function (sorted_data) {
  uniqueYear <- as.vector(unique(sorted_data$fyear))
  yearlyDataMatrix <- matrix(NA, nrow=length(uniqueYear), ncol=3)
  count = 1
  for (y in uniqueYear) {
    subdata <- sorted_data %>% filter(fyear == y) %>% arrange(portfolio)
    # calculate qmj based on returns
    excess.ret <- 0.5*mean(subdata[subdata$portfolio=='B3',]$ret.excess) + 0.5*mean(subdata[subdata$portfolio=='S3',]$ret.excess) -
      0.5*mean(subdata[subdata$portfolio=='B1',]$ret.excess) - 0.5*mean(subdata[subdata$portfolio=='S1',]$ret.excess)
    abnormal.ret <- 0.5*mean(subdata[subdata$portfolio=='B3',]$ret.abnormal) + 0.5*mean(subdata[subdata$portfolio=='S3',]$ret.abnormal) -
      0.5*mean(subdata[subdata$portfolio=='B1',]$ret.abnormal) - 0.5*mean(subdata[subdata$portfolio=='S1',]$ret.abnormal)
    yearlyDataMatrix[count, 1] <- y
    yearlyDataMatrix[count, 2] <- excess.ret
    yearlyDataMatrix[count, 3] <- abnormal.ret
    count <- count + 1
  }
  # rename matrix columns
  colnames(yearlyDataMatrix) <- c('fyear', 'excess_return', 'abnormal_return')
  # function return
  yearlyDataMatrix
}

# sort by quality, profitability, safety, growth and payout
# start with quality score (qmj)
portfolio_sorted_data <- sort_portfolio(by = 'z_quality')
return_table <- portfolio_return(sorted_data = portfolio_sorted_data)
colnames(return_table) <- c('fyear', 'return_z_quality', 'abnormal_return_z_quality')
# and then sort portfolio by profitability, safety, growth and payout, and merge them into return_table
sort_by <- c('z_profit', 'z_growth', 'z_safety', 'z_payout')
for (i in sort_by) {
  portfolio_sorted_data <- sort_portfolio(by = i)
  return_table_new <- portfolio_return(sorted_data = portfolio_sorted_data)
  colnames(return_table_new) <- c('fyear', paste0('excess_return_', i), paste0('abnormal_return_', i))
  return_table <- merge(return_table, return_table_new, by='fyear')
}


# correlation matrix
table5_ret <- cor(return_table[,c(2,4,8,6,10)])
colnames(table5_ret) <- c('QMJ', 'Profitability', 'Safety', 'Growth', 'Payout')
rownames(table5_ret) <- c('QMJ', 'Profitability', 'Safety', 'Growth', 'Payout')
print(table5_ret)

table5_abnormalRet <- cor(return_table[,c(3,5,9,7,11)])
colnames(table5_abnormalRet) <- c('QMJ', 'Profitability', 'Safety', 'Growth', 'Payout')
rownames(table5_abnormalRet) <- c('QMJ', 'Profitability', 'Safety', 'Growth', 'Payout')
print(table5_abnormalRet)

# save results
write.csv(data_table5, 'section5/data_table5_raw.csv', row.names=FALSE) # save data_table5 in case of further use
write.csv(return_table, 'section5/data_table5_cleaned.csv', row.names=FALSE) # cleaned data for calculating table 5
write.csv(table5_ret, 'section5/table5_return_matrix.csv', row.names=TRUE) # table 5, upper pannel a
write.csv(table5_abnormalRet, 'section5/table5_abnormalReturn_matrix.csv', row.names=TRUE) # table 5, lower pannel a



### Table 6 #####
#Keyu Jin, keyujin@brandeis.edu

library(dplyr)

# load data
setwd("/Users/Ella/Documents/研究�?/Field Project/Bridge/SP500")
data <- read.csv('2_data_table4.csv')
qmj_raw<-read.csv('data_table5_cleaned.csv')

#merge qmj factor with other factors
data2 <- merge(data, qmj_raw, by.x=c('fyear'), by.y=c('fyear'), name=NULL)

#select only the factors for each year
data2<- data2 %>% select(mkt,smb,hml,umd,avg_rf,return_z_quality,excess_return_z_profit,
                         excess_return_z_growth,excess_return_z_payout,excess_return_z_safety) %>% distinct(smb,.keep_all = TRUE)
names(data2)[names(data2) == c("return_z_quality",'excess_return_z_profit','excess_return_z_growth',
                               'excess_return_z_payout','excess_return_z_safety')] <- c("QMJ",'Profitability','Growth','Payout','Safety')

#run regressions and create table 6
### QMJ
table_qmj <- as.data.frame(matrix(NA, nrow=11, ncol=1))
colnames(table_qmj) <- c('QMJ')
rownames(table_qmj) <-c('Excess Returns','CAPM-alpha','3-factor-alpha','4-factor-alpha','MKT','SMB','HML','UMD','Sharpe Ratio','Information Ratio','Adjusted R2')
#excess return
table_qmj[1,1]<-mean(data2$QMJ) #should we substract rf or not?

#run regression 
capm_qmj<-lm(data=data2,QMJ~mkt)
f3_qmj<-lm(data=data2,QMJ~mkt+smb+hml)
f4_qmj<-lm(data=data2,QMJ~mkt+smb+hml+umd)

#put them into the table
table_qmj[2,1]<-coef(capm_qmj)[1] #capm-alpha
table_qmj[3,1]<-coef(f3_qmj)[1] #3factor-alpha
#4factor coefficients
for (c in (4:8)) {
  table_qmj[c,1] <- coef(f4_qmj)[c-3] 
} 

#sharpe ratio
table_qmj[9,1] <-  mean(data2$QMJ)/sd(data2$QMJ)

#information ratio
table_qmj[10,1] <- coef(f4_qmj)[1]/sd(f4_qmj$residuals) 

#Adjusted R2
table_qmj[11,1] <- summary(f4_qmj)$adj.r.squared 


### Profitability
table_Profitability <- as.data.frame(matrix(NA, nrow=11, ncol=1))
colnames(table_Profitability) <- c('Profitability')
rownames(table_Profitability) <-c('Excess Returns','CAPM-alpha','3-factor-alpha','4-factor-alpha','MKT','SMB','HML','UMD','Sharpe Ratio','Information Ratio','Adjusted R2')
#excess return
table_Profitability[1,1]<-mean(data2$Profitability) #should we substract rf or not?

#run regression 
capm_Profitability<-lm(data=data2,Profitability~mkt)
f3_Profitability<-lm(data=data2,Profitability~mkt+smb+hml)
f4_Profitability<-lm(data=data2,Profitability~mkt+smb+hml+umd)

#put them into the table
table_Profitability[2,1]<-coef(capm_Profitability)[1] #capm-alpha
table_Profitability[3,1]<-coef(f3_Profitability)[1] #3factor-alpha
#4factor coefficients
for (c in (4:8)) {
  table_Profitability[c,1] <- coef(f4_Profitability)[c-3] 
} 

#sharpe ratio
table_Profitability[9,1] <-  mean(data2$Profitability)/sd(data2$Profitability)

#information ratio
table_Profitability[10,1] <- coef(f4_Profitability)[1]/sd(f4_Profitability$residuals) 

#Adjusted R2
table_Profitability[11,1] <- summary(f4_Profitability)$adj.r.squared 


### Safety
table_Safety <- as.data.frame(matrix(NA, nrow=11, ncol=1))
colnames(table_Safety) <- c('Safety')
rownames(table_Safety) <-c('Excess Returns','CAPM-alpha','3-factor-alpha','4-factor-alpha','MKT','SMB','HML','UMD','Sharpe Ratio','Information Ratio','Adjusted R2')
#excess return
table_Safety[1,1]<-mean(data2$Safety) #should we substract rf or not?

#run regression 
capm_Safety<-lm(data=data2,Safety~mkt)
f3_Safety<-lm(data=data2,Safety~mkt+smb+hml)
f4_Safety<-lm(data=data2,Safety~mkt+smb+hml+umd)

#put them into the table
table_Safety[2,1]<-coef(capm_Safety)[1] #capm-alpha
table_Safety[3,1]<-coef(f3_Safety)[1] #3factor-alpha
#4factor coefficients
for (c in (4:8)) {
  table_Safety[c,1] <- coef(f4_Safety)[c-3] 
} 

#sharpe ratio
table_Safety[9,1] <-  mean(data2$Safety)/sd(data2$Safety)

#information ratio
table_Safety[10,1] <- coef(f4_Safety)[1]/sd(f4_Safety$residuals) 

#Adjusted R2
table_Safety[11,1] <- summary(f4_Safety)$adj.r.squared 

### Growth
table_Growth <- as.data.frame(matrix(NA, nrow=11, ncol=1))
colnames(table_Growth) <- c('Growth')
rownames(table_Growth) <-c('Excess Returns','CAPM-alpha','3-factor-alpha','4-factor-alpha','MKT','SMB','HML','UMD','Sharpe Ratio','Information Ratio','Adjusted R2')
#excess return
table_Growth[1,1]<-mean(data2$Growth) #should we substract rf or not?

#run regression 
capm_Growth<-lm(data=data2,Growth~mkt)
f3_Growth<-lm(data=data2,Growth~mkt+smb+hml)
f4_Growth<-lm(data=data2,Growth~mkt+smb+hml+umd)

#put them into the table
table_Growth[2,1]<-coef(capm_Growth)[1] #capm-alpha
table_Growth[3,1]<-coef(f3_Growth)[1] #3factor-alpha
#4factor coefficients
for (c in (4:8)) {
  table_Growth[c,1] <- coef(f4_Growth)[c-3] 
} 

#sharpe ratio
table_Growth[9,1] <-  mean(data2$Growth)/sd(data2$Growth)

#information ratio
table_Growth[10,1] <- coef(f4_Growth)[1]/sd(f4_Growth$residuals) 

#Adjusted R2
table_Growth[11,1] <- summary(f4_Growth)$adj.r.squared 


### Payout
table_Payout <- as.data.frame(matrix(NA, nrow=11, ncol=1))
colnames(table_Payout) <- c('Payout')
rownames(table_Payout) <-c('Excess Returns','CAPM-alpha','3-factor-alpha','4-factor-alpha','MKT','SMB','HML','UMD','Sharpe Ratio','Information Ratio','Adjusted R2')
#excess return
table_Payout[1,1]<-mean(data2$Payout) #should we substract rf or not?

#run regression 
capm_Payout<-lm(data=data2,Payout~mkt)
f3_Payout<-lm(data=data2,Payout~mkt+smb+hml)
f4_Payout<-lm(data=data2,Payout~mkt+smb+hml+umd)

#put them into the table
table_Payout[2,1]<-coef(capm_Payout)[1] #capm-alpha
table_Payout[3,1]<-coef(f3_Payout)[1] #3factor-alpha
#4factor coefficients
for (c in (4:8)) {
  table_Payout[c,1] <- coef(f4_Payout)[c-3] 
} 

#sharpe ratio
table_Payout[9,1] <-  mean(data2$Payout)/sd(data2$Payout)

#information ratio
table_Payout[10,1] <- coef(f4_Payout)[1]/sd(f4_Payout$residuals) 

#Adjusted R2
table_Payout[11,1] <- summary(f4_Payout)$adj.r.squared 


#Merge 5 tables togeter
table6 <- merge(table_qmj,table_Profitability, by='row.names',all=TRUE)
row.names(table6) <- table6$Row.names
table6 <-table6 %>% select(-Row.names)
table6 <- merge(table6,table_Safety, by='row.names',all=TRUE)
row.names(table6) <- table6$Row.names
table6 <-table6 %>% select(-Row.names)
table6 <- merge(table6,table_Growth, by='row.names',all=TRUE)
row.names(table6) <- table6$Row.names
table6 <-table6 %>% select(-Row.names)
table6 <- merge(table6,table_Payout, by='row.names',all=TRUE)
row.names(table6) <- table6$Row.names
table6 <-table6 %>% select(-Row.names)


#reorder the table
table6<-table6[c('Excess Returns','CAPM-alpha','3-factor-alpha','4-factor-alpha','MKT','SMB','HML','UMD','Sharpe Ratio','Information Ratio','Adjusted R2'),]


#export table 6
write.csv(table6, 'table6.csv', row.names=TRUE)




### Table 9 #####
#Keyu Jin, keyujin@brandeis.edu

library(dplyr)

# load data
setwd("/Users/Ella/Documents/研究�?/Field Project/Bridge/SP500")
data <- read.csv('2_data_table4.csv')
qmj_raw<-read.csv('data_table5_cleaned.csv')

#merge qmj factor with other factors
data2 <- merge(data, qmj_raw, by.x=c('fyear'), by.y=c('fyear'), name=NULL)

#select only the factors for each year
data2<- data2 %>% select(mkt,smb,hml,umd,avg_rf,return_z_quality) %>% distinct(smb,.keep_all = TRUE)
names(data2)[names(data2) == "return_z_quality"] <- "qmj"

#run regression and create table 9
###smb
table_smb <- as.data.frame(matrix(NA, nrow=9, ncol=2))
colnames(table_smb) <- c('SMB','SMB2')
rownames(table_smb) <-c('Excess Returns','Alpha','MKT','HML','UMD','QMJ','Sharpe Ratio','Information Ratio','Adjusted R2')
#excess return
table_smb[1,1]<-mean(data2$smb) #should we substract rf or not?
table_smb[1,2]<-mean(data2$smb)

#run regression 
lm_smb<-lm(data=data2,smb~.-avg_rf-qmj)
lm_smb2<-lm(data=data2,smb~.-avg_rf)

#put them into the table
for (c in (2:9)) {
  table_smb[c,1] <- coef(lm_smb)[c-1] 
  table_smb[c,2] <-coef(lm_smb2)[c-1]
}
#sharpe ratio
table_smb[7,1] <-  mean(data2$smb)/sd(data2$smb)
table_smb[7,2] <-  mean(data2$smb)/sd(data2$smb)

#information ratio
table_smb[8,1] <- coef(lm_smb)[1]/sd(lm_smb$residuals) # alpha of lm_smb/sd.residual
table_smb[8,2] <- coef(lm_smb2)[1]/sd(lm_smb2$residuals) # alpha of lm_smb2/sd.residual

#Adjusted R2
table_smb[9,1] <- summary(lm_smb)$adj.r.squared 
table_smb[9,2] <- summary(lm_smb2)$adj.r.squared


###hml
table_hml <- as.data.frame(matrix(NA, nrow=9, ncol=2))
colnames(table_hml) <- c('HML','HML2')
rownames(table_hml) <-c('Excess Returns','Alpha','MKT','SMB','UMD','QMJ','Sharpe Ratio','Information Ratio','Adjusted R2')
#excess return
table_hml[1,1]<-mean(data2$hml)
table_hml[1,2]<-mean(data2$hml)

#run regression
lm_hml<-lm(data=data2,hml~.-avg_rf-qmj)
lm_hml2<-lm(data=data2,hml~.-avg_rf)

#put them into the table
for (c in (2:9)) {
  table_hml[c,1] <- coef(lm_hml)[c-1] 
  table_hml[c,2] <-coef(lm_hml2)[c-1]
}
#sharpe ratio
table_hml[7,1] <-  mean(data2$hml)/sd(data2$hml)
table_hml[7,2] <-  mean(data2$hml)/sd(data2$hml)

#information ratio
table_hml[8,1] <- coef(lm_hml)[1]/sd(lm_hml$residuals) # alpha of lm_hml/sd.residual
table_hml[8,2] <- coef(lm_hml2)[1]/sd(lm_hml2$residuals) # alpha of lm_hml2/sd.residual

#Adjusted R2
table_hml[9,1] <- summary(lm_hml)$adj.r.squared 
table_hml[9,2] <- summary(lm_hml2)$adj.r.squared


###umd
table_umd <- as.data.frame(matrix(NA, nrow=9, ncol=2))
colnames(table_umd) <- c('UMD','UMD2')
rownames(table_umd) <-c('Excess Returns','Alpha','MKT','SMB','HML','QMJ','Sharpe Ratio','Information Ratio','Adjusted R2')
#excess return
table_umd[1,1]<-mean(data2$umd)
table_umd[1,2]<-mean(data2$umd)

#run regression
lm_umd<-lm(data=data2,umd~.-avg_rf-qmj)
lm_umd2<-lm(data=data2,umd~.-avg_rf)

#put them into the table
for (c in (2:9)) {
  table_umd[c,1] <- coef(lm_umd)[c-1] 
  table_umd[c,2] <-coef(lm_umd2)[c-1]
}
#sharpe ratio
table_umd[7,1] <-  mean(data2$umd)/sd(data2$umd)
table_umd[7,2] <-  mean(data2$umd)/sd(data2$umd)

#information ratio
table_umd[8,1] <- coef(lm_umd)[1]/sd(lm_umd$residuals) # alpha of lm_umd/sd.residual
table_umd[8,2] <- coef(lm_umd2)[1]/sd(lm_umd2$residuals) # alpha of lm_umd2/sd.residual

#Adjusted R2
table_umd[9,1] <- summary(lm_umd)$adj.r.squared 
table_umd[9,2] <- summary(lm_umd2)$adj.r.squared

#Merge 3 table togeter
table9 <- merge(table_smb,table_hml, by='row.names',all=TRUE)
row.names(table9) <- table9$Row.names
table9 <-table9 %>% select(-Row.names)
table9 <- merge(table9,table_umd, by='row.names',all=TRUE)
row.names(table9) <- table9$Row.names
table9 <-table9 %>% select(-Row.names)

#reorder the table
table9<-table9[c('Excess Returns','Alpha','MKT','SMB','HML','UMD','QMJ','Sharpe Ratio','Information Ratio','Adjusted R2'),]
colnames(table9) <- c('SMB','SMB','HML','HML','UMD','UMD')

#export table 9
write.csv(table9, 'table9.csv', row.names=TRUE)




