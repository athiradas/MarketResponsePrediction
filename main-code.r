#rm(list = ls())
setwd("D:/MSBA/Fall2016/IDS572/labs/assignment 2/")

#install.packages("randomForest")
#install.packages("missForest")
library(randomForest)
library(missForest)
library(rpart)
library(lubridate)
library(caret)

#read data
data <- read.csv('transformed.csv', header = TRUE, stringsAsFactors=FALSE)
data1 <- data
summary(data1)
cols_factor <- c("TARGET_B","epva_state", "recinhse","STATE","VETERANS",
                 "WEALTH2",
                 "urbancity_level", "econoic_status", "X96_nk_amount", "X96_nk_recency", "gender")
data1[cols_factor] <- lapply(data1[cols_factor], factor)
cols_date <- c("ODATEDW","ADATE_2", "ADATE_3", "MAXRDATE")
data1[cols_date] <- lapply(data1[cols_date], 
                           function(x) 
                             as.integer(difftime(as.POSIXct("1997-12-01"),
                                      as.POSIXct(as.Date(x, "%d/%m/%y %I:%M %p")), 
                                      units="days"))
                                      )




# Missing values using random forest
pca_l <- which( colnames(data1)=="POP901" )
pca_u <- which( colnames(data1)=="AC2" )
data_pca <- data1[,pca_l:pca_u]
datapca_imputed <- missForest(xmis = data.frame(data_pca), ntree=100, mtry=5, replace=TRUE)
datapca_impute1 <- datapca_imputed$ximp
data_wopca <- data1[,-pca_l:-pca_u]
summary(data_wopca)
data1_imputed <- missForest(xmis = data.frame(data_wopca), ntree=100, mtry=5, replace=TRUE)
data1_impute1 <- data1_imputed$ximp

# data2 <- cbind(datapca_impute1, data1_impute1)
# 
# 
# write.csv(data2, "test11.csv")
 

#Find most important variables - Variable importance plot 
rf_fit <- randomForest(TARGET_B~., data=data1_impute1, importance=TRUE, ntree=500, mtry=2)
#Find variable importance
varImpPlot(rf_fit)
rf_fit$importance

#Plot Decison tree to find important variables 
dt_fit = rpart(TARGET_B~., data=data.frame(data1_imputed$ximp), method="class")
printcp(dt_fit) # display the results
plotcp(dt_fit) # visualize cross-validation results
summary(dt_fit) # detailed summary of splits

#plot tree
plot(dt_fit, uniform=TRUE)
text(dt_fit, use.n=TRUE, all=TRUE, cex=.8)


#Do PCA for hobbies
hobby_pca <- selected_pca(datapca_impute1)[,1:16]
hobby_pca_sel <- as.data.frame(hobby_pca)


#Add PCA components to original data
data3 <- cbind(data1_impute1, hobby_pca_sel)
data4 <- cbind(hobby_pca_sel, data1_impute1$TARGET_B)
#names(data.train)

write.csv(data3, "data3.csv")

write.csv(data4, "data4.csv")


names(data1_impute1)




dataoutpca <- subset(datapca_impute1, select =-c(HHD4,ANC9,IC23,TPE7,OCC7, EIC2,
                                                 EIC12,ETH7,ETHC2,EIC3,AFC3,ANC2,
                                                 ETH8,ETH10,AGE907,HHN2,DW3,
                                                 ETHC6,RHP2,HUPA7,IC5,IC20,PEC1,OCC4,
                                                 OCC5,OCC9,EIC8,EIC9,EIC13,EIC14,OEDC4,
                                                 EC6,AFC4,ANC1,ANC1 ,ANC8 ,HC15,EC8,MC3,HHD1))

dataoutpcavars <- subset(datapca_impute1, select =c(HHD4,ANC9,IC23,TPE7,OCC7, EIC2,
                                                    EIC12,ETH7,ETHC2,EIC3,AFC3,ANC2,
                                                    ETH8,ETH10,AGE907,HHN2,DW3,
                                                    ETHC6,RHP2,HUPA7,IC5,IC20,PEC1,OCC4,
                                                    OCC5,OCC9,EIC8,EIC9,EIC13,EIC14,OEDC4,
                                                    EC6,AFC4,ANC1,ANC1 ,ANC8 ,HC15,EC8,MC3,HHD1))

hobby_pca_wfilter <- selected_pca(dataoutpca)[,1:18]
hobby_pca_sel_wfilter <- as.data.frame(hobby_pca)


data5 <- cbind(data1_impute1, dataoutpcavars, hobby_pca_sel_wfilter)
write.csv(data5, "data5.csv")


dataoutpcavarsforcorr <- dataoutpcavars

highCorr <- findCorrelation(cor(dataoutpcavars, use="pairwise", method="spearman"),
                            cutoff =  0.80, verbose = TRUE) 

dataoutpcavarswocorr <- dataoutpcavarsforcorr [,-highCorr]
data6 <- cbind(data1_impute1, dataoutpcavarswocorr, hobby_pca_sel_wfilter)
write.csv(data5, "data6.csv")






