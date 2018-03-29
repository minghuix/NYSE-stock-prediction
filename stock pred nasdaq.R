install.packages("XML")
install.packages("RCurl")
install.packages("bitops")
library(XML)
library(bitops)
library(RCurl)
library(MASS)
library(e1071)
library(mlbench)
library(caret)
library(missForest)
library(mice)


#### Test if url exists
## ABCD
stock_names<-read.table('C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\a_d_nasdaq.txt',header = TRUE,sep = "\t",quote=NULL)
ex_list <- c('-',"\\.",'ABRN','AC','AEB','AED','AEH','AEK','AEUA','AFC','AFGH','AFSD'
             ,'AFSS','AFST','AFT','AGM.A','AIC','AIF','AIW','AIY','AJXA','AMGP','BDXA','BGCA'
             ,'BGIO','CIC','CLPR','COH','DDT','DKT','DMB','DMO')
stock_clean1 <- stock_names[!grepl(paste(ex_list,collapse = "|"),stock_names$Symbol),]
stock_id<-stock_clean1[,1]
stock_id<-as.character(stock_id)
urls <- paste0("http://finviz.com/quote.ashx?t=", stock_id)
test_df<-data.frame('id'= stock_id,"url_test" = urls)
url_result<-sapply(as.character(test_df[,2]), url.exists)
test_df$results<-url_result
write.table(test_df, "C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\url_test_results\\MyData1.csv", sep=",", 
            row.names=FALSE, col.names=FALSE)

## EFGH
stock_names2<-read.table('C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\e_h_nasdaq.txt',header = TRUE,sep = "\t",quote=NULL)
ex_list2 <- c('-',"\\.")
stock_clean2 <- stock_names2[!grepl(paste(ex_list2,collapse = "|"),stock_names2$Symbol),]
stock_id2<-stock_clean2[,1]
stock_id2<-as.character(stock_id2)
urls2 <- paste0("http://finviz.com/quote.ashx?t=", stock_id2)
test_df2<-data.frame('id'= stock_id2,"url_test" = urls2)
url_result2<-sapply(as.character(test_df2[,2]), url.exists)
test_df2$results<-url_result2
write.table(test_df2, "C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\url_test_results\\MyData2.csv", sep=",", 
            row.names=FALSE, col.names=FALSE)

## IJKL
stock_names3<-read.table('C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\i_l_nasdaq.txt',header = TRUE,sep = "\t",quote=NULL)
ex_list2 <- c('-',"\\.")
stock_clean3 <- stock_names3[!grepl(paste(ex_list2,collapse = "|"),stock_names3$Symbol),]
stock_id3<-stock_clean3[,1]
stock_id3<-as.character(stock_id3)
urls3 <- paste0("http://finviz.com/quote.ashx?t=", stock_id3)
test_df3<-data.frame('id'= stock_id3,"url_test" = urls3)
url_result3<-sapply(as.character(test_df3[,2]), url.exists)
test_df3$results<-url_result3
write.table(test_df3, "C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\url_test_results\\MyData3.csv", sep=",", 
            row.names=FALSE, col.names=FALSE)

## MNOP
stock_names4<-read.table('C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\m_p_nasdaq.txt',header = TRUE,sep = "\t",quote=NULL)
ex_list2 <- c('-',"\\.")
stock_clean4 <- stock_names4[!grepl(paste(ex_list2,collapse = "|"),stock_names4$Symbol),]
stock_id4<-stock_clean4[,1]
stock_id4<-as.character(stock_id4)
urls4 <- paste0("http://finviz.com/quote.ashx?t=", stock_id4)
test_df4<-data.frame('id'= stock_id4,"url_test" = urls4)
url_result4<-sapply(as.character(test_df4[,2]), url.exists)
test_df4$results<-url_result4
write.table(test_df4, "C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\url_test_results\\MyData4.csv", sep=",", 
            row.names=FALSE, col.names=FALSE)

## QRST
stock_names5<-read.table('C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\q_t_nasdaq.txt',header = TRUE,sep = "\t",quote=NULL)
ex_list2 <- c('-',"\\.")
stock_clean5 <- stock_names5[!grepl(paste(ex_list2,collapse = "|"),stock_names5$Symbol),]
stock_id5<-stock_clean5[,1]
stock_id5<-as.character(stock_id5)
urls5 <- paste0("http://finviz.com/quote.ashx?t=", stock_id5)
test_df5<-data.frame('id'= stock_id5,"url_test" = urls5)
url_result5<-sapply(as.character(test_df5[,2]), url.exists)
test_df5$results<-url_result5
write.table(test_df5, "C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\url_test_results\\MyData5.csv", sep=",", 
            row.names=FALSE, col.names=FALSE)

## UVWXYZ
stock_names6<-read.table('C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\u_z_nasdaq.txt',header = TRUE,sep = "\t",quote=NULL)
ex_list2 <- c('-',"\\.")
stock_clean6 <- stock_names6[!grepl(paste(ex_list2,collapse = "|"),stock_names6$Symbol),]
stock_id6<-stock_clean6[,1]
stock_id6<-as.character(stock_id6)
urls6 <- paste0("http://finviz.com/quote.ashx?t=", stock_id6)
test_df6<-data.frame('id'= stock_id6,"url_test" = urls6)
url_result6<-sapply(as.character(test_df6[,2]), url.exists)
test_df6$results<-url_result6
write.table(test_df6, "C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\url_test_results\\MyData6.csv", sep=",", 
            row.names=FALSE, col.names=FALSE)


# ## YZ
# stock_names7<-read.table('C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\Y_Z.txt',header = TRUE,sep = "\t",quote=NULL)
# ex_list2 <- c('-',"\\.")
# stock_clean7 <- stock_names7[!grepl(paste(ex_list2,collapse = "|"),stock_names7$Symbol),]
# stock_id7<-stock_clean7[,1]
# stock_id7<-as.character(stock_id7)
# urls7 <- paste0("http://finviz.com/quote.ashx?t=", stock_id7)
# test_df7<-data.frame('id'= stock_id7,"url_test" = urls7)
# url_result7<-sapply(as.character(test_df7[,2]), url.exists)
# test_df7$results<-url_result7
# write.table(test_df7, "C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\url test results\\MyData7.csv", sep=",", 
#             row.names=FALSE, col.names=FALSE)

test_df<-read.csv("C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\url_test_results\\MyData1.csv",header = FALSE)
test_df2<-read.csv("C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\url_test_results\\MyData2.csv",header = FALSE)
test_df3<-read.csv("C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\url_test_results\\MyData3.csv",header = FALSE)
test_df4<-read.csv("C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\url_test_results\\MyData4.csv",header = FALSE)
test_df5<-read.csv("C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\url_test_results\\MyData5.csv",header = FALSE)
test_df6<-read.csv("C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\url_test_results\\MyData6.csv",header = FALSE)
#test_df7<-read.csv("C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\url test results\\MyData7.csv",header = FALSE)

stock_all <- rbind(test_df, test_df2, test_df3,test_df4,test_df5,test_df6)

colnames(stock_all) <- c("id","url","results")
stock_use <- stock_all[which(stock_all$results == "TRUE"),]
dim(stock_use)
stocks<-stock_use$id
stocks<-as.character(stocks)

## extract data from url
for (s in stocks) {
  url <- paste0("http://finviz.com/quote.ashx?t=", s)
  webpage <- readLines(url)
  html <- htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
  tableNodes <- getNodeSet(html, "//table")
  
# ASSIGN TO STOCK NAMED DFS
  assign(s, readHTMLTable(tableNodes[[9]], 
                          header= c("data1", "data2", "data3", "data4", "data5", "data6",
                                    "data7", "data8", "data9", "data10", "data11", "data12")))
  
# ADD COLUMN TO IDENTIFY STOCK 
  df <- get(s)
  df['stock'] <- s
  assign(s, df)
}

# COMBINE ALL STOCK DATA 
stockdatalist <- cbind(mget(stocks))
stockdata <- do.call(rbind, stockdatalist)
# MOVE STOCK ID TO FIRST COLUMN
stockdata <- stockdata[, c(ncol(stockdata), 1:ncol(stockdata)-1)]

head(stockdata)
write.table(stockdata, "C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\combineddata.csv", sep=",", 
            row.names=FALSE, col.names=c("stock","data1","data2","data3", "data4", "data5", "data6",
                                         "data7", "data8", "data9", "data10", "data11", "data12"))
### data cleaning ###
raw_data_x = read.csv("C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\combineddata.csv",sep = ",")
x_try = raw_data_x[1:12,]

temp0 = data.frame("id" = c("stock", "A"))

for(i in seq(2,13,2)){
  temp0 = cbind(temp0, t(x_try[,i:(i+1)]))
}

feature_names = unlist(temp0[1, 1:73])

total_row = dim(raw_data_x)[1]
true_row = total_row/12
final_dataframe = data.frame(matrix(ncol = 73, nrow = 0))
colnames(final_dataframe) = feature_names
for(i in seq(1, total_row, 12)){
  x_cur = raw_data_x[i:(i+11),]
  temp = data.frame("stock" = c("stock", x_cur[,1][1]))
  for(j in seq(2,13,2)){
    temp = cbind(temp, t(x_cur[,j:(j+1)]))
  }
  rownum = ceiling(i/12)
  final_dataframe[rownum,] = unlist(temp[2,])
  cat("\r", rownum/true_row*100, "%")
}

write.csv(final_dataframe, "C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\stock_clean_v1.csv")
test_final<- read.csv("C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\stock_clean_v1.csv",header = TRUE,sep = ',')



# standardize the unit million/billion
#market cap
test_final$mcap_flag<-grepl("B", test_final$Market.Cap)
test_final$Market.Cap<-sapply(strsplit(as.character(test_final$Market.Cap), "M"), "[", 1)
test_final$Market.Cap<-sapply(strsplit(as.character(test_final$Market.Cap), "B"), "[", 1)
test_final$Market.Cap<-ifelse(test_final$mcap_flag == "FALSE",
                                round(as.numeric(test_final$Market.Cap)/1000,3),
                                round(as.numeric(test_final$Market.Cap),3))

#income
test_final$Inc_flag<-grepl("B", test_final$Income)
test_final$Income<-sapply(strsplit(as.character(test_final$Income), "M"), "[", 1)
test_final$Income<-sapply(strsplit(as.character(test_final$Income), "B"), "[", 1)
test_final$Income<-ifelse(test_final$Inc_flag == "FALSE",
                                round(as.numeric(test_final$Income)/1000,3),
                                round(as.numeric(test_final$Income),3))
#sales
test_final$sales_flag<-grepl("B", test_final$Sales)
test_final$Sales<-sapply(strsplit(as.character(test_final$Sales), "M"), "[", 1)
test_final$Sales<-sapply(strsplit(as.character(test_final$Sales), "B"), "[", 1)
test_final$Sales<-ifelse(test_final$sales_flag == "FALSE",
                                round(as.numeric(test_final$Sales)/1000,3),
                                round(as.numeric(test_final$Sales),3))

#Shs Outstand
test_final$shso_flag<-grepl("B", test_final$Shs.Outstand)
test_final$Shs.Outstand<-sapply(strsplit(as.character(test_final$Shs.Outstand), "M"), "[", 1)
test_final$Shs.Outstand<-sapply(strsplit(as.character(test_final$Shs.Outstand), "B"), "[", 1)
test_final$Shs.Outstand<-ifelse(test_final$shso_flag == "FALSE",
                         round(as.numeric(test_final$Shs.Outstand)/1000,3),
                         round(as.numeric(test_final$Shs.Outstand),3))

#Shs Float
test_final$shsf_flag<-grepl("B", test_final$Shs.Float)
test_final$Shs.Float<-sapply(strsplit(as.character(test_final$Shs.Float), "M"), "[", 1)
test_final$Shs.Float<-sapply(strsplit(as.character(test_final$Shs.Float), "B"), "[", 1)
test_final$Shs.Float<-ifelse(test_final$shsf_flag == "FALSE",
                                  round(as.numeric(test_final$Shs.Float)/1000,3),
                                  round(as.numeric(test_final$Shs.Float),3))

#Avg Volume
test_final$avgv_flag<-grepl("M", test_final$Avg.Volume)
test_final$Avg.Volume<-sapply(strsplit(as.character(test_final$Avg.Volume), "M"), "[", 1)
test_final$Avg.Volume<-sapply(strsplit(as.character(test_final$Avg.Volume), "K"), "[", 1)
test_final$Avg.Volume<-ifelse(test_final$avgv_flag == "FALSE",
                               round(as.numeric(test_final$Avg.Volume)/1000000,7),
                               round(as.numeric(test_final$Avg.Volume)/1000,3))

## get rid of some intermediate columns or unwanted columns
test_final<-test_final[,-c(1,2,11,12,36,55,56,57,70,71,73,74:79)]
test_final<-cbind(unique(raw_data_x$stock),test_final)

head(test_final)

### percentage columns to pure numbers
## percentage column numbers: 8,22,26-45,48,55-60
test_final_2<-test_final
test_final_2_sub<-test_final_2[,c(8,22,26:45,48,55:60)]
test_final_2_sub<-apply(test_final_2_sub,2,function(x) as.numeric(sub("%","",x))/100)
test_final_2<-test_final_2[,-c(8,22,26:45,48,55:60)] 
test_final_2<-cbind(test_final_2,test_final_2_sub)
test_final_2$Volume<-as.numeric(gsub(",","",test_final_2$Volume))
write.csv(test_final_2, "C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\stock_clean_v2.csv")

# adjust magnitude of some columns
test_final_2<-apply(test_final_2[,2:63],2,function(x) as.numeric(x))
test_final_2<-data.frame(cbind(as.character(unique(raw_data_x$stock)),test_final_2))
test_final_2$Employees<-log(as.numeric(test_final_2$Employees))
test_final_2$Volume<-log(as.numeric(test_final_2$Volume))
test_final_2$group<-ifelse(as.numeric(as.character(test_final_2$Perf.Year))>=1.5,1,
                           ifelse(as.numeric(as.character(test_final_2$Perf.Year))>=1.2,2,
                                  ifelse(as.numeric(as.character(test_final_2$Perf.Year))>=0.8,3,
                                         ifelse(as.numeric(as.character(test_final_2$Perf.Year))>=0.6,4,
                                                ifelse(as.numeric(as.character(test_final_2$Perf.Year))>=0.4,5,
                                                       ifelse(as.numeric(as.character(test_final_2$Perf.Year))>=0.2,6,7 ))))))
write.csv(test_final_2, "C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\stock_clean_v3.csv")

## ======= Modeling ======= ##

##imputing missing value
data1<-read.csv("C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\stock_clean_v3.csv",header = TRUE,sep = ",")
na_count <-sapply(data1, function(y) sum(length(which(is.na(y)))))
na_count<- data.frame(na_count)
na_count$na_count<-na_count$na_count/2722
## manually selecting some of the variables that may have higher impacts 
## due to large volume of missing data



data2<-read.csv("C:\\Users\\mxiong\\Downloads\\kaggle\\stock\\NASDAQ\\stock_clean_v4.csv",header = TRUE,sep = ",")
data2<-data2[, -which(colMeans(is.na(data2)) > 0.5)]  ## delete columns with more than 50% missing data
data2<-data2[-which(rowMeans(is.na(data2)) > 0.5),]   ## delete rows with more than 50% missing data

#missForest
# install.packages("missForest")
# library(missForest)
#data2.imp <- missForest(data2[,2:57])
#?missForest
#head(data2.imp$ximp)
#head(data2.imp$OOBerror)
#data3<-data.frame(cbind(as.character(data1$V1),data2.imp$ximp))
# install.packages("Hmisc")
# library(Hmisc)
# ?aregImpute

#### MICE ####
install.packages("mice")
library(mice)
data2_mice_new<- mice(data2[,2:56], m=5, maxit = 50, method = 'pmm', seed = 500)
data2_mice_complete_new <- complete(data2_mice_new,3)
data2_mice_complete_new<-data.frame(cbind(as.character(data2$V1),data2_mice_complete_new,data2$group))
colnames(data2_mice_complete_new)[1]<-"stock"
colnames(data2_mice_complete_new)[57]<-"group"
# data2_mice_complete_new


#Feature selection
# Calculate correlation
# load the library
install.packages("mlbench")
install.packages("caret")
library(mlbench)
library(caret)
# calculate correlation matrix
correlationMatrix <- cor(data2_mice_complete_new[,2:56])
# find attributes that are highly corrected (ideally >0.70)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.60)
# print indexes of highly correlated attributes
print(highlyCorrelated)
data2_mice_complete_new<-data2_mice_complete_new[,-c(47, 36 ,28, 50 ,46 ,53, 52, 26 ,27,  1 , 2,  3, 18 ,23, 11 ,13 ,16)]
na_count <-sapply(data2_mice_complete_new, function(y) sum(length(which(is.na(y)))))
na_count<- data.frame(na_count)
data2_mice_complete_new$group<-ifelse(is.na(data2_mice_complete_new$group),7,data2_mice_complete_new$group)

### LDA ###
#training set
set.seed(16)
data_tr<-data2_mice_complete_new[sample(nrow(data2_mice_complete_new),1000),]
#fit model
fit <- lda(group~., data=data_tr[,2:39])
# make predictions
predictions <- predict(fit, data2_mice_complete_new[,2:39])$class
comparison<-data.frame(cbind(as.character(data1$V1),data1$group,predictions))
comparison$improve<-as.numeric(as.character(comparison$predictions))-as.numeric(as.character(comparison$V2))
# cross-validation
fit_cv <- lda(group~., data=data2_mice_complete_new[,2:39],CV = TRUE)
comparison_lda_cv<-data.frame(cbind(as.character(data1$V1),data2_mice_complete_new$group,as.character(fit_cv$class)))
comparison_lda_cv$improve<-as.numeric(as.character(comparison_lda_cv$X3))-as.numeric(as.character(comparison_lda_cv$X2))



### naive bayes ###
# calculate correlation matrix
correlationMatrix <- cor(data3[,2:42])
# find attributes that are highly corrected (ideally >0.70)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)
data4<-data3[,-c(37, 16, 40, 28 , 7 , 1 ,31, 13, 38, 39)]
data_train<-data4[sample(nrow(data4),1000),]
model.naive <- naiveBayes(as.factor(data_train$group) ~ ., data = data_train)
str(model.naive)
preds <- predict(model.naive, newdata=data4[,1:32])
comparison.naive<-data.frame(cbind(as.character(data1$V1),data1$group,as.character(preds)))
comparison.naive$improve<-as.numeric(as.character(comparison.naive$X3))-as.numeric(as.character(comparison.naive$X2))


### svm ###
install.packages("e1071")
library(e1071)
head(data("BreastCancer"))
data4<-data3[,-1]
data_train<-data4[sample(nrow(data4),1000),]
model.svm<-svm(data_train$group~.,data_train)
res<-predict(model.svm,newdata = data4)
comparison.svm<-data.frame(cbind(as.character(data1$stockid),data1$group,res))
comparison.svm$improve<-as.numeric(as.character(comparison.svm$res))-as.numeric(as.character(comparison$V2))

### Multinomial logistic regression ###

##############################
## PCA for dimension deduction
##############################
data_pca <- data2_mice_complete_new[,-c(1,57)] # delete stock id and group
pca_result <- prcomp(data_pca, scale. = T)
std_dev <- pca_result$sdev # standard deviation
# compute variance
pr_var <- std_dev^2
# proportion of variance explained
prop_var_ex<-pr_var/sum(pr_var)

#cumulative scree plot
plot(cumsum(prop_var_ex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
abline(a = 0.9,b=0) # 32 pc selected

data_post_pca <- data.frame(data2_mice_complete_new[,1],pca_result$x[,1:32],data2_mice_complete_new[,57])
colnames(data_post_pca)[1]<-"stock"
colnames(data_post_pca)[34]<-"group"

### LDA ###
# cross-validation
fit_cv_pca <- lda(group~., data=data_post_pca[,2:34],CV = TRUE)
comparison_lda_cv_pca<-data.frame(cbind(as.character(data2_mice_complete_new$stock),data2_mice_complete_new$group,as.character(fit_cv_pca$class)))
comparison_lda_cv_pca$improve<-as.numeric(as.character(comparison_lda_cv_pca$X3))-as.numeric(as.character(comparison_lda_cv_pca$X2))
