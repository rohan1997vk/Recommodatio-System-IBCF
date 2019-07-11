#Recommandation System
library(recommenderlab)
library(methods)
library(data.table)
library(ggplot2)
library(knitr)
library(dplyr)

#loading data
transaction_data <- fread("transaction_data.csv")
product <- fread("product.csv")
t1 <- inner_join(transaction_data,product,by= "PRODUCT_ID")
sub_trans <- data.table(t1$household_key,t1$BASKET_ID,
                        t1$PRODUCT_ID,t1$COMMODITY_DESC,t1$QUANTITY,t1$SALES_VALUE)
colnames(sub_trans) <- c("household_key","basket_id","product_id","description","quantity","sales")

sub_trans <- sub_trans[complete.cases(sub_trans),]
r100 <- sort(table(sub_trans$product_id))
r1 <- tail(r100,100)
sub_trans1 <- filter(sub_trans, sub_trans$product_id == r1)
sub_trans1 <- data.table(sub_trans1)


setkeyv(sub_trans1, c('product_id', 'description'))
itemCode <- unique(sub_trans1[, c('product_id', 'description')])
setkeyv(sub_trans1, NULL)

df_train_ori <- dcast(sub_trans1, sub_trans1$household_key ~ sub_trans1$product_id, value.var = 'quantity',fun.aggregate = sum, fill=0)

CustomerId <- df_train_ori[,1] 


df_train <- as.matrix(df_train_ori)
df_train <- df_train[rowSums(df_train) > 5,colSums(df_train) > 5] 
df_train <- binarize(as(df_train, "realRatingMatrix"), minRatin = 1)



which_train <- sample(x = c(TRUE, FALSE), size = nrow(df_train_ori),replace = TRUE, prob = c(0.8, 0.2))
y <- df_train[!which_train]
x <- df_train[which_train]

method <- 'IBCF'
+9+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++n_recommended <- 5
n_training <- 1000



recc_model <- Recommender(data = x, method = method, parameter = parameter)
model_details <- getModel(recc_model)
recc_predicted <-predict(object = recc_model, newdata=y,n = n_recommended, type="topNList")
recomm <- as(recc_predicted,"list")[1:297]
recommender_models <- recommenderRegistry$get_entries(dataType ="binaryRatingMatrix")
recommender_models$IBCF_binaryRatingMatrix$parameters



rc <- read.csv("recomm.csv")
replace_name <- data.frame(sub_trans1$product_id,sub_trans1$description)
rc$R1 <- sub_trans1$description[rc$R1]
