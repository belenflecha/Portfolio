##### Master of Science in Business Analytics
#### Hult International Business School 2021-2022
###Creadted by María Belén Flecha Carrascosa
##Date: 11-7-2021

##The goal of the following statistical analysis is to discover which are the factors related to the number of online purchases

library(readxl)
mkt_ds <- read_excel("/Users/beluflecha/Desktop/Portfolio Examples/Statistical Analysis/mkt_ds.xlsx")
View(mkt_ds)

#Massaging the data and cleaning dataset from N/A
my_udf_datasetname<- function (df, col_idx) { 
  new_df<- df
  for(i in 1:col_idx) {
    if (length(which(is.na(new_df[,i]))) > 1){
      new_df <- new_df[-which(is.na(new_df[,i])),]
    } 
  }
  return(new_df)
}
mkt_ds<- my_udf_datasetname(mkt_ds, ncol(mkt_ds))
mkt_ds

##Checking if there is information twice
#Understanding how is the dataset conform
sapply(mkt_ds, function(x) length(unique(x)))
#Checking for uniqueness in the id column. 
length(unique(mkt_ds$ID))
#There are not duplicated ID´s

####Analyzing web purchases (statistical analysis)
#Understading how NumWebPurchases is conform
min(mkt_ds$NumWebPurchases)
mean(mkt_ds$NumWebPurchases) #The mean of purchases is in 4
max(mkt_ds$NumWebPurchases)
hist(mkt_ds$numwebpurchases)
##Converting web purchases into a business success/failure variable
#>1 = business success
#<0 = business failure
is.numeric(mkt_ds$NumWebPurchases)
mkt_ds$Business_Success <- ifelse(mkt_ds$NumWebPurchases>=1,1,0)

###Working with categorical variables
library(udpipe)
library(textrank)
#How is Marital Status compose
marital_status_freq <- txt_freq(x = mkt_ds$Marital_Status)
#Convinning the status to convert the variable into a binary
#married, together; #single, divorce, widow,absurd, YOLO as alone
mkt_ds$Marital_Status_Factor <- as.factor(mkt_ds$Marital_Status)
mkt_ds$Marital_Status_Num <- as.numeric(mkt_ds$Marital_Status)

#How is education compose
eductaion_freq <- txt_freq(x = mkt_ds$Education)

#Running our first logistic regression to see which numeric variables are significant to explain 
#web purchases (using only numerical data)
my_logit <- glm(Business_Success~Income+Kidhome+Teenhome+Recency+MntWines+MntFruits+
                  MntMeatProducts+MntFishProducts+MntSweetProducts+MntGoldProds+
                  NumDealsPurchases+NumCatalogPurchases+NumWebVisitsMonth,
                   data=mkt_ds, family = "binomial")
summary(my_logit)
####
#At a significant level of 0.05, variables significant to explain web purchase are_
#NumCaralogPurchase, MntGoldProds, MntFishProducts, MntMeatProducts,MntWines,Kidhome
#Running a second regression without the insiginificant variables
my_logit_fix <- glm(Business_Success~Kidhome+MntWines+
                  MntMeatProducts+MntFishProducts+MntGoldProds+NumCatalogPurchases,
                data=mkt_ds, family = "binomial")
summary(my_logit_fix)
#Now all variables are significant at a level of 0.05
#Explaining how each variable affect the web purchase
exp(1.545895)-1 #Having a kid home increases the odds of business success(making a web purchase)
                #in 369%
exp(0.066950)-1  #Buying wine increases the odds of web purchase in 6.9%
exp(-0.007009)-1 #Buying meat decreases the odds of web purchase by 0.70%
exp(0.308485)-1 #Buying fish increases the odds of web purchase by 30%
exp(0.154763 )-1 #Buying gold products increases the odds of web purchase by 16%
exp(-1.232261)-1 #Buying through catalog decreases the odds of web purchase by 70%

#Normalizing data
#Creating a UDF to do that
normal <- function(var1){
  my_normal <- (var1 - min(var1)) / (max(var1)- min(var1))
  return (my_normal)
}#closing the normal UDF
#normal function
mkt_ds$kidhome_norm <- normal(mkt_ds$Kidhome)
mkt_ds$teenhome_norm <- normal(mkt_ds$Teenhome)
mkt_ds$recency_norm <- normal(mkt_ds$Recency)
mkt_ds$mnt_wines_norm <- normal(mkt_ds$MntWines)
mkt_ds$mnt_fruits_norm <- normal(mkt_ds$MntFruits)
mkt_ds$mnt_meat_products_norm <- normal(mkt_ds$MntMeatProducts)
mkt_ds$mnt_fish_products_norm <- normal(mkt_ds$MntFishProducts)
mkt_ds$mnt_sweet_products_norm <- normal(mkt_ds$MntSweetProducts)
mkt_ds$mnt_gold_products_norm <- normal(mkt_ds$MntGoldProds)
mkt_ds$num_deals_purchase_norm <- normal(mkt_ds$NumDealsPurchases)
mkt_ds$num_catalog_purchase_norm <- normal(mkt_ds$NumCatalogPurchases)
mkt_ds$num_store_purchase_norm <- normal(mkt_ds$NumStorePurchases)
mkt_ds$num_web_visit_norm <- normal(mkt_ds$NumWebVisitsMonth)

#Runing another regression with normalized data to compare variables side by side
my_logit_norm_fix <- glm(Business_Success~kidhome_norm+mnt_wines_norm+
                      mnt_meat_products_norm+mnt_fish_products_norm+mnt_gold_products_norm+
                     num_catalog_purchase_norm,data=mkt_ds, family = "binomial")
summary(my_logit_norm_fix)
#We can see that the variables that have a strong positive impact in web purchase are:
#mnt_wines, mnt_fish and mnt_gold
#And the variables that have a strong negative impact on web purchase are: mnt_meat and
#catalog_purchase

#Plotting the findings
library(ggplot2)

# Create a data frame with coefficient estimates and p-values
coef_df <- data.frame(
  Predictor = c("Kidhome", "MntWines", "MntMeatProducts", "MntFishProducts", 
                "MntGoldProds", "NumCatalogPurchases"),
  Estimate = c(1.545895, 0.066950, -0.007009, 0.308485, 0.154763, -1.232261),
  Significant = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
)

# Create a bar plot
ggplot(coef_df, aes(x = Predictor, y = Estimate, fill = Significant)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("TRUE" = "#E1CCBE", "FALSE" = "red")) +
  labs(x = "Predictor Variables", y = "Coefficient Estimate",
       title = "Factors affecting Online Purchases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



  