################################################################################
# PROLOG 
################################################################################
# ML topic : k Nearest Neighbour Algorithm  
# Project  : Diabetes diagnostic Prediction Problem 
# Data     : Diabetes in Pima Indian Women from MASS Package 
# Author   : Seung Hyun Sung 
# Created  : 10/10/2021 
# Latest   :  
################################################################################

# 1.0 libraries ----
library(MASS) 
library(caret) 
library(dplyr) 
library(e1071) 
library(ROCR) 
library(GGally)
library(kableExtra)
library(readr)
library(plyr)
library(dplyr)
library(sampling)

set.seed(42)
options(digits = 3, scipen = 100)
10000000
450000*2


# 2.0 resampling ----

data(Pima.tr); data(Pima.te) 

# unbalanced + test data has more data in space 
dim(Pima.tr) # 200   8
dim(Pima.te) # 332   8

Pima <- rbind(Pima.tr, Pima.te)

prop.table(table(Pima$type))


#########################|
# Visualisation 
GGp_r <- ggpairs(Pima[ ,c(8, 1:7)], aes(color = type, alpha =0.75), lower = list(continuous = "smooth")) + theme_bw() + 
  labs(title = "Diabetes") +
  theme(plot.title = element_text(face ='bold', color ='black', hjust=0.5, size =12)) 

write_rds(GGp_r, file = "kNN/02_Visualisation/GGpairs_diabetes_raw_plot.Rds")

#########################|
vars = colnames(Pima)
numeric_vars <- vars[vars != "type"]
target_var   <- vars[vars == "type"]

my_fun <- function(x,y) {
  Table<-table(y, x)
  Test<-chisq.test(Table, corr = TRUE)
  out <- data.frame("Chi.Square" = round(Test$statistic,3)
                    , "df" = Test$parameter
                    , "p.value" = round(Test$p.value, 10)
  )
  
}

Pima_wo_y <- Pima %>% select(-type)
chisq_results <- ldply(Pima_wo_y, my_fun, y = Pima$type)
chisq_results %>% arrange(p.value)
#     .id Chi.Square  df      p.value
# 1   age    127.475  45 0.0000000008
# 2   glu    228.742 125 0.0000000438
# 3 npreg     65.211  16 0.0000000677
# 4  skin     71.087  49 0.0212504160
# 5    bp     52.459  41 0.1082606242
# 6   bmi    236.411 221 0.2271746943
# 7   ped    419.019 412 0.3949946090

grmeans <- Pima_std %>% group_by(type) %>% summarize_all(list(mean))
#   type  npreg   glu    bp  skin   bmi   ped   age
# 1 No     2.93  110.  69.9  27.3  31.4 0.446  29.2
# 2 Yes    4.70  143.  74.7  33.0  35.8 0.617  36.4
log(-3)
grsd <- Pima_std %>% group_by(type) %>% summarize_all(list(sd))
#   type  npreg   glu    bp  skin   bmi   ped   age
# 1 No     2.79  24.3  11.9  10.1  6.55 0.299  9.90
# 2 Yes    3.92  31.3  12.5  10.4  6.61 0.399 10.8 

# Feature scaling usually helps, but it is not guaranteed to improve performance.
# If you use distance-based methods like SVM, omitting scaling will basically 
# result in models that are disproportionally influenced by the subset of 
# features on a large scale. It may well be the case that those features are in 
# fact the best ones you have. In that case, scaling will reduce performance.

psych::describe(Pima)
dim(Pima)

# stratified_sampling
ssample <- strata(Pima, stratanames = c("type"), size =c(177,177), method="srswor")
Pima_ss <- getdata(Pima, ssample)
rearrange(Pima_ss)
caret_split <- caret::createDataPartition(Pima_ss$ID_unit, p = 0.8, list = FALSE)
train <- Pima[caret_split, ]
test <-  Pima[-caret_split, ]

table(train$type)
table(test$type)
lda_fit <- lda(type ~., data = train)

lda_predict <- predict(lda_fit, newdata = test)

caret::confusionMatrix(lda_predict$class, test$type)

# Scaled data
scale_model <- caret::preProcess(Pima %>% select(-type), method = c("center", "scale"))
Pima_std <- predict(scale_model, Pima)

caret_split2 <- caret::createDataPartition(Pima_std$type, p = 0.8, list = FALSE)
train_std <- Pima[caret_split2, ]
test_std <-  Pima[-caret_split2, ]

lda_std_fit <- lda(type ~., data = train_std)

lda_std_predict <- predict(lda_std_fit, newdata = test)

caret::confusionMatrix(lda_std_predict$class, test_std$type)


# With a `strata` argument, the random sampling is conducted *within the 
# stratification variable*. This can help ensure that the resamples have 
# equivalent proportions as the original data set. For a categorical variable,
# sampling is conducted separately within each class.
# 




# 
# Outlier 
# 
library("shiny")  
runGitHub("schoonees/kNN")




