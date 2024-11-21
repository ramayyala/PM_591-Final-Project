---
title: "Report"
author: "Ram Ayyala"
date: '2022-05-03'
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
require(dplyr)
require(reshape2)
require(mlr3)
require(mlr3verse)
require(mlr3viz)
require(GGally)
require(precrec)
require(ggfortify)
require(factoextra)
require(ggplot2)
require(mlr3pipelines)
require(mlr3fselect)
require(mlr3learners)
require(randomForest)
require(pROC)
```

```{r include=FALSE}
df<-read.csv("NIS2012-200K.csv")
```




```{r include=FALSE}

columns <- c("AGE","APRDRG_Risk_Mortality","APRDRG_Severity",
             "CM_AIDS","CM_ALCOHOL","CM_ANEMDEF","CM_ARTH","CM_BLDLOSS",
             "CH_CHF","CM_CHRLUNG","CM_COAG","CM_DEPRESS","CM_DM","CM_DMCX","CM_DRUG","CHM_HTN_C","CM_HYPOTHY",
             "CM_LIVER","CM_LYMPH","CM_LYTERS","CM_METS","CM_NERUO","CM_OBESE","CM_PARA","CM_PERIVASC","CM_PSYCH",
             "CM_PULMCIRC","CM_RENLFAIL","CM_TUMOR","CM_UCLER","CM_VALVE","CM_WGHTLOSS","DIED",
             "FEMALE","HOSP_DIVISION","LOS","NCHRONIC",
             "NDX","NEOMAT","ORPROC","PAY1","RACE","TRAN_IN","TRAN_OUT","YEAR","ZIPINC_QRTL")

columns<-intersect(columns,colnames(df))
df <- df[,columns]
```


```{r include=FALSE}
df$DIED<-na_if(df$DIED, "A")
df$FEMALE<-na_if(df$FEMALE, "C")
df$PAY1<-na_if(df$PAY1, "A")
df$RACE<-na_if(df$RACE, "A")
df$ZIPINC_QRTL<-na_if(df$ZIPINC_QRTL, "A")
```


```{r include=FALSE}
df$DIED <-factor(df$DIED,levels = c(0,1),labels = c("Alive","Died"))
df$APRDRG_Risk_Mortality <- factor(df$APRDRG_Risk_Mortality,levels = c(0,1,2,3,4), 
                                            labels = c("Not specified","Minor Likelihood","Moderate Likelihood","Major Likelihood","Extreme Likelihood"))
df$APRDRG_Severity <- factor(df$APRDRG_Severity,
                                      levels = c(0,1,2,3,4),
                                      labels = c("Not specified","Minor Loss of Function","Moderate Loss of Function","Major Loss of Function","Extreme Loss of Function"))
df$NEOMAT<-factor(df$NEOMAT)

df$ORPROC<-factor(df$ORPROC)
df$PAY1 <- factor(df$PAY1,levels = c(1,2,3,4,5,6),labels = c("Medicare","Medicaid","Private","Self-Pay","No Charge","Other"))
df$RACE <- factor(df$RACE,levels = c(1,2,3,4,5,6),labels = c("White","Black","Hispanic","Asian","Native American","Other"))
df$TRAN_IN<-factor(df$TRAN_IN)
df$TRAN_OUT<-factor(df$TRAN_OUT)
df$ZIPINC_QRTL<-factor(df$ZIPINC_QRTL)
df$FEMALE<-factor(df$FEMALE, levels=c(0,1))
severity_factors <- c("CM_AIDS","CM_ALCOHOL","CM_ANEMDEF","CM_ARTH","CM_BLDLOSS","CM_COAG","CM_DEPRESS","CM_DM", "CM_DMCX","CM_DRUG","CM_HYPOTHY","CM_LIVER","CM_LYMPH","CM_METS","CM_OBESE","CM_PARA","CM_PERIVASC","CM_PSYCH","CM_PULMCIRC","CM_RENLFAIL","CM_TUMOR","CM_VALVE","CM_WGHTLOSS","NEOMAT")
df[severity_factors] <- lapply(df[severity_factors],factor)
```

```{r include=FALSE}
#str(df)
df$AGE<- as.numeric(df$AGE)
df$LOS<- as.numeric(df$LOS)

```


```{r include=FALSE}
df <- df[complete.cases(df),]
```

# Introduction

[The Healthcare Cost and Utilization Project](https://www.hcup-us.ahrq.gov/overview.jsp)(HCUP) provides the opportunity for state data organizations, hospital associations, private data organizations and the federal government to help create a massive family of healthcare databases that can help data analysts to investigate health policy isues such as health services' cost and quality, medical practice patterns, healthcare programs, and treatment outcomes from local market levels all the way to the national market level. In 1998, HCUP started merging all of this data to create what is known as the National Impatient Sample (NIS) data. The NIS contains key patient information such as patient demographics, length of stay at the hospital, and disease classifications, transfer status, and most importantly, risk of mortality. **Using this valuable information, it becomes possible to build a machine learning model to predict patient mortality and even determine what key factors contribute to patient death while hospitalized.** This model could help potentially mitigate patient mortality and help create health policies to better patient life.<br />

## Dataset Information 
For this model, we will be using the NIS data set from 2012, which contains 200,000 randomly selected patients from all over the US. The data itself is an aggregation of discharge records from all participating HCUP hospitals. In order to predict our outcome of interest, which is patient mortality during hospitalization or the **DIED** feature, we will use the following available features:<br />
  **AGE:**Age in years at admission coded 0-124 years<br />
  **APRDRG_Risk_Mortality:**All Patient Refined DRG: Risk of Mortality Subclass:<br /> 
                            (0) No class specified<br />
                            (1) Minor likelihood of dying<br />
                            (2) Moderate likelihood of dying<br />
                            (3) Major likelihood of dying<br />
                            (4) Extreme likelihood of dying<br />
                            
  **APRDRG_Severity:**All Patient Refined DRG: Severity of Illness Subclass:<br />
                            (0) No class specified<br />
                            (1) Minor loss of function (includes cases with no comorbidity or complications)<br />
                            (2) Moderate loss of function<br />
                            (3) Major loss of function<br /> 
                            (4) Extreme loss of function<br />      
  **CM_AIDS:**AHRQ comorbidity measure: Acquired immune deficiency syndrome<br />               
  **CM_ALCOHOL:**AHRQ comorbidity measure: Alcohol abuse<br />           
  **CM_ANEMDEF:**AHRQ comorbidity measure: Deficiency anemias<br />            
  **CM_ARTH:**AHRQ comorbidity measure: Rheumatoid arthritis/collagen vascular diseases<br />               
  **CM_BLDLOSS:**AHRQ comorbidity measure: Chronic blood loss anemia <br />           
  **CM_COAG:**AHRQ comorbidity measure: Coagulopathy<br />               
  **CM_DEPRESS:**AHRQ comorbidity measure: Depression<br />         
  **CM_DM:**AHRQ comorbidity measure: Diabetes, uncomplicated<br />                
  **CM_DMCX:**AHRQ comorbidity measure: Diabetes with chronic complications<br />              
  **CM_DRUG:**AHRQ comorbidity measure: Drug abuse<br />                
  **CM_HYPOTHY:** AHRQ comorbidity measure: Hypothyroidism<br />            
  **CM_LIVER:**AHRQ comorbidity measure: Hypothyroidism<br />             
  **CM_LYMPH:**AHRQ comorbidity measure: Lymphoma<br />              
  **CM_METS:**AHRQ comorbidity measure: Metastatic cancer<br />               
  **CM_OBESE:**AHRQ comorbidity measure: Obesity<br />              
  **CM_PARA:** AHRQ comorbidity measure: Paralysis<br />              
  **CM_PERIVASC:**AHRQ comorbidity measure: Peripheral vascular disorders<br />           
  **CM_PSYCH:**AHRQ comorbidity measure: Psychoses<br />               
  **CM_PULMCIRC:**AHRQ comorbidity measure: Pulmonary circulation disorders<br />          
  **CM_RENLFAIL:**AHRQ comorbidity measure: Renal failure<br />           
  **CM_TUMOR:**AHRQ comorbidity measure: Solid tumor without metastasis<br />             
  **CM_VALVE:** AHRQ comorbidity measure: Valvular disease<br />              
  **CM_WGHTLOSS:**AHRQ comorbidity measure: Weight loss<br />            
  **DIED:**Indicates in-hospital death:<br /> 
                            (0) did not die during hospitalization<br />
                            (1) died during hospitalization<br />                 
  **FEMALE:**Indicates gender for NIS beginning in 1998:<br /> 
                            (0) male<br />
                            (1) female<br />                
  **HOSP_DIVISION:**Census Division of hospital (STRATA):<br /> 
                            (1) New England<br />
                            (2) Middle Atlantic<br />
                            (3) East North Central<br />
                            (4) West North Central<br />
                            (5) South Atlantic<br />
                            (6) East South Central<br />
                            (7) West South Central<br /> 
                            (8) Mountain<br />
                            (9) Pacific<br />         
  **LOS:**Length of stay, edited<br />                   
  **NCHRONIC:**Number of chronic conditions<br />              
  **NDX:**Number of diagnoses coded on the original record<br />                  
  **NEOMAT:**Assigned from diagnoses and procedure codes:<br />
                            (0) not maternal or neonatal<br />
                            (1) maternal diagnosis or procedure
                            (2) neonatal diagnosis<br />
  **ORPROC:**Major operating room procedure indicator: 
                            (0) no major operating room procedure<br />
                            (1) major operating room procedure<br />               
  **PAY1:**Expected primary payer, uniform:<br />
                            (1) Medicare<br />
                            (2) Medicaid<br />
                            (3) private,including HMO<br />
                            (4) self-pay<br />
                            (5) no charge<br />
                            (6) other<br />                  
  **RACE:**Race, uniform coding:<br />
                            (1) white<br />
                            (2) black<br />
                            (3) Hispanic<br />
                            (4) Asian or Pacific Islander<br />
                            (5) Native American<br />
                            (6) other<br />                 
  **TRAN_IN:**Transfer in Indicator:<br /> 
                            (0) not a transfer<br />
                            (1) transferred in from a different acute care hospital [ATYPE NE 4 & (ASOURCE=2 or POO=4)]<br />
                            (2) transferred in from another type of health facility [ATYPE NE 4 & (ASOURCE=3 or POO=5,6)]<br />
               
  **TRAN_OUT:**Transfer out Indicator:<br />
                            (0) not a transfer<br />
                            (1) transferred out to a different acute care hospital<br />
                            (2) transferred out to another type of health facility<br />              
  **YEAR:**Discharge year<br />                  
  **ZIPINC_QRTL:**Median household income national quartiles for patient's ZIP Code<br />           
  
Using these features, it is possible to build a model that can better patient care and pinpoint what could cause an increased patient mortality during hospitalization and potentially prevent patient death.<br />


# Methods
## Data Cleaning 
After data was loaded, relevant columns that would be deemed relevant to the model were selected from the original 175 features, reducing the number of features to 40. Then the data set itself was cleaned by examining features to determine if there were any odd values in the features such as were present in features like **DIED**, **FEMALE**, and **RACE**. From here, each feature was investigated to determine whether they could stay as their current type, or be converted into either numeric, factor, character, or integer. Finally, all NA and missing values were removed from the data set reducing the number of samples from 200,000 samples to `r dim(df)[1]`. From the data cleaning process and as shown in the bar plot below, it became evident that there was a large imbalance between the **majority class (Alive)** and the **minority class (Died)**, which meant that all scoring measures must be calculated using the **Area Under the Curve** metric as classification error can be heavily skewed with such large imbalances. <br />
```{r}
ggplot(df, aes(x="DIED",fill=df$DIED)) + geom_bar()
```


## Modeling 
For the modeling part of this analysis, I chose 3 different models:<br />
  **1) Logistic Regression**<br />
  **2) Random Forest**<br />
  **3) Lasso Regression**<br />

### Logistic Regression
The Logistic Regression model was chosen because it is the baseline binary classifier in machine learning. Given that the outcome variable, **DIED**, is a binary variable, the use case of the Logistic Regression Model is thus justified. For the model, forward selection was employed to determine which features of the selected features should be used to give the best logistic regression model. The forward selection feature analysis found that the only relevant feature was the **APRDRG_Risk_Mortality** feature, which essentially is the patient mortality risk. Because this was the only feature deemed significant, I chose to include the following patient demographic features:<br />
    **1) AGE**<br />
    **2) RACE**<br />
    **3) LOS**<br />
    **4) FEMALE** <br />
These features were included as with only one feature, the logistic regression model was at risk of having a high bias. Once feature selection was completed, hyper parameter tuning was employed on the **epsilon** and the **maxit** parameters of the Logistic Regression Model to determine the optimal parameters. In order to further mitigate the high bias risk factor, I employed cross validation (cv=5) on both the feature selection and the hyper parameter tuning process. AUC was employed as the scoring metric due to the imbalanced classes in the target outcome variable.<br />

### Random Forest
The Random Forest Model was chosen due to its high usage in binary classification problems and its ability to tune the parameters of the model accordingly to the importance of the features. In this model, all features were used due to this capability. In addition to Random Forest, a bagging Random forest model was employed to reduce the risk of over fitting. AUC was employed as the scoring metric due to the imbalanced classes in the target outcome variable.<br />


### Lasso Regression 
The Lasso Regression Model was chosen over other glmnet models such as RIDGE Regression, as they use all of the features, which would increase the risk of an over fitted model. Furthermore, as was shown in the Logistic Regression model, not all features were significant to predicting patient mortality, furthering the justification of the Lasso Regression Model Use. Due to its inherent need to minimize the cost function, the Lasso Regression model will automatically select features that is deems significant, thus preventing the need to use a feature selection algorithm.  In order to further mitigate the high bias risk factor, I employed cross validation (cv=5) on the resampling method. AUC was employed as the scoring metric due to the imbalanced classes in the target outcome variable.<br />

Once all models were created, the AUC scores from all models were calculated to compare performance to choose the best model. <br />



```{r include=FALSE}
set.seed(202)
# create classification task
nis.tsk <- as_task_classif(df, target = "DIED", id = "NIS", positive="Died")

# 70/30 training testing split, ensuring appropriate case/control ratios
holdout.desc <- rsmp("holdout", ratio = 0.7)
nis.tsk$col_roles$stratum <- nis.tsk$target_names
holdout.desc$instantiate(nis.tsk)

# extract training and testing sets
train <- holdout.desc$train_set(1)
test  <- holdout.desc$test_set(1)
intersect(train, test) # check that the train and test set are unique

```


# Results
## Logistic Regression:<br />

```{r include=FALSE}
logreg.lrn  <- lrn("classif.log_reg")
logreg.lrn$predict_type <- "prob"
# create parameter space to search and define resolution

logreg.forward.lrn = AutoFSelector$new(
  learner = logreg.lrn,
  resampling = rsmp("cv", folds=5),
  measure = msr("classif.auc"),
  #terminator = trm("evals", n_evals=100),
  terminator = trm("stagnation", threshold=0.01),
  fselector = fs("sequential", strategy = "sfs")
)

# run forward selection algorithm
logreg.forward.lrn$train(nis.tsk)

# forward selection chosen model
logreg.forward.lrn$fselect_result

# AUC of forward selection
logreg.forward.lrn$fselect_result$classif.auc

```

 
```{r include=FALSE}
df_log_reg<-df[c("APRDRG_Risk_Mortality","AGE","LOS","FEMALE","RACE","DIED")]


nis.tsk <- as_task_classif(df_log_reg, target = "DIED", id = "NIS", positive="Died")

# 70/30 training testing split, ensuring appropriate case/control ratios
holdout.desc <- rsmp("holdout", ratio = 0.7)
nis.tsk$col_roles$stratum <- nis.tsk$target_names
holdout.desc$instantiate(nis.tsk)

# extract training and testing sets
train <- holdout.desc$train_set(1)
test  <- holdout.desc$test_set(1)
intersect(train, test) # check that the train and test set are unique

logreg.lrn  <- lrn("classif.log_reg")
logreg.lrn$predict_type <- "prob"
# create the autotuner
ps<- ps(epsilon = p_dbl(lower=0.00000001, upper=10),
         maxit = p_int(lower=25, upper=50))
tuner <- tnr("grid_search", resolution = 5)
nis.at = AutoTuner$new(
  learner = logreg.lrn,
  resampling = rsmp("cv", folds=5), # 5-fold CV seems reasonable given the size of the training data
  measure = msr("classif.auc"),
  search_space = ps,
  terminator = trm("none"), # search the entire parameter grid
  tuner = tuner
)

# tune and print results
nis.at$train(nis.tsk, row_ids = train)
nis.at$tuning_result %>% knitr::kable()
```


```{r include=FALSE}
logreg_best <- lrn("classif.log_reg", 
               epsilon=nis.at$tuning_result$epsilon,
               maxit=nis.at$tuning_result$maxit)
logreg_best$predict_type <- "prob"

# train tuner and predict
logreg_best$train(nis.tsk, row_ids = train)
logreg_pred <- logreg_best$predict(nis.tsk, row_ids = test)
logreg_pred$confusion

# plot ROC
autoplot(logreg_pred, type="roc")
logreg_pred$score(msr("classif.auc"))
```


```{r}
nis.at$tuning_result %>% knitr::kable(caption="Optimal Parameters for Logistic Regression")
```

From the hyper parameter tuning, the optimal parameters were found to be **epsilon=0** and a **maxit=50**.<br />
```{r}
data.frame(model=c("Training","Test"), AUC_Scores=c(nis.at$tuning_result$classif.auc,logreg_pred$score(msr("classif.auc")))) %>% knitr::kable(caption="Training and Test AUC for Logistic Regression")
```

The Training AUC Score was `r nis.at$tuning_result$classif.auc ` while the Test AUC Score was `r logreg_pred$score(msr("classif.auc"))`. <br />
```{r}
autoplot(logreg_pred, type="roc")
```

The ROC Curve for the Test Score is shown above.<br />

## Random Forest:
Random Forest Alone:<br />

```{r include=FALSE}
set.seed(202)
rf = randomForest(DIED ~ . , data = df[train,],
                        mtry=dim(df[train,])[2]-1,
                        strata = df$DIED[train],
                        sampsize = as.vector(table(df$DIED[train]))) 
```
The Random Forest model had an error rate of `r sum(rf$err.rate[,1])`.<br />
```{r}
varImpPlot(rf, cex.lab=1.5, cex.axis=2, cex=1.3, 
           n.var=ncol(df)-29, main="Random Forest", pch=16, col='red4')
```
Above is the Variable Importance Plot. Like the Logistic Regression Model, the Random Forest Model deemed the APRDRG_Risk_Mortality Feature as the most important variable. Only features with a score of above 100 were selected for the plot.<br />
```{r}
importance(rf)[order(importance(rf)[,1], decreasing = TRUE),]%>% knitr::kable(caption="Variable Importance Scores for Random Forest")
```
Above is the Importance Variable Table with the full list of variables and there importance scores. As shown in the Variable Importance plot, the APRDRG_Risk_Mortality feature has the highest score with a value of `r importance(rf)[order(importance(rf)[,1], decreasing = TRUE),][1]`. Notably, the APRDRG_Severity was not the second highest while the LOS or length of stay feature was ranked with a score of `r importance(rf)[order(importance(rf)[,1], decreasing = TRUE),][2]`.<br />

Random Forest with Bagging<br />
```{r include=FALSE}

rf_bg = randomForest(DIED ~ . , data = df[train,],
                        strata = df$DIED[train],
                        sampsize = as.vector(table(df$DIED[train]))) 
```
The Random Forest Bagging model had an error rate of `sum(rf_bg$err.rate[,1])`.<br />
```{r }
varImpPlot(rf_bg, cex.lab=1.5, cex.axis=2, cex=1.3, 
           n.var=ncol(df)-29, main="Bagging", pch=16, col='red4')

```
Above is the Variable Importance Plot. Unlike the Logistic Regression Model and the Random Forest Model, the Random Forest Bagging Model deemed the LOS or length of stay Feature as the most important variable. Only features with a score of above 100 were selected for the plot.<br />

```{r}
importance(rf_bg)[order(importance(rf_bg)[,1], decreasing = TRUE),] %>% knitr::kable(caption="Variable Importance Scores for Random Forest Bagging")
```

Above is the Importance Variable Table with the full list of variables and there importance scores. As shown in the Variable Importance plot, the LOS feature has the highest score with a value of `r importance(rf)[order(importance(rf)[,1], decreasing = TRUE),][1]`. Again, the APRDRG_Severity feature was not the second highest while the APRDRG_Risk_Mortality feature was ranked with a score of `r importance(rf)[order(importance(rf)[,1], decreasing = TRUE),][2]`. <br />


```{r}
rf_predict = predict(rf, newdata = df[test, ], 
                              type='response')

roc_test = roc(df$DIED[test], as.numeric(rf_predict)-1) 

auc(roc_test)
```

The Test AUC Score for the Random Forest Model is shown above.<br />

```{r}
rf_bg_predict = predict(rf_bg, newdata = df[test, ], 
                              type='response')

roc_test = roc(df$DIED[test], as.numeric(rf_bg_predict)-1) 
auc(roc_test)
```

The Test AUC Score for the Random Forest Model is shown above.<br />



## LASSO Regression:
```{r include=FALSE}
df_lasso<-df
df_lasso[sapply(df_lasso, is.factor)] <- lapply(df_lasso[sapply(df_lasso, is.factor)], 
                                       as.numeric)
df_lasso$DIED<-factor(df_lasso$DIED)
nis.tsk <- as_task_classif(df_lasso, target = "DIED", id = "NIS", positive="2")


# 70/30 training testing split, ensuring appropriate case/control ratios
holdout.desc <- rsmp("holdout", ratio = 0.7)
nis.tsk$col_roles$stratum <- nis.tsk$target_names
holdout.desc$instantiate(nis.tsk)

# extract training and testing sets
train <- holdout.desc$train_set(1)
test  <- holdout.desc$test_set(1)
intersect(train, test) # check that the train and test set are unique
lasso.lrn  <- lrn("classif.cv_glmnet",nfolds=5,alpha=1,type.measure = "auc") 
lasso.lrn$predict_type <- "prob"
lasso.lrn$train(nis.tsk, row_ids = train)
lasso_pred <- lasso.lrn$predict(nis.tsk, row_ids = test)
```

The Training AUC Score for the Lasso Regression Model was `r max(lasso.lrn$model$cvm)` while the Test AUC Score was `r lasso_pred$score(msr("classif.auc"))`.<br />
```{r}
autoplot(lasso.lrn, type="roc")
```
The ROC Curve for the Test Score of the Random Forest Bagging Model is shown above.<br /> 


# Conclusion 

From the models above, the best model by far was the Lasso Regression Model as it had the highest training and test AUC out of the three models proposed. The worst performing model by far was the Random Forest Bagging Model which is probably due to the imbalanced data set. Most of these models treated the patient's mortality risk or the APRDRG_Risk_Mortality, as the priority feature to predict patient mortality, while including variables like the APRDRG_Severity, Length of Stay, and Race, and Age. The Logistic Regression Model, while being second in terms of model performance, was probably affected by the forward feature selection which reduced our number of features to a singular feature. While I did train and test the model using the mortality risk and demographics, this doesn't change the fact that the model itself was limited in terms of the number of features examined. The Random Forest models performed very poorly due to the imbalanced data sets. Even with Bagging added to the model, the performance was still poor, even dropping performance below the standard Random Forest Model. The Lasso Regression Model probably worked the best due to its ability to select features on its own and determine their significance to the targeted outcome.<br /> 
In the future, it would be best to work with a balanced data set or even balance this current data set via methods like oversampling, under-sampling or even using the SMOTE algorithm to account for imbalance. Moreover, it would be interesting to explore the other features left out from this study and see if any other feature would be a better predictor than than the patient mortality risk feature.<br />
Overall, using the Lasso Regression Model is probably the best method to predict patient mortality. 







