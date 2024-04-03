
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bimm

<!-- badges: start -->

<!-- badges: end -->

The goal of `bimm` is to provide an alternative modeling framework for generalized linear mixed models (GLMMs) for binary outcomes. BiMM stands for Binary Mixed Model and the algorithm can be used in conjunction with any type of classifier that produces a predicted probability. BiMM is analogous to the Expectation-Maximization algorithm. In the first step, a classifier is used to predict the binary outcome (ignoring clustering) and the predicted probability is then included in a Bayesian GLMM to account for clustering. In the second step, the predicted probability from the Bayesian GLMM is added to the binary outcome and a split function is applied to update the outcome as a binary variable. The first and second step are repeated until the posterior log likelihood from the Bayesian GLMM is less than a specified tolerance value. Alternatively, one iteration could be used in the BiMM framework (i.e. the first and second step are employed only once and no iterations are repeated). Predictions for observations included in the training data are made using the mixed effects regression model. Predictions for observations not in the training data (i.e., the test data) are made using only the classifier.

We have investigated BiMM with random forest in a method called BiMM forest. BiMM forest is a random forest method for modeling clustered and longitudinal binary outcomes. The paper can be found here: <https://www.sciencedirect.com/science/article/pii/S0169743918304362>. Full citation: Speiser, Jaime Lynn, Bethany J. Wolf, Dongjun Chung, Constantine J. Karvellas, David G. Koch, and Valerie L. Durkalski. "BiMM forest: A random forest method for modeling clustered and longitudinal binary outcomes." Chemometrics and Intelligent Laboratory Systems 185 (2019): 122-134.

We have a second paper that includes variable selection as a pre-step for the BiMM forest method: Speiser, Jaime Lynn. "A random forest method with feature selection for developing medical prediction models with clustered and longitudinal data." Journal of biomedical informatics 117 (2021): 103763. <https://www.sciencedirect.com/science/article/pii/S1532046421000927>

There are a few differences in the `bimm` package from the papers. First, `bimm` uses ranger to employ random forest, whereas our original BiMM forest method used the R package randomForest. We did this to increase computational efficiency. Second, the `bimm` package only includes one iteration and H3 updates. We did not include H1 or H2 updating functions in the `bimm` package, as they did not seem to improve performance compared to one iteration or H3. Third, we do not include code for variable selection in the `bimm` package, as these can be done as a pre-step. Fourth, we note that we previously developed BiMM tree, a decision tree method with the BiMM algorithm, but this is not included in the package due to its worse perforamnce compared to BiMM forest.  

## Installation

You can install the released version of `bimm` from
[GitHub](https://github.com/) with:

``` r
library(devtools)
install_github("bcjaeger/bimm")
```

## Example

The `bimm` packages requires binary, repeated/clustered outcomes and complete data (i.e., no missing values). 

``` r
library(bimm)
```

We use the hospital dataset in the `bimm` package and split it into training and testing data by patient ID (DID). 

``` r
data('hospital', package = 'bimm')

index_train <- sample(unique(hospital$DID), size = 250)

data_train <- hospital[hospital$DID %in% index_train, ]

data_test <- hospital[!(hospital$DID %in% index_train), ] 
```

First, we fit BiMM forest models: a one iteration BiMM forest and a BiMM forest with updates. Using verbose = TRUE, the iterations will be printed out. We can also print and summarize the models. 

``` r
#BiMM forest with one iteration
model1 <- bimm_fit(data = data_train,test=data_test,
                  formula = remission ~ . + (1 | DID),
                  n_iteration = 1)

print(model1)
summary(model1)

#BiMM forest with H3 updates
model2 <- bimm_fit(data = data_train,test=data_test,
                  formula = remission ~ . + (1 | DID),
                  verbose = TRUE)

print(model2)
summary(model2)
```

We can use the BiMM forest model to obtain predictions for test datasets of two types (new subjects: new_sub, not included in the training data; or new observations, with other observations from the cluster included in the training data). Here, we show how to get predictions for new subjects. 

``` r
preds_test1<-bimm_predict(model1, new_data=data_test,type="new_sub")
```

These predictions can then be used to calculate performance metrics such as AUC, sensitivity, specificity, NPV, PPV, F1, Brier, calibration, etc. 


