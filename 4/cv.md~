---
title: "glaucomaMVF.R"
author: "Атамась Семён"
output:
  html_document:
    fig_caption: yes
    fig_height: 15
    fig_width: 15
    highlight: tango
    self_contained: no
    theme: spacelab
---


```r
library("knitr")
library("e1071")
library("lattice")
```
Функция для генерации данных.

```r
my.datagen <- function(sample.size, predictors.size) {
  data <- data.frame(replicate(predictors.size, runif(sample.size)))
  data$Y <- runif(sample.size)
  data
}
```
Функция для геерации модели.

```r
my.fn.gen.formula <- function(data) {
  corrs <- sort(sapply(names(data)[-length(data)], function(x) abs(cor(data$Y, data[[x]]))), decreasing=TRUE)
  predictors <- names(corrs)[1:20]
  as.formula(paste("Y ~ ", paste(predictors, collapse="+")))
}
```
Попытаемся обучиться и валидировать на одних и тех же данных

```r
my.data <- my.fn.gen.data(50, 1000)
my.model.formula <- my.fn.gen.formula(my.data)
my.model <- lm(my.model.formula, data=my.data)
summary(my.model)
```

```
## 
## Call:
## lm(formula = my.model.formula, data = my.data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.22608 -0.06856 -0.00816  0.06251  0.22516 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.812718   0.175555   4.629  7.1e-05 ***
## X893        -0.217723   0.099723  -2.183  0.03725 *  
## X503         0.064618   0.098152   0.658  0.51551    
## X685        -0.042805   0.088181  -0.485  0.63102    
## X647         0.139927   0.080441   1.739  0.09255 .  
## X246        -0.182442   0.091054  -2.004  0.05453 .  
## X869         0.050546   0.093061   0.543  0.59118    
## X982        -0.002993   0.099872  -0.030  0.97630    
## X174        -0.101962   0.096549  -1.056  0.29966    
## X198        -0.209119   0.088902  -2.352  0.02566 *  
## X960        -0.005438   0.103890  -0.052  0.95861    
## X372        -0.088153   0.072693  -1.213  0.23505    
## X599        -0.162433   0.077302  -2.101  0.04442 *  
## X502         0.055752   0.080247   0.695  0.49274    
## X418         0.011995   0.088091   0.136  0.89263    
## X178         0.151870   0.080973   1.876  0.07081 .  
## X692        -0.023129   0.083878  -0.276  0.78470    
## X255         0.100511   0.090342   1.113  0.27504    
## X944        -0.105081   0.101922  -1.031  0.31106    
## X475         0.124426   0.082901   1.501  0.14419    
## X325        -0.261554   0.085775  -3.049  0.00486 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.133 on 29 degrees of freedom
## Multiple R-squared:  0.8555,	Adjusted R-squared:  0.7559 
## F-statistic: 8.587 on 20 and 29 DF,  p-value: 1.847e-07
```

```r
tune(lm, my.model.formula, data=my.data)
```

```
## 
## Error estimation of 'lm' using 10-fold cross validation: 0.03198345
```
Получили достаточно маленькую ошибку, как и ожидалось.

Разделим на тренировочную и тестовую выборки.

```r
my.data2 <- my.fn.gen.data(100, 1000)
my.data2.train <- my.data2[1:50,]
my.data2.test  <- my.data2[-(1:50),]
my.model2.formula <- my.fn.gen.formula(my.data2.train)
my.model2 <- lm(my.model2.formula, data=my.data2.train)
summary(my.model2)
```

```
## 
## Call:
## lm(formula = my.model2.formula, data = my.data2.train)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.223337 -0.046044  0.001666  0.061253  0.245655 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept)  0.4021002  0.1979853   2.031  0.05152 . 
## X48          0.1547466  0.1084586   1.427  0.16432   
## X174         0.1479022  0.0796994   1.856  0.07368 . 
## X691        -0.1650109  0.0967303  -1.706  0.09872 . 
## X824        -0.2424412  0.0802107  -3.023  0.00520 **
## X317         0.0545910  0.0791924   0.689  0.49608   
## X527        -0.1821550  0.0881162  -2.067  0.04774 * 
## X576        -0.0636275  0.0816494  -0.779  0.44213   
## X904         0.1757420  0.0869703   2.021  0.05263 . 
## X120         0.0592876  0.0778227   0.762  0.45231   
## X703         0.0006334  0.0866087   0.007  0.99422   
## X413        -0.0542139  0.0898139  -0.604  0.55079   
## X472        -0.0211822  0.0841990  -0.252  0.80314   
## X983        -0.0812906  0.0830568  -0.979  0.33581   
## X42          0.2628643  0.0780742   3.367  0.00216 **
## X811        -0.2015256  0.0891714  -2.260  0.03151 * 
## X982         0.0281387  0.0865001   0.325  0.74729   
## X252         0.1996693  0.0900086   2.218  0.03452 * 
## X12         -0.1013495  0.0833235  -1.216  0.23366   
## X454        -0.0313184  0.0786799  -0.398  0.69351   
## X903         0.1297295  0.1032090   1.257  0.21880   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.132 on 29 degrees of freedom
## Multiple R-squared:  0.8685,	Adjusted R-squared:  0.7778 
## F-statistic: 9.574 on 20 and 29 DF,  p-value: 5.366e-08
```

```r
tune(lm, my.model2.formula, data=my.data2.test)
```

```
## 
## Error estimation of 'lm' using 10-fold cross validation: 0.1721125
```
Получили ошибку намного больше.
Проделаем нормальную кросс-валидацию

```r
tune(function(formula, data, subset, ...) {
  data <- data[subset,]
  lm(my.fn.gen.formula(data), data=data)
}, my.model.formula, data=my.data)
```

```
## 
## Error estimation of 'function(formula, data, subset, ...) {''    data <- data[subset, ]''    lm(my.fn.gen.formula(data), data = data)''}' using 10-fold cross validation: 0.09699621
```
