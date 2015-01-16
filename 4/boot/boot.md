---
title: "seeds.R"
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
library(lattice)
library(latticeExtra)
library(corrplot)
library(MASS)
library(e1071)
library(nnet)
library(boot)
library(stats)
```

Данные и модель взяты из предыдущей домашки.


```r
data.concrete <- read.csv("../data/Concrete_Data.csv",comment.char = "#")
data.concrete$ageFactor<-data.concrete$Age<90
data.concrete$waterFactor<-data.concrete$Water<190
summary(data.concrete)
```

```
##      Cement      BlastFurnaceSlag     FlyAsh           Water      
##  Min.   :102.0   Min.   :  0.0    Min.   :  0.00   Min.   :121.8  
##  1st Qu.:192.4   1st Qu.:  0.0    1st Qu.:  0.00   1st Qu.:164.9  
##  Median :272.9   Median : 22.0    Median :  0.00   Median :185.0  
##  Mean   :281.2   Mean   : 73.9    Mean   : 54.19   Mean   :181.6  
##  3rd Qu.:350.0   3rd Qu.:142.9    3rd Qu.:118.30   3rd Qu.:192.0  
##  Max.   :540.0   Max.   :359.4    Max.   :200.10   Max.   :247.0  
##  Superplasticizer CoarseAggregate  FineAggregate        Age        
##  Min.   : 0.000   Min.   : 801.0   Min.   :594.0   Min.   :  1.00  
##  1st Qu.: 0.000   1st Qu.: 932.0   1st Qu.:731.0   1st Qu.:  7.00  
##  Median : 6.400   Median : 968.0   Median :779.5   Median : 28.00  
##  Mean   : 6.205   Mean   : 972.9   Mean   :773.6   Mean   : 45.66  
##  3rd Qu.:10.200   3rd Qu.:1029.4   3rd Qu.:824.0   3rd Qu.: 56.00  
##  Max.   :32.200   Max.   :1145.0   Max.   :992.6   Max.   :365.00  
##  ConcreteCompressiveStrength ageFactor       waterFactor    
##  Min.   : 2.33               Mode :logical   Mode :logical  
##  1st Qu.:23.71               FALSE:190       FALSE:394      
##  Median :34.45               TRUE :840       TRUE :636      
##  Mean   :35.82               NA's :0         NA's :0        
##  3rd Qu.:46.13                                              
##  Max.   :82.60
```

```r
fit <- lm(formula = ConcreteCompressiveStrength ~ Cement + BlastFurnaceSlag + 
     FlyAsh + FineAggregate + waterFactor:log(Water) + ageFactor:log(Age), 
     data = data.concrete)
boot_fun <- function(data, subset, formula) {
	fit <- lm(formula, data = data[subset,])
	coef(fit)
}
b <- boot(data.concrete, boot_fun, R=999, formula = fit$call$formula)
b
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = data.concrete, statistic = boot_fun, R = 999, formula = fit$call$formula)
## 
## 
## Bootstrap Statistics :
##         original        bias     std. error
## t1* 142.81527901  1.160401e+00 27.343306743
## t2*   0.11413857  8.866978e-06  0.003509648
## t3*   0.08996676 -3.755607e-05  0.004134865
## t4*   0.06428721 -1.643467e-04  0.005015339
## t5*   0.01098539 -5.967325e-05  0.004363419
## t6* -36.07865544 -2.152548e-01  4.599614009
## t7* -35.78920656 -2.190838e-01  4.708485923
## t8*   8.68720704  1.647143e-03  0.187028330
## t9*   9.25150121  1.095595e-02  0.239313408
```

Посмотрим на нормальность оценок.


```r
for(i in 1:6){
    plot(b, index=i)
}
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-2.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-3.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-4.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-5.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-6.png) 

Всё выглядит нормальным.


```r
for(i in 1:6){
    print(boot.ci(b, type="norm",index=i))
}
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 999 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = b, type = "norm", index = i)
## 
## Intervals : 
## Level      Normal        
## 95%   ( 88.1, 195.2 )  
## Calculations and Intervals on Original Scale
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 999 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = b, type = "norm", index = i)
## 
## Intervals : 
## Level      Normal        
## 95%   ( 0.1073,  0.1210 )  
## Calculations and Intervals on Original Scale
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 999 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = b, type = "norm", index = i)
## 
## Intervals : 
## Level      Normal        
## 95%   ( 0.0819,  0.0981 )  
## Calculations and Intervals on Original Scale
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 999 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = b, type = "norm", index = i)
## 
## Intervals : 
## Level      Normal        
## 95%   ( 0.0546,  0.0743 )  
## Calculations and Intervals on Original Scale
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 999 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = b, type = "norm", index = i)
## 
## Intervals : 
## Level      Normal        
## 95%   ( 0.0025,  0.0196 )  
## Calculations and Intervals on Original Scale
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 999 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = b, type = "norm", index = i)
## 
## Intervals : 
## Level      Normal        
## 95%   (-44.88, -26.85 )  
## Calculations and Intervals on Original Scale
```
