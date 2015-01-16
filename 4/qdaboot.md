---
title: "QDAboot"
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
library("mvtnorm")
library(boot)
```


```r
iris.data <- subset(iris, select = -Species)
means <- aggregate(iris.data, list(groups = iris$Species), mean)
means
```

```
##       groups Sepal.Length Sepal.Width Petal.Length Petal.Width
## 1     setosa        5.006       3.428        1.462       0.246
## 2 versicolor        5.936       2.770        4.260       1.326
## 3  virginica        6.588       2.974        5.552       2.026
```

```r
covatiations<-by(iris.data, iris$Species, cov)
model_list <- list(cov = covatiations, means = means[, -1, drop = FALSE])
```
