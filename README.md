# LoD-Calculator
---
title: "Untitled"
author: "Huizi Wang"
date: "4/6/2022"
output: html_document
---


```{r}
# set up a path
setwd("clipboard")
source("LOD Function.R")

# generate X which is numeric vector
X<- c(rnorm(10, 2, 10))

# generate Y which is numeric vector
Y<- c(rnorm(10, 100, 500))

# apply the 'limit' function
limit(X, Y)
```
