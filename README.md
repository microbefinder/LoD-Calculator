# LoD-Calculator
---
title: "LoD-Calculator"
author: "Huizi Wang"
date: "4/6/2022"
output: html_document
---


```{r}
# set up a path
setwd("clipboard")
source("LoD Function.R")

# generate X which is numeric vector
X<- c(rnorm(10, 2, 10))

# generate Y which is numeric vector
Y<- c(rnorm(10, 100, 500))

# apply the 'limit' function
limit(X, Y)
```


# Detail of LoD Function 
---
title: "LoD Function"
author: "Huizi Wang"
date: "4/28/2022"
output: html_document
---


```{r}
limit<- function(X, Y){
  mu1=mean(X)
  var1=var(X)
  mu2=mean(Y)
  var2=var(Y)
  LOD_50=((mu2/var2-mu1/var1)-sqrt((((mu1-mu2)^2)/(var1*var2))-(1/var2-1/var1)*2*log(sqrt(var2/var1))))/(1/var2-1/var1)
  
  par(mfrow=c(1,2))
  
  Box_Plot = boxplot(X, Y, names = c("Negative", "Positive"), horizontal = F, ylab = "Total Score")
  abline(h=as.numeric(LOD_50), col="red")
  
  prob=function(x){((1/sqrt(var2))*exp(-((x-mu2)^2)/(2*var2)))/((1/sqrt(var2))*exp(-((x-mu2)^2)/(2*var2))+(1/sqrt(var1))*exp(-((x-mu1)^2)/(2*var1)))}
  Prob_Plot=curve( prob(x), xlim=c(LOD_50-min(mu1, mu2), LOD_50+min(mu1, mu2)), ylim=c(0, 1), xlab="Total Score", ylab="Probability of Pathogen Present", col="blue", lwd=2)
  abline(h=0.5, v=as.numeric(LOD_50), col="red")
  
  return(list(mean_of_N=mu1, variance_of_N=var1, mean_of_Y=mu2, variance_of_Y=var2, Limit_of_Detection=c(LOD_50), Box_Plot, Prob_Plot))
  
}
```


# Help File
---
title: "Help File"
author: "Huizi Wang"
date: "4/28/2022"
output: html_document
---


## limit: Calculating Limit of Detection (LoD) and Generating Plot

Description

‘limit’ is used to calculate limit of detection, specified by giving mean and variance for your input values, also generating box plot and probability curve with the certain pathogen. 


Usage

limit(X, Y)           


Arguments

X                                a numeric vector; Total Score (TS) for every observation with negative         
                                 diagnostic assays of PCR.                                             

Y                                a numeric vector; TS for every observation with positive diagnostic 
                                 assays of PCR.
                                              
Value

mean_of_N                        gives mean for the samples with negative diagnostic assays of PCR.
 
variance_of_N                    gives variance for the samples with negative diagnostic assays of PCR.

mean_of_Y                        gives mean for the samples with positive diagnostic assays of PCR.

variance_of_Y                    gives variance for the samples with positive diagnostic assays of PCR.

Limit_of_Detection               gives Limit of Detection (LoD) under 50% chance for getting disease.                         

Box_Plot                         gives box plot of X and Y. The red line indicates the LoD for a pathogen.
                                             
Prob_Plot                        gives the curve of the probability of getting a pathogen given a TS. The 
                                 red horizontal and vertical lines indicate the LoD and its probability. 


