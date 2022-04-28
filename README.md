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
source("LOD Function.R")

# generate X which is numeric vector
X<- c(rnorm(10, 2, 10))

# generate Y which is numeric vector
Y<- c(rnorm(10, 100, 500))

# apply the 'limit' function
limit(X, Y)
```


# Detail of LOD Function 
---
title: "LOD Function"
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
