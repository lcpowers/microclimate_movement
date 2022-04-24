## 
## logit(Survival) = a + b*size + c*(clim + e*aspect) + d*(clim + e*aspect)^2
##
## I will provide the parameters (a,b,c,d)
## set a and b so that I have the correct average survival rates
##   - Fit a regression to get a and b
##   - a and b are the same across sites, no local adaptation, external factors drive differences
##
## The way this is set up there will always be perfect + correlation in vrs between classes within sites/aspects. Across aspects there will be effects 
## Aspect is assumed to scale with 
## a = get from lm
## b = get from lm
## c = supply
## d = supply
## e = scales the effect of aspect
## 
## *maybe* make d constant cross all vital rates
##
## aspect is assigned
## size is assigned
## Climate pulled from random normal
## c and d are what dictates what happens on different aspects
##
##################################
###################################
##################################
##################################
# rm(list=ls())
# library(tidyverse)
# library(MASS)
# 
# logit2prob <- function(logit){
#   odds <- exp(logit)
#   prob <- odds / (1 + odds)
#   return(prob)
# }
 
aspects = c("north","south","top")
szs = c(2,10,15)
survs = c(0.01,0.45,0.75)

#### Getting a and b
ab_lm = glm(survs~szs,family = binomial)
a = ab_lm$coefficients[1]  %>% as.numeric()
b = ab_lm$coefficients[2]  %>% as.numeric()
c = rep(0,3)
d = rep(1,3)
e = c(1,1,1)
aspects = rep(0,3)

# clim = rnorm(1)
clim=0
aspect=0
survival = logit2prob(a + b*szs + c*(clim + e*aspect) + d*(clim + e*aspects)^2)
survival

# Look at clim vals
climdf = data.frame(#clim = rnorm(100),
                    clim=seq(-2,2,by=0.05),
                    S.s=NA,
                    S.j=NA,
                    S.a=NA,
                    c=0)

c = rep(0,3)
d = rep(1,3)

for(i in 1:nrow(climdf)){
  
  survival = logit2prob(a + b*szs - c*(climdf$clim[i] + e*aspects) - d*(climdf$clim[i] + e*aspects)^2)
  climdf[i,2:4] = ifelse(survival>0,survival,0)
  
}

ggplot(climdf,aes(x=clim))+
  geom_point(aes(y=S.s,color="seedling"))+
  geom_point(aes(y=S.j,color="juvenile"))+
  geom_point(aes(y=S.a,color="adult"))+
  theme_bw()+
  geom_hline(yintercept = 0)

##### Growth #####
grwths = c(0.05,0.55)

growth_lm = MASS::gls(grwths~szs[1:2])
g_a = growth_lm$coefficients[1] %>% as.numeric()
g_b = growth_lm$coefficients[2] %>% as.numeric()

# Look at clim vals
climdf = data.frame(#clim = rnorm(100),
  clim=seq(-2,2,by=0.05),
  G.s=NA,
  S.j=NA,
  S.a=NA,
  c=)

c = rep(0,3)
d = rep(1,3)

for(i in 1:nrow(climdf)){
  
  survival = logit2prob(a + b*szs - c*(climdf$clim[i] + e*aspects) - d*(climdf$clim[i] + e*aspects)^2)
  climdf[i,2:4] = ifelse(survival>0,survival,0)
  
}
