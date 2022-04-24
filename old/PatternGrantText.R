rm(list=ls())
library(tidyverse)

# https://www.r-bloggers.com/2021/05/how-to-generate-correlated-data-in-r/
# https://cran.r-project.org/web/packages/faux/vignettes/rnorm_multi.html

##### Not sure where/if this comes in #####

# lam_s = stochastic growth rate of the population
# lam_n and lam_s = net annual growth rates for near and far regions
# rho is correlation between demographic rates for near and far
# S = deterministic sensitivity of population lambda to rate i


# log(lam_s) =~ 
#                log(lam_1) # Net annual growth rate for all regions
#                - 0.5*(lam_1^-2)* # 0.5 because 1/n from JE?
#                (var_lam_near*S_near^2  
#                 + var_lam_far*S_far^2 
#                 + 2*rho*sd_near*S_near*sd_far*S_far)
################################


## Variables for 
## - Number of populations
## - Correlation of vital rates among populations
## 

#### Set parameters ####

## matrix dimensions
matdim <- 4

## Number of reproductive classes
fclasses <- 1

## number of populations
n <- 3

## Create lists vital rates for each population ##
for(i in 1:n){
  
  pop <- letters[i]
  svec <- abs(rnorm(matdim,mean=0.5,sd=0.2)) %>% round(.,4) # very arbitrary right now
  gvec <- abs(rnorm(matdim-2,mean=0.2,sd=0.1) )%>% round(.,4) 
  fvec <- abs(rnorm(fclasses,mean=20,sd=5)) %>% round()
  vitalrates <- list(svec=svec,gvec=gvec,fvec=fvec)
  assign(paste0(pop,'_rates'),vitalrates)
  rm(pop,svec,gvec,fvec,vitalrates)
  
}

#### Construct matrices for each population ####
matlist <- list()

for(i in 1:n){
  
  pop <- letters[i]
  rates <- eval(as.name(paste0(pop,"_rates")))
  
  tmp <- matrix(0,matdim,matdim)
  
  #### Fill in fecundity ####
  tmp[1,4] = rates$fvec
  
  #### Fill in survival ####
  # First class survival value
  tmp[2,1] = rates$svec[1]
  # Last class survival value
  tmp[matdim,matdim] = rates$svec[matdim]
  
  # Intermediate class survival values
  for(j in 2:(matdim-1)){
   
    tmp[j,j] = rates$svec[j]
    tmp[j+1,j] = rates$svec[j]
    
  }
  ## End survival loop ##
  
  #### Fill in growth ####
  for(k in 2:(length(rates$gvec)+1)){
  
    tmp[k,k] = round(tmp[k,k]*(1-rates$gvec[k-1]),4)
    tmp[k+1,k] = round(tmp[k,k]*rates$gvec[k-1],4)
    
  } ## End growth loop ##
  
  #### Add matrix to the list of population matrices ####
  matlist[[LETTERS[i]]] <- tmp
  
  rm(pop,rates,tmp)
} ## End pop matrix construction loop ##


#### Create grand matrix G with individual population matrices on diagonal, zero matrices in all other positions ####

zeromat <- matrix(0,matdim,matdim)
G <- list(zeromat) %>% matrix(.,nrow=n,ncol=n)
for(i in 1:n){
  G[i,i] <- matlist[i]
}




library(compadre)





