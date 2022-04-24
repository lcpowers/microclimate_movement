#'  Generic matrix function for short-lived individuals (will extend to long lived eventually)
#'  
#' @param shortlived default = TRUE
#' @return transition matrix for a population
#' 

mx3by3 <- function(shortlived=TRUE){

  ## if loop for short-lived matrix
  if(shortlived==T){
    
    mx <- matrix(data = 0, nrow = 3, ncol = 3)
    
    Sseed = rexp(1,rate=20)
    Ssdlg = runif(1,min=0.1,max=0.3)
    Sjuv = runif(1,min=0.3,max=0.5)
    Sadult = runif(1,min=0.4,max=0.6)
    
    Gseed = rnorm(1,mean=0.15,sd = 0.05) %>% abs()
    Gsdlg = rnorm(1,mean=0.35,sd = 0.1) %>% abs() 
    Gjuv = rnorm(1,mean=0.55,sd = 0.1) %>% abs()
    
    mx[2,1] = Ssdlg*Gsdlg
    
    mx[2,2] = Sjuv*(1-Gjuv)
    mx[3,2] = Sjuv*Gjuv
    
    mx[3,3] = Sadult

    optfun <- function(F){
      mx[1,3] <- F*Sadult*Sseed
      lam <- Re(eigen(mx)$values[1])
      x = (1-lam)^2
      return(x)
    }
    
    Fec <- optim(par = 500, fn = optfun,method = "L-BFGS-B", lower=0, upper=100000)
    Fpar <- round(Fec$par)
    
    mx[1,3] <- Fpar*Sadult*Sseed
    Re(eigen(mx)$values[1])
  }
  
  ## if for long-lived matrix
  if(shortlived==F){
    
    mx <- matrix(data = 0, nrow = 3, ncol = 3)
    
    Sseed = rexp(1,rate=20)
    Ssdlg = runif(1,min=0.1,max=0.3)
    Sjuv = runif(1,min=0.4,max=0.6)
    Sadult = runif(1,min=0.7,max=0.9)
    
    Gseed = rnorm(1,mean=0.15,sd = 0.05) %>% abs()
    Gsdlg = rnorm(1,mean=0.35,sd = 0.1) %>% abs() 
    Gjuv = rnorm(1,mean=0.55,sd = 0.1) %>% abs()
    
    mx[2,1] = Ssdlg*Gsdlg # Sdlg survival and growth to juv
    mx[2,2] = Sjuv*(1-Gjuv) # juv survival then stay juv
    mx[3,2] = Sjuv*Gjuv # juv survival then grow to adult
    
    mx[3,3] = Sadult
    
    optfun <- function(F){
      mx[1,3] <- F*Sadult*Sseed
      lam <- Re(eigen(mx)$values[1])
      x = (1-lam)^2
      return(x)
    }
    
    Fec <- optim(par = 500, fn = optfun,method = "L-BFGS-B", lower=0, upper=100000)
    Fpar <- round(Fec$par)
    
    mx[1,3] <- Fpar*Sadult*Sseed
    
    Re(eigen(mx)$values[1])
    # eigen(mx)$vectors[,1]/sum(eigen(mx)$vectors[,1])
    
    }
  
  return(mx)
}
