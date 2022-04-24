#'  Generic matrix function for short-lived individuals
#'  
#' @param shortlived default = TRUE
#' @return transition matrix for a population
#' 

mx3by3 <- function(shortlived=TRUE){
    
  mx <- matrix(data = 0, nrow = 3, ncol = 3)
  
  r.vr.options = c("Sseed","Ssdlg","Sjuv","Sadult","Gseed","Gsdlg","Gjuv")
  
  # if(!random.vr%in%r.vr.options){
  #   stop("bad random.vr")
  # }
  
  ##### Survival values #####
  Sseed = 0.05
  Ssdlg = 0.2
  
  # Change juv and adult survivorship to make short- and long-lived matrices
  if(shortlived){
    Sjuv=0.45 ; Sadult=0.55
  } else {
    Sjuv=0.65 ; Sadult=0.75
  }
  
  ##### Growth values #####
  Gseed = 0.15
  Gsdlg = 0.35
  Gjuv = 0.55
  
  ##### Fill in matrix #####
  mx[2,1] = Ssdlg*Gsdlg
  mx[2,2] = Sjuv*(1-Gjuv)
  mx[3,2] = Sjuv*Gjuv
  mx[3,3] = Sadult

  # Find F value that gets lambda near 1
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
  return(mx)
}


### To-do: Add in random variation
