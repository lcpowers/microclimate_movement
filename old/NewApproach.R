#' function to build big G mx
#' 
#' @param clim general climate parameter
#' @param aspect factor variable
#' @param e value to scale the effect of aspect
#' 
#'

build_mx_fun(clim,aspect,surv_e){
  
  # Make this an input?
  szs = c(5,10,15)
  
  ##### Survival #####
  # got this from glm in 'apr132022.R'
  surv_a = -4.750076
  surv_b = 0.413830
  surv_c = rep(0,3)
  surv_d = rep(1,3)
  
  survs = logit2prob(surv_a + surv_b*szs - surv_c*(clim + surv_e*aspect) - surv_d*(clim + surv_e*aspect)^2)
  ##### End Survival #####
  
  ##### Growth #####
  gro_a = -0.45
  gro_b = 0.1
  gro_c = rep(0,3)
  gro_d = rep(1,3)
  
  growths = gro_a + gro_b*szs[1:2] - gro_c*(clim + gro_e*aspect) - gro_d*(clim + gro_e*aspect)^2
  
}


surv_e = 1
clim = rnorm(1)
aspect = matrix(data=c(-1,0,0.5,
                        1,0,-0.5,
                        1,0,-0.5),
                nrow=3,byrow=T)
aspect = aspect[,3]


gro_e = 1


logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
