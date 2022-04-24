#'  Generic matrix function for short-lived individuals (will extend to long lived eventually)
#'  
#' @param Gmx input matrix without movement
#' @param m rate of movement
#' @return Input G mx modified to add movement between populations
#' 
 add_m_fun = function(Gmx,m,npops,dims=3){

  Gmx_out = Gmx
  
  # Create vector of cell index that is the [1,1] for each sub pop
  subpop11s = seq(1,npops*dims,by=dims)
  
  # Get number destination populations 
  mdenom = npops-1
  
  ## Loop to change origin pop F cells (movement away from origin subpop)
  for(i in 1:length(subpop11s)){
    
    # Reduce fecundity cells by 'm'
    Gmx_out[subpop11s[i],3*i] = Gmx[subpop11s[i],3*i]*(1-m)
  
    }
  
  ## Loop to change destination F cells (movement to subpops)
  for(i in 1:npops){
    
    # ID the [1,1] cells for other (not the one in the loop above) populations 
    subs = setdiff(subpop11s,subpop11s[i])
    
    for(sub in subs){
      
      # Add m value to corresponding fecundity cell for each other sub-pop (m/number of destination pops)
      Gmx_out[sub,3*i] = Gmx[subpop11s[i],3*i]*(m/mdenom)
    
      } # End subs loop
  } # End npops loop
  
  return(Gmx_out)

} # End function
