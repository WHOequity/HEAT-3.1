# © Copyright World Health Organization (WHO) 2016.
# This file is part of the Health Equity Assessment Toolkit (HEAT).
# HEAT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License Version 2 as published by
# the Free Software Foundation.
# 
# HEAT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with HEAT. If not, see http://www.gnu.org/licenses/.

######### Between-Groups Variance (BGV)
# Reference: Harper & Lynch (p.8)


bgv <- function(dat, bs=FALSE, bsiter = 200, simulated_results = NULL){
  # absolute, complex, non ordered, weighted, greater than 2, no logscale
  # region
  # TEST WITH dat <- disagdata.split[[4]][[1]]
  
  
  # TODO, need to figure out order and reference groups
  dat <- arrange(dat, subgroup_order)
  funcname <- as.character(match.call()[[1]]) # current function
  make_pieces(dat, funcname) # est, pop, se, ord, na_return
  
  badData <- apply_tests(dat, 
                         two_subgroups,
                         is_ordered,
                         missing_estimates,
                         missing_population,
                         #missing_natl_avg,
                         estimates_all_zero,
                         #natl_all_zero,
                         less_than2_subgroups)
  
 

  # git845 manual computation
  popsh <- pop / sum(pop)
  est_natl <- sum(popsh * est)
  
  if(any(badData) || is.na(est_natl)) return(na_return)
  
  inequal.bgv <- sum(popsh * (est - est_natl)^2)
  
  se.formula <- NA
  se.boot <- NA
  ci <- list(l = NA, u = NA)
  
  if(SEuseful){
    
    # Bootstrap SE
    if(bs){
      n <- bsiter
      inequal.boot.vals <- sapply(1:n, function(x){
        sum(popsh * (rnorm(length(est), est, se) - est_natl)^2)
      })
      se.boot <- sd(inequal.boot.vals)
    } 
    
    
    
    # Formula-based SE: provided by Ahmad Hosseinpoor (WHO, Geneva)
    # This formula is not going to be corrected if the bgv estimate is based on
    # a national_est for the weighted.mean
    weighted.mean <- sum(popsh * est)
    p2__1_p2__se4 <- (popsh^2)*((1-popsh)^2)*(se^4)
    s4 <- (popsh^4)*(se^4)
    s2 <- (popsh^2)*(se^2)
    p2se2__y_mu__2 <- (popsh^2)*(se^2)*((est-weighted.mean)^2)  ################
    
    se.formula <- sqrt(4*(sum(p2se2__y_mu__2))+2*(((sum(s2))^2)-sum(s4)+sum(p2__1_p2__se4)))
    ci <- conf.int.norm(inequal.bgv, se.formula)
  }# end SE required
  

  
  return(list(inequal.bgv=inequal.bgv, se.bgv.boot=se.boot,  se.bgv.formula=se.formula, 
              se.lowerci.bgv = ci$l, se.upperci.bgv = ci$u))  
}



