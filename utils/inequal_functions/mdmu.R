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

######### Mean Difference from the Mean (mdmu)


mdmu <- function(dat, bs=TRUE, bsiter = 200, simulated_results = NULL){
  # absolute, complex, non ordered, weighted, greater than two
  # region
  # TEST WITH dat <- disagdata.split[[4]][[1]]
  
  # TODO, need to figure out order and reference groups
  dat <- arrange(dat, subgroup_order)
  funcname <- as.character(match.call()[[1]]) # current function
  make_pieces(dat, funcname) # est, pop, se, ord, na_return
  estrand <- simulated_results$estrand
  
  
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
  
  est1 <- est / scaleval
  se1 <- se / scaleval
 
  
  inequal.mdmu <- sum(abs(est1 - sum(popsh * est1)))
  #inequal.mdmu <- sum(popsh * abs(est1 - sum(popsh * est1)))
  
  se.formula <- NA
  se.boot <- NA
  
  boot.lcl2 <- NA
  boot.ucl2 <- NA
  
  if(SEuseful){
    if(bs){
      
      inequal.boot.vals <- purrr::map_dbl(estrand, function(x){
        if ((scaleval == 100 & all(x >= 0) & all(x <= 1)) | (scaleval != 100 & all(x >= 0))) {
          # The weighted mean calculated for each simulation was used instead of the national average.
          return(scaleval* sum(abs(x - sum(popsh * x))))
        }else{
          return(NA)
        }
      })
      
      
      boot.lcl2 <- quantile(inequal.boot.vals, probs = c(0.025), na.rm = T)
      boot.ucl2 <- quantile(inequal.boot.vals, probs = c(0.975), na.rm = T)

    }
    
  }

  inequal.mdmu <- inequal.mdmu * scaleval

  
  # Return the results as a list
  return(list(inequal.mdmu=inequal.mdmu, se.mdmu.boot=se.boot,  se.mdmu.formula=NA, 
              se.lowerci.mdmu = boot.lcl2, se.upperci.mdmu = boot.ucl2))  # return a list of the inequality measure and the standard error 
  
}
