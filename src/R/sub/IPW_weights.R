#' Calculation for IPW weights to be used in treatment effects.
#' 
#' Snipped from internet.
#' (https://livefreeordichotomize.com/2019/01/17/understanding-propensity-score-weighting/
#' 
#' @parameter treatment A numeric vector of treatment indicator.
#' @parameter propensity_score A numeric vector of propensity score.
#' @parameter dat A data.frame-class object.
#' 
#' @export

IPW_weights <- function(treatment,propensity_score,dat){
  dat$treatment <- as.numeric(as.factor(treatment)) - 1
  dat <- dat %>%
    mutate(
      w_dummy = 1,
      w_ate = (treatment / propensity_score) + 
        ((1 - treatment) / (1 - propensity_score)),
      w_att = ((propensity_score * treatment) / propensity_score) + 
        ((propensity_score * (1 - treatment)) / (1 - propensity_score)),
      w_atc = (((1 - propensity_score) * treatment) / propensity_score) + 
        (((1 - propensity_score) * (1 - treatment)) / (1 - propensity_score)),
      w_atm = pmin(propensity_score, 1 - propensity_score) / 
        (treatment * propensity_score + (1 - treatment) * (1 - propensity_score)),
      w_ato = (1 - propensity_score) * treatment + 
        propensity_score * (1 - treatment)
    )
  dat[,"propensity_score"] <- propensity_score
  return(dat)
  }


