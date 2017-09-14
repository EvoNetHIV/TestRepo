#' @title Title
#'
#' @description Description
#'
#' @param x A number.
#' @param y A number.
#' @return return value here.
#' @details
#' Additional details here
#' @examples
#' example function call here

#' @export
social_condom_use <- function(dat,at)
{
  # Assigns condom use (0/1) to each row (analagous to act) in discord_coital_df
  # inputs: param$condom_prob, dat$discord_coital_df
  # output: dat$discord_coital_df$condom
  
  ########################################
  #if no disc. pairs or no sex acts, stop fxn
  if(is.null(dat$discord_coital_df)){return(dat)}
  ###########################################
  
  # Calcuate condom use (0/1) based on "condom_prob"
  if(dat$param$risk_comp_cond) {
    condom_prob <- ifelse((dat$pop$vaccinated[dat$discord_coital_df$sus_id] == 1 |
                           (dat$pop$vaccinated[dat$discord_coital_df$inf_id] == 1 & 
                            is.na(dat$pop$diag_status[dat$discord_coital_df$inf_id]))),
                          dat$param$condom_prob * dat$param$risk_comp_cond_rr,
                          dat$param$condom_prob)
  } else {
    condom_prob <- rep(dat$param$condom_prob, nrow(dat$discord_coital_df))
  }
  
  dat$discord_coital_df$condom <- rbinom(n = nrow(dat$discord_coital_df),
                                         size = 1,
                                         prob = condom_prob)
    
  if(dat$param$condom_prob_change == T) {
    dat$param$condom_prob <- ((dat$param$condom_prob_max * (at^dat$param$condom_prob_pow)) /
                             ((dat$param$condom_prob_inflect^dat$param$condom_prob_pow) + (at^dat$param$condom_prob_pow))) + 0.04
  }
  
  return(dat)
  
}