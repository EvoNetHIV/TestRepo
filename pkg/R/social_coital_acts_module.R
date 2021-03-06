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
social_coital_acts_module <- function(dat,at)
{  
  #Description:
  # Calls in following order: social_discord_edgelist_df, 
  # social_coital_acts, social_role_msm, social_condom_use

 # browser()  
  dat <- social_discord_edgelist_df(dat,at)
  dat <- social_coital_acts(dat,at)
  if(dat$param$model_sex == "msm"){
    dat <- social_role_msm(dat,at)
  }
  if(dat$param$model_sex == "hetero") {
    dat <- social_act_type_het(dat, at)
  }
  dat <- social_condom_use(dat,at)
  
  return(dat)
}
