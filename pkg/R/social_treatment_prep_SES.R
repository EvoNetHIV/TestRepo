#' @title PrEP module 
#'
#' @description PrEP module parameterized with MSM values
#'
#' @param x A number.
#' @param y A number.
#' @return return value here.
#' @details
#' Additional details here
#' @examples
#' example function call here

#' @export
social_treatement_prep_SES<-function(dat,at){
  
  #partner list code doesn't work on first timestep
  if(at<3){return(dat)}
  
   #convert fast edgelist indices to evonet indices
   aa<- dat$el[[1]]
   col1=dat$attr$id[aa[,1]]
   col2=dat$attr$id[aa[,2]]
  
   #---------------------------------------------
   ## start prep treatment campaign
   if(at < dat$param$start_prep_campaign[1]){return(dat)}
   
   #---------------------------------------------
   # keep track of duration of relationship with positive partner & diagnosed/disclosed partners
   # for hiv- agents
   status1= dat$pop$Status[col1]
   status2= dat$pop$Status[col2]
   diag1 <- dat$pop$diag_status[col1]
   diag2 <- dat$pop$diag_status[col2]
   disc1 <- dat$pop$disclosure_status[col1]
   disc2 <- dat$pop$disclosure_status[col2]
   
   ix1=which(status1==1 & status2==0 )
   ix2=which(status1==0 & status2==1 )
   ix3=which(status1==0 & status2==0 )
   
   jx1=which(disc1==1 & is.na(disc2))
   jx2=which(is.na(disc1) & disc2==1 )
   jx3=which(is.na(disc1) & is.na(disc2))
   
   #discordant couples
   dat$pop$pos_partner_duration[col1[ix2]] <- dat$pop$pos_partner_duration[col1[ix2]]+1
   dat$pop$pos_partner_duration[col2[ix1]] <- dat$pop$pos_partner_duration[col2[ix1]]+1
   
   #known discordant couples
   dat$pop$known_pos_partner_duration[col1[jx2]] <- dat$pop$known_pos_partner_duration[col1[jx2]]+1
   dat$pop$known_pos_partner_duration[col2[jx1]] <- dat$pop$known_pos_partner_duration[col2[jx1]]+1
   
   #both partners hiv-
   ix3b <- which(!is.element(ix3, c(ix1,ix2)))
   dat$pop$pos_partner_duration[ix3[ix3b]] <- 0
   
   #isolates (not in partnership)
   ix4=1:length(dat$pop$Status)
   ix5=which(!is.element(ix4,unique(c(col1,col2))))
   dat$pop$pos_partner_duration[ix5] <- 0
   
   #end of tracking duration of relationship with positive partner for hiv- agents
   #---------------------------------------------
   
   #---------------------------------------------
   #keep track of number of partners for given time period
   partnerlist <- summary_partner_list(dat)
   length(dat$pop$Status)
   partner_thresh <- at-dat$param$no_past_partners_time_prep
   
   new_part_list<- lapply(1:length(partnerlist),
                          function(x){ length(which(partnerlist[[x]]>partner_thresh))})
   new_part_vec <- unlist(new_part_list)
   #qaqc here
   if(length(new_part_vec)!=length(dat$pop$Status)){browser()}
   
   dat$pop$no_partners_past_prep <- new_part_vec
   #end of tracking number of past partners given value of dat$param$no_past_partners_time_prep
   #---------------------------------------------
   
   #---------------------------------------------
   #keep track of current number of partners
   partner_count_now <- table(c(col1,col2))
   partner_count_vec <- rep(0, length(dat$pop$Status))
   partner_count_vec[as.numeric(names(partner_count_now))] <- as.numeric(partner_count_now)
   dat$pop$no_partners_now_prep <- partner_count_vec
   #end of keeping track of current number of partners
   #---------------------------------------------
   
   #---------------------------------------------
   #track whether at least one diagnosed hiv+ partner
   d1=which(status1==0 & status2==1 & diag2==1)
   d2=which(status1==1 & status2==0 & diag1==1)
   dat$pop$have_diag_partner[col1[d1]] <- 1
   dat$pop$have_diag_partner[col2[d2]] <- 1
   #end of tracking whether at least one diagnosed hiv+ partner
   #---------------------------------------------
   #track whether at least one disclosed hiv+ partner
   disclose1=which(status1==0 & status2==1 & disc2==1)
   disclose2=which(status1==1 & status2==0 & disc1==1)
   dat$pop$have_disc_partner[col1[disclose1]] <- 1
   dat$pop$have_disc_partner[col2[disclose2]] <- 1
   #end of tracking whether at least one disclosed hiv+ partner
   #---------------------------------------------
   
   #---------------------------------------------
   # track condom use
   
   #---------------------------------------------
   
   # more in line with social_treatment_prep
  # prep_ix <-  which((dat$pop$pos_partner_duration >= dat$param$min_pos_partner_duration | # in relationship with positive partner for x duration OR
  #       dat$pop$no_partners_past_prep >= dat$param$min_past_partners_prep |               # have had over y partners in last year OR
  #       dat$pop$no_partners_now_prep >= dat$param$min_current_partners_prep |             # are in over z concurrent partnerships at present OR
  #       dat$pop$have_disc_partner == 1 )  &                                               # current partner has disclosed HIV+ status
  #       is.na(dat$pop$diag_status))                                                       # AND does not have diagnosed HIV
  
   prep_ix <-  which((
        dat$pop$known_pos_partner_duration >= dat$param$min_pos_partner_duration | # in relationship with known positive partner for x duration OR
        dat$pop$have_disc_partner == 1                                      # current partner has disclosed HIV+ status
        #dat$pop$no_partners_past_prep >= dat$param$min_past_partners_prep |  # have had over y partners in last year OR
        #dat$pop$no_partners_now_prep >= dat$param$min_current_partners_prep  # are in over z concurrent partnerships at present OR
        )  &                                               
        is.na(dat$pop$diag_status))                                             # AND does not have diagnosed HIV
   
    
   ### change from social_treatment_prep: there dat$pop$eligible_for_prep is dat$pop$on_prep       
   dat$pop$eligible_for_prep[prep_ix] <- 1
   
   dat$pop$on_prep[prep_ix]<- sample(c(1,NA),
                                    length(prep_ix),
                                    replace=T,
                                    prob=c(dat$param$percent_eligible_on_prep, 1-dat$param$percent_eligible_on_prep))
   
   dat$pop$prep_decrease[prep_ix]<- sample(c(0.0,0.31,0.81,0.95),
                                           length(prep_ix),
                                           replace=T,
                                           prob=c(0.21,0.07,0.10,0.62))
   
   dat$pop$prep_list <- dat$pop$on_prep
   dat$pop$prep_list[is.na(dat$pop$prep_list)] <- 0
   
   #if(at<900){return(dat)}else{browser()}
   return(dat)
}