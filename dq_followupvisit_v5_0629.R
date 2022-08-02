###############################################################################################################
##This script is to generate tables with calculated follow-up dates and actual dates in RC
#calculated 6/12/18 months etc.
#actual 6/12/18 months etc.
#add the variable  to show if  patient has died
# how many people are due for 6 month/12/18 etc as of today. ----looking at only  enrollment date and today()
#visit date precedes enrollment date
### Feb_21_2022 --- Renamed redcap_12_months, redcap_24_months, redcap_36_months as form based annual followup visit
###############################################################################################################

###################################################################################
####### Visit form 						---HIV+/Fibrosis:(Baseline,12,24,36)#######
#######                                 ---HIV+/HCC+, HIV-/HCC+ : Baseline,6,12 ###
#######  								---HIV+/HCC- : Baseline
###################################################################################

mydata <- subset(mydata, !(record_id %in% ex_sub$record_id))#exclude out of study records
# Filter enrollment_arm event to get 'enr_otherid' and 'enr_enroll_d'
mydata_baseline <-
  mydata %>% filter(redcap_event_name %in% c('enrollment_arm_1', 'enrollment_arm_2')) %>% select(record_id, enr_otherid)

####split by visit_type_instance
mydata_visit_ins1 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '1') %>% select(record_id, visit_type, visit_d)

mydata_visit_ins2 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '2') %>% select(record_id, visit_type, visit_d) 
mydata_visit_ins3 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '3') %>% select(record_id, visit_type, visit_d)
mydata_visit_ins4 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '4') %>% select(record_id, visit_type, visit_d)
mydata_visit_ins5 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '5') %>% select(record_id, visit_type, visit_d)
mydata_visit_ins6 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '6') %>% select(record_id, visit_type, visit_d)
#Join baseline(enrollment form) with visit events to get the variables in single row.
# select variables from each event
mydata_events1 <-
  right_join(mydata_baseline, mydata_visit_ins1, by = "record_id")
mydata_events2 <-
  right_join(mydata_baseline, mydata_visit_ins2, by = "record_id")
mydata_events3 <-
  right_join(mydata_baseline, mydata_visit_ins3, by = "record_id")
mydata_events4 <-
  right_join(mydata_baseline, mydata_visit_ins4, by = "record_id")
mydata_events5 <-
  right_join(mydata_baseline, mydata_visit_ins5, by = "record_id")
mydata_events6 <-
  right_join(mydata_baseline, mydata_visit_ins6, by = "record_id")

# combine all data
mydat_allevents <- rbind(mydata_events1,mydata_events2,mydata_events3,mydata_events4,mydata_events5,mydata_events6) 

#sort and rename the dataframe
mydat_allevents_sort <- mydat_allevents[order(mydat_allevents$record_id, mydat_allevents$visit_type),]
u54_visit <- left_join(mydat_allevents_sort,u54_id_sel, by = c("record_id","enr_otherid")) %>% 
             select(record_id, enr_otherid,final_death_y,cohort,enr_enroll_d,visit_type,visit_d) %>% 
             mutate(datediff_enroll_visit = interval(enr_enroll_d, visit_d) %/% months(1))
 
unique(u54_visit$visit_type)
############# visit type - replace raw values with labels  ###################

u54_visit$visit_type[which(u54_visit$visit_type == "0")] = "Baseline"
u54_visit$visit_type[which(u54_visit$visit_type == "1")] = "6 Months"
u54_visit$visit_type[which(u54_visit$visit_type == "2")] = "12 Months"
u54_visit$visit_type[which(u54_visit$visit_type == "3")] = "18 Months"
u54_visit$visit_type[which(u54_visit$visit_type == "4")] = "24 Months"
u54_visit$visit_type[which(u54_visit$visit_type == "5")] = "30 Months"
u54_visit$visit_type[which(u54_visit$visit_type == "6")] = "36 Months"
u54_visit$visit_type[which(u54_visit$visit_type == "88")] = "Unscheduled visit"

#################################################################################

## filter on HIV+/Fibrosis cohort
u54_visit_fibrosis <- u54_visit %>% filter(cohort == "HIV+/Fibrosis")
u54_visit_fibrosis <- unique(u54_visit_fibrosis)

####to crosscheck if there are any dup record_id's; to see if there are any event type that do not belong to this cohort
###number of records
u54_visit_fibrosis_rec <- length(unique(u54_visit_fibrosis$enr_otherid))
event_error_u54_visit_fibrosis <- unique(u54_visit_fibrosis$visit_type)

##commented for luth
##u54_visit_fibrosis  - Calculate number of patients due for each follow-up event
u54_visit_fibrosis_due <- u54_id_sel %>% 
  filter(cohort == "HIV+/Fibrosis") %>% 
  select(record_id, enr_otherid, final_death_y, cohort, enr_enroll_d, month_12, month_24, month_36) %>% 
  rowwise() %>% 
  dplyr::mutate(months_12_due = ifelse(between(month_12, enr_enroll_d, today()), 1, 0), 
                months_24_due = ifelse(between(month_24, enr_enroll_d, today()), 1, 0), 
                months_36_due = ifelse(between(month_36, enr_enroll_d, today()), 1, 0))

#records missing baseline event; 12 months, 24 months, 36 months
u54_visit_fibrosis_bl <- subset(u54_visit_fibrosis, (visit_type == "Baseline")) %>% select(record_id,enr_otherid)
u54_visit_fibrosis_rec_nodup <- u54_visit_fibrosis %>% select(record_id,enr_otherid)
u54_visit_fibrosis_rec_nodup <- unique(u54_visit_fibrosis_rec_nodup)
u54_visit_fibrosis_bl_miss <- sqldf("select test_rec2.record_id,
                                            test_rec2.enr_otherid
                                    from u54_visit_fibrosis_rec_nodup test_rec2 
                                    left join u54_visit_fibrosis_bl test_rec on test_rec2.record_id = test_rec.record_id
                                    where test_rec.record_id is null")

#records missing 12 months follow-up data
u54_visit_fibrosis_12m <- subset(u54_visit_fibrosis, (visit_type == "12 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_visit_fibrosis_12m <- unique(u54_visit_fibrosis_12m)

u54_visit_fibrosis_12m_due <- subset(u54_visit_fibrosis_due, (months_12_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_visit_fibrosis_12m_due <- unique(u54_visit_fibrosis_12m_due)
u54_visit_fibrosis_12m_miss <- anti_join(u54_visit_fibrosis_12m_due,u54_visit_fibrosis_12m, by = c("record_id", "enr_otherid"))

#records missing 24 months follow-up data
u54_visit_fibrosis_24m <- subset(u54_visit_fibrosis, (visit_type == "24 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_visit_fibrosis_24m <- unique(u54_visit_fibrosis_24m)
u54_visit_fibrosis_24m_due <- subset(u54_visit_fibrosis_due, (months_24_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
# test <- sqldf("select * from u54_visit_fibrosis_24m_due t1 left join u54_visit_fibrosis_24m t2 on t1.record_id = t2.record_id")
# write.csv(test, "~/Downloads/test.csv")
u54_visit_fibrosis_24m_miss <- anti_join(u54_visit_fibrosis_24m_due,u54_visit_fibrosis_24m, by = c("record_id", "enr_otherid"))

#records missing 36 months follow-up data
u54_visit_fibrosis_36m <- subset(u54_visit_fibrosis, (visit_type == "36 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_visit_fibrosis_36m_due <- subset(u54_visit_fibrosis_due, (months_36_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_visit_fibrosis_36m_miss <- anti_join(u54_visit_fibrosis_36m_due,u54_visit_fibrosis_36m, by = c("record_id", "enr_otherid"))


#### Missing Visit type/date
u54_visit_fibrosis_missviss <- subset(u54_visit_fibrosis, ((is.na(visit_type) | (is.na(visit_d)))))

#enrollment date preceding visit date
u54_visit_fibrosis_b4enr <- subset(u54_visit_fibrosis, (!is.na(visit_d) & (visit_d < enr_enroll_d)))

#### Non-designated events
u54_visit_fibrosis_nonevent <- u54_visit_fibrosis %>% subset(!(u54_visit_fibrosis$visit_type %in% c("Baseline", "12 Months", "24 Months", "36 Months", NA)))
#########################################################################################################
## filter on HIV+/HCC- cohort
u54_visit_hccneg <- u54_visit %>% filter(cohort %in% c("HIV+/HCC-"))
u54_visit_hccneg <- unique(u54_visit_hccneg)

####to crosscheck if there are any dup record_id's; to see if there are any event type that do not belong to this cohort
###number of records
u54_visit_hccneg_rec <- length(unique(u54_visit_hccneg$enr_otherid))
event_error_u54_visit_hccneg <- unique(u54_visit_hccneg$visit_type)

#records missing baseline event
u54_visit_hccneg_bl <- subset(u54_visit_hccneg, (visit_type == "Baseline")) %>% select(record_id, enr_otherid)
u54_visit_hccneg_rec_nodup <- u54_visit_hccneg %>% select(record_id, enr_otherid)
u54_visit_hccneg_rec_nodup <- unique(u54_visit_hccneg_rec_nodup)
u54_visit_hccneg_bl_miss <- sqldf("select test_rec2.record_id,
                                          test_rec2.enr_otherid
                      							from u54_visit_hccneg_rec_nodup test_rec2
                      							left join	 u54_visit_hccneg_bl test_rec on  test_rec2.record_id = test_rec.record_id
                      							where test_rec.record_id is null")



#### Missing Visit type/date
u54_visit_hccneg_missviss <- subset(u54_visit_hccneg, ((is.na(visit_type) | (is.na(visit_d)))))

#enrollment date preceding visit date
u54_visit_hccneg_b4enr <- subset(u54_visit_hccneg, (!is.na(visit_d) & (visit_d < enr_enroll_d)))

#### Non-designated events
u54_visit_hccneg_nonevent <- u54_visit_hccneg %>% subset(!(u54_visit_hccneg$visit_type %in% c("Baseline", NA)))

#########################################################################################################
## filter on HIV+/HCC+/HIV-/HCC+ cohort
u54_visit_hccpos <- u54_visit %>% filter(cohort %in% c("HIV+/HCC+", "HIV-/HCC+"))
u54_visit_hccpos <- unique(u54_visit_hccpos)

####to crosscheck if there are any dup record_id's; to see if there are any event type that do not belong to this cohort
###number of records
u54_visit_hccpos_rec <- length(unique(u54_visit_hccpos$enr_otherid))
event_error_u54_visit_hccpos <- unique(u54_visit_hccpos$visit_type)


###u54_visit_hccpos  - Calculate number of patients due for each follow-up event
u54_visit_hccpos_due <- u54_id_sel %>% 
  filter(cohort %in% c("HIV+/HCC+", "HIV-/HCC+")) %>% 
  select(record_id, enr_otherid, final_death_y, cohort, enr_enroll_d, month_6, month_12) %>% 
  rowwise() %>% 
  dplyr::mutate(months_6_due = ifelse(between(month_6, enr_enroll_d, today()), 1, 0), 
                months_12_due = ifelse(between(month_12, enr_enroll_d, today()), 1, 0)) 

#records missing baseline event
u54_visit_hccpos_bl <- subset(u54_visit_hccpos, (visit_type == "Baseline")) %>% select(record_id, enr_otherid)
u54_visit_hccpos_rec_nodup <- u54_visit_hccpos %>% select(record_id, enr_otherid)
u54_visit_hccpos_rec_nodup <- unique(u54_visit_hccpos_rec_nodup)
u54_visit_hccpos_bl_miss <- sqldf("select test_rec2.record_id,
			                                    test_rec2.enr_otherid
                			 					  from u54_visit_hccpos_rec_nodup test_rec2
                			 					  left join u54_visit_hccpos_bl test_rec on  test_rec2.record_id = test_rec.record_id
                			 					  where test_rec.record_id is null")


#records missing 6 months follow-up data
u54_visit_hccpos_6m <- subset(u54_visit_hccpos, (visit_type == "6 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_visit_hccpos_6m_due <- subset(u54_visit_hccpos_due, (months_6_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_visit_hccpos_6m_miss <- anti_join(u54_visit_hccpos_6m_due,u54_visit_hccpos_6m, by = c("record_id", "enr_otherid"))
											  
#records missing 12 months follow-up data
u54_visit_hccpos_12m <- subset(u54_visit_hccpos, (visit_type == "12 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_visit_hccpos_12m_due <- subset(u54_visit_hccpos_due, (months_12_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_visit_hccpos_12m_miss <- anti_join(u54_visit_hccpos_12m_due,u54_visit_hccpos_12m, by = c("record_id", "enr_otherid"))


#### Missing Visit type/date
u54_visit_hccpos_missviss <- subset(u54_visit_hccpos, ((is.na(visit_type) | (is.na(visit_d)))))

#enrollment date preceeding visit date
u54_visit_hccpos_b4enr <- subset(u54_visit_hccpos, (!is.na(visit_d) & (visit_d < enr_enroll_d)))
#### Non-designated events
u54_visit_hccpos_nonevent <- u54_visit_hccpos %>% subset(!(u54_visit_hccpos$visit_type %in% c("Baseline", "6 Months", "12 Months", NA)))
 
 
visit_date_b4enr <-
  sum(
    nrow(u54_visit_hccpos_b4enr),
    nrow(u54_visit_hccneg_b4enr),
    nrow(u54_visit_fibrosis_b4enr)
  )

###################################################################################
####### Visit-HIV form ---HIV+/Fibrosis(6,12,18,24,30,36) ---- NO BASELINE  #######
###################################################################################

#### mydata_baseline is run in line 19 ####
####split by visit_hiv_type
mydata_visithiv_ins1 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '1') %>% select(record_id, visit_hiv_type, visit_hiv_d)
mydata_visithiv_ins2 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '2') %>% select(record_id, visit_hiv_type, visit_hiv_d)
mydata_visithiv_ins3 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '3') %>% select(record_id, visit_hiv_type, visit_hiv_d)
mydata_visithiv_ins4 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '4') %>% select(record_id, visit_hiv_type, visit_hiv_d)
mydata_visithiv_ins5 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '5') %>% select(record_id, visit_hiv_type, visit_hiv_d)
mydata_visithiv_ins6 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '6') %>% select(record_id, visit_hiv_type, visit_hiv_d)


#Join baseline(enrollment form) with visit events to get the variables in single row.
#mydata_events0 <- left_join(mydata_baseline,mydata_visithiv_ins0,by = "record_id")
mydatahiv_events1 <-
  right_join(mydata_baseline, mydata_visithiv_ins1, by = "record_id")
mydatahiv_events2 <-
  right_join(mydata_baseline, mydata_visithiv_ins2, by = "record_id")
mydatahiv_events3 <-
  right_join(mydata_baseline, mydata_visithiv_ins3, by = "record_id")
mydatahiv_events4 <-
  right_join(mydata_baseline, mydata_visithiv_ins4, by = "record_id")
mydatahiv_events5 <-
  right_join(mydata_baseline, mydata_visithiv_ins5, by = "record_id")
mydatahiv_events6 <-
  right_join(mydata_baseline, mydata_visithiv_ins6, by = "record_id")

# combine all data
mydathiv_allevents <- rbind(mydatahiv_events1,mydatahiv_events2,mydatahiv_events3,mydatahiv_events4,mydatahiv_events5,mydatahiv_events6) 

#sort and rename the dataframe
mydathiv_allevents_sort <- mydathiv_allevents[order(mydathiv_allevents$record_id,mydathiv_allevents$visit_hiv_type),]
u54_hivvisit <- left_join(mydathiv_allevents_sort,u54_id_sel, by = c("record_id","enr_otherid")) %>%
                select(record_id, enr_otherid,final_death_y,cohort,enr_enroll_d,visit_hiv_type,visit_hiv_d) %>% 
                mutate(datediff_enroll_hivvisit = interval(enr_enroll_d, visit_hiv_d) %/% months(1))

unique(u54_hivvisit$visit_hiv_type)
############# visit type - replace raw values with labels  ###################
u54_hivvisit$visit_hiv_type[which(u54_hivvisit$visit_hiv_type == "0")] = "Baseline"
u54_hivvisit$visit_hiv_type[which(u54_hivvisit$visit_hiv_type == "1")] = "6 Months"
u54_hivvisit$visit_hiv_type[which(u54_hivvisit$visit_hiv_type == "2")] = "12 Months"
u54_hivvisit$visit_hiv_type[which(u54_hivvisit$visit_hiv_type == "3")] = "18 Months"
u54_hivvisit$visit_hiv_type[which(u54_hivvisit$visit_hiv_type == "4")] = "24 Months"
u54_hivvisit$visit_hiv_type[which(u54_hivvisit$visit_hiv_type == "5")] = "30 Months"
u54_hivvisit$visit_hiv_type[which(u54_hivvisit$visit_hiv_type == "6")] = "36 Months"
u54_hivvisit$visit_hiv_type[which(u54_hivvisit$visit_hiv_type == "88")] = "Unscheduled visit"

###################################################################################################

## filter on HIV+/Fibrosis cohort
u54_hivvisit_fibrosis <- u54_hivvisit %>% filter(cohort == "HIV+/Fibrosis")
u54_hivvisit_select <- unique(u54_hivvisit_fibrosis)
###number of records
u54_hivvisit_fibrosis_rec <- length(unique(u54_hivvisit_select$enr_otherid))
event_error_u54_hivvisit_fibrosis <- unique(u54_hivvisit_select$visit_hiv_type)

###u54_hivvisit_select  - Calculate number of patients due for each follow-up event
u54_hivvisit_select_due <- u54_id_sel %>% 
  filter(cohort == "HIV+/Fibrosis") %>% 
  select(record_id, enr_otherid, final_death_y, cohort, enr_enroll_d, month_6, month_12, month_18, month_24, month_30, month_36) %>% 
  rowwise() %>% 
  dplyr::mutate(months_6_due = ifelse(between(month_6, enr_enroll_d, today()), 1, 0), 
                months_12_due = ifelse(between(month_12, enr_enroll_d, today()), 1, 0), 
                months_18_due = ifelse(between(month_18, enr_enroll_d, today()), 1, 0),
                months_24_due = ifelse(between(month_24, enr_enroll_d, today()), 1, 0), 
                months_30_due = ifelse(between(month_30, enr_enroll_d, today()), 1, 0), 
                months_36_due = ifelse(between(month_36, enr_enroll_d, today()), 1, 0))


#### Missing Visit type/date {-}
u54_hivvisit_select_missviss <- subset(u54_hivvisit_select, ((is.na(visit_hiv_type) | (is.na(visit_hiv_d)))))

#records missing 6 months follow-up data
u54_hivvisit_select_6m <- subset(u54_hivvisit_select, (visit_hiv_type == "6 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_hivvisit_select_6m_due <- subset(u54_hivvisit_select_due, (months_6_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_hivvisit_select_6m_miss <- anti_join(u54_hivvisit_select_6m_due,u54_hivvisit_select_6m, by = c("record_id", "enr_otherid"))
											  
#records missing 12 months follow-up data
u54_hivvisit_select_12m <- subset(u54_hivvisit_select, (visit_hiv_type == "12 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_hivvisit_select_12m_due <- subset(u54_hivvisit_select_due, (months_12_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_hivvisit_select_12m_miss <- anti_join(u54_hivvisit_select_12m_due,u54_hivvisit_select_12m, by = c("record_id", "enr_otherid"))

#records missing 18 months follow-up data
u54_hivvisit_select_18m <- subset(u54_hivvisit_select, (visit_hiv_type == "18 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_hivvisit_select_18m_due <- subset(u54_hivvisit_select_due, (months_18_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_hivvisit_select_18m_miss <- anti_join(u54_hivvisit_select_18m_due,u54_hivvisit_select_18m, by = c("record_id", "enr_otherid"))
											  
#records missing 24 months follow-up data
u54_hivvisit_select_24m <- subset(u54_hivvisit_select, (visit_hiv_type == "24 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_hivvisit_select_24m_due <- subset(u54_hivvisit_select_due, (months_24_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_hivvisit_select_24m_miss <- anti_join(u54_hivvisit_select_24m_due,u54_hivvisit_select_24m, by = c("record_id", "enr_otherid"))

#records missing 30 months follow-up data
u54_hivvisit_select_30m <- subset(u54_hivvisit_select, (visit_hiv_type == "30 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_hivvisit_select_30m_due <- subset(u54_hivvisit_select_due, (months_30_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_hivvisit_select_30m_miss <- anti_join(u54_hivvisit_select_30m_due,u54_hivvisit_select_30m, by = c("record_id", "enr_otherid"))
											  
#records missing 36 months follow-up data
u54_hivvisit_select_36m <- subset(u54_hivvisit_select, (visit_hiv_type == "36 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_hivvisit_select_36m_due <- subset(u54_hivvisit_select_due, (months_36_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_hivvisit_select_36m_miss <- anti_join(u54_hivvisit_select_36m_due,u54_hivvisit_select_36m, by = c("record_id", "enr_otherid"))

#enrollment date preceding visit date
u54_hivvisit_select_b4enr <- subset(u54_hivvisit_select, (!is.na(visit_hiv_d) & (visit_hiv_d < enr_enroll_d)))
 
nrow(u54_hivvisit_select_b4enr)
#########################################################################################################

###################################################################################
####### Visit-HCC form ---HIV+/HCC+;HIV-/HCC+(6,12) ---- NO BASELINE  #######
###################################################################################

#### mydata_baseline is run in line 19 ####
# mydata_baseline <- mydata %>% filter(redcap_event_name %in% c('enrollment_arm_1', 'enrollment_arm_2'))

####split by visit_hcc_type
mydata_visithcc_ins1 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '1') %>% select(record_id, visit_hcc_type, visit_hcc_d)
mydata_visithcc_ins2 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '2') %>% select(record_id, visit_hcc_type, visit_hcc_d)
mydata_visithcc_ins3 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '3') %>% select(record_id, visit_hcc_type, visit_hcc_d)


#Join baseline(enrollment form) with visit events to get the variables in single row.
mydatahcc_events1 <-
  right_join(mydata_baseline, mydata_visithcc_ins1, by = "record_id")
mydatahcc_events2 <-
  right_join(mydata_baseline, mydata_visithcc_ins2, by = "record_id")
mydatahcc_events3 <-
  right_join(mydata_baseline, mydata_visithcc_ins3, by = "record_id")

# combine all data
mydathcc_allevents <- rbind(mydatahcc_events1,mydatahcc_events2,mydatahcc_events3)

#sort and rename the dataframe
mydathcc_all_events_sort <- mydathcc_allevents[order(mydathcc_allevents$record_id, mydathcc_allevents$visit_hcc_type),]

# sql join all followup events and filter on 'HIV+/HCC+','HIV-/HCC+' cohort
u54_hccvisit <- left_join(mydathcc_all_events_sort, u54_id_sel, by = c("record_id","enr_otherid")) %>%
                select(record_id,enr_otherid,final_death_y,cohort,enr_enroll_d,visit_hcc_type,visit_hcc_d) %>% 
                mutate(datediff_enroll_hccvisit = interval(enr_enroll_d, visit_hcc_d) %/% months(1))
#u54_hccvisit <- unique(u54_hccvisit)
unique(u54_hccvisit$visit_hcc_type)
############# visit type - replace raw values with labels  ###################
u54_hccvisit$visit_hcc_type[which(u54_hccvisit$visit_hcc_type == "0")] = "Baseline"
u54_hccvisit$visit_hcc_type[which(u54_hccvisit$visit_hcc_type == "1")] = "6 Months"
u54_hccvisit$visit_hcc_type[which(u54_hccvisit$visit_hcc_type == "2")] = "12 Months"
u54_hccvisit$visit_hcc_type[which(u54_hccvisit$visit_hcc_type == "3")] = "18 Months"
u54_hccvisit$visit_hcc_type[which(u54_hccvisit$visit_hcc_type == "4")] = "24 Months"
u54_hccvisit$visit_hcc_type[which(u54_hccvisit$visit_hcc_type == "5")] = "30 Months"
u54_hccvisit$visit_hcc_type[which(u54_hccvisit$visit_hcc_type == "6")] = "36 Months"
u54_hccvisit$visit_hcc_type[which(u54_hccvisit$visit_hcc_type == "88")] = "Unscheduled visit"

######## filter on HIV+/HCC+/HIV-/HCC+ cohort(no need to select variables as they are selected in line #262)
u54_hccvisit_select <- u54_hccvisit %>% filter(cohort %in% c("HIV+/HCC+", "HIV-/HCC+"))
 
u54_hccvisit_select <- unique(u54_hccvisit_select)

###number of records
u54_hccvisit_select_rec <- length(unique(u54_hccvisit_select$enr_otherid))
event_error_u54_hccvisit_select <- unique(u54_hccvisit_select$visit_hcc_type)

###u54_hccvisit_select  - Calculate number of patients due for each follow-up event
u54_hccvisit_select_due <- u54_id_sel %>% 
  filter(cohort %in% c("HIV+/HCC+","HIV-/HCC+")) %>% 
  select(record_id, enr_otherid, final_death_y, cohort, enr_enroll_d, month_6, month_12) %>% 
  rowwise() %>% 
  dplyr::mutate(months_6_due = ifelse(between(month_6, enr_enroll_d, today()), 1, 0), 
                months_12_due = ifelse(between(month_12, enr_enroll_d, today()), 1, 0)) 

#### Missing Visit type/date {-}
u54_hccvisit_select_missviss <- subset(u54_hccvisit_select, ((is.na(visit_hcc_type) | (is.na(visit_hcc_d)))))

#records missing 6 months follow-up data
u54_hccvisit_select_6m <- subset(u54_hccvisit_select, (visit_hcc_type == "6 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_hccvisit_select_6m_due <- subset(u54_hccvisit_select_due, (months_6_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_hccvisit_select_6m_miss <- anti_join(u54_hccvisit_select_6m_due,u54_hccvisit_select_6m, by = c("record_id", "enr_otherid"))
											  
#records missing 12 months follow-up data
u54_hccvisit_select_12m <- subset(u54_hccvisit_select, (visit_hcc_type == "12 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_hccvisit_select_12m_due <- subset(u54_hccvisit_select_due, (months_12_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_hccvisit_select_12m_miss <- anti_join(u54_hccvisit_select_12m_due,u54_hccvisit_select_12m, by = c("record_id", "enr_otherid"))


#enrollment date preceding visit date
u54_hccvisit_select_b4enr <- subset(u54_hccvisit_select, (!is.na(visit_hcc_d) & (visit_hcc_d < enr_enroll_d)))

#Non-designated events
u54_hccvisit_select_nonevent <- u54_hccvisit_select %>% subset(!(u54_hccvisit_select$visit_hcc_type %in% c("6 Months", "12 Months", NA)))

nrow(u54_hccvisit_select_b4enr)
###################################################################################
####### Laboratory testing results form ---HIV+/Fibrosis:(Baseline,12,24,36)#######
#######                                 ---HIV+/HCC+, HIV-/HCC+ : Baseline,6,12 ###
#######  								---HIV+/HCC- : Baseline
###################################################################################

#### mydata_baseline is run in line 19 ####
####split by visit_lab_type
mydata_visitlab_ins1 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '1') %>% select(record_id,visit_lab_type,visit_lab_d)
mydata_visitlab_ins2 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '2') %>% select(record_id,visit_lab_type,visit_lab_d)
mydata_visitlab_ins3 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '3') %>% select(record_id,visit_lab_type,visit_lab_d)
mydata_visitlab_ins4 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '4') %>% select(record_id,visit_lab_type,visit_lab_d)
mydata_visitlab_ins5 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '5') %>% select(record_id,visit_lab_type,visit_lab_d)
mydata_visitlab_ins6 <-
  mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') &
                      redcap_repeat_instance == '6') %>% select(record_id,visit_lab_type,visit_lab_d)

#Join baseline(enrollment form) with visit events to get the variables in single row.
mydatalab_events1 <-
  right_join(mydata_baseline, mydata_visitlab_ins1, by = "record_id")
mydatalab_events2 <-
  right_join(mydata_baseline, mydata_visitlab_ins2, by = "record_id")
mydatalab_events3 <-
  right_join(mydata_baseline, mydata_visitlab_ins3, by = "record_id")
mydatalab_events4 <-
  right_join(mydata_baseline, mydata_visitlab_ins4, by = "record_id")
mydatalab_events5 <-
  right_join(mydata_baseline, mydata_visitlab_ins5, by = "record_id")
mydatalab_events6 <-
  right_join(mydata_baseline, mydata_visitlab_ins6, by = "record_id")

# combine all data
mydatlab_allevents <- rbind(mydatalab_events1,mydatalab_events2,mydatalab_events3,mydatalab_events4,mydatalab_events5,mydatalab_events6)

#sort and rename the dataframe
mydatlab_allevents_sort <- mydatlab_allevents[order(mydatlab_allevents$record_id,mydatlab_allevents$visit_lab_type),]
u54_labvisit <- left_join(mydatlab_allevents_sort,u54_id_sel, by = c("record_id","enr_otherid")) %>%
                select(record_id,enr_otherid,final_death_y,cohort,enr_enroll_d,visit_lab_type,visit_lab_d) %>%
                mutate(datediff_enroll_labvisit = interval(enr_enroll_d, visit_lab_d) %/% months(1))

u54_labvisit <- unique(u54_labvisit)
unique(u54_labvisit$visit_lab_type)

############ visit type - replace raw values with labels  ###################
u54_labvisit$visit_lab_type[which(u54_labvisit$visit_lab_type == "0")] = "Baseline"
u54_labvisit$visit_lab_type[which(u54_labvisit$visit_lab_type == "1")] = "6 Months"
u54_labvisit$visit_lab_type[which(u54_labvisit$visit_lab_type == "2")] = "12 Months"
u54_labvisit$visit_lab_type[which(u54_labvisit$visit_lab_type == "3")] = "18 Months"
u54_labvisit$visit_lab_type[which(u54_labvisit$visit_lab_type == "4")] = "24 Months"
u54_labvisit$visit_lab_type[which(u54_labvisit$visit_lab_type == "5")] = "30 Months"
u54_labvisit$visit_lab_type[which(u54_hivvisit$visit_lab_type == "6")] = "36 Months"
u54_labvisit$visit_lab_type[which(u54_hivvisit$visit_lab_type == "88")] = "Unscheduled visit"

#################################################################################

## filter on HIV+/Fibrosis cohort
u54_labvisit_fibrosis <- u54_labvisit %>% filter(cohort == "HIV+/Fibrosis")
u54_labvisit_fibrosis <- unique(u54_labvisit_fibrosis)

####to crosscheck if there are any dup record_id's; to see if there are any event type that do not belong to this cohort
###number of records
u54_labvisit_fibrosis_rec <- length(unique(u54_labvisit_fibrosis$enr_otherid))
event_error_u54_labvisit_fibrosis <- unique(u54_labvisit_fibrosis$visit_lab_type)

###u54_labvisit_fibrosis  - Calculate number of patients due for each follow-up event
u54_labvisit_fibrosis_due <- u54_id_sel %>% 
  filter(cohort == "HIV+/Fibrosis") %>% 
  select(record_id, enr_otherid, final_death_y, cohort, enr_enroll_d, month_12, month_24, month_36) %>% 
  rowwise() %>% 
  dplyr::mutate(months_12_due = ifelse(between(month_12, enr_enroll_d, today()), 1, 0), 
                months_24_due = ifelse(between(month_24, enr_enroll_d, today()), 1, 0), 
                months_36_due = ifelse(between(month_36, enr_enroll_d, today()), 1, 0))

####records missing baseline event
u54_labvisit_fibrosis_bl <- subset(u54_labvisit_fibrosis, (visit_lab_type == "Baseline")) %>% select(record_id, enr_otherid)
u54_labvisit_fibrosis_rec_nodup <- u54_labvisit_fibrosis %>% select(record_id, enr_otherid)
u54_labvisit_fibrosis_rec_nodup <- unique(u54_labvisit_fibrosis_rec_nodup)
u54_labvisit_fibrosis_bl_miss <- sqldf("select test_rec2.record_id,
			                                    test_rec2.enr_otherid
                			 					  from u54_labvisit_fibrosis_rec_nodup test_rec2
                			 					  left join u54_labvisit_fibrosis_bl test_rec on  test_rec2.record_id = test_rec.record_id
                			 					  where test_rec.record_id is null")
											  

#records missing 12 months follow-up data
u54_labvisit_fibrosis_12m <- subset(u54_labvisit_fibrosis, (visit_lab_type == "12 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_labvisit_fibrosis_12m <- unique(u54_labvisit_fibrosis_12m)
u54_labvisit_fibrosis_12m_due <- subset(u54_labvisit_fibrosis_due, (months_12_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_labvisit_fibrosis_12m_miss <- anti_join(u54_labvisit_fibrosis_12m_due,u54_labvisit_fibrosis_12m, by = c("record_id", "enr_otherid"))

#records missing 24 months follow-up data
u54_labvisit_fibrosis_24m <- subset(u54_labvisit_fibrosis, (visit_lab_type == "24 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_labvisit_fibrosis_24m_due <- subset(u54_labvisit_fibrosis_due, (months_24_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_labvisit_fibrosis_24m_miss <- anti_join(u54_labvisit_fibrosis_24m_due,u54_labvisit_fibrosis_24m, by = c("record_id", "enr_otherid"))

#records missing 36 months follow-up data
u54_labvisit_fibrosis_36m <- subset(u54_labvisit_fibrosis, (visit_lab_type == "36 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_labvisit_fibrosis_36m_due <- subset(u54_labvisit_fibrosis_due, (months_36_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_labvisit_fibrosis_36m_miss <- anti_join(u54_labvisit_fibrosis_36m_due,u54_labvisit_fibrosis_36m, by = c("record_id", "enr_otherid"))

#### Missing Visit type/date {-}
u54_labvisit_fibrosis_missviss <- subset(u54_labvisit_fibrosis, ((is.na(visit_lab_type) | (is.na(visit_lab_d)))))

#enrollment date preceding visit date
u54_labvisit_fibrosis_b4enr <- subset(u54_labvisit_fibrosis, (!is.na(visit_lab_d) & (visit_lab_d < enr_enroll_d)))

#### Non-designated events
u54_labvisit_fibrosis_nonevent <- u54_labvisit_fibrosis %>% subset(!(u54_labvisit_fibrosis$visit_lab_type %in% c("Baseline", "12 Months", "24 Months", "36 Months", NA)))

#####################################################################################################################  
# filter on HIV+/HCC- cohort
u54_labvisit_hccneg <- u54_labvisit %>% filter(cohort %in% c("HIV+/HCC-"))
u54_labvisit_hccneg <- unique(u54_labvisit_hccneg)

####to crosscheck if there are any dup record_id's; to see if there are any event type that do not belong to this cohort
###number of records
u54_labvisit_hccneg_rec <- length(unique(u54_labvisit_hccneg$enr_otherid))
event_error_u54_labvisit_hccneg <- unique(u54_labvisit_hccneg$visit_lab_type)

####records missing baseline event
u54_labvisit_hccneg_bl <- subset(u54_labvisit_hccneg, (visit_lab_type == "Baseline")) %>% select(record_id, enr_otherid)
u54_labvisit_hccneg_rec_nodup <- u54_labvisit_hccneg %>% select(record_id, enr_otherid)
u54_labvisit_hccneg_rec_nodup <- unique(u54_labvisit_hccneg_rec_nodup)
u54_labvisit_hccneg_bl_miss <- sqldf("select test_rec2.record_id,
			                                    test_rec2.enr_otherid
                			 					  from u54_labvisit_hccneg_rec_nodup test_rec2
                			 					  left join u54_labvisit_hccneg_bl test_rec on  test_rec2.record_id = test_rec.record_id
                			 					  where test_rec.record_id is null")


#### Missing Visit type/date
u54_labvisit_hccneg_missviss <- subset(u54_labvisit_hccneg, ((is.na(visit_lab_type) | (is.na(visit_lab_d)))))

#enrollment date preceding visit date
u54_labvisit_hccneg_b4enr <- subset(u54_labvisit_hccneg, (!is.na(visit_lab_d) & (visit_lab_d < enr_enroll_d)))

#### Non-designated events
u54_labvisit_hccneg_nonevent <- u54_labvisit_hccneg %>% subset(!(u54_labvisit_hccneg$visit_lab_type %in% c("Baseline", NA)))

############ As there is only baseline no need for due for additional follow-up event #######################
###u54_labvisit_hccneg  - Calculate number of patients due for each follow-up event
# u54_labvisit_hccneg_due <- u54_id_sel %>% 
#   filter(cohort %in% c("HIV+/HCC-")) %>% 
#   select(record_id, enr_otherid, final_death_y, cohort, enr_enroll_d, month_6, month_12) %>% 
#   rowwise() %>% 
#   dplyr::mutate(months_6_due = ifelse(between(month_6, enr_enroll_d, today()), 1, 0), 
#                 months_12_due = ifelse(between(month_12, enr_enroll_d, today()), 1, 0)) 
#####################################################################################################################  

#####################################################################################################################  

# filter on HIV+/HCC+/HIV-/HCC+ cohort
u54_labvisit_hccpos <- u54_labvisit %>% filter(cohort %in% c("HIV+/HCC+", "HIV-/HCC+"))
u54_labvisit_hccpos <- unique(u54_labvisit_hccpos)

####to crosscheck if there are any dup record_id's; to see if there are any event type that do not belong to this cohort
###number of records
u54_labvisit_hccpos_rec <- length(unique(u54_labvisit_hccpos$enr_otherid))
event_error_u54_labvisit_hccpos <- unique(u54_labvisit_hccpos$visit_lab_type)

###u54_labvisit_hccpos  - Calculate number of patients due for each follow-up event
u54_labvisit_hccpos_due <- u54_id_sel %>% 
  filter(cohort %in% c("HIV+/HCC+", "HIV-/HCC+")) %>% 
  select(record_id, enr_otherid, final_death_y, cohort, enr_enroll_d, month_6, month_12) %>% 
  rowwise() %>% 
  dplyr::mutate(months_6_due = ifelse(between(month_6, enr_enroll_d, today()), 1, 0), 
                months_12_due = ifelse(between(month_12, enr_enroll_d, today()), 1, 0)) 


####records missing baseline event
u54_labvisit_hccpos_bl <- subset(u54_labvisit_hccpos, (visit_lab_type == "Baseline")) %>% select(record_id, enr_otherid)
u54_labvisit_hccpos_rec_nodup <- u54_labvisit_hccpos %>% select(record_id, enr_otherid)
u54_labvisit_hccpos_rec_nodup <- unique(u54_labvisit_hccpos_rec_nodup)
u54_labvisit_hccpos_bl_miss <- sqldf("select test_rec2.record_id,
			                                    test_rec2.enr_otherid
                			 					  from u54_labvisit_hccpos_rec_nodup test_rec2
                			 					  left join u54_labvisit_hccpos_bl test_rec on  test_rec2.record_id = test_rec.record_id
                			 					  where test_rec.record_id is null")

#Missing Visit type/date
u54_labvisit_hccpos_missviss <- subset(u54_labvisit_hccpos, ((is.na(visit_lab_type) | (is.na(visit_lab_d)))))

#records missing 6 months follow-up data
u54_labvisit_hccpos_6m <- subset(u54_labvisit_hccpos, (visit_lab_type == "6 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_labvisit_hccpos_6m_due <- subset(u54_labvisit_hccpos_due, (months_6_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_labvisit_hccpos_6m_miss <- anti_join(u54_labvisit_hccpos_6m_due,u54_labvisit_hccpos_6m, by = c("record_id", "enr_otherid"))
											  
#records missing 12 months follow-up data
u54_labvisit_hccpos_12m <- subset(u54_labvisit_hccpos, (visit_lab_type == "12 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_labvisit_hccpos_12m_due <- subset(u54_labvisit_hccpos_due, (months_12_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_labvisit_hccpos_12m_miss <- anti_join(u54_labvisit_hccpos_12m_due,u54_labvisit_hccpos_12m, by = c("record_id", "enr_otherid"))


#enrollment date preceding visit date
u54_labvisit_hccpos_b4enr <- subset(u54_labvisit_hccpos, (!is.na(visit_lab_d) & (visit_lab_d < enr_enroll_d)))

#### Non-designated events
u54_labvisit_hccpos_nonevent <- u54_labvisit_hccpos %>% subset(!(u54_labvisit_hccpos$visit_lab_type %in% c("Baseline", "12 Months", "24 Months", "36 Months", NA)))

labvisit_date_b4enr <-
  sum(
    nrow(u54_labvisit_hccpos_b4enr),
    nrow(u54_labvisit_hccneg_b4enr),
    nrow(u54_labvisit_fibrosis_b4enr)
  )




u54_id_sel$redcap_event_name[u54_id_sel$redcap_event_name == "enrollment_arm_1"] <- "Enrollment (Arm 1: Aim 1 and Aim 2)"
u54_id_sel$redcap_event_name[u54_id_sel$redcap_event_name == "enrollment_arm_2"] <- "Enrollment (Arm 2: Aim 3)"

alive_u54_id_sel = subset(u54_id_sel, !(final_death_y %in% c('Yes','Unknown/Loss to followup')))

#rmarkdown::render('luth_dq_updated.Rmd', output_dir = 'output/luth', run_pandoc = TRUE)
#rmarkdown::render('juth_dq_updated.Rmd', output_dir = 'output/luth', run_pandoc = TRUE)

####### 02/16 #### write csv files - to check if visittype variable is entered
# library(WriteXLS)
# library(openxlsx)
# wb <- createWorkbook()
# addWorksheet(wb, "u54_visit_baseline" )
# addWorksheet(wb, "u54_lab_visit_baseline")
#
#
# writeData(wb, 1, u54_visit)
# writeData(wb, 2, u54_labvisit)
#
# saveWorkbook(wb, paste0("~/Documents/neelima/Test/output/baseline_visit_type_", Sys.Date(),".xlsx"), overwrite = TRUE)



# #####rename variables
##########################renamed on Feb21st2022
#redcap_12_months - visit
# 
# 
# u54_visit_select_annual <-
#   u54_visit_fibrosis %>% select(
#     record_id,
#     enr_otherid,
#     final_death_y,
#     cohort,
#     enr_enroll_d,
#     #month_12,
#     "visit_12_months" = redcap_12_months,
#     #month_24,
#     "visit_24_months" =  redcap_24_months,
#     #month_36,
#     "visit_36_months" = redcap_36_months
#   ) %>% arrange(record_id) %>% distinct(record_id, .keep_all = TRUE)# remove duplicate rows
# u54_hivvisit_select_annual <-
#   u54_hivvisit_select %>% select(
#     record_id,
#     enr_otherid,
#     "visit_hiv_12_months" = redcap_12_months,
#     "visit_hiv_24_months" = redcap_24_months,
#     "visit_hiv_36_months" = redcap_36_months
#   ) %>% arrange(record_id)
# u54_labvisit_select_annual <-
#   u54_labvisit_fibrosis %>% select(
#     record_id,
#     enr_otherid,
#     "labvisit_12_months" = redcap_12_months,
#     "labvisit_24_months" = redcap_24_months,
#     "labvisit_36_months" = redcap_36_months
#   ) %>% arrange(record_id)
# 
# 
# fibrosis_annualvis <-
#   sqldf(
#     " select v.record_id,v.enr_otherid,v.final_death_y, v.cohort, v.enr_enroll_d,
#                                      v.visit_12_months, hivv.visit_hiv_12_months, labv.labvisit_12_months,
#                                      v.visit_24_months, hivv.visit_hiv_24_months, labv.labvisit_24_months,
#                                      v.visit_36_months, hivv.visit_hiv_36_months, labv.labvisit_36_months
#                             from u54_visit_select_annual v
#                             left join u54_hivvisit_select_annual hivv on v.record_id = hivv.record_id
#                             left join u54_labvisit_select_annual labv on v.record_id = labv.record_id"
#   )
# 
# library(WriteXLS)
# WriteXLS(
#   fibrosis_annualvis,
#   paste(
#     "/Users/nkj5125//Documents/neelima/Test/output/fibrosis_annual_followups_",
#     Sys.Date() ,
#     ".xlsx",
#     sep = ""
#   )
# )
###################################################
###############COMMENTED ON JUNE 21ST 2022
# #6 months
# name13 = lapply(list(
#   u54_labvisit_hccpos = u54_labvisit_hccpos,
#   u54_hccvisit_select = u54_hccvisit_select ,
#   u54_hivvisit_select = u54_hivvisit_select,
#   u54_visit_hccpos = u54_visit_hccpos
# ), function(x) {
#   names(x)[which(names(x) == "month_6")] = "Calculated 6 months visit"
#   x
# })
# list2env(name13, .GlobalEnv)
# 
# #12 months
# name14 = lapply(list(
#   u54_visit_fibrosis = u54_visit_fibrosis,
#   u54_visit_hccpos = u54_visit_hccpos,
#   u54_hivvisit_select = u54_hivvisit_select,
#   u54_hccvisit_select = u54_hccvisit_select,
#   u54_labvisit_fibrosis = u54_labvisit_fibrosis,
#   u54_labvisit_hccpos = u54_labvisit_hccpos
# ), function(x) {
#   names(x)[which(names(x) == "month_12")] = "Calculated 12 months visit"
#   x
# })
# list2env(name14, .GlobalEnv)
# 
# #18 months
# name15 = lapply(list(u54_hivvisit_select = u54_hivvisit_select), function(x) {
#   names(x)[which(names(x) == "month_18")] = "Calculated 18 months visit"
#   x
# })
# list2env(name15, .GlobalEnv)
# 
# #24 months
# name16 = lapply(list(
#   u54_visit_fibrosis = u54_visit_fibrosis,
#   u54_hivvisit_select = u54_hivvisit_select,
#   u54_labvisit_fibrosis = u54_labvisit_fibrosis
# ), function(x) {
#   names(x)[which(names(x) == "month_24")] = "Calculated 24 months visit"
#   x
# })
# list2env(name16, .GlobalEnv)
# 
# #30 months
# name17 = lapply(list(u54_hivvisit_select = u54_hivvisit_select), function(x) {
#   names(x)[which(names(x) == "month_30")] = "Calculated 30 months visit"
#   x
# })
# list2env(name17, .GlobalEnv)
# 
# #36 months
# name18 = lapply(list(
#   u54_visit_fibrosis = u54_visit_fibrosis,
#   u54_hivvisit_select = u54_hivvisit_select,
#   u54_labvisit_fibrosis = u54_labvisit_fibrosis
# ), function(x) {
#   names(x)[which(names(x) == "month_36")] = "Calculated 36 months visit"
#   x
# })
# list2env(name18, .GlobalEnv)
# 
# #enrollment date
# name19 = lapply(list(
#   u54_visit_fibrosis = u54_visit_fibrosis,
#   u54_visit_hccpos = u54_visit_hccpos,
#   u54_visit_hccneg = u54_visit_hccneg,
#   u54_hivvisit_select = u54_hivvisit_select,
#   u54_hccvisit_select = u54_hccvisit_select,
#   u54_labvisit_fibrosis = u54_labvisit_fibrosis,
#   u54_labvisit_hccpos = u54_labvisit_hccpos,
#   u54_labvisit_hccneg = u54_labvisit_hccneg,
#   u54_visit_fibrosis_b4enr = u54_visit_fibrosis_b4enr,
#   u54_visit_hccpos_b4enr = u54_visit_hccpos_b4enr,
#   u54_visit_hccneg_b4enr = u54_visit_hccneg_b4enr,
#   u54_labvisit_fibrosis_b4enr = u54_labvisit_fibrosis_b4enr,
#   u54_labvisit_hccneg_b4enr = u54_labvisit_hccneg_b4enr,
#   u54_labvisit_hccpos_b4enr = u54_labvisit_hccpos_b4enr
# ), function(x) {names(x)[which(names(x) == "enr_enroll_d")] = "Enrollment Date";x})
# list2env(name19, .GlobalEnv)
# # 
# #enr_otherid
# name20 = lapply(list(
#   u54_visit_fibrosis_missviss = u54_visit_fibrosis_missviss,
#   u54_visit_fibrosis_b4enr = u54_visit_fibrosis_b4enr,
#   u54_visit_fibrosis_12 = u54_visit_fibrosis_12,
#   u54_visit_fibrosis_24 = u54_visit_fibrosis_24,
#   u54_visit_fibrosis_36 = u54_visit_fibrosis_36,
#   u54_visit_fibrosis_nonevent = u54_visit_fibrosis_nonevent,
#   u54_visit_hccpos_missviss = u54_visit_hccpos_missviss,
#   u54_visit_hccpos_b4enr = u54_visit_hccpos_b4enr,
#   u54_visit_hccpos_6 = u54_visit_hccpos_6,
#   u54_visit_hccpos_12 = u54_visit_hccpos_12,
#   u54_visit_hccpos_nonevent = u54_visit_hccpos_nonevent,
#   u54_visit_hccneg_missviss = u54_visit_hccneg_missviss,
#   u54_visit_hccneg_b4enr = u54_visit_hccneg_b4enr,
#   u54_visit_hccneg_nonevent = u54_visit_hccneg_nonevent,
#   u54_hivvisit_select_missviss = u54_hivvisit_select_missviss,
#   u54_hivvisit_select_b4enr = u54_hivvisit_select_b4enr,
#   u54_hivvisit_select_6 = u54_hivvisit_select_6,
#   u54_hivvisit_select_12 = u54_hivvisit_select_12,
#   u54_hivvisit_select_18 = u54_hivvisit_select_18,
#   u54_hivvisit_select_24 = u54_hivvisit_select_24,
#   u54_hivvisit_select_30 = u54_hivvisit_select_30,
#   u54_hivvisit_select_36 = u54_hivvisit_select_36,
#   u54_hccvisit_select_missviss = u54_hccvisit_select_missviss,
#   u54_hccvisit_select_b4enr = u54_hccvisit_select_b4enr,
#   u54_hccvisit_select_6 = u54_hccvisit_select_6,
#   u54_hccvisit_select_12 = u54_hccvisit_select_12,
#   u54_hccvisit_select_nonevent = u54_hccvisit_select_nonevent,
#   u54_labvisit_fibrosis_missviss = u54_labvisit_fibrosis_missviss,
#   u54_labvisit_fibrosis_b4enr = u54_labvisit_fibrosis_b4enr,
#   u54_labvisit_fibrosis_12 = u54_labvisit_fibrosis_12,
#   u54_labvisit_fibrosis_24 = u54_labvisit_fibrosis_24,
#   u54_labvisit_fibrosis_36 = u54_labvisit_fibrosis_36,
#   u54_labvisit_fibrosis_nonevent = u54_labvisit_fibrosis_nonevent,
#   u54_labvisit_hccpos_missviss = u54_labvisit_hccpos_missviss,
#   u54_labvisit_hccpos_b4enr = u54_labvisit_hccpos_b4enr,
#   u54_labvisit_hccpos_6 = u54_labvisit_hccpos_6,
#   u54_labvisit_hccpos_12 = u54_labvisit_hccpos_12,
#   u54_labvisit_hccpos_nonevent = u54_labvisit_hccpos_nonevent,
#   u54_labvisit_hccneg_missviss = u54_labvisit_hccneg_missviss,
#   u54_labvisit_hccneg_b4enr = u54_labvisit_hccneg_b4enr,
#   u54_labvisit_hccneg_nonevent = u54_labvisit_hccneg_nonevent,
#   u54_visit_fibrosis_bl_miss = u54_visit_fibrosis_bl_miss,
#   u54_visit_hccpos_bl_miss = u54_visit_hccpos_bl_miss,
#   u54_visit_hccneg_bl_miss = u54_visit_hccneg_bl_miss,
#   u54_labvisit_fibrosis_bl_miss = u54_labvisit_fibrosis_bl_miss,
#   u54_labvisit_hccpos_bl_miss = u54_labvisit_hccpos_bl_miss,
#   u54_labvisit_hccneg_bl_miss = u54_labvisit_hccneg_bl_miss
# ), function(x) {names(x)[which(names(x) == "enr_otherid")] = "U54 SubjectID";x})
# list2env(name20, .GlobalEnv)
# 
# # #record_id
# name21 = lapply(list(
#   u54_visit_fibrosis_missviss = u54_visit_fibrosis_missviss,
#   u54_visit_fibrosis_b4enr = u54_visit_fibrosis_b4enr,
#   u54_visit_fibrosis_12 = u54_visit_fibrosis_12,
#   u54_visit_fibrosis_24 = u54_visit_fibrosis_24,
#   u54_visit_fibrosis_36 = u54_visit_fibrosis_36,
#   u54_visit_fibrosis_nonevent = u54_visit_fibrosis_nonevent,
#   u54_visit_hccpos_missviss = u54_visit_hccpos_missviss,
#   u54_visit_hccpos_b4enr = u54_visit_hccpos_b4enr,
#   u54_visit_hccpos_6 = u54_visit_hccpos_6,
#   u54_visit_hccpos_12 = u54_visit_hccpos_12,
#   u54_visit_hccpos_nonevent = u54_visit_hccpos_nonevent,
#   u54_visit_hccneg_missviss = u54_visit_hccneg_missviss,
#   u54_visit_hccneg_b4enr = u54_visit_hccneg_b4enr,
#   u54_visit_hccneg_nonevent = u54_visit_hccneg_nonevent,
#   u54_hivvisit_select_missviss = u54_hivvisit_select_missviss,
#   u54_hivvisit_select_b4enr = u54_hivvisit_select_b4enr,
#   u54_hivvisit_select_6 = u54_hivvisit_select_6,
#   u54_hivvisit_select_12 = u54_hivvisit_select_12,
#   u54_hivvisit_select_18 = u54_hivvisit_select_18,
#   u54_hivvisit_select_24 = u54_hivvisit_select_24,
#   u54_hivvisit_select_30 = u54_hivvisit_select_30,
#   u54_hivvisit_select_36 = u54_hivvisit_select_36,
#   u54_hccvisit_select_missviss = u54_hccvisit_select_missviss,
#   u54_hccvisit_select_b4enr = u54_hccvisit_select_b4enr,
#   u54_hccvisit_select_6 = u54_hccvisit_select_6,
#   u54_hccvisit_select_12 = u54_hccvisit_select_12,
#   u54_hccvisit_select_nonevent = u54_hccvisit_select_nonevent,
#   u54_labvisit_fibrosis_missviss = u54_labvisit_fibrosis_missviss,
#   u54_labvisit_fibrosis_b4enr = u54_labvisit_fibrosis_b4enr,
#   u54_labvisit_fibrosis_12 = u54_labvisit_fibrosis_12,
#   u54_labvisit_fibrosis_24 = u54_labvisit_fibrosis_24,
#   u54_labvisit_fibrosis_36 = u54_labvisit_fibrosis_36,
#   u54_labvisit_fibrosis_nonevent = u54_labvisit_fibrosis_nonevent,
#   u54_labvisit_hccpos_missviss = u54_labvisit_hccpos_missviss,
#   u54_labvisit_hccpos_b4enr = u54_labvisit_hccpos_b4enr,
#   u54_labvisit_hccpos_6 = u54_labvisit_hccpos_6,
#   u54_labvisit_hccpos_12 = u54_labvisit_hccpos_12,
#   u54_labvisit_hccpos_nonevent = u54_labvisit_hccpos_nonevent,
#   u54_labvisit_hccneg_missviss = u54_labvisit_hccneg_missviss,
#   u54_labvisit_hccneg_b4enr = u54_labvisit_hccneg_b4enr,
#   u54_labvisit_hccneg_nonevent = u54_labvisit_hccneg_nonevent,
#   u54_visit_fibrosis_bl_miss = u54_visit_fibrosis_bl_miss,
#   u54_visit_hccpos_bl_miss = u54_visit_hccpos_bl_miss,
#   u54_visit_hccneg_bl_miss = u54_visit_hccneg_bl_miss,
#   u54_labvisit_fibrosis_bl_miss = u54_labvisit_fibrosis_bl_miss,
#   u54_labvisit_hccpos_bl_miss = u54_labvisit_hccpos_bl_miss,
#   u54_labvisit_hccneg_bl_miss = u54_labvisit_hccneg_bl_miss
# ), function(x) {names(x)[which(names(x) == "record_id")] = "Record ID";x})
# list2env(name21, .GlobalEnv)
# # 
# #cohort
# name22 = lapply(list(
#   u54_visit_fibrosis_missviss = u54_visit_fibrosis_missviss,
#   u54_visit_fibrosis_b4enr = u54_visit_fibrosis_b4enr,
#   u54_visit_fibrosis_12 = u54_visit_fibrosis_12,
#   u54_visit_fibrosis_24 = u54_visit_fibrosis_24,
#   u54_visit_fibrosis_36 = u54_visit_fibrosis_36,
#   u54_visit_fibrosis_nonevent = u54_visit_fibrosis_nonevent,
#   u54_visit_hccpos_missviss = u54_visit_hccpos_missviss,
#   u54_visit_hccpos_b4enr = u54_visit_hccpos_b4enr,
#   u54_visit_hccpos_6 = u54_visit_hccpos_6,
#   u54_visit_hccpos_12 = u54_visit_hccpos_12,
#   u54_visit_hccpos_nonevent = u54_visit_hccpos_nonevent,
#   u54_visit_hccneg_missviss = u54_visit_hccneg_missviss,
#   u54_visit_hccneg_b4enr = u54_visit_hccneg_b4enr,
#   u54_visit_hccneg_nonevent = u54_visit_hccneg_nonevent,
#   u54_hivvisit_select_missviss = u54_hivvisit_select_missviss,
#   u54_hivvisit_select_b4enr = u54_hivvisit_select_b4enr,
#   u54_hivvisit_select_6 = u54_hivvisit_select_6,
#   u54_hivvisit_select_12 = u54_hivvisit_select_12,
#   u54_hivvisit_select_18 = u54_hivvisit_select_18,
#   u54_hivvisit_select_24 = u54_hivvisit_select_24,
#   u54_hivvisit_select_30 = u54_hivvisit_select_30,
#   u54_hivvisit_select_36 = u54_hivvisit_select_36,
#   u54_hccvisit_select_missviss = u54_hccvisit_select_missviss,
#   u54_hccvisit_select_b4enr = u54_hccvisit_select_b4enr,
#   u54_hccvisit_select_6 = u54_hccvisit_select_6,
#   u54_hccvisit_select_12 = u54_hccvisit_select_12,
#   u54_hccvisit_select_nonevent = u54_hccvisit_select_nonevent,
#   u54_labvisit_fibrosis_missviss = u54_labvisit_fibrosis_missviss,
#   u54_labvisit_fibrosis_b4enr = u54_labvisit_fibrosis_b4enr,
#   u54_labvisit_fibrosis_12 = u54_labvisit_fibrosis_12,
#   u54_labvisit_fibrosis_24 = u54_labvisit_fibrosis_24,
#   u54_labvisit_fibrosis_36 = u54_labvisit_fibrosis_36,
#   u54_labvisit_fibrosis_nonevent = u54_labvisit_fibrosis_nonevent,
#   u54_labvisit_hccpos_missviss = u54_labvisit_hccpos_missviss,
#   u54_labvisit_hccpos_b4enr = u54_labvisit_hccpos_b4enr,
#   u54_labvisit_hccpos_6 = u54_labvisit_hccpos_6,
#   u54_labvisit_hccpos_12 = u54_labvisit_hccpos_12,
#   u54_labvisit_hccpos_nonevent = u54_labvisit_hccpos_nonevent,
#   u54_labvisit_hccneg_missviss = u54_labvisit_hccneg_missviss,
#   u54_labvisit_hccneg_b4enr = u54_labvisit_hccneg_b4enr,
#   u54_labvisit_hccneg_nonevent = u54_labvisit_hccneg_nonevent
# ), function(x) {names(x)[which(names(x) == "cohort")] = "Cohort";x})
# list2env(name22, .GlobalEnv)
# 
# #final_death_y
# name23 = lapply(list(
#   u54_visit_fibrosis_missviss = u54_visit_fibrosis_missviss,
#   u54_visit_fibrosis_b4enr = u54_visit_fibrosis_b4enr,
#   u54_visit_fibrosis_12 = u54_visit_fibrosis_12,
#   u54_visit_fibrosis_24 = u54_visit_fibrosis_24,
#   u54_visit_fibrosis_36 = u54_visit_fibrosis_36,
#   u54_visit_fibrosis_nonevent = u54_visit_fibrosis_nonevent,
#   u54_visit_hccpos_missviss = u54_visit_hccpos_missviss,
#   u54_visit_hccpos_b4enr = u54_visit_hccpos_b4enr,
#   u54_visit_hccpos_6 = u54_visit_hccpos_6,
#   u54_visit_hccpos_12 = u54_visit_hccpos_12,
#   u54_visit_hccpos_nonevent = u54_visit_hccpos_nonevent,
#   u54_visit_hccneg_missviss = u54_visit_hccneg_missviss,
#   u54_visit_hccneg_b4enr = u54_visit_hccneg_b4enr,
#   u54_visit_hccneg_nonevent = u54_visit_hccneg_nonevent,
#   u54_hivvisit_select_missviss = u54_hivvisit_select_missviss,
#   u54_hivvisit_select_b4enr = u54_hivvisit_select_b4enr,
#   u54_hivvisit_select_6 = u54_hivvisit_select_6,
#   u54_hivvisit_select_12 = u54_hivvisit_select_12,
#   u54_hivvisit_select_18 = u54_hivvisit_select_18,
#   u54_hivvisit_select_24 = u54_hivvisit_select_24,
#   u54_hivvisit_select_30 = u54_hivvisit_select_30,
#   u54_hivvisit_select_36 = u54_hivvisit_select_36,
#   u54_hccvisit_select_missviss = u54_hccvisit_select_missviss,
#   u54_hccvisit_select_b4enr = u54_hccvisit_select_b4enr,
#   u54_hccvisit_select_6 = u54_hccvisit_select_6,
#   u54_hccvisit_select_12 = u54_hccvisit_select_12,
#   u54_hccvisit_select_nonevent = u54_hccvisit_select_nonevent,
#   u54_labvisit_fibrosis_missviss = u54_labvisit_fibrosis_missviss,
#   u54_labvisit_fibrosis_b4enr = u54_labvisit_fibrosis_b4enr,
#   u54_labvisit_fibrosis_12 = u54_labvisit_fibrosis_12,
#   u54_labvisit_fibrosis_24 = u54_labvisit_fibrosis_24,
#   u54_labvisit_fibrosis_36 = u54_labvisit_fibrosis_36,
#   u54_labvisit_fibrosis_nonevent = u54_labvisit_fibrosis_nonevent,
#   u54_labvisit_hccpos_missviss = u54_labvisit_hccpos_missviss,
#   u54_labvisit_hccpos_b4enr = u54_labvisit_hccpos_b4enr,
#   u54_labvisit_hccpos_6 = u54_labvisit_hccpos_6,
#   u54_labvisit_hccpos_12 = u54_labvisit_hccpos_12,
#   u54_labvisit_hccpos_nonevent = u54_labvisit_hccpos_nonevent,
#   u54_labvisit_hccneg_missviss = u54_labvisit_hccneg_missviss,
#   u54_labvisit_hccneg_b4enr = u54_labvisit_hccneg_b4enr,
#   u54_labvisit_hccneg_nonevent = u54_labvisit_hccneg_nonevent
# ), function(x) {names(x)[which(names(x) == "final_death_y")] = "Has the Patient died?";x})
# list2env(name23, .GlobalEnv)
# 


# 
# 
# #6 months
# name24 = lapply(list(
#   u54_labvisit_hccpos = u54_labvisit_hccpos,
#   u54_hccvisit_select = u54_hccvisit_select ,
#   u54_hivvisit_select = u54_hivvisit_select,
#   u54_visit_hccpos = u54_visit_hccpos
# ), function(x) {names(x)[which(names(x) == "datediff_enr_6months")] = "(Enrollment date - redcap_6_months) IN MONTHS";x})
# list2env(name24, .GlobalEnv)
# 
# #12 months
# name25 = lapply(list(
#   u54_visit_fibrosis = u54_visit_fibrosis,
#   u54_visit_hccpos = u54_visit_hccpos,
#   u54_hivvisit_select = u54_hivvisit_select,
#   u54_hccvisit_select = u54_hccvisit_select,
#   u54_labvisit_fibrosis = u54_labvisit_fibrosis,
#   u54_labvisit_hccpos = u54_labvisit_hccpos
# ), function(x) {names(x)[which(names(x) == "datediff_enr_12months")] = "(Enrollment date - redcap_12_months) IN MONTHS";x})
# list2env(name25, .GlobalEnv)
# 
# #18 months
# name26 = lapply(list(u54_hivvisit_select = u54_hivvisit_select), function(x) {names(x)[which(names(x) == "datediff_enr_18months")] = "(Enrollment date - redcap_18_months) IN MONTHS";x})
# list2env(name26, .GlobalEnv)
# 
# #24 months
# name27 = lapply(list(
#   u54_visit_fibrosis = u54_visit_fibrosis,
#   u54_hivvisit_select = u54_hivvisit_select,
#   u54_labvisit_fibrosis = u54_labvisit_fibrosis
# ), function(x) {names(x)[which(names(x) == "datediff_enr_24months")] = "(Enrollment date - redcap_24_months) IN MONTHS";x})
# list2env(name27, .GlobalEnv)
# 
# #30 months
# name28 = lapply(list(u54_hivvisit_select = u54_hivvisit_select), function(x) {names(x)[which(names(x) == "datediff_enr_30months")] = "(Enrollment date - redcap_30_months) IN MONTHS";x})
# list2env(name28, .GlobalEnv)
# 
# #36 months
# name29 = lapply(list(
#   u54_visit_fibrosis = u54_visit_fibrosis,
#   u54_hivvisit_select = u54_hivvisit_select,
#   u54_labvisit_fibrosis = u54_labvisit_fibrosis
# ), function(x) {names(x)[which(names(x) == "datediff_enr_36months")] = "(Enrollment date - redcap_36_months) IN MONTHS";x})
# list2env(name29, .GlobalEnv)
# 
# #Baseline
# name30 = lapply(list(
#   u54_visit_fibrosis = u54_visit_fibrosis,
#   u54_visit_hccneg = u54_visit_hccneg,
#   u54_visit_hccpos = u54_visit_hccpos,
#   u54_hivvisit_select = u54_hivvisit_select,
#   u54_labvisit_fibrosis = u54_labvisit_fibrosis,
#   u54_labvisit_hccneg = u54_labvisit_hccneg,
#   u54_labvisit_hccpos = u54_labvisit_hccpos,
#   u54_visit_fibrosis_b4enr = u54_visit_fibrosis_b4enr,
#   u54_visit_hccpos_b4enr = u54_visit_hccpos_b4enr,
#   u54_visit_hccneg_b4enr = u54_visit_hccneg_b4enr,
#   u54_labvisit_fibrosis_b4enr = u54_labvisit_fibrosis_b4enr,
#   u54_labvisit_hccneg_b4enr = u54_labvisit_hccneg_b4enr,
#   u54_labvisit_hccpos_b4enr = u54_labvisit_hccpos_b4enr
#   
# ), function(x) {names(x)[which(names(x) == "datediff_enr_baseline")] = "(Enrollment date - baseline) IN MONTHS";x})
# list2env(name30, .GlobalEnv)
# 
# #Rename visit type variables.
# 
# u54_visit_fibrosis <- u54_visit_fibrosis %>%
#   rename( "Visit type" = bvisit_type,
#           "Visit type(12m)" = visit_12_type,
#           "Visit type(24m)" = visit_24_type,
#           "Visit type(36m)" = visit_36_type)
# 
# 

