###############################################################################################################
######This works for both JUTH and lUTH
###############################################################################################################
library(dplyr)
###################################################################################
####### Visit form 						---HIV+/CIN:(Baseline,12,24,36,48.60)#######
#######                                 ---HIV+/ICC+, HIV-/ICC+ : Baseline ###
#######  								---HIV+/ICC- : Baseline
###################################################################################

mydata <- subset(mydata, !(record_id %in% ex_sub$record_id))#exclude out of study records
# Filter enrollment_arm event to get 'enr_otherid' and 'enr_enroll_d'
mydata_baseline <-
  mydata %>% filter(redcap_event_name %in% c('enrollment_arm_1', 'enrollment_arm_2')) %>% select(record_id, enr_otherid)

####split by visit_type_instance 

mydata_visit_ins0 <-
  mydata %>% filter(redcap_event_name %in% c('enrollment_arm_1', 'enrollment_arm_2','visit_arm_2') &
                      is.na(redcap_repeat_instance)) %>% select(record_id, visit_type, visit_d)


mydata_visit_ins1 <-
  mydata %>% filter(redcap_event_name %in% c('enrollment_arm_1','enrollment_arm_2', 'visit_arm_2') &
                      redcap_repeat_instance == '1') %>% select(record_id, visit_type, visit_d)

mydata_visit_ins2 <-
  mydata %>% filter(redcap_event_name %in% c('enrollment_arm_1','enrollment_arm_2', 'visit_arm_2') &
                      redcap_repeat_instance == '2') %>% select(record_id, visit_type, visit_d) 
mydata_visit_ins3 <-
  mydata %>% filter(redcap_event_name %in% c('enrollment_arm_1','enrollment_arm_2', 'visit_arm_2') &
                      redcap_repeat_instance == '3') %>% select(record_id, visit_type, visit_d)
mydata_visit_ins4 <-
  mydata %>% filter(redcap_event_name %in% c('enrollment_arm_1','enrollment_arm_2', 'visit_arm_2') &
                      redcap_repeat_instance == '4') %>% select(record_id, visit_type, visit_d)
mydata_visit_ins5 <-
  mydata %>% filter(redcap_event_name %in% c('enrollment_arm_1','enrollment_arm_2', 'visit_arm_2') &
                      redcap_repeat_instance == '5') %>% select(record_id, visit_type, visit_d)
mydata_visit_ins6 <-
  mydata %>% filter(redcap_event_name %in% c('enrollment_arm_1', 'enrollment_arm_2','visit_arm_2') &
                      redcap_repeat_instance == '6') %>% select(record_id, visit_type, visit_d)
#Join baseline(enrollment form) with visit events to get the variables in single row.
# select variables from each event
mydata_events0 <-
  right_join(mydata_baseline, mydata_visit_ins0, by = "record_id")
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


mydat_events01 <- left_join(mydata_events0,mydata_events1, by = c('record_id')) %>% 
                  dplyr::rename( "enr_otherid_x" = enr_otherid.x, "enr_otherid_y" = enr_otherid.y, "visit_d_x" = visit_d.x,  "visit_d_y" = visit_d.y, "visit_type_x" = visit_type.x, "visit_type_y" = visit_type.y)


# combine all data
# mydat_events01 <- sqldf(" select ev0.record_id,
#                                  ev0.redcap_event_name, 
#                                  ev0.enr_otherid, 
#                                  ev0.visit_type as visit0, 
#                                  ev0.visit_d as visit_d0,
#                                  ev1.visit_type as visit1,
#                                  ev1.visit_d as visit_d1
#                            from mydata_events0 as ev0 left join mydata_events1 as ev1  on ev0.record_id = ev1.record_id and ev0.redcap_event_name = ev1.redcap_event_name") 
# 
# mydat_events01_rec <- unique(mydat_events01$record_id)
# mydat_events01$visit_d0 <- as.Date(mydat_events01$visit_d0, origin = "1970-01-01") 
# mydat_events01$visit_d1 <- as.Date(mydat_events01$visit_d1, origin = "1970-01-01") 


#####filter the records in enrollment 2 arm - HIV+/CIN cohort with visit date and type
mydat_events01_filter <- mydat_events01 %>% dplyr::filter(!is.na(enr_otherid_y))

#####filter the records in enrollment 1 arm - ICC+ and ICC- cohorts with visit date and type
mydat_events01_filter2 <-  mydat_events01 %>% dplyr::filter(!is.na(enr_otherid_x) & is.na(enr_otherid_y)) %>% select(record_id, enr_otherid_x,visit_type_x,visit_d_x)


#####format the dataframe to get all subjects and their visit types in rows - to filter on visit type 
mydat_events01_merge <- sqldf(" select f1.record_id, 
                                 f1.enr_otherid_x,
                                 f2.visit_type_y as visit_type_x,
                                 f2.visit_d_y as visit_d_x
                                 from mydat_events01_filter f1 inner join mydat_events01_filter f2  on f1.enr_otherid_x = f2.enr_otherid_y")
mydat_events01 <- mydat_events01 %>% select (record_id, enr_otherid_x,visit_type_x,visit_d_x)
mydat_allevents <- rbind(mydat_events01_filter2,mydat_events01_merge) %>% dplyr::rename("record_id" = record_id, "enr_otherid" = enr_otherid_x,  "visit_type" = visit_type_x, "visit_d" = visit_d_x)
mydat_allevents <- rbind(mydat_allevents,mydata_events2,mydata_events3,mydata_events4,mydata_events5,mydata_events6)

#sort and rename the dataframe
mydat_allevents_sort <- mydat_allevents[order(mydat_allevents$record_id, mydat_allevents$visit_type),]
u54_visit <- left_join(mydat_allevents_sort,u54_id_sel, by = c("record_id","enr_otherid")) %>% 
  select(record_id, enr_otherid,final_death_y,cohort,enr_enroll_d,visit_type,visit_d) %>% 
  mutate(datediff_enroll_visit = interval(enr_enroll_d, visit_d) %/% months(1))

unique(u54_visit$visit_type)

u54_visit$visit_type[which(u54_visit$visit_type == "0")] = "Baseline"
u54_visit$visit_type[which(u54_visit$visit_type == "1")] = "12 Months"
u54_visit$visit_type[which(u54_visit$visit_type == "2")] = "24 Months"
u54_visit$visit_type[which(u54_visit$visit_type == "3")] = "36 Months"
u54_visit$visit_type[which(u54_visit$visit_type == "5")] = "48 Months"
u54_visit$visit_type[which(u54_visit$visit_type == "6")] = "60 Months"
u54_visit$visit_type[which(u54_visit$visit_type == "88")] = "Unscheduled visit"

#write.csv(u54_visit, "~/Downloads/u54_visit.csv")
u54_visit <- unique(u54_visit)

## filter on HIV+/CIN cohort
u54_visit_cin <- u54_visit %>% filter(cohort == "HIV+/CIN")								   	 						  				  

####to crosscheck if there are any dup record_id's; to see if there are any event type that do not belong to this cohort
###number of records
u54_visit_cin_rec <- length(unique(u54_visit_cin$enr_otherid))
event_error_u54_visit_cin <- unique(u54_visit_cin$visit_type)

library(lubridate)
###u54_visit_cin  - Calculate number of patients due for each follow-up event
u54_visit_cin_due <- u54_id_sel %>% 
  filter(cohort == "HIV+/CIN") %>% 
  select(record_id, enr_otherid, final_death_y, cohort, enr_enroll_d, month_12, month_24, month_36, month_48, month_60)
u54_visit_cin_due <- u54_visit_cin_due %>% rowwise() %>%
  mutate(months_12_due = ifelse(month_12 < today(), 1, 0),
         months_24_due = ifelse(month_24 < today(), 1, 0),
         months_36_due = ifelse(month_36 < today(), 1, 0),
         months_48_due = ifelse(month_48 < today(), 1, 0),
         months_60_due = ifelse(month_60 < today(), 1, 0))
# 
# months_12_due = ifelse(between(month_12, enr_enroll_d, today()), 1, 0),
# months_24_due = ifelse(between(month_24, enr_enroll_d, today()), 1, 0),
# months_36_due = ifelse(between(month_36, enr_enroll_d, today()), 1, 0),
# months_48_due = ifelse(between(month_48, enr_enroll_d, today()), 1, 0),
# months_60_due = ifelse(between(month_60, enr_enroll_d, today()), 1, 0))
##############################################################################################################################################################################

#records missing baseline event; 12 months, 24 months, 36 months
u54_visit_cin_bl <- subset(u54_visit_cin, (visit_type == "Baseline")) %>% select(record_id,enr_otherid)
u54_visit_cin_rec_nodup <- u54_visit_cin %>% select(record_id,enr_otherid)
u54_visit_cin_rec_nodup <- unique(u54_visit_cin_rec_nodup)
u54_visit_cin_bl_miss <- sqldf("select test_rec2.record_id,
                                          test_rec2.enr_otherid
                                  from u54_visit_cin_rec_nodup test_rec2 
                                  left join u54_visit_cin_bl test_rec on test_rec2.record_id = test_rec.record_id
                                  where test_rec.record_id is null")

#records missing 12 months follow-up data
u54_visit_cin_12m <- subset(u54_visit_cin, (visit_type == "12 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_visit_cin_12m <- unique(u54_visit_cin_12m)

u54_visit_cin_12m_due <- subset(u54_visit_cin_due, (months_12_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_visit_cin_12m_due <- unique(u54_visit_cin_12m_due)
u54_visit_cin_12m_miss <- anti_join(u54_visit_cin_12m_due,u54_visit_cin_12m, by = c("record_id", "enr_otherid"))

#records missing 24 months follow-up data
u54_visit_cin_24m <- subset(u54_visit_cin, (visit_type == "24 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_visit_cin_24m <- unique(u54_visit_cin_24m)

u54_visit_cin_24m_due <- subset(u54_visit_cin_due, (months_24_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_visit_cin_24m_miss <- anti_join(u54_visit_cin_24m_due,u54_visit_cin_24m, by = c("record_id", "enr_otherid"))

#records missing 36 months follow-up data
u54_visit_cin_36m <- subset(u54_visit_cin, (visit_type == "36 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)

u54_visit_cin_36m_due <- subset(u54_visit_cin_due, (months_36_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_visit_cin_36m_miss <- anti_join(u54_visit_cin_36m_due,u54_visit_cin_36m, by = c("record_id", "enr_otherid"))

#records missing 48 months follow-up data
u54_visit_cin_48m <- subset(u54_visit_cin, (visit_type == "48 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)

u54_visit_cin_48m_due <- subset(u54_visit_cin_due, (months_48_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_visit_cin_48m_miss <- anti_join(u54_visit_cin_48m_due,u54_visit_cin_48m, by = c("record_id", "enr_otherid"))

#records missing 60 months follow-up data
u54_visit_cin_60m <- subset(u54_visit_cin, (visit_type == "60 Months")) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)

u54_visit_cin_60m_due <- subset(u54_visit_cin_due, (months_60_due == 1)) %>% select(record_id,enr_otherid,enr_enroll_d,final_death_y)
u54_visit_cin_60m_miss <- anti_join(u54_visit_cin_60m_due,u54_visit_cin_60m, by = c("record_id", "enr_otherid"))

#### Missing Visit type/date
u54_visit_cin_missviss <- subset(u54_visit_cin, ((is.na(visit_type) | (is.na(visit_d)))))

#enrollment date preceding visit date
u54_visit_cin_b4enr <- subset(u54_visit_cin, (!is.na(visit_d) & (visit_d < enr_enroll_d)))

############################################################################################################################################

## filter on HIV+/ICC- cohort
u54_visit_iccneg <- u54_visit %>% filter(cohort %in% c("HIV+/ICC-"))
u54_visit_iccneg <- unique(u54_visit_iccneg)

####to crosscheck if there are any dup record_id's; to see if there are any event type that do not belong to this cohort
###number of records
u54_visit_iccneg_rec <- length(unique(u54_visit_iccneg$enr_otherid))
event_error_u54_visit_iccneg <- unique(u54_visit_iccneg$visit_type)

#enrollment date preceding visit date
u54_visit_iccneg_b4enr <- subset(u54_visit_iccneg, (!is.na(visit_d) & (visit_d < enr_enroll_d)))

#### Missing Visit type/date
u54_visit_iccneg_missviss <- subset(u54_visit_iccneg, ((is.na(visit_type) | (is.na(visit_d)))))

#records missing baseline event
u54_visit_iccneg_bl <- subset(u54_visit_iccneg, (visit_type == "Baseline")) %>% select(record_id,enr_otherid)
u54_visit_iccneg_rec_nodup <- u54_visit_iccneg %>% select(record_id,enr_otherid)
u54_visit_iccneg_rec_nodup <- unique(u54_visit_iccneg_rec_nodup)
u54_visit_iccneg_bl_miss <- sqldf("select test_rec2.record_id,
                                          test_rec2.enr_otherid
                                  from u54_visit_iccneg_rec_nodup test_rec2 
                                  left join u54_visit_iccneg_bl test_rec on test_rec2.record_id = test_rec.record_id
                                  where test_rec.record_id is null")


#### Non-designated events
u54_visit_iccneg_nonevent <- u54_visit_iccneg %>% subset(!(u54_visit_iccneg$visit_type %in% c("Baseline", NA)))
############################################################################################################################################
## filter on HIV+/ICC+/HIV-/ICC+ cohort                                               
u54_visit_iccpos <- u54_visit %>% filter(cohort %in% c("HIV+/ICC+","HIV-/ICC+"))
u54_visit_iccpos  <- unique(u54_visit_iccpos)

####to crosscheck if there are any dup record_id's; to see if there are any event type that do not belong to this cohort
###number of records
u54_visit_iccpos_rec <- length(unique(u54_visit_iccpos$enr_otherid))
event_error_u54_visit_iccpos <- unique(u54_visit_iccpos$visit_type)

#enrollment date preceding visit date
u54_visit_iccpos_b4enr <- subset(u54_visit_iccpos, (!is.na(visit_d) & (visit_d < enr_enroll_d)))

#### Missing Visit type/date
u54_visit_iccpos_missviss <- subset(u54_visit_iccpos, ((is.na(visit_type) | (is.na(visit_d)))))

#records missing baseline event
u54_visit_iccpos_bl <- subset(u54_visit_iccpos, (visit_type == "Baseline")) %>% select(record_id,enr_otherid)
u54_visit_iccpos_rec_nodup <- u54_visit_iccpos %>% select(record_id,enr_otherid)
u54_visit_iccpos_rec_nodup <- unique(u54_visit_iccpos_rec_nodup)
u54_visit_iccpos_bl_miss <- sqldf("select test_rec2.record_id,
                                          test_rec2.enr_otherid
                                  from u54_visit_iccpos_rec_nodup test_rec2 
                                  left join u54_visit_iccpos_bl test_rec on test_rec2.record_id = test_rec.record_id
                                  where test_rec.record_id is null")


#### Non-designated events
u54_visit_iccpos_nonevent <- u54_visit_iccpos %>% subset(!(u54_visit_iccpos$visit_type %in% c("Baseline", NA)))



#################################################################################################################
#ADDED survival status - info  ---how many missing --- how many have and list  subjects who miss survival status
#####death date and last follow-up date for Arm 1
status1_surv <- mydata %>% filter(redcap_event_name %in% c('study_end_arm_1')) %>% filter(!is.na(fup_d)) %>% select(record_id,redcap_repeat_instance,fup_d)  %>% group_by (record_id) %>% filter(fup_d == max(fup_d))
status1_oc <- mydata %>% filter((redcap_event_name %in% c('study_end_arm_1')) & (!is.na(final_death_y))) %>% select(record_id,final_death_y,final_death_d, final_fu_d)
#mydata_baseline1 <- mydata_baseline %>% filter(!grepl('JCIN', enr_otherid))
mydata_baseline1 <- mydata %>% filter((redcap_event_name == "enrollment_arm_1") & !(record_id %in% ex_sub)) %>% select(record_id, enr_otherid)

status1 <- left_join(mydata_baseline1,status1_oc, by = "record_id")
status1_combo <- left_join(status1, status1_surv, by = "record_id") %>% mutate(status_fup = ifelse(is.na(final_fu_d),fup_d, final_fu_d)) %>% select(-redcap_repeat_instance,-fup_d,-final_fu_d)
status1_combo$status_fup <- as.Date.numeric(status1_combo$status_fup, origin="1970-01-01")

#####death date and last follow-up date for Arm 2
status2_surv <- mydata %>% filter(redcap_event_name %in% c('study_end_arm_2')) %>% filter(!is.na(fup_d)) %>% select(record_id,redcap_repeat_instance,fup_d)  %>% group_by (record_id) %>% filter(fup_d == max(fup_d))
status2_oc <- mydata %>% filter((redcap_event_name %in% c('study_end_arm_2')) & (!is.na(final_death_y))) %>% select(record_id,final_death_y,final_death_d, final_fu_d)
mydata_baseline2 <- mydata %>% filter((redcap_event_name == "enrollment_arm_2") & !(record_id %in% ex_sub)) %>% select(record_id, enr_otherid)
#mydata_baseline2 <- mydata_baseline %>% filter(grepl('JCIN', enr_otherid)) ## This excludes 1 record where subject ID does not have JCIN format
status2 <- left_join(mydata_baseline2,status2_oc, by = "record_id")
status2_combo <- left_join(status2, status2_surv, by = "record_id") %>% mutate(status_fup = ifelse(is.na(final_fu_d),fup_d, final_fu_d)) %>% select(-redcap_repeat_instance,-fup_d,-final_fu_d)
status2_combo$status_fup <- as.Date.numeric(status2_combo$status_fup, origin="1970-01-01")

#### rbind Arm 1 and arm 2
status <- rbind(status1_combo, status2_combo) %>% mutate(surv_d = ifelse(is.na(final_death_d), status_fup,final_death_d)) %>% select( -status_fup, -final_death_d)
status$surv_d <- as.Date.numeric(status$surv_d, origin="1970-01-01")

#get the cohort and join with combined dataset
id_cohort <- u54_id_sel %>% select(record_id, enr_otherid, cohort)
status_cohort <- left_join(status,id_cohort, by = c("record_id","enr_otherid")) 

status_cohort_na <- status_cohort %>% filter(is.na(surv_d)) %>%
                    arrange(cohort, enr_otherid) %>%
                    dplyr::rename("Record ID" = record_id, "U54 SubjectID" = enr_otherid, "Has the Patient died?" = final_death_y, "Death date/Last followup date" = surv_d,  "Cohort" = cohort)
