###############################################################################################################
##This script is to generate tables with calculated follow-up dates and actual dates in RC
#calculated 6/12/18 months etc.
#actual 6/12/18 months etc.
#add the variable  to show if  patient has died
# how many people are due for 6 month/12/18 etc as of today. ----looking at only  enrollment date and today()
#visit date precedes enrollment date  
###############################################################################################################

###################################################################################
####### Visit form 						---HIV+/Fibrosis:(Baseline,12,24,36)#######
#######                                 ---HIV+/HCC+, HIV-/HCC+ : Baseline,6,12 ###
#######  								---HIV+/HCC- : Baseline
###################################################################################

# Filter enrollment_arm event to get 'enr_otherid' and 'enr_enroll_d'
mydata_baseline <- mydata %>% filter(redcap_event_name %in% c('enrollment_arm_1', 'enrollment_arm_2'))

####split by visit_type 
mydata_visit_ins0 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & (visit_type == '0' | redcap_repeat_instance == '1'))# baseline
mydata_visit_ins1 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_type == '1')# 6 months
mydata_visit_ins2 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_type == '2')# 12 months
#mydata_visit_ins3 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_type == '3')# 18 months
mydata_visit_ins4 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_type == '4')# 24 months
#mydata_visit_ins5 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_type == '5')# 30 months
mydata_visit_ins6 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_type == '6')# 36 months

#Join baseline(enrollment form) with visit events to get the variables in single row.
mydata_events0 <- left_join(mydata_baseline,mydata_visit_ins0,by = "record_id")
mydata_events1 <- left_join(mydata_baseline,mydata_visit_ins1,by = "record_id")
mydata_events2 <- left_join(mydata_baseline,mydata_visit_ins2,by = "record_id")
#mydata_events3 <- left_join(mydata_baseline,mydata_visit_ins3,by = "record_id")
mydata_events4 <- left_join(mydata_baseline,mydata_visit_ins4,by = "record_id")
#mydata_events5 <- left_join(mydata_baseline,mydata_visit_ins5,by = "record_id")
mydata_events6 <- left_join(mydata_baseline,mydata_visit_ins6,by = "record_id")


# select variables from each event 
mydata_vis_baselinedate <- mydata_events0 %>% select(record_id, enr_otherid.x,visit_type.y,visit_d.y)
mydata_vis_month6date <- mydata_events1 %>% select(record_id, enr_otherid.x,visit_type.y,visit_d.y)
mydata_vis_month12date <- mydata_events2 %>% select(record_id, enr_otherid.x,visit_type.y,visit_d.y)
#mydata_vis_month18date <- mydata_events3 %>% select(record_id, enr_otherid.x,visit_type.y,visit_d.y)
mydata_vis_month24date <- mydata_events4 %>% select(record_id, enr_otherid.x,visit_type.y,visit_d.y)
#mydata_vis_month30date <- mydata_events5 %>% select(record_id, enr_otherid.x,visit_type.y,visit_d.y)
mydata_vis_month36date <- mydata_events6 %>% select(record_id, enr_otherid.x,visit_type.y,visit_d.y)

# rename the common variables across multiple dataframes
name1=lapply(list( mydata_vis_baselinedate=mydata_vis_baselinedate,mydata_vis_month6date=mydata_vis_month6date , mydata_vis_month12date = mydata_vis_month12date,   mydata_vis_month24date= mydata_vis_month24date, mydata_vis_month36date = mydata_vis_month36date),function(x){names(x)[which(names(x)=="enr_otherid.x")]="enr_otherid";x})
list2env(name1,.GlobalEnv)
name2=lapply(list(mydata_vis_baselinedate=mydata_vis_baselinedate,mydata_vis_month6date=mydata_vis_month6date , mydata_vis_month12date = mydata_vis_month12date, mydata_vis_month24date= mydata_vis_month24date, mydata_vis_month36date = mydata_vis_month36date),function(x){names(x)[which(names(x)=="visit_d.y")]="visit_d";x})
list2env(name2,.GlobalEnv)
name3=lapply(list( mydata_vis_baselinedate=mydata_vis_baselinedate, mydata_vis_month6date=mydata_vis_month6date , mydata_vis_month12date = mydata_vis_month12date, mydata_vis_month24date= mydata_vis_month24date, mydata_vis_month36date = mydata_vis_month36date),function(x){names(x)[which(names(x)=="visit_type.y")]="visit_type";x})
list2env(name3,.GlobalEnv)

# sql join all followup events
u54_visit <- sqldf('select  u54.record_id, u54.enr_otherid,u54.final_death_y,cohort, -- cohort2_enr_fibrosis_y, 
      						u54.enr_enroll_d, d0.visit_type as bvisit_type , d0.visit_d as redcap_baseline,
							    u54.month_6, d6.visit_type as visit_6_type , d6.visit_d as redcap_6_months,
      						u54.month_12, d12.visit_type as visit_12_type, d12.visit_d as redcap_12_months,
      						u54.month_24, d24.visit_type as visit_24_type, d24.visit_d as redcap_24_months,
      						u54.month_36, d36.visit_type as visit_36_type, d36.visit_d as redcap_36_months
                       from u54_id_sel u54
                       left join mydata_vis_baselinedate d0
					             left join mydata_vis_month6date d6
      						     left join mydata_vis_month12date d12
      						     left join mydata_vis_month24date d24
      						     left join mydata_vis_month36date d36
      						     where  (u54.record_id = d0.record_id and u54.enr_otherid = d0.enr_otherid) and
								 		          (u54.record_id = d6.record_id and u54.enr_otherid = d6.enr_otherid) and
      						            (u54.record_id = d12.record_id and u54.enr_otherid = d12.enr_otherid) and
      							          (u54.record_id = d24.record_id and u54.enr_otherid = d24.enr_otherid) and
      							          (u54.record_id = d36.record_id and u54.enr_otherid = d36.enr_otherid)')
#rm(u54_visit)
# library(formattable)
# u54_visit$redcap_baseline <- ifelse((u54_visit$redcap_baseline < u54_visit$enr_enroll_d),
#   cell_spec(u54_visit$redcap_baseline, background = "red", bold = T),
#   cell_spec(u54_visit$redcap_baseline, background = "white", bold = T))

########## Date variables are changed to numeric after sqldf; So change back into date variables ############
u54_visit$redcap_baseline <- as.Date(u54_visit$redcap_baseline, origin = "1970-01-01")
u54_visit$redcap_6_months <- as.Date(u54_visit$redcap_6_months, origin = "1970-01-01")
u54_visit$redcap_12_months <- as.Date(u54_visit$redcap_12_months, origin = "1970-01-01")
u54_visit$redcap_24_months <- as.Date(u54_visit$redcap_24_months, origin = "1970-01-01")
u54_visit$redcap_36_months <- as.Date(u54_visit$redcap_36_months, origin = "1970-01-01")

## filter on HIV+/Fibrosis cohort
u54_visit_fibrosis <- u54_visit %>% filter(cohort == "HIV+/Fibrosis") %>% 										   	 						  				  
  select(record_id, enr_otherid, final_death_y,cohort, #-cohort2_enr_fibrosis_y, 
         enr_enroll_d, redcap_baseline, 
         month_12, redcap_12_months, 												 
         month_24, redcap_24_months,
         month_36, redcap_36_months)


###u54_visit_fibrosis  - Calculate number of patients due for each follow-up event
u54_visit_fibrosis_due <- u54_visit_fibrosis %>% rowwise() %>%
  mutate(months_12_due = ifelse(between(month_12, enr_enroll_d, today()), 1, 0),
         months_24_due = ifelse(between(month_24, enr_enroll_d, today()), 1, 0),
         months_36_due = ifelse(between(month_36, enr_enroll_d, today()), 1, 0))

#enrollment date preceding visit date
u54_visit_fibrosis_b4enr <- subset(u54_visit_fibrosis, ((!is.na(redcap_baseline) & (redcap_baseline < enr_enroll_d)))|
                                     ((!is.na(redcap_12_months) & (redcap_baseline < enr_enroll_d)))|
                                     ((!is.na(redcap_24_months) & (redcap_baseline < enr_enroll_d)))|
                                     ((!is.na(redcap_36_months) & (redcap_baseline < enr_enroll_d))))

## filter on HIV+/HCC- cohort
u54_visit_hccneg <- u54_visit %>% filter(cohort %in% c("HIV+/HCC-")) %>% 
  select(record_id, enr_otherid,final_death_y, cohort, #-cohort2_enr_fibrosis_y,
         enr_enroll_d, redcap_baseline) 

#enrollment date preceding visit date
u54_visit_hccneg_b4enr <- subset(u54_visit_hccneg, ((!is.na(redcap_baseline) & (redcap_baseline < enr_enroll_d)))) #%>% 
                          #mutate(month_6 = "Not applicable", redcap_6_months = "Not applicable", month_12 = "Not applicable", redcap_12_months = "Not applicable")

## filter on HIV+/HCC+/HIV-/HCC+ cohort                                               
u54_visit_hccpos <- u54_visit %>% filter(cohort %in% c("HIV+/HCC+","HIV-/HCC+")) %>% 
  select(record_id, enr_otherid, final_death_y,cohort, #- cohort2_enr_fibrosis_y, 
         enr_enroll_d, redcap_baseline,
         month_6, redcap_6_months,
         month_12, redcap_12_months) 												 
###u54_visit_hccpos  - Calculate number of patients due for each follow-up event
u54_visit_hccpos_due <- u54_visit_hccpos %>% rowwise() %>%
  mutate(months_6_due = ifelse(between(month_6, enr_enroll_d, today()), 1, 0),
         months_12_due = ifelse(between(month_12, enr_enroll_d, today()), 1, 0))

#enrollment date preceding visit date
u54_visit_hccpos_b4enr <- subset(u54_visit_hccpos, ((!is.na(redcap_baseline) & (redcap_baseline < enr_enroll_d)))|
                                   ((!is.na(redcap_6_months) & (redcap_baseline < enr_enroll_d)))|
                                   ((!is.na(redcap_12_months) & (redcap_baseline < enr_enroll_d))))
visit_date_b4enr <- sum(nrow(u54_visit_hccpos_b4enr),nrow(u54_visit_hccneg_b4enr),nrow(u54_visit_fibrosis_b4enr))

###################################################################################
####### Visit-HIV form ---HIV+/Fibrosis(6,12,18,24,30,36) ---- NO BASELINE  #######
###################################################################################

# Filter enrollment_arm event to get 'enr_otherid' and 'enr_enroll_d'
mydata_baseline <- mydata %>% filter(redcap_event_name %in% c('enrollment_arm_1', 'enrollment_arm_2'))

####split by visit_hiv_type 
#mydata_visithiv_ins0 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_hiv_type == '0')# baseline
mydata_visithiv_ins1 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_hiv_type == '1')# 6 months
mydata_visithiv_ins2 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_hiv_type == '2')# 12 months
mydata_visithiv_ins3 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_hiv_type == '3')# 18 months
mydata_visithiv_ins4 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_hiv_type == '4')# 24 months
mydata_visithiv_ins5 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_hiv_type == '5')# 30 months
mydata_visithiv_ins6 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_hiv_type == '6')# 36 months

#Join baseline(enrollment form) with visit events to get the variables in single row.
#mydata_events0 <- left_join(mydata_baseline,mydata_visithiv_ins0,by = "record_id")
mydata_events1 <- left_join(mydata_baseline,mydata_visithiv_ins1,by = "record_id")
mydata_events2 <- left_join(mydata_baseline,mydata_visithiv_ins2,by = "record_id")
mydata_events3 <- left_join(mydata_baseline,mydata_visithiv_ins3,by = "record_id")
mydata_events4 <- left_join(mydata_baseline,mydata_visithiv_ins4,by = "record_id")
mydata_events5 <- left_join(mydata_baseline,mydata_visithiv_ins5,by = "record_id")
mydata_events6 <- left_join(mydata_baseline,mydata_visithiv_ins6,by = "record_id")


# select variables from each event 
mydata_hivvis_month6date <- mydata_events1 %>% select(record_id, enr_otherid.x,visit_hiv_type.y,visit_hiv_d.y)
mydata_hivvis_month12date <- mydata_events2 %>% select(record_id, enr_otherid.x,visit_hiv_type.y,visit_hiv_d.y)
mydata_hivvis_month18date <- mydata_events3 %>% select(record_id, enr_otherid.x,visit_hiv_type.y,visit_hiv_d.y)
mydata_hivvis_month24date <- mydata_events4 %>% select(record_id, enr_otherid.x,visit_hiv_type.y,visit_hiv_d.y)
mydata_hivvis_month30date <- mydata_events5 %>% select(record_id, enr_otherid.x,visit_hiv_type.y,visit_hiv_d.y)
mydata_hivvis_month36date <- mydata_events6 %>% select(record_id, enr_otherid.x,visit_hiv_type.y,visit_hiv_d.y)

# rename the common variables across multiple dataframes
name4=lapply(list( mydata_hivvis_month6date=mydata_hivvis_month6date , mydata_hivvis_month12date = mydata_hivvis_month12date,  mydata_hivvis_month18date=mydata_hivvis_month18date, mydata_hivvis_month24date= mydata_hivvis_month24date, mydata_hivvis_month30date = mydata_hivvis_month30date, mydata_hivvis_month36date = mydata_hivvis_month36date),function(x){names(x)[which(names(x)=="enr_otherid.x")]="enr_otherid";x})
list2env(name4,.GlobalEnv)
name5=lapply(list(mydata_hivvis_month6date=mydata_hivvis_month6date , mydata_hivvis_month12date = mydata_hivvis_month12date,  mydata_hivvis_month18date=mydata_hivvis_month18date, mydata_hivvis_month24date= mydata_hivvis_month24date, mydata_hivvis_month30date = mydata_hivvis_month30date, mydata_hivvis_month36date = mydata_hivvis_month36date),function(x){names(x)[which(names(x)=="visit_hiv_d.y")]="visit_hiv_d";x})
list2env(name5,.GlobalEnv)
name6=lapply(list( mydata_hivvis_month6date=mydata_hivvis_month6date , mydata_hivvis_month12date = mydata_hivvis_month12date,  mydata_hivvis_month18date=mydata_hivvis_month18date, mydata_hivvis_month24date= mydata_hivvis_month24date, mydata_hivvis_month30date = mydata_hivvis_month30date, mydata_hivvis_month36date = mydata_hivvis_month36date),function(x){names(x)[which(names(x)=="visit_hiv_type.y")]="visit_hiv_type";x})
list2env(name6,.GlobalEnv)

# sql join all followup events and filter on HIV+/Fibrosis cohort
u54_hivvisit <- sqldf("select  u54.record_id, u54.enr_otherid,u54.final_death_y,cohort, -- cohort2_enr_fibrosis_y,
                       u54.enr_enroll_d,
      							   u54.month_6, d6.visit_hiv_type as visithiv_6_type, d6.visit_hiv_d as redcap_6_months,
      							   u54.month_12, d12.visit_hiv_type as visithiv_12_type, d12.visit_hiv_d as redcap_12_months,
      							   u54.month_18, d18.visit_hiv_type as visithiv_18_type, d18.visit_hiv_d as redcap_18_months,
      							   u54.month_24, d24.visit_hiv_type as visithiv_24_type, d24.visit_hiv_d as redcap_24_months,
      							   u54.month_30, d30.visit_hiv_type as visithiv_30_type, d30.visit_hiv_d as redcap_30_months,
      							   u54.month_36, d36.visit_hiv_type as visithiv_36_type, d36.visit_hiv_d as redcap_36_months
                       from u54_id_sel u54
                       left join mydata_hivvis_month6date d6
      						     left join mydata_hivvis_month12date d12
      						     left join mydata_hivvis_month18date d18
      						     left join mydata_hivvis_month24date d24
      						     left join mydata_hivvis_month30date d30
      						     left join mydata_hivvis_month36date d36
      						     where (u54.record_id = d6.record_id and u54.enr_otherid = d6.enr_otherid) and
      						           (u54.record_id = d12.record_id and u54.enr_otherid = d12.enr_otherid) and
      							         (u54.record_id = d18.record_id and u54.enr_otherid = d18.enr_otherid) and
      							         (u54.record_id = d24.record_id and u54.enr_otherid = d24.enr_otherid) and
      							         (u54.record_id = d30.record_id and u54.enr_otherid = d30.enr_otherid) and
      							         (u54.record_id = d36.record_id and u54.enr_otherid = d36.enr_otherid) and
                              cohort='HIV+/Fibrosis' ")


########## Date variables are changed to numeric after sqldf; So change back into date variables ############
u54_hivvisit$redcap_6_months <- as.Date(u54_hivvisit$redcap_6_months, origin = "1970-01-01")
u54_hivvisit$redcap_12_months <- as.Date(u54_hivvisit$redcap_12_months, origin = "1970-01-01")
u54_hivvisit$redcap_18_months <- as.Date(u54_hivvisit$redcap_18_months, origin = "1970-01-01")
u54_hivvisit$redcap_24_months <- as.Date(u54_hivvisit$redcap_24_months, origin = "1970-01-01")
u54_hivvisit$redcap_30_months <- as.Date(u54_hivvisit$redcap_30_months, origin = "1970-01-01")
u54_hivvisit$redcap_36_months <- as.Date(u54_hivvisit$redcap_36_months, origin = "1970-01-01")

u54_hivvisit_select <- u54_hivvisit %>% select( record_id, enr_otherid,final_death_y,cohort, #-cohort2_enr_fibrosis_y,
                                                enr_enroll_d,
                                                month_6, redcap_6_months,
                                                month_12, redcap_12_months,
                                                month_18, redcap_18_months,
                                                month_24, redcap_24_months,
                                                month_30, redcap_30_months,
                                                month_36, redcap_36_months)
###u54_hivvisit_select  - Calculate number of patients due for each follow-up event
u54_hivvisit_select_due <- u54_hivvisit_select %>% rowwise() %>%
  mutate(months_6_due = ifelse(between(month_6, enr_enroll_d, today()), 1, 0),
         months_12_due = ifelse(between(month_12, enr_enroll_d, today()), 1, 0),
         months_18_due = ifelse(between(month_18, enr_enroll_d, today()), 1, 0),
         months_24_due = ifelse(between(month_24, enr_enroll_d, today()), 1, 0),
         months_30_due = ifelse(between(month_30, enr_enroll_d, today()), 1, 0),
         months_36_due = ifelse(between(month_36, enr_enroll_d, today()), 1, 0))

#enrollment date preceding visit date
u54_hivvisit_select_b4enr <- subset(u54_hivvisit_select, ((!is.na(redcap_6_months) & (redcap_6_months < enr_enroll_d)))|
                                      ((!is.na(redcap_12_months) & (redcap_12_months < enr_enroll_d)))|
                                      ((!is.na(redcap_18_months) & (redcap_18_months < enr_enroll_d)))|
                                      ((!is.na(redcap_24_months) & (redcap_24_months < enr_enroll_d)))|
                                      ((!is.na(redcap_30_months) & (redcap_30_months < enr_enroll_d)))|
                                      ((!is.na(redcap_36_months) & (redcap_36_months < enr_enroll_d))))

nrow(u54_hivvisit_select_b4enr)
###################################################################################
####### Visit-HCC form ---HIV+/HCC+;HIV-/HCC+(6,12) ---- NO BASELINE  #######
###################################################################################

# Filter enrollment_arm event to get 'enr_otherid' and 'enr_enroll_d'
mydata_baseline <- mydata %>% filter(redcap_event_name %in% c('enrollment_arm_1', 'enrollment_arm_2'))

####split by visit_hcc_type 
mydata_visithcc_ins1 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_hiv_type == '1')# 6 months
mydata_visithcc_ins2 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_hiv_type == '2')# 12 months

#Join baseline(enrollment form) with visit events to get the variables in single row.
mydata_events1 <- left_join(mydata_baseline,mydata_visithcc_ins1,by = "record_id")
mydata_events2 <- left_join(mydata_baseline,mydata_visithcc_ins2,by = "record_id")

# select variables from each event 
mydata_hccvis_month6date <- mydata_events1 %>% select(record_id, enr_otherid.x,visit_hcc_type.y,visit_hcc_d.y)
mydata_hccvis_month12date <- mydata_events2 %>% select(record_id, enr_otherid.x,visit_hcc_type.y,visit_hcc_d.y)

# rename the common variables across multiple dataframes

name7=lapply(list( mydata_hccvis_month6date=mydata_hccvis_month6date , mydata_hccvis_month12date = mydata_hccvis_month12date),function(x){names(x)[which(names(x)=="enr_otherid.x")]="enr_otherid";x})
list2env(name7,.GlobalEnv)
name8=lapply(list(mydata_hccvis_month6date=mydata_hccvis_month6date , mydata_hccvis_month12date = mydata_hccvis_month12date),function(x){names(x)[which(names(x)=="visit_hcc_d.y")]="visit_hcc_d";x})
list2env(name8,.GlobalEnv)
name9=lapply(list( mydata_hccvis_month6date=mydata_hccvis_month6date , mydata_hccvis_month12date = mydata_hccvis_month12date),function(x){names(x)[which(names(x)=="visit_hcc_type.y")]="visit_hcc_type";x})
list2env(name9,.GlobalEnv)

# sql join all followup events and filter on HIV+/Fibrosis cohort
u54_hccvisit <- sqldf("select  u54.record_id, u54.enr_otherid, u54.final_death_y,cohort, -- cohort2_enr_fibrosis_y, 
                       u54.enr_enroll_d,
							         u54.month_6, d6.visit_hcc_type as visithcc_6_type, d6.visit_hcc_d as redcap_6_months,
							         u54.month_12, d12.visit_hcc_type as visithcc_12_type, d12.visit_hcc_d  as redcap_12_months
							         from u54_id_sel u54
                       left join mydata_hccvis_month6date d6
						           left join mydata_hccvis_month12date d12
						           where (u54.record_id = d6.record_id and u54.enr_otherid = d6.enr_otherid) and
						                 (u54.record_id = d12.record_id and u54.enr_otherid = d12.enr_otherid)and
                              cohort in ('HIV+/HCC+','HIV-/HCC+')")

########## Date variables are changed to numberic after sqldf; So change back into date variables ############
u54_hccvisit$redcap_6_months <- as.Date(u54_hccvisit$redcap_6_months, origin = "1970-01-01")
u54_hccvisit$redcap_12_months <- as.Date(u54_hccvisit$redcap_12_months, origin = "1970-01-01")

u54_hccvisit_select <- u54_hccvisit %>% select(record_id, enr_otherid, final_death_y, cohort, #-cohort2_enr_fibrosis_y, 
                                               enr_enroll_d,
                                               month_6, redcap_6_months,
                                               month_12, redcap_12_months)
###u54_hccvisit_select  - Calculate number of patients due for each follow-up event
u54_hccvisit_select_due <- u54_hccvisit_select %>% rowwise() %>%
  mutate(months_6_due = ifelse(between(month_6, enr_enroll_d, today()), 1, 0),
         months_12_due = ifelse(between(month_12, enr_enroll_d, today()), 1, 0))

#enrollment date preceding visit date
u54_hccvisit_select_b4enr <- subset(u54_hccvisit_select, ((!is.na(redcap_6_months) & (redcap_6_months < enr_enroll_d)))|
                                      ((!is.na(redcap_12_months) & (redcap_12_months < enr_enroll_d))))
nrow(u54_hccvisit_select_b4enr)
###################################################################################
####### Laboratory testing results form ---HIV+/Fibrosis:(Baseline,12,24,36)#######
#######                                 ---HIV+/HCC+, HIV-/HCC+ : Baseline,6,12 ###
#######  								---HIV+/HCC- : Baseline
###################################################################################

# Filter enrollment_arm event to get 'enr_otherid' and 'enr_enroll_d'
mydata_baseline <- mydata %>% filter(redcap_event_name %in% c('enrollment_arm_1', 'enrollment_arm_2'))

####split by visit_lab_type 
mydata_visitlab_ins0 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_lab_type == '0')# baseline
mydata_visitlab_ins1 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_lab_type == '1')# 6 months
mydata_visitlab_ins2 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_lab_type == '2')# 12 months
#mydata_visitlab_ins3 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_lab_type == '3')# 18 months
mydata_visitlab_ins4 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_lab_type == '4')# 24 months
#mydata_visitlab_ins5 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_lab_type == '5')# 30 months
mydata_visitlab_ins6 <- mydata %>% filter(redcap_event_name %in% c('visit_arm_1', 'visit_arm_2') & visit_lab_type == '6')# 36 months

#Join baseline(enrollment form) with visit events to get the variables in single row.
mydata_events0 <- left_join(mydata_baseline,mydata_visitlab_ins0,by = "record_id")
mydata_events1 <- left_join(mydata_baseline,mydata_visitlab_ins1,by = "record_id")
mydata_events2 <- left_join(mydata_baseline,mydata_visitlab_ins2,by = "record_id")
#mydata_events3 <- left_join(mydata_baseline,mydata_visitlab_ins3,by = "record_id")
mydata_events4 <- left_join(mydata_baseline,mydata_visitlab_ins4,by = "record_id")
#mydata_events5 <- left_join(mydata_baseline,mydata_visitlab_ins5,by = "record_id")
mydata_events6 <- left_join(mydata_baseline,mydata_visitlab_ins6,by = "record_id")


# select variables from each event 
mydata_labvis_baselinedate <- mydata_events0 %>% select(record_id, enr_otherid.x,visit_lab_type.y,visit_lab_d.y)
mydata_labvis_month6date <- mydata_events1 %>% select(record_id, enr_otherid.x,visit_lab_type.y,visit_lab_d.y)
mydata_labvis_month12date <- mydata_events2 %>% select(record_id, enr_otherid.x,visit_lab_type.y,visit_lab_d.y)
#mydata_labvis_month18date <- mydata_events3 %>% select(record_id, enr_otherid.x,visit_lab_type.y,visit_lab_d.y)
mydata_labvis_month24date <- mydata_events4 %>% select(record_id, enr_otherid.x,visit_lab_type.y,visit_lab_d.y)
#mydata_labvis_month30date <- mydata_events5 %>% select(record_id, enr_otherid.x,visit_lab_type.y,visit_lab_d.y)
mydata_labvis_month36date <- mydata_events6 %>% select(record_id, enr_otherid.x,visit_lab_type.y,visit_lab_d.y)

# rename the common variables across multiple dataframes
name10=lapply(list( mydata_labvis_baselinedate=mydata_labvis_baselinedate,mydata_labvis_month6date=mydata_labvis_month6date , mydata_labvis_month12date = mydata_labvis_month12date,   mydata_labvis_month24date= mydata_labvis_month24date, mydata_labvis_month36date = mydata_labvis_month36date),function(x){names(x)[which(names(x)=="enr_otherid.x")]="enr_otherid";x})
list2env(name10,.GlobalEnv)
name11=lapply(list(mydata_labvis_baselinedate=mydata_labvis_baselinedate,mydata_labvis_month6date=mydata_labvis_month6date , mydata_labvis_month12date = mydata_labvis_month12date, mydata_labvis_month24date= mydata_labvis_month24date, mydata_labvis_month36date = mydata_labvis_month36date),function(x){names(x)[which(names(x)=="visit_lab_d.y")]="visit_lab_d";x})
list2env(name11,.GlobalEnv)
name12=lapply(list( mydata_labvis_baselinedate=mydata_labvis_baselinedate, mydata_labvis_month6date=mydata_labvis_month6date , mydata_labvis_month12date = mydata_labvis_month12date, mydata_labvis_month24date= mydata_labvis_month24date, mydata_labvis_month36date = mydata_labvis_month36date),function(x){names(x)[which(names(x)=="visit_lab_type.y")]="visit_lab_type";x})
list2env(name12,.GlobalEnv)

# sql join all followup events
u54_labvisit <- sqldf("select  u54.record_id, u54.enr_otherid,u54.final_death_y,cohort, -- cohort2_enr_fibrosis_y, 
      							   u54.enr_enroll_d, d0.visit_lab_type as bvisit_lab_type , d0.visit_lab_d as redcap_baseline,
								       u54.month_6, d6.visit_lab_type as visitlab_6_type , d6.visit_lab_d as redcap_6_months,
      							   u54.month_12, d12.visit_lab_type as visitlab_12_type, d12.visit_lab_d as redcap_12_months,
      							   u54.month_24, d24.visit_lab_type as visitlab_24_type, d24.visit_lab_d as redcap_24_months,
      							   u54.month_36, d36.visit_lab_type as visitlab_36_type, d36.visit_lab_d as redcap_36_months
                       from u54_id_sel u54
                       left join mydata_labvis_baselinedate d0
					             left join mydata_labvis_month6date d6
      						     left join mydata_labvis_month12date d12
      						     left join mydata_labvis_month24date d24
      						     left join mydata_labvis_month36date d36
      						     where  (u54.record_id = d0.record_id and u54.enr_otherid = d0.enr_otherid) and
								 		          (u54.record_id = d6.record_id and u54.enr_otherid = d6.enr_otherid) and
      						            (u54.record_id = d12.record_id and u54.enr_otherid = d12.enr_otherid) and
      							          (u54.record_id = d24.record_id and u54.enr_otherid = d24.enr_otherid) and
      							          (u54.record_id = d36.record_id and u54.enr_otherid = d36.enr_otherid)")

########## Date variables are changed to numeric after sqldf; So change back into date variables ############
u54_labvisit$redcap_baseline <- as.Date(u54_labvisit$redcap_baseline, origin = "1970-01-01")
u54_labvisit$redcap_6_months <- as.Date(u54_labvisit$redcap_6_months, origin = "1970-01-01")
u54_labvisit$redcap_12_months <- as.Date(u54_labvisit$redcap_12_months, origin = "1970-01-01")
u54_labvisit$redcap_24_months <- as.Date(u54_labvisit$redcap_24_months, origin = "1970-01-01")
u54_labvisit$redcap_36_months <- as.Date(u54_labvisit$redcap_36_months, origin = "1970-01-01")



# filter on HIV+/Fibrosis cohort
u54_labvisit_fibrosis <- u54_labvisit %>% filter(cohort == "HIV+/Fibrosis") %>% 										   	 						  				  
  select(record_id, enr_otherid, final_death_y, cohort, #-cohort2_enr_fibrosis_y, 
         enr_enroll_d, redcap_baseline, 
         month_12, redcap_12_months, 												 
         month_24, redcap_24_months,
         month_36, redcap_36_months)
###u54_labvisit_fibrosis  - Calculate number of patients due for each follow-up event
u54_labvisit_fibrosis_due <- u54_labvisit_fibrosis %>% rowwise() %>%
  mutate(months_12_due = ifelse(between(month_12, enr_enroll_d, today()), 1, 0),
         months_24_due = ifelse(between(month_24, enr_enroll_d, today()), 1, 0),
         months_36_due = ifelse(between(month_36, enr_enroll_d, today()), 1, 0))

#enrollment date preceding visit date
u54_labvisit_fibrosis_b4enr <- subset(u54_labvisit_fibrosis, ((!is.na(redcap_baseline) & (redcap_baseline < enr_enroll_d)))|
                                        ((!is.na(redcap_12_months) & (redcap_12_months < enr_enroll_d)))|
                                        ((!is.na(redcap_24_months) & (redcap_24_months < enr_enroll_d)))|
                                        ((!is.na(redcap_36_months) & (redcap_36_months < enr_enroll_d))))
# filter on HIV+/HCC- cohort
u54_labvisit_hccneg <- u54_labvisit %>% filter(cohort %in% c("HIV+/HCC-")) %>% 
  select(record_id, enr_otherid, final_death_y, cohort, #-cohort2_enr_fibrosis_y,
         enr_enroll_d, redcap_baseline) 

#enrollment date preceding visit date
u54_labvisit_hccneg_b4enr <- subset(u54_labvisit_hccneg, ((!is.na(redcap_baseline) & (redcap_baseline < enr_enroll_d))))		 


# filter on HIV+/HCC+/HIV-/HCC+ cohort                                               
u54_labvisit_hccpos <- u54_labvisit %>% filter(cohort %in% c("HIV+/HCC+","HIV-/HCC+")) %>% 
  select(record_id, enr_otherid, final_death_y,cohort, #-cohort2_enr_fibrosis_y, 
         enr_enroll_d, redcap_baseline,
         month_6, redcap_6_months,
         month_12, redcap_12_months) 												 
###u54_labvisit_fibrosis  - Calculate number of patients due for each follow-up event
u54_labvisit_hccpos_due <- u54_labvisit_hccpos %>% rowwise() %>%
  mutate(months_6_due = ifelse(between(month_6, enr_enroll_d, today()), 1, 0),
         months_12_due = ifelse(between(month_12, enr_enroll_d, today()), 1, 0))

#enrollment date preceding visit date
u54_labvisit_hccpos_b4enr <- subset(u54_labvisit_hccpos, ((!is.na(redcap_baseline) & (redcap_baseline < enr_enroll_d)))|
                                      ((!is.na(redcap_6_months) & (redcap_6_months < enr_enroll_d)))|
                                      ((!is.na(redcap_12_months) & (redcap_12_months < enr_enroll_d))))
labvisit_date_b4enr <- sum(nrow(u54_labvisit_hccpos_b4enr),nrow(u54_labvisit_hccneg_b4enr),nrow(u54_labvisit_fibrosis_b4enr))         

# #####rename variables
#6 months
name13=lapply(list( u54_labvisit_hccpos=u54_labvisit_hccpos, u54_hccvisit_select=u54_hccvisit_select , u54_hivvisit_select = u54_hivvisit_select, u54_visit_hccpos= u54_visit_hccpos),function(x){names(x)[which(names(x)=="month_6")]="Calculated 6 months visit";x})
list2env(name13,.GlobalEnv)

#12 months
name14=lapply(list(u54_visit_fibrosis=u54_visit_fibrosis,u54_visit_hccpos=u54_visit_hccpos,u54_hivvisit_select=u54_hivvisit_select,u54_hccvisit_select=u54_hccvisit_select,u54_labvisit_fibrosis=u54_labvisit_fibrosis,u54_labvisit_hccpos=u54_labvisit_hccpos ),function(x){names(x)[which(names(x)=="month_12")]="Calculated 12 months visit";x})
list2env(name14,.GlobalEnv)

#18 months
name15=lapply(list(u54_hivvisit_select = u54_hivvisit_select),function(x){names(x)[which(names(x)=="month_18")]="Calculated 18 months visit";x})
list2env(name15,.GlobalEnv)

#24 months
name16=lapply(list(u54_visit_fibrosis=u54_visit_fibrosis,u54_hivvisit_select = u54_hivvisit_select,u54_labvisit_fibrosis=u54_labvisit_fibrosis),function(x){names(x)[which(names(x)=="month_24")]="Calculated 24 months visit";x})
list2env(name16,.GlobalEnv)

#30 months
name17=lapply(list(u54_hivvisit_select = u54_hivvisit_select),function(x){names(x)[which(names(x)=="month_30")]="Calculated 30 months visit";x})
list2env(name17,.GlobalEnv)

#36 months
name18=lapply(list(u54_visit_fibrosis=u54_visit_fibrosis,u54_hivvisit_select = u54_hivvisit_select,u54_labvisit_fibrosis=u54_labvisit_fibrosis),function(x){names(x)[which(names(x)=="month_36")]="Calculated 36 months visit";x})
list2env(name18,.GlobalEnv)

#enrollment date
name19=lapply(list(u54_visit_fibrosis=u54_visit_fibrosis,u54_visit_hccpos=u54_visit_hccpos,u54_visit_hccneg=u54_visit_hccneg,u54_hivvisit_select=u54_hivvisit_select,u54_hccvisit_select=u54_hccvisit_select,u54_labvisit_fibrosis=u54_labvisit_fibrosis,u54_labvisit_hccpos=u54_labvisit_hccpos,u54_labvisit_hccneg=u54_labvisit_hccneg ),function(x){names(x)[which(names(x)=="enr_enroll_d")]="Enrollment Date";x})
list2env(name19,.GlobalEnv)

#enr_otherid
name20=lapply(list(u54_visit_fibrosis=u54_visit_fibrosis,u54_visit_hccpos=u54_visit_hccpos,u54_visit_hccneg=u54_visit_hccneg,u54_hivvisit_select=u54_hivvisit_select,u54_hccvisit_select=u54_hccvisit_select,u54_labvisit_fibrosis=u54_labvisit_fibrosis,u54_labvisit_hccpos=u54_labvisit_hccpos,u54_labvisit_hccneg=u54_labvisit_hccneg ),function(x){names(x)[which(names(x)=="enr_otherid")]="U54 SubjectID";x})
list2env(name20,.GlobalEnv)

#record_id
name21=lapply(list(outcome_status=outcome_status,u54_visit_fibrosis=u54_visit_fibrosis,u54_visit_hccpos=u54_visit_hccpos,u54_visit_hccneg=u54_visit_hccneg,u54_hivvisit_select=u54_hivvisit_select,u54_hccvisit_select=u54_hccvisit_select,u54_labvisit_fibrosis=u54_labvisit_fibrosis,u54_labvisit_hccpos=u54_labvisit_hccpos,u54_labvisit_hccneg=u54_labvisit_hccneg ),function(x){names(x)[which(names(x)=="record_id")]="Record ID";x})
list2env(name21,.GlobalEnv)

#cohort
name22=lapply(list(u54_visit_fibrosis=u54_visit_fibrosis,u54_visit_hccpos=u54_visit_hccpos,u54_visit_hccneg=u54_visit_hccneg,u54_hivvisit_select=u54_hivvisit_select,u54_hccvisit_select=u54_hccvisit_select,u54_labvisit_fibrosis=u54_labvisit_fibrosis,u54_labvisit_hccpos=u54_labvisit_hccpos,u54_labvisit_hccneg=u54_labvisit_hccneg ),function(x){names(x)[which(names(x)=="cohort")]="Cohort";x})
list2env(name22,.GlobalEnv)

#final_death_y
name23=lapply(list(u54_visit_fibrosis=u54_visit_fibrosis,u54_visit_hccpos=u54_visit_hccpos,u54_visit_hccneg=u54_visit_hccneg,u54_hivvisit_select=u54_hivvisit_select,u54_hccvisit_select=u54_hccvisit_select,u54_labvisit_fibrosis=u54_labvisit_fibrosis,u54_labvisit_hccpos=u54_labvisit_hccpos,u54_labvisit_hccneg=u54_labvisit_hccneg ),function(x){names(x)[which(names(x)=="final_death_y")]="Has the Patient died?";x})
list2env(name23,.GlobalEnv)