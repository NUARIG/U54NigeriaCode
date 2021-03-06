
#'enr_hospital_id',
my_fields =c('record_id',
             
             'enr_pepid',
             'enr_otherid',
             'enr_site',
             'enr_enroll_d',
             'enr_text_hiv',
             'enr_icc_diag',
             'enr_icc_diag_hist',
             'enr_icc_diag_grade',
             'enr_dob_d',
             'enr_age',
             'enr_weight',
             'enr_height',
             'enr_bmi',
             'enr_phone',
             'enr_relphone',
             'enr_rel2patient',
             'enr_marital',
             'enr_occupation',
             'enr_occupation_oth',
             'enr_edu',
             'enr_income_y',
             'enr_income',
             'enr_rep_preg',
             'enr_rep_age',
             'enr_rep_preg_ct',
             'enr_rep_preg_live',
             'enr_rep_preg_live_age',
             'enr_brth_ctrl_y',
             'enr_brth_ctrl_age',
             'enr_brth_ctrl_age_last',
             'enr_brth_ctrl_yrs',
             'enr_brth_ctrl_iud_y',
             'enr_brth_ctrl_iud_yrs',
             'enr_brth_ctrl_cdms_y',
             'enr_brth_ctrl_cdms_use',
             'enr_brth_ctrl_oth',
             'enr_sexhx_age',
             'enr_sexhx_num_partner',
             'enr_sexhx_y',
             'enr_sexhx_std',
             'enr_sexhx_std_type',
             'enr_icc_scrhx_test_y',
             'enr_icc_scrhx_y',
             'enr_icc_scrhx_test_type',
             'enr_icc_scrhx_test_age',
             'enr_icc_pap_hx_y',
             'enr_icc_pap_hx_test_y',
             'enr_icc_pap_hx_tmt',
             'enr_icc_pap_hx_tmt_oth',
             'enr_icc_hpv_y',
             'enr_icc_chly_y',
             'enr_icc_tobacco_life',
             'enr_icc_tobacco_y',
             'enr_icc_tobacco_avg_day',
             'enr_icc_tobacco_reg',
             'enr_icc_tobacco_yr',
             'enr_icc_tobacco_q_age',
             'enr_packyr',
             'enr_icc_tobacco_prtnr',
             'enr_hcc_alcohol_y',
             'enr_alcohol_yrs',
             'enr_icc_iccfhx_y',
             'enr_icc_iccfhx_rel',
             'end_icc_iccfhx_rel_oth',
             'enr_icc_iccfhx_yr',
             'enr_icc_iccfhx_hos',
             'enr_icc_iccfhx_rel_2',
             'end_icc_iccfhx_rel_2_oth',
             'enr_icc_iccfhx_yr_2',
             'enr_icc_iccfhx_hos_2',
             'enr_icc_iccfhx_rel_3',
             'end_icc_iccfhx_rel_3_oth',
             'enr_icc_iccfhx_yr_3',
             'enr_icc_iccfhx_hos_3',
             'enr_hpv_hpvimm',
             'enr_comorbid_malig_y',
             'enr_comorbid_malig_type',
             'enr_oth_diab',
             'enr_oth_htn',
             'enr_oth_kid_dis',
             'enr_oth_med_con',
             'hiv_text_1',
             'hiv_year',
             'hiv_cd4_initial',
             'hiv_aids_y',
             'hiv_who',
             'hiv_prioroi_y',
             'hiv_text_2',
             'hiv_pjp_y',
             'hiv_mac_y',
             'hiv_cmv_y',
             'hiv_pml_y',
             'hiv_candida_y',
             'hiv_crypto_y',
             'hiv_nhl_y',
             'hiv_ks_y',
             'hiv_ade_other',
             'icc_diag_d',
             'icc_diag_figo_version',
             'icc_diag_figo_stg_2009',
             'icc_diag_figo_stg_2018',
             'icc_diag_t_stg',
             'icc_n_lymph_node',
             'icc_diag_metastasis',
             'icc_diag_tumor_size_y',
             'icc_diag_tumor_size',
             'warning',
             'icc_diag_histopath_typ',
             'icc_diag_histopath_typ_oth',
             'icc_diag_tumor_grade',
             'visit_intro_text',
             'visit_d',
             'visit_hiv_y',
             'visit_hiv_text',
             'visit_ex_height',
             'visit_ex_weight',
             'visit_sx_bmi',
             'visit_hiv_rx_y',
             #'visit_hiv_rx_sd',
             'visit_hiv_rx_reg',
             'visit_hiv_rx_reg_oth',
             'visit_tmt_icc_type',
             'visit_tmt_icc_oth',
             'visit_lab_genomicspec_y',
             'visit_cpspy',
             'visit_cpspy_d',
             'visit_cpspy_img',
             'visit_cpspy_dys_grade',
             'visit_dys_class',
             'visit_cervix_biopsy_y',
             'visit_cervix_biopsy_d',
             'visit_biopsy_result',
             'visit_icc_y',
             'visit_icc_text',
             'visit_dur_enr_diag',
             'outcome_text',
             'final_completed_d',
             'outcome_tmt',
             'outcome_tmt_oth',
             'final_death_y',
             'final_death_d',
             'final_fu_d',
             'final_death_causes',
             'final_withdraw_y',
             'final_withdraw_d',
             'final_withdraw_reason',
             'enrollment_complete')

#visit_hiv_rx_sd <- as.Date(as.character(visit_hiv_rx_sd, format('%Y')))
#'visit_hiv_rx_sd',
