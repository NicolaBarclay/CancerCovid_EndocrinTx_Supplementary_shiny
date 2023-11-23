#### PACKAGES -----
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(readr)
library(here)
library(stringr)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(gt)
library(scales)
library(kableExtra)
library(tidyr)
library(stringr)
library(ggplot2)
library(fresh)
library(plotly)
library(ggalt)
library(bslib)
library(readr)

# theme -----
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#605ca8" 
  ),
  adminlte_sidebar(
    dark_bg = "#78B7C5", #  "#D8DEE9",
    dark_hover_bg = "#3B9AB2", #"#81A1C1",
    dark_color ="white" ,
    dark_submenu_bg = "#605ca8"
  ), 
  adminlte_global(
    content_bg = "#eaebea" 
    #content_bg = "white" 
  ),
  adminlte_vars(
    border_color = "black",
    active_link_hover_bg = "#FFF",
    active_link_hover_color = "#112446",
    active_link_hover_border_color = "#112446",
    link_hover_border_color = "#112446",
    table_border_color = "black"
    
  )
)


# printing numbers with 1 decimal place and commas 
nice.num<-function(x) {
  trimws(format(round(x,1),
                big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
# printing numbers with 2 decimal place and commas 
nice.num2<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}
# printing numbers with 3 decimal place and commas 
nice.num3<-function(x) {
  trimws(format(round(x,3),
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}
# printing numbers with 4 decimal place and commas 
nice.num4<-function(x) {
  trimws(format(round(x,4),
                big.mark=",", nsmall = 4, digits=4, scientific=FALSE))}
# for counts- without decimal place
nice.num.count<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 0, digits=1, scientific=FALSE))}

#### Load and extract data -----
#data for characterisation tables
Breast_characteristics_table <- read_csv(here("data/4_Characterisations/Breast_characteristics_for_export.csv"))
Prostate_characteristics_table <- read_csv(here("data/4_Characterisations/Prostate_characteristics_for_export.csv"))

# database details
database_details <- read_csv(here("www", "database_details.csv"), show_col_types = FALSE)

# clinical code lists
concepts_lists <- read_csv(here("www", "concept_list.csv"), show_col_types = FALSE)

# attrition estimates data
IncTxBreast_attrition_estimates <- read_csv(here("data/2_EndocrineTxCancer/IncTxBreast_attrition_estimates.csv"))
IncTxProstate_attrition_estimates <- read_csv(here("data/2_EndocrineTxCancer/IncTxProstate_attrition_estimates.csv"))

# Incidence rates data
IR_Breast_endocrine <- read_csv(here("data/IRR results/EndoTxBreast/IR_table_breast_endo_with_pre_post_lockdown_pivot.csv"))
IR_Prostate_endocrine  <- read_csv(here("data/IRR results/EndoTxProstate/IR_table_prostate_endo_with_pre_post_lockdown_pivot.csv"))
IR_Breast_osteo <- read_csv(here("data/IRR results/OsteoDxBreast/IR_table_endodx_breastAI_with_pre_post_lockdown_pivot.csv"))
IR_Prostate_osteo <- read_csv(here("data/IRR results/OsteoDxProstate/IR_table_endodx_prostate_with_pre_post_lockdown_pivot.csv"))

# N events data
N_EVENTS_PM_breast_endoTx_ <- read_csv(here("data/IRR results/EndoTxBreast/N_EVENTS_PD_table_endoTx_breast.csv"))
N_EVENTS_PM_breast_endoTx_post <- read_csv(here("data/IRR results/EndoTxBreast/N_EVENTS_PM_table_endoTx_breast_POST.csv"))
N_EVENTS_PM_prostate_endoTx <- read_csv(here("data/IRR results/EndoTxProstate/N_EVENTS_PD_table_endo_prostate.csv"))
N_EVENTS_PM_prostate_endoTx_post <- read_csv(here("data/IRR results/EndoTxProstate/N_EVENTS_PM_table_endo_prostate_post.csv"))

N_EVENTS_PM_breastAI_endoDx <- read_csv(here("data/IRR results/OsteoDxBreast/N_EVENTS_PD_table_endodx_breastAI.csv"))
N_EVENTS_PM_breastAI_endoDx_post <- read_csv(here("data/IRR results/OsteoDxBreast/N_EVENTS_PM_table_endodx_breastAI.csv"))
N_EVENTS_PM_prostate_endoDx <- read_csv(here("data/IRR results/OsteoDxProstate/N_EVENTS_PD_table_endodx_prostate.csv"))
N_EVENTS_PD_prostate_endoDx_post <- read_csv(here("data/IRR results/OsteoDxProstate/N_EVENTS_PD_table_endodx_prostate_pre_post.csv"))

# IRR data
IRR_breast_endoTx <- read_csv(here("data/IRR results/EndoTxBreast/IRR_table_breast_endo.csv"))
IRR_breast_endoTx_post <- read_csv(here("data/IRR results/EndoTxBreast/IRR_table_breast_endo_post.csv"))
IRR_prostate_endoTx <- read_csv(here("data/IRR results/EndoTxProstate/IRR_table_prostate_endo.csv"))
IRR_prostate_endoTx_post <- read_csv(here("data/IRR results/EndoTxProstate/IRR_table_prostate_endo_post.csv"))

IRR_endoDx_breastAI <- read_csv(here("data/IRR results/OsteoDxBreast/IRR_table_endodx_breastAI.csv"))
IRR_endoDx_breastAI_post <- read_csv(here("data/IRR results/OsteoDxBreast/IRR_table_endodx_breastAI_POST.csv"))
IRR_endoDx_prostate <- read_csv(here("data/IRR results/OsteoDxProstate/IRR_table_endodx_prostate_ex_bf.csv"))
IRR_endoDx_prostate_post <- read_csv(here("data/IRR results/OsteoDxProstate/IRR_table_endodx_prostate_ex_bf_pre_post.csv"))

# ATTRITION TABLES
IncTxBreast_attrition_table <- IncTxBreast_attrition_estimates %>% filter(analysis_id==1) %>% select(number_subjects, reason, excluded_subjects) 
IncTxBreast_attrition_table<- IncTxBreast_attrition_table[-9,]

IncTxProstate_attrition_table <- IncTxProstate_attrition_estimates %>% filter(analysis_id==100) %>% select(number_subjects, reason, excluded_subjects) 
IncTxProstate_attrition_table <- IncTxProstate_attrition_table[-10,]

# ADD CANCER NAME AND COMBINE THE TWO ATTRITION TABLES 
#ADD COLUMN OF REGION TO THE INCIDENCE OBJECTS
IncTxBreast_attrition_table['Cancer']='Breast'
IncTxBreast_attrition_table <- IncTxBreast_attrition_table[,c(4,1,2,3)]
IncTxProstate_attrition_table['Cancer']='Prostate'
IncTxProstate_attrition_table <- IncTxProstate_attrition_table[,c(4,1,2,3)]

attrition_table <- rbind(IncTxBreast_attrition_table,IncTxProstate_attrition_table)

# 
# # gof results --------
# GOFResults <- GOFResults %>%
#   filter(Adjustment != "Age",  Adjustment != "Sex") %>% 
#   select(c(AIC, BIC, Method, Cancer, Database, Age, Sex)) %>% 
#   mutate(AIC = nice.num.count(AIC), BIC = nice.num.count(BIC))
# 
# # extrapolation parameters --------
# ExtrpolationParameters <- ExtrpolationParameters %>%
#   filter(Adjustment != "Age",  Adjustment != "Sex") %>% 
#   select(!c(sexMale, `age_gr18 to 39`, 
#             `age_gr40 to 49`, 
#             `age_gr50 to 59`, 
#             `age_gr60 to 69`,
#             `age_gr80 +`,
#             Adjustment, Stratification)) %>% 
#   mutate(across(where(is.numeric), ~ifelse(!is.na(.), nice.num3(.), .))) %>% 
#   select(Database, Cancer, Method, Sex, Age, order(names(.)), everything())
# 
# 
# # filter survival results for just stratification
# survivalResults <- survivalResults %>%
#   filter(Adjustment != "Age",  Adjustment != "Sex")
# 
# 





# med_surv_km <- medianResults %>% 
#   filter(Method == "Kaplan-Meier",
#          Adjustment == "None") %>% 
#   select(!c(Adjustment, 
#             rmean,
#             se,
#             median,
#             rmean10yr,
#             se10yr,
#             `surv year 1`,
#             `surv year 5`,
#             `surv year 10`
#             ))
# 
# hot_km <- hazOverTimeResults %>% 
#   filter(Method == "Kaplan-Meier")
# 
# #filter results for stratified results
# # survival_est_strat <- survival_estimates %>% 
# #   filter(Adjustment == "None" )
# 
# survival_est_strat <- survivalResults %>% 
#   filter(Adjustment == "None") %>% 
#   mutate(Method = as.factor(Method) %>% relevel(ref = "Kaplan-Meier"))
# 
# # filter for stratified gof
# goodness_of_fit_results_strat <- GOFResults %>% 
#   filter(Adjustment == "None" )
#   
# # filter for stratified extroplation parameters
# extrapolation_parameters_strat <- ExtrpolationParameters %>% 
#   filter(Adjustment == "None" )
# 
###############################################
##### taken from study code create table one

# # rename cancers with better formats
# tableone <- tableone %>% 
#   dplyr::mutate(group_level = replace(group_level, group_level == "Breastcancer", "Breast")) %>%
#   dplyr::mutate(group_level = replace(group_level, group_level == "Crc", "Colorectal")) %>%
#   dplyr::mutate(group_level = replace(group_level, group_level == "Hncancer", "Head and Neck")) %>%
#   dplyr::mutate(group_level = replace(group_level, group_level == "Livercancer", "Liver")) %>%
#   dplyr::mutate(group_level = replace(group_level, group_level == "Lungcancer", "Lung")) %>%
#   dplyr::mutate(group_level = replace(group_level, group_level == "Pancreaticcancer", "Pancreatic")) %>%
#   dplyr::mutate(group_level = replace(group_level, group_level == "Prostatecancer", "Prostate")) %>%
#   dplyr::mutate(group_level = replace(group_level, group_level == "Stomachcancer", "Stomach")) %>% 
#   dplyr::mutate(variable_level = replace(variable_level, variable_level == "Breastcancer", "Breast Cancer")) %>%
#   dplyr::mutate(variable_level = replace(variable_level, variable_level == "Crc", "Colorectal Cancer")) %>%
#   dplyr::mutate(variable_level = replace(variable_level, variable_level == "Hncancer", "Head and Neck Cancer")) %>%
#   dplyr::mutate(variable_level = replace(variable_level, variable_level == "Livercancer", "Liver Cancer")) %>%
#   dplyr::mutate(variable_level = replace(variable_level, variable_level == "Lungcancer", "Lung Cancer")) %>%
#   dplyr::mutate(variable_level = replace(variable_level, variable_level == "Pancreaticcancer", "Pancreatic Cancer")) %>%
#   dplyr::mutate(variable_level = replace(variable_level, variable_level == "Prostatecancer", "Prostate Cancer")) %>%
#   dplyr::mutate(variable_level = replace(variable_level, variable_level == "Stomachcancer", "Stomach Cancer"))
# 
# # tidy up the table ones
# # overall
# tableone_clean_temp <- list()
# for(tableonecancer in 1:length(unique(tableone$group_level))) {
#   
#   tabledata <- tableone %>%
#     dplyr::filter(group_level == unique(tableone$group_level)[tableonecancer]) %>% 
#     dplyr::filter(strata_name == "Overall")
#   
#   tb1_temp <- reformat_table_one(tabledata) %>% 
#     dplyr::mutate(Cancer = unique(tabledata$group_level),
#                   Stratification = "none",
#                   Sex = "Both" ,
#                   Age = "All" ,
#                   Database = db.name)
#   
#   
#   tableone_clean_temp[[tableonecancer]] <- tb1_temp
#   rm(tb1_temp)
#   
# }
# tableone_overall <- dplyr::bind_rows(tableone_clean_temp)
# 
# # by sex
# tableone_clean_temp <- list()
# for(tableonecancer in 1:length(unique(tableone$group_level))) {
#   
#   tabledata <- tableone %>%
#     dplyr::filter(group_level == unique(tableone$group_level)[tableonecancer]) %>% 
#     dplyr::filter(strata_name == "sex") 
#   
#   if(unique(tableone$group_level)[tableonecancer] != "Prostate") {
#     
#     tb1_tempF <- tabledata %>% 
#       dplyr::filter(strata_level == "Female") %>% 
#       reformat_table_one() %>% 
#       dplyr::mutate(Cancer = unique(tabledata$group_level),
#                     Stratification = "Sex",
#                     Sex = "Female" ,
#                     Age = "All" ,
#                     Database = db.name)
#     
#     tb1_tempM <- tabledata %>% 
#       dplyr::filter(strata_level == "Male") %>% 
#       reformat_table_one() %>% 
#       dplyr::mutate(Cancer = unique(tabledata$group_level),
#                     Stratification = "Sex",
#                     Sex = "Male" ,
#                     Age = "All" ,
#                     Database = db.name)
#     
#     #combine sexes together
#     tb1_temp <- dplyr::bind_rows(tb1_tempF , tb1_tempM)
#     
#     rm(tb1_tempF, tb1_tempM)
#     
#   } else {
#     
#     tb1_tempM <- tabledata %>% 
#       dplyr::filter(strata_level == "Male") %>% 
#       reformat_table_one() %>% 
#       dplyr::mutate(Cancer = unique(tabledata$group_level),
#                     Stratification = "Sex",
#                     Sex = "Male" ,
#                     Age = "All" ,
#                     Database = db.name)
#     
#     tb1_temp <- tb1_tempM
#     
#   }
#   
#   tableone_clean_temp[[tableonecancer]] <- tb1_temp
#   
#   rm(tb1_temp )
#   
# }
# tableone_sex <- dplyr::bind_rows(tableone_clean_temp) 
# 
# # by age
# tableone_clean_temp <- list()
# for(tableonecancer in 1:length(unique(tableone$group_level))) {
#   
#   tabledata <- tableone %>%
#     dplyr::filter(group_level == unique(tableone$group_level)[tableonecancer]) %>% 
#     dplyr::filter(strata_name == "age_gr") 
#   
#   tb1_temp_age <- list()
#   
#   for(z in 1:length(unique(tabledata$strata_level))) {
#     
#     # because some age groups do not have data need to have try catches to make sure loop still continues even if data not available
#     tryCatch(
#       {
#         tb1_temp_age[[z]] <- tabledata %>% 
#           dplyr::filter(strata_level == unique(tabledata$strata_level)[z]) %>% 
#           reformat_table_one() %>% 
#           dplyr::mutate(Cancer = unique(tabledata$group_level),
#                         Stratification = "Age",
#                         Sex = "Both",
#                         Age =  unique(tabledata$strata_level)[z] ,
#                         Database = db.name) %>% 
#           dplyr::filter(!stringr::str_detect(Description, 'Age Group:'))
#         
#       },
#       error = function(e) {
#         cat(conditionMessage(e), "Table one not carried out for ", unique(tableone$group_level)[tableonecancer], "see log for more information")
#         info(logger, paste0(" Table one not carried out for  ",unique(tableone$group_level)[tableonecancer], " ", e))
#         
#       },
#       warning = function(w){
#         cat(conditionMessage(e), "Warning problem with table one ", unique(tableone$group_level)[tableonecancer], "see log for more information")
#         info(logger, paste0(unique(tableone$group_level)[tableonecancer], ": ", w))}
#     )  
#   }
#   
#   tableone_clean_temp[[tableonecancer]] <- dplyr::bind_rows(tb1_temp_age)
#   
#   rm(tb1_temp_age)
# }
# tableone_age <- dplyr::bind_rows(tableone_clean_temp) 
# 
# # by age and sex
# tableone_clean_temp <- list()
# for(tableonecancer in 1:length(unique(tableone$group_level))) {
#   
#   tabledata <- tableone %>%
#     dplyr::filter(group_level == unique(tableone$group_level)[tableonecancer]) %>% 
#     dplyr::filter(strata_name == "sex and age_gr") 
#   
#   tb1_temp_age_sex <- list()
#   
#   for(z in 1:length(unique(tabledata$strata_level))) {
#     
#     # because some age groups do not have data need to have try catches to make sure loop still continues even if data not available
#     tryCatch(
#       {
#         tb1_temp_age_sex[[z]] <- tabledata %>% 
#           dplyr::filter(strata_level == unique(tabledata$strata_level)[z]) %>% 
#           reformat_table_one() %>% 
#           dplyr::mutate(Cancer = unique(tabledata$group_level),
#                         Stratification = "agesex",
#                         Sex = "Both",
#                         agesex =  unique(tabledata$strata_level)[z] ,
#                         Database = db.name) %>% 
#           dplyr::mutate(agesex = str_replace(agesex, " and ", "_")) %>% 
#           separate(col = "agesex",
#                    into = c("Sex", "Age"),
#                    sep = "_") %>% 
#           dplyr::filter(!stringr::str_detect(Description, 'Age Group:'))
#         
#       },
#       error = function(e) {
#         cat(conditionMessage(e), "Table one not carried out for ", unique(tableone$group_level)[tableonecancer], "see log for more information")
#         info(logger, paste0(" Table one not carried out for  ",unique(tableone$group_level)[tableonecancer], " ", e))
#         
#       },
#       warning = function(w){
#         cat(conditionMessage(e), "Warning problem with table one ", unique(tableone$group_level)[tableonecancer], "see log for more information")
#         info(logger, paste0(unique(tableone$group_level)[tableonecancer], ": ", w))}
#     )  
#   }
#   
#   tableone_clean_temp[[tableonecancer]] <- dplyr::bind_rows(tb1_temp_age_sex)
#   
#   rm(tb1_temp_age_sex)
# }
# tableone_age_sex <- dplyr::bind_rows(tableone_clean_temp) 
# 
# # combine all tableone outputs
# tableone_final <- dplyr::bind_rows(tableone_overall, tableone_sex, tableone_age, tableone_age_sex)


# reformat_table_one <- function(table_one_summary){
#   
#   reformatted_table1 <- data.frame(x = character(),  y = character())
#   
#   n1 <- table_one_summary %>% dplyr::filter(variable == "Number subjects") %>% dplyr::pull(estimate)
#   
#   reformatted_table1 <- dplyr::bind_rows(reformatted_table1,
#                                          data.frame(x = paste0("n"),
#                                                     y = paste0(n1)))
#   
#   # variables assembled by min/max etc
#   cat_var <- table_one_summary %>% dplyr::filter(estimate_type == "min") %>% dplyr::select(variable) %>% dplyr::distinct() %>% dplyr::pull(variable)
#   
#   for (i in (1:length(cat_var))){
#     reformatted_table1 <- dplyr::bind_rows(reformatted_table1,
#                                            data.frame(x = paste0(cat_var[[i]], ": median (IQR)"),
#                                                       y = paste0(table_one_summary %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "median") %>% dplyr::pull(estimate),
#                                                                  " (",
#                                                                  table_one_summary %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q25") %>% dplyr::pull(estimate),
#                                                                  " - ",
#                                                                  table_one_summary %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q75") %>% dplyr::pull(estimate),
#                                                                  ")"))
#     )
#   }
#   
#   # sex group variables
#   sex_var <- table_one_summary %>%
#     dplyr::filter(variable == "Sex") %>%
#     dplyr::select(variable_level) %>%
#     dplyr::distinct() %>%
#     dplyr::pull()
#   
#   for (i in (1:length(sex_var))){
#     reformatted_table1 <- dplyr::bind_rows(reformatted_table1, data.frame(x = paste0("Sex: ", sex_var[[i]], " n (%)"),
#                                                                           y = paste0(table_one_summary %>% dplyr::filter(variable_level == sex_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
#                                                                                      " (",
#                                                                                      round(as.numeric(table_one_summary %>% dplyr::filter(variable_level == sex_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
#                                                                                      ")")) )
#   }
#   
#   # age group variables
#   age_var <- table_one_summary %>%
#     dplyr::filter(variable == "Age group") %>%
#     dplyr::select(variable_level) %>%
#     dplyr::distinct() %>%
#     dplyr::pull()
#   
#   for (i in (1:length(age_var))){
#     reformatted_table1 <- dplyr::bind_rows(reformatted_table1, data.frame(x = paste0("Age Group: ", age_var[[i]], " n (%)"),
#                                                                           y = paste0(table_one_summary %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
#                                                                                      " (",
#                                                                                      round(as.numeric(table_one_summary %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
#                                                                                      ")")) )
#   }
#   
#   
#   #condition variables
#   condition_var <- table_one_summary %>%
#     dplyr::filter(stringr::str_detect(variable, 'Conditions flag -inf')) %>%
#     dplyr::select(variable_level) %>%
#     dplyr::distinct() %>%
#     dplyr::pull(variable_level)
#   
#   if(length(condition_var) != 0) {
#     for (i in (1:length(condition_var))){
#       reformatted_table1 <- dplyr::bind_rows(reformatted_table1, data.frame(x = paste0(condition_var[[i]], " n (%)"),
#                                                                             y = paste0(table_one_summary %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
#                                                                                        " (",
#                                                                                        round(as.numeric(table_one_summary %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
#                                                                                        ")")))
#     }
#   }
#   
#   #obesity variables
#   obesity_var <- table_one_summary %>%
#     dplyr::filter(stringr::str_detect(variable, 'Obesity flag -inf')) %>%
#     dplyr::select(variable_level) %>%
#     dplyr::distinct() %>%
#     dplyr::pull(variable_level)
#   
#   if(length(obesity_var) != 0) {
#     for (i in (1:length(obesity_var))){
#       reformatted_table1 <- dplyr::bind_rows(reformatted_table1, data.frame(x = paste0(obesity_var[[i]], " n (%)"),
#                                                                             y = paste0(table_one_summary %>% dplyr::filter(variable_level == obesity_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
#                                                                                        " (",
#                                                                                        round(as.numeric(table_one_summary %>% dplyr::filter(variable_level == obesity_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
#                                                                                        ")")))
#     }
#   }
#   
#   #medication variables
#   medication_var <- table_one_summary %>%
#     dplyr::filter(stringr::str_detect(variable, 'Medications flag -365')) %>%
#     dplyr::select(variable_level) %>%
#     dplyr::distinct() %>%
#     dplyr::pull(variable_level)
#   
#   
#   if(length(medication_var) != 0) {
#     for (i in (1:length(medication_var))){
#       reformatted_table1 <- dplyr::bind_rows(reformatted_table1, data.frame(x = paste0(medication_var[[i]], " n (%)"),
#                                                                             y = paste0(table_one_summary %>% dplyr::filter(variable_level == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
#                                                                                        " (",
#                                                                                        round(as.numeric(table_one_summary %>% dplyr::filter(variable_level == medication_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
#                                                                                        ")")))
#     } 
#   }
#   
#   #cancer outcomes  
#   outcome_var <- table_one_summary %>%
#     dplyr::filter(stringr::str_detect(variable, 'Outcome flag 0 to 0')) %>%
#     dplyr::select(variable_level) %>%
#     dplyr::distinct() %>%
#     dplyr::pull(variable_level)
#   
#   
#   if(length(outcome_var) != 0) {
#     for (i in (1:length(outcome_var))){
#       reformatted_table1 <- dplyr::bind_rows(reformatted_table1, data.frame(x = paste0(outcome_var[[i]], " n (%)"),
#                                                                             y = paste0(table_one_summary %>% dplyr::filter(variable_level == outcome_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
#                                                                                        " (",
#                                                                                        round(as.numeric(table_one_summary %>% dplyr::filter(variable_level == outcome_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
#                                                                                        ")"))) 
#       
#     } 
#   }
#   
#   
#   reformatted_table1 <- reformatted_table1 %>% dplyr::distinct()
#   
#   ###rename columns
#   colnames(reformatted_table1) <- c("Description", "Value") 
#   
#   return(reformatted_table1)
#   
# }