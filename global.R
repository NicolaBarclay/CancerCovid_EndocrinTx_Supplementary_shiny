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


# clinical code lists
concepts_lists <- read_csv(here("www", "concept_list.csv"), show_col_types = FALSE)

# database details
database_details <- read_csv(here("www", "database_details.csv"), show_col_types = FALSE)

# attrition estimates data
IncTxBreast_attrition_estimates <- read_csv(here("data/2_EndocrineTxCancer/IncTxBreast_attrition_estimates.csv"))
IncTxProstate_attrition_estimates <- read_csv(here("data/2_EndocrineTxCancer/IncTxProstate_attrition_estimates.csv"))

# Incidence rates data
IR_Breast_endocrine <- read_csv(here("data/IRR results/EndoTxBreast/IR_table_breast_endo_with_pre_post_lockdown_pivot.csv"))
IR_Prostate_endocrine  <- read_csv(here("data/IRR results/EndoTxProstate/IR_table_prostate_endo_with_pre_post_lockdown_pivot.csv"))
IR_Breast_osteo <- read_csv(here("data/IRR results/OsteoDxBreast/IR_table_endodx_breastAI_with_pre_post_lockdown_pivot.csv"))
IR_Prostate_osteo <- read_csv(here("data/IRR results/OsteoDxProstate/IR_table_endodx_prostate_with_pre_post_lockdown_pivot.csv"))

# N events data
N_EVENTS_PM_breast_endoTx <- read_csv(here("data/IRR results/EndoTxBreast/N_EVENTS_PD_table_endoTx_breast.csv"))
N_EVENTS_PM_breast_endoTx_post <- read_csv(here("data/IRR results/EndoTxBreast/N_EVENTS_PM_table_endoTx_breast_POST.csv"))
N_EVENTS_PM_prostate_endoTx <- read_csv(here("data/IRR results/EndoTxProstate/N_EVENTS_PD_table_endo_prostate.csv"))
N_EVENTS_PM_prostate_endoTx_post <- read_csv(here("data/IRR results/EndoTxProstate/N_EVENTS_PM_table_endo_prostate_post.csv"))

N_EVENTS_PM_breastAI_endoDx <- read_csv(here("data/IRR results/OsteoDxBreast/N_EVENTS_PD_table_endodx_breastAI.csv"))
N_EVENTS_PM_breastAI_endoDx_post <- read_csv(here("data/IRR results/OsteoDxBreast/N_EVENTS_PM_table_endodx_breastAI.csv"))
N_EVENTS_PM_prostate_endoDx <- read_csv(here("data/IRR results/OsteoDxProstate/N_EVENTS_PD_table_endodx_prostate.csv"))
N_EVENTS_PM_prostate_endoDx_post <- read_csv(here("data/IRR results/OsteoDxProstate/N_EVENTS_PD_table_endodx_prostate_pre_post.csv"))

# IRR data
IRR_breast_endoTx <- read_csv(here("data/IRR results/EndoTxBreast/IRR_table_breast_endo.csv"))
IRR_breast_endoTx_post <- read_csv(here("data/IRR results/EndoTxBreast/IRR_table_breast_endo_post.csv"))
IRR_prostate_endoTx <- read_csv(here("data/IRR results/EndoTxProstate/IRR_table_prostate_endo.csv"))
IRR_prostate_endoTx_post <- read_csv(here("data/IRR results/EndoTxProstate/IRR_table_prostate_endo_post.csv"))

IRR_endoDx_breastAI <- read_csv(here("data/IRR results/OsteoDxBreast/IRR_table_endodx_breastAI.csv"))
IRR_endoDx_breastAI_post <- read_csv(here("data/IRR results/OsteoDxBreast/IRR_table_endodx_breastAI_POST.csv"))
IRR_endoDx_prostate <- read_csv(here("data/IRR results/OsteoDxProstate/IRR_table_endodx_prostate_ex_bf.csv"))
IRR_endoDx_prostate_post <- read_csv(here("data/IRR results/OsteoDxProstate/IRR_table_endodx_prostate_ex_bf_pre_post.csv"))

# ATTRITION TABLES ------------------------------------------------------------#
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

# CHARACTERISATION TABLES ------------------------------------------------------------#
Breast_characteristics_table <- Breast_characteristics_table[,-1]
Breast_characteristics_table <-  Breast_characteristics_table %>% rename("Variable" = var,
                                                                        "Aromatase Inhibitors" = AromataseInhibitors, 
                                                                         "Aromatase Inhibitors with GnRH Agonists Or Antagonists" = AromataseInhibitors_withGnRHAgonistsOrAntagonists_UPDATED,
                                                                         "Tamoxifen with GnRH Agonists Or Antagonists" = Tamoxifen_withGnRHAgonistsOrAntagonists_UPDATED)


Prostate_characteristics_table <- Prostate_characteristics_table[,-1] 
Prostate_characteristics_table <- Prostate_characteristics_table %>% rename("Variable" = var,
                                                                            "GNRH Agonists" = GNRH_Agonists,
                                                                            "GNRH Agonists with 1st Gen ADT" = GNRH_Agonists_with1stGenADT_UPDATED,
                                                                            "GNRH LHRH antagonists"= GNRH_LHRH_antagonists,
                                                                            "First generation antiandrogens" = First_generation_antiandrogens,
                                                                            "Second generation antiandrogens" = Second_generation_antiandrogens)
  
# INCIDENCE RATES TABLES ------------------------------------------------------------#
IR_Breast_endocrine['Group']='Breast Cancer Patients'
IR_Breast_endocrine <- IR_Breast_endocrine[,c(11,2,3,4,5,6,7,8,9,10)]
IR_Prostate_endocrine['Group']='Prostate Cancer Patients' 
IR_Prostate_endocrine <- IR_Prostate_endocrine[,c(11,2,3,4,5,6,7,8,9,10)]
IR_Breast_osteo['Group']='Breast Cancer Patients on Aromatase Inhibitors'
IR_Breast_osteo <- IR_Breast_osteo[-2,c(11,2,3,4,5,6,7,8,9,10)]
IR_Prostate_osteo['Group']='Prostate Cancer Patients on Endocrine Treatments'
IR_Prostate_osteo <- IR_Prostate_osteo[-2,c(11,2,3,4,5,6,7,8,9,10)]

# BIND THE TABLES

IR_tables <- rbind(IR_Breast_endocrine, IR_Prostate_endocrine, IR_Breast_osteo, IR_Prostate_osteo)


# NUMBER OF EVENTS TABLES ------------------------------------------------------------#
# remove all columns except last in post file
N_EVENTS_PM_breast_endoTx_post <- N_EVENTS_PM_breast_endoTx_post[,4]
#join with remainder of table
N_EVENTS_PM_breast_endoTx <- cbind(N_EVENTS_PM_breast_endoTx, N_EVENTS_PM_breast_endoTx_post)

N_EVENTS_PM_prostate_endoTx_post <- N_EVENTS_PM_prostate_endoTx_post[,4]
N_EVENTS_PM_prostate_endoTx <- cbind(N_EVENTS_PM_prostate_endoTx, N_EVENTS_PM_prostate_endoTx_post)

N_EVENTS_PM_breastAI_endoDx_post <- N_EVENTS_PM_breastAI_endoDx_post[,4]
N_EVENTS_PM_breastAI_endoDx <- cbind(N_EVENTS_PM_breastAI_endoDx, N_EVENTS_PM_breastAI_endoDx_post)

N_EVENTS_PM_prostate_endoDx_post <- N_EVENTS_PM_prostate_endoDx_post[,4]
N_EVENTS_PM_prostate_endoDx <- cbind(N_EVENTS_PM_prostate_endoDx, N_EVENTS_PM_prostate_endoDx_post)

# add a column to include group for filtering in the app
N_EVENTS_PM_breast_endoTx['Group']='Breast Cancer Patients' 
N_EVENTS_PM_prostate_endoTx['Group']='Prostate Cancer Patients' 
N_EVENTS_PM_breastAI_endoDx['Group']='Breast Cancer Patients on Aromatase Inhibitors'  
N_EVENTS_PM_prostate_endoDx['Group']='Prostate Cancer Patients on Endocrine Treatments' 

# reorder and remove superfluous columns or rows
N_EVENTS_PM_breast_endoTx <- N_EVENTS_PM_breast_endoTx[,c(11,2,3,4,5,6,7,8,9,10)]
N_EVENTS_PM_prostate_endoTx <- N_EVENTS_PM_prostate_endoTx[,c(11,2,3,4,5,6,7,8,9,10)]
N_EVENTS_PM_breastAI_endoDx <- N_EVENTS_PM_breastAI_endoDx[-2,c(11,2,3,4,5,6,7,8,9,10)]
N_EVENTS_PM_prostate_endoDx <- N_EVENTS_PM_prostate_endoDx[-2,c(11,2,3,4,5,6,7,8,9,10)]

# BIND THE TABLES

N_EVENTS_tables <- rbind(N_EVENTS_PM_breast_endoTx, N_EVENTS_PM_prostate_endoTx, N_EVENTS_PM_breastAI_endoDx, N_EVENTS_PM_prostate_endoDx)



# INCIDENCE RATE RATIO TABLES--------------------------------------------------#

# IRR data

IRR_breast_endoTx_post <- IRR_breast_endoTx_post[,3]
IRR_breast_endoTx <- cbind(IRR_breast_endoTx, IRR_breast_endoTx_post) 

IRR_prostate_endoTx_post <- IRR_prostate_endoTx_post[,3]
IRR_prostate_endoTx <- cbind(IRR_prostate_endoTx, IRR_prostate_endoTx_post)
  
IRR_endoDx_breastAI_post <- IRR_endoDx_breastAI_post[,3]
IRR_endoDx_breastAI <- cbind(IRR_endoDx_breastAI, IRR_endoDx_breastAI_post) 

IRR_endoDx_prostate_post <- IRR_endoDx_prostate_post[,3]
IRR_endoDx_prostate <- cbind(IRR_endoDx_prostate, IRR_endoDx_prostate_post) 


IRR_breast_endoTx['Group']='Breast Cancer Patients' 
IRR_prostate_endoTx['Group']='Prostate Cancer Patients'
IRR_endoDx_breastAI['Group']='Breast Cancer Patients on Aromatase Inhibitors'
IRR_endoDx_prostate['Group']='Prostate Cancer Patients on Endocrine Treatments'

# reorder and remove superfluous columns or rows
IRR_breast_endoTx <- IRR_breast_endoTx[,c(10,2,3,4,5,6,7,8,9)]
IRR_prostate_endoTx <- IRR_prostate_endoTx[,c(10,2,3,4,5,6,7,8,9)]
IRR_endoDx_breastAI <- IRR_endoDx_breastAI[-2,c(10,2,3,4,5,6,7,8,9)]
IRR_endoDx_prostate <- IRR_endoDx_prostate[,c(10,2,3,4,5,6,7,8,9)]

# BIND THE TABLES

IRR_tables <- rbind(IRR_breast_endoTx, IRR_prostate_endoTx, IRR_endoDx_breastAI, IRR_endoDx_prostate)


