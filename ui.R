# #### UI -----

# ui shiny ----
ui <- dashboardPage(
  dashboardHeader(
    title = div("Menu", style = "text-align: left;"),  # Align title to the left
    titleWidth = 250  # Adjust the width as needed
  ),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Background",
        tabName = "background"
      ),
        menuSubItem(
          text = "Description",
          tabName = "database_details"
        )
      ),
      menuItem(
        text = "Cohorts",
        tabName = "cohorts",
        menuSubItem(
          text = "Cohort concepts",
          tabName = "cohort_concepts"
        ),
        menuSubItem(
          text = "Cohort attrition",
          tabName = "cohort_attrition"
        )
      ),
      menuItem(
        text = "Characteristics",
        tabName = "char",
        menuSubItem(
          text = "Overall Demographics",
          tabName = "demographics"
        ),
        menuSubItem(
          text = "Characterisation",
          tabName = "tableone"
        )

      ),
      
      # menuItem(
      #   text = "Survival Extrapolation",
      #   tabName = "os",
      #   menuSubItem(
      #     text = "Survival Functions",
      #     tabName = "survival_results"
      #   ),
      #   menuSubItem(
      #     text = "Hazard Functions",
      #     tabName = "hazard_results"
      #   ),
      #   
      #   menuSubItem(
      #     text = "Observed v predicted",
      #     tabName = "predicted_results"
      #   ),
      #   
      #   menuSubItem(
      #     text = "Goodness of Fit",
      #     tabName = "gof_results"
      #   ),
      #   
      #   menuSubItem(
      #     text = "Parameters",
      #     tabName = "parameters_results"
      #   ) ),
      
      # Logo 
      tags$div(
        style = "position: relative; margin-top: -10px; text-align: center; margin-bottom: 0;",
        a(img(
          src = "logoOxford.png",  # Replace with the correct file name and extension
          height = "150px",  # Adjust the height as needed
          width = "auto"     # Let the width adjust proportionally
        ),
        href = "https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology",
        target = "_blank"
        )
    )
  ),
  
  ## body ----
  dashboardBody(
    use_theme(mytheme),
    tabItems(
      # background  ------
      tabItem(
        tabName = "background",
        h3("Long term extrapolation of overall survival for common cancers: a multinational cohort study"),
        tags$h4(tags$strong("Please note, the results presented here should be considered as
                                                preliminary and subject to change.")),
        
        tags$h5(
          "This app is a companion to the study focussing on long term survival extrapolation for eight different cancers
                          (Breast, Colorectal, Lung, Liver, Stomach, Head & Neck, Prostate, and Pancreas) for a variety of different electronic health records and cancer registries across Europe (Spain, Netherlands, Germany, Norway, Finland, Portugal, Estonia, Switzerland, and the United Kingdom)."),
        tags$h5(
          "In the following pages you can find information on the prediction of long term survival using different extrapolation methods. Observed and predicted, median and mean survival and survival as one, five and ten years. A description of the characteristics of the study populations and attrition is also reported. 
                  All results have been performed for the whole population and for each age group (10 year age bands) and also for each sex (apart from prostate cancer)."),
        
        # HTML('<br>'),
        
        tags$h5("The results of this study are published in the following journal:"
        ),
        tags$ol(
          tags$li(strong("TBC"),"(",tags$a(href="https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology","Paper Link"),")" )),
        
        tags$h5("The analysis code used to generate these results can be found",
                tags$a(href="https://github.com/oxford-pharmacoepi", "here"),
                ".The cohort diagnostics including the clinical codelists for each of the 8 cancers can be found",
                tags$a(href="https://dpa-pde-oxford.shinyapps.io/CancerExtrapolationDiagnostics/", "here")
                
        ),
        
        tags$h5("Any questions regarding these results or problems with this shiny app please contact",
                tags$a(href="mailto:danielle.newby@ndorms.ox.ac.uk", "Danielle Newby")
                
        ),
        
        
        tags$hr()
        
        
      ),
      
      
      tabItem(
        tags$h5("Clinical codelists for cancers"),
        tabName = "cohort_concepts",
        htmlOutput('tbl_codelists'),
        tags$hr(),
        div(
          style = "display:inline-block",
          downloadButton(
            outputId = "gt_codelists_word",
            label = "Download table as word"
          ),
          style = "display:inline-block; float:right"
        )
      ) , 
      
      tabItem(
        tabName = "cohort_attrition",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "attrition_cohort_name_selector",
            label = "Study cohort",
            choices = unique(attrition_table$Cancer),
            selected = "Breast",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        htmlOutput('dt_cohort_attrition'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_cohort_attrition_word",
              label = "Download table as word"
            ), 
            style="display:inline-block; float:right")
        
      )
      # tabItem(
      #   tabName = "demographics",
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "demographics_database_name_selector",
      #       label = "Database",
      #       choices = unique(attritioncdm$Database),
      #       selected = unique(attritioncdm$Database),
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "demographics_sex_selector",
      #       label = "Sex",
      #       choices = unique(demographics$Sex),
      #       selected = "Both",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = FALSE
      #     )
      #   ),
      #   
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "demographics_age_selector",
      #       label = "Age",
      #       choices = unique(demographics$Age),
      #       selected = "All",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = FALSE
      #     )
      #   ),
      #   htmlOutput('dt_demographics'),
      #   
      #   div(style="display:inline-block",
      #       downloadButton(
      #         outputId = "gt_demographics_word",
      #         label = "Download demographics as word"
      #       ), 
      #       style="display:inline-block; float:right")
      #   
      # ) ,
      # 
      # tabItem(
      #   tabName = "tableone",
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "tableone_database_name_selector",
      #       label = "Database",
      #       choices = unique(attritioncdm$Database),
      #       selected = unique(attritioncdm$Database),
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "tableone_cohort_name_selector",
      #       label = "Cancer",
      #       choices = unique(tableone_final$Cancer),
      #       selected = "Breast",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = FALSE
      #     )
      #   ),
      #   
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "tableone_sex_selector",
      #       label = "Sex",
      #       choices = unique(tableone_final$Sex),
      #       selected = "Both",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = FALSE
      #     )
      #   ),
      #   
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "tableone_age_selector",
      #       label = "Age",
      #       choices = unique(tableone_final$Age),
      #       selected = "All",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = FALSE
      #     )
      #   ),
      #   
      #   htmlOutput('dt_tableone'),
      #   
      #   div(style="display:inline-block",
      #       downloadButton(
      #         outputId = "gt_tableone_word",
      #         label = "Download table as word"
      #       ), 
      #       style="display:inline-block; float:right")
      #   
      # ),
      # 
      # tabItem(
      #   tabName = "survival_results",
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "survival_database_selector",
      #       label = "Database",
      #       choices = unique(survivalResults$Database),
      #       selected = unique(survivalResults$Database),
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "survival_cohort_name_selector",
      #       label = "Cancer",
      #       choices = unique(survivalResults$Cancer),
      #       selected = "Breast",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "survival_sex_selector",
      #       label = "Sex",
      #       choices = unique(survivalResults$Sex),
      #       selected = "Both",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "survival_age_selector",
      #       label = "Age",
      #       choices = unique(survivalResults$Age),
      #       selected = "All",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "survival_method_selector",
      #       label = "Method",
      #       choices = unique(survivalResults$Method),
      #       selected = unique(survivalResults$Method),
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   
      #   div(style="display: inline-block;vertical-align:top; width: 150px;",
      #       pickerInput(inputId = "surv_plot_facet",
      #                   label = "Facet by",
      #                   choices = c("Cancer",
      #                               "Database",
      #                               "Sex",
      #                               "Age"
      #                   ),
      #                   selected = c("Cancer", "Database" ),
      #                   options = list(
      #                     `actions-box` = TRUE,
      #                     size = 10,
      #                     `selected-text-format` = "count > 3"),
      #                   multiple = TRUE,)
      #   ),
      #   div(style="display: inline-block;vertical-align:top; width: 150px;",
      #       pickerInput(inputId = "surv_plot_group",
      #                   label = "Colour by",
      #                   choices = c("Sex",
      #                               "Age",
      #                               "Cancer",
      #                               "Method"),
      #                   selected = c("Method"),
      #                   options = list(
      #                     `actions-box` = TRUE,
      #                     size = 10,
      #                     `selected-text-format` = "count > 3"),
      #                   multiple = TRUE,)
      #   ),
      #   
      #   plotOutput("survivalPlot", width = "85%", height = "75vh"),
      #   
      # ),
      # 
      # 
      # tabItem(
      #   tabName = "hazard_results", # needs to link up to the side bar menu tab names
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "hot_database_selector",
      #       label = "Database",
      #       choices = unique(hazOverTimeResults$Database),
      #       selected = unique(hazOverTimeResults$Database),
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "hot_cohort_name_selector",
      #       label = "Cancer",
      #       choices = unique(hazOverTimeResults$Cancer),
      #       selected = "Breast",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "hot_sex_selector",
      #       label = "Sex",
      #       choices = unique(hazOverTimeResults$Sex),
      #       selected = "Both",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = FALSE
      #     )
      #   ),
      #   
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "hot_age_selector",
      #       label = "Age",
      #       choices = unique(hazOverTimeResults$Age),
      #       selected = "All",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = FALSE
      #     )
      #   ),
      #   
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "hot_method_selector",
      #       label = "Method",
      #       choices = unique(hazOverTimeResults$Method),
      #       selected = unique(hazOverTimeResults$Method),
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   
      #   div(style="display: inline-block;vertical-align:top; width: 150px;",
      #       pickerInput(inputId = "hot_plot_facet",
      #                   label = "Facet by",
      #                   choices = c("Cancer",
      #                               "Database",
      #                               "Sex",
      #                               "Age"
      #                   ),
      #                   selected = c("Cancer", "Database" ),
      #                   options = list(
      #                     `actions-box` = TRUE,
      #                     size = 10,
      #                     `selected-text-format` = "count > 3"),
      #                   multiple = TRUE,)
      #   ),
      #   div(style="display: inline-block;vertical-align:top; width: 150px;",
      #       pickerInput(inputId = "hot_plot_group",
      #                   label = "Colour by",
      #                   choices = c("Sex",
      #                               "Age",
      #                               "Cancer",
      #                               "Method"),
      #                   selected = c("Method"),
      #                   options = list(
      #                     `actions-box` = TRUE,
      #                     size = 10,
      #                     `selected-text-format` = "count > 3"),
      #                   multiple = TRUE,)
      #   ),
      #   
      #   plotOutput("hotPlot", width = "85%", height = "75vh"),
      #   
      # ),  
      # 
      # 
      # tabItem(
      #   tabName = "gof_results",
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "gof_database_selector",
      #       label = "Database",
      #       choices = unique(GOFResults$Database),
      #       selected = unique(GOFResults$Database),
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "gof_cohort_name_selector",
      #       label = "Cancer",
      #       choices = unique(GOFResults$Cancer),
      #       selected = "Breast",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "gof_sex_selector",
      #       label = "Sex",
      #       choices = unique(GOFResults$Sex),
      #       selected = "Both",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = FALSE
      #     )
      #   ),
      #   
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "gof_age_selector",
      #       label = "Age",
      #       choices = unique(GOFResults$Age),
      #       selected = "All",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = FALSE
      #     )
      #   ),
      #   htmlOutput('dt_gof'),
      #   
      #   div(style="display:inline-block",
      #       downloadButton(
      #         outputId = "gt_gof_word",
      #         label = "Download gof results as word"
      #       ), 
      #       style="display:inline-block; float:right")
      # ),
      # 
      # tabItem(
      #   tabName = "parameters_results",
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "param_database_selector",
      #       label = "Database",
      #       choices = unique(ExtrpolationParameters$Database),
      #       selected = unique(ExtrpolationParameters$Database),
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "param_cohort_name_selector",
      #       label = "Cancer",
      #       choices = unique(ExtrpolationParameters$Cancer),
      #       selected = "Breast",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "param_sex_selector",
      #       label = "Sex",
      #       choices = unique(ExtrpolationParameters$Sex),
      #       selected = "Both",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   
      #   div(
      #     style = "display: inline-block;vertical-align:top; width: 150px;",
      #     pickerInput(
      #       inputId = "param_age_selector",
      #       label = "Age",
      #       choices = unique(ExtrpolationParameters$Age),
      #       selected = "All",
      #       options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
      #       multiple = TRUE
      #     )
      #   ),
      #   
      #   DTOutput("dt_param"),
      #   #htmlOutput('dt_param'),
      #   
      #   div(style="display:inline-block",
      #       downloadButton(
      #         outputId = "gt_param_word",
      #         label = "Download model parameters as word"
      #       ), 
      #       style="display:inline-block; float:right")
      # )
      # 
      # # more tabs here
    
    
  )  
  
  )
)






