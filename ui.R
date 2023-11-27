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
        tabName = "characteristics",
        menuSubItem(
          text = "Breast Characteristics",
          tabName = "breast_characteristics"
        ),
        menuSubItem(
          text = "Prostate Characteristics",
          tabName = "prostate_characteristics"
        )

      ),
    menuItem(
      text = "Incidence Rates and Ratios",
      tabName = "incidence_rates",
      menuSubItem(
        text = "Incidence Rates",
        tabName = "IR_tables"),
      menuSubItem(
        text = "Number of Events",
        tabName = "N_EVENTS_tables"),
        menuSubItem(
          text = "Incidence Rate Ratio",
          tabName = "IRR_tables"
        )
    ),
      
    
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
        h3("Collateral effects of the COVID-19 pandemic on endocrine treatments for breast and prostate cancer in the UK: 
           Implications for bone health"),
        tags$h4(tags$strong("Presented here are the supplementary tables to accompany the manuscript submitted to 
                            The Journal of the National Cancer Institute: Cancer Spectrum.")),
        
        tags$h5(
          "This app shows: 1) concepts used to define breast and prostate cancer; endocine treatments for 
          breast and prostate cancer; and endocrine treatment-related side-effects including bisphosphonate 
          prescriptions, osteopenia, and osteoporosis. 2) Characterisations of breast and prostate cancer
          patients on endocrine treatments. 3) Incidence rates and incidence rate ratios of endocrine treatment
          use and treatment-related outcomes before, during and after the COVID-19 pandemic. The study forcusses on
          electronic health record data from the UK."),
       
        # HTML('<br>'),
        
        tags$h5("The results of this study are under peer review. A pre-print of the manuscript can be found on MedrXiv here:"),
        tags$ol(
          tags$li(strong("Collateral effects of the COVID-19 pandemic on endocrine treatments for breast and prostate cancer in the UK: implications for bone health"),"(",tags$a(href="https://www.medrxiv.org/content/10.1101/2023.11.09.23298305v1","Paper Link"),")" )),
        
        tags$h5("The analysis code used to generate these results can be found",
                tags$a(href="https://github.com/oxford-pharmacoepi/CancerCovidEndocrineTx", "here"),
                 ),
        
        tags$h5("Any questions regarding these results or problems with this shiny app please contact",
                tags$a(href="mailto:nicola.barclay@ndorms.ox.ac.uk", "Nicola Barclay")
                
        ),
        
        
        tags$hr()
        
        
      ),
      
      
      tabItem(
        tags$h5("Clinical codelists for breast and prostate cancers; endocrine treatments; and treatment related outcomes"),
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
        tags$h5("Breast / prostate cancer patient starting counts, final counts after applying excision criteria, and reasons for exclusion"),
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
      
      ,
      

      tabItem(
        tabName = "IR_tables",
        tags$h5("Incidence Rates (95% confidence intervals) per 100,000 person months of endocrine treatments in breast/prostate cancer patients; and treatment related outcomes"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "IR_tables_cohort_name_selector",
            label = "Study cohort",
            choices = unique(IR_tables$Group),
            selected = "Breast Cancer Patients",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),

        htmlOutput('dt_ir_tables'),

        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_ir_tables_word",
              label = "Download table as word"
            ),
            style="display:inline-block; float:right")

      ),
      
      tabItem(
        tabName = "N_EVENTS_tables",
        tags$h5("Number of events and person months (in parentheses) of endocrine treatments in breast/prostate cancer patients; and treatment related outcomes, used to calcuate incidence rates"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "N_EVENTS_tables_cohort_name_selector",
            label = "Study cohort",
            choices = unique(N_EVENTS_tables$Group),
            selected = "Breast Cancer Patients",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        htmlOutput('dt_n_events_tables'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_n_events_tables_word",
              label = "Download table as word"
            ),
            style="display:inline-block; float:right")
        
      ),
      
      
      tabItem(
        tabName = "IRR_tables",
        tags$h5("Incidence Rate Ratios (95% confidence intervals) of endocrine treatments in breast/prostate cancer patients; and treatment related outcomes, across lockdown periods relative to pre-pandemic rates"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "IRR_tables_cohort_name_selector",
            label = "Study cohort",
            choices = unique(IRR_tables$Group),
            selected = "Breast Cancer Patients",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        htmlOutput('dt_irr_tables'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_irr_tables_word",
              label = "Download table as word"
            ),
            style="display:inline-block; float:right")
        
      ),
      
      

      ### breast characteristics
      tabItem(
        tags$h5("Characteristics of Breast Cancer Patients on different Endocrine Treatments"),
        tabName = "breast_characteristics",
        htmlOutput('tbl_breast_characteristics'),
        tags$hr(),
        div(
          style = "display:inline-block",
          downloadButton(
            outputId = "gt_breast_characteristics_word",
            label = "Download table as word"
          ),
          style = "display:inline-block; float:right"
        )
      ) ,
      # ### prostate characteristics
      tabItem(
        tags$h5("Characteristics of Prostate Cancer Patients on different Endocrine Treatments"),
        tabName = "prostate_characteristics",
        htmlOutput('tbl_prostate_characteristics'),
        tags$hr(),
        div(
          style = "display:inline-block",
          downloadButton(
            outputId = "gt_prostate_characteristics_word",
            label = "Download table as word"
          ),
          style = "display:inline-block; float:right"
        )
      )


    
  )  
  
  )
)





