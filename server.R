
options(encoding = "UTF-8")

#### SERVER ------
server <-	function(input, output, session) {
  
    # database details
  output$tbl_database_details <- renderText(kable(database_details) %>%
                                              kable_styling("striped", full_width = F) )
  
  
  output$gt_database_details_word <- downloadHandler(
    filename = function() {
      "database_description.docx"
    },
    content = function(file) {
      x <- gt(database_details)
      gtsave(x, file)
    }
  )
  
  # clinical codelists
  output$tbl_codelists <- renderText(kable(concepts_lists) %>%
                                       kable_styling("striped", full_width = F) )
  
  
  output$gt_codelists_word <- downloadHandler(
    filename = function() {
      "concept_lists.docx"
    },
    content = function(file) {
      x <- gt(concepts_lists)
      gtsave(x, file)
    }
  )
  
  # cohort attrition -----
  get_table_attrition <-reactive({
    
    table <- attrition_table %>% 
      filter(Cancer %in% input$attrition_cohort_name_selector)  
    
    table
  }) 
  
  output$dt_cohort_attrition <- renderText(kable(get_table_attrition()) %>%
                                             kable_styling("striped", full_width = F) )
  
  output$gt_cohort_attrition_word <- downloadHandler(
    filename = function() {
      "cdm_attrition.docx"
    },
    content = function(file) {
      x <- gt(get_table_attrition())
      gtsave(x, file)
    }
  )

 

  # Breast Characteristics --------

output$tbl_breast_characteristics <- renderText(kable(Breast_characteristics_table) %>%
                                       kable_styling("striped", full_width = F) )


output$gt_breast_characteristics_word <- downloadHandler(
  filename = function() {
    "breast_characteristics.docx"
  },
  content = function(file) {
    x <- gt(Breast_characteristics_table)
    gtsave(x, file)
  }
)


# Prostate Characteristics --------

output$tbl_prostate_characteristics <- renderText(kable(Prostate_characteristics_table) %>%
                                                  kable_styling("striped", full_width = F) )


output$gt_prostate_characteristics_word <- downloadHandler(
  filename = function() {
    "prostate_characteristics.docx"
  },
  content = function(file) {
    x <- gt(Prostate_characteristics_table)
    gtsave(x, file)
  }
)



  # # INCIDENCE RATE TABLES --------

  get_ir_tables <- reactive({

    table <- IR_tables %>%
      filter(Group %in% input$IR_tables_cohort_name_selector) 

    table

  })


  output$dt_ir_tables <- renderText(kable(get_ir_tables()) %>%
                                     kable_styling("striped", full_width = F) )


  output$gt_ir_tables_word <- downloadHandler(
    filename = function() {
      "ir_tables.docx"
    },
    content = function(file) {
      x <- gt(get_ir_tables())
      gtsave(x, file)
    }
  )
  
  # # N EVENT TABLES --------
  
  get_n_events_tables <- reactive({
    
    table <- N_EVENTS_tables %>%
      filter(Group %in% input$N_EVENTS_tables_cohort_name_selector) 
    
    table
    
  })
  
  
  output$dt_n_events_tables <- renderText(kable(get_n_events_tables()) %>%
                                      kable_styling("striped", full_width = F) )
  
  
  output$gt_n_events_tables_word <- downloadHandler(
    filename = function() {
      "n_event_tables.docx"
    },
    content = function(file) {
      x <- gt(get_n_events_tables())
      gtsave(x, file)
    }
  )
  
  # # INCIDENCE RATE RATIO TABLES --------
  
  get_irr_tables <- reactive({
    
    table <- IRR_tables %>%
      filter(Group %in% input$IRR_tables_cohort_name_selector) 
    
    table
    
  })
  
  
  output$dt_irr_tables <- renderText(kable(get_irr_tables()) %>%
                                      kable_styling("striped", full_width = F) )
  
  
  output$gt_irr_tables_word <- downloadHandler(
    filename = function() {
      "irr_tables.docx"
    },
    content = function(file) {
      x <- gt(get_irr_tables())
      gtsave(x, file)
    }
  )
  
  
 }