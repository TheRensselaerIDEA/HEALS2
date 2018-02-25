# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# All the inputs that users choose will be automatically saved to 'input' and if you use a $ after input,
# you will see all the user interface input data.

library(tidyverse) # the only real package
library(broom)     # make model output aesthetic
library(survey)    # survey analysis
library(shiny)     # interactive web apps
library(shinyBS)   # mouseover text in shiny stuff

#The real shiny app function
shinyServer(function(input, output, session) {
  
  ## load all the datafiles
  path <- "analytic/"
  list.files(path=path, pattern='.csv') -> dataFileNames
  dataFileNames %>%
    lapply(function(x) suppressMessages(read_csv(paste0(path, x), col_types = cols(.default = "d")))) -> dataFiles
  names(dataFiles) <- dataFileNames
  rm(dataFileNames)
  ## load names and do some munging
  nhanes_variables <- read_csv("allNames.csv") %>%
    select(variable_name, variable_description, data_file_description, component) %>%
    filter(!str_detect(data_file_description, 'Special')) %>%
    mutate(temp = tolower(iconv(variable_description, 'UTF-8', 'ASCII'))) %>%
    distinct(variable_name, .keep_all=TRUE) %>%
    select(-temp)
  
  ## perform analysis for a category of variables (e.g., all phthalates measured together)
  ## tags should be a vector, e.g. c('URX', 'LBX')
  ## exceptions are column vector like c('URXUCR', 'LBXCOT') that look like study variables but aren't
  analysisCategory <- function(disease, responseModel, controls, dataFile, tags, exceptions){
    options(survey.lonely.psu="adjust") # do this to avoid failures in precision cohort -- debatable statistical validity I think
    ## perform analysis for a single study variable
    analysisIndividual <- function(design, studyVar, responseModel){
      print(c(studyVar, controls))
      if(sum(is.na(analyticSet[,studyVar])) > 0){
        return(tibble())
      }
      form <- paste(disease, paste(c(controls, studyVar), collapse='+'), sep='~')
      if(responseModel %in% c('quasibinomial')){
        svyglm(form, design=design, family=quasibinomial()) %>%
          tidy() %>%
          filter(term == studyVar) %>%
          mutate(nPos = sum(analyticSet[,disease]==1),
                 nObs = nrow(analyticSet)) %>%
          return()
      } else{
        svyglm(form, design=design, family=gaussian()) %>%
          tidy() %>%
          filter(term == studyVar) %>%
          mutate(nObs = nrow(analyticSet)) %>%
          return()
      }
    }
    ## remove control variables not found in this dataset and add in creatinine if needed
    controls <- intersect(controls, names(dataFile))
    ## pre-process urinary and blood variables
    urineVars <- names(dataFile)[str_detect(names(dataFile), 'URX')]
    bloodVars <- names(dataFile)[str_detect(names(dataFile), 'LBX')]
    dataFile[,c(urineVars, bloodVars)] <- scale(log(dataFile[,c(urineVars, bloodVars)] + 1e-5))
    ## construct vector of study variables
    studyVars <- names(dataFile)[str_detect(names(dataFile), str_c(tags, collapse='|'))]
    studyVars <- setdiff(studyVars, c(disease, controls, exceptions))
    ## construct study cohort
    analyticSet <- dataFile %>% filter(wt > 0)
    ## are we doing precision cohort analysis?
    if(input$pre_coh == TRUE){
      ## construct precision analysis cohort
      if('begin_year' %in% names(analyticSet)){
        analyticSet <- analyticSet %>% filter(begin_year %in% input$cohort)
      }
      if('RIAGENDR' %in% names(analyticSet)){
        analyticSet <- analyticSet %>% filter(RIAGENDR %in% input$gender)
      }
      if('RIDRETH1' %in% names(analyticSet)){
        analyticSet <- analyticSet %>% filter(RIDRETH1 %in% input$ethnicity)
      }
      if('RIDAGEYR' %in% names(analyticSet)){
        analyticSet <- analyticSet %>%
          mutate(age_cat = case_when(
            .$RIDAGEYR < 18 ~ 'child',
            .$RIDAGEYR < 40 ~ 'young_adult',
            .$RIDAGEYR < 65 ~ 'old_adult',
            TRUE ~ 'elderly')) %>%
          filter(age_cat %in% input$age_cat)
      }
      if('BMXBMI' %in% names(analyticSet)){
        analyticSet <- analyticSet %>%
          mutate(bmi_cat = case_when(
            .$BMXBMI <= 15 ~ 'VSU',
            .$BMXBMI <= 16 ~ 'SU',
            .$BMXBMI <= 18.5 ~ 'MU',
            .$BMXBMI <= 25 ~ 'N',
            .$BMXBMI <= 30 ~ 'Ov',
            .$BMXBMI <= 35 ~ 'MO',
            .$BMXBMI <= 40 ~ 'SO',
            TRUE ~ 'VSO')) %>%
          filter(bmi_cat %in% bmi_cats())
      }
    }
    ## perform survey modeling
    design <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~wt, data=analyticSet, nest=TRUE)
    lapply(studyVars, function(studyVar) analysisIndividual(design, studyVar, responseModel)) %>%
      bind_rows() %>%
      return()
  }
  ## helper function to assign proper names to things
  formalName <- reactive({switch(input$Disease,
                                 breast = 'breast cancer',
                                 corHeart = 'coronary heart disease',
                                 diabetes = 'diabetes-2',
                                 thyroid = 'thyroid condition',
                                 dbp = 'diastolic blood pressure',
                                 sbp = 'systolic blood pressure')})
  
  ## helper function to account for BMI class hierarchy
  bmi_cats <- reactive({bmi_cat <- input$bmi_cat;
                        if('Ob' %in% input$bmi_cat){bmi_cat <- union(bmi_cat, c('MO', 'SO', 'VSO'))};
                        if('U' %in% input$bmi_cat){bmi_cat <- union(bmi_cat, c('MU', 'SU', 'VSU'))};
                        bmi_cat
  })
  
  #Extract the risk factors part of the result table and rearrange the form
  ## try to use observeEvent rather than reactive
  riskfactors <- reactive({
    progress <- Progress$new(min=1, max=15)
    on.exit(progress$close())
    
    progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...')
    disease <- input$Disease
    print(disease)
    ## should we use suggested control variables?
    if(input$suggControlVar == TRUE){
      controls <- switch(disease,
                         breast = c('RIDAGEYR', 'BMXBMI', 'INDFMPIR', 'menarcheAge', 'menopause', 'URXUCR'),
                         corHeart = c('RIDAGEYR', 'RIAGENDR', 'RIDRETH1', 'INDFMPIR', 'BMXBMI', 'URXUCR'),
                         diabetes = c('RIDAGEYR', 'RIAGENDR', 'RIDRETH1', 'INDFMPIR', 'BMXBMI', 'URXUCR'),
                         thyroid = c('RIDAGEYR', 'RIAGENDR', 'RIDRETH1', 'BMXBMI', 'LBXCOT', 'URXUCR'),
                         dbp = c('RIDAGEYR', 'RIAGENDR', 'RIDRETH1', 'BMXBMI', 'URXUCR'),
                         sbp = c('RIDAGEYR', 'RIAGENDR', 'RIDRETH1', 'BMXBMI', 'URXUCR'))
    } else{
      controls <- input$ControlVar
    }
    ## what type of modeling do we need?
    continuousTargets <- c('sbp', 'dbp')
    binaryTargets <- c('breast', 'corHeart', 'diabetes', 'thyroid')
    responseModel <- if_else(disease %in% continuousTargets, 'gaussian', 'quasibinomial')
    ## helper variable for reading data
    diseaseTag = if_else(disease %in% binaryTargets, disease, 'bp')
    exposures <- input$Environmental
    ## extract the datafiles that correspond to the disease and then the exposures data
    specificFiles <- dataFiles[str_detect(names(dataFiles), diseaseTag)]
    specificFiles <- specificFiles[str_detect(names(specificFiles), str_c(exposures, collapse='|'))]
    ## change disease name to what's used in dataframes
    diseaseAlternate <- switch(disease,
                               breast = 'bc',
                               corHeart = 'hrtDis',
                               diabetes = 'diab',
                               thyroid = 'thyroidCond',
                               dbp = 'dbp',
                               sbp = 'sbp')
    print(specificFiles %>% names())
    lapply(specificFiles, function(x) 
      analysisCategory(diseaseAlternate, responseModel, controls, x, c('URX', 'LBX'), c('URXUCR', 'LBXCOT'))) %>%
      bind_rows() %>%
      rename(`Study Variable Code` = term,
             `Regression Coefficient` = estimate,
             `Std Error` = std.error,
             `p-value` = p.value) %>%
      select(-statistic) %>%
      distinct(`Study Variable Code`, .keep_all=TRUE) %>%
      mutate(`BH p-value` = p.adjust(`p-value`, method='fdr')) %>%
      select(-`p-value`) -> results
    ## link with full names
    results <- results %>%
      left_join(., nhanes_variables,
                by=c('Study Variable Code'='variable_name')) %>%
      rename(`Study Variable` = variable_description,
             Category = data_file_description)
    ## split into risk and mitigation factors
    riskFactors <- results %>%
      filter(`Regression Coefficient` > 0, `BH p-value` < input$threshold)
    mitiFactors <- results %>%
      filter(`Regression Coefficient` < 0, `BH p-value` < input$threshold)
    
    for (i in 1:15) {
      progress$set(value = i)
      Sys.sleep(0.5)
    }
    list(a=riskFactors, b=mitiFactors, c=results) %>% return()
  })
  
  # Show the values in an HTML table ----
  output$riskfactor <- renderDataTable({
    results <- riskfactors()
    results$a})
  output$mitifactor <- renderDataTable({
    results <- riskfactors()
    results$b})
  
  addPopover(session, 'riskfactor', placement='left', option=list(container='body'), title='Definitions',
             content=paste0('<b>regression coefficients</b> above zero indicate a positive association with the condition. <br/><br/>',
                         '<b>standard errors</b> quantify the uncertainty in the estimation of the regression coefficient. <br/><br/>',
                         '<b>p-values</b> give the probability of estimating a nonzero regression coefficient ratio by chance. <br/><br/>',
                         '<b>p-values</b> are corrected with the Benjamini-Hochberg false discovery rate scheme. <br/><br/>',
                         'study variable <b>categories</b> are from the analytics ontology.'))
  
  addPopover(session, 'mitifactor', placement='left', option=list(container='body'), title='Definitions',
             content=paste0('<b>regression coefficient</b> ratios below zero indicate a negative association with the condition. <br/><br/>',
                            '<b>standard errors</b> quantify the uncertainty in the estimation of the regression coefficient. <br/><br/>',
                            '<b>p-values</b> give the probability of estimating a nonzero regression coefficient ratio by chance. <br/><br/>',
                            '<b>p-values</b> are corrected with the Benjamini-Hochberg false discovery rate scheme. <br/><br/>',
                            'study variable <b>categories</b> are from the analytics ontology.'))
  
  addPopover(session, 'Source', placement='right', option=list(container='body'), title='Explanation',
             content=paste0('NHANES is the National Health and Nutrition Examination Survey.<br/><br/>',
                            'Currently, only NHANES is supported.'))
  addPopover(session, 'ControlVar', placement='right', option=list(container='body'), title='Definition and Recommendations',
             content=paste0('When looking for associations, it is often helpful to control for conditions or statuses. <br/><br/>',
                            'Recommended control variables for each condition: <br/>',
                            '<b>Breast Cancer:</b> Age, BMI, income, age at menarche, menopausal status <br/>',
                            '<b>Heart Disease:</b> Age, gender, ethnicity, socioeconomic status, BMI <br/>',
                            '<b>Diabetes-2:</b> Age, gender, ethnicity, socioeconomic status, BMI <br/>',
                            '<b>Thyroid Condition:</b> Age, gender, ethnicity, BMI, and serum cotinine <br/> <br/>',
                            "If study variables are measured in a subject's urine, urinary creatinine should be controlled for."))
  addPopover(session, 'Disease', placement='right', option=list(container='body'), title='Definitions',
             content=paste0('<b>Diabetes-2</b> is defined by exceeding a fasting blood glucose threshold. <br/>',
                            '<b>Breast cancer</b>, <b>heart disease</b>, and <b>thyroid condition</b> are defined by an answer on a survey subject questionnaire. ',
                            '<b>Diastolic</b> and <b>systolic blood pressure</b> are measured during an examination. <br/><br/>',
                            'Breast cancer, heart disease, thyroid condition, and diabetes-2 are binary responses, so they are modeled with <b>logistic regression</b>. ',
                            'Diastolic and systolic blood pressure are continuous responses, so they are modeled with <b>linear regression</b>.'))
  addPopover(session, 'suggControlVar', placement='right', option=list(container='body'), title='Explanation',
             content=paste0("If 'Yes' is selected, any user-specified control variables will be replaced with the systems' recommended control ",
                            "variables for the user's chosen disease. The chosen recommended control variables are derived from literature searches."))
  addPopover(session, 'Environmental', placement='right', option=list(container='body'), title='Explanation',
             content=paste0('These are classes of environmental exposure variables. The class definitions are taken from NHANES. ',
                            'Environmental exposure variables tend to be heavily right-skewed. Thus, log transformations are applied ',
                            'prior to analysis.'))
  addPopover(session, 'threshold', placement='right', option=list(container='body'), title='Significance Thresholds',
             content=paste0('A p-value threshold of 0.05 is commonly used to determine significance. As the threshold gets larger',
                            ' it is easier for a study variable to be deemed significant.'))
  addPopover(session, 'manhattanplot', placement='left', option=list(container='body'), title='Explanation',
            content=paste0('<b>p-values</b> above the line are significant at the chosen threshold'))
  addPopover(session,'logoddsplot', placement='left', option=list(container='body'), title='Explanation',
             content=paste0('<b>regression coefficients</b> above zero indicate a positive association with the condition. <br/><br/>',
                          '<b>regression coefficients</b> below zero indicate a negative association with the condition.'))
  addPopover(session, 'age_cat', placement='right', option=list(container='body'), title='Age category definitions:',
             content=paste0('<b>Children</b>: Less than 18 years old <br/>',
                            '<b>Younger adults</b>: 18 - 39 years old <br/>',
                            '<b>Older adults</b>: 40 - 64 years old <br/>',
                            '<b>Elderly</b>: At least 65 years old'))
  addPopover(session, 'bmi_cat', placement='right', option=list(container='body'), title='BMI category definitions:',
             content=paste0('Given BMIs are in kg/m^2.<br/>',
                            '<b>Very severely underweight</b>: BMI < 15 <br/>',
                            '<b>Severely underweight</b>: 15 < BMI < 16 <br/>',
                            '<b>Moderately underweight</b>: 16 < BMI < 18.5 <br/>',
                            '<b>Healthy weight</b>: 18.5 < BMI < 25 <br/>',
                            '<b>Overweight</b>: 25 < BMI <30 <br/>',
                            '<b>Moderately obese</b>: 30 < BMI < 35 <br/>',
                            '<b>Severely obse</b>: 35 < BMI < 40 <br/>',
                            '<b>Very severely obese</b>: BMI > 40 <br/>',
                            '<b>Underweight</b>: Very severely underweight, severely underweight, or moderately underweight <br/>',
                            '<b>Obese</b>: Very severely obese, severely obese, or moderately obese'))
  addPopover(session, 'cohort', placement='right', option=list(container='body'), title='Cohort Years:',
             content='Note: Not all study variables are found in all cohorts')
  
  output$warning <- renderText({'Warning: This application is a work in progress.'})
  
  output$text1 <- renderText({
    paste("The risk factors for", formalName(),'are:')
  })
  
  output$text3 <- renderText({
    paste("The mitigation factors for", formalName(),'are:')
  })  
  
  output$manhattanplot <- renderPlot({
    results <- riskfactors()
    results$c %>%
      ggplot() +
      geom_point(aes(x=reorder(`Study Variable Code`, order(Category)), y=-log10(`BH p-value`), color=Category)) +
      geom_hline(yintercept=-log10(input$threshold)) +
      theme(axis.text.x=element_blank()) +
      ggtitle('Manhattan Plot of Study p-values') +
      xlab('Study Variable')
  })
 
  output$logoddsplot <- renderPlot({
    results <- riskfactors()
    results$c %>%
      filter(`BH p-value` < input$threshold) %>%
      ggplot() +
      geom_point(aes(x=reorder(`Study Variable Code`, order(Category)), y=`Regression Coefficient`, color=Category)) +
      geom_errorbar(aes(x=reorder(`Study Variable Code`, order(Category)), ymin=`Regression Coefficient`-1.96*`Std Error`,
                        ymax=`Regression Coefficient`+1.96*`Std Error`, color=Category)) +
      geom_hline(yintercept=0) +
      theme(axis.text.x=element_text(hjust=1, vjust=1, angle=45)) +
      ggtitle('Significant Study Variable Regression Coefficients With 95% Confidence Intervals') +
      xlab('Study Variable Code')
  })
})

