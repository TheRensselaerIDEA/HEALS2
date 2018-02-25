#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#

library(shinyBS)
library(readr)

shinyUI(navbarPage("NHANES RISK BROWSER",
                   
                   #First tab
                   tabPanel("Terms and Information",
                            mainPanel(
                              h1("Welcome to NHANES Risk Browser"),
                              h5("Warning: This application is a work in progress."),
                              br(),
                              h5("The NHANES Risk Browser provides the ability to select a disease or riskful health condition, a large number of factors that might impact this condition; then it analyzes their association with the selected disease and present the results in tabular and graphical formats."),
                              br(),
                              h5("The workflow is based on the Environment-Wide Association Study (Patel et al (2010) DOI:10.1371/journal.pone.0010746). The data source is the National Health and Nutrition Examination Survey."),
                              br(),
                              h4("The risk browser has several tabs:"),
                              h5("1. Population Analysis - Learn risk models for an entire population."),
                              h5("2. Precision Cohort Analysis - Define a more precise study cohort based on survey year, subject age, gender, ethnicity, or BMI, then learn risk models for that study cohort."),
                              h5("3. Cadre Analysis - Let the data and a more complicated modeling strategy discover informative subpopulations for particular conditions."),
                              br(),
                              h4("Notes:"),
                              h5("1. When doing precision cohort analysis, you have to return to the Population Analysis tab for results to be shown."),
                              h5("2. Because it requires lengthier calculation time, full cadre analysis is not supported. Instead, some interesting but pre-run results are shown."),
                              h5("3. It is possible to define precision cohorts that contain a very low number of positive observations. In these cases, regression coefficient estimates will be noisier."),
                              h5("4. It is possible to define precision cohorts that do not contain any observations at all.")
                              )),
                   
                   #Second tab
                   tabPanel("Population Analysis",
                            
                            # Edit the user selection panel
                            fluidRow(
                              column(3, wellPanel(
                                selectInput('Disease', label=h5('Diseases:'), 
                                            choices=list('Diabetes-2'='diabetes', 'Heart Disease'='corHeart',
                                                         'Breast Cancer'='breast', 'Thyroid Condition'='thyroid',
                                                         'Systolic blood pressure'='sbp', 'Diastolic blood pressure'='dbp'),
                                            selected='thyroid', selectize=TRUE),
                                selectInput("Source", label = h5("Data Sources:"), 
                                            choices = list("NHANES DATA")),
                                selectInput('ControlVar',label = h5('Control Variables:'),
                                            choices = c('Age'='RIDAGEYR','Gender'='RIAGENDR','Race'='RIDRETH1','BMI'='BMXBMI','Income'='INDFMPIR',
                                                        'Cohort Year'='begin_year', 'Urinary Creatinine'='URXUCR', 'Age at Menarche'='menarcheAge',
                                                        'Menopausal Status'='menopause', 'Serum Cotinine'='LBXCOT'), 
                                            selected=c('RIDAGEYR', 'BMXBMI'), multiple=TRUE, selectize=TRUE),
                                selectInput('suggControlVar', label=h5('Use expert-recommended control variables?'),
                                            choices = c('Yes' = TRUE, 'No' = FALSE), selected=FALSE, selectize=TRUE),
                                selectInput('Environmental', label = h5('Environmental Factors:'),
                                            choices =c('Arsenics'='ars', 'Dioxins, Furans, Coplanar PCBs'='dio', 'Heavy Metals'='met',
                                                       'Polyfluoroalkyl Chemicals'='pch', 'Pesticides--Current Use'='pcu', 
                                                       'Pesticides--Environmental'='pen', 'Environmental Phenols'='phe', 'Phthalates'='pht',
                                                       'Polyaromatic Hydrocarbons'='phy', 'Volatile Organic Compounds'='vol'), 
                                            selected=c('met', 'pht'), selectize=TRUE, multiple=TRUE),
                                numericInput('threshold', label = h5('p-value threshold for significance'), min=0, max=1, value=0.05, step=0.001),
                                br(),
                                submitButton("Apply Changes")
                              )),
                              
                              # Edit the output panel
                              column(9, wellPanel(
                                                  textOutput("warning"), br(), 
                                                   textOutput("text1"), dataTableOutput("riskfactor"), 
                                                   textOutput("text3"), dataTableOutput("mitifactor"), 
                                                   plotOutput('manhattanplot'),
                                                   plotOutput('logoddsplot'),
                                                   bsTooltip(id="don't look at me!", placement='left', title='can you see me?', trigger='click')
                                                   )),
                              
                              # Change the color, font size, etc of the output result
                              tags$head(tags$style(
                                 "#warning{color:red;font-size:15px;}",
                                 "#text1{color: red;font-size: 20px;}",
                                 "#text3{color: green;font-size: 20px;}",
                                 "#text4{color: black;font-size: 18px;}",
                                 "#source{color: Blue;font-size: 20px;}",
                                 "#diseases{color: black;font-size: 18px;}",
                                 "#datasource{color: black;font-size: 18px;}",
                                 "#riskfactor{align:center;}",
                                 "#mitifactor{align:center;}",
                                 "#text5{color: Blue;font-size: 20px;}",
                                 "#text6{color: Blue;font-size: 20px;}",
                                 "#text7{color: Blue;font-size: 20px;}",
                                 "#model_results{align:center}",
                                 "#AIC{align:center}",
                                 "#SampleCount{align:center}"
                                 ))
                            )),
                   #Third tab
                   tabPanel("Precision Cohort Analysis",
                            # Edit the user selection panel
                            fluidRow(
                              column(3, wellPanel(
                                selectInput('pre_coh', label=h5('Perform precision cohort analysis?'), selectize=TRUE,
                                            choices=c('Yes'=TRUE, 'No'=FALSE), selected=FALSE),
                                selectInput('age_cat', label=h5('Age Category'), selectize=TRUE, multiple=TRUE,
                                            choices=c('Children'='child', 'Younger Adults'='young_adult', 'Older Adults'='old_adult', 
                                                      'Elderly'='elderly'),
                                            selected=c('young_adult', 'old_adult')),
                                selectInput('gender', label=h5('Gender'), choices=list('Male'=1, 'Female'=2), 
                                            selected=c(1, 2), selectize=TRUE, multiple=TRUE),
                                selectInput('ethnicity', label=h5('Ethnicity'), selectize=TRUE, multiple=TRUE,
                                            choices=list('Mexican American'=1, 'Other Hispanic'=2, 'Non-Hispanic White'=3,
                                                         'Non-Hispanic Black'=4, 'Other'=5),
                                            selected=c(1, 2)),
                                selectInput('bmi_cat', label=h5('BMI Category'), selectize=TRUE, multiple=TRUE,
                                            choices=c('Very severely underweight'='VSU', 'Severely underweight'='SU',
                                                      'Moderately underweight'='MU', 'Healthy weight'='N', 'Overweight'='Ov',
                                                      'Moderately obese'='MO', 'Severely obese'='SO', 'Very severely obese'='VSO',
                                                      'Underweight'='U', 'Obese'='Ob'),
                                            selected=c('MU', 'N', 'MO')),
                                selectInput('cohort', label=h5('Survey Cohort Years'), selectize=TRUE, multiple=TRUE, selected=c(2011,2013),
                                            choices=list('1999-2000'=1999, '2001-2002'=2001, '2003-2004'=2003, '2005-2006'=2005,
                                                         '2007-2008'=2007, '2009-2010'=2009, '2011-2012'=2011, '2013-2014'=2013)),
                                br(),
                                submitButton("Apply Changes")
                              )),
                              
                              # Change the color, font size, etc of the output result
                              tags$head(tags$style(
                                "#warning{color:red;font-size:15px;}",
                                "#text1{color: red;font-size: 20px;}",
                                "#text3{color: green;font-size: 20px;}",
                                "#text4{color: black;font-size: 18px;}",
                                "#source{color: Blue;font-size: 20px;}",
                                "#diseases{color: black;font-size: 18px;}",
                                "#datasource{color: black;font-size: 18px;}",
                                "#riskfactor{align:center;}",
                                "#mitifactor{align:center;}",
                                "#text5{color: Blue;font-size: 20px;}",
                                "#text6{color: Blue;font-size: 20px;}",
                                "#text7{color: Blue;font-size: 20px;}",
                                "#model_results{align:center}",
                                "#AIC{align:center}",
                                "#SampleCount{align:center}"
                              ))
                            ))
        )
)

