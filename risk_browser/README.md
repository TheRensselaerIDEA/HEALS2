# Welcome to NHANES risk browser.

The NHANES Risk Browser provides the ability to select a disease or riskful health condition, a large number of factors that might impact this condition; then it analyzes their association with the selected disease and present the results in tabular and graphical formats.

The workflow is based on the Environment-Wide Association Study (Patel et al (2010) DOI:10.1371/journal.pone.0010746). The data source is the National Health and Nutrition Examination Survey ([NHANES](https://www.cdc.gov/nchs/nhanes/index.htm)). 

The risk browser allows for three levels of analysis.

1. Population Analysis - Learn risk models for an entire population.
2. Precision Cohort Analysis - Define a more precise study cohort based on survey year, subject age, gender, ethnicity, or BMI, then learn risk models for that study cohort.
3. Cadre Analysis - Let the data and a more complicated modeling strategy discover informative subpopulations for particular conditions. Cadre analysis is not yet implemented.

Notes:

1. It is possible to define precision cohorts that contain a very low number of positive observations. In these cases, regression coefficient estimates will be noisier.
2. It is possible to define precision cohorts that do not contain any observations at all.
 
# Running the risk browser

To run the risk browser, locally, download the folder here. You'll need R, RStudio, and the following R packages: 

- `tidyverse` - easy data manipulation
- `survey`    - analysis on complex survey designs
- `shiny`     - interactive web apps in R
- `shinyBS`   - mouseover text in Shiny

Note that `tidyverse` can take some time to install.

Open either `server.R` or `ui.R` in RStudio and click the "Run App" button in the top right corner of the script editor window. Note that, if you have `server.R` or `ui.R` in separate folders, or if you don't have them in the same folder as `allNames.csv` or the `analytic` directory, the browser will fail.

# Folder contents

The two scripts `ui.R` and `server.R` contain all of the browser's code. You need both of them in the same directory to run it.

The `analytic` subdirectory contains all of the NHANES datasets used for the browser. The dataset structure was designed to be easily usable in the browser, so there's a lot of redundant variables in them.

The file `allNames.csv` contains the full names of all the NHANES variables.

# Areas for improvement

- The browser is currently very fragile with respect to failure. It should have better handling of degenerate precision cohorts and incomplete analytic sets.

- Because `server.R` relies heavily on `shiny::reactive` functions, a number of expensive calculations are repeated. This can probably be fixed by rewriting the script to instead use `shiny::observeEvent` functions.

- Selected control variables and a summary of the precision cohort selected should be printed in the Analysis Results column of the Population Analysis window. Right now, the interface is less than transparent.

- The progress bar is not great. It can be made much more informative with functionality-specific progress messages (e.g. "constructing survey cohort", "running models for first dataset", etc.).

- Not all datasets contain all possible control variables -- you can only use subsets of expert-recommended control variables. If more control variables can be added without having a large effect on the number of complete observations, they should be added.
