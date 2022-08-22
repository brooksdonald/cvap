# CoVDP Core Analysis Pipeline (CCAP)
# This pipeline is leveraging local environments to create clean and consice analysis
## History
With the breakout of Coronavirus as global pandemic in 2020, a global collaboration unit - COVAX, was formed to accelerate the development, production and equitable acces to COVID-19 tests, treatments and vaccines. 

With countries around the world reporting their numbers regarding vaccination supplies, utilization, population coverage and funding, the CoVDP Core Analysis Pipeline (CCAP) was produced and maintained by the COVID-19 Vaccine Delivery Partnership to collate, manipulate, and analyze key COVID-19 vaccine implementation metrics for all countries, areas, and territories.

## Envrironment and installation
The codebase is written in R with the exception of one module which is in Python. Both R Studio and VS Code can be used.
### Prerequisites 
1. Ensure R is installed in your machine. If not installed, find the installation download link [here](https://cran.r-project.org/)
2. Install R Studio from [here](https://www.rstudio.com/products/rstudio/)
3. Install Visual Studio Code from [here](https://code.visualstudio.com/Download)
    1.   R extension will be needed to run R in VS Code. Get it from [here](https://code.visualstudio.com/docs/languages/r#:~:text=The%20R%20extension%20for%20Visual%20Studio%20Code%20supports,managing%20packages%20and%20working%20with%20R%20Markdown%20documents.)

If using Ubuntu - Don't forget to update your ubuntu apt
`apt update && apt upgrade`

To resolve languageserver installation issues, just run
`apt install libcurl4-openssl-dev libssl-dev libxml2-dev`

## Architecture overview
### Main files
Path | Content
-------- | --------
data/input | input files ingested into pipeline.
data/input/static | input files that do not change every so often.
data/input/supply_data | input files from BMGF team.
data/input/interim/consolidate | Consolidated files aggregating all outputs from the pipeline. 
data/output | All output files generated from the pipeline. 
src/base | Extraction, ingestion and transformation of base smartsheet & WHO dashboard.
src/demand_planning | Extraction, ingestion and transfomation of demand planning data.
src/entity | Extraction, ingestion and transforation of base entity details. 
src/finance | Extraction, ingestion and transformation of finance data from UNICEF.
src/population | Extraction, ingestion and transformation of population data from UNPOP and WIISEmart.
src/supply | Extraction, ingestion and transformation of vaccine supply data from IMF vaccine tracker and UNICEF market dashboard.
src/vaccines | Extraction, ingestion and transformation of vaccines data from Jeremy's script.
eda/combination | Combining of supply data summary
eda/coverage | Exploratory data analysis of coverage target data. 
eda/financing | Exploratory data analysis of finance data consolidation.
eda/product | exploratory data analysis of product utilization data. 
eda/rank_bin | Grouping and rnking of data 
eda/supplies | exploratory data analysis of supplies data consolidation.
eda/vxrate | consolidation of administration coverage data.
helpers | helper functions repaeatedly & frequently used in the pipeline.
gitignore | file containing any item to be excluded from online repo.
app.r | Main run file for covax pipeline. Includes all consolidated functions in the pipeline and write funtion of all outputs to excel. 
quality_checks.r | quality check script to compare current week outputs from previous week outputs.  

### Diagram
![covax architecture](https://user-images.githubusercontent.com/36184732/177362152-eca32704-22c5-44e4-818d-7d6aaec4874f.PNG)
### Design Principles
Design principles (rules) abided by while building & contributing to this architecture include:
1. Separation of Concerns (SoC)
Each distinct sections are separated to address different individual concerns.
2. Don't Repeat Yourself (DRY)
Minimal to no repition of software patterns are observed. The system is designed with a single, authoritative and unambiguous representation for every piece of knowledge.
3. Keep it short and simple (KISS)
Simple design to building the architecture is observed.
4. SOLID
The design principles used are dedicated to making software designs flexible, maintainable, and flexible.  
## Running the pipeline
- All modules contain run files with run functions named after the specific module. 
- Each module can be tested by calling the run function at the end of the script e.g., run_base() for the base module.
- To collectively run all modules at once, run app.R file.
- Ensure static dates are changed for each new week, with the exception of t70_deadline.
### app.R file content
- All libraries/packages needed to run the R code.
- Static global variable dates.
    - refresh_date: date which one runs the pipeline.
    - dataset_date (sec_date): date reported on the first tab from IMF dataset.
    - del_date: date reported from first tab from UNICEF'S Delivery tables sent weekly via mail.
    - t70_deadline: WHO set timeline to achieve 70% target coverage.
- Configuration variables from the .env file.
- Paths to helper functions.
- Paths to ETL run files.
- Environment variables from each ETL module.
- Paths to EDA run files.
- Environment variables for each EDA module.
- Path to consolidate module.
- Exporting analyzed data to excel using write_xlsx package.

## How to contribute
To build on to the architecture, modify or enhance the current code base, reach out to Donald Brooks (brooksd@who.int) for access to the private repository.

## Next development
1. Cleaning of time series data.
2. De-link dvr scripts.
3. Automate manual cleaning process of vaccination rates.
