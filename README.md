# COVID-19 Vaccine Delivery Partnership (CoVDP) Core Analysis Pipeline (CCAP)
# This pipeline is leveraging local environments to create clean and consice analysis
## History
With the breakout of Coronavirus as global pandemic in 2020, a global collaboration unit - COVAX, was formed to accelerate the development, production and equitable acces to COVID-19 tests, treatments and vaccines. 

With countries around the world reporting their numbers regarding vaccination supplies, utilization, population coverage and funding, the CoVDP Core Analysis Pipeline (CCAP) was produced and maintained by the COVID-19 Vaccine Delivery Partnership to collate, manipulate, and analyze key COVID-19 vaccine implementation metrics for all countries, areas, and territories.

## Envrironment and installation
The codebase is written in R with the exception of one module (dvr) which is in Python. Both R Studio and VS Code can be used.
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
consolidate | module that consolidates all data from the CCAP.
data | data folder holding all data ingested and produced in the CCAP.
data/cleaning_log | folder containing all the log of cleaning done on 1 dose, fully vaccinated, boosters and total doses vaccines.
data/input | input files ingested into pipeline.
data/input/interim | temporary input files produced and consumed during CCAP run.
data/input/static | input files that do not change every so often.
data/output | All output files generated after the CCAP run. 
src | Extraction, transformation and load (ETL) folder handling all ETL for different sections of the CCAP.
src/add_data | Extraction, ingestion and transformation of base smartsheet & WHO dashboard.
src/adm_cov | Extraction, ingestion and transformation of administration coverage data (vaccines).
src/cov_disag | Extraction, ingestion and transformation of coverage disagregation data from WIISEMart API.
src/demand_planning | Extraction, ingestion and transfomation of demand planning data.
src/dvr | Extraction, ingestion and transformation of daily vaccination rate data.
src/entity_characteristics | Extraction, ingestion and transforation of base entity details, UN population (UNPOP) and health care workers (HCW) data. 
src/finance | Extraction, ingestion and transformation of finance data from UNICEF.
src/supply | Extraction, ingestion and transformation of vaccine supply data from IMF vaccine tracker and UNICEF market dashboard.
eda | Exploratory data analysis (EDA) folder containing different modules handling EDA for different CCAP sections.  
eda/adm_cov | EDA and consolidation of administrative coverage data.
eda/cov_targets | EDA and consolidation of coverage target data. 
eda/finance | EDA of finance data consolidation.
eda/prod_util | EDA and consolidation of product utilization data. 
<!-- eda/qual_data |  -->
eda/rank_bin | Grouping and ranking of data.
eda/supply | EDA of supplies data consolidation.
helpers | Folder containing different helper functions which are repeatedly & frequently used in different sections within the CCAP.
.env | Configuration variables from the .env file containing API keys from WIISEMart.
.gitignore | file containing any item to be excluded from online GitHub repository.
app.r | Main run file for CoVDP pipeline. Includes all consolidated functions used within the CCAP and one main function that writes all outputs to one Excel document. 
QC.r | quality check script to compare current week outputs from previous week outputs.  

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
- Each module can be tested by calling the run function at the end of the script e.g., run_add_data() for the add_data module.
- To collectively run all modules at once, run app.R file.
- Ensure static dates are changed for each new week, with the exception of t70_deadline.
### app.R file content
- All libraries/packages needed to run the R code.
- Static global variable dates.
    - refresh_date: date which one runs the pipeline.
    - dataset_date (sec_date): date reported on the first tab from IMF dataset.
    - del_date: date reported from first tab from UNICEF'S Delivery tables sent weekly via mail.
    - t70_deadline: WHO set timeline to achieve 70% target coverage.

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
1. Preparation of monthly time series data view across supply secured, supply received, coverage, daily vaccination rate, financing, and coverage in high risk groups.
2. Build out business analytics view of DAM KPIs: visits to CoVDP Infohub, IAI reports produced, etc.
3. Create SOP for CCAP modification, including module addition (link) and subtraction (de-link), "UAT" branch before merge to main.
