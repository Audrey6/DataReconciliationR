# Data Reconciliation R Project
This reconciliation project captures all discrepanices within two large customer datasets between two systems and categorizes them into flags for systems to clean up. 

## Description
This process was created to simplify and accelerate the data clean up between large customer datasets and allows an analyst to run clean ups on a frequent basis. This script was created in R language and produces reports of data in csv that can be used to housekeep the two systems. 
Please note that the datasets found in the data folder are randomized generated customer data and that there are only 50 rows, however the original clean up was done with over 200,000 rows. This script is meant to handle large datasets.

## Getting Started
### Dependencies
* Install R
* R Program IDE (RStudio)
### Installing
* https://rstudio-education.github.io/hopr/starting.html
### Executing the program
* Save both datasets to a file path
* Install correct packages found in the script
* Ctrl+R, run whole program at once
* Results will be produced into a new file within designated path (12 files)
  
## Datasets
For this example project, let's take datasets from two systems called Franklin and SeeLoad.
Columns found within both datasets:
*Use dataset2 as the source of truth along with the most accurate ID for comparison, DRUID/UUID*

![image](https://github.com/Audrey6/DataReconciliationR/assets/34180394/f643f0ea-bf1c-4479-8a03-58ce99b1dc27)

## Target Flags
Below are the flags the script will produce in the results folder as csv files. Each flag has its own individual file that contains discrepancies within those compared columns between the two datasets.
* Flag A: Enrollment Status
* Flag B: Mismatching Addresses
* Flag C: Serial Numbers
* Flag D: Names
* Flag E: Duplicates
* Flag F: Missing in Dataset 1
* Flag G: Missing in Dataset 2
* Flag H: Complete Match (No Action)

## Results
Please check out the results folder as well as the Summary documentation for all flag files and general analysis. In addition here's a summary run down of the data ran in the code:

![image](https://github.com/Audrey6/DataReconciliationR/assets/34180394/49145779-60b8-4c79-a776-0c458d1b6300)

