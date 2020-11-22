README

**Note:** The canonical version of this replication package is [hosted on Dataverse](https://doi.org/10.7910/DVN/ASGOVU).

This package contains replication code for
"[What is Your Estimand? Defining the Target Quantity Connects Statistical Evidence to Theory](https://doi.org/10.31235/osf.io/ba67n)," by Ian Lundberg, Rebecca Johnson, and Brandon Stewart.

Email: ilundberg@princeton.edu

# Structure of the files

This code assumes that your working directory contains three sub-directories: code, data, and output.

code: This contains the code from the replication package

data: This contains the data (access described below)

output: This is an empty folder where our code will place all results

# Data access

The data used in this study are held by those who administer the data sources. To access the data, you will need to register for an account at each data provdier's website, download the data, and add it to the data subdirectory. We describe data access here and in comments at the top of each code file.

## Data for a_estimation_example.R

The data for this example are available from IPUMS at
https://cps.ipums.org/cps/

1. Register for an account
2. Put the following variables in your cart
YEAR, SERIAL, MONTH, CPSID, ASECFLAG, ASECWTH,
PERNUM, CPSIDP, ASECWT, RELATE, AGE, SEX, RACE,
MARST, NCHILD, AHRSWORKT, EDUC, EARNWT, REPWTP,
CLASSWLY, WKSWORK1, WKSWORK2, UHRSWORKLY, INCWAGE
3. Select the sample "IPUMS-CPS, ASEC 2019"
4. Select data format ".dta (Stata)"
5. Select cases on AGE (ages 25-44 only) and SEX (Female only)
6. Download the data and place it in the data subdirectory

You will need to rename to "cps_00040.dta" for the code to run.

## Data for b_specific_example_1.R

The data for this example are available from the GSS at
https://gssdataexplorer.norc.org/

1. Register for an account
2. Put the following variables in your cart
YEAR, WTSSNR, WTSS, BALLOT, COHORT, FAMILY16, RACE, SEX, MAEDUC, PAEDUC,
EDUC, AGE, ID_, WTSSALL
3. Download the data and place it in the data subdirectory

You will need a .dat and a .dct file. Name them GSS.dat and GSS.dct.

## Data for c_specific_example_2.R

The data for this example are available from the Fragile Families and Child Wellbeing Study.

The study website is: https://fragilefamilies.princeton.edu/

The data are available at: https://www.opr.princeton.edu/archive/

1. Register for an account
2. Download ff_res_merge4.dta and place it in the data subdirectory

## Data for d_risk_ratio_difference.R

This illustration is entirely simulated, so there is no data.

# Computing environment and software

This code was executed on a 2019 MacBook Pro with a 2.4 GHz Quad-Core Intel Core i5 processor and 8 GB 2133 MHz LPDDR3 RAM.

This code was run on R version 4.0.3.

The code calls the following packages:
Amelia (1.7.6),
doParallel (1.0.16),
doRNG (1.8.2),
foreach (1.5.1),
haven (2.3.1),
mgcv (1.8.33),
mvtnorm (1.1.1),
readstata13 (0.9.2),
reshape2 (1.4.4),
tidyverse (1.3.0)
