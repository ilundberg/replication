
README

This replication package contains code for "[The causal impact of segregation on a disparity: A gap-closing approach](https://osf.io/x9evk)" by Ian Lundberg.

Email: [ianlundberg@ucla.edu](mailto:ianlundberg@ucla.edu)

# Structure of the files

This code assumes that your working directory contains four sub-directories: code, data, intermediate, and figures.

code: This contains the code from the replication package

data: This contains the data (access described below)

intermediate: This is an empty folder where the code will place intermediate files (e.g. large model fit objects)

figures: This is an empty folder where the code will place all results, including both figures and text file output

# Data access

The data used in this study are from the Annual Social and Economic Supplement of the Current Population Survey, accessed through IPUMS at [cps.ipums.org](https://cps.ipums.org/cps/).

The data file cps_00050.dat is 4.8 GB. To access the data, you will need to register for an account at the IPUMS website, download the data, and add it to the data subdirectory. 

1. Register for an account
2. Put the following variables in your cart:
YEAR
SERIAL
MONTH
CPSID
ASECFLAG
HFLAG
ASECWTH
MISH
PERNUM
CPSIDP
ASECWT
AGE
SEX
RACE
MARST
BPL
HISPAN
EMPSTAT
OCC
OCC2010
EDUC
SCHLCOLL
DIFFHEAR
DIFFEYE
DIFFREM
DIFFPHYS
DIFFMOB
DIFFCARE
DIFFANY
MARBASECIDP
ASECOVERP
REPWTP
LNKFW1YWT
OCC10LY
DISABWRK
HEALTH
QUITSICK

3. Select the ASEC samples for every year 1988--2020
4. Select cases on AGE (ages 25-60 only)
5. Download the data and place it in the data subdirectory

You will need to rename to cps_00050 for the code to run.
