
README

This replication package contains code for "Occupational segregation contributes to racial disparities in health: A counterfactual perspective" by Ian Lundberg. This is a working paper, and updates to the code should be expected.

Email: [ilundberg@princeton.edu](mailto:ilundberg@princeton.edu)

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

You will need to rename to "cps_00050.dta" for the code to run.

# Computing environment and software

This code was run on a computing cluster running Windows Server 2012 R2 Standard.

Processor: Intel(R) Xeon(R) CPU E7-4850 v3 @2.20 GHz 2.19GHz (4 processors)

Installed memory (RAM): 512 GB

System type: 64-bit Operating System, x64-based processor