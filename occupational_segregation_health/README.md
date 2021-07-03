
README

This replication package contains code for "[Quantifying the contribution of occupational segregation to racial disparities in health: A gap-closing perspective](https://drive.google.com/file/d/1OFYhRVK7CCglXMCfDOelKK2s0hIUWoX7/view?usp=sharing)" by Ian Lundberg. This is a working paper, and updates to the code should be expected.

Email: [ilundberg@princeton.edu](mailto:ilundberg@princeton.edu)

# Structure of the files

This code assumes that your working directory contains sub-directories: data, code, intermediate, logs, figures.

code: This contains the code from the replication package

data: This contains the data (access described below)

intermediate: This is an empty folder where the code will place intermediate files (e.g. large model fit objects)

logs: This is an empty folder where the code will place intermediate text logs

figures: This is an empty folder where the code will place figures

# Data access

The data used in this study are from the Annual Social and Economic Supplement of the Current Population Survey, accessed through IPUMS at [cps.ipums.org](https://cps.ipums.org/cps/).

The data file cps_00050.dat is 4.8 GB. To access the data, you will need to register for an account at the IPUMS website, download the data, and add it to the data subdirectory. The file you download may have a different name.

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

# Computing environment

This code was run on Amazon Web Services EC2 instance of type r5.4xlarge running Ubuntu Linux. The instance was configured with 16 vCPUs and 128 GB memory, using Elastic Block Store for storage. When run, this cost approximately $1.21 per hour. The code ran in just over 5 hours.

# Software

This code was run on R version 4.1.0.

It directly loads the following packages: tidyverse (1.3.1), reshape2 (1.4.4), labelled (2.8.0), ipumsr (0.4.5), mgcv (1.8-36), foreach (1.5.1), doParallel (1.0.16), ggrepel (0.9.1), nnet (7.3-16)
