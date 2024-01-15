
# readme

This folder contains data. You will need to access data from the data distributor.

1. Register for an account in the NLS Investigator: https://www.nlsinfo.org/investigator
2. Log in, navigate to the NLSY79, and click to choose tagset. For "Upload Tagset", upload the occs.NLSY79 file from this repository.
3. Download the data. For easiest use of this package, rename the .dat file to occs.dat.
4. Place occs.dat in this subdirectory.

Our code will call data/occs.R, which is a script to load data. If you modify our tagset at all, that file will not work for you, but your NLS Investigator download can (if you check the box) provide an analogous R script. In our R script, we manually uncommented the line `new_data <- qnames(new_data)` and commented out the lines for ``Handle missing values.''