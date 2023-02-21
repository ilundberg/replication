
# readme

This folder is where you should put the raw NLSY97 data when replicating the analyses. You can choose the variables by uploading the included tagset to the NLS Investigator: https://www.nlsinfo.org/investigator

The code files internally call the downloaded R scripts that come from BLS with the data. In those scripts, I manually uncomment the line
new_data <- qnames(new_data)

I also commented out the lines for "Handle missing values" because I want to know the reason something was missing.