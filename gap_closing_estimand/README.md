This directory contains replication code for
>Lundberg, Ian. Forthcoming. "[The Gap-Closing Estimand: A Causal Approach to Study Interventions that Close Disparities Across Social Categories](https://doi.org/10.31235/osf.io/gx4y3)." _Sociological Methods and Research_.
When finalized, these files will be archived on [Dataverse](https://dataverse.harvard.edu/dataverse/ilundberg).

The code uses the [gapclosing package](https://ilundberg.github.io/gapclosing/) (version 1.0.0). To use the code, you will first need to install the package: <pre>devtools::install_github("ilundberg/gapclosing")</pre>

To replicate the paper using the code in this repository, you will need to retrieve the data files from the websites that administer them. The [General Social Survey](https://gss.norc.org/) website requires registration for data access. The code also uses a [crosswalk](https://osf.io/xb2yz/) provided in a GSS methodological memo.

Follow these steps to access the data and populate the `data` folder.
1. Visit [https://gssdataexplorer.norc.org/](https://gssdataexplorer.norc.org/).
2. In the top right, click "Register". Create an account. Sign in to your account.
3. Visit [https://gssdataexplorer.norc.org/projects/104034](https://gssdataexplorer.norc.org/projects/104034)
4. Click the yellow "Actions" button in the top right and click "Extract Data"
5. Under "Name Your Extract" type "GSS" in the "Extract Name" box. The replication code assumes you use this name.
6. Under "Choose variables", click "ADD ALL" in the top right.
7. Under "Case selection", click "NEXT" without making any selections.
8. Under "Choose output options", choose "R Script." You may also choose any additional options.
9. You are now in "Extracts". Under "Actions", click the down arrow symbol for "Download Extract."
10. Unzip the folder. Place all of the individual files into this data directory. 