The CRANMaker.R and CRANRepoUpdater.R scripts contain functions to make and update repositories

The Script_for_updateing_from_cmd.R script allows the updating to be run from a bash script.

The arguments are the following:
-ps  - Path to script, this is where we need to tell the function where it can find CRANRepoUpdater.R*
-pr  - Path to repository, this is where we need to tell the function where the repository is*
-libr - If we want to add different repository library than the default, here we can do it (note, there is no way currently to add multiple repos here)
-version - If we only want to update one version, here we can add that (note, that there is no way currently to add multiple versions here, can be adjusted in the Upgrade_for_batch.R script)
-ml  - Boolean value for if we want to allow R to cycle through all the default librepos

Suggested use:

Rscript path/to/the/Script_for_updateing_from_cmd.R -ps path/to/CRANRepoUpdater.R -pr path/to/UKDA_CRAN

For this to run Rscript needs to be part of PATH. It can be found at:
C:/Program Files/R/R-4.3.2/bin/x64

Note, that R does not always like paths like ust$\. In cases like this, a driver should be assigned to the paths used for the repo and the scripts.
