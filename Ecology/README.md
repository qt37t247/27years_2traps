# Ecological Modelling based on very long term field survey

We used generalized additive mixed models to investigate the association between abundance corn earworms and the environments.

The analyses of this work is inspired by:
Müller, J., Hothorn, T., Yuan, Y., Seibold, S., Mitesser, O., Rothacher, J., ... & Menzel, A. (2023). Weather explains the decline and rise of insect biomass over 34 years. Nature, 1-6.

## Step 1. Environmental data collection
We collected air quality data from Socioeconomic Data and Applications Center:

https://sedac.ciesin.columbia.edu/

Geographic locations of traps are in "Sites.csv".

We downloaded the rasters and extracted values of environmental variables at the sampling localities (see an example in "crop_extract.R"). 

We extracted other environmental variables (light, vegetation, precipitation, and temperature) from Google Earth Engine (with the python package geemap https://geemap.org/). See python script ("GEE_collect.ipynb").

The extracted data from Google Earth Engine was then processed with R ("Data_process.R").

We collected resulting environmental variables (in csv format) and compile them with the count data (see "Data_build.R") to create a master sheet. 

The master sheet ("master.csv") was served as the input of the subseqeuent modeling.


## Step 2. Generalized additive mixed modelling (GAM)

We combined insect count data with the environmental variables ("Data_build.R").

This resulting the file ("master.csv") for the subsequent GAM.

We deviced six-stepped modelling ("Model_GAM.R") with each step adding additional complexity in the models. 


