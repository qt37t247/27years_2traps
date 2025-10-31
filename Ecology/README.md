# Ecological Modelling based on very long term field survey

We used generalized additive mixed models to investigate the association between abundance corn earworms and the environments.

The analyses of this work is inspired by:
Müller, J., Hothorn, T., Yuan, Y., Seibold, S., Mitesser, O., Rothacher, J., ... & Menzel, A. (2023). Weather explains the decline and rise of insect biomass over 34 years. Nature, 1-6.

## Step 1. Count data processing
We used data from the corn earworm monitoring network deployed across 12 farms in Delaware, operated by the University of Delaware since 1998 (see "**DE.csv**"), and 34 farms in New Jersey, operated by the Rutgers University since 2013 (see "**NJ.csv**"). 

At each farm, a blacklight trap, using continuously operating T15 fluorescent bulbs (GE), and a pheromone trap, using synthetic female pheromone lures (Hercon Zealure) are deployed no more than 0.53 km apart. 

Moths captured the traps are counted and discarded every one to seven days between May and October.

We averaged the trap catches to estimate the monthly average number of moths per trap per night for the subsequent analyses (see "**Count_data_Fig.S1.R**", also use this to produce Fig. S1 for the overview of count data). 


## Step 2. Environmental data collection

Geographic locations of traps are in "**Sites.csv**".

We collected air quality data from NASA Socioeconomic Data and Applications Center.

We downloaded the rasters and extracted values of environmental variables at the sampling localities (see an example in "**crop_extract.R**"). 

We extracted other environmental variables (light, vegetation, precipitation, and temperature) from Google Earth Engine (with the python package geemap https://geemap.org/). See python script ("**GEE_collect.ipynb**").

The extracted data from Google Earth Engine was then processed with R ("**Data_process.R**").

We collected resulting environmental variables (in csv format) and compile them with the count data (see "**Data_build.R**") to create a master sheet. 

The master sheet ("**master.csv**") was served as the input of the subseqeuent modeling.


## Step 3. Generalized additive mixed modelling (GAM)

In R, we deviced six-stepped modelling ("**Model_GAM.R**") with each step adding additional complexity in the models. 


