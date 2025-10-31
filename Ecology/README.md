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

In R, we deviced a five-stepped strategy ("**Model_GAM.R**") to build generalized additive mixed models using the data from Delaware (the training dataset). 

The stepwise strategy comprises five steps, with each step adding complexity to the best model of the previous step: 
In the first and second steps, we investigated how monthly environmental variables alone and combined explain the number of the moth of corn earworm captured. 
In the third step, we added all annual variables to find that the annual environmental variables better explain the insect abundance than the year. 
In the fourth step, we explored the interaction between the type of traps and the environmental variables. 
In the final step, we build our final model to let light intensity interact with light trap while air quality to interact with pheromone trap. 

We used data from New Jersey (the validation dataset) to validate the model with highest quality, as indicated by the lowest value of Akaike information criterion (AIC). 

