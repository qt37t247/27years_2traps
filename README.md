# 25years_2traps
Comparing efficacy of different trapping along 25 year effort and looking for environmental correspondence 

This repository include the key dataset and the script associated with data collection and analyses.

The insect data was from:
Battles, I., Burkness, E., Crossley, M. S., Edwards, C. B., Holmstrom, K., Hutchison, W., ... & Owens, A. C. (2024). Moths are less attracted to light traps than they used to be. Journal of Insect Conservation, 1-12.

The analyses of this work is inspired by:
Müller, J., Hothorn, T., Yuan, Y., Seibold, S., Mitesser, O., Rothacher, J., ... & Menzel, A. (2023). Weather explains the decline and rise of insect biomass over 34 years. Nature, 1-6.

## Step 1. Environmental data collection
We collected air quality data from Socioeconomic Data and Applications Center:

https://sedac.ciesin.columbia.edu/

We used the R code ("crop.R" and "extract.R") to extract environmental variables at the sampling points.


We collected other environmental variables from Google Earth Engine (with the python package geemap https://geemap.org/). See python script ("GEE_collect.ipynb").

The environemental variables are then compiled for the subseqeuent modeling with R ("Data_process.R")


## Step 2. 




## Acknowlegement 



## Contact
tangbenjamin@hotmail.com

qiantang@fas.harvard.edu
