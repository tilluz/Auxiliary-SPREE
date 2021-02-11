# Auxiliary-SPREE

This is the code repository for the paper 'Intercensal updating using structure-preserving methods and satellite imagery' authored by Till Koebe, Alejandra Arias-Salazar, Natalia Rojas-Perilla and Timo Schmid.

All of the data to replicate the findings of this study are available for scientific use.

- [Census 2002](http://anads.ansd.sn/index.php/catalog/9)
- [Census 2013](http://anads.ansd.sn/index.php/catalog/51)
- [WorldPop covariates](https://www.worldpop.org/project/categories?id=14)
- [Weather data](https://www.worldclim.org/data/worldclim21.html)
- [DHS surveys](https://dhsprogram.com/methodology/survey-search.cfm?pgtype=main&SrvyTp=country&ctry_id=36)
- [Population projections](http://www.ansd.sn/ressources/publications/indicateurs/Projections-demographiques-2013-2025+.htm)

The code was run on a standard machine with following specifications:
- 8 x Intel i7 @1.80GHz
- 23.4GiB of RAM
- R version 3.6.3
- Platform: x86_64-pc-linux-gnu (64-bit)
- Running under: Ubuntu 20.04.2 LTS

Libraries used:
 [1] doParallel_1.0.16   iterators_1.0.12    foreach_1.5.0       randomForest_4.6-14 data.table_1.13.0  
 [6] raster_3.3-7        mice_3.11.0         broom_0.5.6         foreign_0.8-75      rgeos_0.5-3        
[11] rgdal_1.5-10        sp_1.4-2            survey_4.0          survival_3.1-8      Matrix_1.2-18      
[16] forcats_0.5.0       stringr_1.4.0       dplyr_1.0.0         purrr_0.3.4         readr_1.3.1        
[21] tidyr_1.1.0         tibble_3.0.1        ggplot2_3.3.1       tidyverse_1.3.0     haven_2.3.1