# kaggle-avito
Winning solution to the Avito CTR competition

Some high level description of the solution can be found in the .pdf file.

How to run:
* change the folder at the top of _fast_10pct_run.R and _full_100pct_run.R to point to where the data files are stored
* (recommended) run _fast_10pct_run.R and verify that it produces expected results -- this will take a few hours
* run _full_100pct_run.R to produce a submission file.

Summary of other files:
avito_data1.R -- Load datasets
avito_phone.R -- feature extraction from phone dataset
avito_search.R -- feature extraction from search dataset
avito_visit.R -- feature extration from visit dataset
avito_cat_cat.R -- more features + data merge
avito_data_merge.R -- rest of the features + data merge
avito_train_xgb.R -- fit xgboost models
avito_utils.R -- some utility functions 

Hardware requirement:
To run the full data solution (_full_100pct_run.R) I recomment a machine with 256GB of ram + (at least) 200GB of swap space. This is due to the combination of my own inefficient code and inefficiency in R's memory handling. 
