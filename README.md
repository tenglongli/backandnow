# backandnow: 
Bayesian back-calculation and nowcasting for line list data during the COVID-19 pandemic

## simulation
- `cluster.R`: Parallel processing for the simulation (complete line list data). 
- `cluster_ss.R`: Parallel processing for the simulation (delayed surveillance initiation and real time estimation).
- `rt.rds`: Daily reproductive numbers used in the simulation.
- The **output** folder: Raw simulation output files and processing code. 

## Source code
- `backnow.cpp`: Bayesian back-calculation and nowcasting. 

## Final Result: clean output used in the paper
- `result.R`: Code for generating all the figures and tables.
- `count.rds`: Daily case count estimates for complete line list data.
- `count_ss.rds`: Daily case count estimates for delayed surveillance initiation.
- `count_ss1.rds`: Daily case count estimates for real time estimation.
- `rest.rds`: Time-varying reproductive number estimates for complete line list data.
- `rest_ss.rds`: Time-varying reproductive number estimates for delayed surveillance initiation.
- `rest_ss1.rds`: Time-varying reproductive number estimates for real time estimation.
- `eva1.rds`: Coverage rates and RMSE for complete line list data.
- `eva2.rds`: Coverage rates and RMSE for delayed surveillance initiation.
- `eva3.rds`: Coverage rates and RMSE for real time estimation.
- `geweke.rds`: Geweke's diagnostics 

## Daily flow rates in Massachusetts
- `pop_flow_county_byday_dataframe.csv`: Data containing average daily flow rates over all Massachusetts counties. 
