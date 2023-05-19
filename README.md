# "Fractional replication" in single-visit multi-season occupancy models: Impacts of spatio-temporal autocorrelation on identifiability

### In prep

### Jeffrey W. Doser and Sara Stoudt (co-first authors)

### Please contact Jeffrey W. Doser (doserjef@msu.edu) or Sara Stoudt (sas072@bucknell.edu) for questions about the code or data used in the manuscript

## Abstract

1. Understanding variation in species occupancy is an important task for conservation and management. When assessing occupancy patterns over multiple temporal seasons, it is often recommended to visit at least a subset of sites multiple times within a season during a period of closure to enable separate estimation of observation biases from species occupancy. However, logistical constraints can inhibit re-visitation of sites within a season, resulting in the use of single-visit multi-season occupancy models. Some have suggested that autocorrelation in space and/or time can provide "fractional replication" to separately estimate occupancy probability from detection probability, but the reliability of such approaches is not well understood.
2. Here we perform an extensive simulation study to assess the reliability of estimates from single-visit multi-season spatial occupancy models under differing amounts of spatial and temporal autocorrelation ("fractional replication"). We assess model performance under both correctly specified models and multiple forms of model mis-specification, and compare estimates from single-visit models to models with varying amounts of within-season replication.
3. We found that high range spatial autocorrelation can reduce bias in estimates from single-visit occupancy models when the model is correctly specified. However, under various forms of model mis-specification, single-visit multi-season spatial occupancy models had high bias and low coverage rates regardless of the characteristics of the "fractional replication". In contrast, models with varying amounts of additional replication (i.e., "mixed design" with 10\% of sites visited twice, two visits at each site, five visits at each site) were robust to model mis-specification.
4. Our findings suggest that "fractional replication'' cannot replace true replication in terms of the identifiability of occupancy and that researchers should consider the potential inaccuracies when using single-visit multi-season spatial occupancy models. We show that a little true replication can go along way with even 10\% of sites being revisited leading to reasonably robust estimates even in the presence of extreme model mis-specifications. When possible, we recommend performing multiple within-season visits at at least a subset of spatial locations or integrating single-visit data with other data sources to mitigate reliance on parametric assumptions required for reliable inference in multi-season spatial occupancy models. 

## Repository Directory

All code and simulated data sets were created using `spOccupancy` v0.6.1. See the [spOccupancy Website](https://www.jeffdoser.com/files/spoccupancy-web/) and [Repository](https://github.com/doserjef/spOccupancy) for more details on working with `spOccupancy`.

### [code](./code)

+ `main-sim-*`: scripts to run the simulations for different mis-specification and visit scenarios (and generate the simulated data sets). The `*` represents a wildcard, which is replaced by the specific scenario for a given combination of amount of replication and mis-specification type. In the file names, `sv` corresponds to single-visit and `dv` corresponds to double visit. 
+ `correlation-plot.R`: script to generate Figure 1 in the associated manuscript.
+ `curve-plots.R`: script to generate Figure 2 in the associated manuscript.
+ `summary.R`: script to generate all other figures in the manuscript and summarize the simulation results. 

### [figures](./figures)

Contains all figures included in the main text and the supplemental information that are generated from the scripts in the `code` directory. File names of the figures corresponding to the figure number in the manuscript.

### [results](./results)

Summary results files for the different mis-specification and visit scenarios. Note that the complete results are not included because the files are too large for Github, and rather these summary files can be used to generate the figures in the manuscript. The file names indicate the specific scenario for a given combination of the amount of replication and mis-specification type. In the file names, `sv` corresponds to single-visit, `dv` corresponds to double-visit, and `fv` corresponds to five-visit.




