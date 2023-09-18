# "Fractional replication" in single-visit multi-season occupancy models: Impacts of spatio-temporal autocorrelation on identifiability

### In review

### Jeffrey W. Doser and Sara Stoudt (co-first authors)

### Please contact Jeffrey W. Doser (doserjef@msu.edu) or Sara Stoudt (sas072@bucknell.edu) for questions about the code or data used in the manuscript

## Abstract

1. Understanding variation in species occupancy is an important task for conservation. When assessing occupancy patterns over multiple temporal seasons, it is recommended to visit at least a subset of sites multiple times within a season during a period of closure to account for observation biases. However, logistical constraints can inhibit re-visitation of sites within a season, resulting in the use of single-visit multi-season occupancy models. Some have suggested that autocorrelation in space and/or time can provide "fractional replication" to separately estimate occupancy probability from detection probability, but the reliability of such approaches is not well understood.
2. We perform an extensive simulation study to assess the reliability of estimates from single-visit multi-season occupancy models under differing amounts of spatial and temporal autocorrelation ("fractional replication"). We assess model performance under both correctly specified models and multiple forms of model mis-specification, and compare estimates from single-visit models to models with varying amounts of within-season replication. We also assess the reliability of single-visit models to estimate occupancy probability of Ovenbirds (*Seiurus aurocapilla*) in New Hampshire, USA.
3. We found less bias in estimates from single-visit occupancy models with long-range spatial autocorrelation in occupancy probability compared to short-range spatial autocorrelation when the model is correctly specified. However, under certain forms of model mis-specification, estimates from single-visit multi-season occupancy models were biased and had low coverage rates regardless of the characteristics of the "fractional replication". In contrast, models with varying amounts of additional replication were robust to model mis-specification.
4. Our findings suggest that "fractional replication" cannot replace true replication in terms of occupancy probability identifiability and that researchers should consider the potential inaccuracies when using single-visit multi-season occupancy models. We show that a little true replication can go a long way with even 10% of sites being revisited within a season leading to reasonably robust estimates even in the presence of extreme model mis-specifications. When possible, we recommend performing multiple within-season visits at at least a subset of spatial locations or integrating single-visit data with other data sources to mitigate reliance on parametric assumptions required for reliable inference in single-visit multi-season occupancy models. 

## Repository Directory

All code and simulated data sets were created using `spOccupancy` v0.6.1. See the [spOccupancy Website](https://www.jeffdoser.com/files/spoccupancy-web/) and [Repository](https://github.com/doserjef/spOccupancy) for more details on working with `spOccupancy`.

### [code/case-study](./code/case-study)

+ `data-prep.R`: script to prepare the Ovenbird data for use in `spOccupancy`.
* `main.R`: script to run the 11 candidate models with varying amounts of within-season replication for the Overbird case study.
+ `summary.R`: script to summarize the results of the case study.

### [code/simulations](./code/simulations)
+ `main-sim-*`: scripts to run the simulations for different mis-specification and visit scenarios. The `*` represents a wildcard, which is replaced by the specific scenario for a given combination of amount of replication and mis-specification type. In the file names, `sv` corresponds to single-visit and `dv` corresponds to double visit. 
+ `get-sim-data-*`: scripts to generate the different data sets under the different mis-specification types.
+ `correlation-plot.R`: script to generate Figure 1 in the associated manuscript.
+ `curve-plots.R`: script to generate Figure 2 in the associated manuscript.
+ `summary.R`: script to generate all other figures in the manuscript and summarize the simulation results. 

### [figures](./figures)

Contains all figures included in the main text and the supplemental information that are generated from the scripts in the `code` directory. File names of the figures corresponding to the figure number in the manuscript.

### [data](./data)

The simulated data are too large to put on GitHub, but they can be quickly generated with the `get-sim-data-*` scripts in the [code/simulations](./code/simulations) directory.

+ `hbef-spOccupancy-data.rda`: the formatted Ovenbird data for use in fitting models with `spOccupancy`.

### [results](./results)

Summary results files for the different mis-specification and visit scenarios. Note that the complete results are not included because the files are too large for Github, and rather these summary files can be used to generate the figures in the manuscript. The file names indicate the specific scenario for a given combination of the amount of replication and mis-specification type. In the file names, `sv` corresponds to single-visit, `dv` corresponds to double-visit, and `fv` corresponds to five-visit.




