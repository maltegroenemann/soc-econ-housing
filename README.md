# A socio-economic Model of Segregation, Residential Mobility and Housing Inequality

## Abstract

Landlords have been largely overlooked in research on segregation, neighborhoods and housing inequality. However, they have significant agency about the type and quality of housing in a given area. Given that households often value properties of housing units more than properties of the neighborhood and that housing is unequally distributed in space, existing theories likely misrepresent how and why residential segregation emerges and neighborhoods form. In this article, I present a theoretical model of the housing market that considers both the preferences and decisions of households as well as the agency of landlords. Based on three empirically grounded assumptions (households value the quality of a housing unit and the status of the neighborhood while landlords invest in their units when rents in the neighborhood rise), I can unify and explain multiple phenomena in urban sociology, such as the emergence and stability of segregated neighborhoods and patterns of inequalities in residential mobility, housing and neighborhood quality, as well as housing affordability.

## Contents and Structure of the Project

This repository contains all code and analyses written for the agent-based model underlying the respective article. 

The **root folder** of this repository contains:

- SocEconHousing.tex the tex-file of the article
- SocEconHousing.pdf the current preprint of the article
- SocEconHousing1_abm.ipynb is a Jupyter notebook that describes the theoretical model in full detail, runs an animation of the model for inspection and gives a summary of all the computational experiments.

The **experiments** folder contains code and analysis files of all computational experiments. 

As the data preparation is identical for all experiments, I have only one script for data preparation.
- SocEconHousing2_dataprep.R

Each computational experiment, the main experiment and the robustness checks, described in the Jupyter notebook in the root folder has a subfolder:
- a_main: main analysis of the model and basis for graphics in the article
- b_popdyn: effect of population dynamics
- c_ineq: varies the level of income inequality in the simulations
- d_vision: varies the definition of neighborhood
- e_NetLogo: implements the model in NetLogo as a robustness check
- d_hypotheses: experiment only using reasonable parameters for hypothesis generation

Each of these folders contains:
- SocEconHousing1*_exp.py: a Python script that runs the computational experiments and outputs the raw simulation data. (in e, it is obviously a NetLogo file)
- SocEconHousing3*_analysis.Rmd: a R Markdown that performs the analysis of the respective experiment.
- SocEconHousing3*_analysis.html: This html file is probably the most important file in each folder. It contains all analyses including the interpretations of the graphics and tables. There is again a description of the respective experiment and a summary of the results from this experiment at the top.
- in a: SocEconHousing3_article.R creates the graphics that are saved to the images folder and shown in the article

The **images** folder contains all images imported into the article. Most are based on the main experiment, created by SocEconHousing3_article.R.

## Availability of Simulation Data

Due to large file sizes, I have not uploaded the simulation data (which are saved in a *data* folder in each experiment folder). They are saved as parquet files (NetLogo: the raw data are a CSV). They are eventually available in the future. If you want access, please reach out to me.
- The paramaters dataset links each sample_id to the used values of global parameters. They are merged with the raw data in data preparation.
- The raw unit data contain all variables concerning housing units and households occupying them.
- The unit data disgard the burn-in period, contain some additional calculated variables and neighborhood averages.
- The neighborhood data takes mean values of the housing units in each neighborhood and calculates a rank order within each city.
- The city data contain aggregate measures of segregation and inequality.


