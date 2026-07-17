### A socio-economic Theory of Residential Segregation
### Part 1: Agent-based Model
### A: Main Experiment
### Malte Grönemann

from SocEconHousing1_abm import experiment

# Parameters and data directory for the experiment
data_dict = './data/main/'
params_exp = {
    'r_correlation': [0, 0.25, 0.5, 0.75, 1],
    'a_preferences': [0, 0.25, 0.5, 0.75, 1],
    'b_inertia': [0, 0.8, .95, 0.98, 1],
    'size': [30],
    'density': [0.85],
    'distribution': [2],
    'vision': [1],
    'turnover': [0.02],
    'max_time': [400]
    }
iterations = 15

parameters, results = experiment(params_exp, n_iterations=iterations, n_cores=-1)
parameters.to_parquet(data_dict + 'parameters.parquet')
results.to_parquet(data_dict + 'unit_raw.parquet')