### A socio-economic Theory of Residential Segregation
### Part 1: Agent-based Model
### Robustness Check D: Population Dynamics
### Malte Grönemann

from SocEconHousing1_abm import experiment

# Parameters and data directory for the experiment
data_dict = './data/popdyn/'
params_exp = {
    'r_correlation': [0, 0.5, 1],
    'a_preferences': [0, 0.25, 1],
    'b_inertia': [0.8, 0.95, 0.98],
    'size': [30],
    'density': [0.85],
    'distribution': [2],
    'vision': [1],
    'turnover': [0, 0.02, 0.05, 0.1],
    'max_time': [200]
    }
iterations = 10

parameters, results = experiment(params_exp, n_iterations=iterations, n_cores=-1)
parameters.to_parquet(data_dict + 'parameters.parquet')
results.to_parquet(data_dict + 'unit_raw.parquet')