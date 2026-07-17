### A socio-economic Theory of Residential Segregation
### Part 1: Agent-based Model
### Robustness Check G: Size and Density
### Malte Grönemann

from SocEconHousing1_abm import experiment

# Parameters and data directory for the experiment
data_dict = './data/size/'
params_exp = {
    'r_correlation': [0, 0.5],
    'a_preferences': [0, 0.25, 1],
    'b_inertia': [0.8, 0.98],
    'size': [10, 30, 50],
    'density': [0.85, 0.9, 0.95],
    'distribution': [2],
    'vision': [1],
    'turnover': [0.02],
    'max_time': [200]
    }
iterations = 10

parameters, results = experiment(params_exp, n_iterations=iterations, n_cores=-1)
parameters.to_parquet(data_dict + 'parameters.parquet')
results.to_parquet(data_dict + 'unit_raw.parquet')