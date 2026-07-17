### A socio-economic Theory of Residential Segregation
### Part 1: Agent-based Model
### Part B: Experiment for Hypotheses Generation
### Malte Grönemann

from SocEconHousing1_abm import experiment

# Parameters and data directory for the experiment
data_dict = './data/hypotheses/'
params_exp = {
    'r_correlation': [0.4, 0.5, 0.6],
    'a_preferences': [0.2, 0.3, 0.4],
    'b_inertia': [0.94, 0.96, 0.98],
    'size': [30],
    'density': [0.85],
    'distribution': [1.5, 2, 2.5],
    'vision': [1],
    'turnover': [0.02],
    'max_time': [200]
    }
iterations = 10

parameters, results = experiment(params_exp, n_iterations=iterations, n_cores=-1)
parameters.to_parquet(data_dict + 'parameters.parquet')
results.to_parquet(data_dict + 'unit_raw.parquet')