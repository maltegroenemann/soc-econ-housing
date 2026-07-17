### Schelling Model in Python
### Malte Grönemann

import numpy as np
import pandas as pd
import random
from itertools import product
from concurrent.futures import ProcessPoolExecutor
import multiprocessing as mp
import plotly.express as px


class Household:
    """Households are agents that can move around the city. They want to find a residence where at least
    a given proportion (threshold) of neighbors belong to their own race."""
    def __init__(self, model, hh_id, x, y):
        self.model = model
        self.hh_id = hh_id
        self.x = x
        self.y = y
        self.pos = (x, y)

        if random.random() < self.model.prop_black:
            self.race = 1
        else:
            self.race = 0


    def move(self):
        """Moves the household to a random empty position if it is dissatisfied."""
        own_landlord = self.model.landlord_by_pos[self.pos]
        nb_race = own_landlord.nb_race

        if self.race == 0:
                dissatisfied = nb_race > self.model.threshold
        if self.race == 1:
            dissatisfied = (1 - nb_race) > self.model.threshold

        if dissatisfied:
            options = [ll for ll in self.model.landlords if ll.empty]
            if options:
                old_pos = self.pos
                new_pos = random.choice(options).pos
                self.pos = new_pos
                self.model.update_household_position(self, old_pos, new_pos)


class Landlord:
    """Landlords are agents that represent the cities' residences. They have a fixed position on the city grid,
    which determines which other residences are their neighborhood. The size of their neighborhood is determined by the global parameter vision.
    The Landlord class also keeps track of the households that live on them to export their data to the model's dataframe."""
    def __init__(self, model, x, y):
        self.model = model
        self.x = x
        self.y = y
        self.pos = (x, y)
        self.nb_pos = []
        self.nb_ll = []
        self.empty = True
        self.hh_id = np.nan
        self.hh_race = np.nan
        self.nb_race = np.nan

    def neighborhood(self):
        """ Returns the coordinates of the neighborhood of a given position on a torus. The size of their neighborhood is
        determined by the global parameter vision. The neighborhood is then the Moore neighborhood of all units within a distance of vision."""
        vision = self.model.vision
        size = self.model.size
        return [((self.x + dx) % size, (self.y + dy) % size)
                for dx in range(-vision, vision + 1)
                for dy in range(-vision, vision + 1)
                if not (dx == 0 and dy == 0)]

    def update(self):
        """Updates the landlord's data. The household id and race are determined by the household that occupies the landlord's position."""
        own_hh = self.model.household_by_pos.get(self.pos, None)
        if own_hh:
            self.empty = False
            self.hh_id = own_hh.hh_id
            self.hh_race = own_hh.race
        else:
            self.empty = True
            self.hh_id = np.nan
            self.hh_race = np.nan

        nb_race_values = [ll.hh_race for ll in self.nb_ll]
        if np.all(np.isnan(nb_race_values)):
            self.nb_race = np.nan
        else:
            self.nb_race = np.nanmean(nb_race_values)


class SchellingModel:
    """The Schelling model is a discrete-time stochastic agent-based model that simulates the segregation of a city.
    The model is initialized with a given size, density, proportion of black households, threshold for segregation,
    and vision parameter for neighborhood size. The model runs for a given number of time steps.
    The parameters are supplied to the model as a dictionary.
    The model's data is exported to a pandas dataframe."""
    def __init__(self, params, sample_id=1):
        self.sample_id = sample_id
        self.size = params['size']
        self.density = params['density']
        self.prop_black = params['prop_black']
        self.threshold = params['threshold']
        self.vision = params['vision']
        self.max_time = params['max_time']
        self.time = 0

        # Initialize agents and dataframe
        self.landlords = []
        self.households = []
        self.df = pd.DataFrame()

        for i in range(self.size):
            for j in range(self.size):
                self.landlords.append(Landlord(self, x=i, y=j))
                if random.random() < self.density:
                    # hh_id works for sizes below 1000, adapt for larger models to ensure unique hh_ids
                    self.households.append(Household(self, hh_id=i*1000+j, x=i, y=j))

        # Add spatial indices
        self.landlord_by_pos = {ll.pos: ll for ll in self.landlords}
        self.household_by_pos = {}
        for hh in self.households:
            self.household_by_pos[hh.pos] = hh

        for landlord in self.landlords:
            landlord.nb_pos = landlord.neighborhood()
            landlord.nb_ll = [ll for ll in self.landlords if ll.pos in landlord.nb_pos]
            landlord.update()

    def update_household_position(self, household, old_pos, new_pos):
        """Update the spatial index when a household moves"""
        if old_pos in self.household_by_pos:
            del self.household_by_pos[old_pos]
        self.household_by_pos[new_pos] = household

    def report(self):
        """ Returns a dataframe with the model's data. """
        current_data = []
        for landlord in self.landlords:
            current_data.append([landlord.x, landlord.y, landlord.empty, landlord.hh_id, landlord.hh_race, landlord.nb_race])
        current_df = pd.DataFrame(current_data, columns=['x', 'y', 'empty', 'hh_id', 'hh_race', 'nb_race'])
        current_df['sample_id'] = self.sample_id
        current_df['time'] = self.time
        current_df = pd.concat([self.df, current_df], ignore_index=True)
        return current_df.reset_index(drop=True)

    def run(self, t_messages=5):
        """ Runs the model for a given number of time steps. It reports the model's status every t_messages time steps."""
        print(f"Model {self.sample_id} started.")
        while self.time < self.max_time:
            if self.time % t_messages == 0:
                print(f"Model {self.sample_id}, step {self.time} out of {self.max_time}")
            np.random.shuffle(self.households)
            for hh in self.households:
                hh.move()
                affected_landlords = set()
                own_landlord = self.landlord_by_pos[hh.pos]
                affected_landlords.add(own_landlord)
                affected_landlords.update(own_landlord.nb_ll) # neighbors are also affected
                for landlord in affected_landlords:
                    landlord.update()
            self.df = self.report()
            self.time += 1
        print(f"Model {self.sample_id} finished.")
        return self.df

def run_single(params, sample_id):
    """Helper function to run a single experiment. This is needed for parallel execution in the experiments."""
    model = SchellingModel(params=params, sample_id=sample_id)
    model.run(t_messages=20)  # Less frequent messages to reduce output
    return model.df


def experiment(param_grid, n_iterations=5, n_cores=-1):
    """
    This function takes a dictionary of parameter values and runs the model with each combination of these values for n_iterations.
    The runs are distributed over n_cores processes. n_cores=-1 uses all available cores.
    Returns a tuple of (parameters_df, results_df) where:
    - parameters_df contains the sample_id and parameter values for each run
    - results_df contains the simulation results with sample_id linking to parameters
    """
    if n_cores == -1:
        n_cores = mp.cpu_count()
    print(f"Running experiments with {n_cores} cores")

    # Create all parameter combinations
    param_names = list(param_grid.keys())
    param_values = list(param_grid.values())
    combinations = list(product(*param_values))

    # Create full parameter list with iterations
    all_params = []
    sample_ids = []
    sample_id = 1
    for combination in combinations:
        for iteration in range(n_iterations):
            params = dict(zip(param_names, combination))
            params['sample_id'] = sample_id
            all_params.append(params)
            sample_ids.append(sample_id)
            sample_id += 1

    # Create parameters dataframe
    parameters_df = pd.DataFrame(all_params)

    print(f"Total number of experiments to run: {len(all_params)}")

    # Run experiments in parallel
    with ProcessPoolExecutor(max_workers=n_cores) as executor:
        results = list(executor.map(run_single, all_params, sample_ids))
    results_df = pd.concat(results, ignore_index=True)

    print("All experiments completed!")
    return parameters_df, results_df


## Single Run with Animation
params_singlerun = {'sample_id': 1,
                    'size': 30,
                    'density': 0.85,
                    'prop_black': 0.5,
                    'threshold': 0.3,
                    'vision': 1,
                    'max_time': 100}

model = SchellingModel(params=params_singlerun)
model.run()
results = model.df


grid_size = model.size
time_steps = sorted(results['time'].unique())
frames = []
for t in time_steps:
    time_data = results[results['time'] == t]
    # Create a pivot table to get a 2D array with explicit reindexing to ensure consistent shape
    grid = time_data.pivot(index='y', columns='x', values='hh_race')
    # Reindex to ensure all coordinates are present and in order
    grid = grid.reindex(index=range(grid_size), columns=range(grid_size), fill_value=-1)
    frames.append(grid.values)

frames = np.array(frames)
fig = px.imshow(frames,
                animation_frame=0,
                title='Segregation Over Time',
                labels={'animation_frame': 'Time Step'},
                color_continuous_scale=['gray', 'blue', 'red'],
                zmin=-1, zmax=1)

fig.update_layout(
    xaxis_title='X',
    yaxis_title='Y',
    width=600,
    height=600
)
fig.show()

## Experiment Run
params_exp = {
    'size': [30],
    'density': [0.85],
    'prop_black': [0.3, 0.5],
    'threshold': [0.2, 0.4],
    'vision': [1],
    'max_time': [100]
}

parameters, results = experiment(params_exp, n_iterations=2, n_cores=4)

parameters.to_parquet('./experiments/test/Schelling_parameters.parquet')
results.to_parquet('./experiments/test/Schelling.parquet')
