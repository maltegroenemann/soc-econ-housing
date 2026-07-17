### A socio-economic Theory of Residential Segregation
### Part 1: Agent-based Model
### Gentrification Variant and Experiment
### Malte Grönemann

import numpy as np
import pandas as pd
import random
from itertools import product
from concurrent.futures import ProcessPoolExecutor
import multiprocessing as mp


class Household:
    """Households are agents that can move around the city. They want to find a residence where at least
    a given proportion (threshold) of neighbors belong to their own race."""
    def __init__(self, model, hh_id, x, y):
        self.model = model
        self.hh_id = hh_id
        self.pos = (x, y)

        r_correlation = self.model.p['r_correlation']
        distribution = self.model.p['distribution']
        # fixed attributes
        self.income = np.random.beta(a = distribution, b = 2.5 * distribution, size = 1)[0]
        self.status = (1 - r_correlation) * np.random.beta(a = distribution, b = 2.5 * distribution, size = 1)[0] + r_correlation * self.income


    def move(self):
        """ Households move to the housing unit that maximises their residential satisfaction given their income.
        If households cannot afford any available housing units, they move to the unit with cheapest rent. """
        choice_set = set([ll for ll in self.model.landlords if ll.empty])
        choice_set.add(self.model.landlord_by_pos[self.pos])
        # Filter choice set by affordability (budget set)
        budget_set = [ll for ll in choice_set if ll.rent <= self.income]
        if budget_set:  # If there are options within the budget
            # Find the housing unit with the maximum utility within the budget
            max_utility_landlord = max(budget_set, key=lambda landlord: landlord.utility)
            housing_unit = max_utility_landlord.pos
        else:  # No affordable options, select the cheapest rent
            min_rent_landlord = min(choice_set, key=lambda landlord: landlord.rent)
            housing_unit = min_rent_landlord.pos
        # Move to the selected housing unit
        old_pos = self.pos
        new_pos = housing_unit
        self.pos = new_pos
        self.model.update_household_position(self, old_pos, new_pos)
        return old_pos, new_pos  # Return both positions for tracking affected landlords in run



class Landlord:
    """ Landlord agents are initiated with initially random housing quality.
        Utility and rents are equal to the housing quality at setup.
        Various variables are initiated that are filled at model setup or updated throughout the simulation."""
    def __init__(self, model, x, y):
        self.model = model
        self.x = x
        self.y = y
        self.pos = (x, y)
        self.nb_pos = []
        self.nb_ll = []
        self.empty = True

        distribution = self.model.p['distribution']
        self.housing_quality = np.random.beta(a = distribution, b = 2.5 * distribution, size = 1)[0]
        self.utility = self.housing_quality
        self.rent = self.housing_quality
        self.hh_id = None # updated throughout simulation based on household occupying the unit
        self.hh_income = None
        self.hh_status = None


    def neighborhood(self):
        """ Returns the coordinates of the neighborhood of a given position on a torus. The size of their neighborhood is
        determined by the global parameter vision. The neighborhood is then the Moore neighborhood of all units within a distance of vision."""
        vision = self.model.p['vision']
        size = self.model.p['size']
        return [((self.x + dx) % size, (self.y + dy) % size)
                for dx in range(-vision, vision + 1)
                for dy in range(-vision, vision + 1)
                if not (dx == 0 and dy == 0)]


    def invest(self):
        """ If neighborhood average rent increases, landlords invest in their housing quality proportionally to the change. If average rents decrease, housining quality decreases as well. Housing quality is path dependent, a proportion of the previous quality is retained. Utility is calculated based on the housing quality and the status of the households in the neighborhood."""
        # update quality
        a_preferences = self.model.p['a_preferences']
        b_inertia = self.model.p['b_inertia']
        mean_rent = np.mean([ll.rent for ll in self.nb_ll])
        self.housing_quality = b_inertia * self.housing_quality + (1 - b_inertia) * mean_rent

        # update utility
        hh_status_nb = [ll.hh_status for ll in self.nb_ll]
        hh_status_nb = [x for x in hh_status_nb if not np.isnan(x)]
        if hh_status_nb:
            mean_status = np.mean(hh_status_nb)
        else:
            mean_status = 0
        self.utility = (mean_status ** a_preferences) * (self.housing_quality ** (1 - a_preferences))


    def update_hhvars(self):
        """ I only export data from the landlords. To also have access to the household data, I get the household id, income, and status from the household agent that occupies the landlord's unit."""
        my_renter = self.model.household_by_pos.get(self.pos, None)
        if my_renter:
            self.empty = False
            self.hh_id = my_renter.hh_id
            self.hh_income = my_renter.income
            self.hh_status = my_renter.status
        else:
            self.empty = True
            self.hh_id = np.nan
            self.hh_income = np.nan
            self.hh_status = np.nan


    def update_rent(self): # for substantive description, see next section.
        """ Rents are calculated based on the city-wide distribution of utility and income."""
        competition = self.model.utility_income_df[
            (self.model.utility_income_df['utility'] <= self.utility) &
            self.model.utility_income_df['hh_income'].notna()
            ]['hh_income']
        if competition.empty:
            self.rent = self.model.utility_income_df['hh_income'].min()
        else:
            self.rent = np.percentile(competition, 75)


## Create a dataframe of all incomes and utilities for rent calculations of households
def utility_income_data(model):
    """ Create a dataframe of all incomes and utilities for rent calculations of landlords. """
    utility_income_df = pd.DataFrame(
        [(ll.utility, ll.hh_income) for ll in model.landlords],
        columns=['utility', 'hh_income']
    )
    return utility_income_df

## Defining the Model
class SocEconHousing:
    """This model is a discrete-time stochastic agent-based model that simulates the socio-economic segregation of a city and the formation of stable distinct neighborhoods, both demographically and in housing. The model is initialized with a given size, density, ... . The model runs for a given number of time steps.
    The parameters are supplied to the model as a dictionary.
    The model's data is exported to a pandas dataframe."""
    def __init__(self, params, sample_id=1):
        self.sample_id = sample_id
        self.p = params
        self.time = 0

        # Intervention-related attributes
        self.intervention_time = self.p.get('intervention_time', None)
        self.post_distribution = self.p.get('post_distribution', None)
        self.post_density = self.p.get('post_density', None)
        self.intervention_applied = False

        # Initialize agents and dataframe
        self.landlords = []
        self.n_households = int((self.p['size'] ** 2) * self.p['density'])
        self.households = []
        self.df = pd.DataFrame()

        for i in range(self.p['size']):
            for j in range(self.p['size']):
                self.landlords.append(Landlord(self, x=i, y=j))

        for ll in random.sample(self.landlords, self.n_households):
            # hh_id works for sizes below 1000, adapt for larger models to ensure unique hh_ids
            self.households.append(Household(self, hh_id=ll.x*1000+ll.y, x=ll.x, y=ll.y))

        # Add spatial indices
        self.landlord_by_pos = {ll.pos: ll for ll in self.landlords}
        self.household_by_pos = {}
        for hh in self.households:
            self.household_by_pos[hh.pos] = hh

        # Prepare neighborhood variables of the landlords
        for landlord in self.landlords:
            landlord.nb_pos = landlord.neighborhood()
            landlord.nb_ll = [ll for ll in self.landlords if ll.pos in landlord.nb_pos]
            landlord.update_hhvars()


    def update_household_position(self, household, old_pos, new_pos):
        """Update the spatial index when a household moves"""
        if old_pos in self.household_by_pos:
            del self.household_by_pos[old_pos]
        self.household_by_pos[new_pos] = household


    def report(self):
        """ Returns a dataframe with the model's data. """
        current_data = []
        for ll in self.landlords:
            current_data.append([ll.x, ll.y, ll.housing_quality, ll.utility, ll.rent, ll.hh_id, ll.hh_income, ll.hh_status])
        current_df = pd.DataFrame(current_data, columns=['x', 'y', 'housing_quality', 'utility', 'rent', 'hh_id', 'hh_income', 'hh_status'])
        current_df['sample_id'] = self.sample_id
        current_df['t'] = self.time
        current_df = pd.concat([self.df, current_df], ignore_index=True)
        return current_df.reset_index(drop=True)


    def population_dynamics(self):
        """Remove and add households to simulate population dynamics"""
        pop_change = int(self.p['turnover'] * self.n_households)
        # Removing households
        outmovers = random.sample(self.households, pop_change)
        affected_by_removal = set()
        for hh in outmovers:
            if hh.pos in self.household_by_pos:
                old_landlord = self.landlord_by_pos[hh.pos]
                affected_by_removal.add(old_landlord)
                affected_by_removal.update(old_landlord.nb_ll)
                del self.household_by_pos[hh.pos] # Remove from spatial index
        self.households = [hh for hh in self.households if hh not in outmovers] # Remove households from list
        del outmovers # delete agent objects from memory
        for ll in affected_by_removal:
            ll.update_hhvars()
        # Adding households
        available_positions = [ll.pos for ll in self.landlords if ll.empty]
        new_positions = random.sample(available_positions, pop_change)
        affected_by_addition = set()
        for i, pos in enumerate(new_positions):
            x, y = pos
            hh_id = self.time * 10000 + i # Create unique hh_id
            new_hh = Household(self, hh_id, x, y)
            self.households.append(new_hh)
            self.household_by_pos[pos] = new_hh
            new_landlord = self.landlord_by_pos[pos]
            affected_by_addition.add(new_landlord)
            affected_by_addition.update(new_landlord.nb_ll)
        for ll in affected_by_addition:
            ll.update_hhvars()


    def apply_intervention(self):
        """
        Apply time-dependent changes to model parameters after equilibrium.
        This is called once when self.time == self.intervention_time.
        """
        # Change distribution parameter used for new households / attributes
        if self.post_distribution is not None:
            self.p['distribution'] = self.post_distribution

        # Change density by adjusting number of households in the city
        if self.post_density is not None:
            target_n_households = int((self.p['size'] ** 2) * self.post_density)
            current_n = len(self.households)

            if target_n_households < current_n:
                # Remove households to reduce density
                n_remove = current_n - target_n_households
                to_remove = random.sample(self.households, n_remove)
                affected = set()
                for hh in to_remove:
                    if hh.pos in self.household_by_pos:
                        old_landlord = self.landlord_by_pos[hh.pos]
                        affected.add(old_landlord)
                        affected.update(old_landlord.nb_ll)
                        del self.household_by_pos[hh.pos]
                self.households = [hh for hh in self.households if hh not in to_remove]
                for ll in affected:
                    ll.update_hhvars()

            elif target_n_households > current_n:
                # Add households to increase density
                n_add = target_n_households - current_n
                available_positions = [ll.pos for ll in self.landlords if ll.empty]
                if n_add > len(available_positions):
                    n_add = len(available_positions)
                new_positions = random.sample(available_positions, n_add)
                affected = set()
                for i, pos in enumerate(new_positions):
                    x, y = pos
                    hh_id = self.time * 10000 + i  # keep id-creation scheme
                    new_hh = Household(self, hh_id, x, y)
                    self.households.append(new_hh)
                    self.household_by_pos[pos] = new_hh
                    new_landlord = self.landlord_by_pos[pos]
                    affected.add(new_landlord)
                    affected.update(new_landlord.nb_ll)
                for ll in affected:
                    ll.update_hhvars()

            # Keep internal reference consistent with new density
            self.p['density'] = self.post_density
            self.n_households = len(self.households)

        self.intervention_applied = True
        print(f"Intervention applied to Model {self.sample_id} at time {self.time}.")


    def run(self, t_messages=5):
        """ Runs the model for a given number of time steps. It reports the model's status every t_messages time steps.
        Order of operations: Landlords invest first, then households move.
        This creates more realistic, smoother dynamics as landlords make decisions based on
        recent neighborhood trends rather than instantaneous household movements.

        1. Landlords invest (update quality and utility based on previous period)
        2. Landlords update rent (based on new utilities)
        3. Households move (based on updated rents and utilities)
        4. Record data
        5. Population dynamics (if applicable)

        t_messages specifies how often to report the status of a model run. Default is 5, suppress messages by setting to 0.
        """
        print(f"Model {self.sample_id} started.")
        while self.time < self.p['max_time']:
            if (
                    self.intervention_time is not None
                    and not self.intervention_applied
                    and self.time >= self.intervention_time
            ):
                self.apply_intervention()

            if t_messages > 0 and self.time % t_messages == 0:
                print(f"Model {self.sample_id}, step {self.time} out of {self.p['max_time']}")
            # Step 1: Landlords invest based on PREVIOUS period's neighborhood composition
            for ll in self.landlords:
                ll.invest()
            # Step 2: Update rents based on NEW utilities and PREVIOUS period's household distribution
            self.utility_income_df = utility_income_data(self)
            for ll in self.landlords:
                ll.update_rent()
            # Step 3: Households move (based on updated rents and utilities)
            np.random.shuffle(self.households)
            for hh in self.households:
                hh.move()
                for ll in self.landlords:
                    ll.update_hhvars()
            # Step 5: Record data
            self.df = self.report()
            # Step 6: Population dynamics (if turnover > 0)
            if self.p['turnover'] > 0:
                self.population_dynamics()
            # increase time by one
            self.time += 1
        print(f"Model {self.sample_id} finished.")
        return self.df


## Experiment functions
def run_single(params, sample_id):
    """Helper function to run a single experiment. This is needed for parallel execution in the experiments."""
    model = SocEconHousing(params=params, sample_id=sample_id)
    model.run(t_messages=0)  # Less frequent messages to reduce output
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

## Run Gentrification Experiment
data_dict = './test/' # TODO './data/gentri/'
params_exp = {
    'r_correlation': [0.5],
    'a_preferences': [0.25],
    'b_inertia': [0.95],
    'size': [30],
    'density': [0.9],
    'distribution': [2],
    'vision': [1],
    'turnover': [0.02],
    'max_time': [200],
    # New experiment parameters:
    'intervention_time': [100],
    'post_distribution': [1, 2, 3],
    'post_density': [0.85, 0.9, 0.95],
}
iterations = 1 # TODO 20

parameters, results = experiment(params_exp, n_iterations=iterations, n_cores=5)
parameters.to_parquet(data_dict + 'parameters.parquet')
results.to_parquet(data_dict + 'unit_raw.parquet')