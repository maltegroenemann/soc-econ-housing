### A socio-economic Theory of Residential Segregation
### Part 1: Agent-based Model
### Robustness Check 1: Population Dynamics
### Malte Grönemann

import agentpy as ap
import numpy as np
import pandas as pd
import random


## Defining Household Class Agents
class Household(ap.Agent):

    def setup(self):
        """ Household agents are initiated with random income and status. """
        r_correlation = self.model.p.r_correlation
        distribution = self.model.p.distribution
        # fixed attributes
        self.income = np.random.beta(a = distribution, b = 2.5 * distribution, size = 1)[0]
        self.status = (1 - r_correlation) * np.random.beta(a = distribution, b = 2.5 * distribution, size = 1)[0] + r_correlation * self.income

    def moving(self):
        """ Households move to the housing unit that maximises their residential satisfaction given their income.
        If households cannot afford any available housing units, they move to the unit with cheapest rent. """
        available_positions = set(self.model.household_grid.empty)
        available_positions.add(self.model.household_grid.positions[self])
        # Filter the available choice set (Landlords in available positions)
        choice_set = [
            landlord for landlord, pos in self.model.landlord_grid.positions.items()
            if pos in available_positions
        ]
        # Filter choice set by affordability (budget set)
        budget_set = [landlord for landlord in choice_set if landlord.rent <= self.income]
        if budget_set:  # If there are options within the budget
            # Find the housing unit with the maximum utility within the budget
            max_utility = max(budget_set, key=lambda landlord: landlord.utility)
            housing_unit = self.model.landlord_grid.positions[max_utility]
        else:  # No affordable options, select the cheapest rent
            min_rent = min(choice_set, key=lambda landlord: landlord.rent)
            housing_unit = self.model.landlord_grid.positions[min_rent]
        # Move to the selected housing unit
        self.model.household_grid.move_to(self, housing_unit)


## Defining Landlord Class Agents
class Landlord(ap.Agent):

    def setup(self):
        """ Landlord agents are initiated with initially random housing quality.
        Utility and rents are equal to the housing quality at setup.
        Various variables are initiated that are filled at model setup or updated throughout the simulation."""
        distribution = self.model.p.distribution
        self.housing_quality = np.random.beta(a=distribution, b=2.5 * distribution, size=1)[0]
        self.utility = self.housing_quality
        self.rent = self.housing_quality
        self.pos = None  # filled at model setup
        self.x_coord = None
        self.y_coord = None
        self.nb_pos = []
        self.nb_landlords = []
        self.hh_id = None  # updated throughout simulation based on household occupying the unit
        self.hh_income = None
        self.hh_status = None

    def update_quality_and_utility(self):
        """ If neighborhood average rent rises, landlords invest in their housing quality. If not, it decays.
        Utility is calculated based on the housing quality and the status of the households in the neighborhood."""
        a_preferences = self.model.p.a_preferences
        d_decay = self.model.p.d_decay
        households_nb = (household for household, position in self.model.household_grid.positions.items() if
                         position in self.nb_pos)

        mean_rent = np.mean([landlord.rent for landlord in self.nb_landlords])
        if self.housing_quality <= mean_rent:
            self.housing_quality = mean_rent
        else:
            self.housing_quality *= d_decay

        household_status_values = [household.status for household in households_nb]
        if household_status_values:
            mean_status = np.mean(household_status_values)
            self.utility = (mean_status ** a_preferences) * (self.housing_quality ** (1 - a_preferences))
        else:
            self.utility = 0

    def reporting(self):
        """ I only export data from the landlords.
        To also have access to the household data,
        I get the household id, income, and status from the household agent that occupies the landlord's unit."""
        my_renter = next( # as there is only one, the iteration can stop when a match has been found
            (household for household, pos in self.model.household_grid.positions.items() if pos == self.pos),
            None
        )
        if my_renter is None:
            self.hh_id = None
            self.hh_income = None
            self.hh_status = None
        else:
            self.hh_id = my_renter.id
            self.hh_income = my_renter.income
            self.hh_status = my_renter.status

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
        [(landlord.utility, landlord.hh_income) for landlord in model.landlords],
        columns=['utility', 'hh_income']
    )
    return utility_income_df


## Defining the Model
class SocEconHousing(ap.Model):

    def setup(self):
        size = self.p.size
        density = self.p.density
        vision = self.p.vision
        n_housing_units = size ** 2
        n_households = int(density * n_housing_units)

        # Create Household grid and Household class agents and distribute them randomly on empty cells.
        self.household_grid = ap.Grid(self,
                                      shape = (size, size),
                                      torus = True,
                                      track_empty = True)
        self.households = ap.AgentList(self, n_households, Household)
        self.household_grid.add_agents(self.households,
                                       random = True,
                                       empty = True)
        for household in self.household_grid.agents:
            household.pos = self.household_grid.positions[household]
            household.x_coord, household.y_coord = household.pos

        # Create Landlord grid and Landlord class agents and add exactly one to every cell.
        self.landlord_grid = ap.Grid(self,
                                     shape = (size, size),
                                     torus = True,
                                     track_empty = True)
        self.landlords = ap.AgentList(self, n_housing_units, Landlord)
        self.landlord_grid.add_agents(self.landlords,
                                      random = True,
                                      empty = True)
        for landlord in self.landlord_grid.agents:
            landlord.pos = self.landlord_grid.positions[landlord]
            landlord.x_coord, landlord.y_coord = landlord.pos
            landlord.nb_landlords = self.model.landlord_grid.neighbors(landlord, distance = vision).to_list()  # list of Landlord class agents on neighboring positions
            landlord.nb_pos = [self.model.landlord_grid.positions[landlord] for landlord in landlord.nb_landlords]  # list of position tuples of the neighborhood

        self.utility_income_df = utility_income_data(self)


    def step(self):
        """ Step function for the model. First, the landlords decide whether to invest into their housing quality,
        then they set their rents, and finally the households move to the housing unit with the highest utility.
        If there is turnover, households are removed and new ones are added."""
        self.landlords.update_quality_and_utility()
        self.utility_income_df = utility_income_data(self)
        self.landlords.update_rent()
        self.landlords.reporting()

        self.household_grid.agents.moving()

        # population dynamics
        if self.model.p.turnover > 0:
            turnover = self.p.turnover
            n_households = int(self.p.density * self.p.size * self.p.size)
            pop_change = int(turnover * n_households)

            outmovers = random.sample(list(self.household_grid.agents), pop_change)
            self.household_grid.remove_agents(outmovers)

            inmovers = ap.AgentList(self, pop_change, Household)
            self.household_grid.add_agents(inmovers, random=True, empty=True)
            for household in inmovers:
                household.pos = self.household_grid.positions[household]
                household.x_coord, household.y_coord = household.pos

    def update(self):
        """ Update function for the model. I only export data from the landlords."""
        self.landlords.record(['id', 'x_coord', 'y_coord', 'housing_quality', 'utility', 'rent', 'hh_id', 'hh_income', 'hh_status'])


## Population Dynamics and Noise Experiment
parameters = dict({
    'r_correlation': ap.Values(0, 0.5, 1),
    'a_preferences': ap.Values(0, 0.25, 1),
    'd_decay': ap.Values(0.95, 0.8),
    'size': 30,
    'density': 0.85,
    'vision': 1,
    'distribution': 2,
    'turnover': ap.Values(0, 0.02, 0.05, 0.1),
    'steps': 200
})
iterations = 10

## Experiment Run
parameters_df = pd.DataFrame()
unit_raw = pd.DataFrame()
for i in range(iterations):
    experiment = ap.Experiment(SocEconHousing,
                               ap.Sample(parameters),
                               iterations=1,
                               record=True)
    results = experiment.run(n_jobs=-1, verbose=10)  # parallelize simulation using all available cores

    parameters_df_temp = results["parameters"]["sample"].reset_index()
    parameters_df_temp['sample_id'] = str(i) + "_" + parameters_df_temp['sample_id'].astype(str)
    unit_raw_temp = results["variables"]["Landlord"].reset_index()
    unit_raw_temp['sample_id'] = str(i) + "_" + unit_raw_temp['sample_id'].astype(str)

    parameters_df = pd.concat([parameters_df, parameters_df_temp], ignore_index=True)
    unit_raw = pd.concat([unit_raw, unit_raw_temp], ignore_index=True)

## Data Export
parameters_df.to_parquet('./data/popdyn/parameters.parquet')
unit_raw.to_parquet('./data/popdyn/unit_raw.parquet')