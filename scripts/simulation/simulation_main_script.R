### SIMULATION ###

setwd("~/Desktop/samplesize_for_decisionmaking")

# source packages and additional functions
source("./scripts/simulation/functions_for_simulation.R")
source("./scripts/data_wrangling/load_packages.R")


### Sample size / replication success approaches ###
### as outlined in Table 1 of preregistration ######
####################################################

# Approach A:
# Replication study powered for the effect size obtained 
# in the original study at 80% and 95% respectively

source("./scripts/simulation/sim_approach_a.R")

# Approach B:
# Replication study powered at 50% for the smallest effect size of interest (SESOI)
# SESOI 1 = 0.5
# SESOI 2 = 1.0

source("./scripts/simulation/sim_approach_b.R")

# Approach C:
# Replication study powered at 80% for the lower 80% confidence bound 
# obtained from the original study

# source("./scripts/simulation/sim_approach_c.R")

# Approach D:
# Replication study powered for reverse Bayesian approach (skeptical p-value)
# with an effect size shrinkage estimate of 25%  

# source("./scripts/simulation/sim_approach_d.R")


# combining data sets and plotting results

source("./scripts/analysis_plotting/plots.R") 


