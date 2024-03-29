# 0.1.15

Fast-forward fixes:

  * Fix failure to update burden_estimate_set->responsibility 
    and responsibility->scenario during a fast-forward.

  * Runtime checks to prevent the above happening in the future, and look for any inconsistencies
    between responsibility->scenario->touchstone and responsibility->repsonsibility_set->touchstone.

  * Also prevent a dettl failure when no responsibility_comments or responsibility_set_comments exist
    during a fast-forward.

# 0.1.12

Support for pruning obsolete burden estimate sets

# 0.1.11

Correctly quote strings with commas when dumping CSVs. (VIMC-6544)

# 0.1.5

Various fast-forwarding improvements, including default new responsibility being open.

# 0.1.1

Support for fast-forwarding burden estimate sets to a future touchstone

# 0.0.9

Support for adding DALYs to a dataset, including inline with stochastic processing

# 0.0.8

Support for stochastic processing and upload

# 0.0.7

Support for scenario_type.csv metadata

# 0.0.6

Support for responsibilities.csv metadata

# 0.0.5

Allow dumping an existing touchstone into Stoner CSVs for edit/re-import

# 0.0.4

Support for touchstone_countries.csv metadata

# 0.0.3

Support for touchstone_demographic_dataset.csv metadata

# 0.0.2

Support for scenario_description.csv metadata

# 0.0.1

Support for touchstone.csv and touchstone_name.csv metadata.
