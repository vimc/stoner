# Stoner 0.1.20

* Allow missing yll for processing older stochastics

# Changes in 0.1.19

* Add stoner_stochastic_central to create central parquet from standardised stochastics.

# Changes in 0.1.18

* Add stoner_stochastic_standardise for converting (drop)box stochastics to standard form
* Add stoner_stochastic_graphs for quick plotting of stochastics.

# Changes in 0.1.15

Fast-forward fixes:

* Fix failure to update burden_estimate_set->responsibility 
  and responsibility->scenario during a fast-forward.

* Runtime checks to prevent the above happening in the future, and look for any inconsistencies
  between responsibility->scenario->touchstone and responsibility->repsonsibility_set->touchstone.

* Prevent a dettl failure when no responsibility_comments or responsibility_set_comments exist
  during a fast-forward.

# Changes in 0.1.12

* Support for pruning obsolete burden estimate sets

# Changes in 0.1.11

* Correctly quote strings with commas when dumping CSVs. (VIMC-6544)

# Changes in 0.1.5

* Various fast-forwarding improvements, including default new responsibility being open.

# Changes in 0.1.1

* Support for fast-forwarding burden estimate sets to a future touchstone

# Changes in 0.0.9

* Support for adding DALYs to a dataset, including inline with stochastic processing

# Changes in 0.0.8

* Support for stochastic processing and upload

# Changes in 0.0.7

* Support for scenario_type.csv metadata

# Changes in 0.0.6

* Support for responsibilities.csv metadata

# Changes in 0.0.5

* Allow dumping an existing touchstone into Stoner CSVs for edit/re-import

# Changes in 0.0.4

* Support for touchstone_countries.csv metadata

# Changes in 0.0.3

* Support for touchstone_demographic_dataset.csv metadata

# Changes in 0.0.2

* Support for scenario_description.csv metadata

# Changes in 0.0.1

* Support for touchstone.csv and touchstone_name.csv metadata.
