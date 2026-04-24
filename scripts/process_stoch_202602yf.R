base_in_path <- "//wpia-hn2.hpc.dide.ic.ac.uk/vimc_stochastics_dropbox/latest/202602yf"
base_out_path <- "//wpia-hn2.hpc.dide.ic.ac.uk/vimc_stochastics/202602yf"

scenarios = c("yf-no-vaccination", "yf-routine-default",
              "yf-routine-campaign-default")

stoner::stone_stochastic_standardise(
  group = "IC-Gaythorpe",
  in_path = file.path(base_in_path, "YF-IC-Gaythorpe"),
  out_path = file.path(base_out_path, "YF_IC-Gaythorpe"),
  scenarios = scenarios,
  files = c("burden_results_stochastic_2026_updates_01_novacc_:index.csv.xz",
            "burden_results_stochastic_2026_updates_02_default_routine_:index.csv.xz",
            "burden_results_stochastic_2026_updates_03_default_routine_campaign_:index.csv.xz"),
  index = 1:200)

stoner::stone_stochastic_standardise(
  group = "UND-Perkins",
  in_path = file.path(base_in_path, "YF-UND-Perkins"),
  out_path = file.path(base_out_path, "YF_UND-Perkins"),
  scenarios = scenarios,
  files = "stochastic_burden_est_YF_UND-Perkins_yf-scenario-1-default-Routine_and_Campaign_:index.csv.xz",
  index = 1:200)
