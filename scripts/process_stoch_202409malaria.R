base_in_path <- "//wpia-hn2.hpc.dide.ic.ac.uk/vimc_stochastics_dropbox/latest/202409malaria-1"
base_out_path <- "//wpia-hn2.hpc.dide.ic.ac.uk/vimc_stochastics/202409malaria"

scenarios = c("malaria_no_vaccination", "malaria-r3-r4-default", "malaria-rts3-rts4-default")

stoner::stone_stochastic_standardise(
  group = "IC-Okell",
  in_path = file.path(base_in_path, "Malaria-IC-Okell"),
  out_path = file.path(base_out_path, "Malaria_IC-Okell"),
  scenarios = scenarios,
  files = c("stochastic-burden-est-no-vaccination_:index Lydia Haile.csv.xz",
            "stochastic-burden-est-malaria-r3-r4-default_:index Lydia Haile.csv.xz",
            "stochastic-burden-est-malaria-rts3-rts4-default_:index Lydia Haile.csv.xz"),
  index = 1:200
)

stoner::stone_stochastic_standardise(
  group = "UAC-Kakai",
  in_path = file.path(base_in_path, "Malaria-UAC-Kakai"),
  out_path = file.path(base_out_path, "Malaria_UAC-Kakai"),
  scenarios = scenarios,
  files = c("Stochastic_Burden_Estimates_Glele_Kakai_No_Vaccine_:index.csv.xz",
            "Stochastic_Burden_Estimates_Glele_Kakai_Default_r34_:index.csv.xz",
            "Stochastic_Burden_Estimates_Glele_Kakai_Default_rts34_:index.csv.xz"),
  index = 1:200
)

stoner::stone_stochastic_standardise(
  group = "TKI-Penny",
  in_path = file.path(base_in_path, "Malaria-TKI-Penny"),
  out_path = file.path(base_out_path, "Malaria_TKI-Penny"),
  scenarios = scenarios,
  files = c("stochastic_burden_est_malaria_:index_novaccine Josephine Malinga.csv.xz",
            "stochastic_burden_est_malaria_:index_r21_d4_default Josephine Malinga.csv.xz",
            "stochastic_burden_est_malaria_:index_rtss_d4_default Josephine Malinga.csv.xz"),
  index = 1:31
)
