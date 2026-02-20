base_in_path <- "//wpia-hn2.hpc.dide.ic.ac.uk/vimc_stochastics_dropbox/latest/202310gavi"
base_out_path <- "//wpia-hn2.hpc.dide.ic.ac.uk/vimc_stochastics/202310gavi"


###############
# Cholera

scenarios = c("cholera-no-vaccination", "cholera-ocv1-default", "cholera-ocv1-ocv2-default")

stoner::stone_stochastic_standardise(
  group = "IVI-Kim",
  in_path = file.path(base_in_path, "Cholera-IVI-Kim"),
  out_path = file.path(base_out_path, "Cholera-IVI-Kim"),
  scenarios = scenarios,
  files = c("stoch_Cholera_novacc_20250704T130601 Jong-Hoon Kim.csv.xz",
            "stoch_Cholera_campaign_default_ocv1_20250704T130601 Jong-Hoon Kim.csv.xz",
            "stoch_Cholera_campaign_default_ocv12_20250704T130601 Jong-Hoon Kim.csv.xz")
  )

stoner::stone_stochastic_standardise(
  group = "JHU-Lee",
  in_path = file.path(base_in_path, "Cholera-JHU-Lee"),
  out_path = file.path(base_out_path, "Cholera-JHU-Lee"),
  scenarios = scenarios,
  files = c("stochastic-burden-template.202310gavi-4.Cholera_standard_template.529.no-vaccination Christina Alam.csv.xz",
            "stochastic-burden-template.202310gavi-4.Cholera_standard_template.529.ocv1-default_one Christina Alam.csv.xz",
            "stochastic-burden-template.202310gavi-4.Cholera_standard_template.529.ocv1-ocv2-default_two Christina Alam.csv.xz")
)


###############
# COVID

 scenarios = c("covid-no-vaccination", "covid-no-vaccination_severe",
               "covid-primary-bluesky", "covid-primary-bluesky_severe",
               "covid-primary-booster-bluesky", "covid-primary-booster-bluesky_severe",
               "covid-primary-booster-default", "covid-primary-booster-default_severe",
               "covid-primary-default", "covid-primary-default_severe")

stoner::stone_stochastic_standardise(
  group = "IC-Ghani",
  in_path = file.path(base_in_path, "COVID-IC-Ghani", "wes-modified"),
  out_path = file.path(base_ou6t_path, "COVID_IC-Ghani"),
  scenarios = scenarios,
  files = c("covid-no-vaccination Gemma Gilani_:index.csv.xz",
            "covid-no-vaccination_severe Gemma Gilani_:index.csv.xz",
            "covid-primary-bluesky Daniela Olivera_:index.csv.xz",
            "covid-primary-bluesky_severe Daniela Olivera_:index.csv.xz",
            "covid-primary-booster-bluesky Daniela Olivera_:index.csv.xz",
            "covid-primary-booster-bluesky_severe Daniela Olivera_:index.csv.xz",
            "covid-primary-booster-default Gemma Gilani_:index.csv.xz",
            "covid-primary-booster-default_severe Gemma Gilani_:index.csv.xz",
            "covid-primary-default Gemma Gilani_:index.csv.xz",
            "covid-primary-default_severe Gemma Gilani_:index.csv.xz"),
  index = 1:200)

stoner::stone_stochastic_standardise(
  group = "LSHTM-Liu",
  in_path = file.path(base_in_path, "COVID-LSHTM-Liu"),
  out_path = file.path(base_out_path, "COVID_LSHTM-Liu"),
  scenarios = scenarios,
  files = "stochastic_burden_est_covid-LSHTM-Liu_:scenario Yang Liu.csv.xz")


###############
# HepB

 scenarios <- scenarios = c("hepb-hepb3-bd-bluesky", "hepb-hepb3-bd-default",
               "hepb-hepb3-bd-ia2030", "hepb-hepb3-bluesky",
               "hepb-hepb3-default", "hepb-hepb3-ia2030",
               "hepb-no-vaccination")

stoner::stone_stochastic_standardise(
  group = "Li",
  in_path = file.path(base_in_path, "HepB-Li"),
  out_path = file.path(base_out_path, "HepB_Li"),
  scenarios = scenarios,
  files = ":scenario:index.csv.xz",
  index = 1:200)

stoner::stone_stochastic_standardise(
  group = "IC-Hallett",
  in_path = file.path(base_in_path, "HepB-IC-Hallett"),
  out_path = file.path(base_out_path, "HepB_IC-Hallett"),
  scenarios = scenarios,
  files = c("stochastic_burden_est_HepB-IC-Hallett_hepb_hepb3_bd_bluesky_:index.csv.xz",
            "stochastic_burden_est_HepB-IC-Hallett_hepb_hepb3_bd_default_:index.csv.xz",
            "stochastic_burden_est_HepB-IC-Hallett_hepb_hepb3_bd_ia2030_:index.csv.xz",
            "stochastic_burden_est_HepB-IC-Hallett_hepb_hepb3_bluesky_:index.csv.xz",
            "stochastic_burden_est_HepB-IC-Hallett_hepb_hepb3_default_:index.csv.xz",
            "stochastic_burden_est_HepB-IC-Hallett_hepb_hepb3_ia2030_:index.csv.xz",
            "stochastic_burden_est_HepB-IC-Hallett_hepb_no_vaccination_:index.csv.xz"),
  index = 1:200)

stoner::stone_stochastic_standardise(
  group = "Burnet-Scott",
  in_path = file.path(base_in_path, "HepB-Burnet-Scott"),
  out_path = file.path(base_out_path, "HepB_Burnet-Scott"),
  scenarios = scenarios,
  files = c("stochastic_burden_hepb_hepb3_bd_bluesky_:index.csv.xz",
            "stochastic_burden_hepb_hepb3_bd_default_:index.csv.xz",
            "stochastic_burden_hepb_hepb3_bd_ia2030_:index.csv.xz",
            "stochastic_burden_hepb_hepb3_bluesky_:index.csv.xz",
            "stochastic_burden_hepb_hepb3_default_:index.csv.xz",
            "stochastic_burden_hepb_hepb3_ia2030_:index.csv.xz",
            "stochastic_burden_hepb_no_vaccination_:index.csv.xz"),
  index = 1:117)


###############
# HPV

 scenarios =  c("hpv-no-vaccination", "hpv-campaign-default",
                "hpv-campaign-bluesky", "hpv-campaign-routine-bluesky",
                "hpv-campaign-routine-default", "hpv-campaign-default_transition_hpv_1d",
                "hpv-campaign-routine-default_transition_hpv_1d",
                "hpv-campaign-ia2030", "hpv-campaign-routine-ia2030")

stoner::stone_stochastic_standardise(
  group = "Harvard-Sweet",
  in_path = file.path(base_in_path, "HPV-Harvard-Kim"),
  out_path = file.path(base_out_path, "HPV_Harvard-Kim"),
  scenarios = scenarios,
  files = c("stochastic-burden-est.novacc_run_:index Allison Portnoy.csv.xz",
            "stochastic-burden-est.coverage_202310gavi-4_hpv-campaign-default_run_:index Allison Portnoy.csv.xz",
            "stochastic-burden-est.coverage_202310gavi-4_hpv-campaign-bluesky_run_:index Allison Portnoy.csv.xz",
            "stochastic-burden-est.coverage_202310gavi-4_hpv-campaign-routine-bluesky_run_:index Allison Portnoy.csv.xz",
            "stochastic-burden-est.coverage_202310gavi-4_hpv-campaign-routine-default_run_:index Allison Portnoy.csv.xz",
            "stochastic-burden-est.coverage_202310gavi-4_hpv-campaign-default_transition_hpv_1d_run_:index Allison Portnoy.csv.xz",
            "stochastic-burden-est.coverage_202310gavi-4_hpv-campaign-routine-default_transition_hpv_1d_run_:index Allison Portnoy.csv.xz",
            "stochastic-burden-est.coverage_202310gavi-4_hpv-campaign-ia2030_run_:index Allison Portnoy.csv.xz",
            "stochastic-burden-est.coverage_202310gavi-4_hpv-campaign-routine-ia2030_run_:index Allison Portnoy.csv.xz"),
  index = 1:200)

stoner::stone_stochastic_standardise(
  group = "LSHTM-Jit",
  in_path = file.path(base_in_path, "HPV-LSHTM-Jit"),
  out_path = file.path(base_out_path, "HPV_LSHTM-Jit"),
  scenarios =  scenarios,
  files = c("stochastic-burden-novaccination_all_202310gavi-7_hpv-no-vaccination Kaja Abbas.csv.xz",
            "stochastic-burden-vaccination_all_202310gavi-7_hpv-campaign-default Kaja Abbas.csv.xz",
            "stochastic-burden-vaccination_all_202310gavi-7_hpv-campaign-bluesky Kaja Abbas.csv.xz",
            "stochastic-burden-vaccination_all_202310gavi-7_hpv-campaign-routine-bluesky Kaja Abbas.csv.xz",
            "stochastic-burden-vaccination_all_202310gavi-7_hpv-campaign-routine-default Kaja Abbas.csv.xz",
            "stochastic-burden-vaccination_all_202310gavi-7_hpv-campaign-default_transition_hpv_1d Kaja Abbas.csv.xz",
            "stochastic-burden-vaccination_all_202310gavi-7_hpv-campaign-routine-default_transition_hpv_1d Kaja Abbas.csv.xz",
            "stochastic-burden-vaccination_all_202310gavi-7_hpv-campaign-ia2030 Kaja Abbas.csv.xz",
            "stochastic-burden-vaccination_all_202310gavi-7_hpv-campaign-routine-ia2030 Kaja Abbas.csv.xz"))


###############
# Measles

 scenarios = c("measles-no-vaccination", "measles-mcv1-bluesky", "measles-mcv1-default",
               "measles-mcv1-ia2030", "measles-mcv1-mcv2-bluesky", "measles-mcv1-mcv2-campaign-bluesky",
               "measles-mcv1-mcv2-campaign-default", "measles-mcv1-mcv2-campaign-default_under5sia",
               "measles-mcv1-mcv2-campaign-default_update", "measles-mcv1-mcv2-campaign-ia2030",
               "measles-mcv1-mcv2-default", "measles-mcv1-mcv2-ia2030")

stoner::stone_stochastic_standardise(
  group = "PSU-Ferrari",
  in_path = file.path(base_in_path, "Measles-PSU-Ferrari"),
  out_path = file.path(base_out_path, "Measles_PSU-Ferrari"),
  scenarios = scenarios,
  files = c("no-vaccination.202310gavi-6.Measles_PSU-Ferrari_standard.csv.xz",
            "mcv1-bluesky.202310gavi-6.Measles_PSU-Ferrari_standard.csv.xz",
            "mcv1-default.202310gavi-6.Measles_PSU-Ferrari_standard.csv.xz",
            "mcv1-ia2030.202310gavi-6.Measles_PSU-Ferrari_standard.csv.xz",
            "mcv1-mcv2-bluesky.202310gavi-6.Measles_PSU-Ferrari_standard.csv.xz",
            "mcv1-mcv2-campaign-bluesky.202310gavi-6.Measles_PSU-Ferrari_standard.csv.xz",
            "mcv1-mcv2-campaign-default.202310gavi-6.Measles_PSU-Ferrari_standard.csv.xz",
            "measles-mcv1-mcv2-campaign-default_under5sia_PSU_Ferrari_Stochastic_Runs_Revised_05102025.csv.xz",
            "measles-mcv1-mcv2-campaign-default_update_PSU_Ferrari_Stochastic_Runs_Revised_05102025.csv.xz",
            "mcv1-mcv2-campaign-ia2030.202310gavi-6.Measles_PSU-Ferrari_standard.csv.xz",
            "mcv1-mcv2-default.202310gavi-6.Measles_PSU-Ferrari_standard.csv.xz",
            "mcv1-mcv2-ia2030.202310gavi-6.Measles_PSU-Ferrari_standard.csv.xz"))

stoner::stone_stochastic_standardise(
  group = "LSHTM-Jit",
  in_path = file.path(base_in_path, "Measles-LSHTM-Jit"),
  out_path = file.path(base_out_path, "Measles_LSHTM-Jit"),
  scenarios = scenarios,
  files = c("stochastic_burden_estimate_measles-LSHTM-Jit-no-vaccination Han Fu.csv.xz",
            "stochastic_burden_estimate_measles-LSHTM-Jit-mcv1-bluesky Han Fu.csv.xz",
            "stochastic_burden_estimate_measles-LSHTM-Jit-mcv1-default Han Fu.csv.xz",
            "stochastic_burden_estimate_measles-LSHTM-Jit-mcv1-ia2030 Han Fu.csv.xz",
            "stochastic_burden_estimate_measles-LSHTM-Jit-mcv1-mcv2-bluesky Han Fu.csv.xz",
            "stochastic_burden_estimate_measles-LSHTM-Jit-mcv1-mcv2-campaign-bluesky Han Fu.csv.xz",
            "stochastic_burden_estimate_measles-LSHTM-Jit-mcv1-mcv2-campaign-default.csv.xz",
            "stochastic_burden_estimate_measles-LSHTM-Jit-mcv1-mcv2-campaign-default_under5sia.csv.xz",
            "stochastic_burden_estimate_measles-LSHTM-Jit-mcv1-mcv2-campaign-default_update.csv.xz",
            "stochastic_burden_estimate_measles-LSHTM-Jit-mcv1-mcv2-campaign-ia2030 Han Fu.csv.xz",
            "stochastic_burden_estimate_measles-LSHTM-Jit-mcv1-mcv2-default Han Fu.csv.xz",
            "stochastic_burden_estimate_measles-LSHTM-Jit-mcv1-mcv2-ia2030 Han Fu.csv.xz"))


###############
# MenA

 scenarios = c("mena-no-vaccination", "mena-campaign-default", "mena-campaign-routine-default",
                "mena-campaign-bluesky", "mena-campaign-routine-bluesky",
                "mena-campaign-ia2030", "mena-campaign-routine-ia2030")

# NB campaign-default same as campaign-ia2030
stoner::stone_stochastic_standardise(
  group = "Cambridge-Trotter",
  in_path = file.path(base_in_path, "MenA-Cambridge-Trotter"),
  out_path = file.path(base_out_path, "MenA_Cambridge-Trotter"),
  scenarios = scenarios,
  files = c(
    "stochastic-burden-template202310gavi-7MenA_Cambridge-Trotter_mena-novaccination (:index) Andromachi Karachaliou.csv.xz",
    "stochastic-burden-template202310gavi-7MenA_Cambridge-Trotter_mena-campaign-default (:index) Andromachi Karachaliou.csv.xz",
    "stochastic-burden-template202310gavi-7MenA_Cambridge-Trotter_mena-routine-default (:index) Andromachi Karachaliou.csv.xz",
    "stochastic-burden-template202310gavi-7MenA_Cambridge-Trotter_mena-campaign-bluesky (:index) Andromachi Karachaliou.csv.xz",
    "stochastic-burden-template202310gavi-7MenA_Cambridge-Trotter_mena-routine-bluesky (:index) Andromachi Karachaliou.csv.xz",
    "stochastic-burden-template202310gavi-7MenA_Cambridge-Trotter_mena-campaign-default (:index) Andromachi Karachaliou.csv.xz",
    "stochastic-burden-template202310gavi-7MenA_Cambridge-Trotter_mena-routine-ia2030 (:index) Andromachi Karachaliou.csv.xz"),
  index = 1:26)


###############
# Rubella

  scenarios = c("rubella-no-vaccination", "rubella-campaign-default",
                "rubella-campaign-bluesky", "rubella-campaign-rcv1-rcv2-bluesky",
                "rubella-campaign-rcv1-bluesky", "rubella-campaign-rcv1-rcv2-default",
                "rubella-campaign-rcv1-default", "rubella-campaign-ia2030",
                "rubella-campaign-rcv1-rcv2-ia2030", "rubella-campaign-rcv1-ia2030")

stoner::stone_stochastic_standardise(
  group = "UGA-Winter",
  in_path = file.path(base_in_path, "Rubella-UGA"),
  out_path = file.path(base_out_path, "Rubella_UGA-Winter"),
  scenarios = scenarios,
  files = c("stochastic_burden_est-rubella-no-vaccination_:index Amy Winter.csv.xz",
            "stochastic_burden_est-rubella-campaign-default_:index Amy Winter.csv.xz",
            "stochastic_burden_est-rubella-campaign-bluesky_:index Amy Winter.csv.xz",
            "stochastic_burden_est-rubella-campaign-rcv1-rcv2-bluesky_:index Amy Winter.csv.xz",
            "stochastic_burden_est-rubella-campaign-rcv1-bluesky_:index Amy Winter.csv.xz",
            "stochastic_burden_est-rubella-campaign-rcv1-rcv2-default_:index Amy Winter.csv.xz",
            "stochastic_burden_est-rubella-campaign-rcv1-default_:index Amy Winter.csv.xz",
            "stochastic_burden_est-rubella-campaign-ia2030_:index Amy Winter.csv.xz",
            "stochastic_burden_est-rubella-campaign-rcv1-rcv2-ia2030_:index Amy Winter.csv.xz",
            "stochastic_burden_est-rubella-campaign-rcv1-ia2030_:index Amy Winter.csv.xz"),
  index = 1:12)

stoner::stone_stochastic_standardise(
  group = "UKHSA-Vynnycky",
  in_path = file.path(base_in_path, "Rubella-UKHSA-Vynnycky"),
  out_path = file.path(base_out_path, "Rubella_UKHSA-Vynnycky"),
  scenarios = scenarios,
  files = c("imp.c:indexs401.csv.xz",
            "stochastic_burden_est_rubella_Vynnycky_rubella-campaign-default_:index.csv.xz",
            "stochastic_burden_est_rubella_Vynnycky_rubella-campaign-bluesky_:index.csv.xz",
            "Vynnycky-camp-rcv1-rcv2-bluesky_country:index.csv.xz",
            "Vynnycky-camp-rcv1-bluesky_country:index.csv.xz",
            "stochastic_burden_est_rubella_Vynnycky_rubella-campaign-rcv1-rcv2-default_:index.csv.xz",
            "stochastic_burden_est_rubella_Vynnycky_rubella-campaign-rcv1-default_:index.csv.xz",
            "stochastic_burden_est_rubella_Vynnycky_rubella-campaign-ia2030_:index.csv.xz",
            "Vynnycky-camp-rcv1-rcv2-ia2030_country:index.csv.xz",
            "Vynnycky-camp-rcv1-ia2030_country:index.csv.xz"),
  index = 1:117)


###############
# Typhoid

 scenarios = c("typhoid-no-vaccination",
                "typhoid-campaign-default", "typhoid-campaign-routine-default",
                "typhoid-campaign-bluesky",  "typhoid-campaign-routine-bluesky")

stoner::stone_stochastic_standardise(
  group = "IVI-Kim",
  in_path = file.path(base_in_path, "Typhoid-IVI-Kim"),
  out_path = file.path(base_out_path, "Typhoid_IVI-Kim"),
  scenarios = scenarios,
  file = c("stoch_Typhoid_novacc_20240314T233526 Jong-Hoon Kim.csv.xz",
           "stoch_Typhoid_campaign_default_20240314T233526 Jong-Hoon Kim.csv.xz",
           "stoch_Typhoid_campaign_routine_default_20240314T233526 Jong-Hoon Kim.csv.xz",
           "stoch_Typhoid_campaign_bluesky_20240314T233526 Jong-Hoon Kim.csv.xz",
           "stoch_Typhoid_campaign_routine_bluesky_20240314T233526 Jong-Hoon Kim.csv.xz"))

stoner::stone_stochastic_standardise(
  group = "Yale-Pitzer",
  in_path = file.path(base_in_path, "Typhoid-Yale-Pitzer"),
  out_path = file.path(base_out_path, "Typhoid_Yale-Pitzer"),
  scenarios = scenarios,
  files = ":scenario_stochastic_output2023_Yale Virginia Pitzer.csv.xz")


###############
# YF

scenarios = c("yf-no-vaccination", "yf-routine-bluesky",
              "yf-routine-campaign-bluesky", "yf-routine-campaign-default",
              "yf-routine-campaign-ia2030", "yf-routine-default",
              "yf-routine-ia2030")

stoner::stone_stochastic_standardise(
  group = "IC-Garske",
  in_path = file.path(base_in_path, "YF-IC-Garske"),
  out_path = file.path(base_out_path, "YF_IC-Garske"),
  scenarios = scenarios,
  files = "burden_results_stochastic_202310gavi-3_:scenario Keith Fraser.csv.xz")

stoner::stone_stochastic_standardise(
  group = "UND-Perkins",
  in_path = file.path(base_in_path, "YF-UND-Perkins"),
  out_path = file.path(base_out_path, "YF_UND-Perkins"),
  scenarios = scenarios,
  files = c("stochastic_burden_est_YF_UND-Perkins_yf-no_vaccination_:index.csv.xz",
            "stochastic_burden_est_YF_UND-Perkins_yf-routine_bluesky_:index.csv.xz",
            "stochastic_burden_est_YF_UND-Perkins_yf-routine_campaign_bluesky_:index.csv.xz",
            "stochastic_burden_est_YF_UND-Perkins_yf-routine_campaign_default_:index.csv.xz",
            "stochastic_burden_est_YF_UND-Perkins_yf-routine_campaign_ia2030_:index.csv.xz",
            "stochastic_burden_est_YF_UND-Perkins_yf-routine_default_:index.csv.xz",
            "stochastic_burden_est_YF_UND-Perkins_yf-routine_ia2030_:index.csv.xz"),
  index = 1:200)


###############
# Malaria R2

scenarios = c("malaria-no-vaccination",
              "malaria-rts3-default", "malaria-rts3-rts4-default",
              "malaria-rts3-bluesky", "malaria-rts3-rts4-bluesky"),

stoner::stone_stochastic_standardise(
  group = "UAC-Kakai",
  in_path = file.path(base_in_path, "Malaria-UAC-Kakai", "round2"),
  out_path = file.path(base_out_path, "Malaria_UAC-Kakai"),
  scenarios = scenarios,
  files = c("Stochastic_Burden_Estimates_Glele_Kakai_No_Vaccine_:index.csv.xz",
            "Stochastic_Burden_Estimates_Glele_Kakai_Default_rts3_:index.csv.xz",
            "Stochastic_Burden_Estimates_Glele_Kakai_Default_rts3_4_:index.csv.xz",
            "stochastic_Burden_Estimates_Glele_Kakai_Blue_Sky_rts3_:index.csv.xz",
            "Stochastic_Burden_Estimates_Glele_Kakai_Blue_Sky_rts34_:index.csv.xz"),
  index = 1:200)

stoner::stone_stochastic_standardise(
  group = "IC-Okell",
  in_path = file.path(base_in_path, "Malaria-IC_Okell"),
  out_path = file.path(base_out_path, "Malaria_IC-Okell"),
  scenarios = scenarios,
  files = c("stochastic-burden-est.202310gavi-7.Malaria_IC-Okell_no-vaccination_draw_:index Lydia Haile.csv.xz",
            "stochastic-burden-est.202310gavi-7.Malaria_IC-Okell_malaria-rts3-default_draw_:index Lydia Haile.csv.xz",
            "stochastic-burden-est.202310gavi-7.Malaria_IC-Okell_malaria-rts3-rts4-default_draw_:index Lydia Haile.csv.xz",
            "stochastic-burden-est.202310gavi-7.Malaria_IC-Okell_malaria-rts3-bluesky_draw_:index Lydia Haile.csv.xz",
            "stochastic-burden-est.202310gavi-7.Malaria_IC-Okell_malaria-rts3-rts4-bluesky_draw_:index Lydia Haile.csv.xz"),
  index = 1:200)

stoner::stone_stochastic_standardise(
  group = "TKI-Penny",
  in_path = file.path(base_in_path, "Malaria-TKI-Penny"),
  out_path = file.path(base_out_path, "Malaria_TKI-Penny"),
  scenarios = scenarios,
  files =  c("stochastic_burden_est_malaria_:index_novaccine Josephine Malinga.csv.xz",
             "stochastic_burden_est_malaria_:index_rtss_d3_default Josephine Malinga.csv.xz",
             "stochastic_burden_est_malaria_:index_rtss_d4_default Josephine Malinga.csv.xz",
             "stochastic_burden_est_malaria_:index_rtss_d3_bluesky Josephine Malinga.csv.xz",
             "stochastic_burden_est_malaria_:index_rtss_d4_bluesky Josephine Malinga.csv.xz"),
  index = 1:31)
