base_in_path <- "//wpia-hn2.hpc.dide.ic.ac.uk/vimc_stochastics_dropbox/latest/202110gavi"
base_out_path <- "//wpia-hn2.hpc.dide.ic.ac.uk/vimc_stochastics/202110gavi"

vault <- vaultr::vault_client(login = "github")
password <- vault$read("/secret/vimc/database/production/users/readonly")$password
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "montagu",
                      host = "montagu.vaccineimpact.org",
                      port = 5432, password = password,
                      user = "readonly")

fetch_scenarios <- function(disease) {
  sort(DBI::dbGetQuery(con, "
    SELECT scenario_description FROM
           scenario JOIN scenario_description
           ON scenario.scenario_description = scenario_description.id
           WHERE disease = $1 AND touchstone='202110gavi-3'", disease)$scenario_description)
}

# Let's unleash the cow

setwd("Q:/testcow")
writeLines("vimc/stoner@VIMC-9230", "pkgdepends.txt")
hipercow::hipercow_init(driver = "dide-windows")
hipercow::hipercow_provision()
# Network/memory might be too much for more than a job per node.
hres <- hipercow::hipercow_resources(cores = 1L, exclusive = TRUE)

###############
# Cholera

scenarios = c("cholera-no-vaccination", "cholera-campaign-default")

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "IVI-Kim",
  in_path = file.path(base_in_path, "IVI-Kim-Cholera"),
  out_path = file.path(base_out_path, "Cholera_IVI-Kim"),
  scenarios = scenarios,
  files = c("Jong-Hoon Kim - stoch_Cholera_campaign_20211221T00.csv.xz",
            "Jong-Hoon Kim - stoch_Cholera_campaign_20211222T212131.csv.xz")
  ))

stoner::stone_stochastic_standardise(
  group = "JHU-Lee",
  in_path = file.path(base_in_path, "JHU-Lee-Cholera"),
  out_path = file.path(base_out_path, "Cholera_JHU-Lee"),
  scenarios = scenarios,
  files = c("Kaiyue Zou - no-vaccination.csv.xz",
            "Kaiyue Zou - campaign-default.csv.xz"))

###############
# HepB

scenarios <- c("hepb-no-vaccination",
               "hepb-bd-routine-default",
               "hepb-bd-routine-ia2030_target",
               "hepb-bd-routine-ia2030_target-hepb-routine-ia2030_target",
               "hepb-bd-default-hepb-routine-default",
               "hepb-hepb-routine-default",
               "hepb-hepb-routine-ia2030_target")

hipercow::task_create_expr(resources = hres, expr =
  stoner::stone_stochastic_standardise(
    group = "Li",
    in_path = file.path(base_in_path, "Li"),
    out_path = file.path(base_out_path, "HepB_Li"),
    scenarios = scenarios,
    files = ":scenario:index.csv.xz",
    index = 1:200))

hipercow::task_create_expr(resources = hres, expr =
  stoner::stone_stochastic_standardise(
    group = "IC-Hallett",
    in_path = file.path(base_in_path, "IC-Hallett"),
    out_path = file.path(base_out_path, "HepB_IC-Hallett"),
    scenarios = scenarios,
    files = "stochastic_burden_est_HepB-IC-Hallett_:scenario_:index.csv.xz",
    index = 1:200))

###############
# HIB

scenarios <- c("hib-no-vaccination", "hib-routine-default",
               "hib-routine-ia2030_target")

stoner::stone_stochastic_standardise(
  group = "JHU-Tam",
  in_path = file.path(base_in_path, "JHU-Tam-Carter-Hib"),
  out_path = file.path(base_out_path, "Hib_JHU-Tam"),
  scenarios = scenarios,
  files = c("hib-no-vaccination-LiST.csv.xz",
            "hib-routine-default-LiST.csv.xz",
            "hib-routine-ia2030_target-LiST.csv.xz"))

stoner::stone_stochastic_standardise(
  group = "LSHTM-Clark",
  in_path = file.path(base_in_path, "LSHTM-Clark_Hib"),
  out_path = file.path(base_out_path, "Hib_LSHTM-Clark"),
  scenarios = scenarios,
  files = c("Kaja Abbas - PSA_202110gavi-3_hib-no-vaccination.csv.xz",
            "Kaja Abbas - PSA_202110gavi-3_hib-routine-default.csv.xz",
            "Kaja Abbas - PSA_202110gavi-3_hib-routine-ia2030_target.csv.xz"))

###############
# HPV

scenarios <- c("hpv-no-vaccination", "hpv-campaign-default",
               "hpv-routine-default","hpv-campaign-ia2030_target",
               "hpv-routine-ia2030_target")

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "Harvard-Sweet",
  in_path = file.path(base_in_path, "Harvard-Sweet"),
  out_path = file.path(base_out_path, "HPV_Harvard-Sweet"),
  scenarios = scenarios,
  files = c("Allison Portnoy - stochastic-burden-est.novacc_run_200.csv.xz",
            "Allison Portnoy - stochastic-burden-est.coverage_202110gavi-3_hpv-campaign-default_run_:index.csv.xz",
            "Allison Portnoy - stochastic-burden-est.coverage_202110gavi-3_hpv-routine-default_run_:index.csv.xz",
            "Allison Portnoy - stochastic-burden-est.coverage_202110gavi-3_hpv-campaign-ia2030_target_run_:index.csv.xz",
            "Allison Portnoy - stochastic-burden-est.coverage_202110gavi-3_hpv-routine-ia2030_target_run_:index.csv.xz"),
  index = 1:200))

id <- hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "LSHTM-Jit",
  in_path = file.path(base_in_path, "LSHTM-Jit_HPV"),
  out_path = file.path(base_out_path, "HPV_LSHTM-Jit"),
  scenarios =  scenarios,
  files =
    c("stochastic-burden-novaccination_all_202110gavi-3_hpv-no-vaccination.csv.xz",
      "stochastic-burden-vaccination_all_202110gavi-3_hpv-campaign-default.csv.xz",
      "stochastic-burden-vaccination_all_202110gavi-3_hpv-routine-default.csv.xz",
      "stochastic-burden-vaccination_all_202110gavi-3_hpv-campaign-ia2030_target.csv.xz",
      "stochastic-burden-vaccination_all_202110gavi-3_hpv-routine-ia2030_target.csv.xz")))

###############
# JE

scenarios <- c("je-routine-no-vaccination", "je-campaign-default",
               "je-campaign-ia2030_target", "je-routine-default",
               "je-routine-ia2030_target")

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "NUS-Clapham",
  in_path = file.path(base_in_path, "NUS-Clapham-JE"),
  out_path = file.path(base_out_path, "JE_NUS-Clapham"),
  scenarios =  scenarios,
  files = c(
    "Naive_Stochastic__:index.csv.xz",
    "Campaign_Stochastic_:index.csv.xz",
    "Campaign_Target_Stochastic_:index.csv.xz",
    "Routine_Stochastic_:index.csv.xz",
    "Routine_Target_Stochastic_:index.csv.xz"),
  index = 1:200))

stub <- "Sean Moore - stochastic_burden_est_JE_UND-Moore"

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "UND-Moore",
  in_path = file.path(base_in_path, "UND-Moore-JE"),
  out_path = file.path(base_out_path, "JE_UND-Moore"),
  scenarios =  scenarios,
  files = sprintf("%s_:scenario.csv.xz", stub)))


###############
# Measles
scenarios <- c("measles-no-vaccination",
               "measles-campaign-default",
               "measles-campaign-ia2030_target",
               "measles-campaign-only-default",
               "measles-campaign-only-ia2030_target",
               "measles-mcv1-default",
               "measles-mcv1-ia2030_target",
               "measles-mcv2-default",
               "measles-mcv2-ia2030_target")

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "PSU-Ferrari",
  in_path = file.path(base_in_path, "PSU-Ferrari-Measles"),
  out_path = file.path(base_out_path, "Measles_PSU-Ferrari"),
  scenarios = scenarios,
  files = "coverage_202110gavi-3_:scenario.csv.xz"))

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "LSHTM-Jit",
  in_path = file.path(base_in_path, "LSHTM-Jit_Measles"),
  out_path = file.path(base_out_path, "Measles_LSHTM-Jit"),
  scenarios = scenarios,
  files = c("Han Fu - stochastic_burden_estimate_measles-LSHTM-Jit-no-vaccination.csv.xz",
            "Han Fu - stochastic_burden_estimate_measles-LSHTM-Jit-campaign-default.csv.xz",
            "Han Fu - stochastic_burden_estimate_measles-LSHTM-Jit-campaign-ia2030_target.csv.xz",
            "Han Fu - stochastic_burden_estimate_measles-LSHTM-Jit-campaign-only-default.csv.xz",
            "Han Fu - stochastic_burden_estimate_measles-LSHTM-Jit-campaign-only-ia2030_target.csv.xz",
            "Han Fu - stochastic_burden_estimate_measles-LSHTM-Jit-mcv1-default.csv.xz",
            "Han Fu - stochastic_burden_estimate_measles-LSHTM-Jit-mcv1-ia2030_target.csv.xz",
            "Han Fu - stochastic_burden_estimate_measles-LSHTM-Jit-mcv2-default.csv.xz",
            "Han Fu - stochastic_burden_estimate_measles-LSHTM-Jit-mcv2-ia2030_target.csv.xz"
            )))

###############
# MenA

scenarios = c("mena-no-vaccination", "mena-campaign-default",
              "mena-campaign-ia2030_target", "mena-routine-default",
              "mena-routine-ia2030_target", "mena-booster-default")
stub <- "Andromachi Karachaliou - stochastic-burden.202110gavi-2.MenA_Cambridge-Trotter"

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "Cambridge-Trotter",
  in_path = file.path(base_in_path, "Cambridge-Trotter"),
  out_path = file.path(base_out_path, "MenA_Cambridge-Trotter"),
  scenarios = scenarios,
  files = c(sprintf("%s_no_vaccination_:index.csv.xz", stub),
            sprintf("%s_campaign_default_:index.csv.xz", stub),
            sprintf("%s_campaign-ia2030_target_:index.csv.xz", stub),
            sprintf("%s_routine_default_:index.csv.xz", stub),
            sprintf("%s_routine-ia2030_target_:index.csv.xz", stub),
            sprintf("%s_booster_:index.csv.xz", stub)),
  index = 1:26))

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "KPW-Jackson",
  in_path = file.path(base_in_path, "KPW-Jackson-MenA"),
  out_path = file.path(base_out_path, "MenA_KPW-Jackson"),
  scenarios = scenarios,
  files = c("stochastic_burden_est_MenA_KPWA_none_default_:index.csv.xz",
            "stochastic_burden_est_MenA_KPWA_campaign_default_:index.csv.xz",
            "stochastic_burden_est_MenA_KPWA_campaign_ia2030_target_:index.csv.xz",
            "stochastic_burden_est_MenA_KPWA_routine_default_:index.csv.xz",
            "stochastic_burden_est_MenA_KPWA_routine_ia2030_target_:index.csv.xz",
            "stochastic_burden_est_MenA_KPWA_booster_default_:index.csv.xz"),
  index = 1:26))

###############
# PCV

scenarios <- c("pcv-no-vaccination", "pcv-routine-default",
               "pcv-routine-ia2030_target")

stoner::stone_stochastic_standardise(
  group = "JHU-Tam",
  in_path = file.path(base_in_path, "JHU-Tam-Carter-PCV"),
  out_path = file.path(base_out_path, "PCV_JHU-Tam"),
  scenarios = scenarios,
  files = ":scenario-LiST.csv.xz")

stoner::stone_stochastic_standardise(
  group = "NUS-Chen",
  in_path = file.path(base_in_path, "LSHTM-NUS-Chen_PCV"),
  out_path = file.path(base_out_path, "PCV_NUS-Chen"),
  scenarios = scenarios,
  files = "stochastic_burden_est_:scenario.csv.xz")

###############
# Rota

scenarios <- c("rota-no-vaccination", "rota-routine-default",
               "rota-routine-ia2030_target")

stoner::stone_stochastic_standardise(
  group = "JHU-Tam",
  in_path = file.path(base_in_path, "JHU-Tam-Carter-Rota"),
  out_path = file.path(base_out_path, "Rota_JHU-Tam"),
  scenarios = scenarios,
  files = ":scenario-LiST.csv.xz")

stoner::stone_stochastic_standardise(
  group = "LSHTM-Clark",
  in_path = file.path(base_in_path, "LSHTM-Clark_Rota"),
  out_path = file.path(base_out_path, "Rota_LSHTM-Clark"),
  scenarios = scenarios,
  files = "Kaja Abbas - PSA_202110gavi-3_:scenario.csv.xz")

stoner::stone_stochastic_standardise(
  group = "Emory-Lopman",
  in_path = file.path(base_in_path, "Emory-Lopman"),
  out_path = file.path(base_out_path, "Rota_Emory-Lopman"),
  scenarios = scenarios,
  files = c("Aniruddha Deshpande - stochastic_burden_est_lopman_no_vaccination_2022_01_31.csv.xz",
            "Aniruddha Deshpande - stochastic_burden_est_lopman_routine_2022_01_31.csv.xz",
            "Aniruddha Deshpande - stochastic_burden_est_lopman_ia2030_target_2022_01_31.csv.xz"))

###############
# Rubella

scenarios <- c("rubella-routine-no-vaccination",
  "rubella-campaign-default", "rubella-campaign-ia2030_target",
  "rubella-rcv1-default", "rubella-rcv1-ia2030_target",
  "rubella-rcv1-rcv2-default", "rubella-rcv1-rcv2-ia2030_target",
  "rubella-rcv2-default", "rubella-rcv2-ia2030_target")

id10 <- hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "JHU-Lessler",
  in_path = file.path(base_in_path, "JHU-UGA-Winter-Rubella"),
  out_path = file.path(base_out_path, "Rubella_JHU-Lessler"),
  scenarios = scenarios,
  files = "Amy Winter - stochastic_burden_est-:scenario_:index.csv.xz",
  index = 1:11))

id11<-hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "PHE-Vynnycky",
  in_path = file.path(base_in_path, "PHE-Vynnycky_Rubella"),
  out_path = file.path(base_out_path, "Rubella_PHE-Vynnycky"),
  scenarios = scenarios,
  files = c("VIMC_NV_RCV1RCV2Camp_country:index.csv.xz",
            "VIMC_DF_Camp_country:index.csv.xz",
            "VIMC_IA_Camp_country:index.csv.xz",
            "VIMC_DF_RCV1Camp_country:index.csv.xz",
            "VIMC_IA_RCV1Camp_country:index.csv.xz",
            "VIMC_DF_RCV1RCV2_country:index.csv.xz",
            "VIMC_IA_RCV1RCV2_country:index.csv.xz",
            "VIMC_DF_RCV1RCV2Camp_country:index.csv.xz",
            "VIMC_IA_RCV1RCV2Camp_country:index.csv.xz"),
  index = 1:112))



###############
# Typhoid

scenarios = c("typhoid-no-vaccination",
              "typhoid-campaign-default",
              "typhoid-campaign-ia2030_target",
              "typhoid-routine-default",
              "typhoid-routine-ia2030_target")

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "IVI-Kim",
  in_path = file.path(base_in_path, "IVI-Kim-Typhoid"),
  out_path = file.path(base_out_path, "Typhoid_IVI-Kim"),
  scenarios = scenarios,
  files = c("Jong-Hoon Kim - stoch_Typhoid_novacc_20220509T233408.csv.xz",
            "Jong-Hoon Kim - stoch_Typhoid_campaign-default_20220510T103619.csv.xz",
            "Jong-Hoon Kim - stoch_Typhoid_campaign-ia2030_20220510T105521.csv.xz",
            "Jong-Hoon Kim - stoch_Typhoid_routine-default_20220510T103357.csv.xz",
            "Jong-Hoon Kim - stoch_Typhoid_routine-ia2030_20220514.csv.xz")))

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "Yale-Pitzer",
  in_path = file.path(base_in_path, "Yale-Pitzer-Typhoid"),
  out_path = file.path(base_out_path, "Typhoid_Yale-Pitzer"),
  scenarios = scenarios,
  files = c("Holly Burrows - stochastic_burden_est_TF-Yale-Burrows-novacc_202110.csv.xz",
            "Holly Burrows - stochastic_burden_est_TF-Yale-Burrows_campaign-default_202110.csv.xz",
            "Holly Burrows - stochastic_burden_est_TF-Yale-Burrows_campaign-IA2030_202110.csv.xz",
            "Holly Burrows - stochastic_burden_est_TF-Yale-Burrows_routine-default_202110.csv.xz",
            "Holly Burrows - stochastic_burden_est_TF-Yale-Burrows_routine-IA2030_202110.csv.xz")))



###############
# YF

scenarios <- c("yf-no-vaccination", "yf-preventive-ia2030_target",
               "yf-preventive-default", "yf-routine-ia2030_target",
               "yf-routine-default")

stub <- "Keith Fraser - stochastic-burden-estimates.202110gavi-3_YF_IC-Garske"
stoner::stone_stochastic_standardise(
  group = "IC-Garske",
  in_path = file.path(base_in_path, "IC-Garske"),
  out_path = file.path(base_out_path, "YF_IC-Garske"),
  scenarios = scenarios,
  files = sprintf("%s_:scenario_:index.csv.xz", stub),
  index = 1:200)

stoner::stone_stochastic_standardise(
  group = "UND-Perkins",
  in_path = file.path(base_in_path, "UND-Perkins-YF"),
  out_path = file.path(base_out_path, "YF_UND-Perkins"),
  scenarios = scenarios,
  files = "stochastic_burden_est_YF_UND-Perkins_:scenario_:index.csv.xz",
  index = 1:200)
