base_in_path <- "//wpia-hn2.hpc.dide.ic.ac.uk/vimc_stochastics_dropbox/latest/201910gavi"
base_out_path <- "//wpia-hn2.hpc.dide.ic.ac.uk/vimc_stochastics/201910gavi"

# In case useful for looking up scenarios

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
           WHERE disease = $1 AND touchstone='201910gavi-5'", disease)$scenario_description)
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

stoner::stone_stochastic_standardise(
  group = "IVI-Kim",
  in_path = file.path(base_in_path, "IVI-Kim-Cholera"),
  out_path = file.path(base_out_path, "Cholera_IVI-Kim"),
  scenarios = scenarios,
  files = c("Jong-Hoon Kim - stoch_output_Cholera_novacc_20210902.csv.xz",
            "Jong-Hoon Kim - stoch_output_Cholera_campaign_20210902.csv.xz")
  )

stoner::stone_stochastic_standardise(
  group = "JHU-Lee",
  in_path = file.path(base_in_path, "JHU-Lee"),
  out_path = file.path(base_out_path, "Cholera_JHU-Lee"),
  scenarios = scenarios,
  files = c("Kaiyue Zou - stochastic-burden-template.201910gavi-5.Cholera_no-vaccination.csv.xz",
            "Kaiyue Zou - stochastic-burden-template.201910gavi-5.Cholera_campaign-default.csv.xz")
)

###############
# HepB

scenarios <- c("hepb-no-vaccination",
               "hepb-bd-default-hepb-routine-default",
               "hepb-bd-routine-bestcase-hepb-routine-bestcase",
               "hepb-bd-routine-bestcase",
               "hepb-bd-routine-default",
               "hepb-hepb-routine-bestcase",
               "hepb-hepb-routine-default",
               "hepb-stop")

stub <- "Ivane Gamkrelidze - stochastic-burden-template.201910gavi-4.HepB_CDA-Razavi"

stoner::stone_stochastic_standardise(
  group = "CDA-Razavi",
  in_path = file.path(base_in_path, "CDA-Razavi"),
  out_path = file.path(base_out_path, "HepB_CDA-Razavi"),
  scenarios = scenarios,
  files =
    c(sprintf("%s_all_hepb-no-vaccination.csv.xz", stub),
      sprintf("%s_all_hepb-bd-default-hepb-routine-default.csv.xz", stub),
      sprintf("%s_all_hepb-bd-routine-bestcase-hepb-routine-bestcase.csv.xz", stub),
      sprintf("%s_bd_hepb-bd-routine-bestcase.csv.xz", stub),
      sprintf("%s_bd_hepb-bd-routine-default.csv.xz", stub),
      sprintf("%s_non_bd_hepb-hepb-routine-bestcase.csv.xz", stub),
      sprintf("%s_non_bd_hepb-hepb-routine-default.csv.xz", stub),
      sprintf("%s_all_hepb-stop.csv.xz", stub))
)

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
               "hib-routine-bestcase")

stoner::stone_stochastic_standardise(
  group = "JHU-Tam",
  in_path = file.path(base_in_path, "JHU-Tam-Hib"),
  out_path = file.path(base_out_path, "Hib_JHU-Tam"),
  scenarios = scenarios,
  files = c("novac:index.csv.xz", "default:index.csv.xz", "best:index.csv.xz"),
  index = 1:14)

hipercow::task_create_expr(resources = hres, expr =
  stoner::stone_stochastic_standardise(
  group = "LSHTM-Clark",
  in_path = file.path(base_in_path, "LSHTM-Clark_Hib"),
  out_path = file.path(base_out_path, "Hib_LSHTM-Clark"),
  scenarios = scenarios,
  files = c("VIMC_Hib_PSA_NoVax.csv.xz", "VIMC_Hib_PSA_Default.csv.xz",
            "VIMC_Hib_PSA_Best.csv.xz")))

###############
# HPV

scenarios <- c("hpv-no-vaccination", "hpv-routine-bestcase",
               "hpv-campaign-bestcase", "hpv-routine-default",
               "hpv-campaign-default")

stub <- "stochastic-burden-est.201910gavi-5.HPV_Harvard-Sweet"

stoner::stone_stochastic_standardise(
  group = "Harvard-Sweet",
  in_path = file.path(base_in_path, "Harvard-Sweet"),
  out_path = file.path(base_out_path, "HPV_Harvard-Sweet"),
  scenarios = scenarios,
  files = c(sprintf("%s_novacc_run_:index.csv.xz", stub),
            sprintf("%s_routine-bestcase_run_:index.csv.xz", stub),
            sprintf("%s_campaign-bestcase_run_:index.csv.xz", stub),
            sprintf("%s_routine-default_run_:index.csv.xz", stub),
            sprintf("%s_campaign-default_run_:index.csv.xz", stub)),
  index = 1:200)

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "LSHTM-Jit",
  in_path = file.path(base_in_path, "LSHTM-Jit_HPV"),
  out_path = file.path(base_out_path, "HPV_LSHTM-Jit"),
  scenarios =  scenarios,
  files =
    c("stochastic-burden-novaccination_201910gavi-4_hpv-no-vaccination.csv.xz",
      "stochastic-burden-vaccination_201910gavi-4_hpv-campaign-bestcase.csv.xz",
      "stochastic-burden-vaccination_201910gavi-4_hpv-campaign-default.csv.xz",
      "stochastic-burden-vaccination_201910gavi-4_hpv-routine-bestcase.csv.xz",
      "stochastic-burden-vaccination_201910gavi-4_hpv-routine-default.csv.xz")))

###############
# JE

scenarios <- c("je-routine-no-vaccination", "je-campaign-default",
               "je-campaign-bestcase", "je-routine-default",
               "je-routine-bestcase")

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "OUCRU-Clapham",
  in_path = file.path(base_in_path, "OUCRU-Clapham"),
  out_path = file.path(base_out_path, "JE_OUCRU-Clapham"),
  scenarios =  scenarios,
  files = c(
    "Template_Stochastic_Naive4_correcting_:index.csv.xz",
    "Template_Stochastic_Campaign_Default4_correcting_:index.csv.xz",
    "Template_Stochastic_Campaign_Best4_correcting_:index.csv.xz",
    "Template_Stochastic_Routine_Default4_correcting_:index.csv.xz",
    "Template_Stochastic_Routine_Best4_correcting_:index.csv.xz"),
  index = 1:200))

stub <- "Sean Moore - stochastic_burden_est_JE_UND-Moore"

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "UND-Moore",
  in_path = file.path(base_in_path, "UND-Moore"),
  out_path = file.path(base_out_path, "JE_UND-Moore"),
  scenarios =  scenarios,
  files = c(
    sprintf("%s_je-campaign-bestcase.csv.xz", stub),
    sprintf("%s_je-campaign-default.csv.xz", stub),
    sprintf("%s_je-no-vaccination.csv.xz", stub),
    sprintf("%s_je-routine-bestcase.csv.xz", stub),
    sprintf("%s_je-routine-default.csv.xz", stub))))



###############
# Measles
scenarios <- c("measles-no-vaccination", "measles-mcv1-default",
               "measles-mcv2-default", "measles-mcv1-bestcase",
               "measles-mcv2-bestcase", "measles-campaign-default",
               "measles-campaign-only-default", "measles-campaign-bestcase",
               "measles-campaign-only-bestcase", "measles-stop")

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "PSU-Ferrari",
  in_path = file.path(base_in_path, "PSU-Ferrari"),
  out_path = file.path(base_out_path, "Measles_PSU-Ferrari"),
  scenarios = scenarios,
  files = c(
    "Heather Santos - novax_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz",
    "Heather Santos - default_mcv1_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz",
    "Heather Santos - default_mcv2_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz",
    "Heather Santos - bestcase_mcv1_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz",
    "Heather Santos - bestcase_mcv2_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz",
    "Heather Santos - default_campaign_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz",
    "Heather Santos - default_campaign_only_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz",
    "stochastic:index_burden_Measles-PSU-Ferrari.csv.xz",
    "Heather Santos - bestcase_campaign_only_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz",
    "Heather Santos - stop_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz"),
  index = 1:8))

stub <- "stochastic_burden_estimate_measles-LSHTM-Jit"

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "LSHTM-Jit",
  in_path = file.path(base_in_path, "LSHTM-Jit_Measles"),
  out_path = file.path(base_out_path, "Measles_LSHTM-Jit"),
  scenarios = scenarios,
  files = c(sprintf("%s-no-vaccination_Portnoy.csv.xz", stub),
            sprintf("%s-mcv1-default_Portnoy.csv.xz", stub),
            sprintf("%s-mcv2-default_Portnoy.csv.xz", stub),
            sprintf("%s-mcv1-bestcase_Portnoy.csv.xz", stub),
            sprintf("%s-mcv2-bestcase_Portnoy.csv.xz", stub),
            sprintf("%s-campaign-default_Portnoy.csv.xz", stub),
            sprintf("%s-campaign-only-default_Portnoy.csv.xz", stub),
            sprintf("%s-campaign-bestcase_Portnoy.csv.xz", stub),
            sprintf("%s-campaign-only-bestcase_Portnoy.csv.xz", stub),
            sprintf("%s-stop_Portnoy.csv.xz", stub))))

###############
# MenA

scenarios = c("mena-no-vaccination", "mena-campaign-default",
              "mena-campaign-bestcase", "mena-routine-default",
              "mena-routine-bestcase")
stub <- "Andromachi Karachaliou - stochastic-burden.201910gavi-4.MenA_Cambridge-Trotter"

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "Cambridge-Trotter",
  in_path = file.path(base_in_path, "Cambridge-Trotter"),
  out_path = file.path(base_out_path, "MenA_Cambridge-Trotter"),
  scenarios = scenarios,
  files = c(sprintf("%s_no-vaccination_:index.csv.xz", stub),
            sprintf("%s_campaign-default_:index.csv.xz", stub),
            sprintf("%s_campaign-bestcase_:index.csv.xz", stub),
            sprintf("%s_routine-default_:index.csv.xz", stub),
            sprintf("%s_routine-bestcase_:index.csv.xz", stub)),
  index = 1:52))

stub <- "Michael Jackson - stochastic_burden_est_MenA_KPWA"
hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "KPW-Jackson",
  in_path = file.path(base_in_path, "KPW-Jackson"),
  out_path = file.path(base_out_path, "MenA_KPW-Jackson"),
  scenarios = scenarios,
  files = c(sprintf("%s_both_bestcase_:index.csv.xz", stub),
            sprintf("%s_both_default_:index.csv.xz", stub),
            sprintf("%s_campaign_bestcase_:index.csv.xz", stub),
            sprintf("%s_campaign_default_:index.csv.xz", stub),
            sprintf("%s_none_default_:index.csv.xz", stub)),
  index = 1:26))

###############
# PCV

scenarios <- c("pcv-no-vaccination", "pcv-routine-default",
               "pcv-routine-bestcase")

stoner::stone_stochastic_standardise(
  group = "JHU-Tam",
  in_path = file.path(base_in_path, "JHU-Tam-PCV"),
  out_path = file.path(base_out_path, "PCV_JHU-Tam"),
  scenarios = scenarios,
  files = c("novac:index.csv.xz", "default:index.csv.xz", "best:index.csv.xz"),
  index = 1:14)

stoner::stone_stochastic_standardise(
  group = "LSHTM-Clark",
  in_path = file.path(base_in_path, "LSHTM-Clark_PCV"),
  out_path = file.path(base_out_path, "PCV_LSHTM-Clark"),
  scenarios = scenarios,
  files = c("VIMC_Sp_PSA_NoVax.csv.xz", "VIMC_Sp_PSA_Default.csv.xz",
            "VIMC_Sp_PSA_Best.csv.xz"))

###############
# Rota

scenarios <- c("rota-no-vaccination", "rota-routine-default",
               "rota-routine-bestcase")

stoner::stone_stochastic_standardise(
  group = "JHU-Tam",
  in_path = file.path(base_in_path, "JHU-Tam-Rota"),
  out_path = file.path(base_out_path, "Rota_JHU-Tam"),
  scenarios = scenarios,
  files = c("novac:index.csv.xz", "default:index.csv.xz", "best:index.csv.xz"),
  index = 1:14)

stoner::stone_stochastic_standardise(
  group = "LSHTM-Clark",
  in_path = file.path(base_in_path, "LSHTM-Clark_Rota"),
  out_path = file.path(base_out_path, "Rota_LSHTM-Clark"),
  scenarios = scenarios,
  files = c("Hira Tanvir - VIMC_Rota_PSA_NoVax.csv.xz",
            "Hira Tanvir - VIMC_Rota_PSA_Default.csv.xz",
            "Hira Tanvir - VIMC_Rota_PSA_Best.csv.xz"))

stoner::stone_stochastic_standardise(
  group = "Emory-Lopman",
  in_path = file.path(base_in_path, "Emory-Lopman"),
  out_path = file.path(base_out_path, "Rota_Emory-Lopman"),
  scenarios = scenarios,
  files = paste0("Molly Steele - stochastic-burden.201910gavi-4.",
                "Rota_Emory-Lopman_:scenario.csv.xz"))

###############
# Rubella

scenarios <- c(
  "rubella-routine-no-vaccination", "rubella-campaign-default",
  "rubella-campaign-bestcase", "rubella-rcv1-default",
  "rubella-rcv1-bestcase", "rubella-rcv1-rcv2-default",
  "rubella-rcv1-rcv2-bestcase", "rubella-rcv2-default",
  "rubella-rcv2-bestcase", "rubella-stop")

stub <- "Amy Winter - stochastic_burden_est-rubella"

id10 <- hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "JHU-Lessler",
  in_path = file.path(base_in_path, "JHU-Lessler"),
  out_path = file.path(base_out_path, "Rubella_JHU-Lessler"),
  scenarios = scenarios,
  files = c(sprintf("%s-no-vaccination_:index.csv.xz", stub),
            sprintf("%s-campaign-default_:index.csv.xz", stub),
            sprintf("%s-campaign-bestcase_:index.csv.xz", stub),
            sprintf("%s-rcv1-default_:index.csv.xz", stub),
            sprintf("%s-rcv1-bestcase_:index.csv.xz", stub),
            sprintf("%s-rcv1-rcv2-default_:index.csv.xz", stub),
            sprintf("%s-rcv1-rcv2-bestcase_:index.csv.xz", stub),
            sprintf("%s-rcv2-default_:index.csv.xz", stub),
            sprintf("%s-rcv2-bestcase_:index.csv.xz", stub),
            sprintf("%s-stop_:index.csv.xz", stub)),
  index = 1:12))

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "PHE-Vynnycky",
  in_path = file.path(base_in_path, "PHE-Vynnycky"),
  out_path = file.path(base_out_path, "Rubella_PHE-Vynnycky"),
  scenarios = scenarios,
  files = "stochastic_burden_est_:scenario_country:index.csv.xz",
  index = 1:112))


###############
# Typhoid

scenarios = c("typhoid-no-vaccination",
                "typhoid-campaign-default", "typhoid-routine-default")

stoner::stone_stochastic_standardise(
  group = "IVI-Kim",
  in_path = file.path(base_in_path, "IVI-Kim-Typhoid"),
  out_path = file.path(base_out_path, "Typhoid_IVI-Kim"),
  scenarios = scenarios,
  files = c("Jong-Hoon Kim - stoch_Typhoid_novacc.csv.xz",
           "Jong-Hoon Kim - stoch_Typhoid_campaign.csv.xz",
           "Jong-Hoon Kim - stoch_Typhoid_routine.csv.xz"))

stoner::stone_stochastic_standardise(
  group = "Yale-Pitzer",
  in_path = file.path(base_in_path, "Yale-Pitzer"),
  out_path = file.path(base_out_path, "Typhoid_Yale-Pitzer"),
  scenarios = scenarios,
  files = c("Virginia Pitzer - 2021-02-18 17.00.26 - stochastic_output_TF-Yale-Pitzer_novacc.csv.xz",
            "Virginia Pitzer - 2021-02-18 16.58.03 - stochastic_output_TF-Yale-Pitzer_campaign.csv.xz",
            "Virginia Pitzer - 2021-02-18 16.59.14 - stochastic_output_TF-Yale-Pitzer_camproutine.csv.xz"))

###############
# YF

scenarios <- c("yf-no-vaccination", "yf-preventive-bestcase",
               "yf-preventive-default", "yf-routine-bestcase",
               "yf-routine-default", "yf-stop")

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "IC-Garske",
  in_path = file.path(base_in_path, "IC-Garske"),
  out_path = file.path(base_out_path, "YF_IC-Garske"),
  scenarios = scenarios,
  files = "stochastic-burden-estimates.201910gavi-4_YF_IC-Garske_:scenario_:index.csv.xz",
  index = 1:200))

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "UND-Perkins",
  in_path = file.path(base_in_path, "UND-Perkins"),
  out_path = file.path(base_out_path, "YF_UND-Perkins"),
  scenarios = scenarios,
  files = "stochastic_burden_est_YF_UND-Perkins_:scenario_:index.csv.xz",
  index = 1:200))
