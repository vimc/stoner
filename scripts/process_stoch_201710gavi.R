base_in_path <- "//wpia-hn2.hpc.dide.ic.ac.uk/vimc_stochastics_dropbox/latest/201710gavi"
base_out_path <- "//wpia-hn2.hpc.dide.ic.ac.uk/vimc_stochastics/201710gavi"

# In case useful for looking up scenarios

vault <- vaultr::vault_client(login = "github")
password <- vault$read("/secret/vimc/database/production/users/readonly")$password
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "montagu",
                      host = "montagu.vaccineimpact.org",
                      port = 5432, password = password,
                      user = "readonly")

fetch_scenarios <- function(disease, touchstone='201710gavi-5') {
  sort(DBI::dbGetQuery(con, "
    SELECT scenario_description FROM
           scenario JOIN scenario_description
           ON scenario.scenario_description = scenario_description.id
           WHERE disease = $1 AND touchstone = $2", list(disease, touchstone))$scenario_description)
}

# Let's unleash the cow

setwd("Q:/testcow")
writeLines("vimc/stoner@VIMC-9230", "pkgdepends.txt")
hipercow::hipercow_init(driver = "dide-windows")
hipercow::hipercow_provision()
# Network/memory might be too much for more than a job per node.
hres <- hipercow::hipercow_resources(cores = 1L, exclusive = TRUE)


###############
# HepB

scenarios <- c("hepb-no-vaccination",
               "hepb-hepb-routine-with",
               "hepb-bd-routine-with",
               "hepb-bd-routine-with-hepb-routine-with",
               "hepb-bd-routine-best-hepb-routine-with")

stoner::stone_stochastic_standardise(
  group = "CDA-Razavi",
  in_path = file.path(base_in_path, "CDA-Razavi"),
  out_path = file.path(base_out_path, "HepB_CDA-Razavi"),
  scenarios = scenarios,
  files = "Devin Razavi-Shearer - stochastic_burden_template_HepB-CDA-Razavi_:scenario_:index.csv.xz",
  index = 1:98,
  allow_missing_indexes = TRUE)

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
    files = "stochastic_burden_est_:scenario_:index.csv.xz",
    index = 1:200))


###############
# HIB

scenarios <- c("hib-no-vaccination", "hib-routine-gavi")

stoner::stone_stochastic_standardise(
  group = "JHU-Tam",
  in_path = file.path(base_in_path, "JHU-Tam-Hib"),
  out_path = file.path(base_out_path, "Hib_JHU-Tam"),
  scenarios = scenarios,
  files = c("No_Hib_all.csv.xz", "With_Hib_all.csv.xz"))

stoner::stone_stochastic_standardise(
  group = "LSHTM-Clark",
  in_path = file.path(base_in_path, "LSHTM-Clark-Hib"),
  out_path = file.path(base_out_path, "Hib_LSHTM-Clark"),
  scenarios = scenarios,
  files = c("Kaja Abbas - stochastic-burden-estimate.201710gavi-5.Hib_LSHTM-CLark_standard-Hib-no-vaccination.csv.xz",
            "Kaja Abbas - stochastic-burden-estimate.201710gavi-5.Hib_LSHTM-CLark_standard-Hib-routine-gavi.csv.xz"))


###############
# HPV

scenarios <- c("hpv-no-vaccination", "hpv-routine-gavi",
               "hpv-campaign-gavi")

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "Harvard-Sweet",
  in_path = file.path(base_in_path, "Harvard-Sweet"),
  out_path = file.path(base_out_path, "HPV_Harvard-Sweet"),
  scenarios = scenarios,
  files = c("1. central_burden_template_HPV-Harvard-Sweet No Vaccine_PSA-:index_08.22.19.csv.xz",
            "2. central_burden_template_HPV-Harvard-Sweet Routine_PSA-:index_09.09.19.csv.xz",
            "3. central_burden_template_HPV-Harvard-Sweet Campaign_PSA-:index_08.22.19.csv.xz"),
  index = 1:200))

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "LSHTM-Jit",
  in_path = file.path(base_in_path, "LSHTM-Jit-HPV"),
  out_path = file.path(base_out_path, "HPV_LSHTM-Jit"),
  scenarios =  scenarios,
  files = "Kaja Abbas - stochastic-burden-estimate.201710gavi-5.HPV_LSHTM-Jit_standard-:scenario.csv.xz"))

###############
# JE

scenarios <- c("je-routine-no-vaccination", "je-campaign-gavi",
               "je-routine-gavi")

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "OUCRU-Clapham",
  in_path = file.path(base_in_path, "OUCRU-Clapham"),
  out_path = file.path(base_out_path, "JE_OUCRU-Clapham"),
  scenarios =  scenarios,
  files = "Duy Nguyen - stochastic-burden-template.201710gavi-6.JE_OUCRU-Clapham_standard_:scenario_:index.csv.xz",
  index = 1:200))

stub <- "Sean Moore - stochastic_burden_est_JE_UND-Moore"

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "UND-Moore",
  in_path = file.path(base_in_path, "UND-Moore"),
  out_path = file.path(base_out_path, "JE_UND-Moore"),
  scenarios =  scenarios,
  files = c("Sean Moore - stochastic_burden_est_JE_UND-Moore_je-no-vaccination.csv.xz",
            "Sean Moore - stochastic_burden_est_JE_UND-Moore_je-campaign-gavi.csv.xz",
            "Sean Moore - stochastic_burden_est_JE_UND-Moore_je-routine-gavi.csv.xz")))



###############
# Measles
scenarios <- c("measles-no-vaccination", "measles-mcv1-gavi",
               "measles-mcv2-gavi", "measles-campaign-gavi")

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "PSU-Ferrari",
  in_path = file.path(base_in_path, "PSU-Ferrari"),
  out_path = file.path(base_out_path, "Measles_PSU-Ferrari"),
  scenarios = scenarios,
  files = c("Matthew Ferrari - stochastic_burden_novax_Measles-PSU-Ferrari-5.csv.xz",
            "Matthew Ferrari - stochastic_burden_mcv1_Measles-PSU-Ferrari-5.csv.xz",
            "Matthew Ferrari - stochastic_burden_mcv2_Measles-PSU-Ferrari-5.csv.xz",
            "Matthew Ferrari - stochastic_burden_sia_Measles-PSU-Ferrari-5.csv.xz")))

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "LSHTM-Jit",
  in_path = file.path(base_in_path, "LSHTM-Jit-Measles"),
  out_path = file.path(base_out_path, "Measles_LSHTM-Jit"),
  scenarios = scenarios,
  files = c("stochastic_burden_template_Measles-LSHTM-Jit-no_vaxx.csv.xz",
            "Petra Klepac - stochastic_burden_template_Measles-LSHTM-Jit-mcv1-only.csv.xz",
            "stochastic_burden_template_Measles-LSHTM-Jit-mcv1_mcv2.csv.xz",
            "stochastic_burden_template_Measles-LSHTM-Jit-mcv1-mcv2-campaigns.csv.xz")))

###############
# MenA

scenarios = c("mena-no-vaccination", "mena-campaign-gavi",
              "mena-routine-gavi")

stoner::stone_stochastic_standardise(
  group = "Cambridge-Trotter",
  in_path = file.path(base_in_path, "Cambridge-Trotter"),
  out_path = file.path(base_out_path, "MenA_Cambridge-Trotter"),
  scenarios = scenarios,
  files = c("Cambridge-Trotter-mena-no-vacc.csv.xz",
            "Cambridge-Trotter-mena-campaign.csv.xz",
            "Cambridge-Trotter-mena-routine.csv.xz"))

stoner::stone_stochastic_standardise(
  group = "KPW-Jackson",
  in_path = file.path(base_in_path, "KPW-Jackson"),
  out_path = file.path(base_out_path, "MenA_KPW-Jackson"),
  scenarios = scenarios,
  files = c("Mike Jackson - stochastic_burden_est_MenA-KPW-Jackson_mena-no-vaccination_:index.csv.xz",
            "Mike Jackson - stochastic_burden_est_MenA-KPW-Jackson_mena-campaign-gavi_:index.csv.xz",
            "Mike Jackson - stochastic_burden_est_MenA-KPW-Jackson_mena-routine-gavi_:index.csv.xz"),
  index = 1:2,
  allow_missing_indexes = TRUE)

###############
# PCV

scenarios <- c("pcv-no-vaccination", "pcv-routine-gavi")

stoner::stone_stochastic_standardise(
  group = "JHU-Tam",
  in_path = file.path(base_in_path, "JHU-Tam-PCV"),
  out_path = file.path(base_out_path, "PCV_JHU-Tam"),
  scenarios = scenarios,
  files = c("No_PCV_all.csv.xz", "With_PCV_all.csv.xz"))

stoner::stone_stochastic_standardise(
  group = "LSHTM-Clark",
  in_path = file.path(base_in_path, "LSHTM-Clark-PCV"),
  out_path = file.path(base_out_path, "PCV_LSHTM-Clark"),
  scenarios = scenarios,
  files = c("Kaja Abbas - stochastic-burden-estimate.201710gavi-5.PCV_LSHTM-CLark_standard-PCV-no-vaccination.csv.xz",
            "Kaja Abbas - stochastic-burden-estimate.201710gavi-5.PCV_LSHTM-CLark_standard-PCV-routine-gavi.csv.xz"))

###############
# Rota

scenarios <- c("rota-no-vaccination", "rota-routine-gavi")

stoner::stone_stochastic_standardise(
  group = "JHU-Tam",
  in_path = file.path(base_in_path, "JHU-Tam-Rota"),
  out_path = file.path(base_out_path, "Rota_JHU-Tam"),
  scenarios = scenarios,
  files = c("No_Rota_all.csv.xz", "With_Rota_all.csv.xz"))

stoner::stone_stochastic_standardise(
  group = "LSHTM-Clark",
  in_path = file.path(base_in_path, "LSHTM-Clark-Rota"),
  out_path = file.path(base_out_path, "Rota_LSHTM-Clark"),
  scenarios = scenarios,
  files = c("Kaja Abbas - stochastic-burden-estimate.201710gavi-5.Rota_LSHTM-CLark_standard-Rota-no-vaccination.csv.xz",
            "Kaja Abbas - stochastic-burden-estimate.201710gavi-5.Rota_LSHTM-CLark_standard-Rota-routine-gavi.csv.xz"))

###############
# Rubella

scenarios <- c(
  "rubella-routine-no-vaccination", "rubella-campaign-gavi",
  "rubella-rcv1-gavi", "rubella-rcv2-gavi")

#hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "JHU-Lessler",
  in_path = file.path(base_in_path, "JHU-Lessler"),
  out_path = file.path(base_out_path, "Rubella_JHU-Lessler"),
  scenarios = scenarios,
  files = c("Amy Winter - stochastic_burden_est-rubella-no-vaccination_:index.csv.xz",
            "Amy Winter - stochastic_burden_est-rubella-campaign-gavi_:index.csv.xz",
            "Amy Winter - stochastic_burden_est-rubella-routine-gavi_:index.csv.xz",
            "Amy Winter - stochastic_burden_est-rubella-rcv2-gavi_:index.csv.xz"),
  index = 1:10)

scenarios <- c(
  "rubella-routine-no-vaccination", "rubella-campaign-gavi",
  "rubella-rcv2-gavi")

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "PHE-Vynnycky",
  in_path = file.path(base_in_path, "PHE-Vynnycky"),
  out_path = file.path(base_out_path, "Rubella_PHE-Vynnycky"),
  scenarios = scenarios,
  files = c("stochastic_burden_estimate_Rubella-PHE-Vynnycky_rubella-routine-no-vaccination_:index.csv.xz",
            "stochastic_burden_estimate_Rubella-PHE-Vynnycky_rubella-campaign-gavi_:index.csv.xz",
            "stochastic_burden_estimate_Rubella-PHE-Vynnycky_rubella-routine-gavi_:index.csv.xz"),
  index = 1:200))


###############
# YF

scenarios <- c("yf-no-vaccination", "yf-preventive-gavi",
               "yf-routine-gavi")

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "IC-Garske",
  in_path = file.path(base_in_path, "IC-Garske"),
  out_path = file.path(base_out_path, "YF_IC-Garske"),
  scenarios = scenarios,
  files = c("Katy Gaythorpe - NEW_NEW_stochastic_burden_estimate_YF_no-vaccination_:index.csv.xz",
            "Katy Gaythorpe - NEW_NEW_stochastic_burden_estimate_YF_preventive-gavi_:index.csv.xz",
            "Katy Gaythorpe - NEW_NEW_stochastic_burden_estimate_YF_routine-gavi_:index.csv.xz"),
  index = 1:200))

hipercow::task_create_expr(resources = hres, expr =
stoner::stone_stochastic_standardise(
  group = "UND-Perkins",
  in_path = file.path(base_in_path, "UND-Perkins"),
  out_path = file.path(base_out_path, "YF_UND-Perkins"),
  scenarios = scenarios,
  files = c("John Huber - stochastic_burden_est_YF_UND-Perkins_yf-no-vaccination-gavi_:index.csv.xz",
            "John Huber - stochastic_burden_est_YF_UND-Perkins_yf-preventive-gavi_:index.csv.xz",
            "John Huber - stochastic_burden_est_YF_UND-Perkins_yf-routine-gavi_:index.csv.xz"),
  index = 1:200))
