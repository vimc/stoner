## This script runs stochastic processing for each modelling group and disease
## There is a function for each year
## Before running check the in_path, output_path, pre_aggregation_path
## and log_file are set correctly
## This also uses dettl to create the db connection so you will need to
## set dettl_root to path to a local dettl repo.

continue_on_error <- function(expr) {
  tryCatch(
    withCallingHandlers(
      eval(expr),
      error = function(e) {
        calls <- sys.calls()
        calls <- calls[8:length(calls)] ## Ignore all try catch stuff from top of stack trace
        message(paste(e$message, limitedLabels(calls), sep = "\n"))
      }
    ),
    error = function(e) {invisible(NULL)}
  )
}


do_stochastics_2021 <- function(con, test_run) {
  in_path <- "Z:/File requests/latest/202110gavi/"
  out_path <- "Z:/stochastic_2021_output/aggregated/"
  pre_aggregation_path <- "Z:/stochastic_2021_output/pre-aggregate/"
  log_file <- "Z:/stochastic_2021_output/log.txt"
  output_files <- "Z:/stochastic_2021_output/output_files.csv"
  files <- data.frame(
    touchstone = character(0),
    modelling_group = character(0),
    disease = character(0),
    files = character(0),
    is_cohort = logical(0),
    is_under5 = logical(0)
  )
  dir.create(out_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(pre_aggregation_path, showWarnings = FALSE, recursive = TRUE)
  write.csv(files, output_files, row.names = FALSE)

  lines <- Inf
  if (isTRUE(test_run)) {
    lines <- 30
  }

  stub <- "Andromachi Karachaliou - stochastic-burden.202110gavi-2.MenA_Cambridge-Trotter_"
  modelling_group = "Cambridge-Trotter"
  disease = "MenA"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("mena-no-vaccination", "mena-campaign-default",
                    "mena-routine-default", "mena-booster-default",
                    "mena-campaign-ia2030_target", "mena-routine-ia2030_target"),
      in_path = file.path(in_path, "Cambridge-Trotter"),
      files = c(paste0(stub, "no_vaccination_:index.csv.xz"),
                paste0(stub, "campaign_default_:index.csv.xz"),
                paste0(stub, "routine_default_:index.csv.xz"),
                paste0(stub, "booster_:index.csv.xz"),
                paste0(stub, "campaign-ia2030_target_:index.csv.xz"),
                paste0(stub, "routine-ia2030_target_:index.csv.xz")),
      cert = "",
      index_start = 1,
      index_end = 26,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      bypass_cert_check = TRUE,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  #############################################################################

  stub <- "Aniruddha Deshpande - stochastic_burden_est_lopman_"
  modelling_group = "Emory-Lopman"
  disease = "Rota"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("rota-no-vaccination",
                    "rota-routine-default",
                    "rota-routine-ia2030_target"),
      in_path = file.path(in_path, "Emory-Lopman"),
      files = c(paste0(stub, "no_vaccination_2022_01_31.csv.xz"),
                paste0(stub, "routine_2022_01_31.csv.xz"),
                paste0(stub, "ia2030_target_2022_01_31.csv.xz")),
      cert = "",
      index_start = NA,
      index_end = NA,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      allow_missing_disease = TRUE,
      bypass_cert_check = TRUE,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  #############################################################################

  stub <- "Allison Portnoy - stochastic-burden-est."
  modelling_group = "Harvard-Sweet"
  disease = "HPV"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("hpv-no-vaccination",
                    "hpv-campaign-default",
                    "hpv-campaign-ia2030_target",
                    "hpv-routine-default",
                    "hpv-routine-ia2030_target"),
      in_path = file.path(in_path, "Harvard-Sweet"),
      files = c(paste0(stub, "novacc_run_:index.csv.xz"),
                paste0(stub, "coverage_202110gavi-3_hpv-campaign-default_run_:index.csv.xz"),
                paste0(stub, "coverage_202110gavi-3_hpv-campaign-ia2030_target_run_:index.csv.xz"),
                paste0(stub, "coverage_202110gavi-3_hpv-routine-default_run_:index.csv.xz"),
                paste0(stub, "coverage_202110gavi-3_hpv-routine-ia2030_target_run_:index.csv.xz")),
      cert = "",
      index_start = 1,
      index_end = 200,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      runid_from_file = TRUE,
      bypass_cert_check = TRUE,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  #############################################################################

  stub <- "Keith Fraser - stochastic-burden-estimates.202110gavi-3_YF_IC-Garske_"
  modelling_group = "IC-Garske"
  disease = "YF"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("yf-no-vaccination",
                    "yf-preventive-default",
                    "yf-preventive-ia2030_target",
                    "yf-routine-default",
                    "yf-routine-ia2030_target"),
      in_path = file.path(in_path, "IC-Garske"),
      files = paste0(stub, ":scenario_:index.csv.xz"),
      cert = "",
      index_start = 1,
      index_end = 200,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      bypass_cert_check = TRUE,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  #############################################################################

  modelling_group = "IVI-Kim"
  disease = "Typhoid"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("typhoid-no-vaccination",
                    "typhoid-campaign-default", "typhoid-campaign-ia2030_target",
                    "typhoid-routine-default",  "typhoid-routine-ia2030_target"),
      in_path = file.path(in_path, "IVI-Kim-Typhoid"),
      files = c("Jong-Hoon Kim - stoch_Typhoid_novacc_20211217T1.csv.xz",
                "Jong-Hoon Kim - stoch_Typhoid_campaign-default_20211217T1.csv.xz",
                "Jong-Hoon Kim - stoch_Typhoid_campaign-ia2030_20211217T1.csv.xz",
                "Jong-Hoon Kim - stoch_Typhoid_routine-default_20211217T1.csv.xz",
                "Jong-Hoon Kim - stoch_Typhoid_routine-ia2030_20211217T1.csv.xz"),
      cert = "",
      index_start = NA,
      index_end = NA,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      bypass_cert_check = TRUE,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  #####################################################

  stub <- "stochastic_burden_est_HepB-IC-Hallett_"
  modelling_group = "IC-Hallett"
  disease = "HepB"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("hepb-bd-default-hepb-routine-default",
                    "hepb-bd-routine-default",
                    "hepb-bd-routine-ia2030_target-hepb-routine-ia2030_target",
                    "hepb-bd-routine-ia2030_target",
                    "hepb-hepb-routine-default",
                    "hepb-hepb-routine-ia2030_target",
                    "hepb-no-vaccination"),
      in_path = file.path(in_path, "IC-Hallett"),
      files = paste0(stub, ":scenario_:index.csv.xz"),
      cert = "Margaret de Villiers - cert115",
      index_start = 1,
      index_end = 200,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      deaths = "deaths",
      cases = c("hepb_cases_acute_severe","hepb_cases_comp_cirrh",
                "hepb_cases_hcc_no_cirrh"),
      dalys = "dalys",
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  #############################################################################

  modelling_group = "IVI-Kim"
  disease = "Cholera"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("cholera-no-vaccination", "cholera-campaign-default"),
      in_path = file.path(in_path, "IVI-Kim-Cholera"),
      files = c("Jong-Hoon Kim - stoch_Cholera_novacc_20211221T00.csv.xz",
                "Jong-Hoon Kim - stoch_Cholera_campaign_20211222T212131.csv.xz"),
      cert = "",
      index_start = NA,
      index_end = NA,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      bypass_cert_check = TRUE,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  #############################################################################

  modelling_group = "JHU-Lee"
  disease = "Cholera"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("cholera-no-vaccination", "cholera-campaign-default"),
      in_path = file.path(in_path, "JHU-Lee-Cholera"),
      files = c("Kaiyue Zou - no-vaccination.csv.xz",
                "Kaiyue Zou - campaign-default.csv.xz"),
      cert = "",
      index_start = NA,
      index_end = NA,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      bypass_cert_check = TRUE,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  #############################################################################


  stub <- "Amy Winter - stochastic_burden_est-rubella-"
  modelling_group = "JHU-Lessler"
  disease = "Rubella"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("rubella-routine-no-vaccination",
                    "rubella-campaign-default",
                    "rubella-rcv1-default",
                    "rubella-rcv2-default",
                    "rubella-rcv1-rcv2-default",
                    "rubella-campaign-ia2030_target",
                    "rubella-rcv1-ia2030_target",
                    "rubella-rcv2-ia2030_target",
                    "rubella-rcv1-rcv2-ia2030_target"),
      in_path = file.path(in_path, "JHU-UGA-Winter-Rubella"),
      files = c(paste0(stub, "routine-no-vaccination_:index.csv.xz"),
                paste0(stub, "campaign-default_:index.csv.xz"),
                paste0(stub, "rcv1-default_:index.csv.xz"),
                paste0(stub, "rcv2-default_:index.csv.xz"),
                paste0(stub, "rcv1-rcv2-default_:index.csv.xz"),
                paste0(stub, "campaign-ia2030_target_:index.csv.xz"),
                paste0(stub, "rcv1-ia2030_target_:index.csv.xz"),
                paste0(stub, "rcv2-ia2030_target_:index.csv.xz"),
                paste0(stub, "rcv1-rcv2-ia2030_target_:index.csv.xz")),
      cert = "",
      index_start = 1,
      index_end = 11,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      deaths = "rubella_deaths_congenital",
      cases = "rubella_cases_congenital",
      dalys = "dalys",
      bypass_cert_check = TRUE,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  #############################################################################

  list_params_hib_pcv <- data.frame(
    outcome = c("cases_men", "cases_men", "cases_men", "cases_men", "cases_men",
                "cases_pneumo", "cases_pneumo", "deaths_men", "deaths_pneumo"),
    proportion = c(1, 0.014, 0.045, 0.021, 0.017, 1, 0.06, 1, 1),
    average_duration = c(0.04,1000,1000,1000,1000,0.02,1000,1000,1000),
    disability_weight = c(0.133, 0.043, 0.027, 0.552, 0.61, 0.051, 0.019, 1, 1)
  )

  hib_scenarios <- c("hib-no-vaccination-LiST",
                     "hib-routine-default-LiST",
                     "hib-routine-ia2030_target-LiST")

  modelling_group = "JHU-Tam"
  disease = "Hib"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = hib_scenarios,
      in_path = file.path(in_path, "JHU-Tam-Carter-Hib"),
      files = ":scenario.csv.xz",
      cert = "",
      index_start = NA,
      index_end = NA,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      deaths = c("deaths_men", "deaths_pneumo"),
      cases = c("cases_men", "cases_pneumo"),
      dalys = list_params_hib_pcv,
      bypass_cert_check = TRUE,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  # And to sort out DALYs on the centrals:

  for (hib_scenario in hib_scenarios) {
    stoner::stoner_dalys_for_db(con, list_params_hib_pcv, "JHU-Tam",
                                "Hib", "202110gavi-3", hib_scenario,
                                output_file = file.path(out_path, sprintf("%s_central_dalys.qs",
                                                                          hib_scenario)))
  }

  pcv_scenarios <- c("pcv-no-vaccination-LiST",
                     "pcv-routine-default-LiST",
                     "pcv-routine-ia2030_target-LiST")

  modelling_group = "JHU-Tam"
  disease = "PCV"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = pcv_scenarios,
      in_path = file.path(in_path, "JHU-Tam-Carter-PCV"),
      files = ":scenario.csv.xz",
      cert = "",
      index_start = NA,
      index_end = NA,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      deaths = c("deaths_men", "deaths_pneumo"),
      cases = c("cases_men", "cases_pneumo"),
      dalys = list_params_hib_pcv,
      bypass_cert_check = TRUE,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  # And to sort out DALYs on the centrals:

  for (pcv_scenario in pcv_scenarios) {
    stoner::stoner_dalys_for_db(con, list_params_hib_pcv, "JHU-Tam",
                                "PCV", "202110gavi-3", pcv_scenario,
                                output_file = file.path(out_path, sprintf("%s_central_dalys.qs",
                                                                          pcv_scenario)))
  }


  list_params_rota <- data.frame(
    outcome = c("cases", "deaths"),
    proportion = c(1, 1),
    average_duration = c(0.01, 1000),
    disability_weight = c(0.247, 1)
  )

  rota_scenarios <- c("rota-no-vaccination-LiST",
                      "rota-routine-default-LiST",
                      "rota-routine-ia2030_target-LiST")

  modelling_group = "JHU-Tam"
  disease = "Rota"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = rota_scenarios,
      in_path = file.path(in_path, "JHU-Tam-Carter-Rota"),
      files = ":scenario.csv.xz",
      cert = "",
      index_start = NA,
      index_end = NA,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      dalys = list_params_rota,
      bypass_cert_check = TRUE,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })


  # And to sort out DALYs on the centrals:

  for (rota_scenario in rota_scenarios) {
    stoner::stoner_dalys_for_db(con, list_params_rota, "JHU-Tam",
                                "Rota", "202110gavi-3", rota_scenario,
                                output_file = file.path(out_path, sprintf("%s_central_dalys.qs",
                                                                          rota_scenario)))
  }

  ####################################################################################


  stub <- "stochastic_burden_est_MenA_KPWA_"
  modelling_group = "KPW-Jackson"
  disease = "MenA"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("mena-booster-default",
                    "mena-campaign-default",
                    "mena-campaign-ia2030_target",
                    "mena-no-vaccination",
                    "mena-routine-default",
                    "mena-routine-ia2030_target"),
      in_path = file.path(in_path, "KPW-Jackson-MenA"),
      files = c(paste0(stub, "booster_default_:index.csv.xz"),
                paste0(stub, "campaign_default_:index.csv.xz"),
                paste0(stub, "campaign_ia2030_target_:index.csv.xz"),
                paste0(stub, "none_default_:index.csv.xz"),
                paste0(stub, "routine_default_:index.csv.xz"),
                paste0(stub, "routine_ia2030_target_:index.csv.xz")),
      cert = "",
      index_start = 1,
      index_end = 26,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      bypass_cert_check = TRUE,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  #############################################################################

  modelling_group = "Li"
  disease = "HepB"
  touchstone = "202110gavi-2"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("hepb-bd-default-hepb-routine-default",
                    "hepb-bd-routine-default",
                    "hepb-bd-routine-ia2030_target-hepb-routine-ia2030_target",
                    "hepb-bd-routine-ia2030_target",
                    "hepb-hepb-routine-default",
                    "hepb-hepb-routine-ia2030_target",
                    "hepb-no-vaccination"
      ),
      in_path = file.path(in_path, "Li"),
      files = paste0(":scenario:index.csv.xz"),
      cert = "cert105",
      index_start = 1,
      index_end = 200,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      deaths = c("hepb_deaths_acute", "hepb_deaths_total_cirrh", "hepb_deaths_hcc"),
      cases = c("hepb_cases_acute_symp", "hepb_cases_fulminant",
                "hepb_cases_chronic", "hepb_chronic_symptomatic_in_acute_phase"),
      dalys = "dalys",
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  #############################################################################

  stub <- "Kaja Abbas - PSA_202110gavi-3_"
  modelling_group = "LSHTM-Clark"
  disease = "Hib"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("hib-no-vaccination","hib-routine-default","hib-routine-ia2030_target"),
      in_path = file.path(in_path, "LSHTM-Clark_Hib"),
      files = c(paste0(stub, ":scenario.csv.xz")),
      cert = "Kaja Abbas - hib_cert116",
      index_start = NA,
      index_end = NA,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  #############################################################################


  stub <- "Kaja Abbas - PSA_202110gavi-3_"
  modelling_group = "LSHTM-Clark"
  disease = "Rota"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("rota-no-vaccination","rota-routine-default","rota-routine-ia2030_target"),
      in_path = file.path(in_path, "LSHTM-Clark_Rota"),
      files = c(paste0(stub, ":scenario.csv.xz")),
      cert = "Kaja Abbas - rota_cert117",
      index_start = NA,
      index_end = NA,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  #############################################################################


  stub <- "stochastic-burden-"
  modelling_group = "LSHTM-Jit"
  disease = "HPV"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("hpv-no-vaccination",
                    "hpv-campaign-default",
                    "hpv-routine-default",
                    "hpv-campaign-ia2030_target",
                    "hpv-routine-ia2030_target"),
      in_path = file.path(in_path, "LSHTM-Jit_HPV"),
      files = c(paste0(stub, "novaccination_all_202110gavi-3_hpv-no-vaccination.csv.xz"),
                paste0(stub, "vaccination_all_202110gavi-3_hpv-campaign-default.csv.xz"),
                paste0(stub, "vaccination_all_202110gavi-3_hpv-routine-default.csv.xz"),
                paste0(stub, "vaccination_all_202110gavi-3_hpv-campaign-ia2030_target.csv.xz"),
                paste0(stub, "vaccination_all_202110gavi-3_hpv-routine-ia2030_target.csv.xz")),
      cert = "cert104", index_start = NA, index_end = NA, out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      bypass_cert_check = TRUE,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  #############################################################################

  stub <- "Han Fu - stochastic_burden_estimate_measles-LSHTM-Jit-"
  modelling_group = "LSHTM-Jit"
  disease = "Measles"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("measles-no-vaccination",
                    "measles-campaign-default",
                    "measles-campaign-only-default",
                    "measles-mcv1-default",
                    "measles-mcv2-default",
                    "measles-campaign-ia2030_target",
                    "measles-campaign-only-ia2030_target",
                    "measles-mcv1-ia2030_target",
                    "measles-mcv2-ia2030_target"),
      in_path = file.path(in_path, "LSHTM-Jit_Measles"),
      files = c(paste0(stub, "no-vaccination.csv.xz"),
                paste0(stub, "campaign-default.csv.xz"),
                paste0(stub, "campaign-only-default.csv.xz"),
                paste0(stub, "mcv1-default.csv.xz"),
                paste0(stub, "mcv2-default.csv.xz"),
                paste0(stub, "campaign-ia2030_target.csv.xz"),
                paste0(stub, "campaign-only-ia2030_target.csv.xz"),
                paste0(stub, "mcv1-ia2030_target.csv.xz"),
                paste0(stub, "mcv2-ia2030_target.csv.xz")),
      cert = "Han Fu - cert112_measles-LSHTM-Jit_202110gavi-3",
      index_start = NA,
      index_end = NA,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  #############################################################################

  stub <- "stochastic_burden_est_"
  modelling_group = "NUS-Chen"
  disease = "PCV"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("pcv-no-vaccination","pcv-routine-default","pcv-routine-ia2030_target"),
      in_path = file.path(in_path, "LSHTM-NUS-Chen_PCV"),
      files = paste0(stub, ":scenario.csv.xz"),
      cert = "Jemima Koh - cert121",
      index_start = NA,
      index_end = NA,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  #############################################################################

  modelling_group = "PHE-Vynnycky"
  disease = "Rubella"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("rubella-routine-no-vaccination",
                    "rubella-campaign-default",
                    "rubella-rcv1-default",
                    "rubella-rcv2-default",
                    "rubella-rcv1-rcv2-default",
                    "rubella-campaign-ia2030_target",
                    "rubella-rcv1-ia2030_target",
                    "rubella-rcv2-ia2030_target",
                    "rubella-rcv1-rcv2-ia2030_target"),
      in_path = file.path(in_path, "PHE-Vynnycky_Rubella"),
      files = c("VIMC_NV_RCV1RCV2Camp_country:index.csv.xz",
                "VIMC_DF_Camp_country:index.csv.xz",
                "VIMC_DF_RCV1Camp_country:index.csv.xz",
                "VIMC_DF_RCV1RCV2Camp_country:index.csv.xz",
                "VIMC_DF_RCV1RCV2_country:index.csv.xz",
                "VIMC_IA_Camp_country:index.csv.xz",
                "VIMC_IA_RCV1Camp_country:index.csv.xz",
                "VIMC_IA_RCV1RCV2Camp_country:index.csv.xz",
                "VIMC_IA_RCV1RCV2_country:index.csv.xz"),
      cert = "",
      index_start = 1,
      index_end = 112,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      deaths = "rubella_deaths_congenital",
      cases = "rubella_cases_congenital",
      dalys = "dalys",
      bypass_cert_check = TRUE,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  #############################################################################

  stub <- "coverage_202110gavi-3_"
  modelling_group = "PSU-Ferrari"
  disease = "Measles"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("measles-campaign-default",
                    "measles-campaign-ia2030_target",
                    "measles-campaign-only-default",
                    "measles-campaign-only-ia2030_target",
                    "measles-mcv1-default",
                    "measles-mcv1-ia2030_target",
                    "measles-mcv2-default",
                    "measles-mcv2-ia2030_target",
                    "measles-no-vaccination"),
      in_path = file.path(in_path, "PSU-Ferrari-Measles"),
      files = paste0(stub, ":scenario.csv.xz"),
      cert = "",
      index_start = NA,
      index_end = NA,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      bypass_cert_check = TRUE,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })

  #############################################################################

  stub <- "Sean Moore - stochastic_burden_est_JE_UND-Moore_"
  modelling_group = "UND-Moore"
  disease = "JE"
  touchstone = "202110gavi-2"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("je-routine-no-vaccination",
                    "je-campaign-default",
                    "je-routine-default",
                    "je-campaign-ia2030_target",
                    "je-routine-ia2030_target"
      ),
      in_path = file.path(in_path, "UND-Moore-JE"),
      files = paste0(stub, ":scenario.csv.xz"),
      cert = "Sean Moore - cert108",
      index_start = NA,
      index_end = NA,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })


  #############################################################################


  stub <- "stochastic_burden_est_YF_UND-Perkins_"
  modelling_group = "UND-Perkins"
  disease = "YF"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("yf-no-vaccination",
                    "yf-preventive-default",
                    "yf-preventive-ia2030_target",
                    "yf-routine-default",
                    "yf-routine-ia2030_target"),
      in_path = file.path(in_path, "UND-Perkins-YF"),
      files = paste0(stub, ":scenario_:index.csv.xz"),
      cert = "",
      index_start = 1,
      index_end = 200,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      bypass_cert_check = TRUE,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })


  #############################################################################


  stub <- "Holly Burrows - stochastic_burden_est_TF-Yale-Burrows"
  modelling_group = "Yale-Pitzer"
  disease = "Typhoid"
  touchstone = "202110gavi-3"
  continue_on_error({
    paths <- stone_stochastic_process(
      con,
      modelling_group = modelling_group,
      disease = disease,
      touchstone = touchstone,
      scenarios = c("typhoid-no-vaccination",
                    "typhoid-campaign-default", "typhoid-campaign-ia2030_target",
                    "typhoid-routine-default",  "typhoid-routine-ia2030_target"),
      in_path = file.path(in_path, "Yale-Pitzer-Typhoid"),
      files = c(paste0(stub, "-novacc_202110.csv.xz"),
                paste0(stub, "_campaign-default_202110.csv.xz"),
                paste0(stub, "_campaign-IA2030_202110.csv.xz"),
                paste0(stub, "_routine-default_202110.csv.xz"),
                paste0(stub, "_routine-IA2030_202110.csv.xz")),
      cert = "",
      index_start = NA,
      index_end = NA,
      out_path = out_path,
      pre_aggregation_path = pre_aggregation_path,
      log_file = log_file,
      bypass_cert_check = TRUE,
      lines = lines)
    files <- data.frame(
      touchstone = touchstone,
      modelling_group = modelling_group,
      disease = disease,
      files = c(paths$all_u5_cal_file, paths$all_u5_coh_file,
                paths$all_cal_file, paths$all_coh_file),
      is_cohort = c(FALSE, TRUE, FALSE, TRUE),
      is_under5 = c(TRUE, TRUE, FALSE, FALSE)
    )
    write.table(files, output_files, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
  })
}

do_stochastics_2019 <- function(con, test_run) {
  in_path <- "E:/Dropbox (SPH Imperial College)/File requests/latest/201910gavi"
  out_path <- "E:/stochastic_2019_output/aggregated/"
  pre_aggregation_path <- "E:/stochastic_2019_output/pre-aggregate/"
  log_file <- "E:/stochsatic_2019_output/log.txt"

  lines <- Inf
  if (isTRUE(test_run)) {
    lines <- 30
  }

  #############################################################################

  stub <- "Andromachi Karachaliou - stochastic-burden.201910gavi-4.MenA_Cambridge-Trotter_"
  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "Cambridge-Trotter",
    disease = "MenA",
    touchstone = "201910gavi-5",
    scenarios = c("mena-campaign-bestcase", "mena-campaign-default", "mena-no-vaccination",
                  "mena-routine-bestcase", "mena-routine-default"),
    in_path = file.path(in_path, "Cambridge-Trotter"),
    files = c(paste0(stub, "campaign-bestcase_:index.csv.xz"),
              paste0(stub, "campaign-default_:index.csv.xz"),
              paste0(stub, "no-vaccination_:index.csv.xz"),
              paste0(stub, "routine-bestcase_:index.csv.xz"),
              paste0(stub, "routine-default_:index.csv.xz")),
    cert = "cert60",
    index_start = 1,
    index_end = 52,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    lines = lines))

  #############################################################################

  stub <- "Ivane Gamkrelidze - stochastic-burden-template.201910gavi-4.HepB_CDA-Razavi_"
  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "CDA-Razavi",
    disease = "HepB",
    touchstone = "201910gavi-5",
    senarios = c("hepb-bd-default-hepb-routine-default",
                 "hepb-bd-routine-bestcase-hepb-routine-bestcase",
                 "hepb-no-vaccination",
                 "hepb-stop",
                 "hepb-bd-routine-bestcase",
                 "hepb-bd-routine-default",
                 "hepb-hepb-routine-bestcase",
                 "hepb-hepb-routine-default"
    ),
    in_path = file.path(in_path, "CDA-Razavi"),
    files = c(paste0(stub, "all_:scenario.csv.xz"),
              paste0(stub, "all_:scenario.csv.xz"),
              paste0(stub, "all_:scenario.csv.xz"),
              paste0(stub, "all_:scenario.csv.xz"),
              paste0(stub, "bd_:scenario.csv.xz"),
              paste0(stub, "bd_:scenario.csv.xz"),
              paste0(stub, "non_bd_:scenario.csv.xz"),
              paste0(stub, "non_bd_:scenario.csv.xz")),
    cert = "Ivane Gamkrelidze - cert68",
    index_start = NA,
    index_end = NA,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    deaths = c("hepb_deaths_acute","hepb_deaths_dec_cirrh","hepb_deaths_hcc"),
    cases = c("hepb_cases_acute_severe","hepb_cases_dec_cirrh","hepb_cases_hcc"),
    dalys = "dalys",
    lines = lines))

  #############################################################################

  stub <- "Molly Steele - stochastic-burden.201910gavi-4.Rota_Emory-Lopman_"
  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "Emory-Lopman",
    disease = "Rota",
    touchstone = "201910gavi-5",
    scenarios = c("rota-no-vaccination",
                  "rota-routine-bestcase",
                  "rota-routine-default"),
    in_path = file.path(in_path, "Emory-Lopman"),
    files = paste0(stub, ":scenario.csv.xz"),
    cert = "Molly Steele - cert66",
    index_start = NA,
    index_end = NA,
    out_path= out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    allow_missing_disease = TRUE,
    lines = lines))

  #############################################################################

  stub <- "stochastic-burden-est.201910gavi-5.HPV_Harvard-Sweet_"
  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "Harvard-Sweet",
    disease = "HPV",
    touchstone = "201910gavi-5",
    sceanrios = c("hpv-campaign-bestcase",
                  "hpv-campaign-default",
                  "hpv-no-vaccination",
                  "hpv-routine-bestcase",
                  "hpv-routine-default"),
    in_path = file.path(in_path, "Harvard-Sweet"),
    files = c(paste0(stub, "campaign-bestcase_run_:index.csv.xz"),
              paste0(stub, "campaign-default_run_:index.csv.xz"),
              paste0(stub, "novacc_run_:index.csv.xz"),
              paste0(stub, "routine-bestcase_run_:index.csv.xz"),
              paste0(stub, "routine-default_run_:index.csv.xz")),
    cert = "",
    index_start = 1,
    index_end = 200,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    runid_from_file = TRUE,
    bypass_cert_check = TRUE,
    lines = lines))

  #############################################################################

  stub <- "stochastic-burden-estimates.201910gavi-4_YF_IC-Garske_"
  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "IC-Garske",
    disease = "YF",
    touchstone = "201910gavi-5",
    scenarios = c("yf-no-vaccination",
                  "yf-preventive-bestcase",
                  "yf-preventive-default",
                  "yf-routine-bestcase",
                  "yf-routine-default",
                  "yf-stop"),
    in_path = file.path(in_path, "IC-Garske2"),
    files = paste0(stub, ":scenario_:index.csv.xz"),
    cert = "Katy Gaythorpe - cert62",
    index_start = 1,
    index_end = 200,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    lines = lines))

  #############################################################################

  stub <- "stochastic_burden_est_HepB-IC-Hallett_"
  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "IC-Hallett",
    disease = "HepB",
    touchstone = "201910gavi-5",
    scenarios = c("hepb-bd-default-hepb-routine-default",
                  "hepb-bd-routine-bestcase-hepb-routine-bestcase",
                  "hepb-no-vaccination",
                  "hepb-stop",
                  "hepb-bd-routine-bestcase",
                  "hepb-bd-routine-default",
                  "hepb-hepb-routine-bestcase",
                  "hepb-hepb-routine-default"),
    in_path = file.path(in_path, "IC-Hallett"),
    files = paste0(stub, ":scenario_:index.csv.xz"),
    cert = "Margaret de Villiers  - cert73",
    index_start = 1,
    index_end = 200,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    deaths = "deaths",
    cases = c("hepb_cases_acute_severe","hepb_cases_comp_cirrh",
              "hepb_cases_hcc_no_cirrh"),
    dalys = "dalys",
    lines = lines))

  #############################################################################

  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "IVI-Kim",
    disease = "Cholera",
    touchstone = "201910gavi-5",
    scenarios = c("cholera-no-vaccination", "cholera-campaign-default"),
    in_path = file.path(in_path, "IVI-Kim-Cholera"),
    files = c("Jong-Hoon Kim - stoch_output_Cholera_novacc_20210902.csv.xz",
              "Jong-Hoon Kim - stoch_output_Cholera_campaign_20210902.csv.xz"),
    cert = "Jong-Hoon Kim - cert89",
    index_start = NA,
    index_end = NA,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    lines = lines))

  #############################################################################

  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "IVI-Kim",
    disease = "Typhoid",
    touchstone = "201910gavi-5",
    scenarios = c("typhoid-no-vaccination", "typhoid-campaign-default", "typhoid-routine-default"),
    in_path = file.path(in_path, "IVI-Kim-Typhoid"),
    files = c("Jong-Hoon Kim - stoch_Typhoid_novacc.csv.xz",
              "Jong-Hoon Kim - stoch_Typhoid_campaign.csv.xz",
              "Jong-Hoon Kim - stoch_Typhoid_routine.csv.xz"),
    cert = "Jong-Hoon Kim - cert90",
    index_start = NA,
    index_end = NA,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    lines = lines))

  #############################################################################

  stub <- "Amy Winter - stochastic_burden_est-"
  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "JHU-Lessler",
    disease = "Rubella",
    touchstone = "201910gavi-5",
    scenarios = c("rubella-campaign-bestcase",
                  "rubella-campaign-default",
                  "rubella-routine-no-vaccination",
                  "rubella-rcv1-bestcase",
                  "rubella-rcv1-default",
                  "rubella-rcv1-rcv2-bestcase",
                  "rubella-rcv1-rcv2-default",
                  "rubella-rcv2-bestcase",
                  "rubella-rcv2-default",
                  "rubella-stop"),
    in_path = file.path(in_path, "JHU-Lessler"),
    files = c(rep(paste0(stub, ":scenario_:index.csv.xz"), 2),
              paste0(stub, "rubella-no-vaccination_:index.csv.xz"),
              rep(paste0(stub, ":scenario_:index.csv.xz"), 7)),
    cert = "Amy Winter - cert70",
    index_start = 1, index_end = 12, out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    deaths = "rubella_deaths_congenital",
    cases = "rubella_cases_congenital",
    dalys = "dalys",
    lines = lines))

  #############################################################################

  stub <- "Michael Jackson - stochastic_burden_est_MenA_KPWA_"
  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "KPW-Jackson",
    disease = "MenA",
    touchstone = "201910gavi-5",
    sceanrios = c("mena-routine-bestcase",
                  "mena-routine-default",
                  "mena-campaign-bestcase",
                  "mena-campaign-default",
                  "mena-no-vaccination"),
    in_path = file.path(in_path, "KPW-Jackson"),
    files = c(paste0(stub, "both_bestcase_:index.csv.xz"),
              paste0(stub, "both_default_:index.csv.xz"),
              paste0(stub, "campaign_bestcase_:index.csv.xz"),
              paste0(stub, "campaign_default_:index.csv.xz"),
              paste0(stub, "none_default_:index.csv.xz")),
    cert = "cert61",
    index_start = 1,
    index_end = 26,
    out_path = out_path,
    lines = lines))

  #############################################################################

  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "JHU-Lee",
    disease = "Cholera",
    touchstone = "201910gavi-5",
    scenarios = c("cholera-no-vaccination", "cholera-campaign-default"),
    in_path = file.path(in_path, "JHU-Lee"),
    files = c("Kaiyue Zou - stochastic-burden-template.201910gavi-5.Cholera_no-vaccination.csv.xz",
              "Kaiyue Zou - stochastic-burden-template.201910gavi-5.Cholera_campaign-default.csv.xz"),
    cert = "",
    index_start = NA,
    index_end = NA,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    bypass_cert_check = TRUE,
    lines = lines))

  #############################################################################

  list_params_hib_pcv <- data_frame(
    outcome = c("cases_men", "cases_men", "cases_men", "cases_men", "cases_men",
                "cases_pneumo", "cases_pneumo", "deaths_men", "deaths_pneumo"),
    proportion = c(1, 0.014, 0.045, 0.021, 0.017, 1, 0.06, 1, 1),
    average_duration = c(0.04,1000,1000,1000,1000,0.02,1000,1000,1000),
    disability_weight = c(0.133, 0.043, 0.027, 0.552, 0.61, 0.051, 0.019, 1, 1)
  )

  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "JHU-Tam",
    disease = "Hib",
    touchstone = "201910gavi-5",
    scenarios = c("hib-no-vaccination-LiST", "hib-routine-default-LiST", "hib-routine-bestcase-LiST"),
    in_path = file.path(in_path, "JHU-Tam-Hib"),
    files = c("novac:index.csv.xz", "default:index.csv.xz", "best:index.csv.xz"),
    cert = "",
    index_start = 1,
    index_end = 14,
    out_path = out_path,
    deaths = c("deaths_men", "deaths_pneumo"),
    cases = c("cases_men", "cases_pneumo"),
    dalys = list_params_hib_pcv,
    bypass_cert_check = TRUE,
    lines = lines))

  # And to add DALYs to the existing

  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "JHU-Tam",
    disease = "PCV",
    touchstone = "201910gavi-5",
    scenarios = c("pcv-no-vaccination-LiST", "pcv-routine-default-LiST", "pcv-routine-bestcase-LiST"),
    in_path = file.path(in_path, "JHU-Tam-PCV"),
    files = c("novac:index.csv.xz", "default:index.csv.xz", "best:index.csv.xz"),
    cert = "",
    index_start = 1,
    index_end = 14,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    deaths = c("deaths_men", "deaths_pneumo"),
    cases = c("cases_men", "cases_pneumo"),
    dalys = list_params_hib_pcv,
    bypass_cert_check = TRUE,
    lines = lines))

  list_params_rota <- data_frame(
    outcome = c("cases", "deaths"),
    proportion = c(1, 1),
    average_duration = c(0.01, 1000),
    disability_weight = c(0.247, 1)
  )

  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "JHU-Tam",
    disease = "Rota",
    touchstone = "201910gavi-5",
    scenarios = c("rota-no-vaccination-LiST", "rota-routine-default-LiST", "rota-routine-bestcase-LiST"),
    in_path = file.path(in_path, "JHU-Tam-Rota"),
    files = c("novac:index.csv.xz", "default:index.csv.xz", "best:index.csv.xz"),
    cert  = "",
    index_start = 1,
    index_end = 14,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    dalys = list_params_rota,
    bypass_cert_check = TRUE,
    lines = lines))

  #############################################################################

  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "Li",
    disease = "HepB",
    touchstone = "201910gavi-5",
    scenarios = c("hepb-bd-default-hepb-routine-default",
                  "hepb-bd-routine-bestcase-hepb-routine-bestcase",
                  "hepb-no-vaccination",
                  "hepb-stop",
                  "hepb-bd-routine-bestcase",
                  "hepb-bd-routine-default",
                  "hepb-hepb-routine-bestcase",
                  "hepb-hepb-routine-default"
    ),
    in_path = file.path(in_path, "Li"),
    files = paste0(":scenario:index.csv.xz"),
    cert = "cert74",
    index_start = 1,
    index_end = 200,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    deaths = c("hepb_deaths_acute", "hepb_deaths_total_cirrh", "hepb_deaths_hcc"),
    cases = c("hepb_cases_acute_symp", "hepb_cases_fulminant",
              "hepb_cases_chronic", "hepb_chronic_symptomatic_in_acute_phase"),
    dalys = "dalys",
    lines = lines))

    #############################################################################

  stub <- "VIMC_Hib_PSA_"
  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "LSHTM-Clark",
    disease = "Hib",
    touchstone = "201910gavi-5",
    scenarios = c("hib-no-vaccination","hib-routine-bestcase","hib-routine-default"),
    in_path = file.path(in_path, "LSHTM-Clark_Hib"),
    files = c(paste0(stub, "NoVax.csv.xz"),
              paste0(stub, "Best.csv.xz"),
              paste0(stub, "Default.csv.xz")),
    cert = "cert81",
    index_start = NA,
    index_end = NA,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    lines = lines))

  stub <- "VIMC_Sp_PSA_"
  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "LSHTM-Clark",
    disease = "PCV",
    touchstone = "201910gavi-5",
    scenarios = c("pcv-no-vaccination","pcv-routine-bestcase","pcv-routine-default"),
    in_path = file.path(in_path, "LSHTM-Clark_PCV"),
    files = c(paste0(stub, "NoVax.csv.xz"),
      paste0(stub, "Best.csv.xz"),
      paste0(stub, "Default.csv.xz")),
    cert = "cert82",
    index_start = NA,
    index_end = NA,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    lines = lines))

  stub <- "Hira Tanvir - VIMC_Rota_PSA_"
  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "LSHTM-Clark",
    disease = "Rota",
    touchstone = "201910gavi-5",
    scenarios = c("rota-no-vaccination","rota-routine-bestcase","rota-routine-default"),
    in_path = file.path(in_path, "LSHTM-Clark_Rota"),
    files = c(paste0(stub, "NoVax.csv.xz"),
              paste0(stub, "Best.csv.xz"),
              paste0(stub, "Default.csv.xz")),
    cert = "",
    index_start = NA,
    index_end = NA,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    lines = lines))

  #############################################################################

  stub <- "stochastic-burden-"
  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "LSHTM-Jit",
    disease = "HPV",
    touchstone = "201910gavi-5",
    scenarios = c("hpv-campaign-bestcase",
                  "hpv-campaign-default",
                  "hpv-no-vaccination",
                  "hpv-routine-bestcase",
                  "hpv-routine-default"),
    in_path = file.path(in_path, "LSHTM-Jit_HPV"),
    files = c(paste0(stub, "vaccination_201910gavi-4_hpv-campaign-bestcase.csv.xz"),
              paste0(stub, "vaccination_201910gavi-4_hpv-campaign-default.csv.xz"),
              paste0(stub, "novaccination_201910gavi-4_hpv-no-vaccination.csv.xz"),
              paste0(stub, "vaccination_201910gavi-4_hpv-routine-bestcase.csv.xz"),
              paste0(stub, "vaccination_201910gavi-4_hpv-routine-default.csv.xz")),
    cert = "Kaja Abbas - stochastic_parameters_certificate_HPV_LSHTM-Jit_201910gavi-4",
    index_start = NA,
    index_end = NA,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    bypass_cert_check = TRUE,
    lines = lines))

  #############################################################################

  stub <- "stochastic_burden_estimate_measles-LSHTM-Jit-"
  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "LSHTM-Jit",
    disease = "Measles",
    touchstone = "201910gavi-5",
    scenarios = c("measles-no-vaccination",
                  "measles-campaign-default","measles-campaign-bestcase",
                  "measles-campaign-only-default","measles-campaign-only-bestcase",
                  "measles-mcv1-default","measles-mcv1-bestcase",
                  "measles-mcv2-default","measles-mcv2-bestcase",
                  "measles-stop"),
    in_path = file.path(in_path, "LSHTM-Jit_Measles"),
    files = c(paste0(stub, "no-vaccination_Portnoy.csv.xz"),
              paste0(stub, "campaign-default_Portnoy.csv.xz"), paste0(stub, "campaign-bestcase_Portnoy.csv.xz"),
              paste0(stub, "campaign-only-default_Portnoy.csv.xz"), paste0(stub, "campaign-only-bestcase_Portnoy.csv.xz"),
              paste0(stub, "mcv1-default_Portnoy.csv.xz"), paste0(stub, "mcv1-bestcase_Portnoy.csv.xz"),
              paste0(stub, "mcv2-default_Portnoy.csv.xz"), paste0(stub, "mcv2-bestcase_Portnoy.csv.xz"),
              paste0(stub, "stop_Portnoy.csv.xz")),
    cert = "cert83",
    index_start = NA,
    index_end = NA,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    lines = lines))

  #############################################################################

  stub <- "Template_Stochastic_"
  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "OUCRU-Clapham",
    disease = "JE",
    touchstone = "201910gavi-5",
    scenarios = c("je-campaign-bestcase",
                  "je-campaign-default",
                  "je-routine-no-vaccination",
                  "je-routine-bestcase",
                  "je-routine-default"),
    in_path = file.path(in_path, "OUCRU-Clapham"),
    files = c(paste0(stub, "Campaign_Best4_correcting_:index.csv.xz"),
              paste0(stub, "Campaign_Default4_correcting_:index.csv.xz"),
              paste0(stub, "Naive4_correcting_:index.csv.xz"),
              paste0(stub, "Routine_Best4_correcting_:index.csv.xz"),
              paste0(stub, "Routine_Default4_correcting_:index.csv.xz")),
    cert = "cert76",
    index_start = 1,
    index_end = 200,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    lines = lines))

  #############################################################################

  stub <- "stochastic_burden_est_"
  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "PHE-Vynnycky",
    disease = "Rubella",
    touchstone = "201910gavi-5",
    scenarios = c("rubella-campaign-bestcase",
                  "rubella-campaign-default",
                  "rubella-routine-no-vaccination",
                  "rubella-rcv1-bestcase",
                  "rubella-rcv1-default",
                  "rubella-rcv1-rcv2-bestcase",
                  "rubella-rcv1-rcv2-default",
                  "rubella-rcv2-bestcase",
                  "rubella-rcv2-default",
                  "rubella-stop"),
    in_path = file.path(in_path, "PHE-Vynnycky"),
    files = paste0(stub, ":scenario_country:index.csv.xz"),
    cert = "", index_start = 1,
    index_end = 112,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    deaths = "rubella_deaths_congenital",
    cases = "rubella_cases_congenital",
    dalys = "dalys",
    lines = lines))

  #############################################################################

  stub <- "Heather Santos - "
  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "PSU-Ferrari",
    disease = "Measles",
    touchstone = "201910gavi-5",
    scenarios = c("measles-no-vaccination",
                  "measles-mcv1-bestcase",
                  "measles-mcv2-bestcase",
                  "measles-campaign-bestcase",
                  "measles-mcv1-default",
                  "measles-mcv2-default",
                  "measles-campaign-default",
                  "measles-stop",
                  "measles-campaign-only-bestcase",
                  "measles-campaign-only-default"),
    in_path = file.path(in_path, "PSU-Ferrari"),
    files = c(paste0(stub, "novax_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz"),
              paste0(stub, "bestcase_mcv1_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz"),
              paste0(stub, "bestcase_mcv2_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz"),
              "stochastic:index_burden_Measles-PSU-Ferrari.csv.xz",
              paste0(stub, "default_mcv1_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz"),
              paste0(stub, "default_mcv2_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz"),
              paste0(stub, "default_campaign_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz"),
              paste0(stub, "stop_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz"),
              paste0(stub, "bestcase_campaign_only_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz"),
              paste0(stub, "default_campaign_only_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz")),
    cert = "Heather Santos - cert80",
    index_start = 1,
    index_end = 8,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    lines = lines))

  #############################################################################

  stub <- "Sean Moore - stochastic_burden_est_JE_UND-Moore_"
  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "UND-Moore",
    disease = "JE",
    touchstone = "201910gavi-5",
    scenarios = c("je-campaign-bestcase",
                  "je-campaign-default",
                  "je-routine-no-vaccination",
                  "je-routine-bestcase",
                  "je-routine-default"),
    in_path = file.path(in_path, "UND-Moore"),
    files = c(paste0(stub, ":scenario.csv.xz"),
              paste0(stub, ":scenario.csv.xz"),
              paste0(stub, "je-no-vaccination.csv.xz"),
              paste0(stub, ":scenario.csv.xz"),
              paste0(stub, ":scenario.csv.xz")),
    cert = "Sean Moore - cert58",
    index_start = NA,
    index_end = NA,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    lines = lines))

  #############################################################################

  stub <- "stochastic_burden_est_YF_UND-Perkins_"
  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "UND-Perkins",
    disease = "YF",
    touchstone = "201910gavi-5",
    scenarios = c("yf-no-vaccination",
                  "yf-preventive-bestcase",
                  "yf-preventive-default",
                  "yf-routine-bestcase",
                  "yf-routine-default",
                  "yf-stop"),
    in_path = file.path(in_path, "UND-Perkins"),
    files = paste0(stub, ":scenario_:index.csv.xz"),
    cert = "John Huber - cert85",
    index_start = 1,
    index_end = 200,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    lines = lines))

  #############################################################################

  continue_on_error(stone_stochastic_process(
    con,
    modelling_group = "Yale-Pitzer",
    disease = "Typhoid",
    touchstone = "201910gavi-5",
    scenarios = c("typhoid-no-vaccination", "typhoid-campaign-default", "typhoid-routine-default"),
    in_path = file.path(in_path, "Yale-Pitzer"),
    files = c("Virginia Pitzer - 2021-02-18 17.00.26 - stochastic_output_TF-Yale-Pitzer_novacc.csv.xz",
              "Virginia Pitzer - 2021-02-18 16.58.03 - stochastic_output_TF-Yale-Pitzer_campaign.csv.xz",
              "Virginia Pitzer - 2021-02-18 16.59.14 - stochastic_output_TF-Yale-Pitzer_camproutine.csv.xz"),
    cert = "Virginia Pitzer - cert88",
    index_start = NA,
    index_end = NA,
    out_path = out_path,
    pre_aggregation_path = pre_aggregation_path,
    log_file = log_file,
    lines = lines))
}

library(stoner)
## Change dettl root to local (just using dettl to get a db connection to prod)
dettl_root <- "~/projects/montagu-imports/"
con <- dettl:::db_connect("production", dettl_root)

do_stochastics_2021(con, test_run = TRUE)

