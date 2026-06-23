get_con <- function() {
  vault <- vaultr::vault_client(login = "github")
  password <- vault$read("/secret/vimc/database/production/users/readonly")$password
  DBI::dbConnect(RPostgres::Postgres(),
                 dbname = "montagu",
                 host = "montagu.vaccineimpact.org",
                 port = 5432, password = password,
                 user = "readonly")
}

get_annex <- function() {
  vault <- vaultr::vault_client(login = "github")
  password <- vault$read("/secret/vimc/annex/users/vimc")$password
  DBI::dbConnect(RPostgres::Postgres(),
                 dbname = "montagu",
                 host = "annex.montagu.dide.ic.ac.uk",
                 port = 15432,
                 password = password,
                 user = "vimc")

}


upload4 <- function(out_path, modelling_group, disease, touchstone,
                    con, annex) {
  stub <- file.path(out_path, sprintf("%s_%s_", modelling_group, disease))

  stoner::stone_stochastic_upload(
    paste0(stub, "calendar.qs"),
    con, annex, modelling_group, disease, touchstone,
    is_cohort = FALSE, is_under5 = FALSE)

  stoner::stone_stochastic_upload(
    paste0(stub, "calendar_u5.qs"),
    con, annex, modelling_group, disease, touchstone,
    is_cohort = FALSE, is_under5 = TRUE)

  stoner::stone_stochastic_upload(
    paste0(stub, "cohort.qs"),
    con, annex, modelling_group, disease, touchstone,
    is_cohort = TRUE, is_under5 = FALSE)

  stoner::stone_stochastic_upload(
    paste0(stub, "cohort_u5.qs"),
    con, annex, modelling_group, disease, touchstone,
    is_cohort = TRUE, is_under5 = TRUE)
}

do_stochastics_2024 <- function() {
  con <- get_con()
  annex <- get_annex()
  in_path <- "D:/Dropbox (SPH Imperial College)/File requests/latest/202409malaria-1/"
  out_path <- "D:/stochastic_2024/"

  # UAC-Kakai

  stoner::stone_stochastic_process(con, "UAC-Glele_Kakai", "Malaria", "202409malaria-1",
    c("malaria-no-vaccination",
      "malaria-r3-r4-default", "malaria-rts3-rts4-default"),
    file.path(in_path, "Malaria-UAC-Kakai"),
     c("Stochastic_Burden_Estimates_Glele_Kakai_No_Vaccine_:index.csv.xz",
       "Stochastic_Burden_Estimates_Glele_Kakai_Default_r34_:index.csv.xz",
       "Stochastic_Burden_Estimates_Glele_Kakai_Default_rts34_:index.csv.xz"),
       "", 1, 200, out_path, bypass_cert_check = TRUE)

  upload4(out_path, "UAC-Glele_Kakai", "Malaria", "202409malaria-1", con, annex)

  # IC-Okell
  in_path <- "D:/Dropbox (SPH Imperial College)/File requests/latest/202409malaria-1/Malaria-IC-Okell/"
  out_path <- "D:/stochastic_2024/"
  stoner::stone_stochastic_process(con,
    "IC-Okell", "Malaria", "202409malaria-1",
  c("malaria-no-vaccination",
    "malaria-r3-r4-default", "malaria-rts3-rts4-default"),
   in_path,
   c("stochastic-burden-est-no-vaccination_:index Lydia Haile.csv.xz",
     "stochastic-burden-est-malaria-r3-r4-default_:index Lydia Haile.csv.xz",
     "stochastic-burden-est-malaria-rts3-rts4-default_:index Lydia Haile.csv.xz"),
          "", 1, 200, out_path, bypass_cert_check = TRUE)

  upload4(out_path, "IC-Okell", "Malaria", "202409malaria-1", con, annex)

  # TKI-Penny

  in_path <- "D:/Dropbox (SPH Imperial College)/File requests/latest/202409malaria-1/Malaria-TKI-Penny/"

  stoner::stone_stochastic_process(con,
    "TKI-Penny", "Malaria", "202409malaria-1",
    c("malaria-no-vaccination",
      "malaria-r3-r4-default",
      "malaria-rts3-rts4-default"),
      in_path,
    c("stochastic_burden_est_malaria_:index_novaccine Josephine Malinga.csv.xz",
      "stochastic_burden_est_malaria_:index_r21_d4_default Josephine Maasd   linga.csv.xz",
      "stochastic_burden_est_malaria_:index_rtss_d4_default Josephine asd fgMalinga.csv.xz"),
      "", 1, 31, out_path, bypass_cert_check = TRUE)

  upload4(out_path, "TKI-Penny", "Malaria", "202409malaria-1", con, annex)

}


do_stochastics_2023 <- function() {
  con <- get_con()
  annex <- get_annex()
  in_path <- "D:/Dropbox (SPH Imperial College)/File requests/latest/202310gavi/"
  out_path <- "D:/stochastic_2023/"

  ###################### HepB #########################################

  stoner::stone_stochastic_process(con, "Li", "HepB", "202310gavi-4",
                                   c(  "hepb-hepb3-bd-bluesky",
                                       "hepb-hepb3-bd-default",
                                       "hepb-hepb3-bd-ia2030",
                                       "hepb-hepb3-bluesky",
                                       "hepb-hepb3-default",
                                       "hepb-hepb3-ia2030",
                                       "hepb-no-vaccination"
                                   ),
                                   file.path(in_path, "HepB-Li"),
                                   paste0(":scenario:index.csv.xz"),
                                   "cert156", 1, 200, out_path, NULL,
                                   "deaths", "cases", "dalys")

  upload4(out_path, "Li", "HepB", "202310gavi-4", con, annex)

  stoner::stone_stochastic_process(
    con, "IC-Hallett", "HepB", "202310gavi-4",
    c(  "hepb-hepb3-bd-bluesky",
        "hepb-hepb3-bd-default",
        "hepb-hepb3-bd-ia2030",
        "hepb-hepb3-bluesky",
        "hepb-hepb3-default",
        "hepb-hepb3-ia2030",
        "hepb-no-vaccination"),
    file.path(in_path, "HepB-IC-Hallett"),
    c("stochastic_burden_est_HepB-IC-Hallett_hepb_hepb3_bd_bluesky_:index.csv.xz",
      "stochastic_burden_est_HepB-IC-Hallett_hepb_hepb3_bd_default_:index.csv.xz",
      "stochastic_burden_est_HepB-IC-Hallett_hepb_hepb3_bd_ia2030_:index.csv.xz",
      "stochastic_burden_est_HepB-IC-Hallett_hepb_hepb3_bluesky_:index.csv.xz",
      "stochastic_burden_est_HepB-IC-Hallett_hepb_hepb3_default_:index.csv.xz",
      "stochastic_burden_est_HepB-IC-Hallett_hepb_hepb3_ia2030_:index.csv.xz",
      "stochastic_burden_est_HepB-IC-Hallett_hepb_no_vaccination_:index.csv.xz"),
    "", 1, 200, out_path, NULL,
    "deaths", "cases", "dalys", bypass_cert_check = TRUE )

  upload4(out_path, "IC-Hallett", "HepB", "202310gavi-4", con, annex)


  stoner::stone_stochastic_process(
    con, "IC-Hallett", "HepB", "202310gavi-4",
    c(  "hepb-hepb3-bd-bluesky",
        "hepb-hepb3-bd-default",
        "hepb-hepb3-bd-ia2030",
        "hepb-hepb3-bluesky",
        "hepb-hepb3-default",
        "hepb-hepb3-ia2030",
        "hepb-no-vaccination"),
    file.path(in_path, "HepB-IC-Hallett"),
    c("stochastic_burden_est_HepB-IC-Hallett_hepb_hepb3_bd_bluesky_:index.csv.xz",
      "stochastic_burden_est_HepB-IC-Hallett_hepb_hepb3_bd_default_:index.csv.xz",
      "stochastic_burden_est_HepB-IC-Hallett_hepb_hepb3_bd_ia2030_:index.csv.xz",
      "stochastic_burden_est_HepB-IC-Hallett_hepb_hepb3_bluesky_:index.csv.xz",
      "stochastic_burden_est_HepB-IC-Hallett_hepb_hepb3_default_:index.csv.xz",
      "stochastic_burden_est_HepB-IC-Hallett_hepb_hepb3_ia2030_:index.csv.xz",
      "stochastic_burden_est_HepB-IC-Hallett_hepb_no_vaccination_:index.csv.xz"),
    "", 1, 200, out_path, NULL,
    "deaths", "cases", "dalys", bypass_cert_check = TRUE )

  upload4(out_path, "IC-Hallett", "HepB", "202310gavi-4", con, annex)



  ###################### YF #########################################

  in_path <- "//fi--didenas1-app/Test/YF"
  out_path <- in_path

  stoner::stone_stochastic_process(
    con, "IC-Garske", "YF", "202310gavi-4",
    c("yf-no-vaccination",
      "yf-routine-bluesky",
      "yf-routine-campaign-bluesky",
      "yf-routine-campaign-default",
      "yf-routine-campaign-ia2030",
      "yf-routine-default",
      "yf-routine-ia2030"),
    in_path,
    "burden_results_stochastic_202310gavi-3_:scenario Keith Fraser.csv.xz",
    "", NA, NA, out_path, outcomes = list(cases = "cases", deaths = "deaths",
                                          dalys = "dalys", yll = "yll"), bypass_cert_check = TRUE )

  upload4(out_path, "IC-Garske", "YF", "202310gavi-4", con, annex)


  stoner::stone_stochastic_process(
    con, "UND-Perkins", "YF", "202310gavi-7",
    c("yf-no-vaccination",
      "yf-routine-bluesky",
      "yf-routine-campaign-bluesky",
      "yf-routine-campaign-default",
      "yf-routine-campaign-ia2030",
      "yf-routine-default",
      "yf-routine-ia2030"),
    file.path(in_path, "YF-UND-Perkins"),
    c("stochastic_burden_est_YF_UND-Perkins_yf-no_vaccination_:index.csv.xz",
      "stochastic_burden_est_YF_UND-Perkins_yf-routine_bluesky_:index.csv.xz",
      "stochastic_burden_est_YF_UND-Perkins_yf-routine_campaign_bluesky_:index.csv.xz",
      "stochastic_burden_est_YF_UND-Perkins_yf-routine_campaign_default_:index.csv.xz",
      "stochastic_burden_est_YF_UND-Perkins_yf-routine_campaign_ia2030_:index.csv.xz",
      "stochastic_burden_est_YF_UND-Perkins_yf-routine_default_:index.csv.xz",
      "stochastic_burden_est_YF_UND-Perkins_yf-routine_ia2030_:index.csv.xz"),

    "", 1, 200, out_path, outcomes = list(cases = "cases", deaths = "deaths",
                                          dalys = "dalys", yll = "yll"), bypass_cert_check = TRUE )

  upload4(out_path, "UND-Perkins", "YF", "202310gavi-7", con, annex)



  ###################### HPV #########################################

  stoner::stone_stochastic_process(
    con, "Harvard-Sweet", "HPV", "202310gavi-4",
    c("hpv-no-vaccination",
      "hpv-campaign-default",
      "hpv-campaign-bluesky",
      "hpv-campaign-routine-bluesky",
      "hpv-campaign-routine-default",
      "hpv-campaign-default_transition_hpv_1d",
      "hpv-campaign-routine-default_transition_hpv_1d",
      "hpv-campaign-ia2030",
      "hpv-campaign-routine-ia2030"),
    file.path(in_path, ""),
    c("stochastic-burden-est.coverage_202310gavi-4_:scenario_run_:index Allison Portnoy.csv.xz"),
    "", 1, 200, out_path, NULL,
    "deaths", "cases", "dalys", "yll", bypass_cert_check = TRUE )

  upload4(out_path, "Harvard-Sweet", "HPV", "202310gavi-4", con, annex)

  in_path <- "//fi--didenas1-app/Test/hpv-jit"
  stoner::stone_stochastic_process(
    con, "LSHTM-Jit", "HPV", "202310gavi-7",
    c("hpv-no-vaccination",
      "hpv-campaign-default",
      "hpv-campaign-bluesky",
      "hpv-campaign-routine-bluesky",
      "hpv-campaign-routine-default",
      "hpv-campaign-default_transition_hpv_1d",
      "hpv-campaign-routine-default_transition_hpv_1d",
      "hpv-campaign-ia2030",
      "hpv-campaign-routine-ia2030"),
    file.path(in_path, ""),
    c("stochastic-burden-novaccination_all_202310gavi-7_hpv-no-vaccination Kaja Abbas.csv.xz",
      "stochastic-burden-vaccination_all_202310gavi-7_hpv-campaign-default Kaja Abbas.csv.xz",
      "stochastic-burden-vaccination_all_202310gavi-7_hpv-campaign-bluesky Kaja Abbas.csv.xz",
      "stochastic-burden-vaccination_all_202310gavi-7_hpv-campaign-routine-bluesky Kaja Abbas.csv.xz",
      "stochastic-burden-vaccination_all_202310gavi-7_hpv-campaign-routine-default Kaja Abbas.csv.xz",
      "stochastic-burden-vaccination_all_202310gavi-7_hpv-campaign-default_transition_hpv_1d Kaja Abbas.csv.xz",
      "stochastic-burden-vaccination_all_202310gavi-7_hpv-campaign-routine-default_transition_hpv_1d Kaja Abbas.csv.xz",
      "stochastic-burden-vaccination_all_202310gavi-7_hpv-campaign-ia2030 Kaja Abbas.csv.xz",
      "stochastic-burden-vaccination_all_202310gavi-7_hpv-campaign-routine-ia2030 Kaja Abbas.csv.xz"),
    "", NA, NA, in_path, NULL,
    "deaths", "cases", "dalys", "yll", bypass_cert_check = TRUE )

  upload4(in_path, "LSHTM-Jit", "HPV", "202310gavi-7", con, annex)


  ###################### Cholera #########################################

  in_path <- "D:/Dropbox (SPH Imperial College)/File requests/latest/202310gavi/Cholera-IVI-Kim"
  out_path <- "D:/stochastic_2023/"
  stoner::stone_stochastic_process(con,
                                   "IVI-Kim", "Cholera", "202310gavi-7",
                                   c("cholera-no-vaccination", "cholera-ocv1-default", "cholera-ocv1-ocv2-default"),
                                   file.path(in_path),
                                   c("stoch_Cholera_novacc_20250704T130601 Jong-Hoon Kim.csv.xz",
                                     "stoch_Cholera_campaign_default_ocv1_20250704T130601 Jong-Hoon Kim.csv.xz",
                                     "stoch_Cholera_campaign_default_ocv12_20250704T130601 Jong-Hoon Kim.csv.xz"),

                                   "", NA, NA, out_path, NULL, bypass_cert_check = TRUE )
  upload4(out_path, "IVI-Kim", "Cholera", "202310gavi-7", con, annex)


  in_path <- "K:/jhu-chol"
  stoner::stone_stochastic_process(con,
                                   "JHU-Lee", "Cholera", "202310gavi-7",
                                   c("cholera-no-vaccination", "cholera-ocv1-default", "cholera-ocv1-ocv2-default"),
                                   file.path(in_path),
                                   c("stochastic-burden-template.202310gavi-4.Cholera_standard_template.529.no-vaccination Christina Alam.csv.xz",
                                     "stochastic-burden-template.202310gavi-4.Cholera_standard_template.529.ocv1-default_one Christina Alam.csv.xz",
                                     "stochastic-burden-template.202310gavi-4.Cholera_standard_template.529.ocv1-ocv2-default_two Christina Alam.csv.xz"),
                                   "cert172 Christina Alam", NA, NA, out_path, NULL,
                                   "deaths", "cases", "dalys", "yll", bypass_cert_check = TRUE )
  upload4(out_path, "JHU-Lee", "Cholera", "202310gavi-4", con, annex)


  ###################### COVID #########################################

  in_path <- "//fi--didenas1-app/Test/covid-liu"
  stoner::stone_stochastic_process(con,
                                   "LSHTM-Liu", "COVID", "202310gavi-7",
                                   c("covid-no-vaccination",
                                     "covid-no-vaccination_severe",
                                     "covid-primary-bluesky",
                                     "covid-primary-bluesky_severe",
                                     "covid-primary-booster-bluesky",
                                     "covid-primary-booster-bluesky_severe",
                                     "covid-primary-booster-default",
                                     "covid-primary-booster-default_severe",
                                     "covid-primary-default",
                                     "covid-primary-default_severe"),
                                   in_path,
                                   c("stochastic_burden_est_covid-LSHTM-Liu_covid-no-vaccination Yang Liu.csv.xz",
                                     "stochastic_burden_est_covid-LSHTM-Liu_covid-no-vaccination_severe Yang Liu.csv.xz",
                                     "stochastic_burden_est_covid-LSHTM-Liu_covid-primary-bluesky Yang Liu.csv.xz",
                                     "stochastic_burden_est_covid-LSHTM-Liu_covid-primary-bluesky_severe Yang Liu.csv.xz",
                                     "stochastic_burden_est_covid-LSHTM-Liu_covid-primary-booster-bluesky Yang Liu.csv.xz",
                                     "stochastic_burden_est_covid-LSHTM-Liu_covid-primary-booster-bluesky_severe Yang Liu.csv.xz",
                                     "stochastic_burden_est_covid-LSHTM-Liu_covid-primary-booster-default Yang Liu.csv.xz",
                                     "stochastic_burden_est_covid-LSHTM-Liu_covid-primary-booster-default_severe Yang Liu.csv.xz",
                                     "stochastic_burden_est_covid-LSHTM-Liu_covid-primary-default Yang Liu.csv.xz",
                                     "stochastic_burden_est_covid-LSHTM-Liu_covid-primary-default_severe Yang Liu.csv.xz"),
                                   "", NA, NA, in_path, NULL, "deaths", "cases", "dalys", "yll", bypass_cert_check = TRUE)
  upload4(in_path, "LSHTM-Liu", "COVID", "202310gavi-7", con, annex)


  in_path <- "//fi--didenas1-app/Test/cov"
  stoner::stone_stochastic_process(con,
                                   "IC-Ghani", "COVID", "202310gavi-7",
                                   c("covid-no-vaccination",
                                     "covid-no-vaccination_severe",
                                     "covid-primary-bluesky",
                                     "covid-primary-bluesky_severe",
                                     "covid-primary-booster-bluesky",
                                     "covid-primary-booster-bluesky_severe",
                                     "covid-primary-booster-default",
                                     "covid-primary-booster-default_severe",
                                     "covid-primary-default",
                                     "covid-primary-default_severe"),
                                   in_path,
                                   c("covid-no-vaccination Gemma Gilani_:index.csv.xz",
                                     "covid-no-vaccination_severe Gemma Gilani_:index.csv.xz",
                                     "covid-primary-bluesky Daniela Olivera_:index.csv.xz",
                                     "covid-primary-bluesky_severe Daniela Olivera_:index.csv.xz",
                                     "covid-primary-booster-bluesky Daniela Olivera_:index.csv.xz",
                                     "covid-primary-booster-bluesky_severe Daniela Olivera_:index.csv.xz",
                                     "covid-primary-booster-default Gemma Gilani_:index.csv.xz",
                                     "covid-primary-booster-default_severe Gemma Gilani_:index.csv.xz",
                                     "covid-primary-default Gemma Gilani_:index.csv.xz",
                                     "covid-primary-default_severe Gemma Gilani_:index.csv.xz"),
                                   "", 1, 200, in_path, NULL, "deaths", "cases", "dalys", "yll", bypass_cert_check = TRUE)
  upload4(in_path, "IC-Ghani", "COVID", "202310gavi-7", con, annex)




  ###################### Measles #########################################

  # Updated June 2025

  stoner::stone_stochastic_process(
    con,
    "PSU-Ferrari", "Measles", "202310gavi-7",
    c("measles-no-vaccination",
      "measles-mcv1-bluesky",
      "measles-mcv1-default",
      "measles-mcv1-ia2030",
      "measles-mcv1-mcv2-bluesky",
      "measles-mcv1-mcv2-campaign-bluesky",
      "measles-mcv1-mcv2-campaign-default",
      "measles-mcv1-mcv2-campaign-default_under5sia",
      "measles-mcv1-mcv2-campaign-default_update",
      "measles-mcv1-mcv2-campaign-ia2030",
      "measles-mcv1-mcv2-default",
      "measles-mcv1-mcv2-ia2030"),
    file.path(in_path),
    c("no-vaccination.202310gavi-6.Measles_PSU-Ferrari_standard.csv.xz",
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
      "mcv1-mcv2-ia2030.202310gavi-6.Measles_PSU-Ferrari_standard.csv.xz"),
      "", NA, NA, out_path, bypass_cert_check = TRUE)

  upload4(out_path, "PSU-Ferrari", "Measles", "202310gavi-7", con, annex)


  stoner::stone_stochastic_process(
    con,
    "LSHTM-Jit", "Measles", "202310gavi-7",
    c("measles-no-vaccination",
      "measles-mcv1-bluesky",
      "measles-mcv1-default",
      "measles-mcv1-ia2030",
      "measles-mcv1-mcv2-bluesky",
      "measles-mcv1-mcv2-campaign-bluesky",
      "measles-mcv1-mcv2-campaign-default",
      "measles-mcv1-mcv2-campaign-default_under5sia",
      "measles-mcv1-mcv2-campaign-default_update",
      "measles-mcv1-mcv2-campaign-ia2030",
      "measles-mcv1-mcv2-default",
      "measles-mcv1-mcv2-ia2030"),
    in_path,
    c("stochastic_burden_estimate_measles-LSHTM-Jit-no-vaccination Han Fu.csv.xz",
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
      "stochastic_burden_estimate_measles-LSHTM-Jit-mcv1-mcv2-ia2030 Han Fu.csv.xz"),
    "", NA, NA, out_path, bypass_cert_check = TRUE)
  upload4(out_path, "LSHTM-Jit", "Measles", "202310gavi-7", con, annex)


  ###################### Rubella #########################################

  in_path <- "//fi--didenas1-app/Test/rub-uga"
  stoner::stone_stochastic_process(con,
                                   "JHU-Lessler", "Rubella", "202310gavi-7",
                                   c("rubella-no-vaccination",
                                     "rubella-campaign-default",
                                     "rubella-campaign-bluesky",
                                     "rubella-campaign-rcv1-rcv2-bluesky",
                                     "rubella-campaign-rcv1-bluesky",
                                     "rubella-campaign-rcv1-rcv2-default",
                                     "rubella-campaign-rcv1-default",
                                     "rubella-campaign-ia2030",
                                     "rubella-campaign-rcv1-rcv2-ia2030",
                                     "rubella-campaign-rcv1-ia2030"),
                                   in_path,
                                   c("stochastic_burden_est-rubella-no-vaccination_:index Amy Winter.csv.xz",
                                     "stochastic_burden_est-rubella-campaign-default_:index Amy Winter.csv.xz",
                                     "stochastic_burden_est-rubella-campaign-bluesky_:index Amy Winter.csv.xz",
                                     "stochastic_burden_est-rubella-campaign-rcv1-rcv2-bluesky_:index Amy Winter.csv.xz",
                                     "stochastic_burden_est-rubella-campaign-rcv1-bluesky_:index Amy Winter.csv.xz",
                                     "stochastic_burden_est-rubella-campaign-rcv1-rcv2-default_:index Amy Winter.csv.xz",
                                     "stochastic_burden_est-rubella-campaign-rcv1-default_:index Amy Winter.csv.xz",
                                     "stochastic_burden_est-rubella-campaign-ia2030_:index Amy Winter.csv.xz",
                                     "stochastic_burden_est-rubella-campaign-rcv1-rcv2-ia2030_:index Amy Winter.csv.xz",
                                     "stochastic_burden_est-rubella-campaign-rcv1-ia2030_:index Amy Winter.csv.xz"),
                                   "", 1, 12, in_path, NULL,
                                   "rubella_deaths_congenital",
                                   "rubella_cases_congenital", "dalys", "yll", bypass_cert_check = TRUE)
  upload4(in_path, "JHU-Lessler", "Rubella", "202310gavi-7", con, annex)

  in_path <- "//fi--didenas1-app/Test/rub-ukhsa"
  stoner::stone_stochastic_process(con,
                                   "PHE-Vynnycky", "Rubella", "202310gavi-7",
                                   c("rubella-no-vaccination",
                                     "rubella-campaign-default",
                                     "rubella-campaign-bluesky",
                                     "rubella-campaign-rcv1-rcv2-bluesky",
                                     "rubella-campaign-rcv1-bluesky",
                                     "rubella-campaign-rcv1-rcv2-default",
                                     "rubella-campaign-rcv1-default",
                                     "rubella-campaign-ia2030",
                                     "rubella-campaign-rcv1-rcv2-ia2030",
                                     "rubella-campaign-rcv1-ia2030"),
                                   in_path,
                                   c("imp.c:indexs401.csv.xz",
                                     "stochastic_burden_est_rubella_Vynnycky_rubella-campaign-default_:index.csv.xz",
                                     "stochastic_burden_est_rubella_Vynnycky_rubella-campaign-bluesky_:index.csv.xz",
                                     "Vynnycky-camp-rcv1-rcv2-bluesky_country:index.csv.xz",
                                     "Vynnycky-camp-rcv1-bluesky_country:index.csv.xz",
                                     "stochastic_burden_est_rubella_Vynnycky_rubella-campaign-rcv1-rcv2-default_:index.csv.xz",
                                     "stochastic_burden_est_rubella_Vynnycky_rubella-campaign-rcv1-default_:index.csv.xz",
                                     "stochastic_burden_est_rubella_Vynnycky_rubella-campaign-ia2030_:index.csv.xz",
                                     "Vynnycky-camp-rcv1-rcv2-ia2030_country:index.csv.xz",
                                     "Vynnycky-camp-rcv1-ia2030_country:index.csv.xz"),
                                   "", 1, 117, in_path, NULL,
                                   "rubella_deaths_congenital",
                                   "rubella_cases_congenital", "dalys", "yll", bypass_cert_check = TRUE)
  upload4(in_path, "PHE-Vynnycky", "Rubella", "202310gavi-7", con, annex)



  ###################### Typhoid #########################################

  in_path <- "//fi--didenas1-app/Test/ivi-typhoid"
  stoner::stone_stochastic_process(con, "IVI-Kim", "Typhoid", "202310gavi-7",
                                   c("typhoid-no-vaccination",
                                     "typhoid-campaign-default", "typhoid-campaign-routine-default",
                                     "typhoid-campaign-bluesky",  "typhoid-campaign-routine-bluesky"),
                                   in_path,
                                   c("stoch_Typhoid_novacc_20240314T233526 Jong-Hoon Kim.csv.xz",
                                     "stoch_Typhoid_campaign_default_20240314T233526 Jong-Hoon Kim.csv.xz",
                                     "stoch_Typhoid_campaign_routine_default_20240314T233526 Jong-Hoon Kim.csv.xz",
                                     "stoch_Typhoid_campaign_bluesky_20240314T233526 Jong-Hoon Kim.csv.xz",
                                     "stoch_Typhoid_campaign_routine_bluesky_20240314T233526 Jong-Hoon Kim.csv.xz"),
                                   "", NA, NA, in_path, bypass_cert_check = TRUE)
  upload4(in_path, "IVI-Kim", "Typhoid", "202310gavi-7", con, annex)


  ###################### Malaria R2 #########################################

  in_path <- "//fi--didenas1-app/Test/uac-malaria"
  stoner::stone_stochastic_process(con, "UAC-Glele_Kakai", "Malaria", "202310gavi-7",
                                   c("malaria-no-vaccination",
                                     "malaria-rts3-default", "malaria-rts3-rts4-default",
                                     "malaria-rts3-bluesky", "malaria-rts3-rts4-bluesky"
                                   ),
                                   in_path,
                                   c("Stochastic_Burden_Estimates_Glele_Kakai_No_Vaccine_:index.csv.xz",
                                     "Stochastic_Burden_Estimates_Glele_Kakai_Default_rts3_:index.csv.xz",
                                     "Stochastic_Burden_Estimates_Glele_Kakai_Default_rts3_4_:index.csv.xz",
                                     "stochastic_Burden_Estimates_Glele_Kakai_Blue_Sky_rts3_:index.csv.xz",
                                     "Stochastic_Burden_Estimates_Glele_Kakai_Blue_Sky_rts34_:index.csv.xz"),
                                   "", 1, 200, in_path, bypass_cert_check = TRUE)

  upload4(in_path, "UAC-Glele_Kakai", "Malaria", "202310gavi-7", con, annex)

  in_path <- "//fi--didenas1-app/Test/mal"
  stoner::stone_stochastic_process(con, "IC-Okell", "Malaria", "202310gavi-7",
                                   c("malaria-no-vaccination",
                                     "malaria-rts3-default", "malaria-rts3-rts4-default",
                                     "malaria-rts3-bluesky", "malaria-rts3-rts4-bluesky"
                                   ),
                                   in_path,
                                   c("stochastic-burden-est.202310gavi-7.Malaria_IC-Okell_no-vaccination_draw_:index Lydia Haile.csv.xz",
                                     "stochastic-burden-est.202310gavi-7.Malaria_IC-Okell_malaria-rts3-default_draw_:index Lydia Haile.csv.xz",
                                     "stochastic-burden-est.202310gavi-7.Malaria_IC-Okell_malaria-rts3-rts4-default_draw_:index Lydia Haile.csv.xz",
                                     "stochastic-burden-est.202310gavi-7.Malaria_IC-Okell_malaria-rts3-bluesky_draw_:index Lydia Haile.csv.xz",
                                     "stochastic-burden-est.202310gavi-7.Malaria_IC-Okell_malaria-rts3-rts4-bluesky_draw_:index Lydia Haile.csv.xz"
                                   ),
                                   "", 1, 200, in_path, bypass_cert_check = TRUE)
  upload4(in_path, "IC-Okell", "Malaria", "202310gavi-7", con, annex)



  in_path <- "//fi--didenas1-app/Test/tki"
  stoner::stone_stochastic_process(con, "TKI-Penny", "Malaria", "202310gavi-7",
                                   c("malaria-no-vaccination",
                                     "malaria-rts3-default", "malaria-rts3-rts4-default",
                                     "malaria-rts3-bluesky", "malaria-rts3-rts4-bluesky"
                                   ),
                                   in_path,
                                   c("stochastic_burden_est_malaria_:index_novaccine Josephine Malinga.csv.xz",
                                     "stochastic_burden_est_malaria_:index_rtss_d3_default Josephine Malinga.csv.xz",
                                     "stochastic_burden_est_malaria_:index_rtss_d4_default Josephine Malinga.csv.xz",
                                     "stochastic_burden_est_malaria_:index_rtss_d3_bluesky Josephine Malinga.csv.xz",
                                     "stochastic_burden_est_malaria_:index_rtss_d4_bluesky Josephine Malinga.csv.xz"),
                                   "", 1, 31, in_path, bypass_cert_check = TRUE)
  upload4(in_path, "TKI-Penny", "Malaria", "202310gavi-7", con, annex)


  ################################################################
  # MENA
  # NB - mena-campaign-default and mena-campaign-ia2030 are identical,
  # so same file used below.

  in_path <- "//fi--didenas1-app/Test/MenA"
  out_path <- in_path


  stub <- "stochastic-burden-template202310gavi-7MenA_Cambridge-Trotter_mena-"
  stoner::stone_stochastic_process(con,
                                   "Cambridge-Trotter", "MenA", "202310gavi-7",
                                   c("mena-no-vaccination", "mena-campaign-default", "mena-campaign-routine-default",
                                     "mena-campaign-bluesky", "mena-campaign-routine-bluesky",
                                     "mena-campaign-ia2030", "mena-campaign-routine-ia2030"),
                                   in_path,
                                   c(paste0(stub, "novaccination (:index) Andromachi Karachaliou.csv"),
                                     paste0(stub, "campaign-default (:index) Andromachi Karachaliou.csv"),
                                     paste0(stub, "routine-default (:index) Andromachi Karachaliou.csv"),
                                     paste0(stub, "campaign-bluesky (:index) Andromachi Karachaliou.csv"),
                                     paste0(stub, "routine-bluesky (:index) Andromachi Karachaliou.csv"),
                                     paste0(stub, "campaign-default (:index) Andromachi Karachaliou.csv"),
                                     paste0(stub, "routine-ia2030 (:index) Andromachi Karachaliou.csv")),
                                   "", 1, 26, out_path, outcomes = list(cases = "cases", deaths = "deaths",
                                                                        dalys = "dalys", yll = "yll", cases_cwyx = "cases_cwyx",
                                                                        deaths_cwyx = "deaths_cwyx", dalys_cwyx = "dalys_cwyx", yll_cwyx = "yll_cwyx"),
                                   bypass_cert_check = TRUE)
  upload4(in_path, "Cambridge-Trotter", "MenA", "202310gavi-7", con, annex)

}

do_stochastics_2021 <- function(con) {
  out_path <- "E:/stochastic_2023/"
  in_path <- "E:/Dropbox (SPH Imperial College)/File requests/latest/202110gavi/"


  stub <- "Andromachi Karachaliou - stochastic-burden.202110gavi-2.MenA_Cambridge-Trotter_"
  stone_stochastic_process(con,
                           "Cambridge-Trotter", "MenA", "202110gavi-3",
                           c("mena-no-vaccination", "mena-campaign-default", "mena-routine-default",
                             "mena-booster-default", "mena-campaign-ia2030_target",
                             "mena-routine-ia2030_target"),
                           file.path(in_path, "Cambridge-Trotter"),
                           c(paste0(stub, "no_vaccination_:index.csv.xz"),
                             paste0(stub, "campaign_default_:index.csv.xz"),
                             paste0(stub, "routine_default_:index.csv.xz"),
                             paste0(stub, "booster_:index.csv.xz"),
                             paste0(stub, "campaign-ia2030_target_:index.csv.xz"),
                             paste0(stub, "routine-ia2030_target_:index.csv.xz")),
                           "", 1, 26, out_path, bypass_cert_check = TRUE)

  #############################################################################

  stub <- "Aniruddha Deshpande - stochastic_burden_est_lopman_"
  stone_stochastic_process(con,
                           "Emory-Lopman", "Rota", "202110gavi-3",
                           c("rota-no-vaccination",
                             "rota-routine-default",
                             "rota-routine-ia2030_target"),
                           file.path(in_path, "Emory-Lopman"),
                           c(paste0(stub, "no_vaccination_2022_01_31.csv.xz"),
                             paste0(stub, "routine_2022_01_31.csv.xz"),
                             paste0(stub, "ia2030_target_2022_01_31.csv.xz")),
                           "", NA, NA, out_path, allow_missing_disease = TRUE, bypass_cert_check = TRUE)

  #############################################################################

  stub <- "Allison Portnoy - stochastic-burden-est."
  stone_stochastic_process(con,
                           "Harvard-Sweet", "HPV", "202110gavi-3",
                           c("hpv-no-vaccination",
                             "hpv-campaign-default",
                             "hpv-campaign-ia2030_target",
                             "hpv-routine-default",
                             "hpv-routine-ia2030_target"),
                           in_path,
                           c(paste0(stub, "novacc_run_:index.csv.xz"),
                             paste0(stub, "coverage_202110gavi-3_hpv-campaign-default_run_:index.csv.xz"),
                             paste0(stub, "coverage_202110gavi-3_hpv-campaign-ia2030_target_run_:index.csv.xz"),
                             paste0(stub, "coverage_202110gavi-3_hpv-routine-default_run_:index.csv.xz"),
                             paste0(stub, "coverage_202110gavi-3_hpv-routine-ia2030_target_run_:index.csv.xz")),
                           "", 1, 200, out_path,
                           runid_from_file = TRUE, bypass_cert_check = TRUE)

  #############################################################################

  stub <- "Keith Fraser - stochastic-burden-estimates.202110gavi-3_YF_IC-Garske_"
  stoner::stone_stochastic_process(con,
                                   "IC-Garske", "YF", "202110gavi-3",
                                   c("yf-no-vaccination",
                                     "yf-preventive-default",
                                     "yf-preventive-ia2030_target",
                                     "yf-routine-default",
                                     "yf-routine-ia2030_target"),
                                   file.path(in_path, "IC-Garske-YF"),
                                   paste0(stub, ":scenario_:index.csv.xz"),
                                   "", 1, 200, out_path, bypass_cert_check = TRUE)

  #############################################################################

  stone_stochastic_process(con, "IVI-Kim", "Typhoid", "202110gavi-3",
                           c("typhoid-no-vaccination",
                             "typhoid-campaign-default", "typhoid-campaign-ia2030_target",
                             "typhoid-routine-default",  "typhoid-routine-ia2030_target"),
                           in_path,
                           c("Jong-Hoon Kim - stoch_Typhoid_novacc_20211217T1.csv.xz",
                             "Jong-Hoon Kim - stoch_Typhoid_campaign-default_20211217T1.csv.xz",
                             "Jong-Hoon Kim - stoch_Typhoid_campaign-ia2030_20211217T1.csv.xz",
                             "Jong-Hoon Kim - stoch_Typhoid_routine-default_20211217T1.csv.xz",
                             "Jong-Hoon Kim - stoch_Typhoid_routine-ia2030_20211217T1.csv.xz"),
                           "", NA, NA, out_path, bypass_cert_check = TRUE)

  #####################################################

  stub <- "stochastic_burden_est_HepB-IC-Hallett_"
  stone_stochastic_process(con,
                           "IC-Hallett", "HepB", "202110gavi-3",
                           c("hepb-bd-default-hepb-routine-default",
                             "hepb-bd-routine-default",
                             "hepb-bd-routine-ia2030_target-hepb-routine-ia2030_target",
                             "hepb-bd-routine-ia2030_target",
                             "hepb-hepb-routine-default",
                             "hepb-hepb-routine-ia2030_target",
                             "hepb-no-vaccination"),
                           in_path,
                           paste0(stub, ":scenario_:index.csv.xz"),
                           "Margaret de Villiers - cert115", 1, 200, out_path,
                           "deaths", c("hepb_cases_acute_severe","hepb_cases_comp_cirrh",
                                       "hepb_cases_hcc_no_cirrh"), "dalys")

  #############################################################################

  stoner::stone_stochastic_process(con,
                                   "IVI-Kim", "Cholera", "202110gavi-3",
                                   c("cholera-no-vaccination", "cholera-campaign-default"),
                                   file.path(in_path, "IVI-Kim-Cholera"),
                                   c("Jong-Hoon Kim - stoch_Cholera_novacc_20211221T00.csv.xz",
                                     "Jong-Hoon Kim - stoch_Cholera_campaign_20211222T212131.csv.xz"),
                                   "", NA, NA, out_path, bypass_cert_check = TRUE)

  #############################################################################

  stone_stochastic_process(con,
                           "JHU-Lee", "Cholera", "202110gavi-3",
                           c("cholera-no-vaccination", "cholera-campaign-default"),
                           file.path(in_path, "JHU-Lee-Cholera"),
                           c("Kaiyue Zou - no-vaccination.csv.xz",
                             "Kaiyue Zou - campaign-default.csv.xz"),
                           "", NA, NA, out_path, bypass_cert_check = TRUE)

  #############################################################################


  stub <- "Amy Winter - stochastic_burden_est-rubella-"
  stone_stochastic_process(con,
                           "JHU-Lessler", "Rubella", "202110gavi-3",
                           c("rubella-routine-no-vaccination",
                             "rubella-campaign-default",
                             "rubella-rcv1-default",
                             "rubella-rcv2-default",
                             "rubella-rcv1-rcv2-default",
                             "rubella-campaign-ia2030_target",
                             "rubella-rcv1-ia2030_target",
                             "rubella-rcv2-ia2030_target",
                             "rubella-rcv1-rcv2-ia2030_target"),
                           in_path,
                           c(paste0(stub, "routine-no-vaccination_:index.csv.xz"),
                             paste0(stub, "campaign-default_:index.csv.xz"),
                             paste0(stub, "rcv1-default_:index.csv.xz"),
                             paste0(stub, "rcv2-default_:index.csv.xz"),
                             paste0(stub, "rcv1-rcv2-default_:index.csv.xz"),
                             paste0(stub, "campaign-ia2030_target_:index.csv.xz"),
                             paste0(stub, "rcv1-ia2030_target_:index.csv.xz"),
                             paste0(stub, "rcv2-ia2030_target_:index.csv.xz"),
                             paste0(stub, "rcv1-rcv2-ia2030_target_:index.csv.xz")),
                           "", 1, 11, out_path,
                           "rubella_deaths_congenital",
                           "rubella_cases_congenital", "dalys", bypass_cert_check = TRUE)

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

  stone_stochastic_process(con,
                           "JHU-Tam", "Hib", "202110gavi-3",
                           hib_scenarios,
                           file.path(in_path, "JHU-Tam-Carter-Hib"), ":scenario.csv.xz",
                           "", NA, NA, out_path,
                           outcomes <- list(deaths = c("deaths_men", "deaths_pneumo"),
                                            cases = c("cases_men", "cases_pneumo"),
                                            dalys = list_params_hib_pcv),
                           bypass_cert_check = TRUE)

  # And to sort out DALYs on the centrals:

  for (hib_scenario in hib_scenarios) {
    stoner::stoner_dalys_for_db(con, list_params_hib_pcv, "JHU-Tam",
                                "Hib", "202110gavi-3", hib_scenario,
                                output_file = file.path(out_path, sprintf("%s_central_dalys.csv",
                                                                          hib_scenario)))
  }

  pcv_scenarios <- c("pcv-no-vaccination-LiST",
                     "pcv-routine-default-LiST",
                     "pcv-routine-ia2030_target-LiST")

  stone_stochastic_process(con,
                           "JHU-Tam", "PCV", "202110gavi-3",
                           pcv_scenarios,
                           file.path(in_path, "JHU-Tam-Carter-PCV"), ":scenario.csv.xz",
                           "", NA, NA, out_path,
                           deaths = c("deaths_men", "deaths_pneumo"),
                           cases = c("cases_men", "cases_pneumo"),
                           dalys = list_params_hib_pcv,
                           bypass_cert_check = TRUE)

  # And to sort out DALYs on the centrals:

  for (pcv_scenario in pcv_scenarios) {
    stoner::stoner_dalys_for_db(con, list_params_hib_pcv, "JHU-Tam",
                                "PCV", "202110gavi-3", pcv_scenario,
                                output_file = file.path(out_path, sprintf("%s_central_dalys.csv",
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

  stone_stochastic_process(con,
                           "JHU-Tam", "Rota", "202110gavi-3",
                           rota_scenarios,
                           file.path(in_path, "JHU-Tam-Carter-Rota"), ":scenario.csv.xz",
                           "", NA, NA, out_path,
                           dalys = list_params_rota,
                           bypass_cert_check = TRUE)


  # And to sort out DALYs on the centrals:

  for (rota_scenario in rota_scenarios) {
    stoner::stoner_dalys_for_db(con, list_params_rota, "JHU-Tam",
                                "Rota", "202110gavi-3", rota_scenario,
                                output_file = file.path(out_path, sprintf("%s_central_dalys.csv",
                                                                          rota_scenario)))
  }

  ####################################################################################

  stub <- "Eric Johnson - "
  stone_stochastic_process(con,
                           "KPW-Jackson", "MenA", "202110gavi-3",
                           c("mena-booster-default",
                             "mena-campaign-default",
                             "mena-campaign-ia2030_target",
                             "mena-no-vaccination",
                             "mena-routine-default",
                             "mena-routine-ia2030_target"),
                           file.path(in_path, "KPW-Jackson-MenA"),
                           paste0(stub, ":scenario-202111gavi-2.MenA_KPW-Johnson.csv.xz"),
                           "", NA, NA, out_path, bypass_cert_check = TRUE)

  #############################################################################

  stoner::stone_stochastic_process(con,
                                   "Li", "HepB", "202110gavi-2",
                                   c(  "hepb-bd-default-hepb-routine-default",
                                       "hepb-bd-routine-default",
                                       "hepb-bd-routine-ia2030_target-hepb-routine-ia2030_target",
                                       "hepb-bd-routine-ia2030_target",
                                       "hepb-hepb-routine-default",
                                       "hepb-hepb-routine-ia2030_target",
                                       "hepb-no-vaccination"

                                   ),
                                   file.path(in_path, "Li"),
                                   paste0(":scenario:index.csv"),
                                   "cert105", 1, 200, out_path,
                                   c("hepb_deaths_acute", "hepb_deaths_total_cirrh", "hepb_deaths_hcc"),
                                   c("hepb_cases_acute_symp", "hepb_cases_fulminant",
                                     "hepb_cases_chronic", "hepb_chronic_symptomatic_in_acute_phase"),
                                   "dalys"
  )

  #############################################################################

  stub <- "Kaja Abbas - PSA_202110gavi-3_"
  stone_stochastic_process(con,
                           "LSHTM-Clark", "Hib", "202110gavi-3",
                           c("hib-no-vaccination","hib-routine-default","hib-routine-ia2030_target"),
                           file.path(in_path, "LSHTM-Clark_Hib"),
                           c(paste0(stub, ":scenario.csv.xz")),
                           "Kaja Abbas - hib_cert116", NA, NA, out_path)

  #############################################################################


  stub <- "Kaja Abbas - PSA_202110gavi-3_"
  stone_stochastic_process(con,
                           "LSHTM-Clark", "Rota", "202110gavi-3",
                           c("rota-no-vaccination","rota-routine-default","rota-routine-ia2030_target"),
                           file.path(in_path, "LSHTM-Clark_Rota"),
                           c(paste0(stub, ":scenario.csv.xz")),
                           "Kaja Abbas - rota_cert117", NA, NA, out_path)

  #############################################################################


  stub <- "stochastic-burden-"
  stoner::stone_stochastic_process(con,
                                   "LSHTM-Jit", "HPV", "202110gavi-3",
                                   c("hpv-no-vaccination",
                                     "hpv-campaign-default",
                                     "hpv-routine-default",
                                     "hpv-campaign-ia2030_target",
                                     "hpv-routine-ia2030_target"),
                                   "E:/Dropbox (SPH Imperial College)/File requests/latest/202110gavi/LSHTM-Jit_HPV",
                                   c(paste0(stub, "novaccination_all_202110gavi-3_hpv-no-vaccination.csv.xz"),
                                     paste0(stub, "vaccination_all_202110gavi-3_hpv-campaign-default.csv.xz"),
                                     paste0(stub, "vaccination_all_202110gavi-3_hpv-routine-default.csv.xz"),
                                     paste0(stub, "vaccination_all_202110gavi-3_hpv-campaign-ia2030_target.csv.xz"),
                                     paste0(stub, "vaccination_all_202110gavi-3_hpv-routine-ia2030_target.csv.xz")),
                                   "cert104", NA, NA, out_path, bypass_cert_check = TRUE)

  #############################################################################

  stub <- "Han Fu - stochastic_burden_estimate_measles-LSHTM-Jit-"
  stone_stochastic_process(con, "LSHTM-Jit", "Measles", "202110gavi-2",
                           c("measles-no-vaccination",
                             "measles-campaign-default",
                             "measles-campaign-only-default",
                             "measles-mcv1-default",
                             "measles-mcv2-default",
                             "measles-campaign-ia2030_target",
                             "measles-campaign-only-ia2030_target",
                             "measles-mcv1-ia2030_target",
                             "measles-mcv2-ia2030_target"),
                           file.path(in_path, "LSHTM-Jit_Measles"),
                           c(paste0(stub, "no-vaccination.csv.xz"),
                             paste0(stub, "campaign-default.csv.xz"),
                             paste0(stub, "campaign-only-default.csv.xz"),
                             paste0(stub, "mcv1-default.csv.xz"),
                             paste0(stub, "mcv2-default.csv.xz"),
                             paste0(stub, "campaign-ia2030_target.csv.xz"),
                             paste0(stub, "campaign-only-ia2030_target.csv.xz"),
                             paste0(stub, "mcv1-ia2030_target.csv.xz"),
                             paste0(stub, "mcv2-ia2030_target.csv.xz")),
                           "Han Fu - cert107", NA, NA, out_path)

  #############################################################################

  stub <- "stochastic_burden_est_"
  stone_stochastic_process(con,
                           "NUS-Chen", "PCV", "202110gavi-3",
                           c("pcv-no-vaccination","pcv-routine-default","pcv-routine-ia2030_target"),
                           file.path(in_path, "LSHTM-NUS-Chen_PCV"),
                           paste0(stub, ":scenario.csv.xz"),
                           "Jemima Koh - cert121", NA, NA, "E:/Stochastic_2021")



  stoner::stone_stochastic_process(con,
                                   "PHE-Vynnycky", "Rubella", "202110gavi-3",
                                   c("rubella-routine-no-vaccination",
                                     "rubella-campaign-default",
                                     "rubella-rcv1-default",
                                     "rubella-rcv2-default",
                                     "rubella-rcv1-rcv2-default",
                                     "rubella-campaign-ia2030_target",
                                     "rubella-rcv1-ia2030_target",
                                     "rubella-rcv2-ia2030_target",
                                     "rubella-rcv1-rcv2-ia2030_target"),
                                   file.path(in_path, "PHE-Vynnycky_Rubella"),
                                   c("VIMC_NV_RCV1RCV2Camp_country:index.csv.xz",
                                     "VIMC_DF_Camp_country:index.csv.xz",
                                     "VIMC_DF_RCV1Camp_country:index.csv.xz",
                                     "VIMC_DF_RCV1RCV2Camp_country:index.csv.xz",
                                     "VIMC_DF_RCV1RCV2_country:index.csv.xz",
                                     "VIMC_IA_Camp_country:index.csv.xz",
                                     "VIMC_IA_RCV1Camp_country:index.csv.xz",
                                     "VIMC_IA_RCV1RCV2Camp_country:index.csv.xz",
                                     "VIMC_IA_RCV1RCV2_country:index.csv.xz"),
                                   "", 1, 112, out_path,
                                   "rubella_deaths_congenital",
                                   "rubella_cases_congenital", "dalys", bypass_cert_check = TRUE)

  #############################################################################

  stub <- "Sean Moore - stochastic_burden_est_JE_UND-Moore_"
  stone_stochastic_process(con,
                           "UND-Moore", "JE", "202110gavi-2",
                           c("je-routine-no-vaccination",
                             "je-campaign-default",
                             "je-routine-default",
                             "je-campaign-ia2030_target",
                             "je-routine-ia2030_target"
                           ),
                           file.path(in_path, "UND-Moore-JE"),
                           paste0(stub, ":scenario.csv.xz"),
                           "Sean Moore - cert108", NA, NA, out_path)

  stub <- "stochastic_burden_est_YF_UND-Perkins_"
  stoner::stone_stochastic_process(con,
                                   "UND-Perkins", "YF", "202110gavi-3",
                                   c("yf-no-vaccination",
                                     "yf-preventive-default",
                                     "yf-preventive-ia2030_target",
                                     "yf-routine-default",
                                     "yf-routine-ia2030_target"),
                                   file.path(in_path, "UND-Perkins-YF"),
                                   paste0(stub, ":scenario_:index.csv.xz"),
                                   "", 1, 200, out_path, bypass_cert_check = TRUE)

  stub <- "Holly Burrows - stochastic_burden_est_TF-Yale-Burrows"
  stone_stochastic_process(con,
                           "Yale-Pitzer", "Typhoid", "202110gavi-3",
                           c("typhoid-no-vaccination",
                             "typhoid-campaign-default", "typhoid-campaign-ia2030_target",
                             "typhoid-routine-default",  "typhoid-routine-ia2030_target"),
                           file.path(in_path, "Yale-Pitzer-Typhoid"),
                           c(paste0(stub, "-novacc_202110.csv.xz"),
                             paste0(stub, "_campaign-default_202110.csv.xz"),
                             paste0(stub, "_campaign-IA2030_202110.csv.xz"),
                             paste0(stub, "_routine-default_202110.csv.xz"),
                             paste0(stub, "_routine-IA2030_202110.csv.xz")),
                           "", NA, NA, out_path, bypass_cert_check = TRUE)


  upload4(out_path, "Cambridge-Trotter", "MenA", "202110gavi-3", con, annex)
  upload4(out_path, "Emory-Lopman", "Rota", "202110gavi-3", con, annex)
  upload4(out_path, "IC-Garske", "YF", "202110gavi-3", con, annex)
  upload4(out_path, "IC-Hallett", "HepB", "202110gavi-3", con, annex)
  upload4(out_path, "IVI-Kim", "Cholera", "202110gavi-3", con, annex)
  upload4(out_path, "IVI-Kim", "Typhoid", "202110gavi-3", con, annex)
  upload4(out_path, "JHU-Lee", "Cholera", "202110gavi-3", con, annex)
  upload4(out_path, "JHU-Lessler", "Rubella", "202110gavi-3", con, annex)
  upload4(out_path, "JHU-Tam", "Hib", "202110gavi-3", con, annex)
  upload4(out_path, "JHU-Tam", "PCV", "202110gavi-3", con, annex)
  upload4(out_path, "JHU-Tam", "Rota", "202110gavi-3", con, annex)
  upload4(out_path, "Li", "HepB", "202110gavi-2", con, annex)
  upload4(out_path, "LSHTM-Clark", "Hib", "202110gavi-3", con, annex)
  upload4(out_path, "LSHTM-Clark", "Rota", "202110gavi-3", con, annex)
  upload4(out_path, "LSHTM-Jit", "HPV", "202110gavi-3", con, annex)
  upload4(out_path, "LSHTM-Jit", "Measles", "202110gavi-2", con, annex)
  upload4(out_path, "NUS-Chen", "PCV", "202110gavi-3", con, annex)
  upload4(out_path, "PHE-Vynnycky", "Rubella", "202110gavi-2", con, annex)
  upload4(out_path, "UND-Moore", "JE", "202110gavi-3", con, annex)
  upload4(out_path, "UND-Perkins", "YF", "202110gavi-3", con, annex)
  upload4(out_path, "Yale-Pitzer", "Typhoid", "202110gavi-3", con, annex)



}

do_stochastics_2019 <- function() {
  in_path <- "E:/Dropbox (SPH Imperial College)/File requests/latest/201910gavi"
  out_path <- "E:/Stocastic_2019"

  #############################################################################

  stub <- "Andromachi Karachaliou - stochastic-burden.201910gavi-4.MenA_Cambridge-Trotter_"
  stone_stochastic_process(con,
                           "Cambridge-Trotter", "MenA", "201910gavi-5",
                           c("mena-campaign-bestcase", "mena-campaign-default", "mena-no-vaccination",
                             "mena-routine-bestcase", "mena-routine-default"),
                           file.path(in_path, "Cambridge-Trotter"),
                           c(paste0(stub, "campaign-bestcase_:index.csv.xz"),
                             paste0(stub, "campaign-default_:index.csv.xz"),
                             paste0(stub, "no-vaccination_:index.csv.xz"),
                             paste0(stub, "routine-bestcase_:index.csv.xz"),
                             paste0(stub, "routine-default_:index.csv.xz")),
                           "cert60", 1, 52, out_path)

  #############################################################################

  stub <- "Ivane Gamkrelidze - stochastic-burden-template.201910gavi-4.HepB_CDA-Razavi_"
  stone_stochastic_process(con,
                           "CDA-Razavi", "HepB", "201910gavi-5",
                           c("hepb-bd-default-hepb-routine-default",
                             "hepb-bd-routine-bestcase-hepb-routine-bestcase",
                             "hepb-no-vaccination",
                             "hepb-stop",
                             "hepb-bd-routine-bestcase",
                             "hepb-bd-routine-default",
                             "hepb-hepb-routine-bestcase",
                             "hepb-hepb-routine-default"
                           ),
                           file.path(in_path, "CDA-Razavi"),
                           c(paste0(stub, "all_:scenario.csv.xz"),
                             paste0(stub, "all_:scenario.csv.xz"),
                             paste0(stub, "all_:scenario.csv.xz"),
                             paste0(stub, "all_:scenario.csv.xz"),
                             paste0(stub, "bd_:scenario.csv.xz"),
                             paste0(stub, "bd_:scenario.csv.xz"),
                             paste0(stub, "non_bd_:scenario.csv.xz"),
                             paste0(stub, "non_bd_:scenario.csv.xz")),
                           "Ivane Gamkrelidze - cert68", NA, NA, out_path,
                           c("hepb_deaths_acute","hepb_deaths_dec_cirrh","hepb_deaths_hcc"),
                           c("hepb_cases_acute_severe","hepb_cases_dec_cirrh","hepb_cases_hcc"),
                           "dalys"
  )

  #############################################################################

  stub <- "Molly Steele - stochastic-burden.201910gavi-4.Rota_Emory-Lopman_"
  stone_stochastic_process(con,
                           "Emory-Lopman", "Rota", "201910gavi-5",
                           c("rota-no-vaccination",
                             "rota-routine-bestcase",
                             "rota-routine-default"),
                           file.path(in_path, "Emory-Lopman"),
                           paste0(stub, ":scenario.csv.xz"),
                           "Molly Steele - cert66", NA, NA, out_path,
                           allow_missing_disease = TRUE)

  #############################################################################

  stub <- "stochastic-burden-est.201910gavi-5.HPV_Harvard-Sweet_"
  stone_stochastic_process(con,
                           "Harvard-Sweet", "HPV", "201910gavi-5",
                           c("hpv-campaign-bestcase",
                             "hpv-campaign-default",
                             "hpv-no-vaccination",
                             "hpv-routine-bestcase",
                             "hpv-routine-default"),
                           file.path(in_path, "Harvard-Sweet"),
                           c(paste0(stub, "campaign-bestcase_run_:index.csv.xz"),
                             paste0(stub, "campaign-default_run_:index.csv.xz"),
                             paste0(stub, "novacc_run_:index.csv.xz"),
                             paste0(stub, "routine-bestcase_run_:index.csv.xz"),
                             paste0(stub, "routine-default_run_:index.csv.xz")),
                           "", 1, 200, out_path,
                           runid_from_file = TRUE, bypass_cert_check = TRUE)

  #############################################################################

  stub <- "stochastic-burden-estimates.201910gavi-4_YF_IC-Garske_"
  stone_stochastic_process(con,
                           "IC-Garske", "YF", "201910gavi-5",
                           c("yf-no-vaccination",
                             "yf-preventive-bestcase",
                             "yf-preventive-default",
                             "yf-routine-bestcase",
                             "yf-routine-default",
                             "yf-stop"),
                           file.path(in_path, "IC-Garske2"),
                           paste0(stub, ":scenario_:index.csv.xz"),
                           "Katy Gaythorpe - cert62", 1, 200, out_path)

  #############################################################################

  stub <- "stochastic_burden_est_HepB-IC-Hallett_"
  stone_stochastic_process(con,
                           "IC-Hallett", "HepB", "201910gavi-5",
                           c("hepb-bd-default-hepb-routine-default",
                             "hepb-bd-routine-bestcase-hepb-routine-bestcase",
                             "hepb-no-vaccination",
                             "hepb-stop",
                             "hepb-bd-routine-bestcase",
                             "hepb-bd-routine-default",
                             "hepb-hepb-routine-bestcase",
                             "hepb-hepb-routine-default"),
                           file.path(in_path, "IC-Hallett"),
                           paste0(stub, ":scenario_:index.csv.xz"),
                           "Margaret de Villiers  - cert73", 1, 200, out_path,
                           "deaths", c("hepb_cases_acute_severe","hepb_cases_comp_cirrh",
                                       "hepb_cases_hcc_no_cirrh"), "dalys")

  #############################################################################

  stone_stochastic_process(con,
                           "IVI-Kim", "Cholera", "201910gavi-5",
                           c("cholera-no-vaccination", "cholera-campaign-default"),
                           file.path(in_path, "IVI-Kim-Cholera"),
                           c("Jong-Hoon Kim - stoch_output_Cholera_novacc_20210902.csv.xz",
                             "Jong-Hoon Kim - stoch_output_Cholera_campaign_20210902.csv.xz"),
                           "Jong-Hoon Kim - cert89", NA, NA, out_path)

  #############################################################################

  stone_stochastic_process(con,
                           "IVI-Kim", "Typhoid", "201910gavi-5",
                           c("typhoid-no-vaccination", "typhoid-campaign-default", "typhoid-routine-default"),
                           file.path(in_path, "IVI-Kim-Typhoid"),
                           c("Jong-Hoon Kim - stoch_Typhoid_novacc.csv.xz",
                             "Jong-Hoon Kim - stoch_Typhoid_campaign.csv.xz",
                             "Jong-Hoon Kim - stoch_Typhoid_routine.csv.xz"),
                           "Jong-Hoon Kim - cert90", NA, NA, out_path)

  #############################################################################

  stub <- "Amy Winter - stochastic_burden_est-"
  stone_stochastic_process(con,
                           "JHU-Lessler", "Rubella", "201910gavi-5",
                           c("rubella-campaign-bestcase",
                             "rubella-campaign-default",
                             "rubella-routine-no-vaccination",
                             "rubella-rcv1-bestcase",
                             "rubella-rcv1-default",
                             "rubella-rcv1-rcv2-bestcase",
                             "rubella-rcv1-rcv2-default",
                             "rubella-rcv2-bestcase",
                             "rubella-rcv2-default",
                             "rubella-stop"),
                           file.path(in_path, "JHU-Lessler"),
                           c(rep(paste0(stub, ":scenario_:index.csv.xz"), 2),
                             paste0(stub, "rubella-no-vaccination_:index.csv.xz"),
                             rep(paste0(stub, ":scenario_:index.csv.xz"), 7)),
                           "Amy Winter - cert70", 1, 12, out_path,
                           "rubella_deaths_congenital",
                           "rubella_cases_congenital", "dalys")

  #############################################################################

  stub <- "Michael Jackson - stochastic_burden_est_MenA_KPWA_"
  stone_stochastic_process(con,
                           "KPW-Jackson", "MenA", "201910gavi-5",
                           c("mena-routine-bestcase",
                             "mena-routine-default",
                             "mena-campaign-bestcase",
                             "mena-campaign-default",
                             "mena-no-vaccination"),
                           file.path(in_path, "KPW-Jackson"),
                           c(paste0(stub, "both_bestcase_:index.csv.xz"),
                             paste0(stub, "both_default_:index.csv.xz"),
                             paste0(stub, "campaign_bestcase_:index.csv.xz"),
                             paste0(stub, "campaign_default_:index.csv.xz"),
                             paste0(stub, "none_default_:index.csv.xz")),
                           "cert61", 1, 26, out_path)

  #############################################################################

  stone_stochastic_process(con,
                           "JHU-Lee", "Cholera", "201910gavi-5",
                           c("cholera-no-vaccination", "cholera-campaign-default"),
                           file.path(in_path, "JHU-Lee"),
                           c("Kaiyue Zou - stochastic-burden-template.201910gavi-5.Cholera_no-vaccination.csv.xz",
                             "Kaiyue Zou - stochastic-burden-template.201910gavi-5.Cholera_campaign-default.csv.xz"),
                           "", NA, NA, out_path, bypass_cert_check = TRUE)

  #############################################################################

  list_params_hib_pcv <- data_frame(
    outcome = c("cases_men", "cases_men", "cases_men", "cases_men", "cases_men",
                "cases_pneumo", "cases_pneumo", "deaths_men", "deaths_pneumo"),
    proportion = c(1, 0.014, 0.045, 0.021, 0.017, 1, 0.06, 1, 1),
    average_duration = c(0.04,1000,1000,1000,1000,0.02,1000,1000,1000),
    disability_weight = c(0.133, 0.043, 0.027, 0.552, 0.61, 0.051, 0.019, 1, 1)
  )

  stone_stochastic_process(con,
                           "JHU-Tam", "Hib", "201910gavi-5",
                           c("hib-no-vaccination-LiST", "hib-routine-default-LiST", "hib-routine-bestcase-LiST"),
                           file.path(in_path, "JHU-Tam-Hib"),
                           c("novac:index.csv.xz", "default:index.csv.xz", "best:index.csv.xz"),
                           "", 1, 14, out_path,
                           deaths = c("deaths_men", "deaths_pneumo"),
                           cases = c("cases_men", "cases_pneumo"),
                           dalys = list_params_hib_pcv,
                           bypass_cert_check = TRUE)

  # And to add DALYs to the existing

  stone_stochastic_process(con,
                           "JHU-Tam", "PCV", "201910gavi-5",
                           c("pcv-no-vaccination-LiST", "pcv-routine-default-LiST", "pcv-routine-bestcase-LiST"),
                           file.path(in_path, "JHU-Tam-PCV"),
                           c("novac:index.csv.xz", "default:index.csv.xz", "best:index.csv.xz"),
                           "", 1, 14, out_path,
                           deaths = c("deaths_men", "deaths_pneumo"),
                           cases = c("cases_men", "cases_pneumo"),
                           dalys = list_params_hib_pcv,
                           bypass_cert_check = TRUE)

  list_params_rota <- data_frame(
    outcome = c("cases", "deaths"),
    proportion = c(1, 1),
    average_duration = c(0.01, 1000),
    disability_weight = c(0.247, 1)
  )

  stone_stochastic_process(con,
                           "JHU-Tam", "Rota", "201910gavi-5",
                           c("rota-no-vaccination-LiST", "rota-routine-default-LiST", "rota-routine-bestcase-LiST"),
                           file.path(in_path, "JHU-Tam-Rota"),
                           c("novac:index.csv.xz", "default:index.csv.xz", "best:index.csv.xz"),
                           "", 1, 14, out_path,
                           dalys = list_params_rota,
                           bypass_cert_check = TRUE)

  #############################################################################

  stone_stochastic_process(con,
                           "Li", "HepB", "201910gavi-5",
                           c("hepb-bd-default-hepb-routine-default",
                             "hepb-bd-routine-bestcase-hepb-routine-bestcase",
                             "hepb-no-vaccination",
                             "hepb-stop",
                             "hepb-bd-routine-bestcase",
                             "hepb-bd-routine-default",
                             "hepb-hepb-routine-bestcase",
                             "hepb-hepb-routine-default"
                           ),
                           file.path(in_path, "Li"),
                           paste0(":scenario:index.csv.xz"),
                           "cert74", 1, 200, out_path,
                           c("hepb_deaths_acute", "hepb_deaths_total_cirrh", "hepb_deaths_hcc"),
                           c("hepb_cases_acute_symp", "hepb_cases_fulminant",
                             "hepb_cases_chronic", "hepb_chronic_symptomatic_in_acute_phase"),
                           "dalys"
  )

  #############################################################################

  stub <- "VIMC_Hib_PSA_"
  stone_stochastic_process(con,
                           "LSHTM-Clark", "Hib", "201910gavi-5",
                           c("hib-no-vaccination","hib-routine-bestcase","hib-routine-default"),
                           file.path(in_path, "LSHTM-Clark_Hib"),
                           c(paste0(stub, "NoVax.csv.xz"),
                             paste0(stub, "Best.csv.xz"),
                             paste0(stub, "Default.csv.xz")),
                           "cert81", NA, NA, out_path)

  stub <- "VIMC_Sp_PSA_"
  stone_stochastic_process(con,
                           "LSHTM-Clark", "PCV", "201910gavi-5",
                           c("pcv-no-vaccination","pcv-routine-bestcase","pcv-routine-default"),
                           file.path(in_path, "LSHTM-Clark_PCV"),
                           c(paste0(stub, "NoVax.csv.xz"),
                             paste0(stub, "Best.csv.xz"),
                             paste0(stub, "Default.csv.xz")),
                           "cert82", NA, NA, "E:/Stochastic_2019")

  stub <- "Hira Tanvir - VIMC_Rota_PSA_"
  stone_stochastic_process(con,
                           "LSHTM-Clark", "Rota", "201910gavi-5",
                           c("rota-no-vaccination","rota-routine-bestcase","rota-routine-default"),
                           file.path(in_path, "LSHTM-Clark_Rota"),
                           c(paste0(stub, "NoVax.csv.xz"),
                             paste0(stub, "Best.csv.xz"),
                             paste0(stub, "Default.csv.xz")),
                           "", NA, NA, "E:/Stochastic_2019")

  #############################################################################

  stub <- "stochastic-burden-"
  stone_stochastic_process(con,
                           "LSHTM-Jit", "HPV", "201910gavi-5",
                           c("hpv-campaign-bestcase",
                             "hpv-campaign-default",
                             "hpv-no-vaccination",
                             "hpv-routine-bestcase",
                             "hpv-routine-default"),
                           file.path(in_path, "LSHTM-Jit_HPV"),
                           c(paste0(stub, "vaccination_201910gavi-4_hpv-campaign-bestcase.csv.xz"),
                             paste0(stub, "vaccination_201910gavi-4_hpv-campaign-default.csv.xz"),
                             paste0(stub, "novaccination_201910gavi-4_hpv-no-vaccination.csv.xz"),
                             paste0(stub, "vaccination_201910gavi-4_hpv-routine-bestcase.csv.xz"),
                             paste0(stub, "vaccination_201910gavi-4_hpv-routine-default.csv.xz")),
                           "Kaja Abbas - stochastic_parameters_certificate_HPV_LSHTM-Jit_201910gavi-4",
                           NA, NA, out_path, bypass_cert_check = TRUE)

  #############################################################################

  stub <- "stochastic_burden_estimate_measles-LSHTM-Jit-"
  stone_stochastic_process(con, "LSHTM-Jit", "Measles", "201910gavi-5",
                           c("measles-no-vaccination",
                             "measles-campaign-default","measles-campaign-bestcase",
                             "measles-campaign-only-default","measles-campaign-only-bestcase",
                             "measles-mcv1-default","measles-mcv1-bestcase",
                             "measles-mcv2-default","measles-mcv2-bestcase",
                             "measles-stop"),
                           file.path(in_path, "LSHTM-Jit_Measles"),
                           c(paste0(stub, "no-vaccination_Portnoy.csv.xz"),
                             paste0(stub, "campaign-default_Portnoy.csv.xz"), paste0(stub, "campaign-bestcase_Portnoy.csv.xz"),
                             paste0(stub, "campaign-only-default_Portnoy.csv.xz"), paste0(stub, "campaign-only-bestcase_Portnoy.csv.xz"),
                             paste0(stub, "mcv1-default_Portnoy.csv.xz"), paste0(stub, "mcv1-bestcase_Portnoy.csv.xz"),
                             paste0(stub, "mcv2-default_Portnoy.csv.xz"), paste0(stub, "mcv2-bestcase_Portnoy.csv.xz"),
                             paste0(stub, "stop_Portnoy.csv.xz")),
                           "cert83", NA, NA, out_path)

  #############################################################################

  stub <- "Template_Stochastic_"
  stone_stochastic_process(con,
                           "OUCRU-Clapham", "JE", "201910gavi-5",
                           c("je-campaign-bestcase",
                             "je-campaign-default",
                             "je-routine-no-vaccination",
                             "je-routine-bestcase",
                             "je-routine-default"),
                           file.path(in_path, "OUCRU-Clapham"),
                           c(paste0(stub, "Campaign_Best4_correcting_:index.csv.xz"),
                             paste0(stub, "Campaign_Default4_correcting_:index.csv.xz"),
                             paste0(stub, "Naive4_correcting_:index.csv.xz"),
                             paste0(stub, "Routine_Best4_correcting_:index.csv.xz"),
                             paste0(stub, "Routine_Default4_correcting_:index.csv.xz")),
                           "cert76", 1, 200, out_path)

  #############################################################################

  stub <- "stochastic_burden_est_"
  stone_stochastic_process(con,
                           "PHE-Vynnycky", "Rubella", "201910gavi-5",
                           c("rubella-campaign-bestcase",
                             "rubella-campaign-default",
                             "rubella-routine-no-vaccination",
                             "rubella-rcv1-bestcase",
                             "rubella-rcv1-default",
                             "rubella-rcv1-rcv2-bestcase",
                             "rubella-rcv1-rcv2-default",
                             "rubella-rcv2-bestcase",
                             "rubella-rcv2-default",
                             "rubella-stop"),
                           file.path(in_path, "PHE-Vynnycky"),
                           paste0(stub, ":scenario_country:index.csv.xz"),
                           "", 1, 112, out_path,
                           "rubella_deaths_congenital",
                           "rubella_cases_congenital", "dalys")

  #############################################################################

  stub <- "Heather Santos - "
  stone_stochastic_process(con,
                           "PSU-Ferrari", "Measles", "201910gavi-5",
                           c("measles-no-vaccination",
                             "measles-mcv1-bestcase",
                             "measles-mcv2-bestcase",
                             "measles-campaign-bestcase",
                             "measles-mcv1-default",
                             "measles-mcv2-default",
                             "measles-campaign-default",
                             "measles-stop",
                             "measles-campaign-only-bestcase",
                             "measles-campaign-only-default"),
                           file.path(in_path, "PSU-Ferrari"),
                           c(paste0(stub, "novax_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz"),
                             paste0(stub, "bestcase_mcv1_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz"),
                             paste0(stub, "bestcase_mcv2_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz"),
                             "stochastic:index_burden_Measles-PSU-Ferrari.csv.xz",
                             paste0(stub, "default_mcv1_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz"),
                             paste0(stub, "default_mcv2_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz"),
                             paste0(stub, "default_campaign_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz"),
                             paste0(stub, "stop_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz"),
                             paste0(stub, "bestcase_campaign_only_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz"),
                             paste0(stub, "default_campaign_only_stochastic:index_burden_Measles-PSU-Ferrari.csv.xz")),
                           "Heather Santos - cert80", 1, 8, out_path)

  #############################################################################

  stub <- "Sean Moore - stochastic_burden_est_JE_UND-Moore_"
  stone_stochastic_process(con,
                           "UND-Moore", "JE", "201910gavi-5",
                           c("je-campaign-bestcase",
                             "je-campaign-default",
                             "je-routine-no-vaccination",
                             "je-routine-bestcase",
                             "je-routine-default"),
                           file.path(in_path, "UND-Moore"),
                           c(paste0(stub, ":scenario.csv.xz"),
                             paste0(stub, ":scenario.csv.xz"),
                             paste0(stub, "je-no-vaccination.csv.xz"),
                             paste0(stub, ":scenario.csv.xz"),
                             paste0(stub, ":scenario.csv.xz")),
                           "Sean Moore - cert58", NA, NA, out_path)

  #############################################################################

  stub <- "stochastic_burden_est_YF_UND-Perkins_"
  stone_stochastic_process(con,
                           "UND-Perkins", "YF", "201910gavi-5",
                           c("yf-no-vaccination",
                             "yf-preventive-bestcase",
                             "yf-preventive-default",
                             "yf-routine-bestcase",
                             "yf-routine-default",
                             "yf-stop"),
                           file.path(in_path, "UND-Perkins"),
                           paste0(stub, ":scenario_:index.csv.xz"),
                           "John Huber - cert85", 1, 200, out_path)

  #############################################################################

  stone_stochastic_process(con,
                           "Yale-Pitzer", "Typhoid", "201910gavi-5",
                           c("typhoid-no-vaccination", "typhoid-campaign-default", "typhoid-routine-default"),
                           file.path(in_path, "Yale-Pitzer"),
                           c("Virginia Pitzer - 2021-02-18 17.00.26 - stochastic_output_TF-Yale-Pitzer_novacc.csv.xz",
                             "Virginia Pitzer - 2021-02-18 16.58.03 - stochastic_output_TF-Yale-Pitzer_campaign.csv.xz",
                             "Virginia Pitzer - 2021-02-18 16.59.14 - stochastic_output_TF-Yale-Pitzer_camproutine.csv.xz"),
                           "Virginia Pitzer - cert88", NA, NA, out_path)

  #############################################################################

  upload4(out_path, "Cambridge-Trotter", "MenA", "201910gavi-5", con, annex)
  upload4(out_path, "CDA-Razavi", "HepB", "201910gavi-5", con, annex)
  upload4(out_path, "Emory-Lopman", "Rota", "201910gavi-5", con, annex)
  upload4(out_path, "Harvard-Sweet", "HPV", "201910gavi-5", con, annex)
  upload4(out_path, "IC-Garske", "YF", "201910gavi-5", con, annex)
  upload4(out_path, "IC-Hallett", "HepB", "201910gavi-5", con, annex)
  upload4(out_path, "IVI-Kim", "Cholera", "201910gavi-5", con, annex)
  upload4(out_path, "IVI-Kim", "Typhoid", "201910gavi-5", con, annex)
  upload4(out_path, "KPW-Jackson", "MenA", "201910gavi-5", con, annex)
  upload4(out_path, "JHU-Lee", "Cholera", "201910gavi-5", con, annex)
  upload4(out_path, "JHU-Tam", "Hib", "201910gavi-5", con, annex)
  upload4(out_path, "JHU-Tam", "PCV", "201910gavi-5", con, annex)
  upload4(out_path, "JHU-Tam", "Rota", "201910gavi-5", con, annex)
  upload4(out_path, "JHU-Lessler", "Rubella", "201910gavi-5", con, annex)
  upload4(out_path, "Li", "HepB", "201910gavi-5", con, annex)
  upload4(out_path, "LSHTM-Clark", "Hib", "201910gavi-5", con, annex)
  upload4(out_path, "LSHTM-Clark", "PCV", "201910gavi-5", con, annex)
  upload4(out_path, "LSHTM-Clark", "Rota", "201910gavi-5", con, annex)
  upload4(out_path, "LSHTM-Jit", "HPV", "201910gavi-5", con, annex)
  upload4(out_path, "LSHTM-Jit", "Measles", "201910gavi-5", con, annex)
  upload4(out_path, "OUCRU-Clapham", "JE", "201910gavi-5", con, annex)
  upload4(out_path, "PHE-Vynnycky", "Rubella", "201910gavi-5", con, annex)
  upload4(out_path, "PSU-Ferrari", "Measles", "201910gavi-5", con, annex)
  upload4(out_path, "UND-Moore", "JE", "201910gavi-5", con, annex)
  upload4(out_path, "UND-Perkins", "YF", "201910gavi-5", con, annex)
  upload4(out_path, "Yale-Pitzer", "Typhoid", "201910gavi-5", con, annex)

}
