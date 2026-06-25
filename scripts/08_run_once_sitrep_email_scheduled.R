# PREIS scheduled runner — one check only
ROOT_DIR <- "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"
source(file.path(ROOT_DIR, "scripts", "08_monitor_sitrep_email.R"))
check_preis_sitrep_once(send_now = TRUE, run_pipeline = TRUE)
