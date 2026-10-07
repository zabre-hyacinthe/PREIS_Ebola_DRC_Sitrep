# ASCII uniquement. Signature commune a TOUS les e-mails PREIS (source unique).
# Chargee par : 08_cloud_sitrep_monitor.R, preis_email_enrichi.R, 05_send_africacdc_sitrep_email.R,
#               05b_send_africacdc_sitrep_brief_full_email.R, preis_watchdog_gap_alert.R
# Pour changer la signature, modifier UNIQUEMENT ce fichier.
PREIS_SIG_LINES <- c(
  "Dr R. Hyacinthe ZABRE",
  "Epidemio-Biostat, PREIS developer",
  "Email: zrhyacinthe@gmail.com",
  "WhatsApp: +22678088770"
)
preis_signature_text <- function() paste0("\n\n-- \n", paste(PREIS_SIG_LINES, collapse = "\n"), "\n")
preis_signature_html <- function() paste0(
  "<div style='margin-top:22px;padding-top:10px;border-top:1px solid #d0d0d0;font-family:Segoe UI,Arial,sans-serif;",
  "font-size:13px;line-height:1.5;color:#252525'>",
  "<b>", PREIS_SIG_LINES[1], "</b><br>", PREIS_SIG_LINES[2], "<br>",
  "Email: <a href='mailto:zrhyacinthe@gmail.com' style='color:#0B4F3C'>zrhyacinthe@gmail.com</a><br>",
  "WhatsApp: ", sub("^WhatsApp: ", "", PREIS_SIG_LINES[4]), "</div>")
