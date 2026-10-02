############################################################
# PREIS Ebola DRC
# DISABLED old SMTP scientific fallback patch
#
# This file is intentionally disabled.
# The only authorized email sender is:
# scripts/preis_safe_scientific_email.R
#
# Do not re-enable this file.
#
# CORRECTIF 2026-10-02 : ce fichier est source() (pas execute en
# standalone) depuis scripts/08_cloud_sitrep_monitor.R. Le quit()
# qui etait ici ne desactivait pas que CE fichier : il terminait tout
# le processus R appelant, AVANT que 08_cloud_sitrep_monitor.R ait pu
# scanner INSP, resoudre le PDF ou le telecharger. Consequence prouvee :
# aucun SitRep n'a ete telecharge automatiquement depuis le 2026-07-10
# (commit cbc34c22, qui a introduit ce quit()) malgre un step CI tou-
# jours vert (quit(status=0) = succes). Voir audit PREIS pour le detail
# et la reproduction isolee. Les deux message() ci-dessous suffisent a
# documenter que l'ancien fallback est desactive ; plus besoin de quit().
############################################################

message('[PREIS EMAIL] Old SMTP scientific fallback patch is disabled.')
message('[PREIS EMAIL] Use scripts/preis_safe_scientific_email.R only.')
