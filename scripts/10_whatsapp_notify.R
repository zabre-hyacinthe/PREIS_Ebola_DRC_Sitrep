############################################################
# PREIS Ebola RDC â€” Notification WhatsApp (Cloud API)
# scripts/10_whatsapp_notify.R
#   - appele APRES un email envoye avec succes
#   - WA_ENABLED != TRUE  -> "WhatsApp disabled"
#   - non bloquant: erreurs capturees ; n'arrete QUE si WA_STRICT=TRUE
#   - destinataires individuels (WA_TO) et groupes (WA_GROUPS)
#
# Variables:
#   WA_ENABLED          TRUE | FALSE
#   WA_ACCESS_TOKEN     token Meta            (obligatoire si actif)
#   WA_PHONE_NUMBER_ID  id numero expediteur  (obligatoire si actif)
#   WA_TO               numeros sans +, separes par ; ou ,
#                       ex: 22678088770;243xxxxxxxxx
#   WA_GROUPS           GROUP_ID(s) separes par ; ou ,
#                       (obtenus via Groups API / webhooks)
#   WA_API_VERSION      defaut v22.0
#   WA_STRICT           TRUE | FALSE (defaut FALSE)
#   WA_TEMPLATE_NAME    optionnel: envoi via modele approuve Meta
#   WA_TEMPLATE_LANG    code langue du modele (defaut en)
#
# Au moins un de WA_TO / WA_GROUPS doit etre fourni.
# Hors fenetre 24h, Meta exige un modele -> WA_TEMPLATE_NAME.
############################################################

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})

preis_wa_env <- function(name, default = "") {
  v <- Sys.getenv(name, unset = default)
  if (is.na(v)) default else v
}

preis_wa_truthy <- function(name) {
  toupper(preis_wa_env(name, "FALSE")) %in% c("TRUE", "1", "YES", "Y")
}

preis_wa_enabled <- function() preis_wa_truthy("WA_ENABLED")
preis_wa_strict  <- function() preis_wa_truthy("WA_STRICT")

preis_wa_split_to <- function(x) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  parts <- unlist(strsplit(x, "[;,[:space:]]+"))
  parts <- gsub("[^0-9]", "", parts)
  unique(parts[nzchar(parts)])
}

preis_wa_split_ids <- function(x) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  parts <- unlist(strsplit(x, "[;,[:space:]]+"))
  unique(parts[nzchar(parts)])
}

.preis_wa_log <- function(...) {
  if (exists("log_msg", mode = "function")) {
    log_msg(...)
  } else {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- ", paste0(..., collapse = ""), "\n", sep = "")
  }
}

preis_send_whatsapp <- function(sitrep_no = "",
                                page_url  = "",
                                pdf_url   = "",
                                message   = NULL) {

  result <- list(enabled = FALSE, sent = 0L, failed = 0L, errors = character(0))

  if (!preis_wa_enabled()) {
    .preis_wa_log("WhatsApp disabled")
    return(invisible(result))
  }
  result$enabled <- TRUE

  token    <- preis_wa_env("WA_ACCESS_TOKEN")
  phone_id <- preis_wa_env("WA_PHONE_NUMBER_ID")
  to_raw   <- preis_wa_env("WA_TO")
  groups_raw <- preis_wa_env("WA_GROUPS")
  api_ver  <- preis_wa_env("WA_API_VERSION", "v22.0")
  template <- preis_wa_env("WA_TEMPLATE_NAME")
  lang     <- preis_wa_env("WA_TEMPLATE_LANG", "en")

  hard_missing <- c(
    WA_ACCESS_TOKEN    = !nzchar(token),
    WA_PHONE_NUMBER_ID = !nzchar(phone_id)
  )
  if (any(hard_missing)) {
    m <- paste0("Secrets WhatsApp manquants: ", paste(names(hard_missing)[hard_missing], collapse = ", "))
    result$errors <- c(result$errors, m)
    .preis_wa_log("WhatsApp ", m)
    if (preis_wa_strict()) stop(m, call. = FALSE)
    return(invisible(result))
  }

  recipients <- preis_wa_split_to(to_raw)
  groups     <- preis_wa_split_ids(groups_raw)

  targets <- c(
    lapply(recipients, function(x) list(id = x, type = "individual")),
    lapply(groups,     function(x) list(id = x, type = "group"))
  )

  if (length(targets) == 0) {
    m <- "Aucun destinataire: WA_TO et WA_GROUPS sont vides"
    result$errors <- c(result$errors, m)
    .preis_wa_log("WhatsApp ", m)
    if (preis_wa_strict()) stop(m, call. = FALSE)
    return(invisible(result))
  }

  if (is.null(message) || !nzchar(message)) {
    message <- paste0(
      "PREIS Ebola DRC Alert\n\n",
      "A new INSP Ebola SitRep has been detected.\n\n",
      "SitRep: N", sitrep_no, "\n",
      "INSP page: ", page_url, "\n",
      "PDF: ", pdf_url, "\n\n",
      "The official PDF has also been sent by email.\n",
      "Automated analytical outputs will follow once generated and validated.\n\n",
      "Contact: Dr Hyacinthe Zabre, WhatsApp +226 78 08 87 70"
    )
  }

  url <- sprintf("https://graph.facebook.com/%s/%s/messages", api_ver, phone_id)

  for (t in targets) {

    base <- list(
      messaging_product = "whatsapp",
      recipient_type    = t$type,
      to                = t$id
    )

    payload <- if (nzchar(template)) {
      c(base, list(
        type     = "template",
        template = list(name = template, language = list(code = lang))
      ))
    } else {
      c(base, list(
        type = "text",
        text = list(preview_url = FALSE, body = message)
      ))
    }

    ok <- tryCatch({
      resp <- httr::POST(
        url,
        httr::add_headers(Authorization = paste("Bearer", token)),
        httr::content_type_json(),
        body   = jsonlite::toJSON(payload, auto_unbox = TRUE),
        encode = "raw",
        httr::timeout(45)
      )
      code <- httr::status_code(resp)
      if (code >= 200 && code < 300) {
        .preis_wa_log("WhatsApp envoye (", t$type, ") ", t$id)
        TRUE
      } else {
        txt <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"),
                        error = function(e) "")
        .preis_wa_log("WhatsApp echec (", t$type, ") ", t$id, " HTTP ", code, " ", substr(txt, 1, 300))
        FALSE
      }
    }, error = function(e) {
      .preis_wa_log("WhatsApp erreur reseau (", t$type, ") ", t$id, ": ", conditionMessage(e))
      FALSE
    })

    if (isTRUE(ok)) {
      result$sent <- result$sent + 1L
    } else {
      result$failed <- result$failed + 1L
      result$errors <- c(result$errors, paste0("echec ", t$type, ": ", t$id))
    }
  }

  if (result$failed > 0 && preis_wa_strict()) {
    stop("WhatsApp: ", result$failed, " envoi(s) en echec (WA_STRICT=TRUE)", call. = FALSE)
  }

  invisible(result)
}