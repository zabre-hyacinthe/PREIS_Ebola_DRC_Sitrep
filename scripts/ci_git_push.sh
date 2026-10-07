#!/usr/bin/env bash
# PREIS - commit + push robustes pour les workflows GitHub Actions.
#
# Pourquoi (cause prouvee des doublons du 07/10/2026) : les anciens steps faisaient
#   git pull --rebase origin main || echo "No remote changes to rebase"
#   git push || echo "No changes to push"
# Un echec de rebase (fichiers d'etat modifies des deux cotes) ou de push etait donc AVALE :
# les mails etaient partis mais l'etat anti-doublon n'etait jamais enregistre -> le run
# suivant renvoyait tout.
#
# Usage : scripts/ci_git_push.sh "<message de commit>" [--strict]
#   Les fichiers a enregistrer doivent deja etre "git add"-es par l'appelant.
#   --strict : sortie en erreur (run rouge) si le push echoue apres 4 essais.
set -u
MSG="${1:-PREIS state}"
STRICT="${2:-}"

if git diff --cached --quiet; then
  echo "ci_git_push: rien a enregistrer."
  exit 0
fi
git commit -m "$MSG" || { echo "ci_git_push: commit impossible"; [ "$STRICT" = "--strict" ] && exit 1 || exit 0; }

for attempt in 1 2 3 4; do
  # -X theirs : en cas de conflit, NOS modifications (les commits rejoues) l'emportent ;
  # les fichiers d'etat sont ecrits par un seul run a la fois (groupe de concurrence).
  if git pull --rebase --autostash -X theirs origin main && git push origin HEAD:main; then
    echo "ci_git_push: push reussi (essai $attempt)."
    exit 0
  fi
  echo "::warning::ci_git_push: essai $attempt echoue, nouvel essai..."
  git rebase --abort 2>/dev/null || true
  sleep $((attempt * 5))
done

echo "::error::ci_git_push: ECHEC du push apres 4 essais - l'etat anti-doublon n'est PAS enregistre (risque de renvoi au prochain run)."
[ "$STRICT" = "--strict" ] && exit 1
exit 0
