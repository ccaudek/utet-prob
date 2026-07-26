#!/usr/bin/env bash
# =============================================================
#  Deploy di un sito Quarto del progetto UTET.
#  Identico in tutti i repository: l'unica cosa da adattare è
#  PROTETTI, l'elenco dei file che non devono mai sparire.
# =============================================================
set -euo pipefail

MSG="${1:-aggiornamento del sito}"

# Percorsi che devono esistere dopo il render. Vuoto in quasi
# tutti i siti; nel Companion contiene l'alias dell'indirizzo
# stampato nel volume.
PROTETTI=()
# esempio per utet-companion:
# PROTETTI=("docs/guida-al-manuale/index.html")

echo "→ render"
quarto render --clean

echo "→ controllo link interni"
Rscript R/check_link.R

if [ ${#PROTETTI[@]} -gt 0 ]; then
  echo "→ controllo percorsi protetti"
  for p in "${PROTETTI[@]}"; do
    test -f "$p" || { echo "ERRORE: manca $p"; exit 1; }
  done
fi

echo "→ commit dei sorgenti"
git add -A
git commit -m "$MSG" || echo "  (niente da committare)"
git push

echo "→ pubblicazione"
OUT=$(grep -E '^\s*output-dir\s*:' _quarto.yml 2>/dev/null \
      | head -1 | sed 's/.*: *//' | tr -d '"'"'"' ' || true)
OUT="${OUT:-docs}"
ghp-import -n -p -f "$OUT"

echo "Fatto."
