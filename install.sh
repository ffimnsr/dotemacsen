#!/usr/bin/env bash
set -euo pipefail

TARGET_DIR="${HOME}/.emacs.d"
SOURCE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TIMESTAMP="$(date +%Y%m%d-%H%M%S)"

if ! command -v emacs >/dev/null 2>&1; then
  echo "Warning: 'emacs' command not found in PATH."
  echo "Install Emacs 30+ before launching this config."
fi

if [ -L "${TARGET_DIR}" ]; then
  CURRENT_LINK="$(readlink "${TARGET_DIR}")"
  if [ "${CURRENT_LINK}" = "${SOURCE_DIR}" ]; then
    echo "${TARGET_DIR} is already linked to this repo. Nothing to do."
    exit 0
  fi
fi

if [ -e "${TARGET_DIR}" ] || [ -L "${TARGET_DIR}" ]; then
  BACKUP_PATH="${TARGET_DIR}.backup.${TIMESTAMP}"
  mv "${TARGET_DIR}" "${BACKUP_PATH}"
  echo "Backed up existing ${TARGET_DIR} to ${BACKUP_PATH}"
fi

ln -s "${SOURCE_DIR}" "${TARGET_DIR}"

echo "Linked ${SOURCE_DIR} -> ${TARGET_DIR}"
echo "Next step: run 'emacs' and wait for straight.el bootstrap to finish."
