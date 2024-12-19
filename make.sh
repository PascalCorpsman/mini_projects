#!/usr/bin/env bash

function priv_clippit
(
    cat <<EOF
Usage: bash ${0} [OPTIONS]
Options:
    build   Build program
EOF
)

function priv_lazbuild
(
    if ! (which lazbuild); then
        source '/etc/os-release'
        case ${ID:?} in
            debian | ubuntu)
                sudo apt-get update
                sudo apt-get install -y lazarus{-ide-qt5,}
                ;;
        esac
    fi
    declare -r COMPONENTS='use/components.txt'
    if [[ -d "${COMPONENTS%%/*}" ]]; then
        git submodule update --init --recursive --force --remote
        if [[ -f "${COMPONENTS}" ]]; then
            while read -r; do
                if [[ -n "${REPLY}" ]] &&
                    ! (lazbuild --verbose-pkgsearch "${REPLY}") &&
                    ! (lazbuild --add-package "${REPLY}") &&
                    ! [[ -d "${COMPONENTS%%/*}/${REPLY}" ]]; then
                        declare -A VAR=(
                            [url]="https://packages.lazarus-ide.org/${REPLY}.zip"
                            [out]=$(mktemp)
                        )
                        wget --output-document "${VAR[out]}" "${VAR[url]}" >/dev/null
                        unzip -o "${VAR[out]}" -d "${COMPONENTS%%/*}/${REPLY}"
                        rm --verbose "${VAR[out]}"
                    fi
            done < "${COMPONENTS}"
        fi
        while read -r; do
            printf '%(%y-%m-%d_%T)T\x1b[32m\t:Add dependence {}\x1b[0m\n' -1
            lazbuild --add-package "${REPLY}" ||
                lazbuild --add-package-link "${REPLY}"
        done < <(find "${COMPONENTS%%/*}" -type 'f' -name '*.lpk' | sort)
    fi
    while read -r; do
        printf '%(%y-%m-%d_%T)T\x1b[32m\t:Build project {}\x1b[0m\n' -1
        if ! (lazbuild --no-write-project --recursive --no-write-project --widgetset=qt5 "${REPLY}"); then
            lazbuild --no-write-project --recursive --no-write-project --widgetset=qt5 "${REPLY}" 1>&2
        fi
    done < <(find 'miniprojects' -type 'f' -name '*.lpi' | sort)
)

function priv_main
(
    set -euo pipefail
    if ((${#})); then
        case ${1} in
            build) priv_lazbuild ;;
            *) priv_clippit ;;
        esac
    else
        priv_clippit
    fi
)

priv_main "${@}" >/dev/null
