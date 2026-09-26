#!/bin/sh
# deck.org under epresent, in an isolated Emacs.  Your Doom config is not touched.
#   DECK_THEME=light ./epresent.sh     start in light mode (t toggles on stage)
#   DECK_ALL=1 ./epresent.sh           every build revealed, for review
cd "$(dirname "$0")" || exit 1
exec /Applications/Emacs.app/Contents/MacOS/Emacs -Q \
  --eval '(setq native-comp-jit-compilation nil native-comp-enable-subr-trampolines nil native-comp-async-report-warnings-errors nil inhibit-automatic-native-compilation t warning-suppress-types (quote ((native-compiler) (comp))) warning-suppress-log-types (quote ((native-compiler) (comp))) warning-minimum-level :error)' \
  -l "$PWD/deck-present.el"
