# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal GNU Emacs (v30.2) configuration (`~/.emacs.d`, upstream `esm-yoshioka/dotemacs.d`).
All comments and documentation are written in Japanese. Configuration must work across
**two platforms** — `windows-nt` and `gnu/linux` (WSL) — so nearly every OS-specific
feature is guarded with `(eq system-type ...)`.

## Editing conventions

- Every package is declared with the **`leaf`** macro (not `use-package`). New packages
  follow the existing `leaf NAME :ensure t ...` pattern with a `:doc` string. Sections are
  separated by the `;; ----` banner comments in `init.el`.
- `:ensure t` = install from MELPA/ELPA; `:ensure nil` = built-in / extension of an
  already-installed package (e.g. `vertico-directory`).
- Guard anything platform- or executable-dependent. The codebase's style is to **degrade
  gracefully**: check `executable-find` / `file-exists-p` / `bound-and-true-p`, and
  `(message "Warning: ...")` rather than error when an optional dependency (migemo/cmigemo,
  tr-ime, git) is missing. Preserve this — a broken external tool must not break startup.
- `migemo` (Japanese romaji→kanji incremental search) is a soft dependency threaded through
  `anzu`, `orderless`, and `consult` (ripgrep). Its integrations all check
  `(bound-and-true-p migemo-process)` before activating. Keep that guard on any new migemo use.

## Architecture / load order

1. **`early-init.el`** — runs before the package system and GUI init. Sets
   `user-emacs-directory`, GC tuning for startup, package archives, coding systems
   (Japanese / utf-8-unix), frame defaults, and **all backup/auto-save/vars directory
   paths**. Defines the key path constants `my:d:vars` (→ `vars/`) and `my:d:backup`
   (→ `backup/`), which `init.el` references. Backup and auto-save files are redirected out
   of the working tree into `backup/`.
2. **`init.el`** — the main config: bootstraps `leaf`, loads the optional untracked
   `local-config.el`, then configures everything (IME, completion stack, dirvish, magit,
   sql, etc.) section by section.
3. **`custom.el`** — machine-written by Emacs Custom (`custom-file`). **Do not hand-edit**;
   it is gitignored.
4. **`local-config.el`** — untracked, gitignored, loaded early by `init.el` with a missing-ok
   flag. Holds machine-local secrets/vars such as `my/pg-user` and `my/pg-databases` (consumed
   by the `sql` block to build `sql-connection-alist`). Referenced but never committed.

## Key subsystems

- **Completion stack**: `vertico` + `marginalia` + `orderless` + `consult` + `corfu`/`cape` +
  `embark` + `nerd-icons`. `orderless-migemo` defines custom completion styles wired per
  category via `completion-category-overrides`.
- **File manager**: `dirvish` (overrides dired). `dirvish-quick-access-entries` are
  per-platform.
- **Custom keybindings of note**: `C-h` is rebound to `delete-backward-char` globally (and
  `isearch-delete-char` in isearch) — `<f1>` is the help prefix instead. `C-t` = other-window,
  `C-<f1>` opens `cheatsheet.org`. See `cheatsheet.org` for the full user-facing keymap.
- **view-mode**: files open read-only via `find-file-hook` except new files and VCS message
  buffers (COMMIT/MERGE/TAG/PULLREQ/REBASE — see `my:exclude-view-list`). Adding a filename
  that should stay editable means extending that list.

## Testing changes

There is no test suite. Validate config changes by launching Emacs against this directory:

```
emacs --debug-init                    # interactive, full config, backtrace on error
emacs -Q --batch -l early-init.el -l init.el   # headless load check (may need display for some pkgs)
emacs --batch -f batch-byte-compile init.el    # byte-compile to surface warnings
```

Prefer `--debug-init` for a real check, since much of the config depends on GUI frames,
fonts (HackGen Console NF), and per-monitor font sizing.

## Setup (from README)

- Fonts: `cd HackGen && ./fonts_cp.sh`
- Icons: `M-x nerd-icons-install-fonts`
- Linux extras used at runtime: `cmigemo` (+ dict at `/usr/share/cmigemo/utf-8/migemo-dict`),
  `mozc`, `ripgrep`, `git`, `vterm` deps. Windows uses bundled `cmigemo-default-win64/`
  (gitignored) and `tr-ime`.
