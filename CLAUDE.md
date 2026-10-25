# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal GNU Emacs (v31.1) configuration (`~/.emacs.d`, upstream `esm-yoshioka/dotemacs.d`).
The target version is recorded in the `Version:` header of both `init.el` and
`early-init.el`; keep them in sync when moving to a new Emacs release. The config already
relies on 30+/31 features (`grep-use-headings`, `visual-wrap-prefix-mode`,
`recentf-autosave-interval`, built-in `which-key`).
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
  `(message "Warning: ...")` rather than error when an optional dependency is missing.
  Existing examples: migemo/cmigemo (command + dictionary), tr-ime (`condition-case`),
  mozc (`mozc_emacs_helper`), vterm (`my:vterm-available-p`, gating with `:if`), git
  (magit `:if`), and the HackGen font (`font-family-list`). Preserve this — a broken
  external tool must not break startup.
- `migemo` (Japanese romaji→kanji incremental search) is a soft dependency threaded through
  `anzu`, `orderless`, and `consult` (ripgrep). Its integrations all check
  `(bound-and-true-p migemo-process)` before activating. Keep that guard on any new migemo use.

## Architecture / load order

1. **`early-init.el`** — runs before the package system and GUI init. Sets
   `user-emacs-directory`, GC / process-output tuning for startup, package archives
   (gnu, nongnu, melpa), coding systems (Japanese / utf-8-unix), **plain `setq` editing
   defaults** (indentation, scrolling, kill/mark ring, `use-short-answers`, Windows pipe
   tuning, ...) and frame defaults. Defines the key path constants `my:d:vars` (→ `vars/`)
   and `my:d:backup` (→ `backup/`) and creates both directories. Backup and auto-save files
   are redirected out of the working tree into `backup/`; lock files are disabled.
   Built-in settings that are not tied to a package go here rather than into a `leaf` block.
2. **`init.el`** — the main config: bootstraps `leaf` (+ `leaf-keywords`, `leaf-convert`),
   loads the optional untracked `local-config.el`, then configures everything section by
   section: Language (IME) → Operation → File → User Interface → View → Org → dirvish →
   Japanese Search (migemo) → Completion → Terminal → Git → SQL → Program → Original →
   Key Bind. Per-package state files (`savehist`, `recentf`, `saveplace`) are placed under
   `my:d:vars` via backquoted `:custom`; new packages that persist state should do the same.
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
  `C-<f1>` opens `cheatsheet.org`, `C-o` = `embark-act`, `C-z` = `winner-undo`,
  `C-x C-b` = `consult-buffer` / `C-x b` = `bs-show`. `M-.` / `M-,` are intentionally bound
  to `text-scale-increase/decrease` (via `:bind*`), not xref. See `cheatsheet.org` for the
  full user-facing keymap, and update it alongside any keybinding change.
- **view-mode**: files open read-only via `find-file-hook` except new files and VCS message
  buffers. `my:exclude-view-list` holds **exact base names** (`COMMIT_EDITMSG`, `MERGE_MSG`,
  `git-rebase-todo`, ...) matched with `member`, mirroring magit's
  `git-commit-filename-regexp` / `git-rebase-filename-regexp`. Adding a filename that should
  stay editable means extending that list. Do not go back to substring matching: since
  `case-fold-search` is t, `"TAG"` also matches `voltage.log` and `"COMMIT"` matches
  `committee.txt`.

## Testing changes

There is no test suite. Validate config changes by launching Emacs against this directory:

```
emacs --debug-init                              # interactive, full config, backtrace on error
emacs -Q --batch -l early-init.el -l init.el    # headless load check (may need display for some pkgs)
```

Prefer `--debug-init` for a real check, since much of the config depends on GUI frames,
fonts (HackGen Console NF), and per-monitor font sizing.

### Byte-compile check

`init.el` cannot be byte-compiled on its own — `emacs --batch -f batch-byte-compile init.el`
does not work. Two independent reasons:

- `--batch` implies `-q`, so `package-activate-all` never runs and the `leaf` macro stays
  undefined. Every `leaf` form is then compiled as a plain function call, which yields a flood
  of bogus `reference to free variable <block name>` warnings and finally a hard error on the
  dotted pairs in `:bind`.
- `leaf`'s `:custom` evaluates a backquoted list **at macro-expansion time**, so `my:d:vars`
  (a `defconst` in `early-init.el`) must already be bound. Compiling `init.el` without
  `early-init.el` fails with `void-variable my:d:vars`.

So load `early-init.el` and bootstrap `leaf` first. Also redirect the `.elc` output: a stale
`init.elc` left in this directory would be loaded in preference to `init.el` on every
subsequent startup, since `load-suffixes` puts `.elc` before `.el` and `load-prefer-newer`
is nil.

```
emacs -Q --batch -l early-init.el \
  --eval "(progn (require 'package) (package-initialize) \
                 (require 'leaf-keywords) (leaf-keywords-init) \
                 (setq byte-compile-dest-file-function \
                       (lambda (f) (expand-file-name (concat (file-name-nondirectory f) \"c\") \
                                                     temporary-file-directory))))" \
  -f batch-byte-compile init.el
```

This currently (Emacs 31.1) reports ~43 warnings, all of the expected "package not loaded at compile time"
kind (`assignment to free variable 'dashboard-*'`, `the function 'migemo-init' is not known to
be defined`, functions defined in a `:preface` not being visible later, ...). None of them are
actionable, so the value of this check is that macro expansion and compilation complete at
all — treat a *new kind* of warning, or a hard error, as the signal.

## Setup

From README:

- Fonts: `cd HackGen && ./fonts_cp.sh`
- Icons: `M-x nerd-icons-install-fonts`

External tools used at runtime (not in README):

- Linux: `cmigemo` (+ dict at `/usr/share/cmigemo/utf-8/migemo-dict`), `mozc_emacs_helper`,
  `ripgrep`, `git`, `keychain` (SSH agent env via `keychain-environment`), and
  `cmake` + `libtool` to build `vterm-module`.
- Windows: bundled `cmigemo-default-win64/` (gitignored), `tr-ime`, and Git for Windows
  (`C:/Program Files/Git/bin/git.exe` is preferred over `PATH`).

Runtime-generated, gitignored entries in this directory: `elpa/`, `eln-cache/`, `vars/`,
`transient/`, `tramp`, `custom.el`. `backup/` itself is not in `.gitignore`, but its
contents are ignored by the `*~` pattern.
