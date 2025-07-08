;;; init.el --- My init.el  -*- coding: utf-8 ; lexical-binding: t -*-
;; 
;; Author : esm-yoshioka
;; Version: 30.1
;; 

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file)))))
  )

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ))
  (package-initialize)
  (use-package leaf :ensure t)

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf blackout :ensure t)
    :config
    (leaf-keywords-init))
  )

(leaf leaf-convert
  :doc "Convert many format to leaf format"
  :ensure t
  )

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el")))
  )

;; Describe own settings below
;; =========================================================================================

;; local-const -----------------------------------------------------------------------------

(defun my:ensure-dir (dir)
  (unless (file-directory-p dir)
  (make-directory dir t)))

(defconst my:d:vars
  (expand-file-name "vars/" user-emacs-directory))
(defconst my:d:backup
  (expand-file-name "backup/" user-emacs-directory))

(my:ensure-dir my:d:vars)
(my:ensure-dir my:d:backup)

;; language --------------------------------------------------------------------------------

(leaf languages
  :config
  (set-language-environment "Japanese")
  (set-default-coding-systems 'utf-8-unix)
  (prefer-coding-system 'utf-8)
  ;; font
  ;; (set-face-attribute 'default nil :family "HackGen" :height 120)
  ;; (set-face-attribute 'default nil :family "HackGen Console" :height 120)
  (set-face-attribute 'default nil :family "HackGen Console NF" :height 120)
  )

(leaf windows-ime
  :when (eq system-type 'windows-nt)
  :config
  (leaf tr-ime
    :doc "Emulator of IME patch for Windows"
    :ensure t
    :config
    (tr-ime-advanced-install)
    )

  (setq default-input-method "W32-IME")
  (setq-default w32-ime-mode-line-state-indicator "[Aa]")
  (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))
  (w32-ime-initialize)
  ;; IME disable pattern
  (w32-ime-wrap-function-to-control-ime 'universal-argument)
  (w32-ime-wrap-function-to-control-ime 'read-string)
  (w32-ime-wrap-function-to-control-ime 'read-char)
  (w32-ime-wrap-function-to-control-ime 'read-from-minibuffer)
  (w32-ime-wrap-function-to-control-ime 'y-or-n-p)
  (w32-ime-wrap-function-to-control-ime 'yes-or-no-p)
  (w32-ime-wrap-function-to-control-ime 'map-y-or-n-p)
  (w32-ime-wrap-function-to-control-ime 'register-read-with-preview)
  )

(leaf linux-ime
  :when (eq system-type 'gnu/linux)
  :config
  (leaf mozc :ensure t)
  (leaf mozc-popup :ensure t)
  (setq default-input-method "japanese-mozc")
  )

;; general-setting -------------------------------------------------------------------------

(leaf settings
  :setq
  ;; default directory
  (default-directory . "~/")
  (command-line-default-directory . "~/")

  :custom
  (confirm-kill-emacs . 'y-or-n-p)      ; check on exit
  (use-short-answers . t)               ; y-or-n
  (echo-keystrokes . 0.1)               ; echo Area key-strokes
  (ring-bell-function . 'ignore)        ; error beep off
  (kill-ring-max . 200)                 ; keep kill-ring
  (mark-ring-max . 50)                  ; keep mark-ring
  (require-final-newline . t)           ; auto-insert last line
  (use-dialog-box . nil)                ; always using the echo area
  ;; tab
  (indent-tabs-mode . nil)
  (tab-width . 4)
  ;; scroll
  (scroll-conservatively . 1)
  (scroll-margin . 3)
  (next-screen-context-lines . 3)
  (scroll-preserve-screen-position . t)
  )

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :custom (auto-revert-interval . 1)
  :global-minor-mode global-auto-revert-mode
  )

(leaf image
  :doc "always display picture"
  :global-minor-mode auto-image-file-mode
  )

(leaf winner
  :doc "Restore old window configurations"
  :global-minor-mode winner-mode
  :bind
  ("C-z" . winner-undo)
  )

(leaf isearch
  :doc "incremental search minor mode"
  :bind (:isearch-mode-map
         ("C-h" . isearch-delete-char)
         )
  :custom
  (case-fold-search . t)
  (isearch-case-fold-search . t)
  (completion-ignore-case . t)
  (read-file-name-completion-ignore-case . t)
  (read-buffer-completion-ignore-case . t)
  )

(leaf tab-bar
  :doc "frame-local tabs with named persistent window configurations"
  :custom-face
  (tab-bar-tab . '((t (:foreground "chocolate"))))
  (tab-bar-tab-inactive . '((t (:foreground "DarkGrey"))))
  :config
  (tab-bar-mode t)
  )

;; file ------------------------------------------------------------------------------------

(leaf files
  :config
  ;; backupファイルを集約
  (setq backup-directory-alist `((".*" . ,(expand-file-name my:d:backup))))
  (setq auto-save-file-name-transforms `((".*" ,(expand-file-name my:d:backup) t)))

  :custom
  ;; backup file
  (backup-by-copying . t)
  (version-control . t)
  (kept-new-versions . 50)
  (kept-old-versions . 0)
  (delete-old-versions . t)
  ;; auto-save file
  (auto-save-timeout . 15)
  (auto-save-interval . 120)
  (auto-save-list-file-prefix . nil)
  ;; lock file
  (create-lockfiles . nil)
  )

(leaf savehist
  :doc "Save minibuffer history"
  :global-minor-mode savehist-mode
  :custom `((savehist-file . ,(expand-file-name "history" my:d:vars)))
  )

(leaf recentf
  :doc "keep track of recently opened files"
  :preface
  (defmacro with-suppressed-message (&rest body)
    "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
    (declare
     (indent 0))
    (let ((message-log-max nil))
      `(with-temp-message (or (current-message) "") ,@body)))
  :custom
  `((recentf-save-file . ,(expand-file-name "recentf" my:d:vars))
    (recentf-max-saved-items . 2000)
    (recentf-auto-cleanup . 'never)
    (recentf-exclude . '("recentf"))
    )
  :config
  (run-with-idle-timer 60 t '(lambda () ; 60s
                               (with-suppressed-message (recentf-save-list))))
  (recentf-mode)
  )

(leaf recentf-ext
  :doc "Recentf extensions"
  :ensure t
  )

(leaf saveplace
  :doc "automatically save place in files"
  :custom `((save-place-file . ,(expand-file-name "save-places" my:d:vars)))
  :config
  (save-place-mode 1)
  )

;; looks -----------------------------------------------------------------------------------

(leaf style
  :custom-face
  (show-paren-match . '((t (:foreground "DeepSkyBlue"))))
  :config
  (setq frame-title-format (format "emacs@%s : %%f" (system-name)))
  (set-frame-parameter nil 'alpha 85)              ; frame transparency
  (set-frame-parameter nil 'fullscreen 'maximized) ; fullscreen
  (set-frame-parameter nil 'cursor-type 'box)      ; cursor type
  (blink-cursor-mode 0)                            ; disable cursor blinking
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)              ; file size
  (global-display-line-numbers-mode t)
;  (custom-set-variables '(display-line-numbers-width-start t))
  (setq show-paren-style 'expression)

  :custom
  (menu-bar-mode . nil)                 ; non-display menu-bar
  (scroll-bar-mode . nil)               ; non-display scroll-bar
  (tool-bar-mode . nil)                 ; non-display tool-bar
  (inhibit-startup-message . t)         ; non-display startup
  (initial-scratch-message . "")        ; scratch is null
  (line-spacing . 0.25)                 ; line spacing size
  (show-paren-mode . t)                 ; hightlight matching paren
  (display-line-numbers-width-start . t) ; display line-number
  )

(leaf doom-themes
  :doc "Modern color-themes"
  :ensure t
  :hook (after-init-hook . (lambda () (load-theme 'doom-vibrant t)))
  :custom
  (doom-themes-enable-italic . nil)
  (doom-themes-enable-bold . nil)
  )

(leaf dashboard
  :doc "A startup screen extracted from Spacemacs."
  :ensure t
  :setq
  (dashboard-startup-banner . 'logo)
  (dashboard-center-content . t)
  (dashboard-vertically-center-content . t)
  (dashboard-items . '((recents . 20)
                       ))
  (dashboard-display-icons-p . t)
  (dashboard-icon-type . 'nerd-icons)
  (dashboard-set-heading-icons . t)
  (dashboard-set-file-icons . t)
  :config
  (setq dashboard-banner-logo-title (concat "GNU/Emacs " emacs-version))
  (dashboard-setup-startup-hook)
  )

(leaf nerd-icons
  :doc "Emacs Nerd Font Icons Library. Installing fonts with [M-x nerd-icons-install-fonts]"
  :ensure t
  )

(leaf nerd-icons-completion
  :doc "Add icons to completion candidates"
  :ensure t
  :after marginalia
  :hook ((marginalia-mode-hook . nerd-icons-completion-marginalia-setup))
  :config
  (nerd-icons-completion-mode)
  )

(leaf nerd-icons-dired
  :doc "Shows icons for each file in dired mode"
  :ensure t
  :after nerd-icons
  :hook ((dired-mode-hook . nerd-icons-dired-mode))
  )

(leaf nerd-icons-corfu
  :doc "Icons for Corfu via nerd-icons"
  :ensure t
  :after corfu nerd-icons
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  )

(leaf color
  :custom-face
  (region . '((t (:background "Purple4"))))
  (mode-line . '((t (:background "DarkOrchid4"))))
  (hl-line . '((t (:background "midnightblue"))))  
  :config
  (global-hl-line-mode t)
  :custom
  (transient-mark-mode . t)
  )

(leaf uniquify
  :doc "unique buffer names dependent on file name"
  :custom
  ((uniquify-buffer-name-style . 'post-forward-angle-brackets)
   (uniquify-min-dir-content . 2))    ; directory hierarchy
  )

;; view ------------------------------------------------------------------------------------

(leaf view
  :doc "Open new file or non-specified file in view-mode"
  :hook
  (find-file-hook . exclude-view-list-mode)
  :config
  (defvar exclude-view-list
    '("COMMIT" "MERGE" "TAG" "PULLREQ" "REBASE"))

  (defun exclude-view-list-mode ()
    (let* ((filename (buffer-file-name))
           (basename (and filename (file-name-nondirectory filename)))
           (exclude-list-related-file-p
            (and basename
                 (cl-some (lambda (keyword)
                            (string-match-p (regexp-quote keyword) basename))
                          exclude-view-list)))
           (new-file-p (and filename (not (file-exists-p filename)))))
      (unless (or exclude-list-related-file-p new-file-p)
        (view-mode 1))))
  :bind (:view-mode-map
         ("h" . backward-char)
         ("l" . forward-char)
         ("j" . next-line)
         ("k" . previous-line)
         ("a" . move-beginning-of-line)
         ("e" . move-end-of-line)
         ("n" . scroll-up)
         ("p" . scroll-down)
         )
  )

;; package ---------------------------------------------------------------------------------

(leaf migemo-linux
  :when (and
         (eq system-type 'gnu/linux)
         (executable-find "cmigemo"))
  :custom
  (migemo-command . "cmigemo")
  (migemo-dictionary . "/usr/share/cmigemo/utf-8/migemo-dict")
  )

(leaf migemo-win
  :when (and
         (eq system-type 'windows-nt)
         (file-exists-p "D:/Home/.emacs.d/cmigemo-default-win64/dict/utf-8/migemo-dict"))
  :custom
  (migemo-command . "D:/Home/.emacs.d/cmigemo-default-win64/cmigemo.exe")
  (migemo-dictionary . "D:/Home/.emacs.d/cmigemo-default-win64/dict/utf-8/migemo-dict")
  )

(leaf migemo
  :doc "Japanese incremental search through dynamic pattern expansion"
  :ensure t
  :require t
  :custom
  (migemo-options . '("-q" "--emacs" "--nonewline"))
  (migemo-user-dictionary . nil)
  (migemo-regex-dictionary . nil)
  (migemo-coding-system . 'utf-8-unix)
  :config
  (migemo-init)
  )

(leaf anzu
  :doc "display current count & total match"
  :ensure t
  :require t
  :after migemo
  :bind
  ("M-%" . anzu-query-replace)
  :custom
  (anzu-use-migemo . t)
  (anzu-minimum-input-length . 3)       ; count target
  (anzu-replace-to-string-separator . " => ")
  :config
  (global-anzu-mode)
  )

(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :ensure t
  :custom
  (vertico-count . 20)                  ; max display num
  (vertico-cycle . t)
  (vertico-resize . t)
  :config
  (vertico-mode)
  )

(leaf extensions/vertico-directory
  :ensure nil
  :config
  (with-eval-after-load 'vertico
    (unless (fboundp 'vertico-directory-up)
      (autoload 'vertico-directory-up "extensions/vertico-directory" nil t))
    (bind-keys :package extensions/vertico-directory
               :map vertico-map
               ("C-l" . vertico-directory-up))
    )
  )

(leaf marginalia
  :doc "enable richer annotations"
  :ensure t
  :config
  (marginalia-mode)
  )

(leaf orderless
  :doc "fuzzy completion"
  :ensure t
  :custom
  (completion-styles . '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  )

(leaf orderless-migemo
  :doc "using migemo with orderless"
  :after migemo orderless
  :config
  (defun orderless-migemo (component)
    (let ((pattern (migemo-get-pattern component)))
      (condition-case nil
          (progn (string-match-p pattern "") pattern)
        (invalid-regexp nil))))
  (orderless-define-completion-style orderless-default-style
	(orderless-matching-styles '(orderless-initialism
								 orderless-literal
								 orderless-regexp)))
  (orderless-define-completion-style orderless-migemo-style
	(orderless-matching-styles '(orderless-initialism
								 orderless-literal
								 orderless-regexp
								 orderless-migemo)))
  :custom
  (completion-category-overrides .
        '((command (styles orderless-default-style))           ; M-x
          (file (styles orderless-migemo-style))               ; find-file
          (buffer (styles orderless-migemo-style))
          (symbol (styles orderless-default-style))
          (consult-location (styles orderless-migemo-style))   ; consult-line etc
          (consult-multi (styles orderless-migemo-style))      ; consult-buffer etc
          (unicode-name (styles orderless-migemo-style))
          (variable (styles orderless-default-style))))
  )

(leaf consult
  :doc "Consulting completing-read"
  :ensure t
  :bind
  ("C-x b" . consult-buffer)
  ("C-x C-b" . bs-show)
  ("C-x C-o" . consult-recent-file)
  ("C-." . bs-cycle-next)
  ("C-," . bs-cycle-previous)
  ("C-c s" . consult-line)
  ("C-c m" . consult-line-multi)
  ("C-c j" . consult-mark)
  ("C-c f" . consult-find)
  ("C-c r" . consult-ripgrep)
  ("M-y" . consult-yank-from-kill-ring)
  )

(leaf consult-dir
  :doc "Insert paths into the minibuffer prompt"
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         (vertico-map
          ("C-x C-d" . consult-dir)))
  )

;; ----- consult-ripgrep default function -----
;; (defun consult--default-regexp-compiler (input type ignore-case)
;;   (setq input
;;         (consult--split-escaped input))
;;   (cons (mapcar (lambda (x) (consult--convert-regexp x type))
;;                 input)
;;         (when-let (regexps (seq-filter #'consult--valid-regexp-p
;;                                        input))
;;           (apply-partially #'consult--highlight-regexps regexps ignore-case)))
;;   )
;;
;; --- migemo対応 ---
;; (defvar consult--migemo-regexp "")
;; (defun consult--migemo-regexp-compiler (input type ignore-case)
;;   (setq consult--migemo-regexp
;;         (mapcar #'migemo-get-pattern (consult--split-escaped input)))
;;   (cons (mapcar (lambda (x) (consult--convert-regexp x type))
;;                 consult--migemo-regexp)
;;         (when-let (regexps (seq-filter #'consult--valid-regexp-p
;;                                        consult--migemo-regexp))
;;           (apply-partially #'consult--highlight-regexps regexps ignore-case)))
;;   )
;; (setq consult--regexp-compiler #'consult--migemo-regexp-compiler)
(leaf consult-ripgrep-migemo
  :preface
  (defun consult--migemo-regexp-compiler (input type ignore-case)
    (setq consult--migemo-regexp (mapcar #'migemo-get-pattern
                                         (consult--split-escaped input)))
    (cons
     (mapcar
      (lambda (x)
        (consult--convert-regexp x type))
      consult--migemo-regexp)
     (when-let (regexps
                (seq-filter #'consult--valid-regexp-p consult--migemo-regexp))
       (apply-partially #'consult--highlight-regexps regexps ignore-case))))

  :setq ((consult--migemo-regexp . "")
         (consult--regexp-compiler function consult--migemo-regexp-compiler))
  )

(leaf corfu
  :doc "COmpletion in Region FUnction"
  :ensure t
  :bind
  ("S-SPC" . 'corfu-insert-separator)  ; M-SPCだとWSL上のemacsで效かないので変更
  :custom-face
  (corfu-default . '((t (:background "midnightblue"))))
  (corfu-current . '((t (:foreground "orange"))))
  :custom
  (corfu-auto . t)                      ; corfu on
  (corfu-cycle . t)
  (corfu-auto-delay . 0.1)
  (corfu-auto-prefix . 2)
  (corfu-popupinfo-delay . 0.5)
  (corfu-min-width . 30)
  (corfu-preselect . 'prompt)
  (text-mode-ispell-word-completion . nil)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  )

(leaf cape
  :doc "Completion At Point Extensions"
  :ensure t
  :config
  (add-to-list 'completion-at-point-functions 'cape-dabbrev)
  (add-to-list 'completion-at-point-functions 'cape-file)
  (add-to-list 'completion-at-point-functions 'cape-elisp-block)
  )

;; program ---------------------------------------------------------------------------------

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :if (cond
        ((eq system-type 'windows-nt)
         (let ((git-path "C:/Program Files/Git/bin/git.exe"))
           (when (file-executable-p git-path)
             (setq magit-git-executable git-path)
             t)))
        ((eq system-type 'gnu/linux)
         (executable-find "git")))
  :ensure t
  )

(leaf git-gutter
  :doc "Port of Sublime Text plugin GitGutter."
  :ensure t
  :global-minor-mode global-git-gutter-mode
  :bind
  ("C-x v p" . git-gutter:previous-hunk)
  ("C-x v n" . git-gutter:next-hunk)
  ("C-x v r" . git-gutter:revert-hunk)
  ("C-x v SPC" . git-gutter:popup-hunk)
  :custom
  ((git-gutter:added-sign . "++")
   (git-gutter:deleted-sign . "--")
   (git-gutter:modified-sign . "=="))
  )

(leaf powershell
  :doc "Mode for editing PowerShell scripts"
  :ensure t
  )

(leaf csv-mode
  :doc "Major mode for editing comma/char separated values"
  :ensure t
  )

(leaf yaml-mode
  :doc "Major mode for editing YAML files"
  :ensure t
  :mode
  ("\\.ya?ml$" . yaml-mode)
  )

(leaf markdown-mode
  :doc "Major mode for Markdown-formatted text"
  :ensure t
  :mode
  ("\\.md\\'" . gfm-mode)
  )

(leaf sql-mode
  :custom
  (sql-product . 'postgres)
  )

(leaf sql-indent
  :doc "Support for indenting code in SQL files."
  :ensure t
  :setq
  (sql-indent-offset . 2)
  )

(leaf indent-bars
  :doc "Highlight indentation with bars"
  :ensure t
  :hook
  prog-mode-hook
  web-mode-hook
  yaml-mode-hook
  )

(leaf rainbow-delimiters
  :doc "Highlight brackets according to their depth."
  :ensure t
  :hook
  prog-mode-hook
  web-mode-hook
  markdown-mode-hook
  )

;; original --------------------------------------------------------------------------------

(leaf set-alpha
  :doc "change transparency"
  :preface
  (defun set-alpha (alpha-mun)
    "set frame parameter 'alpha"
    (interactive "nAlplha: ")
    (set-frame-parameter nil 'alpha
                         (cons alpha-mun
                               '(90))))
  )

(leaf open-cheat
  :doc "open my-cheatsheet other windows"
  :preface
  (defun open-cheat ()
    (interactive)
    (split-window-horizontally)
    (next-window-any-frame)
    (find-file-read-only "~/.emacs.d/cheatsheet.md")
    )
  )

;; key-bind --------------------------------------------------------------------------------

(leaf global-key
  :doc "global key bind"
  :bind
  ("C-:" . undo-redo)
  ("C-t" . other-window)
  ("C-0" . delete-window)
  ("C-h" . delete-backward-char)
  ("M-," . text-scale-decrease)
  ("M-." . text-scale-increase)
  ("M-<up>" . balance-windows)
  ("M-<down>" . enlarge-window)
  ("M-<left>" . shrink-window-horizontally)
  ("M-<right>" . enlarge-window-horizontally)
  ("C-<f1>" . open-cheat)
  )

;; =========================================================================================

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
