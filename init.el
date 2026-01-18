;;; init.el --- My init.el  -*- coding: utf-8 ; lexical-binding: t -*-
;; 
;;   Author : esm-yoshioka
;;   Version: 30.2
;;

;; ------------------------------------------------------
;; Package system bootstrap
;; ------------------------------------------------------
(require 'package)
(package-initialize)

;; ------------------------------------------------------
;;    leaf
;; ------------------------------------------------------
(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf)
  )

(leaf leaf-keywords
  :ensure t
  :config
  ;; initialize leaf-keywords.el
  (leaf-keywords-init)
  )

(leaf leaf-convert
  :ensure t
  )


;; ------------------------------------------------------
;;    Language
;; ------------------------------------------------------
(leaf custom-font-monitor-switcher
  :doc "Adjust font size to match resolution"
  :preface
  (defun my/apply-font-size-per-monitor (&optional frame)
    (interactive)
    (let* ((f (or frame (selected-frame)))
           (monitor-attrs (frame-monitor-attributes f))
           (geometry (assoc 'geometry monitor-attrs))
           (width (nth 3 geometry))
           (font-height (if (>= width 2880) 200 120)))
      (unless (eq (face-attribute 'default :height f) font-height)
        (when (member "HackGen Console NF" (font-family-list))
          (set-face-attribute 'default f 
                              :family "HackGen Console NF"
                              :height font-height)))))
  :config
  (add-hook 'window-setup-hook #'my/apply-font-size-per-monitor)
  (add-hook 'after-make-frame-functions #'my/apply-font-size-per-monitor)
  (add-hook 'window-configuration-change-hook #'my/apply-font-size-per-monitor)
  (add-hook 'focus-in-hook #'my/apply-font-size-per-monitor)
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


;; ------------------------------------------------------
;;    Operation
;; ------------------------------------------------------
(leaf autorevert
  :doc "revert buffers when files on disk change"
  :global-minor-mode global-auto-revert-mode
  :custom (auto-revert-interval . 1)
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
  :global-minor-mode tab-bar-mode
  :custom-face
  (tab-bar-tab . '((t (:foreground "chocolate"))))
  (tab-bar-tab-inactive . '((t (:foreground "DarkGrey"))))
  )

(leaf uniquify
  :doc "unique buffer names dependent on file name"
  :custom
  ((uniquify-buffer-name-style . 'post-forward-angle-brackets)
   (uniquify-min-dir-content . 2))    ; directory hierarchy
  )

(leaf vundo
  :doc "Visual undo tree"
  :ensure t
  :bind(("C-;" . vundo)
        ("C-/" . undo-only)
        ("C-:" . undo-redo))
  )

(leaf anzu
  :doc "display current count & total match"
  :ensure t
  :require t
  :after migemo
  :bind
  ("M-%" . anzu-query-replace)
  :global-minor-mode global-anzu-mode
  :custom
  (anzu-use-migemo . t)
  (anzu-minimum-input-length . 3)       ; count target
  (anzu-replace-to-string-separator . " => ")
  )


;; ------------------------------------------------------
;;    File
;; ------------------------------------------------------
(leaf savehist
  :doc "Save minibuffer history"
  :global-minor-mode savehist-mode
  :custom `((savehist-file . ,(expand-file-name "history" my:d:vars)))
  )

(leaf recentf
  :doc "Keep track of recently opened files"
  :custom
  `((recentf-save-file . ,(expand-file-name "recentf" my:d:vars))
    (recentf-max-saved-items . 2000)
    (recentf-auto-cleanup . 'never)
    (recentf-exclude . '("recentf" "\\.elc$")))
  :config
  ;; Save recentf list periodically without polluting *Messages*
  (run-with-idle-timer
   60 t
   (lambda ()
     (let ((message-log-max nil))
       (with-temp-message (or (current-message) "")
         (recentf-save-list)))))
  :hook
  (after-init-hook . recentf-mode)
  )

(leaf saveplace
  :doc "automatically save place in files"
  :global-minor-mode save-place-mode
  :custom `((save-place-file . ,(expand-file-name "save-places" my:d:vars)))
  )


;; ------------------------------------------------------
;;    User Interface
;; ------------------------------------------------------
(leaf style
  :custom-face
  (show-paren-match . '((t (:foreground "DeepSkyBlue" :background "DarkSlateGray" :weight bold))))
  :config
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)              ; file size
  (global-display-line-numbers-mode t)
  (setq show-paren-style 'expression)

  :custom
  (show-paren-mode . t)                  ; hightlight matching paren
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

;; (leaf nerd-icons-dired
;;   :doc "Shows icons for each file in dired mode"
;;   :ensure t
;;   :after nerd-icons
;;   :hook ((dired-mode-hook . nerd-icons-dired-mode))
;;   )

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
  (mode-line . '((t (:background "MediumPurple4"))))
  (hl-line . '((t (:background "MidnightBlue"))))
  :config
  (global-hl-line-mode t)
  :custom
  (transient-mark-mode . t)
  )

(leaf ansi-color
  :preface
  (defun my/ansi-colorize-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  :hook
  (compilation-filter-hook . my/ansi-colorize-buffer)
  )


;; ------------------------------------------------------
;;    View
;; ------------------------------------------------------
(leaf view
  :doc "Open new file or non-specified file in view-mode"
  :hook
  (find-file-hook . my/exclude-view-list-mode)
  :config
  (defconst my:exclude-view-list
    '("COMMIT" "MERGE" "TAG" "PULLREQ" "REBASE"))

  (defun my/exclude-view-list-mode ()
    (let* ((filename (buffer-file-name))
           (basename (and filename (file-name-nondirectory filename)))
           (exclude-list-related-file-p
            (and basename
                 (cl-some (lambda (keyword)
                            (string-match-p (regexp-quote keyword) basename))
                          my:exclude-view-list)))
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


;; ------------------------------------------------------
;;    dirvish
;; ------------------------------------------------------
(leaf dirvish
  :doc "A modern file manager based on dired mode."
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :bind (
         ("C-c d" . dirvish-side)
         ("C-x d" . dirvish)
         (:dirvish-mode-map
          ("TAB" . dirvish-subtree-toggle)
          ("a" . dirvish-quick-access)
          ("h" . dired-up-directory)
          ("l" . dired-find-file)
          )
         )
  :custom
  (dirvish-attributes . '(
                          ;; vc-state      ; Gitのマーク(M, Uなど)を優先表示
                          nerd-icons
                          file-time
                          file-size
                          file-modes
                          ))
  (dirvish-side-attributes . '(
                               ;; vc-state
                               nerd-icons
                               file-time
                               ))
  (dirvish-side-width . 45)
  (dired-recursive-copies . 'always)
  (dired-recursive-deletes . 'always)
  (dired-dwim-target . t)
  (ls-lisp-dirs-first . t)
  :config
  (setq dirvish-quick-access-entries
        '(("e" "~/.emacs.d" "Emacs")
          ("g" "~/git" "Git Directory")
          ("w" "~/work" "Work")))
  (dirvish-peek-mode)
  (dirvish-side-follow-mode)
  )


;; ------------------------------------------------------
;;    Japanese Search
;; ------------------------------------------------------
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


;; ------------------------------------------------------
;;    Completion
;; ------------------------------------------------------
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
  ("C-x C-b" . consult-buffer)
  ("C-x b" . bs-show)
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
  (corfu-default . '((t (:background "MidnightBlue"))))
  (corfu-current . '((t (:foreground "orange" :background "SlateBlue4"))))
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
  (add-to-list 'completion-at-point-functions 'cape-dabbrev) ; current buffer
  (add-to-list 'completion-at-point-functions 'cape-keyword) ; programming keyword
  (add-to-list 'completion-at-point-functions 'cape-file)    ; file name
  )


;; ------------------------------------------------------
;;    Terminal
;; ------------------------------------------------------
(leaf vterm
  :when (eq system-type 'gnu/linux)
  :ensure t
  :custom
  (vterm-max-scrollback . 10000)
  (vterm-buffer-name-string . "vterm: %s")
  ;; delete "C-h", add <f1>, <f2>
  (vterm-keymap-exceptions
    . '("<f1>" "<f2>" "C-c" "C-x" "C-u" "C-g" "C-l" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y"))
  )

(leaf vterm-toggle
  :when (eq system-type 'gnu/linux)
  :ensure t
  :bind
  ("<f2>" . vterm-toggle)
  (:vterm-mode-map
   ("C-<f2>" . my/vterm-new-buffer-in-current-window)
   ("C-." . vterm-toggle-forward)
   ("C-," . vterm-toggle-backward)
   )
  :custom
  (vterm-toggle-scope . 'project)
  :config
  ;; Show vterm buffer in the window located at bottom
  (add-to-list 'display-buffer-alist
               '((lambda (bufname _)
                   ;; vterm-toggle のバッファ名は通常 "*vterm" または "vterm:" で始まるのでチェック
                   (or (string-prefix-p "*vterm" bufname)
                       (string-prefix-p "vterm:" bufname)))
                 (display-buffer-reuse-window display-buffer-in-direction)
                 (direction . bottom)
                 (reusable-frames . visible)
                 (window-height . 0.4)))
  ;; Above display config affects all vterm command, not only vterm-toggle
  (defun my/vterm-new-buffer-in-current-window()
    (interactive)
    (let ((display-buffer-alist nil))
            (vterm)))
  )


;; ------------------------------------------------------
;;    Git
;; ------------------------------------------------------
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

(leaf diff-hl
  :doc "Highlight uncommitted changes using VC"
  :ensure t
  :global-minor-mode global-diff-hl-mode
  :bind (("M-g p" . diff-hl-previous-hunk)
         ("M-g n" . diff-hl-next-hunk)
         ("M-g r" . diff-hl-revert-hunk)
         ("M-g s" . diff-hl-show-hunk)
         )
  :config
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  )


;; ------------------------------------------------------
;;    SQL
;; ------------------------------------------------------
(leaf sql-mode
  :custom
  (sql-set-product . 'postgres)
  (sql-connection-alist .
                        '(("local-db"
                           (sql-product 'postgres)
                           (sql-server "localhost")
                           (sql-port 5432))))
  )

(leaf sql-indent
  :doc "Support for indenting code in SQL files."
  :ensure t
  :hook (sql-mode-hook . sqlind-minor-mode)
  :custom
  (sql-indent-offset . 2)
  (sql-indent-maybe-tab . t)
  )

(leaf sqlup-mode
  :ensure t
  :hook
  (sql-mode-hook . sqlup-mode)
  )


;; ------------------------------------------------------
;;    Program
;; ------------------------------------------------------
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

(leaf indent-bars
  :doc "Highlight indentation with bars"
  :ensure t
  :hook ((prog-mode-hook . indent-bars-mode)
         (yaml-mode-hook . indent-bars-mode))
  )

(leaf rainbow-delimiters
  :doc "Highlight brackets according to their depth."
  :ensure t
  :hook ((prog-mode-hook . rainbow-delimiters-mode))
  )


;; ------------------------------------------------------
;;    Original
;; ------------------------------------------------------
(leaf open-cheat
  :doc "open my-cheatsheet other windows"
  :preface
  (defun my/open-cheat ()
    (interactive)
    (let* ((target-path (expand-file-name "~/.emacs.d/cheatsheet.md"))
           (current-path (buffer-file-name))
           (buffer (find-buffer-visiting target-path)))
      (unless (and current-path
                   (string= (expand-file-name current-path) target-path))
        (if buffer
            (switch-to-buffer-other-window buffer)
          (find-file-read-only-other-window target-path)))))
  )


;; ------------------------------------------------------
;;    Key Bind
;; ------------------------------------------------------
(leaf global-key
  :doc "global key bind"
  :bind
  ("C-h" . delete-backward-char)
  :bind*
  ("C-t" . other-window)
  ("C-0" . delete-window)
  ("M-," . text-scale-decrease)
  ("M-." . text-scale-increase)
  ("M-<up>" . balance-windows)
  ("M-<down>" . enlarge-window)
  ("M-<left>" . shrink-window-horizontally)
  ("M-<right>" . enlarge-window-horizontally)
  ("C-<f1>" . my/open-cheat)
  )


;;; init.el ends her
