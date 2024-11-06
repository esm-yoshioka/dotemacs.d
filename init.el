;;; init.el --- My init.el  -*- coding: utf-8 ; lexical-binding: t -*-
;; 
;; Author : esm-yoshioka
;; Version: 29.4
;; 

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file)))))
  )

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init))
  )

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left)))
  )

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el")))
  )

;; Describe own settings below
;; =========================================================================================

(when load-file-name
  (setq user-emacs-directory
        (expand-file-name (file-name-directory load-file-name)))
  )

;; local-const
(defconst my:d:vars
  (expand-file-name "vars/" user-emacs-directory))
(unless (file-directory-p my:d:vars)
  (make-directory my:d:vars))

(defconst my:d:backup
  (expand-file-name "backup/" user-emacs-directory))
(unless (file-directory-p my:d:backup)
  (make-directory my:d:backup))

;; -----------------------------------------------------------------------------------------

(leaf languages
  :doc "language setting"
  :config
  (set-language-environment "Japanese")
  (set-default-coding-systems 'utf-8-unix)
  (prefer-coding-system 'utf-8)
  ;; font
  ;; (set-face-attribute 'default nil :family "HackGen" :height 120)
  ;; (set-face-attribute 'default nil :family "HackGen Console" :height 120)
  (set-face-attribute 'default nil :family "HackGen Console NF" :height 120)

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
  )

(leaf settings
  :doc "general settings"
  :config
  (leaf autorevert
    :doc "revert buffers when files on disk change"
    :custom (auto-revert-interval . 1)
    :global-minor-mode global-auto-revert-mode
    )

  (leaf image
    :doc "always display picture"
    :global-minor-mode auto-image-file-mode
    )

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

(leaf files
  :doc "system file"
  :setq
  (auto-save-file-name-transforms '((".*" "~/.emacs.d/backup/" t)))
  :config
  (leaf saveplace
    :doc "automatically save place in files"
    :custom
    `((save-place . t)
      (save-place-file
       . ,(expand-file-name "save-places" my:d:vars))
      )
    :hook (emacs-startup-hook . save-place-mode)
    :config
    (setq save-place-ingore-files-regexp
          (format "\\(%s\\)\\|\\(%s\\)"
                  save-place-ignore-files-regexp
                  tramp-file-name-regexp))
    )

  (leaf savehist
    :doc "Save minibuffer history"
    :custom `((savehist-file . ,(expand-file-name "history" my:d:vars)))
    :config
    (savehist-mode)
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

  (leaf recentf-ext :ensure t)

  ;; backupファイルを集約
  (setq backup-directory-alist (cons
                                (cons ".*"
                                      (expand-file-name my:d:backup))
                                backup-directory-alist)
        )
  ;; auto-saveファイルを集約
  (setq auto-save-file-name-transforms `((".*" ,(expand-file-name my:d:backup)
                                          t))
        )

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

(leaf search
  :doc "search settings"
  :custom
  ;; ignore upper/lower case
  (case-fold-search . t)
  (isearch-case-fold-search . t)
  (completion-ignore-case . t)
  (read-file-name-completion-ignore-case . t)
  (read-buffer-completion-ignore-case . t)

  :config
  (leaf isearch
    :bind (:isearch-mode-map
           ("C-h" . isearch-delete-char))
    )
  )

(leaf looks
  :doc "app style"
  :config
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
    :config
    (setq dashboard-banner-logo-title (concat "GNU/Emacs " emacs-version))
    (dashboard-setup-startup-hook)
    )

  (leaf color
    :config
    (custom-set-faces
     '(region ((t (:background "Purple4"))))
     '(mode-line ((t (:background "Blue Violet"))))
     '(hl-line ((t (:background "midnightblue"))))
     )
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

  (setq frame-title-format (format "emacs@%s : %%f" (system-name)))
  (set-frame-parameter nil 'alpha 85)              ; frame transparency
  (set-frame-parameter nil 'fullscreen 'maximized) ; fullscreen
  (set-frame-parameter nil 'cursor-type 'box)      ; cursor type
  (blink-cursor-mode 0)                            ; disable cursor blinking
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)              ; file size

  :custom
  (menu-bar-mode . nil)                 ; non-display menu-bar
  (scroll-bar-mode . nil)               ; non-display scroll-bar
  (tool-bar-mode . nil)                 ; non-display tool-bar
  (inhibit-startup-message . t)         ; non-display startup
  (initial-scratch-message . "")        ; scratch is null
  (show-paren-mode . t)                 ; hightlight matching paren
  (line-spacing . 0.25)                 ; line spacing size
  )

(leaf view
  ;; If not writable, leave in view-mode
  :preface
  (defmacro do-not-exit-view-mode-unless-writable-advice (f)
    `(defadvice ,f (around do-not-exit-view-mode-unless-writable activate)
       (if (and
            (buffer-file-name)
            (not view-mode-force-exit)
            (not (file-writable-p
                  (buffer-file-name))))
           (message "File is unwritable, so stay in view-mode.")
         ad-do-it)))
  :require view
  :setq (view-mode-force-exit)
  :config
  (defvar exclude-list
    (list
     "~/.emacs.d/"
     "~/work/"
     )
    "Directory List of not open for view-mode")
  (add-hook 'find-file-hook
            '(lambda nil
               (when (listp exclude-list)
                 (let ((inhibit-ptn (concat "^\\("
                                            (mapconcat
                                             '(lambda (str)
                                                (regexp-quote
                                                 (expand-file-name str)))
                                             exclude-list "\\|")
                                            "\\)")))
                   (unless (string-match inhibit-ptn buffer-file-name)
                     (when (file-exists-p buffer-file-name)
                       (read-only-mode t)
                       (view-mode t)))))))
  (do-not-exit-view-mode-unless-writable-advice view-mode-exit)
  (do-not-exit-view-mode-unless-writable-advice view-mode-disable)
  :bind(:view-mode-map
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

;; -----------------------------------------------------------------------------------------

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
  (migemo-options . '("-q" "--emacs"))
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
  :bind ("M-%" . anzu-query-replace)
  :custom
  (anzu-use-migemo . t)
  (anzu-minimum-input-length . 3)       ; count target
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
               ("C-l" . vertico-directory-up)))
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
  (completion-styles . '(orderless))
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
  ("C-," . bs-cycle-previous)
  ("C-." . bs-cycle-next)
  ("C-c s" . consult-line)
  ("C-c m" . consult-line-multi)
  ("C-c j" . consult-mark)
  ("C-c f" . consult-find)
  ("C-c r" . consult-ripgrep)
  ("M-y" . consult-yank-from-kill-ring)
  )

(leaf corfu
  :doc "COmpletion in Region FUnction"
  :ensure t
  :bind ("S-SPC" . 'corfu-insert-separator)  ; M-SPCだとWSL上のemacsで效かないので変更
  :custom
  (corfu-auto . t)                      ; corfu on
  (corfu-cycle . t)
  (corfu-auto-delay . 0.1)
  (corfu-auto-prefix . 2)
  (corfu-popupinfo-delay . 1.0)
  (corfu-min-width . 30)
  (corfu-preselect . 'prompt)
  :config
  (custom-set-faces
   '(corfu-default ((t (:background "navy"))))
   '(corfu-current ((t (:foreground "orange"))))
   )
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

;; -----------------------------------------------------------------------------------------

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :when (and
         (eq system-type 'gnu/linux)
         (executable-find "git"))
  :ensure t
  )

(leaf programs
  :config
  (leaf powershell :ensure t)
  (leaf csv-mode :ensure t)
  (leaf yaml-mode
    :ensure t
    :mode
    ("\\.ya?ml$" . yaml-mode)
    )
  (leaf markdown-mode
    :ensure t
    :mode
    ("\\.md\\'" . gfm-mode)
    )
  (leaf sql-mode
    :custom
    (sql-product . 'postgres)
    )
  (leaf indent-bars
    :doc "Highlight indentation with bars"
    :ensure t
    :hook
    prog-mode-hook
    web-mode-hook
    yaml-mode-hook
    )
  )

(leaf rainbow-delimiters
  :ensure t
  :hook
  prog-mode-hook
  web-mode-hook
  markdown-mode-hook
  :config
  (rainbow-delimiters-mode)
  )

;; -----------------------------------------------------------------------------------------

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
    ;; 選択中の次のWindowにチートシートを表示する
    (split-window-horizontally)
    (next-window-any-frame)
    (find-file-read-only "~/.emacs.d/cheatsheet.md")
    )
  )

;; -----------------------------------------------------------------------------------------

(leaf global-key
  :doc "global key bind"
  :bind*
  ("C-:" . undo-redo)
  ("C-t" . other-window)
  ("C-0" . delete-window)
  ("C-h" . delete-backward-char)
  ("C-x C-d" . dired)
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
