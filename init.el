;;; init.el --- My init.el  -*- coding: utf-8 ; lexical-binding: t -*-
;; 
;; Author : esm-yoshioka
;; Version: 29.3
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
  :tag "builtin" "faces" "help"
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

  (leaf wsl-ime
    :when (eq system-type 'gnu/linux)
    :config
    (leaf mozc
      :doc "japanese input method"
      :ensure t)
    (leaf mozc-popup
      :ensure t)
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
    :custom
    `((savehist-file . ,(expand-file-name "history" my:d:vars)))
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

  (leaf recentf-ext
    :doc "Recentf extensions"
    :ensure t
    )

  :custom
  ;; backup file
  ;; issue_wsl: wsl上で実行してるとbackup設定が効いてない
  (backup-directory-alist . `(("." . ,my:d:backup)))
  (backup-by-copying . t)
  (version-control . t)
  (kept-new-versions . 50)
  (kept-old-versions . 0)
  (delete-old-versions . t)
  ;; auto-save file
  ;; issue_com: backupディレクトリに作成されずauto-save-list内のまま
  ;; (auto-save-list-file-prefix . ,`(locate-user-emacs-file "backup/.saves-"))
  (auto-save-file-name-transforms . `((".*" ,my:d:backup t)))
  (auto-save-timeout . 15)
  (auto-save-interval . 120)
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
    :ensure t
    :hook (after-init-hook . (lambda () (load-theme 'doom-vibrant t)))
    :custom
    (doom-themes-enable-italic . nil)
    (doom-themes-enable-bold . nil)
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
  :setq (view-mode-force-exit)
  :config
  (do-not-exit-view-mode-unless-writable-advice view-mode-exit)
  (do-not-exit-view-mode-unless-writable-advice view-mode-disable)

  :bind (:view-mode-map
         ("h" . backward-char)
         ("l" . forward-char)
         ("j" . next-line)
         ("k" . previous-line)
         ("a" . move-beginning-of-line)
         ("e" . move-end-of-line)
         ("n" . scroll-up)
         ("p" . scroll-down)
         ;; ("s" . isearch-forward)
         ;; ("r" . isearch-backward)
         ("c" . consult-line)
         ("m" . consult-line-multi)
         )
  :hook (find-file-hook . view-mode)
  :custom
  (view-read-only . t)
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
  )

(leaf corfu
  :doc "COmpletion in Region FUnction"
  :ensure t
  :bind ("S-SPC" . #'corfu-insert-separator)  ; M-SPCだとWSL上のemacsで效かないので変更
  :custom
  (corfu-auto . t)                      ; corfu on
  (corfu-cycle . t)
  (corfu-auto-delay . 0.1)
  (corfu-auto-prefix . 2)
  (corfu-popupinfo-delay . 1.0)
  (corfu-min-width . 30)
  ;; (tab-always-indent 'complete)
  ;; (corfu-preview-current . nil)
  ;; (completion-cycle-threshold . nil)
  ;; (corfu-quit-at-boundary . 'separator)
  ;; (corfu-separator . ?\s)
  ;; (corfu-on-exact-match . nil)
  ;; (corfu-quit-no-match . t)
  ;; (corfu-preselect-first . nil)
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
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  )

(leaf rainbow-delimiters
  :doc "Highlight brackets according to their depth"
  :ensure t
  :hook
  web-mode-hook
  prog-mode-hook
  :config
  (rainbow-delimiters-mode)
  )

;; -----------------------------------------------------------------------------------------

(leaf programs
  :config
  (leaf powershell
    :doc "Mode for editing PowerShell scripts"
    :ensure t
    )

  (leaf yaml-mode
    :doc "Major mode for editing YAML files"
    :ensure t
    )

  (leaf typescript-mode
    :doc "Major mode for editing typescript"
    :ensure t
    )

  (leaf vue-mode
    :doc "Major mode for vue component based on mmm-mode"
    :ensure t
    )

  (leaf csv-mode
    :doc "Major mode for editing comma/char separated values"
    :ensure t
    )
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

;; -----------------------------------------------------------------------------------------

(leaf global-key
  :doc "global key bind"
  :bind
  ("C-:" . undo-redo)
  ("C-t" . other-window)
  ("C-0" . delete-window)
  ("C-h" . delete-backward-char)
  ("M-," . text-scale-decrease)
  ("M-." . text-scale-increase)
  )

;; =========================================================================================

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
