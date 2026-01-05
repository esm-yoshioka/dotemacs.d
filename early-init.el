;;; early-init.el --- My early-init.el  -*- coding: utf-8 ; lexical-binding: t -*-
;; 
;;   Author : esm-yoshioka
;;   Version: 30.2
;; 

;; ------------------------------------------------------
;;    User-Directory Setting
;; ------------------------------------------------------
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file)))))
  )


;; ------------------------------------------------------
;;    Optimizing Startup Performance(GC Tuning)
;; ------------------------------------------------------
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.2)))


;; ------------------------------------------------------
;;    Package Management
;; ------------------------------------------------------
(setq package-enable-at-startup nil)	; Disable automatic initialization
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))


;; ------------------------------------------------------
;;    Operation Settings
;; ------------------------------------------------------
;; languges
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8)

;; general
(setq confirm-kill-emacs 'y-or-n-p)
(setq use-short-answers t)
(setq use-dialog-box nil)
(setq echo-keystrokes 0.1)

(setq ring-bell-function 'ignore)

(setq kill-ring-max 200)
(setq mark-ring-max 50)

(setq require-final-newline t)

;; tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; scroll
(setq scroll-conservatively 1)
(setq scroll-margin 5)
(setq next-screen-context-lines 5)
(setq scroll-preserve-screen-position t)


;; ------------------------------------------------------
;;    Directory Initial Settings
;; ------------------------------------------------------
;; local-const
(defconst my:d:vars
  (expand-file-name "vars/" user-emacs-directory))
(defconst my:d:backup
  (expand-file-name "backup/" user-emacs-directory))

(dolist (dir (list my:d:vars my:d:backup))
  (unless (file-directory-p dir)
    (make-directory dir t)))


;; ------------------------------------------------------
;;    File Initial Settings
;; ------------------------------------------------------
(setq custom-file (locate-user-emacs-file "custom.el"))

;; backup file
(setq backup-directory-alist `((".*" . ,my:d:backup)))
(setq version-control t)
(setq backup-by-copying t)
(setq kept-new-versions 50)
(setq kept-old-versions 0)
(setq delete-old-versions t)

;; auto-save file
(setq auto-save-list-file-prefix nil)
(setq auto-save-file-name-transforms `((".*" ,my:d:backup t)))
(setq auto-save-timeout 15)
(setq auto-save-interval 120)

;; lock file
(setq create-lockfiles nil)


;; ------------------------------------------------------
;;    UI Initial Settings
;; ------------------------------------------------------
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))

(add-to-list 'default-frame-alist '(alpha-background . 90))        ;issue Not working on Windows
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(cursor-type . bar))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode 0)

(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq line-spacing 0.25)


;;; early-init.el ends here
