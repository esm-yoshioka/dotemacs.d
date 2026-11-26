;;; local-config.example.el --- 端末ごとの設定のひな形  -*- coding: utf-8 ; lexical-binding: t -*-
;;
;;; Commentary:
;;
;; local-config.el のひな形。コピーして値を書き換える。
;;
;;   cp local-config.example.el local-config.el
;;
;; local-config.el は gitignore 対象で、なくても起動する(init.el の Local config で読み込む)。
;; 書かなかった変数は init.el / site-lisp 側の既定値になる。不要な設定は消してよい。

;;; Code:

;; ------------------------------------------------------
;;    SQL(init.el の sql ブロック)
;; ------------------------------------------------------
;; 接続ユーザー(既定値: "postgres")
(setq my/pg-user "postgres")

;; 接続先の DB 名(既定値: なし)。DB ごとに sql-connection-alist(C-c q)の接続定義を作る
;; 接続先は localhost:5432 で固定
;; パスワードは ~/.pgpass(Windows は %APPDATA%/postgresql/pgpass.conf)があれば聞かない
(setq my/pg-databases '("app_development"
                        "app_test"
                        "app_staging"))

;; ------------------------------------------------------
;;    dirvish(init.el の dirvish ブロック)
;; ------------------------------------------------------
;; よく使うフォルダ。dirvish で a を押すと候補に出る
;; (KEY PATH NAME) のリスト。user-emacs-directory などを使うときはバッククォートで書く
;; 既定値は OS ごとに次のとおり。使う OS の方を有効にして書き換える
;; gnu/linux
;; (setq my/dirvish-quick-access-entries
;;       `(("e" ,user-emacs-directory "Emacs")
;;         ("g" "~/git" "Git")
;;         ("w" "~/work" "Work")))
;; windows-nt
;; (setq my/dirvish-quick-access-entries
;;       `(("e" ,user-emacs-directory "Emacs")
;;         ("m" "G:/マイドライブ/memo" "Memo")))

;; ------------------------------------------------------
;;    Docker(site-lisp/compose-list.el、gnu/linux のみ)
;; ------------------------------------------------------
;; compose ファイルを探すディレクトリのパターン(ワイルドカード可)
(setq my/compose-search-patterns '("~/git/*/*/"))

;; 作業環境の親ディレクトリ。この下の <name>/ で一覧を開くと、同じ名前の候補を先頭に並べる
(setq my/compose-work-directory "~/work/")

;; non-nil なら、切り替え時に起動中のプロジェクトを down する前に確認する
(setq my/compose-switch-confirm t)

;;; local-config.example.el ends here
