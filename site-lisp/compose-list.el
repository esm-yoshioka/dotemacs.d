;;; compose-list.el --- docker compose の一覧から起動・停止・切り替え  -*- coding: utf-8 ; lexical-binding: t -*-
;;
;;   Author : esm-yoshioka
;;
;;; Commentary:
;;
;; compose ファイルのあるフォルダを一覧表示し、up / down / 切り替えを行う(自作)。
;; 一覧はフォルダ単位で、コマンドは -f を付けずにそのフォルダで実行する。
;; そのため .env の COMPOSE_FILE / COMPOSE_PROJECT_NAME や compose.override.yaml は
;; docker compose 自身が解釈する。
;;
;; 探索するディレクトリは `my/compose-search-patterns' を local-config.el で上書きする。

;;; Code:

(require 'tabulated-list)
(require 'seq)
(require 'subr-x)

;; 探索するディレクトリ(ワイルドカード可)。local-config.el で上書きする
(defvar my/compose-search-patterns '("~/git/*/*/")
  "compose ファイルを探すディレクトリのパターン。")
;; 作業環境の親ディレクトリ。この下の <name>/ で開くと、同じ名前の候補を先頭に並べる
(defvar my/compose-work-directory "~/work/"
  "SQL などを作成する作業環境の親ディレクトリ。")
(defvar my/compose-switch-confirm t
  "non-nil なら、切り替え時に起動中のプロジェクトを down する前に確認する。")

(defconst my/compose-file-regexp
  "\\`\\(?:docker-\\)?compose.*\\.ya?ml\\'")
(defconst my/compose-buffer-name "*compose*")
(defvar my/compose--work-key nil
  "一覧を開いたときの作業ディレクトリ名(先頭に並べる候補の判定用)。")

(defun my/compose--env-value (dir key)
  "DIR の .env から KEY の値を返す。なければ nil。"
  (let ((env (expand-file-name ".env" dir)))
    (when (file-readable-p env)
      (with-temp-buffer
        (insert-file-contents env)
        (when (re-search-forward
               (concat "^[ \t]*\\(?:export[ \t]+\\)?" (regexp-quote key)
                       "[ \t]*=[ \t]*\\(.*?\\)[ \t]*$")
               nil t)
          (string-trim (match-string 1) "[\"']" "[\"']"))))))

(defun my/compose--compose-files (dir)
  "DIR 直下の compose ファイル名のリスト。"
  (directory-files dir nil my/compose-file-regexp))

(defun my/compose--dirs ()
  "探索パターンに一致するディレクトリのうち、compose ファイルがあるか
.env に COMPOSE_FILE があるものを集める。"
  (delete-dups
   (seq-filter (lambda (dir)
                 (or (my/compose--compose-files dir)
                     (my/compose--env-value dir "COMPOSE_FILE")))
               (mapcar #'file-name-as-directory
                       (seq-filter #'file-directory-p
                                   (mapcan (lambda (pat)
                                             (file-expand-wildcards (expand-file-name pat)))
                                           my/compose-search-patterns))))))

(defun my/compose--projects ()
  "`docker compose ls -a' の結果を (NAME STATUS CONFIG-FILES) のリストで返す。"
  (with-temp-buffer
    (if (not (eq 0 (call-process "docker" nil '(t nil) nil
                                 "compose" "ls" "-a" "--format" "json")))
        (progn (message "Warning: docker compose ls failed (docker が起動していない?)")
               nil)
      (mapcar (lambda (p)
                (list (alist-get 'Name p)
                      (alist-get 'Status p)
                      (mapcar #'file-truename
                              (split-string (or (alist-get 'ConfigFiles p) "") "," t))))
              (json-parse-string (buffer-string)
                                 :object-type 'alist :array-type 'list)))))

(defun my/compose--project-of (dir projects)
  "DIR で起動したプロジェクトを PROJECTS から探す。
compose は先頭の設定ファイルのディレクトリを作業ディレクトリにするので、それで照合する。"
  (let ((true (file-name-as-directory (file-truename dir))))
    (seq-find (lambda (p)
                (let ((first (car (nth 2 p))))
                  (and first (string= true (file-name-directory first)))))
              projects)))

(defun my/compose--running-p (project)
  (string-prefix-p "running" (nth 1 project)))

(defun my/compose--work-key ()
  "今いるディレクトリが作業環境の下なら、その直下のディレクトリ名を返す。"
  (let ((work (file-name-as-directory (expand-file-name my/compose-work-directory)))
        (dir (expand-file-name default-directory)))
    (and (string-prefix-p work dir)
         (car (split-string (substring dir (length work)) "/" t)))))

(defun my/compose--dir-name (dir)
  (file-name-nondirectory (directory-file-name dir)))

(defun my/compose--match-p (dir key)
  "DIR の名前が KEY と同じか `-KEY' で終わるか。"
  (and key
       (let ((name (my/compose--dir-name dir)))
         (or (string= name key) (string-suffix-p (concat "-" key) name)))))

(defun my/compose--files-label (dir)
  "一覧の「ファイル」列に出す文字列。.env の COMPOSE_FILE を優先する。"
  (let ((env (my/compose--env-value dir "COMPOSE_FILE")))
    (if env
        (format "%s (.env)" env)
      (string-join (my/compose--compose-files dir) ", "))))

(defun my/compose--refresh ()
  "一覧を作り直す。作業ディレクトリに対応する候補、起動中の候補の順に並べる。"
  (let* ((projects (my/compose--projects))
         (key my/compose--work-key)
         (rank (lambda (d)
                 (+ (if (my/compose--match-p d key) 0 2)
                    (let ((p (my/compose--project-of d projects)))
                      (if (and p (my/compose--running-p p)) 0 1)))))
         (dirs (sort (my/compose--dirs)
                     (lambda (a b)
                       (let ((ra (funcall rank a)) (rb (funcall rank b)))
                         (if (= ra rb) (string< a b) (< ra rb)))))))
    (setq tabulated-list-entries
          (mapcar (lambda (d)
                    (let* ((p (my/compose--project-of d projects))
                           (status (if p (nth 1 p) "-")))
                      (list d (vector (propertize status 'face
                                                  (if (and p (my/compose--running-p p))
                                                      'success 'shadow))
                                      (or (car p) (my/compose--dir-name d))
                                      (my/compose--files-label d)
                                      (abbreviate-file-name d)))))
                  dirs))))

(defun my/compose--display (buf)
  "BUF を選択中のウィンドウ(一覧)の下に表示する。表示中ならそのウィンドウを使う。"
  (display-buffer buf '((display-buffer-reuse-window display-buffer-below-selected)
                        (window-height . 0.4))))

(defun my/compose--append (buf string)
  "BUF の末尾に STRING を追記し、BUF を表示しているウィンドウを末尾までスクロールする。"
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert string)))
    (dolist (win (get-buffer-window-list buf nil t))
      (set-window-point win (with-current-buffer buf (point-max))))))

(defun my/compose--run (command dir)
  "COMMAND(シェル文字列)を DIR で非同期に実行し、結果を *compose* に表示する。"
  (let ((buf (get-buffer-create my/compose-buffer-name))
        (list-buf (current-buffer)))
    (with-current-buffer buf
      (setq default-directory dir)
      (unless (derived-mode-p 'special-mode) (special-mode)))
    (my/compose--display buf)
    (my/compose--append buf (format "\n$ cd %s && %s\n" (abbreviate-file-name dir) command))
    (let* ((default-directory dir)
           (process-connection-type nil)
           (proc (start-file-process-shell-command "compose" buf command)))
      ;; 既定のフィルタはウィンドウをスクロールしないので、追記ごとに末尾へ動かす
      (set-process-filter proc (lambda (p out) (my/compose--append (process-buffer p) out)))
      (set-process-sentinel
       proc
       (lambda (p event)
         (when (memq (process-status p) '(exit signal))
           (my/compose--append (process-buffer p) (format "[%s]\n" (string-trim event)))
           (when (buffer-live-p list-buf)
             (with-current-buffer list-buf (revert-buffer)))))))))

(defun my/compose--dir-at-point ()
  (or (tabulated-list-get-id) (user-error "compose のフォルダの行ではありません")))

(defun my/compose--cmd (&rest args)
  "docker compose ARGS のシェル文字列。-f は付けず、.env などの解釈は compose に任せる。"
  (mapconcat #'shell-quote-argument (append '("docker" "compose") args) " "))

(defun my/compose-up ()
  "この行の compose を起動する(他のプロジェクトは止めない)。"
  (interactive)
  (my/compose--run (my/compose--cmd "up" "-d") (my/compose--dir-at-point)))

(defun my/compose-down ()
  "この行の compose を down する。"
  (interactive)
  (my/compose--run (my/compose--cmd "down") (my/compose--dir-at-point)))

(defun my/compose-switch ()
  "起動中の他のプロジェクトを down してから、この行の compose を起動する。
コンテナ名やポートの衝突を避けるため。"
  (interactive)
  (let* ((d (my/compose--dir-at-point))
         (projects (my/compose--projects))
         (target (my/compose--project-of d projects))
         (others (seq-filter (lambda (p) (and (my/compose--running-p p)
                                              (not (eq p target))))
                             projects))
         (names (mapcar #'car others)))
    (when (or (null others)
              (not my/compose-switch-confirm)
              (y-or-n-p (format "%s を down して %s を起動しますか? "
                                (string-join names ", ")
                                (my/compose--dir-name d))))
      (my/compose--run
       (mapconcat #'identity
                  (append (mapcar (lambda (n) (my/compose--cmd "-p" n "down")) names)
                          (list (my/compose--cmd "up" "-d")))
                  " && ")
       d))))

(defun my/compose-show-log ()
  "*compose* バッファを一覧の下に表示する。"
  (interactive)
  (my/compose--display (get-buffer-create my/compose-buffer-name)))

(defun my/compose-quit ()
  "*compose* のウィンドウを閉じてから一覧を閉じる。バッファ(実行履歴)は残す。"
  (interactive)
  (when-let* ((buf (get-buffer my/compose-buffer-name)))
    (dolist (win (get-buffer-window-list buf nil))
      (quit-window nil win)))
  (quit-window))

(defvar-keymap my/compose-list-mode-map
  "u" #'my/compose-up
  "d" #'my/compose-down
  "s" #'my/compose-switch
  "l" #'my/compose-show-log
  "q" #'my/compose-quit)

(define-derived-mode my/compose-list-mode tabulated-list-mode "Compose"
  "docker compose のフォルダの一覧。"
  (setq tabulated-list-format
        [("状態" 12 t) ("プロジェクト" 20 t) ("ファイル" 40 t) ("フォルダ" 0 t)])
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook #'my/compose--refresh nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun my/compose-list ()
  "compose のフォルダの一覧を開く。"
  (interactive)
  (let ((key (my/compose--work-key)))
    (with-current-buffer (get-buffer-create "*compose-list*")
      (my/compose-list-mode)
      (setq-local my/compose--work-key key)
      (my/compose--refresh)
      (tabulated-list-print)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(provide 'compose-list)
;;; compose-list.el ends here
