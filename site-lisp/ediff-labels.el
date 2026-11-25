;;; ediff-labels.el --- ediff の各画面に表示内容のラベルを出す  -*- coding: utf-8 ; lexical-binding: t -*-
;;
;;   Author : esm-yoshioka
;;
;;; Commentary:
;;
;; ediff の A/B/C の各バッファの header-line に、何を表示しているかを出す(自作)。
;; magit のステージ用 ediff(B が index)のときは、操作キーも合わせて出す。
;; init.el の leaf から ediff-startup-hook / ediff-cleanup-hook に登録する。

;;; Code:

;; ediff 起動後にしか呼ばれないため、変数は宣言だけしておく
(defvar ediff-buffer-A)
(defvar ediff-buffer-B)
(defvar ediff-buffer-C)

(defun my:ediff-label (buf)
  "BUF が何を表示しているかの説明を返す(magit 以外の ediff 用)。"
  (let ((name (buffer-name buf)))
    (cond
     ((string-match "\\.~\\(.+\\)~\\'" name)
      (format "リビジョン %s" (match-string 1 name)))
     ((buffer-file-name buf)
      (format "ファイル %s" (abbreviate-file-name (buffer-file-name buf))))
     (t name))))

(defun my:ediff-show-labels ()
  "ediff の各画面の上端に、表示内容(と magit のステージ用ならキー)を出す。"
  (let* ((stage-p (and (buffer-live-p ediff-buffer-B)
                       (buffer-live-p ediff-buffer-C)
                       (string-suffix-p ".~{index}~" (buffer-name ediff-buffer-B))))
         (labels
          (if stage-p
              '((A . "最新のコミット(HEAD)")
                (B . "ステージ済み(index)   [c b] 右の差分をステージ  [a b] ステージを取り消す")
                (C . "作業中のファイル   [b c] 変更を破棄(中央の内容に戻す)"))
            (mapcar (lambda (k)
                      (let ((buf (symbol-value (intern (format "ediff-buffer-%s" k)))))
                        (cons k (and (buffer-live-p buf) (my:ediff-label buf)))))
                    '(A B C)))))
    (dolist (l labels)
      (let ((buf (symbol-value (intern (format "ediff-buffer-%s" (car l))))))
        (when (and (cdr l) (buffer-live-p buf))
          (with-current-buffer buf
            (setq-local header-line-format
                        (propertize (format " %s: %s" (car l) (cdr l))
                                    'face (intern (format "ediff-current-diff-%s" (car l)))))))))))

(defun my:ediff-remove-labels ()
  "my:ediff-show-labels で付けたラベルを消し、view-mode なら読み取り専用に戻す。"
  (dolist (buf (list ediff-buffer-A ediff-buffer-B ediff-buffer-C))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (kill-local-variable 'header-line-format)
        (when (bound-and-true-p view-mode)
          (setq buffer-read-only t))))))

(provide 'ediff-labels)
;;; ediff-labels.el ends here
