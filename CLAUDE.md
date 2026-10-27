# CLAUDE.md

## 概要

GNU Emacs 31.1 向けの個人設定(upstream: `esm-yoshioka/dotemacs.d`)。コメント・文書は日本語で書く。
`windows-nt` と `gnu/linux`(WSL)の両方で動く必要があり、OS 依存の設定は `(eq system-type ...)` で分岐する。
対象バージョンは `init.el` / `early-init.el` 冒頭の `Version:` に記載しており、更新時は両方を揃える。

## 編集ルール

- パッケージはすべて **`leaf`** で宣言する(`use-package` は使わない)。新規追加は既存の
  `leaf NAME :doc "..." :ensure t ...` の形に合わせ、`init.el` の `;; ----` 見出しで区切られた該当セクションに置く。
- `:ensure t` は ELPA/MELPA からインストール、`:ensure nil` は組み込み、またはインストール済みパッケージの拡張(例: `vertico-directory`)。
- パッケージに紐づかない組み込みの設定(`setq` だけで済むもの)は `early-init.el` に書く。
- 状態ファイルは `my:d:vars`(`vars/`)の下に置く。保存先は `init.el` 冒頭の `no-littering` が `vars/`(設定ファイルは `vars/etc/`)に振り分けるので、通常は個別の指定は不要。
  no-littering が対応していないパッケージだけ、`:custom` で `` `((VAR . ,(expand-file-name "FILE" my:d:vars))) `` のように指定する。
  no-littering は `auto-save-list-file-prefix` を書き換えるため、`:config` で `nil` に戻している(`early-init.el` の設定を維持するため)。
- **外部依存がなくても起動を壊さない。** `executable-find` / `file-exists-p` / `bound-and-true-p` で確認し、なければエラーにせず `(message "Warning: ...")` を出す。
  既存の例: migemo、tr-ime(`condition-case`)、mozc、vterm(`my:vterm-available-p` + `:if`)、magit(`:if`)、HackGen フォント。
- migemo は任意の依存で、anzu / orderless / consult(ripgrep)から使っている。連携部分は必ず
  `(bound-and-true-p migemo-process)` を確認してから使う。新しく migemo を使う箇所でもこの確認を外さない。
- キーバインドを変更したら `cheatsheet.org` も更新する。

## 意図的な設定(戻さないこと)

- `C-h` は `delete-backward-char`(isearch では `isearch-delete-char`)。ヘルプは `<f1>` を使う。
- `M-.` / `M-,` は `:bind*` で `text-scale-increase/decrease` に割り当てている。xref には戻さない。
- view-mode: `find-file-hook` でファイルを読み取り専用で開く。例外は新規ファイルと、`my:exclude-view-list` に**完全一致するファイル名**(`COMMIT_EDITMSG`、`git-rebase-todo` など。magit の `git-commit-filename-regexp` に対応)。
  部分一致に戻してはいけない。`case-fold-search` が t のため、`"TAG"` が `voltage.log` に、`"COMMIT"` が `committee.txt` に一致してしまう。

## ファイル構成

- `early-init.el` — パッケージ初期化と GUI の前に読まれる。パッケージアーカイブ、文字コード、編集の基本設定、フレームの既定値を設定する。
  パス定数 `my:d:vars` / `my:d:backup` もここで定義しており、バックアップと自動保存は `backup/` に出力する。
- `init.el` — 本体。`leaf` の導入 → `local-config.el` の読み込み → 各セクションの順に並ぶ。
- `vars/etc/custom.el` — Emacs Custom が自動生成する(`early-init.el` の `custom-file`)。**手で編集しない**(`vars/` ごと gitignore 対象)。
- `local-config.el` — 未追跡・gitignore 対象で、なくても動く。端末ごとの値(`my/pg-user`、`my/pg-databases` など)を置き、`sql` ブロックがこれを使って `sql-connection-alist` を組み立てる。コミットしない。

## 動作確認

テストはない。次の方法で確認する。

```
emacs --debug-init                              # 実際の確認はこちら(GUI・フォント依存が多いため)
emacs -Q --batch -l early-init.el -l init.el    # 簡易的な読み込み確認
```

### byte-compile での確認

`emacs --batch -f batch-byte-compile init.el` では確認できない。理由は 2 つ。
`--batch` だとパッケージが有効にならず `leaf` が未定義になる。また、`leaf` の `:custom` はマクロ展開時に評価されるため、
`early-init.el` で定義する `my:d:vars` が先に定義されている必要がある。
さらに、`.elc` はこのディレクトリの外に出力すること。`init.elc` が残っていると、次回以降の起動で `init.el` より優先して読まれてしまう。

```
emacs -Q --batch -l early-init.el \
  --eval "(progn (require 'package) (package-initialize) \
                 (require 'leaf-keywords) (leaf-keywords-init) \
                 (setq byte-compile-dest-file-function \
                       (lambda (f) (expand-file-name (concat (file-name-nondirectory f) \"c\") \
                                                     temporary-file-directory))))" \
  -f batch-byte-compile init.el
```

現状(31.1)で警告は約 45 件出るが、すべて「コンパイル時にパッケージが読み込まれていない」ことによる想定内の警告(`assignment to free variable`、`not known to be defined` など)で、対応は不要。
**新しい種類の警告やエラー**が出た場合だけ問題として扱う。
