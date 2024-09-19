# Emacs Cheat Sheet

## Basic

### emacs
- C-x C-c : emacsの終了
- C-x C-z : emacsを最小化
- C-g : 実行中の処理をキャンセル
- "F1" : ヘルプ

### Move
- C-f : 1文字後（次の文字）へ移動
- C-b : 1文字前（前の文字）へ移動
- C-n : 次の行（１つ下）へ移動
- C-p : 前の行（１つ上）へ移動
- C-a : 行頭へ移動
- C-e : 行末へ移動
- M-f : 1単語後へ移動
- M-b : 1単語前へ移動
- C-v : 次の画面（１画面下）へ移動
- M-v : 前の画面（１画面上）へ移動
- M-< (Shift + ,) : バッファの最初の行へ移動
- M-> (Shift + .) : バッファの最後の行へ移動

### Windows
- C-x 2 : ウィンドウを上下2つに分割
- C-x 3 : ウィンドウを左右2つに分割
- C-x 1 : 他のウィンドウを消す
- C-0 / C-x 0 : 現在のウィンドウを閉じる
- C-t / C-x o : 次のウィンドウへ移動
- M-"left" : ウィンドウサイズ（横）を縮める
- M-"right" : ウィンドウサイズ（横）を広げる
- M-"down" : ウィンドウサイズ（縦）を広げる
- M-"up" : ウィンドウサイズを均等に戻す

### Buffer
- C-x k : バッファを消す
- C-x b : バッファを切り替える
- C-x C-b : ミニバッファリストを開く
  - d : バッファを削除
  - q : バッファリストを閉じる
- C-. : 次のバッファを開く
- C-, : 前のバッファを開く

### File
- C-x C-f : ファイルを開く
- C-x C-s : ファイルを保存する（上書き保存）
- C-x C-w : ファイルを指定ファイルに保存する（別名保存）
- C-x C-o : 最近使ったファイルの一覧を開く

### Cut & Paste
- C-"SPC" : カーソル位置からmarkする
- C-x "SPC" : カーソル位置から矩形でmarkする
- C-x h : バッファ全体をmarkする
- C-w : 切り取り（regionをkill-ringに追加する）
- M-w : コピー（regionをコピーしてkill-ringに追加する）
- C-y : 貼り付け（regionをyankする）
- M-y : kill-ringから選択してyankする
- C-k : カーソル位置から行末まで切り取ってkill-ringに追加する
- C-d : カーソル位置の１文字を削除
- C-h : カーソル位置の１文字前を削除（BackSpace）
- C-/ : UNDO
- C-: : REDO

### Search / Replace
- C-s : カーソルから下を検索（前方検索）
- C-r : カーソルから上を検索（後方検索）
- C-c s : カレントバッファから全検索
- C-c m : 開いている全バッファから検索
- M-% (Shift + 5) : カーソルから下を置換
  - ! (Shift + 1) : 以降を一括置換
  - "SPC" : 置換して次の候補
  - "DEL" : 置換せずに次の候補
- C-c j : mark（C-"SPC"）した箇所にジャンプ
- C-c r : ripgrep
  - C-u C-c r : 検索するルートディレクトリを指定してripgrep
- C-c f : ファイル名検索（find）
  - C-u C-c f : 検索するルートディレクトリを指定してfind

### Display
- C-l : カーソルのある行を画面中央に表示
- M-, : 表示している文字サイズを小さくする
- M-. : 表示している文字サイズを大きくする

### Command
- C-u [num] [command] : [num]回分、[command]を繰り返す
- C-x C-q : バッファのReadOnlyのOn/Off切り替え
- "F3" : マクロの記録開始
- "F4" : マクロの記録を終了 / マクロの実行開始
- C-x d : diredを開く
- M-x [command] : [command]を実行する
  - eval-buffer : カレントバッファの内容を評価
  - package-list-packages : elispパッケージ一覧を表示
  - set-buffer-file-coding-system : カレントバッファの文字コードを変更
  - revert-buffer-with-coding-system : 文字コードを指定して、カレントバッファを開き直す
  - toggle-truncate-lines : 長い行の折り返し表示を切り替える
  - untabify : Region内のTABを空白に置き換える
  - set-alpha : 透過度を変更
  - describe-key : 入力したキーのコマンド内容を表示
<!---
    - rg :: ripgrep
    - sgml-pretty-print :: XMLファイルの整形
-->

## package list
- i : インストール対象としてマーク
- d : 削除対象としてマーク
- u : マークを解除
- U : アップデート対象を全てマーク
- x : インストール／削除の実行
- r : 一覧の更新
- q : package listを閉じる

## magit
- C-x g : magit バッファを開く
  - n : 次の行（１つ下）へ移動
  - p : 前の行（１つ上）へ移動
  - q : キャンセル（画面を閉じる）
  - g : 表示内容の更新
  - s : stageに追加（git add）
  - u : unstageに戻す（git restore）
  - k : 変更を破棄（git checkout .）
  - v : commitを破棄（git revert）
  - i : gitignoreに対象ファイルを追加（untrackedの場合のみ）
  - "tab" : インライン展開
    - インライン展開された変更箇所（hunk）内でs, u, vの操作が可能
    - k : unstaged状態で謳歌すると編集前の状態に戻る
  - c c : comitメッセージの入力画面を開く
    - C-c C-c : commit（git commit）
  - l l : commit logを確認（git log）
    - "ENTER" : 各行でEnter押下するとcommitの詳細を表示
  - P u : push（git push）
  - F u : pull（git pull）

<!---
* dired
- n :: 次のディレクトリへ移動
- p :: 前のディレクトリへ移動
- FILE <ENT> :: 指定したファイルを別のウィンドウで開く
- DIR <ENT> :: １階層下のディレクトリへ移動
- j :: １階層上のディレクトリへ移動
- i :: サブディレクトリを展開
- <TAB> :: サブディレクトリを閉じる
- g :: 一覧の最新化
- s :: 一覧のソート
- m :: 対象をマーク
  - u :: 対象のマークを解除
  - U :: 全てのマークを解除
  - C :: マークされた対象をコピー
  - R :: マークされた対象を移動
  - D :: マークされた対象を削除
- + :: ディレクトリを作成
- q :: diredを閉じる
- r :: エディタモード（wdired）
  - C-c C-c :: 変更結果を反映
  - C-c C-g :: 変更結果を破棄
* view
- l :: 1文字後（次の文字）へ移動
- h :: 1文字前（前の文字）へ移動
- j :: 次の行（１つ下）へ移動
- k :: 前の行（１つ上）へ移動
- n :: 次の画面（１画面下）へ移動
- p :: 前の画面（１画面上）へ移動
- s :: カーソルから下を検索（前方検索）
- r :: カーソルから上を検索（後方検索） 
-->
