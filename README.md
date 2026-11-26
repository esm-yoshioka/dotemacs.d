# dotemacs.d

git clone https://github.com/esm-yoshioka/dotemacs.d ~/.emacs.d

## initialize
- 端末ごとの設定(SQL の接続先、docker compose の探索先など)

```
cp local-config.example.el local-config.el
```

- フォントのインストール

```
cd HackGen
./fonts_cp.sh
```

- アイコンのインストール

```
M-x nerd-icons-install-fonts
```
