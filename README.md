ac-mozc
=======

ac-mozcは[auto-complete](http://cx4a.org/software/auto-complete/) + [Mozc](http://code.google.com/p/mozc/)で日本語入力するためのEmacsプラグインです。

auto-completeの情報源としてMozcの変換結果を使えるようになります。

全角/半角モードの切り替え無しに、日本語と半角英数字が混ざった文章を楽に書けることを目標にしています。（モードレス日本語入力）

Demo Video
----------

- [ac-mozc - YouTube](http://www.youtube.com/watch?v=O3XpDx8iCKo)

インストール
------------

ac-mozcにはauto-completeとmozc.elが必要です。Emacsにこれらがインストールされていて、正しく動作することを確認してください。

ac-mozc.elをダウンロードしてload-pathの通った場所に置いてください。

init.elに以下のように書いてください。

```lisp
(require 'ac-mozc)
(define-key ac-mode-map (kbd "C-c C-SPC") 'ac-complete-mozc)
```

auto-completeの情報源 `ac-sources` に `ac-source-mozc` を追加すると日本語を入力できるようになります。  
org-modeにac-mozcを設定する例:

```lisp
(require 'org)
(add-to-list 'ac-modes 'org-mode)

(defun my-ac-mozc-setup ()
  (setq ac-sources
        '(ac-source-mozc ac-source-ascii-words-in-same-mode-buffers))
  (set (make-local-variable 'ac-auto-show-menu) 0.2))

(add-hook 'org-mode-hook 'my-ac-mozc-setup)
```
- ac-source-mozcはMozcの変換結果、ac-source-ascii-words-in-same-mode-buffersはバッファ中の英単語 (ASCII characters) を補完するための情報源です。
- ac-auto-show-menuは補完メニューが表示されるまでの時間です。お好みの値を設定してください。

auto-completeの設定方法は、公式の[Auto Complete Modeユーザーマニュアル](http://cx4a.org/software/auto-complete/manual.ja.html)も参照して下さい。

操作方法
--------

基本的にはauto-completeの操作方法そのままです。ac-mozcがauto-completeの情報源として設定されているバッファであれば、普通にタイプすれば日本語の変換候補が現れます。

半角英数字の直後に日本語を入力したい、例えば `Emacs拡張` と入力したい場合は、`Emacs kakucyou` のように間にスペースを1つ入れてください。変換を実行するとスペースが削除されます。undoすると再びスペースが挿入されます。

1文字だけ変換したい場合 (母音、句読点など) は、ac-complete-mozcを実行 (前述の設定通りだと `C-c C-SPC`) してください。

注意
----

ac-mozcはまだ試作段階で、いろいろなものが予告なく変更されます。ご了承下さい。
