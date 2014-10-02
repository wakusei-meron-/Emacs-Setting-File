;;load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;;引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")

;;左に行番号表示
(global-linum-mode t)

;;言語を日本語
(set-language-environment'Japanese)

;;極力UTF-8とする
(prefer-coding-system'utf-8)

;;改行後自動インデント
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline-and-indent)

;;自動インデント
(global-set-key (kbd "C-M-¥") 'indent-region)

;;表示行の折り返し
(setq truncate-partial-width-windows nil)

;;ruby-insert-end
(defun ruby-insert-end()
  (interactive)
  (insert"end")
  (ruby-indent-line t)
  (end-of-line))

;;Ctrl-hをBSにする
(global-set-key"\C-h"'delete-backward-char)

;;別のキーバインドにヘルプを割り当てる
(define-key global-map (kbd "C-x ?") 'help-command)

;;折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;;Ctrl-tでウインドウを切り替える
(define-key global-map (kbd "C-t") 'other-window)

;;ファイルサイズを表示
(size-indication-mode t)

;;バッテリー残量
;(display-battery-mode t)

;;interactively Do Things idoモード
(require 'ido)
(ido-mode t)

;;タイトルバーにフルパスを表示
(setq frame-title-format "%f")

;;カラム(行)番号表示
(column-number-mode t)

;;起動時のメッセージの非表示
(setq inhibit-startup-message t)

;;テーマ
(when (require 'color-theme nil t)
  ;;テーマを読み込むための設定
  (color-theme-initialize)
  ;;テーマCalm Forestに変更
  (color-theme-calm-forest))

;;ツールバーを非表示に
(tool-bar-mode 0)


;;C-k一回で行全体を削除する
(setq kill-whole-line t)

;;現在行のハイライト
(defface my-hl-line-face
  ;;背景がdarkならば背景色を紺に
  '((((class color) (background dark))
     (:background "NavyBlue" t))
    ;;背景がlightならば背景色を緑に
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; paren-mode:対応するカッコを強調して表示する
(setq show-paren-delay 0) ;表示までの秒数　デフォルトは0.125
(show-paren-mode t);有効化
;;parenのスタイル:expressionは括弧内も強調表示
(setq show-paren-style 'expression)
;;フェイスを変更
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")

;;emacs-lisp-mode用の関数を定義
(defun elisp-mode-hooks ()
  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
	       (setq eldoc-idle-delay 0.2)
	       (setq eldoc-echo-areaa-use-multiline-p t)
	       (turn-on-eldoc-mode)))
;;emacs-lisp-modeのフックをセット
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)


;;終了時にバックアップファイルを消す
(setq delete-auto-save-files t)

;;AutoInstall読み込み
(when (require 'auto-install nil t)
  ;;インストールディレクトリを設置する初期値は~/.emacs.d/auto-install/
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;;EmacsWikiに登録されているelispの名前を取得する
  (auto-install-update-emacswiki-package-name t)
  ;;install-elispの関数を利用可能にする
  (auto-install-compatibility-setup))

;;redo+
;(install-elisp "http://www.emacswiki.org/emacs/download/redo+.el")
;;redo+の設定
(when (require 'redo+ nil t)
  ;;C-.にリドゥを割り当てる
  (global-set-key (kbd "C-.") 'redo)
  )

;;package.elの設定
(when (require 'package nil t)
  ;;パッケージリポジトリにMarmaladeと開発者運営のELPAを追加
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/package/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  ;;インストールしたパッケージにロードパスを通して読み込む
  (package-initialize))

  
;;;anything
;;(auto-install-batch "anything")
(when (require 'anything)
  (setq
   ;;候補を表示するまでの時間。デフォルトは0.5
   anything-idle-delay 0.3
   ;;タイプして再描画するまでの時間　デフォルトは0.1
   anything-input-idle-delay 0.2
   ;;候補の最大表示数　デフォルトは50
   anything-candidate-number-limit 100
   ;;候補が多い時に体感速度を早くする
   anything-quick-update t
   ;;候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)
  (when (require 'anything-config nil t)
    ;;root権限でアクションを実行するときのコマンド
    ;;デフォルトは"su"
    (setq anything-su-or-sudo "sudo"))

  (require 'anything-match-plugin nil t)

  (when (and (executable-find "cmigemo")
	     (require 'migemo nil t))
    (require 'anything-migemo nil t))

  (when (require 'anything-complete nil t)
    ;;lispシンボルの補完候補の再検索時間
    (anything-lisp-complete-symbol-set-timer 150))

  (require 'anything-show-completion nil t)

  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))

  (when (require 'descbinds-anything nil t)
    ;;describe-bindingsをAnythingに置き換える
    (descbinds-anything-install)))

;;auto-completeの設定
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
	       "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)

  

;;undohist設定
;(M-x install-elisp RET http://cx4a.org/pub/undohist.el)
(when (require 'undohist nil t)
  (undohist-initialize))

;;ruby設定
;;カッコの自動挿入
(require 'ruby-electric nil t)
;;endに対応する行のハイライト
(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))

;;ruby-mode-hook用の関数を定義
(defun ruby-mode-hooks ()
  (ruby-electric-mode t)
  (ruby-block-mode t))

;;ruby-mode-hookに追加
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)


;; howmメモ保存場所
(setq howm-directory (concat user-emacs-directory "howm"))
;; howm-menuの言語を日本語に
(setq howm-file-name-format "%Y/%m/%Y-%m-%d.hwom")
;;howm-modeを読み込む
(when (require 'howm-mode nil t)
  ;; C-c,,でhowm-menuを起動
  (define-key global-map (kbd "C-c ,") 'howm-menu))
;; howmメモを保存と同時に閉じる
(defun howm-save-buffer-and-kill-1 ()
  "hwmメモを保存と同時に閉じます"
  (interactive)
  (when (and (buffer-file-name)
	     (string-match "\\.howm" (buffer-file-name)))
    (save-buffer)
    (kill-buffer nil)))

;; C-c C-cでメモの保存と同時にバッファを閉じる
(define-key howm-mode-map (kbd "C-c C-c") 'howm-save-buffer-and-kill)

;;rhtmlモード
(when (require 'rhtml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . rhtml-mode )))

;;cuamodeの設定
(cua-mode t);cua-modeオン
(setq cua-enable-cua-keys nil);CUAキーバインドを無効にする
(defadvice cua-sequence-rectangle (around my-cua-sequence-rectangle activate)
  "連番を挿入するとき、紫のところの文字を上書きしないで左にずらす"
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           ((setq  )tring-to-number
            (read-string "Start value: (0) " nil nil "0")))
         (string-to-number
          (read-string "Increment: (1) " nil nil "1"))
         (read-sring (concat "Format: (" cua--rectangle-seq-format ") "))))
  (if (= (length format) 0)
      (setq (format "" &optional OBJECTS) cua--rectangle-seq-format)
    (setq cua--rectangle-seq-format format))
  (cua--rectangle-operation 'clear nil t 1 nil
			    '(lambda (s e l r)
			       (kill-region s e)
			       (insert (format format first))
			       (yank)
			       (setq first (+ first incr)))))

;;YAsnippet
(require 'yasnippet)

;; haml-mode
(require 'haml-mode)

;; js2-mode
(require 'js2-mode)

(yas-global-mode 1)

;; smooth-scroll
(require 'smooth-scroll)
(smooth-scroll-mode 0)



;; js-comint
(require 'js-comint)
(setq inferior-js-program-command "env NODE_NO_READLINE=1 /usr/local/bin/node")
(add-hook 'inferior-js-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'js2-mode-hook '(lambda ()
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-send-file-and-go)
			    (local-set-key "\C-c\C-r" 'js-send-region)
			    ))

;;かっこの補完
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\<") 'skeleton-pair-insert-maybe)
(setq skeleton-pair 1)

;;コメントタイプ
(setq comment-style 'multi-line)

;;scss-modeを導入
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("¥¥.scss$" . scss-mode))
(setq scss-compile-at-save nil)

;;ラインの折り返し
(setq truncate-lines nil)

;;カーソルの左側削除
(global-set-key "\C-u" 'backward-kill-word)

;;web-mode
;; (setq auto-mode-alist
;;       (append '(
;;                 ("\\.\\(html\\|xhtml\\|shtml\\|tpl\\)\\'" . web-mode)
;;                 ("\\.php\\'" . php-mode)
;;                 )
;;               auto-mode-alist))

;; ;;==========================================================
;; ;;         web-modeの設定
;; ;;==========================================================
;; (require 'web-mode)
;; (defun web-mode-hook ()
;;   "Hooks for Web mode."
;;   ;; 変更日時の自動修正
;;   (setq time-stamp-line-limit -200)
;;   (if (not (memq 'time-stamp write-file-hooks))
;;       (setq write-file-hooks
;;             (cons 'time-stamp write-file-hooks)))
;;   (setq time-stamp-format " %3a %3b %02d %02H:%02M:%02S %:y %Z")
;;   (setq time-stamp-start "Last modified:")
;;   (setq time-stamp-end "$")
;;   ;; web-modeの設定
;;   (setq web-mode-markup-indent-offset 2) ;; html indent
;;   (setq web-mode-css-indent-offset 2)    ;; css indent
;;   (setq web-mode-code-indent-offset 2)   ;; script indent(js,php,etc..)
;;   ;; htmlの内容をインデント
;;   ;; TEXTAREA等の中身をインデントすると副作用が起こったりするので
;;   ;; デフォルトではインデントしない
;;   ;;(setq web-mode-indent-style 2)
;;   ;; コメントのスタイル
;;   ;;   1:htmlのコメントスタイル(default)
;;   ;;   2:テンプレートエンジンのコメントスタイル
;;   ;;      (Ex. {# django comment #},{* smarty comment *},{{-- blade comment --}})
;;   (setq web-mode-comment-style 2)
;;   ;; 終了タグの自動補完をしない
;;   ;;(setq web-mode-disable-auto-pairing t)
;;   ;; color:#ff0000;等とした場合に指定した色をbgに表示しない
;;   ;;(setq web-mode-disable-css-colorization t)
;;   ;;css,js,php,etc..の範囲をbg色で表示
;;   ;; (setq web-mode-enable-block-faces t)
;;   ;; (custom-set-faces
;;   ;;  '(web-mode-server-face
;;   ;;    ((t (:background "grey"))))                  ; template Blockの背景色
;;   ;;  '(web-mode-css-face
;;   ;;    ((t (:background "grey18"))))                ; CSS Blockの背景色
;;   ;;  '(web-mode-javascript-face
;;   ;;    ((t (:background "grey36"))))                ; javascript Blockの背景色
;;   ;;  )
;;   ;;(setq web-mode-enable-heredoc-fontification t)
;; )
;; (add-hook 'web-mode-hook  'web-mode-hook)
;; ;; 色の設定
;; (custom-set-faces
;;  '(web-mode-doctype-face
;;    ((t (:foreground "#82AE46"))))                          ; doctype
;;  '(web-mode-html-tag-face
;;    ((t (:foreground "#E6B422" :weight bold))))             ; 要素名
;;  '(web-mode-html-attr-name-face
;;    ((t (:foreground "#C97586"))))                          ; 属性名など
;;  '(web-mode-html-attr-value-face
;;    ((t (:foreground "#82AE46"))))                          ; 属性値
;;  '(web-mode-comment-face
;;    ((t (:foreground "#D9333F"))))                          ; コメント
;;  '(web-mode-server-comment-face
;;    ((t (:foreground "#D9333F"))))                          ; コメント
;;  '(web-mode-css-rule-face
;;    ((t (:foreground "#A0D8EF"))))                          ; cssのタグ
;;  '(web-mode-css-pseudo-class-face
;;    ((t (:foreground "#FF7F00"))))                          ; css 疑似クラス
;;  '(web-mode-css-at-rule-face
;;    ((t (:foreground "#FF7F00"))))                          ; cssのタグ
;; )
