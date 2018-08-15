; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ----------------------------------------------------------------------------
;; system-type predicates
;; (setq darwin-p (eq system-type 'darwin)
;;      ns-p (eq window-system 'ns)
;;      carbon-p (eq window-system 'mac)
;;      linux-p (eq system-type 'gnu/linux)
;;      cygwin-p (eq system-type 'cygwin)
;;      nt-p (eq system-type 'windows-nt)
;;      meadow-p (featurep 'meadow)
;;      windows-p (or cygwin-p nt-p meadow-p))
;;
;; OS固有の設定ファイルを記述したディレクトリをロードパスに追加する
(setq load-path (append '("~/.emacs.d/config") load-path))

;; ----------------------------------------------------------------------------
;; package.el
;; packageのリポジトリを追加する
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("melpa" . "http://melpa.org/packages/")
						 ("org" . "http://orgmode.org/elpa")
						 ("melpa-stable" . "http://stable.melpa.org/packages/")))

;; 初期化
(add-to-list 'load-path "~/.emacs.d/sites")
(require 'use-package)


;; ------------------------------------------------------------------------
;; 環境変数
(use-package exec-path-from-shell
  :config
  (let ((envs '("PATH" "GOPATH"))) (exec-path-from-shell-copy-envs envs)))

;; ------------------------------------------------------------------------
;; @ frame

;; フレームタイトルの設定
(setq frame-title-format "%b")

;; ------------------------------------------------------------------------
;; @ buffer

;; バッファ画面外文字の切り詰め表示
(setq truncate-lines nil)

;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
(setq truncate-partial-width-windows t)

;; 同一バッファ名にディレクトリ付与
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

;; ------------------------------------------------------------------------
;; @ fringe

;; バッファ中の行番号表示
(global-linum-mode t)

;; 行番号のフォーマット
;; (set-face-attribute 'linum nil :foreground "red" :height 0.8)
(set-face-attribute 'linum nil :height 0.8)
(setq linum-format "%4d")

;; ------------------------------------------------------------------------
;; @ modeline

;; 行番号の表示
(line-number-mode t)

;; 列番号の表示
(column-number-mode t)

;; 時刻の表示
(use-package time
  :config
  (setq display-time-24hr-format t)
  (setq display-time-string-forms '(24-hours ":" minutes))
  (display-time-mode t))

;; cp932エンコード時の表示を「P」とする
(coding-system-put 'cp932 :mnemonic ?P)
(coding-system-put 'cp932-dos :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac :mnemonic ?P)

;; ------------------------------------------------------------------------
;; @ cursor

;; カーソル点滅表示
(blink-cursor-mode 0)

;; スクロール時のカーソル位置の維持
(setq scroll-preserve-screen-position t)

;; スクロール行数（一行ごとのスクロール）
(setq vertical-centering-font-regexp ".*")
(setq scroll-conservatively 35)
(setq scroll-margin 0)
(setq scroll-step 1)

;; 画面スクロール時の重複行数
(setq next-screen-context-lines 1)

;; ------------------------------------------------------------------------
;; @ default setting

;; 起動メッセージの非表示
(setq inhibit-startup-message t)

;; スタートアップ時のエコー領域メッセージの非表示
(setq inhibit-startup-echo-area-message -1)

;; ------------------------------------------------------------------------
;; @ backup

;; 変更ファイルのバックアップ
(setq make-backup-files nil)

;; 変更ファイルの番号つきバックアップ
(setq version-control nil)

;; 編集中ファイルのバックアップ
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)

;; 編集中ファイルのバックアップ先
(setq auto-save-file-name-transforms
	  `((".*" ,temporary-file-directory t)))

;; 編集中ファイルのバックアップ間隔（秒）
(setq auto-save-timeout 30)

;; 編集中ファイルのバックアップ間隔（打鍵）
(setq auto-save-interval 500)

;; バックアップ世代数
(setq kept-old-versions 1)
(setq kept-new-versions 2)

;; 上書き時の警告表示
;; (setq trim-versions-without-asking nil)

;; 古いバックアップファイルの削除
(setq delete-old-versions t)

;; ------------------------------------------------------------------------
;; @ key bind

;; 標準キーバインド変更
;; (global-set-key "\C-z"          'scroll-down)

;; ------------------------------------------------------------------------
;; @ scroll

;; バッファの先頭までスクロールアップ
;; (defadvice scroll-up (around scroll-up-around)
;;   (interactive)
;;   (let* ( (start_num (+ 1 (count-lines (point-min) (point))) ) )
;; 	(goto-char (point-max))
;; 	(let* ( (end_num (+ 1 (count-lines (point-min) (point))) ) )
;; 	  (goto-line start_num )
;; 	  (let* ( (limit_num (- (- end_num start_num) (window-height)) ))
;; 		(if (< (- (- end_num start_num) (window-height)) 0)
;; 			(goto-char (point-max))
;; 		  ad-do-it)) )) )
;; (ad-activate 'scroll-up)

;; バッファの最後までスクロールダウン
;; (defadvice scroll-down (around scroll-down-around)
;;   (interactive)
;;   (let* ( (start_num (+ 1 (count-lines (point-min) (point)))) )
;; 	(if (< start_num (window-height))
;; 		(goto-char (point-min))
;; 	  ad-do-it) ))
;; (ad-activate 'scroll-down)

;; Window間の移動を矢印キーで行う。
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; ----------------------------------------------------------------------------
;; デフォルトのtab幅設定
(setq default-tab-width 4)

;; 閉じ括弧自動入力
(electric-pair-mode 1)

;; 各種メジャーモード
;; cc-mode style(C,C++,Java,Objective-C)
(use-package cc-mode
  :config
  (add-hook 'c-mode-common-hook
			'(lambda()
			   (c-set-style "stroustrup")
			   (c-set-offset 'inline-open 0)
			   (c-set-offset 'innamespace 0)
			   ;; (setq c-auto-newline t)
			   (setq tab-width 4))))

;; auto complete
(use-package auto-complete
  :config
  (require 'auto-complete-config)    ; 必須ではないですが一応
  (global-auto-complete-mode t))

;; C++ headers complete
(defun my:ac-c-headers-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers))

(add-hook 'c++-mode-hook 'my:ac-c-headers-init)
(add-hook 'c-mode-hook 'my:ac-c-headers-init)

;; flycheck - syntax check
(use-package flycheck
  :config
  (progn
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;; cmake-ide
;; (use-package cmake-ide
;;   :bind
;;   (("<f9>" . cmake-ide-compile))
;;   :config
;;   (progn
;;     (setq 
;;      ; rdm & rcコマンドへのパス。コマンドはRTagsのインストール・ディレクトリ下。
;;      cmake-ide-rdm-executable "/usr/local/bin/rdm"
;;      cmake-ide-rc-executable  "/usr/local/bin/rc")))
;;

;; cmake-mode
(use-package cmake-mode
  :config
  (setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist)))


;; yasnippet
(eval-after-load "yasnippet"
  '(progn
     ;; companyと競合するのでyasnippetのフィールド移動は "C-i" のみにする
     (define-key yas-keymap (kbd "<tab>") nil)
     (yas-global-mode 1)))

;; company mode : 補完用パッケージ
(use-package company
  :config
  (global-company-mode 1)
  (global-set-key (kbd "C-M-i") 'company-complete)
  ;; (setq company-idle-delay nil) ; 自動補完をしない
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection))

;; rtags
;; (use-package rtags
;;   :config
;;   (progn
;;     (rtags-enable-standard-keybindings c-mode-base-map)
;; 						; 関数cmake-ide-setupを呼ぶのはrtagsをrequireしてから。
;;     (cmake-ide-setup)))
 
;; irony 
(defun my-irony-mode-hook ()
  (define-key irony-mode-map
    [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map
    [remap complete-symbol]
    'irony-completion-at-point-async)
  )

(use-package irony
  :config
  (progn
	; ironyのビルド&インストール時にCMAKE_INSTALL_PREFIXで指定したディレクトリへのパス。
    (setq irony-server-install-prefix "~/.emacs.d")
	(custom-set-variables '(irony-additional-clang-options '("-std=c++11")))
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    (add-hook 'irony-mode-hook 'irony-eldoc)
    (add-to-list 'company-backends 'company-irony)
    )
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(irony-additional-clang-options (quote ("-std=c++11")))
 '(package-selected-packages
   (quote
	(irony-eldoc flycheck use-package php-mode go-mode ggtags exec-path-from-shell)))
 '(safe-local-variable-values
   (quote
	((cmake-ide-dir . /home/yamamoto/projects/intage/fastcontainer/build/Debug)
	 (cmake-ide-dir . ~/projects/intage/fastcontainer/build/Debug)))))

;;(add-hook 'c++-mode-hook 'flycheck-mode)
;;(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)

;; SQL
(add-hook 'sql-mode-hook
	  '(lambda()
	     (setq sql-indent-offset 4)
	     (setq sql-indent-maybe-tab t)
	     (setq sql-tab-width 4)))

;; PHP
(use-package php-mode)

;; markdown
(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; ----------------------------------------------------------------------------
;; clipbordを使ってコピペ
(cond
 (window-system (setq x-select-enable-clipboard t))
)

;; ----------------------------------------------------------------------------
;; @ OS
(if (eq window-system 'w32)	(load "init-win"))
(if (eq window-system 'x)	(load "init-linux"))
(if (eq window-system 'ns)	(load "init-mac"))
(if (eq window-system 'mac)	(load "init-mac"))

;; ---------------------------------------------------------------------------------
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "color-236")))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; 起動時のColor theme
;; (unless (display-graphic-p)
;;   	(load-theme 'manoj-dark)
;; 	(enable-theme 'manoj-dark))
(load-theme 'manoj-dark t)

;; IME OFF時の初期カーソルカラー
(set-cursor-color "green")

;; IME ON/OFF時のカーソルカラー
(add-hook 'input-method-activate-hook
		  (lambda() (set-cursor-color "red")))
(add-hook 'input-method-inactivate-hook
		  (lambda() (set-cursor-color "green")))

;; emacs term内のcolor設定
;; (setq ansi-term-color-vector
;;	  [unspecified "black" "red1" "lime green" "yellow2"
;;				   "DeepSkyBlue3" "magenta2" "cyan2" "white"])
(setq ansi-term-color-vector                                                
      [term
       term-color-black
       term-color-red
       term-color-green
       term-color-yellow
       term-color-cyan
       term-color-magenta                                                   
       term-color-cyan
       term-color-white
       term-color-black
       term-color-red
       term-color-green
       term-color-yellow
       term-color-cyan
       term-color-magenta
       term-color-cyan
       term-color-white])                              

;; ハイライト
(global-hl-line-mode t)                 ;; 現在行をハイライト

;; 対応する括弧のハイライト
(show-paren-mode t)                     ;; 対応する括弧をハイライト
(setq show-paren-style 'parenthesis)    ;; 対応する括弧だけをハイライト

;;; Frame parameters
(setq default-frame-alist
      (append '((vertical-scroll-bars . nil) ;;スクロールバーはいらない
		(vertical-scroll-bars . right)
		(line-spacing . 0)
		(cursor-type . box))
			  default-frame-alist))

(setq initial-frame-alist default-frame-alist)

;; フルスクリーン起動
(set-frame-parameter nil 'fullscreen 'maximized)
