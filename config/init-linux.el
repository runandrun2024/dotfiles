; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ------------------------------------------------------------------------
;; @ coding system
;; 言語環境
(set-language-environment "Japanese")

;; デフォルトの文字コード
;; 1.ファイルを新規作成した場合のデフォルト
;; 2.サブプロセスでのIO
;; 3.他の項目が指定されていない場合のデフォルト値
(prefer-coding-system 'utf-8-unix)

;; ファイル名
(set-file-name-coding-system 'utf-8)

;; キーボード
(set-keyboard-coding-system 'utf-8)

;; ターミナル(コンソール) - ターミナルモードで起動した場合
(set-terminal-coding-system 'utf-8)

;; なにも指定されていないサブプロセス（やネットワークストリーム） の入出力に使用する
(setq default-process-coding-system '(utf-8 . utf-8))

;; ------------------------------------------------------------------------
;; @ ime

;; 標準IMEの設定
;; (setq default-input-method "anthy")
(require 'mozc)
(setq default-input-method "japanese-mozc")

;; IME OFF時の初期カーソルカラー
(set-cursor-color "green")

;; IME ON/OFF時のカーソルカラー
(add-hook 'input-method-activate-hook
		  (lambda() (set-cursor-color "red")))
(add-hook 'input-method-inactivate-hook
		  (lambda() (set-cursor-color "green")))


;; markdown
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq markdown-command "/usr/local/bin/marked")

;; (setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.markdown" . gfm-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . gfm-mode) auto-mode-alist))

;; -------------------------------------------------------------------------------
;; auto complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; -------------------------------------------------------------------------------
;;; Frame parameters
(setq default-frame-alist
      (append '((foreground-color . "white")
		(background-color . "gray15")
		(mouse-color . "white")
		(cursor-color . "green")
		(vertical-scroll-bars . nil) ;;スクロールバーはいらない
		(width . 160)
		(height . 55)
		(left . 350)
		(vertical-scroll-bars . right)
		(line-spacing . 0)
		(cursor-type . box))
	      default-frame-alist))

(setq initial-frame-alist default-frame-alist)

;; ----------------------------------------------------------------------------
;; font
(add-to-list 'default-frame-alist '(font . "-unknown-Takaoゴシック-normal-normal-normal-*-16-*-*-*-d-0-iso10646-1"))
