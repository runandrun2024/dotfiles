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

;; ----------------------------------------------------------------------------
;; font
(add-to-list 'default-frame-alist '(font . "-*-Takaoゴシック-normal-normal-normal-*-16-*-*-*-d-0-iso10646-1"))
