; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ------------------------------------------------------------------------
;; @ coding system

;; 言語
(set-language-environment "Japanese")

;; デフォルトの文字コード
;; 1.ファイルを新規作成した場合のデフォルト
;; 2.サブプロセスでのIO
;; 3.他の項目が指定されていない場合のデフォルト値
(prefer-coding-system 'cp932-dos)

;; ファイル名
(set-file-name-coding-system 'cp932)

;; キーボード
(set-keyboard-coding-system 'cp932)

;; ターミナル(コンソール) - ターミナルモードで起動した場合
(set-terminal-coding-system 'cp932)

;; なにも指定されていないサブプロセス（やネットワークストリーム） の入出力に使用する
(setq default-process-coding-system '(cp932 . cp932))

;; ------------------------------------------------------------------------
;; @ ime

;; 標準IMEの設定
(setq default-input-method "W32-IME")

;; IME状態のモードライン表示
(setq-default w32-ime-mode-line-state-indicator "[Aa]")
(setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))

;; IMEの初期化
(w32-ime-initialize)

;; IME OFF時の初期カーソルカラー
(set-cursor-color "red")

;; IME ON/OFF時のカーソルカラー
(add-hook 'input-method-activate-hook
          (lambda() (set-cursor-color "green")))
(add-hook 'input-method-inactivate-hook
          (lambda() (set-cursor-color "red")))

;; バッファ切り替え時にIME状態を引き継ぐ
(setq w32-ime-buffer-switch-p nil)

;; ------------------------------------------------------------------------
;; @ encode

   ;; 機種依存文字
   ;; (require 'cp5022x)
   ;; (define-coding-system-alias 'euc-jp 'cp51932)

   ;; decode-translation-table の設定
   (coding-system-put 'euc-jp :decode-translation-table
              (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
   (coding-system-put 'iso-2022-jp :decode-translation-table
              (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
   (coding-system-put 'utf-8 :decode-translation-table
              (get 'japanese-ucs-jis-to-cp932-map 'translation-table))

   ;; encode-translation-table の設定
   (coding-system-put 'euc-jp :encode-translation-table
              (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
   (coding-system-put 'iso-2022-jp :encode-translation-table
              (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
   (coding-system-put 'cp932 :encode-translation-table
              (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
   (coding-system-put 'utf-8 :encode-translation-table
              (get 'japanese-ucs-jis-to-cp932-map 'translation-table))

   ;; charset と coding-system の優先度設定
   (set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
             'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
   (set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)

   ;; PuTTY 用の terminal-coding-system の設定
   (apply 'define-coding-system 'utf-8-for-putty
      "UTF-8 (translate jis to cp932)"
      :encode-translation-table 
      (get 'japanese-ucs-jis-to-cp932-map 'translation-table)
      (coding-system-plist 'utf-8))
   (set-terminal-coding-system 'utf-8-for-putty)

   ;; East Asian Ambiguous
   (defun set-east-asian-ambiguous-width (width)
     (while (char-table-parent char-width-table)
       (setq char-width-table (char-table-parent char-width-table)))
     (let ((table (make-char-table nil)))
       (dolist (range 
            '(#x00A1 #x00A4 (#x00A7 . #x00A8) #x00AA (#x00AD . #x00AE)
             (#x00B0 . #x00B4) (#x00B6 . #x00BA) (#x00BC . #x00BF)
             #x00C6 #x00D0 (#x00D7 . #x00D8) (#x00DE . #x00E1) #x00E6
             (#x00E8 . #x00EA) (#x00EC . #x00ED) #x00F0 
             (#x00F2 . #x00F3) (#x00F7 . #x00FA) #x00FC #x00FE
             #x0101 #x0111 #x0113 #x011B (#x0126 . #x0127) #x012B
             (#x0131 . #x0133) #x0138 (#x013F . #x0142) #x0144
             (#x0148 . #x014B) #x014D (#x0152 . #x0153)
             (#x0166 . #x0167) #x016B #x01CE #x01D0 #x01D2 #x01D4
             #x01D6 #x01D8 #x01DA #x01DC #x0251 #x0261 #x02C4 #x02C7
             (#x02C9 . #x02CB) #x02CD #x02D0 (#x02D8 . #x02DB) #x02DD
             #x02DF (#x0300 . #x036F) (#x0391 . #x03A9)
             (#x03B1 . #x03C1) (#x03C3 . #x03C9) #x0401 
             (#x0410 . #x044F) #x0451 #x2010 (#x2013 . #x2016)
             (#x2018 . #x2019) (#x201C . #x201D) (#x2020 . #x2022)
             (#x2024 . #x2027) #x2030 (#x2032 . #x2033) #x2035 #x203B
             #x203E #x2074 #x207F (#x2081 . #x2084) #x20AC #x2103
             #x2105 #x2109 #x2113 #x2116 (#x2121 . #x2122) #x2126
             #x212B (#x2153 . #x2154) (#x215B . #x215E) 
             (#x2160 . #x216B) (#x2170 . #x2179) (#x2190 . #x2199)
             (#x21B8 . #x21B9) #x21D2 #x21D4 #x21E7 #x2200
             (#x2202 . #x2203) (#x2207 . #x2208) #x220B #x220F #x2211
             #x2215 #x221A (#x221D . #x2220) #x2223 #x2225
             (#x2227 . #x222C) #x222E (#x2234 . #x2237)
             (#x223C . #x223D) #x2248 #x224C #x2252 (#x2260 . #x2261)
             (#x2264 . #x2267) (#x226A . #x226B) (#x226E . #x226F)
             (#x2282 . #x2283) (#x2286 . #x2287) #x2295 #x2299 #x22A5
             #x22BF #x2312 (#x2460 . #x24E9) (#x24EB . #x254B)
             (#x2550 . #x2573) (#x2580 . #x258F) (#x2592 . #x2595) 
             (#x25A0 . #x25A1) (#x25A3 . #x25A9) (#x25B2 . #x25B3)
             (#x25B6 . #x25B7) (#x25BC . #x25BD) (#x25C0 . #x25C1)
             (#x25C6 . #x25C8) #x25CB (#x25CE . #x25D1) 
             (#x25E2 . #x25E5) #x25EF (#x2605 . #x2606) #x2609
             (#x260E . #x260F) (#x2614 . #x2615) #x261C #x261E #x2640
             #x2642 (#x2660 . #x2661) (#x2663 . #x2665) 
             (#x2667 . #x266A) (#x266C . #x266D) #x266F #x273D
             (#x2776 . #x277F) (#xE000 . #xF8FF) (#xFE00 . #xFE0F) 
             #xFFFD
             ))
     (set-char-table-range table range width))
       (optimize-char-table table)
       (set-char-table-parent table char-width-table)
       (setq char-width-table table)))
   (set-east-asian-ambiguous-width 2)

   ;; emacs-w3m
   (eval-after-load "w3m"
     '(when (coding-system-p 'cp51932)
        (add-to-list 'w3m-compatible-encoding-alist '(euc-jp . cp51932))))

   ;; Gnus
   (eval-after-load "mm-util"
     '(when (coding-system-p 'cp50220)
        (add-to-list 'mm-charset-override-alist '(iso-2022-jp . cp50220))))

   ;; SEMI (cf. http://d.hatena.ne.jp/kiwanami/20091103/1257243524)
   (eval-after-load "mcs-20"
     '(when (coding-system-p 'cp50220)
        (add-to-list 'mime-charset-coding-system-alist 
             '(iso-2022-jp . cp50220))))

   ;; 全角チルダ/波ダッシュをWindowsスタイルにする
   (let ((table (make-translation-table-from-alist '((#x301c . #xff5e))) ))
     (mapc
      (lambda (coding-system)
        (coding-system-put coding-system :decode-translation-table table)
        (coding-system-put coding-system :encode-translation-table table)
        )
      '(utf-8 cp932 utf-16le)))

;; ------------------------------------------------------------------------
;; @ font
   ;; (set-face-attribute 'default nil :family "MS Gothic" :height 110)
   (set-fontset-font nil 'ascii (font-spec :family "MS Gothic" :size 18))
   (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "MS Gothic"))
;;   (setq face-font-rescale-alist '(("" . 1.04)))


   ;; 標準フォントの設定
   ;; (set-default-font "M+2VM+IPAG circle-12")

   ;; IME変換時フォントの設定（テストバージョンのみ）
   ;; (setq w32-ime-font-face "MigMix 1M")
   ;; (setq w32-ime-font-height 22)

   ;; 固定等幅フォントの設定
   ;; (set-face-attribute 'fixed-pitch    nil :family "M+2VM+IPAG circle")

   ;; 可変幅フォントの設定
   ;; (set-face-attribute 'variable-pitch nil :family "M+2VM+IPAG circle")


;; ------------------------------------------------------------------------
;; @ image-library
   (setq image-library-alist
         '((xpm "libxpm.dll")
           (png "libpng14.dll")
           (jpeg "libjpeg.dll")
           (tiff "libtiff3.dll")
           (gif "libungif4.dll")
           (svg "librsvg-2-2.dll")
           (gdk-pixbuf "libgdk_pixbuf-2.0-0.dll")
           (glib "libglib-2.0-0.dll")
           (gobject "libgobject-2.0-0.dll"))
         )

;; ------------------------------------------------------------------------
;; @ print

   (require 'cl)
   (defun listsubdir (basedir)
     (remove-if (lambda (x) (not (file-directory-p x)))
                (directory-files basedir t "^[^.]")))

   (setq ps-print-color-p t
         ps-lpr-command "gswin32c.exe"
         ps-multibyte-buffer 'non-latin-printer
         ps-lpr-switches '("-sDEVICE=mswinpr2" "-dNOPAUSE" "-dBATCH" "-dWINKANJI")
         printer-name nil
         ps-printer-name nil
         ps-printer-name-option nil
         ps-print-header nil          ; ヘッダの非表示
         )

;; ------------------------------------------------------------------------
;; @ setup-cygwin
;;   (setq cygwin-mount-cygwin-bin-directory
;;         (concat (getenv "CYGWIN_DIR") "\\bin"))
;;   (require 'setup-cygwin)

;; ------------------------------------------------------------------------
;; @ shell
   (require 'shell)
   (setq explicit-shell-file-name "bash.exe")
   (setq shell-command-switch "-c")
   (setq shell-file-name "bash.exe")

   ;; (M-! and M-| and compile.el)
   (setq shell-file-name "bash.exe")
   (modify-coding-system-alist 'process ".*sh\\.exe" 'cp932)

   ;; shellモードの時の^M抑制
   (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

   ;; shell-modeでの補完 (for drive letter)
   (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")

   ;; エスケープシーケンス処理の設定
   (autoload 'ansi-color-for-comint-mode-on "ansi-color"
             "Set `ansi-color-for-comint-mode' to t." t)

   (setq shell-mode-hook
         (function
          (lambda ()

            ;; シェルモードの入出力文字コード
            (set-buffer-process-coding-system 'sjis-dos 'sjis-unix)
            (set-buffer-file-coding-system    'sjis-unix)
            )))

;; ------------------------------------------------------------------------
;; @ menu-tree
   ;; (setq menu-tree-coding-system 'utf-8)
   ;; (require 'menu-tree)

;; ------------------------------------------------------------------------
;; @ migemo/cmigemo
;;   (setq migemo-command (concat (getenv "INST_DIR")
;;                                "\\app\\cmigemo\\cmigemo"))
;;   (setq migemo-options '("-q" "--emacs"))
;;   (setq migemo-dictionary (concat (getenv "INST_DIR")
;;                                   "\\app\\cmigemo\\dict\\utf-8\\migemo-dict"))
;;   (setq migemo-user-dictionary nil)
;;   (setq migemo-regex-dictionary nil)
;;   (setq migemo-use-pattern-alist t)
;;   (setq migemo-use-frequent-pattern-alist t)
;;   (setq migemo-pattern-alist-length 1024)
;;   (setq migemo-coding-system 'utf-8-unix)
;;   (load-library "migemo")
;;   (migemo-init)

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
;;		(ime-font . "-*-ＭＳ ゴシック-*-*-*-*-12-*-*-*-*-*-jisx0208-sjis")
;;		(font . "-*-ＭＳ ゴシック-*-*-*-*-16-*-*-*-*-*-jisx0208-sjis")
		(vertical-scroll-bars . right)
		(line-spacing . 0)
		(cursor-type . box))
	      default-frame-alist))

;;; ===== 印刷 =====
(setq exec-path (cons "c:\\Application\\xyzzy" exec-path))
(setq print-region-function
 (lambda (start end
        &optional lpr-prog
        delete-text buf display
        &rest rest)
        (let* ((procname (make-temp-name "w32-print-"))
        (tempfile
        (subst-char-in-string
        ?/ ?\\
        (expand-file-name procname temporary-file-directory)))
        (coding-system-for-write 'cp932-dos))
        (write-region start end tempfile)
        (set-process-sentinel
        (start-process procname nil "xyzzy.exe" tempfile)
        (lambda (process event)
        (let ((tempfile
        (expand-file-name (process-name process)
        temporary-file-directory)))
        (when (file-exists-p tempfile)
        (delete-file tempfile)))))))) 

(setq initial-frame-alist default-frame-alist)

(cd "~/")
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((encoding . cp932)))))
(custom-set-faces)

