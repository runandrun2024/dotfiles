;; region の色
(set-face-background 'region "SkyBlue")
(set-face-foreground 'region "black")


;; メニューバーを隠す
(tool-bar-mode -1)

;; フォント設定
(set-face-attribute 'default nil :family "monaco" :height 120)
(set-fontset-font
 (frame-parameter nil 'font)
 'japanese-jisx0208
 '("Hiragino Kaku Gothic ProN" . "iso10646-1"))
(set-fontset-font
 (frame-parameter nil 'font)
 'japanese-jisx0212
 '("Hiragino Kaku Gothic ProN" . "iso10646-1")) 
(set-fontset-font
 (frame-parameter nil 'font)
 'mule-unicode-0100-24ff
 '("monaco" . "iso10646-1"))
(setq face-font-rescale-alist
      '(("^-apple-hiragino.*" . 1.2)
	(".*osaka-bold.*" . 1.2)
	(".*osaka-medium.*" . 1.2)
	(".*courier-bold-.*-mac-roman" . 1.0)
	(".*monaco cy-bold-.*-mac-cyrillic" . 0.9) (".*monaco-bold-.*-mac-roman" . 0.9)
	("-cdac$" . 1.3)))

(if (boundp 'window-system)
    (setq initial-frame-alist
          (append (list
                   '(foreground-color . "azure3") ;; 文字が白
                   '(background-color . "black") ;; 背景は黒
                   '(border-color     . "black")
                   '(mouse-color      . "white")
                   '(cursor-color     . "white")
                   '(cursor-type      . box)
                   '(menu-bar-lines . 1)
                   ;;15.2 フォントの設定 (2008/04/16) で設定したフォントを使用
                   ;;'(font . "my-fontset")
                   ;; 東雲なら shinonome16-fontset などを指定
                  initial-frame-alist))))
(setq default-frame-alist initial-frame-alist)

;; 日本語モードの設定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-buffer-file-coding-system 'utf-8)
;; (setq default-buffer-file-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
(setq default-input-method "MacOSX")
(require 'package)
