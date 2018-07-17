;;;===============================================================================
;;; package
;;; required packages are installed in initialization.
;;;===============================================================================
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(require 'cl)
(defvar installing-package-list '(
			       ;;package list
			       linum
			       tabbar
			       hiwin
			       auto-complete
			       popwin
			       rainbow-delimiters
			       helm
			       migemo
			       eldoc
			       rainbow-mode
			       sudo-edit
			       material-theme
			       google-translate
			       clojure-mode
			       ))
(let ((not-installed (loop for x in installing-package-list
			   when (not (package-installed-p x))
			   collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))


;;;===============================================================================
;;; title-insert
;;; init.el 編集用
;;;===============================================================================

(defun title-insert()
  (interactive)
  (insert ";;;===============================================================================
;;; title
;;; comment
;;;==============================================================================="
	  ))

;;;===============================================================================
;;; my-config                                                                    
;;;===============================================================================

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

(define-key global-map (kbd "\C-h") 'delete-backward-char)
(define-key key-translation-map (kbd "\C-h") (kbd "<DEL>"))
(define-key global-map (kbd "\C-k") 'kill-whole-line)
(define-key global-map (kbd "\C-c C-k") 'kill-line)
(define-key global-map (kbd "\C-a") 'back-to-indentation)
(define-key global-map (kbd "\C-x C-M-x") 'edebug-defun)
(define-key global-map (kbd "\C-z") 'set-mark-command)
(global-set-key [f8] 'eshell)

(setq scroll-step 1)
(scroll-bar-mode -1)
(menu-bar-mode 1)
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

;; カーソルの点滅
(blink-cursor-mode 0)
;; 初期画面の設定
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

;; 起動時最大化
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; 起動時フルスクリーン
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;; clojure-mode を有効化
(require 'clojure-mode)

;; 対応括弧を強調
(show-paren-mode 1)
(setq show-paren-style 'mixed)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match-face "blue")

;; フレーム タイトル
(setq frame-title-format
      '("emacs " emacs-version (buffer-file-name " - %f")))

;; recenter-top-bottomのポジション
(setq recenter-positions '(middle top bottom))
(setq-default truncate-partial-width-windows nil)


;; 行番号の表示
;; (line-number-mode t)

;; 列番号の表示
;; (column-number-mode t)

;;;===============================================================================
;;; dired
;;; qでdiredを閉じる
;;;===============================================================================
(setq dired-dwin-target t)
(defun dired-mode-hooks()
  (local-set-key "q" '(lambda()
                        "diredを閉じる"
                        (interactive)
                        (kill-buffer (current-buffer)))))
(add-hook 'dired-mode-hook 'dired-mode-hooks)


;;;===============================================================================
;;; eshell
;;; F8でeshell
;;;===============================================================================

(setq eshell-path-env (getenv "PATH"))
(global-set-key [f8] 'eshell) ;f8 eshell
;;(add-hook 'after-init-hook (lambda() (eshell))) ;startup eshell
(setq eshell-ask-to-save-history 'always)
(setq eshell-cmpl-ignore-case t)
(setq eshell-cmpl-cycle-completions t)
(setq eshell-cmpl-cycle-cutoff-length 5)
(setq eshell-hist-ignoredups t)
(setq eshell-history-size 1000)
(setq eshell-prompt-function
      (lambda ()
        (concat "\n[tanai3@Tanai3sMac] " (eshell/pwd)
                (if (= (user-uid) 0) "\n# " "\n$ ")
                )))


(add-hook 'eshell-mode-hook
          '(lambda()
             (progn
               (define-key eshell-mode-map "\C-a" 'eshell-bol)
               (define-key eshell-mode-map "\C-c \C-k" 'eshell-kill-process)
               (define-key eshell-mode-map "\C-k" 'kill-line)
               (define-key eshell-mode-map "\C-c \C-c" 'eshell-interrupt-process)
               )))
(defadvice eshell-get-old-input (after eshell-read-only-korosu activate)
  (setq ad-return-value (substring-no-properties ad-return-value)))
(setq eshell-command-aliases-list
      (append
       (list
        (list "ll" "ls -l")
        (list "la" "ls -a"))))
(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; (eshell-send-input)
    ))
;;---------------------------------------------------------------------------


;;;===============================================================================
;;; my-elisp
;;;===============================================================================
(require 'my-comment-out)
(define-key global-map (kbd "C-;") 'my-comment-out)
(define-key global-map (kbd "C-x ;") 'my-comment-out)

(require 'duplicate-current-line)
(define-key global-map (kbd "M-c") 'duplicate-current-line)

(require 'move-line)
(define-key global-map (kbd "<M-up>") 'move-line-up)
(define-key global-map (kbd "<M-down>") 'move-line-down)

;; Dropbox以下のファイルをemacs間自動共有
(require 'auto-revert-file)

;;;===============================================================================
;;; window-split
;;; C-t でバッファ分割　分割済みならバッファを移動
;;;===============================================================================
(defun other-window-or-split()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)

;;;===============================================================================
;;; all-indent
;;; バッファ全体をインデント適応
;;;===============================================================================
;; all-indent
(defun all-indent()
  (interactive)
  (save-excursion
    (indent-region(point-min)(point-max))))

;;;===============================================================================
;;; cut-or-kill-word
;;; C-w をマーク中ならマーク範囲カット　そうでないなら単語削除
;;;===============================================================================
(defun cut-or-kill-word()
  (interactive)
  (save-excursion
    (if (use-region-p)
	(kill-region (region-beginning) (region-end))
      (backward-kill-word 1))))
(define-key global-map (kbd "\C-w") 'cut-or-kill-word)

;;;===============================================================================
;;; quit-buffer
;;;===============================================================================
(defun my-quit-buffer()
  (interactive)
  (kill-buffer (current-buffer))
  (and (not (one-window-p)) (delete-window))
  (abort-recursive-edit))
(define-key completion-list-mode-map "q" 'my-quit-buffer)

;;;===============================================================================
;;; sudo-edit
;;; M-x sudo-edit でファイルを管理者権限で開き直す
;;;===============================================================================
(require 'sudo-edit)

;;;===============================================================================
;;; linum
;;; 遅延表示することで負荷軽減
;;;===============================================================================
(require 'linum)
;; 行移動を契機に描画
(defvar linum-line-number 0)
(declare-function linum-update-current "linum" ())
(defadvice linum-update-current
    (around linum-update-current-around activate compile)
  (unless (= linum-line-number (line-number-at-pos))
    (setq linum-line-number (line-number-at-pos))
    ad-do-it
    ))

;; バッファ中の行番号表示の遅延設定
(defvar linum-delay nil)
(setq linum-delay t)
(defadvice linum-schedule (around linum-schedule-around () activate)
  (run-with-idle-timer 1.0 nil #'linum-update-current))

;; 行番号の書式
(defvar linum-format nil)
(setq linum-format "%5d")

;; バッファ中の行番号表示
(global-linum-mode t)

;; 文字サイズ
(set-face-attribute 'linum nil :height 0.50)

;;;===============================================================================
;;; anything
;;;===============================================================================

;; (require 'anything-config)
;; (require 'anything-match-plugin)
;; (require 'anything-startup nil t)
;; (require 'anything-migemo)
;; (define-key global-map (kbd "\C-x C-b") 'anything)
;; (define-key global-map (kbd "M-y") 'anything-show-kill-ring)
;; (define-key global-map (kbd "\C-c i") 'anything-imenu)
;; (define-key global-map (kbd "M-x") 'anything-M-x)
;; (setq desktop-globals-to-save '(extended-command-history))
;; (setq desktop-files-not-to-save "")
;; (desktop-save-mode 1)

;;;===============================================================================
;;; helm
;;;===============================================================================

(require 'helm)
(require 'helm-config)
(helm-mode 1)
(define-key global-map (kbd "C-x C-b") 'helm-mini)
(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key global-map (kbd "C-c i") 'helm-imenu)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
;; (Setq desktop-globals-to-save '(extended-command-history))
;; (setq desktop-files-not-to-save "")
;; (desktop-save-mode 1)

;;;===============================================================================
;;; auto-complete
;;;===============================================================================

(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-delay 0.5)
(setq ac-auto-start 4)
(ac-set-trigger-key "TAB")

;;;===============================================================================
;;; eldoc
;;; 関数定義をミニバッファに表示
;;;===============================================================================

(require 'eldoc)
;; (require 'eldoc-extension)
(setq eldoc-idle-delay 0.05)
(setq eldoc-echo-area-use-multiline-p t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)

;;;===============================================================================
;;; rainbow-mode
;;; カラーコードに色表示
;;; #ffffff #123450
;;;===============================================================================

(require 'rainbow-mode)
(add-hook 'python-mode-hook 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)

;;;===============================================================================
;;; rainbow-delimiters
;;; 括弧を階層ごとに色付け
;;;===============================================================================

(require 'rainbow-delimiters)
(defun my-rainbow-delimiters-mode-turn-on ()
  (rainbow-delimiters-mode t))
(add-hook 'emacs-lisp-mode-hook 'my-rainbow-delimiters-mode-turn-on)
(add-hook 'java-mode-hook 'my-rainbow-delimiters-mode-turn-on)
(add-hook 'python-mode-hook 'my-rainbow-delimiters-mode-turn-on)
(add-hook 'clojure-mode-hook 'my-rainbow-delimiters-mode-turn-on)

;;;===============================================================================
;;; tabbar
;;;===============================================================================
(require 'tabbar)

;; tabbar有効化
(call-interactively 'tabbar-mode t)

;; タブ切替にマウスホイールを使用（0：有効，-1：無効）
(call-interactively 'tabbar-mwheel-mode -1)
(remove-hook 'tabbar-mode-hook      'tabbar-mwheel-follow)
(remove-hook 'mouse-wheel-mode-hook 'tabbar-mwheel-follow)

;; タブグループを使用（t：有効，nil：無効）
(defvar tabbar-buffer-groups-function nil)
(setq tabbar-buffer-groups-function nil)

;; タブの表示間隔
(defvar tabbar-separator nil)
(setq tabbar-separator '(1.0))

;; タブ切り替え
(global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
(global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)

;;;===============================================================================
;;; hl-line+
;;; 現在行を強調表示
;;;===============================================================================
;; (require 'hl-line+)
;; (toggle-hl-line-when-idle)
;; (set-face-background 'hl-line "navy")

;;;===============================================================================
;;; google-translate
;;; マークで囲った範囲をGoogle翻訳に投げる
;;;===============================================================================
(require 'google-translate)
(defvar google-translate-english-chars "[:ascii:]’“”–"
  "これらの文字が含まれているときは英語とみなす")
(defun google-translate-enja-or-jaen (&optional string)
  "regionか、現在のセンテンスを言語自動判別でGoogle翻訳する。"
  (interactive)
  (setq string
        (cond ((stringp string) string)
              (current-prefix-arg
               (read-string "Google Translate: "))
              ((use-region-p)
               (buffer-substring (region-beginning) (region-end)))
              (t
               (save-excursion
                 (let (s)
                   (forward-char 1)
                   (backward-sentence)
                   (setq s (point))
                   (forward-sentence)
                   (buffer-substring s (point)))))))
  (let* ((asciip (string-match
                  (format "\\`[%s]+\\'" google-translate-english-chars)
                  string)))
    (run-at-time 0.1 nil 'deactivate-mark)
    (google-translate-translate
     (if asciip "en" "ja")
     (if asciip "ja" "en")
     string)))
(global-set-key (kbd "\C-x C-t") 'google-translate-enja-or-jaen)


;;;===============================================================================
;;; popwin
;;; バッファをポップアップ表示
;;;===============================================================================
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer) ;;emacs26.1以降では動かない
;; (setq special-display-function 'popwin:special-display-popup-window)
;; ポップアップを画面下に表示
(setq popwin:popup-window-position 'bottom)
;; Google-translate.elの翻訳バッファをポップアップで表示させる
(push '("*Google Translate*") popwin:special-display-config)

;;;===============================================================================
;;; aspell
;;; スペルチェッカ
;;;===============================================================================
;; aspell
(setq-default ispell-program-name "/usr/local/bin/aspell")
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))


;;;===============================================================================
;;; hiwin
;;; comment
;;;===============================================================================
;; (require 'hiwin)

;; hiwin-modeを有効化
;; アクティブなウィンドウを強調
;; (hiwin-activate)
;; (set-face-background 'hiwin-face "gray30")

;;;===============================================================================
;;; migemo
;;; "migemo" -> "みげも" を検索可能になる
;;; cmigemoのバイナリが必要 yaourtで入る
;;;===============================================================================
(require 'migemo)
(setq migemo-command "/usr/local/bin/cmigemo")
(setq migemo-options '("-q" "--emacs"))

;; Set your installed path
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(migemo-init)

;; google-material のテーマ適応
(load-theme 'material t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.p
 '(package-selected-packages
   (quote
    (minimap clojure-mode google-translate material-theme regex-tool ag magit rainbow-mode migemo helm rainbow-delimiters popwin auto-complete hiwin tabbar)))
 '(tabbar-mode t nil (tabbar)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;===============================================================================
;;; backup
;;;===============================================================================
(setq make-backup-files nil) ;; バックアップファイル無効
(setq auto-save-default nil) ;; 自動保存ファイル無効

;;;===============================================================================
;;; Mac用設定
;;;===============================================================================
(setq ns-command-modifier (quote meta))   ;; CommandキーをMeta扱い
(setq ns-alternate-modifier (quote meta)) ;; AltキーをMeta扱い
(set-face-attribute 'default nil :family "Source Code Pro" :height 150) ;; fontsize = (* 15 10) points
(setq ring-bell-function 'ignore) ;; 警告音を無効化
(define-key global-map [?¥] [?\\])  ;; ¥の代わりにバックスラッシュを入力する
(setq redisplay-dont-pause nil) ;; 日本語入力のちらつき防止 obsoleteな変数なのでいつかサポート対象外になるかも
