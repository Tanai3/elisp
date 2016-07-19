;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Dropbox                                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ site-lisp                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(let ( (default-directory
         (file-name-as-directory (concat user-emacs-directory "site-lisp")))
       )
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path)
  )


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - coding system                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; デフォルトの文字コード
(set-default-coding-systems 'utf-8-unix)

;; テキストファイル／新規バッファの文字コード
(prefer-coding-system 'utf-8-unix)

;; ファイル名の文字コード
(set-file-name-coding-system 'utf-8-unix)

;; キーボード入力の文字コード
(set-keyboard-coding-system 'utf-8-unix)

;; サブプロセスのデフォルト文字コード
(setq default-process-coding-system '(undecided-dos . utf-8-unix))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ key binding - keyboard                                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; Altキーを使用せずにMetaキーを使用
;;(setq w32-alt-is-meta nil)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - fontset                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ;; デフォルト フォント
;; (set-face-attribute 'default nil :family "Migu 1M" :height 110)

;; ;; プロポーショナル フォント
;; (set-face-attribute 'variable-pitch nil :family "Migu 1M" :height 110)

;; ;; 等幅フォント
;; (set-face-attribute 'fixed-pitch nil :family "Migu 1M" :height 110)

;; ;; ツールチップ表示フォント
;; (set-face-attribute 'tooltip nil :family "Migu 1M" :height 90)

;; ;;; fontset

;; ;; フォントサイズ調整
;; (global-set-key (kbd "C-<wheel-up>")   '(lambda() (interactive) (text-scale-increase 1)))
;; (global-set-key (kbd "C-=")            '(lambda() (interactive) (text-scale-increase 1)))
;; (global-set-key (kbd "C-<wheel-down>") '(lambda() (interactive) (text-scale-decrease 1)))
;; (global-set-key (kbd "C--")            '(lambda() (interactive) (text-scale-decrease 1)))

;; ;; フォントサイズ リセット
;; (global-set-key (kbd "M-0") '(lambda() (interactive) (text-scale-set 0)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - frame                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; (setq default-frame-alist
;;       (append '((width                . 85)  ; フレーム幅
;;                 (height               . 38 ) ; フレーム高
;; 		;; (left                 . 70 ) ; 配置左位置
;; 		;; (top                  . 28 ) ; 配置上位置
;;                 (line-spacing         . 0  ) ; 文字間隔
;;                 (left-fringe          . 10 ) ; 左フリンジ幅
;;                 (right-fringe         . 11 ) ; 右フリンジ幅
;;                 (menu-bar-lines       . 1  ) ; メニューバー
;;                 (tool-bar-lines       . 1  ) ; ツールバー
;;                 (vertical-scroll-bars . 1  ) ; スクロールバー
;;                 (scroll-bar-width     . 0 ) ; スクロールバー幅
;;                 (cursor-type          . box) ; カーソル種別
;;                 (alpha                . 100) ; 透明度
;;                 ) default-frame-alist) )
;; (setq initial-frame-alist default-frame-alist)

;; フレーム タイトル
(setq frame-title-format
      '("emacs " emacs-version (buffer-file-name " - %f")))

;; 初期画面の非表示
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - mode line                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 行番号の表示
(line-number-mode t)

;; 列番号の表示
(column-number-mode t)

;; モードライン カスタマイズ
(setq-default
 mode-line-format
 `(
   ""
   w32-ime-mode-line-state-indicator
   " "
   mode-line-mule-info
   mode-line-modified
   mode-line-frame-identification
   mode-line-buffer-identification
   " "
   global-mode-string
   " %[("
   mode-name
   mode-line-process
   "%n"
   ")%] "
   (which-func-mode ("" which-func-format " "))
   (line-number-mode
    (:eval
     (format "L%%l/L%d " (count-lines (point-max) 1) )))
   (column-number-mode " C%c ")
   (-3 . "%p")
   )
 )
(setq mode-line-frame-identification " ")

;; cp932エンコードの表記変更
(coding-system-put 'cp932 :mnemonic ?P)
(coding-system-put 'cp932-dos :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac :mnemonic ?P)

;; UTF-8エンコードの表記変更
(coding-system-put 'utf-8 :mnemonic ?U)
(coding-system-put 'utf-8-with-signature :mnemonic ?u)

;; 改行コードの表記追加
(setq eol-mnemonic-dos       ":Dos ")
(setq eol-mnemonic-mac       ":Mac ")
(setq eol-mnemonic-unix      ":Unx ")
(setq eol-mnemonic-undecided ":??? ") 


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - buffer                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; バッファ画面外文字の切り詰め表示
(setq truncate-lines nil)

;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
(setq truncate-partial-width-windows t)

;; 同一バッファ名にディレクトリ付与
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - cursor                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; カーソルの点滅
(blink-cursor-mode 0)

;; 非アクティブウィンドウのカーソル表示
(setq-default cursor-in-non-selected-windows t)

;; IME無効／有効時のカーソルカラー定義
(unless (facep 'cursor-ime-off)
  (make-face 'cursor-ime-off)
  (set-face-attribute 'cursor-ime-off nil
                      :background "DarkRed" :foreground "White")
  )
(unless (facep 'cursor-ime-on)
  (make-face 'cursor-ime-on)
  (set-face-attribute 'cursor-ime-on nil
                      :background "DarkGreen" :foreground "White")
  )

;; IME無効／有効時のカーソルカラー設定
(add-hook
 'input-method-inactivate-hook
 '(lambda()
    (if (facep 'cursor-ime-off)
        (let ( (fg (face-attribute 'cursor-ime-off :foreground))
               (bg (face-attribute 'cursor-ime-off :background)) )
          (set-face-attribute 'cursor nil :foreground fg :background bg)
          )
      )
    )
 )
(add-hook
 'input-method-activate-hook
 '(lambda()
    (if (facep 'cursor-ime-on)
        (let ( (fg (face-attribute 'cursor-ime-on :foreground))
               (bg (face-attribute 'cursor-ime-on :background)) )
          (set-face-attribute 'cursor nil :foreground fg :background bg)
          )
      )
    )
 )

;; バッファ切り替え時の状態引継ぎ設定
(setq w32-ime-buffer-switch-p nil)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - linum                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

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


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - tabbar                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ search - isearch                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 大文字・小文字を区別しないでサーチ
(setq-default case-fold-search nil)

;; インクリメント検索時に縦スクロールを有効化
(setq isearch-allow-scroll nil)

;; C-dで検索文字列を一文字削除
(define-key isearch-mode-map (kbd "C-d") 'isearch-delete-char)
;; C-yで検索文字列にヤンク貼り付け
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)

;; C-eで検索文字列を編集
(define-key isearch-mode-map (kbd "C-e") 'isearch-edit-string)

;; Tabで検索文字列を補完
(define-key isearch-mode-map (kbd "TAB") 'isearch-yank-word)

;; C-gで検索を終了
(define-key isearch-mode-map (kbd "C-g")
  '(lambda() (interactive) (isearch-done)))

;; 日本語の検索文字列をミニバッファに表示
(define-key isearch-mode-map (kbd "<compend>")
  '(lambda() (interactive) (isearch-update)))
(define-key isearch-mode-map (kbd "<kanji>")
  'isearch-toggle-input-method)
(add-hook
 'isearch-mode-hook
 '(lambda() (setq w32-ime-composition-window (minibuffer-window)))
 )
(add-hook
 'isearch-mode-end-hook
 '(lambda() (setq w32-ime-composition-window nil))
 )


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - hiwin                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'hiwin)

;; hiwin-modeを有効化
(hiwin-activate)
(set-face-background 'hiwin-face "gray10")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;  @search - migemo                                               ;;;
;;  https://github.com/emacs-jp/migemo                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ file - backup                                                 ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ファイルオープン時のバックアップ（~）
(setq make-backup-files   t)  ;; 自動バックアップの実行有無
(setq version-control     t)  ;; バックアップファイルへの番号付与
(setq kept-new-versions   3)  ;; 最新バックアップファイルの保持数
(setq kept-old-versions   0)  ;; 最古バックアップファイルの保持数
(setq delete-old-versions t)  ;; バックアップファイル削除の実行有無

;; ファイルオープン時のバックアップ（~）の格納ディレクトリ
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "/tmp/emacsbk"))
            backup-directory-alist))

;; 編集中ファイルの自動バックアップ
(setq backup-inhibited nil)

;; 終了時に自動バックアップファイルを削除
(setq delete-auto-save-files nil)

;; 編集中ファイルのバックアップ
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)

;; 編集中ファイルのバックアップ間隔（秒）
(setq auto-save-timeout 3)

;; 編集中ファイルのバックアップ間隔（打鍵）
(setq auto-save-interval 100)

;; 編集中ファイル（##）の格納ディレクトリ
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "/tmp/emacsbk") t)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ file - lockfile                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ロックファイルの生成を抑止
(setq create-lockfiles nil)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ scroll                                                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; スクロール時のカーソル位置を維持
(setq scroll-preserve-screen-position t)

;; スクロール開始の残り行数
(setq scroll-margin 0)

;; スクロール時の行数
(setq scroll-conservatively 10000)

;; スクロール時の行数（scroll-marginに影響せず）
(setq scroll-stp 0)

;; 画面スクロール時の重複表示する行数
(setq next-screen-context-lines 1)

;; キー入力中の画面更新を抑止
;;(setq redisplay-dont-pause t)

;; recenter-top-bottomのポジション
(setq recenter-positions '(middle top bottom))

;; 横スクロール開始の残り列数
(setq hscroll-margin 1)

;; 横スクロール時の列数
(setq hscroll-step 1)

;; スクロールダウン
(global-set-key (kbd "C-z") 'scroll-down)

;; バッファの最後までスクロールダウン
(defadvice scroll-down (around scroll-down activate compile)
  (interactive)
  (let (
        (bgn-num (+ 1 (count-lines (point-min) (point))))
        )
    (if (< bgn-num (window-height))
        (goto-char (point-min))
      ad-do-it) ))

;; バッファの先頭までスクロールアップ
(defadvice scroll-up (around scroll-up activate compile)
  (interactive)
  (let (
        (bgn-num (+ 1 (count-lines (point-min) (point))))
        (end-num nil)
        )
    (save-excursion
      (goto-char (point-max))
      (setq end-num (+ 1 (count-lines (point-min) (point))))
      )
    (if (< (- (- end-num bgn-num) (window-height)) 0)
        (goto-char (point-max))
      ad-do-it) ))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ shell                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ package manager                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ theme                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ================================================================
;; @ doc-view
;; ================================================================

;; ================================================================
;; @ eww
;; ================================================================
;; ================================================================
;; @ my-config
;; ================================================================

;;---------------------------------------------------------------------------

(define-key global-map (kbd "\C-h") 'delete-backward-char) ;;c-h BackSpac
(define-key key-translation-map (kbd "\C-h") (kbd "<DEL>"))
(define-key global-map (kbd  "\C-x h") 'describe-key) ;;C-x h key help
(define-key global-map (kbd "\C-x C-k") 'kill-buffer)
(define-key global-map (kbd "\C-k") 'kill-whole-line)
(define-key global-map (kbd "\C-c C-k") 'kill-line)
(define-key global-map (kbd "M-m") 'move-beginning-of-line)
(define-key global-map (kbd "\C-x C-a") 'move-beginning-of-line)
(define-key global-map (kbd "\C-a") 'back-to-indentation)
;; (define-key global-map (kbd "\C-o") 'backward-kill-word) ;;前一単語削除
;; (define-key global-map (kbd "M-C-x") 'edebug-defun)
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)
(define-key global-map (kbd "\C-c C-l") 'recenter)
(define-key global-map (kbd "C-l") 'recenter-top-bottom)
(define-key global-map (kbd "M-g") 'goto-line) ;;\M-g go-to-line
(define-key global-map (kbd "\C-x C-b") 'anything)
(define-key global-map (kbd "\C-x C-M-x") 'edebug-defun)

(require 'generic-x)

(menu-bar-mode 0)
(tool-bar-mode -1) ;;tool-bar-mode
(fset 'yes-or-no-p 'y-or-n-p)

(electric-indent-mode 1)


;;paren
(show-paren-mode 1)
(setq show-paren-style 'mixed)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match-face "blue")

;;region
(set-face-background 'region "#555")

;;line
;;   (setq kill-whole-line t)
(setq require-finel-newline t)
(setq next-line-add-newlines nil)


;;dired----------------------------------------------------------------------

(setq dired-dwin-target t)
(defun dired-mode-hooks()
  (local-set-key "q" '(lambda()
			"diredを閉じる"
			(interactive)
			(kill-buffer (current-buffer)))))
(add-hook 'dired-mode-hook 'dired-mode-hooks)

;;---------------------------------------------------------------------------

;;auto-complete--------------------------------------------------------------
(require 'auto-complete)
(require 'auto-complete-config)
;;グローバルでauto-completeを利用
(global-auto-complete-mode t)
;;キーマップ変更
;;(define-key ac-completing-map (kbd "M-n") 'ac-next)     ;M-nで次候補選択
;;(define-key ac-completing-map (kbd "M-p") 'ac-previous) ;M-pで前候補選択
;; (setq ac-auto-start nil) ;手動で変更
(setq ac-delay 0.5)
(setq ac-auto-start 4);4文字目から補完開始
(ac-set-trigger-key "TAB") ;TABで補完開始
;;(add-hock 'Emacs-Lisp (lambda()(add-to-list 'ac-sources 'ac-source-symbols)))

;;---------------------------------------------------------------------------

;;scroll---------------------------------------------------------------------

;;(scroll-bar-mode -1) ;スクロールバーを無効
(setq-default truncate-partial-width-windows nil)

;;---------------------------------------------------------------------------

;;my-function----------------------------------------------------------------

(defun move-to-mark()
  (interactive)
  (let ((pos (point)))
    (goto-char (mark))
    (push-mark pos)))

(defun my-quit-buffer()
  (interactive)
  (kill-buffer (current-buffer))
  (and (not (one-window-p)) (delete-window))
  (abort-recursive-edit))
(define-key completion-list-mode-map "q" 'my-quit-buffer)

(defun all-indent()
  (interactive)
  (indent-region(point-min)(point-max)))

;;window-split
(defun other-window-or-split()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)

(require 'my-comment-out)
(define-key global-map (kbd "C-;") 'my-comment-out)
(define-key global-map (kbd "C-x ;") 'my-comment-out)

(defun backward-kill-word-or-kill-region ()
  (interactive)
  (if (or (not transient-mark-mode) (region-active-p))
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))
(global-set-key "\C-w" 'backward-kill-word-or-kill-region)
(define-key minibuffer-local-completion-map "\C-w" 'backward-kill-word)

(require 'quick-japanese)
(define-key global-map (kbd "C-o") 'quick-japanese)
;;---------------------------------------------------------------------------

;;popwin---------------------------------------------------------------------

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;;---------------------------------------------------------------------------

;;eshell----------------------------------------------------------------------

(global-set-key [f8] 'eshell) ;f8 eshell
;;(add-hook 'after-init-hook (lambda() (eshell))) ;startup eshell
(setq eshell-ask-to-save-history 'always)
(setq eshell-cmpl-ignore-case t)
(setq eshell-cmpl-cycle-completions t)
(setq eshell-cmpl-cycle-cutoff-length 5)
(setq eshell-hist-ignoredups t)
(setq eshell-history-size 1000)
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
	(list "ll" "ls -l"))))
(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; (eshell-send-input)
    ))
;;---------------------------------------------------------------------------


;;mejor-mode---------------------------------------------------------------------
;; (require 'javacc-mode)
;; (setq auto-mode-alist
      ;; (append '(("\\.jj$" . javacc-mode)) auto-mode-alist))

;; (require 'clojure-mode)
;; (setq auto-mode-alist
      ;; (append '(("\\.clj$" . clojure-mode)) auto-mode-alist))

;; (require 'android-mode)
;; (setq android-mode-sdk-dir "/home/chayka/Android/Sdk/")

;; (require 'php-mode)
;; (setq auto-mode-alist
      ;; (append '(("\\.php" . php-mode)) auto-mode-alist))
;;------------------------------------------------------------------------------

;;anything----------------------------------------------------------------------
(require 'anything-config)
(require 'anything-match-plugin)
(require 'anything-startup nil t)
;; (require 'anything-migemo)
;;------------------------------------------------------------------------------

;;cmigemo-----------------------------------------------------------------------
;; (require 'migemo)

;; (defvar migemo-command nil)
;; (defvar migemo-options nil)
;; (defvar migemo-dictionary nil)
;; (defvar migemo-user-dictionary nil)
;; (defvar migemo-regex-dictionary nil)
;; (defvar migemo-coding-system nil)
;; (setq migemo-coding-system 'utf-8-unix)

;; (when (locate-library "migemo")
;;   (setq migemo-command "/usr/bin/cmigemo") ; HERE cmigemoバイナリ
;;   (setq migemo-options '("-q" "--emacs"))
;;   (setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict") ; HERE Migemo辞書
;;   (setq migemo-user-dictionary nil)
;;   (setq migemo-regex-dictionary nil)
;;   (setq migemo-coding-system 'utf-8-unix)
;;   (load-library "migemo")
;;   (migemo-init))
;;------------------------------------------------------------------------------

;;org-mode----------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-c l" 'org-store-link)
(global-set-key "\C-c c" 'org-capture)
(global-set-key "\C-c a" 'org-agenda)
(global-set-key "\C-c b" 'org-iswitchb)
(setq org-capture-templates
      '(("t" "TODO" entry(file+headline "~/org/gtd.org" "Tasks")
	 "* TODO %?\n %i\n %a")
	("j" "Journal" entry(file+datatree "~/org/journal.org")
	 "* %?\nEntred on %U\n %i\n %a")))
;;------------------------------------------------------------------------------

;;eldoc-------------------------------------------------------------------------
(require 'eldoc)
(require 'eldoc-extension)
(setq eldoc-idle-delay 0.05)
(setq eldoc-echo-area-use-multiline-p t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
;; (add-hook 'python-mode-hook 'turn-on-eldoc-mode)

;;------------------------------------------------------------------------------

