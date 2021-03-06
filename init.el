;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ------------------------------------------------------------------------

;; 文字コード
(cond
 ((eq system-type 'gnu/linux)
  (prefer-coding-system 'utf-8)
  (setq coding-system-for-read 'utf-8)
  (setq coding-system-for-write 'utf-8))
 ((eq system-type 'darwin)
  (set-default-coding-systems 'utf-8-unix)
  (set-terminal-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)))

;; load environment value
(load-file (expand-file-name "~/.emacs.d/shellenv.el"))
(dolist (path (reverse (split-string (getenv "PATH") ":")))
  (add-to-list 'exec-path path))

;; common lisp
(eval-when-compile (require 'cl))
;; (require 'cl)

;; @ load-path
(setq default-directory "~/")
(setq command-line-default-directory "~/")
;; load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; load-pathに追加するフォルダ
;; 2つ以上フォルダを指定する場合の引数 => (add-to-load-path "elisp" "xxx" "xxx")
(add-to-load-path "elisp" "elpa" "conf" "public_repos")

;;; --- バックアップとオートセーブ ---
;; バックアップファイルとオートセーブファイルを ~/.emacs.d/backups/ へ集める
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

(when (fboundp 'mac-input-source)
  (defun my-mac-selected-keyboard-input-source-chage-function ()
    "英語のときはカーソルの色を黄色に、日本語のときは赤にします."
    (let ((mac-input-source (mac-input-source)))
      (set-cursor-color
       (if (string-match "\\.US$" mac-input-source)
           "yellow" "Red"))))
  (add-hook 'mac-selected-keyboard-input-source-change-hook
            'my-mac-selected-keyboard-input-source-chage-function))

(require 'package)

;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Marmaladeを追加
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/"))

;; 初期化
(package-initialize)

;; auto-install
(require 'auto-install)
(auto-install-compatibility-setup)

;; ミニバッファに表示し, かつ, オーバレイする.
;; (setq ruby-block-highlight-toggle t)

(require 'linum)
(global-linum-mode)

;;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)

;;; splash screenを無効にする
(setq inhibit-splash-screen t)

;;; 同じ内容を履歴に記録しないようにする
(setq history-delete-duplicates t)

;; C-u C-SPC C-SPC ...でどんどん過去のマークを遡る
(setq set-mark-command-repeat-pop t)

;;; 複数のディレクトリで同じファイル名のファイルを開いたときのバッファ名を調整する
(require 'uniquify)
;; filename<dir> 形式のバッファ名にする
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;; ファイルを開いた位置を保存する
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))


;;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode 1)

;;; モードラインに時刻を表示する
(display-time)

;;; GCを減らして軽くする
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;;; ログの記録行数を増やす
(setq message-log-max 10000)

;;; 履歴をたくさん保存する
(setq history-length 1000)

;;; メニューバーとツールバーとスクロールバーを消す
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)



(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))

;; Set your installed path
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")

(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8)
(load-library "migemo")
(migemo-init)



;; smart-compile
(require 'smart-compile)
(setq smart-compile-alist
      (append
       '(("\\.rb$" . "ruby %f"))
       smart-compile-alist))
(global-set-key (kbd "C-x f") 'dired-toggle)
(global-set-key (kbd "C-x c") 'smart-compile)
(global-set-key (kbd "C-x C-x") (kbd "C-x c C-m"))
(setq compilation-window-height 15) ;; default window height is 15

; ======================================================================
;           magit
; ======================================================================
;(require 'magit)

; =====================================================================
;           git-gutter
;======================================================================
(require 'git-gutter)
(add-hook 'enh-ruby-mode 'git-gutter-mode)
(add-hook 'web-mode 'git-gutter-mode)


;; If you enable global minor mode
(global-git-gutter-mode t)

;; If you would like to use git-gutter.el and linum-mode
(git-gutter:linum-setup)

;; If you enable git-gutter-mode for some modes
(add-hook 'enh-ruby-mode-hook 'git-gutter-mode)

(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)

;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

;; anzu
(global-anzu-mode +1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-search-threshold 1000)
 '(custom-safe-themes
   (quote
    ("bdaab014ec6785f64b72efbea80808b762d8971247aacf2ffc6b76a39b9ed97c" default)))
 '(rspec-use-rake-when-possible nil)
 '(tab-width 2))

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)    ; 必須ではないですが一応
(global-auto-complete-mode t)
(define-key ac-completing-map (kbd "M-n") 'ac-next)      ; M-nで次候補選択
(define-key ac-completing-map (kbd "M-p") 'ac-previous)  ; C-p で前候補選択
(ac-config-default)
(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'web-mode)
(add-to-list 'ac-modes 'inferior-scheme3-mode)
(add-to-list 'ac-modes 'gauche-mode)
(add-to-list 'ac-modes 'kahua-mode)

;;; ac-slime
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

;;; popwin -----------------------
(require 'popwin)
(popwin-mode 1)
;; M-x anything
(defvar helm-samewindow nil)
(push '("*anything*" :height 20) popwin:special-display-config)

;; M-x dired-jump-other-window
(push '(dired-mode :position top) popwin:special-display-config)

;; M-!
(push "*Shell Command Output*" popwin:special-display-config)

;; M-x compile
(push '(compilation-mode :noselect t) popwin:special-display-config)

;; slime
(push "*slime-apropos*" popwin:special-display-config)
(push "*slime-macroexpansion*" popwin:special-display-config)
(push "*slime-description*" popwin:special-display-config)
(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
(push "*slime-xref*" popwin:special-display-config)
(push '(sldb-mode :stick t) popwin:special-display-config)
(push 'slime-repl-mode popwin:special-display-config)
(push 'slime-connection-list-mode popwin:special-display-config)

;; undo-tree
(push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)
;;; ------------------------------

;; helm
(require 'helm-config)
(helm-mode 1)
;; ミニバッファでC-hをバックスペースに割り当て
(define-key helm-read-file-map (kbd "C-h") 'delete-backward-char)
;; C-iで補完
(define-key helm-read-file-map (kbd "C-i") 'helm-execute-persistent-action)
(when (require 'popwin)
  (setq helm-samewindow nil)
  (defvar display-buffer-function 'popwin:display-buffer)
  (setq popwin:special-display-config '(("*compilatoin*" :noselect t)
                                        ("helm" :regexp t :height 0.4)
                                        )))

(require 'helm-migemo)
;;; この修正が必要
(eval-after-load "helm-migemo"
  '(defun helm-compile-source--candidates-in-buffer (source)
     (helm-aif (assoc 'candidates-in-buffer source)
         (append source
                 `((candidates
                    . ,(or (cdr it)
                           (lambda ()
                             ;; Do not use `source' because other plugins
                             ;; (such as helm-migemo) may change it
                             (helm-candidates-in-buffer (helm-get-current-source)))))
                   (volatile) (match identity)))
       source)))


(require 'helm-swoop)
;;; isearchからの連携を考えるとC-r/C-sにも割り当て推奨
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)

;;; 検索結果をcycle
(setq helm-swoop-move-to-line-cycle t)

(cl-defun helm-swoop-nomigemo (&key $query ($multiline current-prefix-arg))
  "シンボル検索用Migemo無効版helm-swoop"
  (interactive)
  (let ((helm-swoop-pre-input-function
         (lambda () (format "\\_<%s\\_> " (thing-at-point 'symbol)))))
    (helm-swoop :$source (delete '(migemo) (copy-sequence (helm-c-source-swoop)))
                :$query $query :$multiline $multiline)))
;;; C-M-:に割り当て
(global-set-key (kbd "C-M-:") 'helm-swoop-nomigemo)

;;; [2014-11-25 Tue]
(when (featurep 'helm-anything)
  (defadvice helm-resume (around helm-swoop-resume activate)
    "helm-anything-resumeで復元できないのでその場合に限定して無効化"
    ad-do-it))

;;; ace-isearch
(global-ace-isearch-mode 1)

;;; [2015-03-23 Mon]C-u C-s / C-u C-u C-s
(defun isearch-forward-or-helm-swoop (use-helm-swoop)
  (interactive "p")
  (let (current-prefix-arg
        (helm-swoop-pre-input-function 'ignore))
    (call-interactively
     (case use-helm-swoop
       (1 'isearch-forward)
       (4 'helm-swoop)
       (16 'helm-swoop-nomigemo)))))
(global-set-key (kbd "C-s") 'isearch-forward-or-helm-swoop)


;; dired
;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
(setq dired-dwim-target t)
;; ディレクトリを再帰的にコピーする
(setq dired-recursive-copies 'always)
;; diredバッファでC-sした時にファイル名だけにマッチするように
;; (defvar dired-isearch-filenames t)

(require 'dired-subtree)
;;; iを置き換え
(define-key dired-mode-map (kbd "i") 'dired-subtree-insert)
;;; org-modeのようにTABで折り畳む
(define-key dired-mode-map (kbd "<tab>") 'dired-subtree-remove)
;;; C-x n nでsubtreeにナローイング
(define-key dired-mode-map (kbd "C-x n n") 'dired-subtree-narrow)

;;; ファイル名以外の情報を(と)で隠したり表示したり
(require 'dired-details)
(dired-details-install)
(setq dired-details-hidden-string "")
(setq dired-details-hide-link-targets nil)
(setq dired-details-initially-hide nil)

;;; dired-subtreeをdired-detailsに対応させる
(defun dired-subtree-after-insert-hook--dired-details ()
  (dired-details-delete-overlays)
  (dired-details-activate))
(add-hook 'dired-subtree-after-insert-hook
          'dired-subtree-after-insert-hook--dired-details)

;; find-dired対応
(defadvice find-dired-sentinel (after dired-details (proc state) activate)
  (ignore-errors
    (with-current-buffer (process-buffer proc)
      (dired-details-activate))))
;; (progn (ad-disable-advice 'find-dired-sentinel 'after 'dired-details) (ad-update 'find-dired-sentinel))

;;; [2014-12-30 Tue]^をdired-subtreeに対応させる
(defun dired-subtree-up-dwim (&optional arg)
  "subtreeの親ディレクトリに移動。そうでなければ親ディレクトリを開く(^の挙動)。"
  (interactive "p")
  (or (dired-subtree-up arg)
      (dired-up-directory)))
(define-key dired-mode-map (kbd "^") 'dired-subtree-up-dwim)

; ======================================================================
;       open-junk-file
; ======================================================================
(require 'open-junk-file)
(setq open-junk-file-format "~/Documents/junk/%Y-%m%d-%H%M%S.")
(global-set-key "\C-xj" 'open-junk-file)

; ======================================================================
;       org
; ======================================================================

(require 'org)
(require 'ob-scheme)
(require 'ob-ruby)

(setq org-directory "~/Documents/junk")
(setq org-agenda-files (list org-directory))

(setq org-src-fontify-natively t)

(defun my-org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "scheme")
           (string= lang "emacs-lisp")
           (string= lang "ruby")
           (string= lang "C")
           (string= lang "cpp")
           )))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
(global-set-key (kbd "C-c l")
                'org-store-link)

;; ------------------------------------------------------------------------
;; @ general

;;フォントをRictyにする
(set-face-font 'default "Ricty-13:nil")

;; スタートアップ非表示
(setq inhibit-startup-screen t)

;; 括弧の自動補完
(require 'smartparens-config)
(smartparens-global-mode t)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; ツールバー非表示
(tool-bar-mode -1)

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format "%f")

;; 行番号表示
(global-linum-mode t)
(set-face-attribute 'linum nil
                    :foreground "#800"
                    :height 0.9)

;; 行番号フォーマット
(setq linum-format "%4d")

(load-theme 'gotham t)

;; ;; 括弧の範囲内を強調表示
(show-paren-mode t)
(defvar show-paren-delay 0)
;; (setq show-paren-style 'expression)
;; (set-face-background 'show-paren-match-face "gray05")
;; (set-face-foreground 'show-paren-match-face "medium violet red")
;; (set-face-bold 'show-paren-match-face t)

;; ;; 括弧の範囲色
;;(set-face-background 'show-paren-match-face "red")

;; 対応する括弧を表示する
(show-paren-mode t)

;; 選択領域の色
(set-face-background 'region "#555555")

;; 行末の空白を強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")


;; タブ幅


;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)
; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; 最近使ったファイルをメニューに表示
;; (recentf-mode t)

;; 最近使ったファイルの表示数
;; (setq recentf-max-menu-items 10)

;; 最近開いたファイルの保存数を増やす
(setq recentf-max-saved-items 3000)

;; ミニバッファの履歴を保存する
(savehist-mode 1)

;; ミニバッファの履歴の保存数を増やす
(setq history-length 3000)

;; バックアップを残さない
(setq make-backup-files nil)

;; 行間
(setq-default line-spacing 0)

;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
     scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

;; フレームの透明度
(set-frame-parameter (selected-frame) 'alpha '(0.90))


;; モードラインに行番号表示
(line-number-mode t)

;; モードラインに列番号表示
(column-number-mode t)

;; C-Ret で矩形選択
;; 詳しいキーバインド操作：http://dev.ariel-networks.com/articles/emacs/part5/
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; emacs-lisp-mode-hook 用の関数を定義
(defun elisp-mode-hooks ()
  "Lisp-mode-hooks"
  (when (require 'eldoc nil t)
  (setq eldoc-idle-delay 0.2)
  (setq eldoc-echo-area-use-multiline-p t)
  (turn-on-eldoc-mode)))

;; emacs-lisp-mode のフックをセットアップ
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)

;; ピープ音を鳴らさない
(setq visible-bell t)
(setq ring-bell-function 'ignore)



;; ------------------------------------------------------------------------
;; @ modeline

;; モードラインの割合表示を総行数表示
(defvar my-lines-page-mode t)
(defvar my-mode-line-format)

(when my-lines-page-mode
  (setq my-mode-line-format "%d")
  (if size-indication-mode
      (setq my-mode-line-format (concat my-mode-line-format " of %%I")))
  (cond ((and (eq line-number-mode t) (eq column-number-mode t))
         (setq my-mode-line-format (concat my-mode-line-format " (%%l,%%c)")))
        ((eq line-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format " L%%l")))
        ((eq column-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format " C%%c"))))

  (setq mode-line-position
        '(:eval (format my-mode-line-format
                        (count-lines (point-max) (point-min))))))


;; スクリーンの最大化
(set-frame-parameter nil 'fullscreen 'maximized)

;; C-k で行全体を削除（改行を含む）
(setq kill-whole-line t)

;; C-h を バックスペースへ
(keyboard-translate ?\C-h ?\C-?)

;; C-x ? を help へ
(global-set-key "\C-x?" 'help-command)

;; インタラクティブにウィンドウを分割する
(setq split-height-threshold nil)
(setq split-width-threshold 100)

;; C-tab でウィンドウを切り替える。
(global-set-key "\C-t" 'other-window)

;; C-j で改行とインデント
(global-set-key "\C-j" 'newline-and-indent)

;; M-f,M-bの挙動を変更
(require 'misc)
(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "M-b") 'backward-to-word)

;; C-a で行の先頭に。もう一度 C-aで文字の始まる位置に移動
(defun my-goto-line-beginning-or-indent (&optional $position)
  (interactive)
  (or $position (setq $position (point)))
  (let (($starting-position (progn (back-to-indentation) (point))))
    (if (eq $starting-position $position)
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'my-goto-line-beginning-or-indent)

; カーソル位置のフェイス情報を表示
(defun my-get-face (&optional $point)
  (interactive)
  (or $point (setq $point (point)))
  (let (($face (or (get-char-property $point 'read-face-name)
                   (get-char-property $point 'face))))
    (if $face
        (message (format "%s" $face))
      (message "no face"))))

;; indent-guide
(require 'indent-guide)
(indent-guide-global-mode)
(setq indent-guide-recursive t)



;; emacsでGauche
; --------------------------------------------------------------------
(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
(defvar scheme-program-name "gosh -i")
(add-to-list 'auto-mode-alist '("\\.scm" . gauche-mode))
;; (setq auto-mode-alist
;;      (cons '("\.\(scm\)$" . gauche-mode) auto-mode-alist))
(autoload 'gauche-mode "gauche-mode" "Major mode for Scheme." t)
(autoload 'run-scheme "gauche-mode" "Run an inferior Scheme process." t)
;; (autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
;; (autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

(defun scheme-other-window ()
  "Run Gauche on other window"
  (interactive)
  (split-window-horizontally (/ (frame-width) 2))
  (let ((buf-name (buffer-name (current-buffer))))
    (gauche-mode)
    (switch-to-buffer-other-window
     (get-buffer-create "*scheme*"))
    (run-scheme scheme-program-name)
    (switch-to-buffer-other-window
     (get-buffer-create buf-name))))

(define-key global-map
  "\C-cg" 'scheme-other-window)

(put 'and-let* 'scheme-indent-function 1)
(put 'begin0 'scheme-indent-function 0)
(put 'call-with-client-socket 'scheme-indent-function 1)
(put 'call-with-input-conversion 'scheme-indent-function 1)
(put 'call-with-input-file 'scheme-indent-function 1)
(put 'call-with-input-process 'scheme-indent-function 1)
(put 'call-with-input-string 'scheme-indent-function 1)
(put 'call-with-iterator 'scheme-indent-function 1)
(put 'call-with-output-conversion 'scheme-indent-function 1)
(put 'call-with-output-file 'scheme-indent-function 1)
(put 'call-with-output-string 'scheme-indent-function 0)
(put 'call-with-temporary-file 'scheme-indent-function 1)
(put 'call-with-values 'scheme-indent-function 1)
(put 'dolist 'scheme-indent-function 1)
(put 'dotimes 'scheme-indent-function 1)
(put 'if-match 'scheme-indent-function 2)
(put 'let*-values 'scheme-indent-function 1)
(put 'let-args 'scheme-indent-function 2)
(put 'let-keywords* 'scheme-indent-function 2)
(put 'let-match 'scheme-indent-function 2)
(put 'let-optionals* 'scheme-indent-function 2)
(put 'let-syntax 'scheme-indent-function 1)
(put 'let-values 'scheme-indent-function 1)
(put 'let/cc 'scheme-indent-function 1)
(put 'let1 'scheme-indent-function 2)
(put 'letrec-syntax 'scheme-indent-function 1)
(put 'make 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)
(put 'match-lambda 'scheme-indent-function 1)
(put 'match-let 'scheme-indent-fucntion 1)
(put 'match-let* 'scheme-indent-fucntion 1)
(put 'match-letrec 'scheme-indent-fucntion 1)
(put 'match-let1 'scheme-indent-function 2)
(put 'match-define 'scheme-indent-fucntion 1)
(put 'multiple-value-bind 'scheme-indent-function 2)
(put 'parameterize 'scheme-indent-function 1)
(put 'parse-options 'scheme-indent-function 1)
(put 'receive 'scheme-indent-function 2)
(put 'rxmatch-case 'scheme-indent-function 1)
(put 'rxmatch-cond 'scheme-indent-function 0)
(put 'rxmatch-if  'scheme-indent-function 2)
(put 'rxmatch-let 'scheme-indent-function 2)
(put 'syntax-rules 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'until 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'while 'scheme-indent-function 1)
(put 'with-builder 'scheme-indent-function 1)
(put 'with-error-handler 'scheme-indent-function 0)
(put 'with-error-to-port 'scheme-indent-function 1)
(put 'with-input-conversion 'scheme-indent-function 1)
(put 'with-input-from-port 'scheme-indent-function 1)
(put 'with-input-from-process 'scheme-indent-function 1)
(put 'with-input-from-string 'scheme-indent-function 1)
(put 'with-iterator 'scheme-indent-function 1)
(put 'with-module 'scheme-indent-function 1)
(put 'with-output-conversion 'scheme-indent-function 1)
(put 'with-output-to-port 'scheme-indent-function 1)
(put 'with-output-to-process 'scheme-indent-function 1)
(put 'with-output-to-string 'scheme-indent-function 1)
(put 'with-port-locking 'scheme-indent-function 1)
(put 'with-string-io 'scheme-indent-function 1)
(put 'with-time-counter 'scheme-indent-function 1)
(put 'with-signal-handlers 'scheme-indent-function 1)
(put 'with-locking-mutex 'scheme-indent-funcion 1)
(put 'guard 'scheme-indent-function 1)

; --------------------------------------------------------------------



(setq ruby-insert-encoding-magic-comment nil)





;; (load-theme 'warm-night t)

;; (when (require 'color-theme nil t)
;; (color-theme-initialize)
;; (color-theme-hober))

; ================================================================
;                          Ruby
; ================================================================
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$latex " . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

(setq ruby-insert-encoding-magic-comment nil)

;; ;; ruby-electric
;; (require 'ruby-electric)
;; (eval-after-load "enh-ruby-mode"
;;       '(add-hook 'enh-ruby-mode-hook 'ruby-electric-mode))

;; 保存時にmagic commentを追加しないようにする
(defadvice enh-ruby-mode-set-encoding (around stop-enh-ruby-mode-set-encoding)
  "If enh-ruby-not-insert-magic-comment is true, stops enh-ruby-mode-set-encoding."
  (if (and (boundp 'enh-ruby-not-insert-magic-comment)
           (not enh-ruby-not-insert-magic-comment))
      ad-do-it))
(ad-activate 'enh-ruby-mode-set-encoding)
(setq-default enh-ruby-not-insert-magic-comment t)

;; ruby-block
(require 'ruby-block)
(ruby-block-mode t)

(require 'rcodetools)

(ad-disable-advice 'comment-dwim 'around 'rct-hack)
(ad-update 'comment-dwim)
(defadvice comment-dwim (around rct-hack activate)
  "If comment-dwim is successively called, add => mark."
  (if (and (member major-mode '(ruby-mode enh-ruby-mode))
           (eq last-command 'comment-dwim))
      (insert "=>")
    ad-do-it))



;; xmpfilter
(eval-after-load 'enh-ruby-mode
  '(progn
     (define-key enh-ruby-mode-map (kbd "C-c C-d") 'xmp)
     (define-key enh-ruby-mode-map (kbd "M-C-i") 'rct-complete-symbol)))


(defun make-ruby-scratch-buffer ()
  (with-current-buffer (get-buffer-create "*ruby scratch*")
    (ruby-mode)
    (current-buffer)))
(defun ruby-scratch ()
  (interactive)
  (pop-to-buffer (make-ruby-scratch-buffer)))


;;======================================================================
;                      Ruby on Rails
;;======================================================================

;;(require 'projectile)
;;(projectile-global-mode)

;;(require 'projectile-rails)
;;(add-hook 'projectile-mode-hook 'projectile-rails-on)


;; rirariと同様のキーバインドを使う
;;(define-key projectile-rails-mode-map (kbd "C-c ; f m") 'projectile-rails-find-current-model)
;;(define-key projectile-rails-mode-map (kbd "C-c ; f c") 'projectile-rails-find-current-controller)
;;(define-key projectile-rails-mode-map (kbd "C-c ; f v") 'projectile-rails-find-current-view)
;;(define-key projectile-rails-mode-map (kbd "C-c ; f s") 'projectile-rails-find-current-spec)
;;(define-key projectile-rails-mode-map (kbd "C-c ; c") 'projectile-rails-console)

;; evilの`gf`で`projectile-rails-goto-file-at-point`を使うように
;(evil-define-key 'normal projectile-rails-mode-map (kbd "gf")
;  'projectile-rails-goto-file-at-point)
;; fix above keybind can't be applied til state changes
;; https://bitbucket.org/lyro/evil/issue/301/evil-define-key-for-minor-mode-does-not
;;(add-hook 'find-file-hook
;;          '(lambda ()
;;              (when projectile-rails-mode
;;                  (evil-normalize-keymaps))))

;; `app/views/application`と`app/views/shared`のビューも探す候補に入れる
;;(defun projectile-rails-goto-template-at-point ()
;;  (interactive)
;;  (let* ((template (projectile-rails-filename-at-point))
;;         (dir (projectile-rails-template-dir template))
;;         (name (projectile-rails-template-name template))
;;         (format (projectile-rails-template-format template)))
;;    (if format
;;        (loop for processor in '("erb" "haml" "slim")
;;              for template = (s-lex-format "${dir}${name}.${format}.${processor}")
;;              for partial = (s-lex-format "${dir}_${name}.${format}.${processor}")
;;              for partial-2 = (expand-file-name
;;                               (s-lex-format "_${name}.${format}.${processor}")
;;                               (projectile-expand-root "app/views/application"))
;;              for partial-3 = (expand-file-name
;;                               (s-lex-format "_${name}.${format}.${processor}")
;;                               (projectile-expand-root "app/views/shared"))
;;              until (or
;;                     (projectile-rails-ff template)
;;                     (projectile-rails-ff partial)
;;                     (projectile-rails-ff partial-2)
;;                     (projectile-rails-ff partial-3)))
;;      (message "Could not recognize the template's format")
;;      (dired dir))))


; RSPEC
(require 'rspec-mode)
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(web-mode-comment-face ((t (:foreground "#D9333F"))))
 ;; '(web-mode-css-at-rule-face ((t (:foreground "#FF7F00"))))
 ;; '(web-mode-css-pseudo-class-face ((t (:foreground "#FF7F00"))))
 ;; '(web-mode-css-rule-face ((t (:foreground "#A0D8EF"))))
 ;; '(web-mode-doctype-face ((t (:foreground "#82AE46"))))
 ;; '(web-mode-html-attr-name-face ((t (:foreground "#C97586"))))
 ;; '(web-mode-html-attr-value-face ((t (:foreground "#82AE46"))))
 ;; '(web-mode-html-tag-face ((t (:foreground "#E6B422" :weight bold))))
 ;; '(web-mode-server-comment-face ((t (:foreground "#D9333F"))))
 )


;;==========================================================
;;         web-modeの設定
;;==========================================================
(require 'web-mode)

(defun web-mode-hook ()
  "Hooks for Web mode."
  ;; 変更日時の自動修正
  (setq time-stamp-line-limit -200)
  (if (not (memq 'time-stamp write-file-hooks))
      (setq write-file-hooks
            (cons 'time-stamp write-file-hooks)))
  (setq time-stamp-format " %3a %3b %02d %02H:%02M:%02S %:y %Z")
;;  (setq time-stamp-start "Last modified: 木曜日 1月 29 20:49:10 2015 JST
  (setq time-stamp-end "$")
  ;; web-modeの設定
  (setq web-mode-markup-indent-offset 2) ;; html indent
  (setq web-mode-css-indent-offset 2)    ;; css indent
  (setq web-mode-code-indent-offset 2)   ;; script indent(js,php,etc..)
  ;; htmlの内容をインデント
  ;; TEXTAREA等の中身をインデントすると副作用が起こったりするので
  ;; デフォルトではインデントしない
  ;;(setq web-mode-indent-style 2)
  ;; コメントのスタイル
  ;;   1:htmlのコメントスタイル(default)
  ;;   2:テンプレートエンジンのコメントスタイル
  ;;      (Ex. {# django comment #},{* smarty comment *},{{-- blade comment --}})
  (setq web-mode-comment-style 2)
  ;; 終了タグの自動補完をしない
  ;;(setq web-mode-disable-auto-pairing t)
  ;; color:#ff0000;等とした場合に指定した色をbgに表示しない
  ;;(setq web-mode-disable-css-colorization t)
  ;;css,js,php,etc..の範囲をbg色で表示
  ;; (setq web-mode-enable-block-faces t)
  ;; (custom-set-faces
  ;;  '(web-mode-server-face
  ;;    ((t (:background "grey"))))                  ; template Blockの背景色
  ;;  '(web-mode-css-face
  ;;    ((t (:background "grey18"))))                ; CSS Blockの背景色
  ;;  '(web-mode-javascript-face
  ;;    ((t (:background "grey36"))))                ; javascript Blockの背景色
  ;;  )
  ;;(setq web-mode-enable-heredoc-fontification t)
  )

(add-hook 'web-mode-hook  'web-mode-hook)

;; 色の設定

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(setq web-mode-engines-alist
'(("php"    . "\\.phtml\\'")
  ("blade"  . "\\.blade\\.")))

; ======================================================================


;; js2-mode
(require 'js2-mode)
(autoload 'js2-mode "js" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; minimap-mode
(require 'minimap)

;;; カスタマイズできる項目！
;;; これらはload-themeの前に配置すること
;; fringeを背景から目立たせる
;; (setq solarized-distinct-fringe-background t)
;;
;; mode-lineを目立たせる(Fig3)
;; (setq solarized-high-contrast-mode-line t)
;;
;; bold度を減らす
;; (setq solarized-use-less-bold t)
;;
;; italicを増やす
;; (setq solarized-use-more-italic t)
;;
;; インジケータの色を減らす (git-gutter, flycheckなど)
;; (setq solarized-emphasize-indicators nil)
;;
;; orgの見出し行の文字の大きさを変えない
;; (setq solarized-scale-org-headlines nil)
;;
;; フォントサイズを変更しない
;; (setq solarized-height-minus-1 1)
;; (setq solarized-height-plus-1 1)
;; (setq solarized-height-plus-2 1)
;; (setq solarized-height-plus-3 1)
;; (setq solarized-height-plus-4 1)

;; (load-theme 'solarized-light t)
;; (load-theme 'solarized-dark t)


;;emacs-lisp

;;式の評価結果を注釈するための設定
(require 'lispxmp)
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)

;;括弧の対応を保持して編集する設定
;; (require 'paredit)
;; (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook 'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook 'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook 'enable-paredit-mode)
(require 'auto-async-byte-compile)

;; 自動コンパイルを無効にするファイル名の正規表現
(setq auto-async-byte-compile-exclude-file-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idledeley 0.2)
(setq eldoc-minor-mode-string "")

;;find-functionをキー割り当てする
(find-function-setup-keys)

(require 'kahua)


;;; slime
(require 'slime)
;; (load (expand-file-name "~/.roswell/impls/ALL/ALL/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "ros -L sbcl -Q run")
(slime-setup '(slime-repl slime-fancy slime-banner slime-indentation))
(setq slime-net-coding-system 'utf-8-unix)
;; (setf slime-lisp-implementations
;;       `((sbcl    ("sbcl" "--dynamic-space-size" "2000"))
;;         (roswell ("ros" "dynamic-space-size=2000" "-Q" "-l" "~/.sbclrc" "run"))))
;; (setf slime-default-lisp 'roswell)

(require 'multi-term)

(setq multi-term-program shell-file-name)

(global-set-key (kbd "C-c n") 'multi-term-next)
(global-set-key (kbd "C-c p") 'multi-term-prev)

(add-hook 'term-mode-hook
      (lambda ()
        (define-key term-raw-map (kbd "C-t") 'other-window)
        (define-key term-raw-map (kbd "C-y") 'term-paste)
        (define-key term-raw-map (kbd "C-h") 'term-send-backspace)
            (define-key term-raw-map (kbd "M-d") 'term-send-forward-kill-word)
            (define-key term-raw-map (kbd "M-<backspace>") 'term-send-backward-kill-word)
            (define-key term-raw-map (kbd "M-DEL") 'term-send-backward-kill-word)
            (define-key term-raw-map (kbd "C-v") nil)
        (define-key term-raw-map (kbd "ESC ESC") 'term-send-raw)
        (define-key term-raw-map (kbd "C-q") 'toggle-term-view)))

(defun toggle-term-view () (interactive)
  (cond ((eq major-mode 'term-mode)
     (fundamental-mode)
     (view-mode-enable)
     (local-set-key (kbd "C-c C-c") 'toggle-term-view)
     (setq multi-term-cursor-point (point)))
    ((eq major-mode 'fundamental-mode)
     (view-mode-disable)
     (goto-char multi-term-cursor-point)
     (multi-term-internal))))


;;; キーボードマクロ
(fset 'endspace
   "\C-e  \C-n")
