;; cask and pallet
(when (or (require 'cask "~/.cask/cask.el" t)
	  (require 'cask nil t))
  (cask-initialize))

(require 'use-package)
(pallet-mode t)

;; coding system
(prefer-coding-system 'utf-8)

;;; 行番号の表示
(require 'linum)
(global-linum-mode)

;; ツールバーを非表示
(tool-bar-mode 0)

;; スクロール時のカーソル位置の維持
(setq scroll-preserve-screen-position t)

;;; スクロール行数（一行ごとのスクロール）
(setq vertical-centering-font-regexp ".*")
(setq scroll-conservatively 35)
(setq scroll-margin 0)
(setq scroll-step 1)

;;; 画面スクロール時の重複行数
(setq next-screen-context-lines 1)

;;; 起動メッセージの非表示
(setq inhibit-startup-message t)

;;; スタートアップ時のエコー領域メッセージの非表示
(setq inhibit-startup-echo-area-message -1)

;;; 変更ファイルのバックアップ
(setq make-backup-files nil)

;;; 変更ファイルの番号つきバックアップ
(setq version-control nil)

;;; 編集中ファイルのバックアップ
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)

;;; 編集中ファイルのバックアップ先
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; 編集中ファイルのバックアップ間隔（秒）
(setq auto-save-timeout 30)

;;; 編集中ファイルのバックアップ間隔（打鍵）
(setq auto-save-interval 500)

;;;バックアップ世代数
(setq kept-old-versions 1)
(setq kept-new-versions 2)

;;; 古いバックアップファイルの削除
(setq delete-old-versions t)

;; リージョンに上書き
(delete-selection-mode t)


;; fcitx setting
(fcitx-default-setup)
(setq fcitx-use-dbus t)

;; font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "源ノ角ゴシック Code JP" :foundry "adobe" :slant normal :weight normal :height 113 :width normal))))
 '(whitespace-tab ((t (:foreground "dark gray" :underline t :weight bold)))))


;; ido
(ido-mode t)
;; (ido-everywhere 1)
;; (require 'ido-ubiquitous)
;; (ido-ubiquitous-mode 1)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(setq ido-cannot-complete-command 'ido-next-match)

;; smooth-scroll
(require 'smooth-scroll)
(smooth-scroll-mode t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;;; one line at at time
(setq mouse-wheel-progressive-speed nil) ;;; dont accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;;; scroll window under mouse

;;;YaTexの設定
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode)  auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-latex-message-code 'utf-8)
(setq YaTeX-use-hilit19 nil)
(setq YaTex-use-font-lock nil)
;; Yatexの自動改行をなしにする．
(add-hook 'yatex-mode-hook
		  '(lambda () (auto-fill-mode -1)))
;;;(setq tex-command "latexmk -pvc")  ;;保存したら自動で再コンパイル
(setq tex-command "latexmk -f") ;; 強制コンパイル
(setq dvi2-command "evince")
(add-hook 'yatex-mode-hook
	  '(lambda ()
	     (define-key YaTeX-mode-map (kbd "C-c c") 'compile) ;;C-c c でmake できるようにする
	     (define-key YaTeX-mode-map (kbd "C-c r") 'recompile)
	     (auto-complete-mode t)
	     (setq compile-command "latexmk -f") 
	     ))


;; .latexmkrcファイルをperlモードで開く
(add-to-list 'auto-mode-alist '("/\\.latexmkrc\\'" . perl-mode))
;;; inverse search
(require 'dbus)
(defun un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))

(defun evince-inverse-search (file linecol &rest ignored)
  (let* ((fname (un-urlify file))
	 (buf (find-file fname))
	 (line (car linecol))
	 (col (cadr linecol)))
    (if (null buf)
	(message "[Synctex]: %s is not opened..." fname)
      (switch-to-buffer buf)
      (goto-line (car linecol))
      (unless (= col -1)
	(move-to-column col)))))

(dbus-register-signal
 :session nil "/org/gnome/evince/Window/0"
 "org.gnome.evince.Window" "SyncSource"
 'evince-inverse-search)

(add-hook 'yatex-mode-hook '(lambda () (reftex-mode t)))

;;; popwin
(require 'popwin)
(popwin-mode 1)
(setq display-buffer-function 'popwin:display-buffer)

;;コンパイル画面でスクロールする
(setq compilation-scroll-output t)

;; 指定したディレクトリでM-xをやる 
(defun in-directory (dir)
  "Runs execute-extended-command with default-directory set to the given directory."
  (interactive "DIn directory: ")
  (let ((default-directory dir))
	(call-interactively 'execute-extended-command)))
(global-set-key (kbd "M-X") 'in-directory)

;; roslaunch highlighting
(add-to-list 'auto-mode-alist '("\\.launch$" . nxml-mode))
(add-hook 'nxml-mode-hook
          (lambda ()
            (setq auto-fill-mode -1)
            ;; スラッシュの入力で終了タグを自動補完
            (setq nxml-slash-auto-complete-flag t)))

;; shellに色をつける
(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'compilation-filter-hook
          '(lambda ()
             (let ((start-marker (make-marker))
                   (end-marker (process-mark 
				(get-buffer-process (current-buffer)))))
               (set-marker start-marker (point-min))
               (ansi-color-apply-on-region start-marker end-marker))))

;; elscreen.el
;;プレフィクスキーはC-z
(setq elscreen-prefix-key (kbd "C-z"))
(elscreen-start)
;;; タブの先頭に[X]を表示しない
(setq elscreen-tab-display-kill-screen nil)
;;; header-lineの先頭に[<->]を表示しない
(setq elscreen-tab-display-control nil)
;;; バッファ名・モード名からタブに表示させる内容を決定する(デフォルト設定)
(setq elscreen-buffer-to-nickname-alist
      '(("^dired-mode$" .
         (lambda ()
           (format "Dired(%s)" dired-directory)))
        ("^Info-mode$" .
         (lambda ()
           (format "Info(%s)" (file-name-nondirectory Info-current-file))))
        ("^mew-draft-mode$" .
         (lambda ()
           (format "Mew(%s)" (buffer-name (current-buffer)))))
        ("^mew-" . "Mew")
        ("^irchat-" . "IRChat")
        ("^liece-" . "Liece")
        ("^lookup-" . "Lookup")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")
        ("-telnet" . "telnet")
        ("dict" . "OnlineDict")
        ("*WL:Message*" . "Wanderlust")))


;;; color-theme
;;(load-theme 'monokai t)
(load-theme 'spacemacs-dark t)
;;(set-frame-parameter nil 'background-mode 'dark)
;;(load-theme 'solarized t)
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/theme/")
;;(load-theme 'molokai t)
;; powerline theme
(require 'powerline)
(powerline-default-theme)
(require 'airline-themes)
(load-theme 'airline-powerlineish t)

;; 画面の下の方を綺麗にする
;; (require 'powerline)
;; (powerline-default-theme)
;; (set-face-attribute 'mode-line nil
;;                     :foreground "White"
;;                     :background "DarkCyan"
;;                     :box nil)


;; smartparent
(smartparens-global-mode)
;; [DEL]キーもしくは[C-h]に当てられているdelete-backward-charにadviceをかけられて削除するたびにフリーズする．これを無効化.
(ad-disable-advice 'delete-backward-char 'before 'sp-delete-pair-advice)
(ad-activate 'delete-backward-char)

;;対応かっこのハイライト
(show-paren-mode 1)
(setq show-paren-delay 0)

;; gnuplot-mode
(require 'gnuplot-mode)
(setq auto-mode-alist 
(append '(("\\.\\(gp\\|gnuplot\\|plt\\)$" . gnuplot-mode)) auto-mode-alist))

;; for ssh
(require 'tramp)
(setq tramp-default-method "scp")

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
'(("php"    . "\\.phtml\\'")
  ("blade"  . "\\.blade\\.")))
(setq web-mode-enable-current-element-highlight t)
(defun my-web-mode-hook () "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing nil)
  )
(add-hook 'web-mode-hook 'my-web-mode-hook)
(defun sp-web-mode-is-code-context (id action context)
  (and (eq action 'insert)
       (not (or (get-text-property (point) 'part-side)
                (get-text-property (point) 'block-side)))))

(sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; 画面の端に来たら反対側に移動する
(setq windmove-wrap-around t)

;; 別のwindowに移動するキーバインド
(global-set-key "\C-t" 'other-window)

;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      (append yas-snippet-dirs
	      '("~/.emacs.d/snippets")))
(eval-after-load "yasnippet"
  '(progn
     ;; companyと競合するのでyasnippetのフィールド移動は "C-i" のみにする
     (define-key yas-keymap (kbd "<tab>") nil)
     (yas-global-mode 1)))


;; =============
;; company mode
;; =============
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;; set default `company-backends'
(setq company-backends
      '((company-files          ; files & directory
         company-keywords       ; keywords
         company-capf
         company-yasnippet
	 company-dabbrev-code
         )))
(setq company-dabbrev-code-mode t)
(setq company-dabbrev-code-everywhere t)
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 1) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
(setq company-transformers '(company-sort-by-statistics company-sort-by-backend-importance))
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
(add-hook 'after-init-hook 'company-statistics-mode)

;; ;; Add yasnippet support for all company backends
;; ;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
	    '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))


;; =============
;; irony-mode
;; =============
(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'arduino-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook
	  ((lambda ()
	     (add-to-list (make-local-variable 'company-backends)
			  'company-irony))))

;; =============
;; eldoc-mode
;; =============
(add-hook 'irony-mode-hook 'irony-eldoc)

;; ======================================
;; C++ indentation style setting for ROS
;; ======================================
(defun ROS-c-mode-hook()
  (setq c-basic-offset 2)
  (setq indent-tabs-mode nil)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'case-label '+))
(add-hook 'c++-mode-hook 'ROS-c-mode-hook)

;;; In order to get namespace indentation correct, .h files must be opened in C++ mode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;; =============
;; python-mode
;; =============
(require 'jedi-core)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook
                   '(lambda ()
                        (setq indent-tabs-mode nil)
                        (setq indent-level 4)
                        (setq python-indent 4)
                        (setq tab-width 4)))
(add-hook 'python-mode-hook
	  (lambda ()
	    (add-to-list (make-local-variable 'company-backends)
			 'company-jedi)))

;; ===============
;; Markdwon mode
;; ===============
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-open-command "mark")
(setq markdown-preview-style "http://thomasf.github.io/solarized-css/solarized-light.min.css")
(add-hook 'markdown-mode-hook
          '(lambda ()
             (set (make-local-variable 'whitespace-action) nil)))
;; =============
;; cmake mode
;; =============
(require 'cmake-mode); Add cmake listfile names to the mode list.
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))
(add-hook 'cmake-mode-hook '(lambda ()
                              (setq-local electric-layout-rules '((?\) . after)))
                              ))

;; ==========================
;; for environment like path
;; ==========================
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; =============
;; winner mode
;; =============
(winner-mode 1)
(global-set-key (kbd "C-M-z") 'winner-undo)
;;(global-set-key (kbd "C-M-z") 'winner-redo)

;; =============
;; mmm-mode
;; http://jblevins.org/log/mmm
;; =============
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(defun my-mmm-markdown-auto-class (lang &optional submode)
  "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
If SUBMODE is not provided, use `LANG-mode' by default."
  (let ((class (intern (concat "markdown-" lang)))
        (submode (or submode (intern (concat lang "-mode"))))
        (front (concat "^```" lang "[\n\r]+"))
        (back "^```"))
    (mmm-add-classes (list (list class :submode submode :front front :back back)))
    (mmm-add-mode-ext-class 'markdown-mode nil class)))

;; Mode names that derive directly from the language name
(mapc 'my-mmm-markdown-auto-class
      '("awk" "bibtex" "c" "cpp" "css" "html" "latex" "lisp" "makefile"
        "markdown" "python" "r" "ruby" "sql" "stata" "xml"))
(my-mmm-markdown-auto-class "shell" 'shell-script-mode)

(setq mmm-parse-when-idle 't)

;; =============
;; whitespace-mode
;; http://keisanbutsuriya.hateblo.jp/entry/2015/02/03/153149
;; =============
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))

(setq whitespace-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

(global-whitespace-mode 1)
(defvar my/bg-color "#292B2E")
(set-face-attribute 'whitespace-trailing nil
                    :background my/bg-color
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :background my/bg-color
                    :foreground "LightSkyBlue"
                    :underline t)
(set-face-attribute 'whitespace-space nil
                    :background my/bg-color
                    :foreground "GreenYellow"
                    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
                    :background my/bg-color)

;; カーソルの設定
(setq-default cursor-type '(bar . 3))
(set-cursor-color "turquoise")

;;-------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "0788bfa0a0d0471984de6d367bb2358c49b25e393344d2a531e779b6cec260c5" "fbcdb6b7890d0ec1708fa21ab08eb0cc16a8b7611bb6517b722eba3891dfc9dd" "6998bd3671091820a6930b52aab30b776faea41449b4246fdce14079b3e7d125" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "e87a2bd5abc8448f8676365692e908b709b93f2d3869c42a4371223aab7d9cf8" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "2f00a1b3809f6e471d21742ba038146fc14c06ea9c31522a699953f7769e2836" default)))
 '(irony-additional-clang-options (quote ("-std=c++11")))
 '(markdown-preview-style "http://dakrone.github.io/org.css" t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(setq eww-search-prefix "http://www.google.co.jp/search?q=")
(defvar eww-disable-colorize t)
(defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
  (unless eww-disable-colorize
    (funcall orig start end fg)))
(advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
(advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
(defun eww-disable-color ()
  "eww で文字色を反映させない"
  (interactive)
  (setq-local eww-disable-colorize t)
  (eww-reload))
(defun eww-enable-color ()
  "eww で文字色を反映させる"
  (interactive)
  (setq-local eww-disable-colorize nil)
  (eww-reload))

(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,
close the *compilation* buffer if the compilation is successful,
and set the focus back to Emacs frame"
  (if (string-match "^finished" msg)
      (shell-command
       (format
	"notify-send -i ~/.emacs.d/documents/emacs.png \'Compilation Success!!!\'"))
    (shell-command
     (format
      "notify-send -i ~/.emacs.d/documents/emacs.png \'Compilation Failed\'")))
  )

(add-to-list 'compilation-finish-functions
	     'notify-compilation-result)

;; Org-mode
;; ref. https://github.com/fujimisakari/.emacs.d/blob/master/inits/40-org-mode.el
(setq org-support-shift-select 't)
(setq org-startup-truncated nil) ; org-mode開始時は折り返しするよう設定
(setq org-startup-with-inline-images t) ; 画像をインライン表示
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(setq org-latex-pdf-process '("latexmk -f")) ;; pdf process = latexmk
(setq org-latex-default-class "jsarticle") ;; default class = jsarticle
;; Make TAB the yas trigger key in the org-mode-hook and enable flyspell mode and autofill
(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet
            (make-variable-buffer-local 'yas/trigger-key)
            (org-set-local 'yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-group)
            ;; flyspell mode for spell checking everywhere
            (flyspell-mode 1)))
;; org-latex-classes
(add-to-list 'org-latex-classes
             '("jsarticle"
               "\\documentclass[11pt,a4paper,uplatex]{jsarticle}
                [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
               ))

;;------------------------------------------------------
;; マウス設定
;;------------------------------------------------------
(if window-system (progn
;; 右ボタンの割り当て(押しながらの操作)をはずす。
(global-unset-key [down-mouse-3])

;; マウスの右クリックメニューを出す(押して、離したときにだけメニューが出る)
(defun bingalls-edit-menu (event)
  (interactive "e")
  (popup-menu menu-bar-edit-menu))
(global-set-key [mouse-3] 'bingalls-edit-menu)
))

;; -------------------------------------------------------------------------
;; @multiple-cursors.el
;;
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M->") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-unset-key (kbd "C-<down-mouse-1>"))
(global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)

;; -------------------------------------------------------------------------
;; @pandoc-mode
;;
(add-hook 'markdown-mode-hook 'pandoc-mode)

;; -------------------------------------------------------------------------
;; @transform multi byte
;; http://qiita.com/ShingoFukuyama/items/e425356d0b7c2041d164
(defun my-replace-strings-in-region-by-list ($list)
  "Replace strings in a region according to $list"
  (if mark-active
      (let* (($beg (region-beginning))
             ($end (region-end))
             ($word (buffer-substring-no-properties $beg $end)))
        (mapc (lambda ($r)
                (setq $word (replace-regexp-in-string (car $r) (cdr $r) $word)))
              $list)
        (delete-region $beg $end)
        (insert $word))
    (error "Need to make region")))
;; 全角数字を半角数字に
(defun my-convert-to-single-byte-number ()
  "Convert multi-byte number in region into single-byte number"
  (interactive)
  (my-replace-strings-in-region-by-list
   '(("１" . "1")
     ("２" . "2")
     ("３" . "3")
     ("４" . "4")
     ("５" . "5")
     ("６" . "6")
     ("７" . "7")
     ("８" . "8")
     ("９" . "9")
     ("０" . "0"))))
;; 半角数字を全角数字に
(defun my-convert-to-multi-byte-number ()
  "Convert multi-byte number in region into single-byte number"
  (interactive)
  (my-replace-strings-in-region-by-list
   '(("1" ."１")
     ("2" ."２")
     ("3" ."３")
     ("4" ."４")
     ("5" ."５")
     ("6" ."６")
     ("7" ."７")
     ("8" ."８")
     ("9" ."９")
     ("0" ."０"))))
;; 句読点などの約物を半角に
(defun my-convert-yakumono-to-half-width ()
  "Replace multi byte punctuation marks to half width chars"
  (interactive)
  (my-replace-strings-in-region-by-list
   '(("、" . "，")
     ("。" . "．")
     ("「" . "｢")
     ("」" . "｣")
     ("［" . "[")
     ("］" . "]")
     ("｛" . "{")
     ("｝" . "}")
     ("（" . "(")
     ("）" . ")")
     ("・" . "･"))))

;; rosemacs
(add-to-list 'load-path "/opt/ros/indigo/share/emacs/site-lisp")
(require 'rosemacs-config)

;; helm
(require 'helm)
(require 'helm-config)
(require 'helm-files)
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "<f1>") 'helm-find)
(define-key helm-find-files-map (kbd "<return>") 'helm-execute-persistent-action)

;; 指定したディレクトリでhelm-findをやる
(defun in-directory-helm-find (dir)
  "Runs execute-extended-command with default-directory set to the given directory."
  (interactive "DIn directory: ")
  (let ((default-directory dir))
    (call-interactively 'helm-find)))
(define-key global-map (kbd "C-<f1>") 'in-directory-helm-find)
;; ディレクトリだったら補完する。ファイルだったら開く
(defun fu/helm-find-files-navigate-forward (orig-fun &rest args)
  (if (file-directory-p (helm-get-selection))
      (apply orig-fun args)
    (helm-maybe-exit-minibuffer)))
(advice-add 'helm-execute-persistent-action :around #'fu/helm-find-files-navigate-forward)
;; バックスペースでディレクトリだったら一段分戻る
(defun fu/helm-find-files-navigate-back (orig-fun &rest args)
  (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
      (helm-find-files-up-one-level 1)
    (apply orig-fun args)))
(advice-add 'helm-ff-delete-char-backward :around #'fu/helm-find-files-navigate-back)
