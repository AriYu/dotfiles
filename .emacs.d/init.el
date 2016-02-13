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

;; font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ricty for Powerline" :foundry "unknown" :slant normal :weight normal :height 128 :width normal)))))

;;; color-theme
(load-theme 'monokai t)

;; 画面の下の方を綺麗にする
(require 'powerline)
(powerline-default-theme)
(set-face-attribute 'mode-line nil
                    :foreground "White"
                    :background "DarkCyan"
                    :box nil)

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
;; Yatexの自動改行をなしにする．
(add-hook 'yatex-mode-hook
		  '(lambda () (auto-fill-mode -1)))
;;;(setq tex-command "latexmk -pvc")  ;;保存したら自動で再コンパイル
(setq tex-command "latexmk -f") ;; 強制コンパイル
(setq dvi2-command "evince")
(add-hook 'yatex-mode-hook
      #'(lambda ()
          (define-key YaTeX-mode-map (kbd "C-c c") 'compile) ;;C-c c でmake できるようにする
          (auto-complete-mode t)
	  (setq compile-command "latexmk -f") 
          ))
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
(add-to-list 'auto-mode-alist '("\\.launch$" . xml-mode))

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
;;; プレフィクスキーはC-z
;; (setq elscreen-prefix-key (kbd "C-z"))
;; (elscreen-start)
;; ;;; タブの先頭に[X]を表示しない
;; (setq elscreen-tab-display-kill-screen nil)
;; ;;; header-lineの先頭に[<->]を表示しない
;; (setq elscreen-tab-display-control nil)
;; ;;; バッファ名・モード名からタブに表示させる内容を決定する(デフォルト設定)
;; (setq elscreen-buffer-to-nickname-alist
;;       '(("^dired-mode$" .
;;          (lambda ()
;;            (format "Dired(%s)" dired-directory)))
;;         ("^Info-mode$" .
;;          (lambda ()
;;            (format "Info(%s)" (file-name-nondirectory Info-current-file))))
;;         ("^mew-draft-mode$" .
;;          (lambda ()
;;            (format "Mew(%s)" (buffer-name (current-buffer)))))
;;         ("^mew-" . "Mew")
;;         ("^irchat-" . "IRChat")
;;         ("^liece-" . "Liece")
;;         ("^lookup-" . "Lookup")))
;; (setq elscreen-mode-to-nickname-alist
;;       '(("[Ss]hell" . "shell")
;;         ("compilation" . "compile")
;;         ("-telnet" . "telnet")
;;         ("dict" . "OnlineDict")
;;         ("*WL:Message*" . "Wanderlust")))

;; smartparent
(smartparens-global-mode)
;; [DEL]キーもしくは[C-h]に当てられているdelete-backward-charにadviceをかけられて削除するたびにフリーズする．これを無効化.
(ad-disable-advice 'delete-backward-char 'before 'sp-delete-pair-advice)
(ad-activate 'delete-backward-char)

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
  ) 
(add-hook 'web-mode-hook 'my-web-mode-hook)

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
;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/yasnippet-snippets"
;;         ))
;; (yas-load-directory (concat (cask-dependency-path cask-bundle 'yasnippet)
;;                             "/snippets"))
;; (cask-dependency-path cask-bundle 'yasnippet)
(eval-after-load "yasnippet"
  '(progn
     ;; companyと競合するのでyasnippetのフィールド移動は "C-i" のみにする
     (define-key yas-keymap (kbd "<tab>") nil)
     (yas-global-mode 1)))

;; company-mode 補完
(when (locate-library "company")
  (global-company-mode 1) ; 全バッファで有効にする
  (setq company-idle-delay 0) ; デフォルトは0.5
  (setq company-minimum-prefix-length 1) ; デフォルトは4
  (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  (global-set-key (kbd "C-M-i") 'company-complete)
  ;; (setq company-idle-delay nil) ; 自動補完をしない
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  ;; (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  )
(defun company--insert-candidate2 (candidate)
  (when (> (length candidate) 0)
    (setq candidate (substring-no-properties candidate))
    (if (eq (company-call-backend 'ignore-case) 'keep-prefix)
        (insert (company-strip-prefix candidate))
      (if (equal company-prefix candidate)
          (company-select-next)
          (delete-region (- (point) (length company-prefix)) (point))
        (insert candidate))
      )))
(defun company-complete-common2 ()
  (interactive)
  (when (company-manual-begin)
    (if (and (not (cdr company-candidates))
             (equal company-common (car company-candidates)))
        (company-complete-selection)
      (company--insert-candidate2 company-common))))

(define-key company-active-map [tab] 'company-complete-common2)
;; (define-key company-active-map [backtab] 'company-select-previous) 

(set-face-attribute 'company-tooltip nil
                    :foreground "black" :background "lightgrey")
(set-face-attribute 'company-tooltip-common nil
                    :foreground "black" :background "lightgrey")
(set-face-attribute 'company-tooltip-common-selection nil
                    :foreground "white" :background "steelblue")
(set-face-attribute 'company-tooltip-selection nil
                    :foreground "black" :background "steelblue")
(set-face-attribute 'company-preview-common nil
                    :background nil :foreground "lightgrey" :underline t)
(set-face-attribute 'company-scrollbar-fg nil
                    :background "DarkCyan")
(set-face-attribute 'company-scrollbar-bg nil
                    :background "gray40")

;; for c++ 
(require 'irony)
(eval-after-load "irony"
  '(progn
     (custom-set-variables '(irony-additional-clang-options '("-std=c++11")))
     (add-to-list 'company-backends 'company-irony)
     (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
     (add-hook 'c-mode-common-hook 'irony-mode)))
;;; C++ style
;; (load-file "~/.emacs.d/elpa/google-c-style-20140929.1118/google-c-style.el")
;; (require 'google-c-style)
;; (add-hook 'c-mode-common-hook 'google-set-c-style)

;; python
(require 'jedi-core)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(add-hook 'python-mode-hook 'jedi:setup)
(add-to-list 'company-backends 'company-jedi) ; backendに追加
(add-hook 'python-mode-hook
                   '(lambda ()
                        (setq indent-tabs-mode nil)
                        (setq indent-level 4)
                        (setq python-indent 4)
                        (setq tab-width 4)))


;; for arduino-mode
(require 'arduino-mode)
;; (require 'company-arduino)
;; (add-hook 'irony-mode-hook 'company-arduino-turn-on)
;; (defun my-company-c-headers-get-system-path ()
;;   "Return the system include path for the current buffer."
;;   (let ((default '("/usr/include/" "/usr/local/include/")))
;;     (company-arduino-append-include-dirs default t)))
;; (setq company-c-headers-path-system 'my-company-c-headers-get-system-path)

;; Activate irony-mode on arudino-mode
(add-hook 'arduino-mode-hook 'irony-mode)

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
