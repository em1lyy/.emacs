;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; German keyboard layout (QWERTZ)
(setq default-input-method "german-prefix")

;; line numbers, etc.
(global-linum-mode 1)
(global-visual-line-mode 1)
(column-number-mode 1)
(elcord-mode 1)

(defun my-disable-electric-indent ()
    "Disable electric indenting."
    (electric-indent-local-mode -1))

;; If emacs is windowed, disable all toolbars etc.
(if window-system
    (progn
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (toggle-scroll-bar -1))
)

(defun use-tabs-in-buffer ()
  "Use tabs instead of spaces in a buffer
This is useful for editing things such as Makefiles
where tabs are required"
  (setq indent-tabs-mode t))

;; Tab width & 1 Tab = 4 Spaces
(setq tab-width 4)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(add-hook 'makefile-mode-hook #'use-tabs-in-buffer)
(setq tab-stop-list (number-sequence 4 120 4))

;; Indent case in switch statement (because not doing so is ugly)
(c-set-offset 'case-label '+)
(c-set-offset 'cpp-macro -64)

;; Enable <backtab>
(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
(defun un-indent-by-removing-4-spaces ()
  "removes 4 spaces from the beginning of the current line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))

;; C ElDoc mode!
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)

;; Remember: C-h c to find what a key does,
;; C-h w to find where a function is on the kbd.

;; Enable inline lisp eval & replace
(defun eval-region-inline ()
  "evaluates the selected region and replaces it with the eval result."
  (interactive)
  (let ((eval-result (pp-to-string (eval (pp-last-sexp) lexical-binding))))
    (delete-region (save-excursion (backward-sexp) (point)) (point))
    (insert eval-result)))
(global-set-key (kbd "C-c C-e") 'eval-region-inline)

;; Eval inline lisp in markdown-mode
(defun markdown-eval-inline-lisp ()
  "evaluates all inline lisp (marked by $`(expr)`$)"
  (interactive)
  (save-excursion (goto-char (point-min))
                  (if (search-forward "$`" nil t)
                      (let ((delete-pos-beginning (point)))
                        (if (search-forward "`$" (line-end-position) t)
                            (progn
                              (goto-char (- (point) 2))
                              (delete-region (- delete-pos-beginning 2) delete-pos-beginning)
                              (delete-region (point) (+ (point) 2))
                              (eval-region-inline)
                              (markdown-eval-inline-lisp)))))))

;; syntax highlighting in markdown code blocks
(setq markdown-fontify-code-blocks-natively t)

;; Moe Theme & Moe Powerline
(require 'powerline)
(require 'moe-theme)

;; Title sizes
(setq moe-theme-resize-markdown-title '(1.5 1.4 1.3 1.2 1.0 1.0))
(setq moe-theme-resize-org-title '(1.5 1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0))
(setq moe-theme-resize-rst-title '(1.5 1.4 1.3 1.2 1.1 1.0))

;; Select colors & update powerline
(moe-theme-apply-color 'magenta)
(moe-dark)
(moe-theme-powerline)

;; Rainbow parentheses
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Emacs RSS Feeds
(setq elfeed-feeds
      '(
        ;; work
        ("file:///home/emily/Dokumente/corona_ha/aufgabenfeed.rss" iserv)

        ;; reddit
        ("https://www.reddit.com/r/SinonAssOnline.rss" sinon)
        ("https://www.reddit.com/r/Lucina.rss" lucina)
        ("https://www.reddit.com/r/awwnime.rss" awwnime)
))
(global-set-key (kbd "C-c M-m f") 'elfeed)

;; Some keybinds
(global-set-key (kbd "C-x C-k C-r") 'revert-buffer)
(global-set-key (kbd "C-x C-ö") 'comment-line)  ; C-c C-c comment-region
(global-set-key (kbd "C-x M-c") 'compile)       ; wanted to do C-x c but too similar to C-x C-c for my taste

;; Server
(server-start)

;; CompAny tab auto complete & plugins
; (global-set-key (kbd "TAB") 'company-complete-selection)
; (global-set-key (kbd "<right>") 'company-complete-common)
;; (setq company-dabbrev-downcase nil)
;; (setq company-tooltip-idle-delay 0)

;; (add-hook 'prog-mode-hook 'company-mode)

;; LSP Mode
;; (require 'lsp-mode)
;; (add-hook 'c-mode-hook #'lsp)
;; (add-hook 'python-mode-hook #'lsp)
;; (add-hook 'c++-mode-hook #'lsp)
;; (add-hook 'go-mode-hook #'lsp)
;; (add-hook 'javascript-mode-hook #'lsp)

;; LSP gopls (Golang)
;; (defun lsp-go-install-save-hooks ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; CompAny fuzzy
;; (setq company-fuzzy-sorting-backend 'alphabetic)
;; (setq company-fuzzy-prefix-on-top t)
;; (global-company-fuzzy-mode 1)

;; CompAny C Headers (a thing vscodium does really well)
;; (add-to-list 'company-backends 'company-c-headers)

;; move lines & region

(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun move-line-up ()
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun move-line-down ()
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(defun move-line-region-up (&optional start end n)
  (interactive "r\np")
  (if (use-region-p) (move-region-up start end n) (move-line-up)))

(defun move-line-region-down (&optional start end n)
  (interactive "r\np")
  (if (use-region-p) (move-region-down start end n) (move-line-down)))

(global-set-key (kbd "M-p") 'move-line-region-up)
(global-set-key (kbd "M-n") 'move-line-region-down)

;; mark ring convenience
(setq set-mark-command-repeat-pop 1)

;; display-time-mode format
(setq display-time-format "%H:%M %a, %d. %b")
(setq display-time-default-load-average nil)

;; Python running, because the python.el built-in version sucks
;; IMO and is not compatible with my setup and requirements

(defun python-run-current-buffer-file-in-shell ()
  (interactive)
  (cond
   ((string-equal system-type "windows-nt") ; Lots of duplication, but Windows might need additional adjustments/fixes
    (cond
     (buffer-file-name
      (progn
        (save-buffer)
        (start-process "python-run" "*Python-Run*" "py.exe" "-3" buffer-file-name)
        (with-current-buffer "*Python-Run*"
          (linum-mode t)
          (goto-char (point-min)))
        (display-buffer "*Python-Run*")))
     (t (error "Current buffer is not associated with a file"))))
   ((or (string-equal system-type "gnu/linux") (string-equal system-type "darwin"))
    (cond
     (buffer-file-name
      (progn
        (save-buffer)
        (start-process "python-run" "*Python-Run*" "sh" "-c" (concat "python3 " buffer-file-name))
        (with-current-buffer "*Python-Run*"
          (linum-mode t)
          (goto-char (point-min)))
        (display-buffer "*Python-Run*")))
     (t (error "Current buffer is not associated with a file"))))
   (t (error "Unsupported platform"))))

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "C-c C-b") 'python-run-current-buffer-file-in-shell)))

;; Shell command on buffer
;;  * Operates on same buffer
;;  * Replaces contents with output
;;  * Tries to restore cursor position

(defun shell-command-on-buffer ()
  (interactive)
  (let ((line (line-number-at-pos)))
    ;; replace buffer with output of shell command
    (shell-command-on-region (point-min) (point-max) (read-shell-command "Shell command on buffer: ") nil t)
    ;; restore cursor position
    (goto-line line)
    (recenter-top-bottom)))

(global-set-key (kbd "M-°") 'shell-command-on-buffer)

;; Window navigation needs to be quicker

(defun other-window-backward ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-x O") 'other-window-backward)

;; Custom workspace/project manager

(setq current-project-name nil)

(defun kill-old-session-buffers ()
  (interactive)
  (mapc
   (lambda (x)
     (let ((name (buffer-name x)))
       (unless (or (eq ?\s (aref name 0)) (eq ?* (aref name 0)))
         (kill-buffer x))))
   (buffer-list)))

(defun make-directory-list (dir-list)
  (mapcar
   (lambda (dir)
      (unless (file-directory-p dir)
	(make-directory dir)))
   dir-list))

(defun build-completion-list ()
  (make-directory-list
   (list (concat user-emacs-directory "projects/")))
  (directory-files (concat user-emacs-directory "projects/") nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))

(defun save-project (&optional pronamearg)
  (interactive)
  (let* ((proname (if pronamearg
                      pronamearg
                    (completing-read "Project Name: " (build-completion-list))))
         (basepath (concat user-emacs-directory "projects/"))
         (propath (concat basepath proname "/")))
    (make-directory-list
     (list basepath propath))
    (desktop-save propath)
    (setq current-project-name proname)))

(defun save-current-project ()
  (interactive)
  (if current-project-name
      (save-project current-project-name)
    (error "No current project to save")))

(defun save-current-project-and-release-lock ()
  (interactive)
  (if current-project-name
      (let* ((propath (concat user-emacs-directory "projects/" current-project-name "/")))
        (desktop-save propath t))
    (message "No current project to save")))

(defun load-project (&optional pronamearg)
  (interactive)
  (let* ((proname (if pronamearg
                      pronamearg
                    (completing-read "Project Name: " (build-completion-list))))
         (basepath (concat user-emacs-directory "projects/"))
         (propath (concat basepath proname "/")))
    (make-directory-list
     (list basepath propath))
    (save-current-project-and-release-lock)
    (kill-old-session-buffers)
    (if (desktop-read propath)
        (progn
          (message (concat "Loaded project " proname))
          (setq current-project-name proname))
      (error "No such project"))))

(defun save-and-unload-project ()
  (interactive)
  (save-current-project-and-release-lock)
  (kill-old-session-buffers)
  (setq current-project-name nil))

(add-hook 'kill-emacs-hook 'save-current-project-and-release-lock)

(global-set-key (kbd "C-c d s") 'desktop-save)
(global-set-key (kbd "C-c d r") 'desktop-read)
(global-set-key (kbd "C-c p s") 'save-project)
(global-set-key (kbd "C-c p S") 'save-current-project)
(global-set-key (kbd "C-c p l") 'load-project)
(global-set-key (kbd "C-c p u") 'save-and-unload-project)

;; Abbrev-mode

(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq save-abbrevs 'silent)

;; ERC

(autoload 'erc "erc" "IRC client." t)
(autoload 'erc-tls "erc" "IRC over TLS client." t)
(require 'erc-services)
(erc-services-mode 1)

;; Make C-c RET (or C-c C-RET) send messages instead of RET.
(define-key erc-mode-map (kbd "RET") nil)
(define-key erc-mode-map (kbd "C-c RET") 'erc-send-current-line)
(define-key erc-mode-map (kbd "C-c C-RET") 'erc-send-current-line)

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
;(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)

;; Custom Prompt
(setq erc-prompt (lambda () (concat "[" (buffer-name) "]")))

;; Coloured nicks

;; Pool of colors to use when coloring IRC nicks.
(setq erc-colors-list '("green" "blue" "red"
                        "dark gray" "dark orange"
                        "dark magenta" "maroon"
                        "indian red" "black" "forest green"
                        "midnight blue" "dark violet"))

;; special colors for some people
;(setq erc-nick-color-alist '(("John" . "blue")
;                             ("Bob" . "red")))

(defun erc-get-color-for-nick (nick)
  "Gets a color for NICK. If NICK is in erc-nick-color-alist, use that color, else hash the nick and use a random color from the pool"
  (or (cdr (assoc nick erc-nick-color-alist))
      (nth
       (mod (string-to-number
             (substring (md5 (downcase nick)) 0 6) 16)
            (length erc-colors-list))
       erc-colors-list)))

(defun erc-put-color-on-nick ()
  "Modifies the color of nicks according to erc-get-color-for-nick"
  (save-excursion
    (goto-char (point-min))
    (while (forward-word 1)
      (setq bounds (bounds-of-thing-at-point 'word))
      (setq word (buffer-substring-no-properties
                  (car bounds) (cdr bounds)))
      (when (or (and (erc-server-buffer-p) (erc-get-server-user word))
                (and erc-channel-users (erc-get-channel-user word)))
        (put-text-property (car bounds) (cdr bounds) 
                           'face (cons 'foreground-color
                                       (erc-get-color-for-nick word)))))))

(add-hook 'erc-insert-modify-hook 'erc-put-color-on-nick)

(add-hook 'erc-mode-hook (lambda () 
                           (modify-syntax-entry ?\_ "w" nil)
                           (modify-syntax-entry ?\- "w" nil)))

(setq inhibit-startup-message t)
(setq major-mode 'text-mode)
(setq-default major-mode 'text-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-term-color-vector
   [unspecified "#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#a1efe4" "#fcfcfa"])
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "d9646b131c4aa37f01f909fbdd5a9099389518eb68f25277ed19ba99adeb7279" "bc7627a5d14001acb3237151df7ccb413b57e4a820295ec24562c132efcacb2e" "0feb7052df6cfc1733c1087d3876c26c66410e5f1337b039be44cb406b6187c6" "5846b39f2171d620c45ee31409350c1ccaddebd3f88ac19894ae15db9ef23035" "27a1dd6378f3782a593cc83e108a35c2b93e5ecc3bd9057313e1d88462701fcd" default))
 '(delete-selection-mode nil)
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   '(nginx-mode go-mode nim-mode zig-mode elixir-mode elcord rainbow-delimiters elfeed powerline gruvbox-theme moe-theme markdown-mode))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#282828"))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(tab-always-indent t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
