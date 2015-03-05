;; Personal init file for Emacs
;; Copyright (C) 2013-2014 Scott Weldon

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;----------------------------------------------------------------------------;;
;;                                Usage                                       ;;
;;----------------------------------------------------------------------------;;
;; This is my personal init file for Emacs. If your preferences are similar   ;;
;; mine, it should be usable with very little modification. To use, place the ;;
;; following code in your actual .init, modifying to fit your needs.          ;;
;;                                                                            ;;
;; This init file assumes that you use git to manage Emacs packages. To do so ;;
;; choose a folder as base, and do 'git clone <repo>' for each package.       ;;
;; Usually, the same is done with this repo.                                  ;;
;;                                                                            ;;
;; Note also that this init file assumes you will generally do                ;;
;; `emacs --daemon` once, and connect to it later with emacsclient. This is   ;;
;; used over (server-start) in case I want to start a non-server instance,    ;;
;; to debug changes to this config, for example.                              ;;
;;                                                                            ;;
;; In addition to the config listed here, I have defined several variables    ;;
;; that are `custom-set`-able. To edit them through the customize interface,  ;;
;; do `M-x customize-group RET weldon RET`. Otherwise, search for             ;;
;; "Custom Variables" below to see what variables are available, and set them ;;
;; by doing `(setq <var> <val>)` at any point in your main init file.         ;;
;;                                                                            ;;
;; This ends the introduction. The example code follows (in correct order):   ;;
;;----------------------------------------------------------------------------;;
;; Windows specific:                                                          ;;
;; (setq notify-method 'notify-via-growl)                                     ;;
;;----------------------------------------------------------------------------;;
;; Cross platform:                                                            ;;
;; (setq my-external-library-location "/path/to/git/folders")                 ;;
;; (setq my-irc-servers '(("alias" "irc.example.com" t 6667 "password")       ;;
;;                        ("freenode" "irc.freenode.net" nil 6667 nil))       ;;
;;   where the parameters of each sub-list are as follows: a custom name for  ;;
;;   the server (given as input to my-irc-actual), the server IP, whether or  ;;
;;   not to use SSL, the server port, and the password.                       ;;
;; (setq erc-nick '("myUserName" "secondChoice" "thirdChoice"))               ;;
;;                                                                            ;;
;; (load-file "/path/to/this/file")                                           ;;
;;                                                                            ;;
;; Or, more simply:                                                           ;;
;; (load-file (concat my-external-library-location "/emacs/init.el"))         ;;
;;----------------------------------------------------------------------------;;

;; Log beginning and end of loading, to help with troubleshooting.
(message "Begin loading Scott Weldon's custom init-file.")

;;----------------------------------------------------------------------------;;
;;                          Custom Load Path                                  ;;
;;----------------------------------------------------------------------------;;
;; Load path for various external libraries, managed with git.

(unless (boundp 'my-external-library-location)
  (error "External library location not defined!"))

(add-to-list 'load-path my-external-library-location)

(let* ((my-lisp-dir my-external-library-location)
       (default-directory my-lisp-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-lisp-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

;;----------------------------------------------------------------------------;;
;;                           Load Libraries                                   ;;
;;----------------------------------------------------------------------------;;
;; Load all needed libraries first
;; Using require so it is obvious when something breaks

(load "2048.el") ;; Breaks with require, probably because of number-only name.
(require 'ace-jump-mode)
(require 'ace-window)
(require 'aggressive-indent)
(require 'auto-complete-config)
(require 'auto-dim-other-buffers)
(require 'battery)
(require 'chess)
(require 'csharp-mode) ;; TODO find git repo
(require 'csv-mode) ;; TODO find git repo
(require 'deft)
(require 'editorconfig)
(require 'emms-setup)
(require 'em-alias)
(require 'em-banner)
(require 'em-basic)
(require 'em-cmpl)
(require 'em-dirs)
(require 'em-glob)
(require 'em-hist)
(require 'em-ls)
(require 'em-prompt)
(require 'em-script)
(require 'em-term)
(require 'em-unix)
(require 'erc-chess) ;; TODO find git repo
(require 'erc-join)
(require 'erc-log)
(require 'erc-match)
(require 'facebook)
(require 'fic-mode)
(require 'flx)
(require 'flx-ido)
(require 'flymake)
(require 'git-gutter)
(require 'gnus)
(require 'hideshow)
(require 'inf-ruby)
(require 'magit)
(require 'magit-blame)
(require 'magit-gitflow)
(require 'markdown-mode)
(require 'mew)
(require 'mk-project)
(require 'notify) ;; TODO find git repo
(require 'org)
(require 'org-checklist) ;; TODO find git repo
(require 'org-expiry)
(require 'org-feed)
(require 'os) ;; org-sync
(require 'org-trello)
;; FIXME
(setq persp-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-z s") 'persp-switch)
        (define-key map (kbd "C-z k") 'persp-remove-buffer)
        (define-key map (kbd "C-z c") 'persp-kill)
        (define-key map (kbd "C-z r") 'persp-rename)
        (define-key map (kbd "C-z a") 'persp-add-buffer)
        (define-key map (kbd "C-z A") 'persp-set-buffer)
        (define-key map (kbd "C-z i") 'persp-import)
        (define-key map (kbd "C-z n")       'persp-next)
        (define-key map (kbd "C-z <right>") 'persp-next)
        (define-key map (kbd "C-z p")       'persp-prev)
        (define-key map (kbd "C-z <left>")  'persp-prev)
        map))
(require 'perspective)
(require 'persp-projectile)
(require 'projectile)
(require 'python)
(require 'ruby-electric)
(require 'simple-rtm)
(require 'soundcloud)
(require 'tls)
(require 'twittering-mode)
(require 'uniquify)
(require 'vagrant-tramp)
(require 'web-mode)
(require 'whitespace)
(require 'xkcd)
(require 'yaml-mode)

;;----------------------------------------------------------------------------;;
;;                             Functions                                      ;;
;;----------------------------------------------------------------------------;;
;; Mostly designed to be called directly (or have a key-combo bound to).
;; All begin with prefix "my".

;; Open the window all over that screen.
(defun my-all-over-the-screen (&number)
  (interactive)
  (delete-other-windows)
  (let ((splits (or &number 3)))
    (dotimes (i (- splits 1))
      (split-window-horizontally)))
  (balance-windows)
  (follow-mode t))

(defun my-directory-files (directory &optional full match nosort)
  "Like `directory-files', but excluding \".\" and \"..\"."
  (let* ((files (cons nil (directory-files directory full match nosort)))
         (parent files)
         (current (cdr files))
         (exclude (if (full) ;; For absolute paths
                      (list (concat directory "/.")
                            (concat directory "/..")))
                  (list "." "..")) ;; For relative paths
         (file nil))
    (while (and current exclude)
      (setq file (car current))
      (if (not (member file exclude))
          (setq parent current)
        (setcdr parent (cdr current))
        (setq exclude (delete file exclude)))
      (setq current (cdr current)))
    (cdr files)))

(defun my-group(string)
  (concat "[" string "]"))

;;(defun my-ido-complete-and-exit()
;;(interactive)
;;(ido-complete)
;;(ido-exit-minibuffer))

(defun my-increment-eshell-command-count ()
  "Increments the eshell command count var."
  (incf my-eshell-command-count))

(defun my-insert-space-or-newline-and-indent()
  (interactive)
  (if (>= (current-column) fill-column)
      (newline-and-indent)
    (insert-char ? )))

(defun my-irc-actual(selected-name)
  "Connect to specific IRC server."
  (interactive "sWhich IRC server? ")
  (dotimes (i (length my-irc-servers))
    (let* ((curr-server (nth i my-irc-servers))
           (name (car curr-server))
           (ip (nth 1 curr-server))
           (ssl (nth 2 curr-server))
           (port (nth 3 curr-server))
           (pass (nth 4 curr-server)))
      (when (equal name selected-name)
        (message "name %s ip %s ssl %s port %s pass %s" name ip ssl port pass)
        (when ssl
          (if pass
              (erc-tls :server ip :port port :password pass)
            (erc-tls :server ip :port port)))
        (unless ssl
          (if pass
              (erc :server ip :port port :password pass)
            (erc :server ip :port port)))))))

(defun my-irc-all()
  "Connect to all IRC servers."
  (interactive)
  (dotimes (i (length my-irc-servers))
    (let ((name (car (nth i my-irc-servers))))
      (my-irc-actual name))))

(defun my-kill-buffer()
  (interactive)
  (catch 'quit
    (save-window-excursion
      (let (done)
        (if (and buffer-file-name (buffer-modified-p))
            (progn
              (while (not done)
                (let ((response (read-char-choice
                                 (format "Save file %s? (y, n, d, q) "
                                         (buffer-file-name))
                                 '(?y ?n ?d ?q))))
                  (setq done (cond
                              ((eq response ?q) (throw 'quit nil))
                              ((eq response ?y) (save-buffer) t)
                              ((eq response ?n) (set-buffer-modified-p nil) t)
                              ((eq response ?d) (diff-buffer-with-file) nil)))))
              (kill-buffer (current-buffer)))
          ;; Else
          (ido-kill-buffer))))))

(defun my-kill-buffer-and-jump()
  (interactive)
  (my-kill-buffer)
  (other-window 1))

;; TODO: kill only sub-dirs of given dir?
(defun my-kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun my-org-agenda-skip-tag(tag &optional others)
  "Skip all entries that correspond to TAG.

If OTHERS is true, skip all entries that do not correspond to TAG."
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
        (current-headline (or (and (org-at-heading-p)
                                   (point))
                              (save-excursion (org-back-to-heading)))))
    (if others
        (if (not (member tag (org-get-tags-at current-headline)))
            next-headline
          nil)
      (if (member tag (org-get-tags-at current-headline))
          next-headline
        nil))))

(defun my-org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done."
  (let (org-log-done org-log-states)
    (org-todo (if (= n-not-done 0) "DONE" (if (= n-done 0) "TODO" "STARTED")))))

(defun my-prev-window()
  (interactive)
  (other-window -1))

(defun my-set-twitter-timelines(symbol value)
  (set-default symbol value)
  (set-default 'twittering-initial-timeline-spec-string value))

(defun my-smooth-scroll (down)
  (if down
      (setq my-scroll 'scroll-up)
    (setq my-scroll 'scroll-down))
  (let ((n 1)
        (scrolled 0)
        (next-scroll 1)
        (height (- (window-height) 3)))
    (while (< next-scroll height)
      (sit-for (/ 1.0 (+ n 20)))
      (funcall my-scroll n)
      (setq scrolled (+ scrolled n))
      (setq n (lsh n 1))
      (setq next-scroll (+ next-scroll n)))
    (unless (= scrolled height)
      (funcall my-scroll (- height scrolled)))))

(defun my-smooth-scroll-down ()
  (interactive)
  (my-smooth-scroll t))

(defun my-smooth-scroll-up ()
  (interactive)
  (my-smooth-scroll nil))

(defun my-tomorrow-day ()
  "Returns the day of the week for tomorrow."
  (let ((day (1+ (string-to-number (format-time-string "%w")))))
    (if (= day 7)
        0
      day)))

;;----------------------------------------------------------------------------;;
;;                           Custom Variables                                 ;;
;;----------------------------------------------------------------------------;;

;; This section is for internal use only, thus, defvar.
(defvar my-eshell-command-count 0 "Variable to keep track of command count")
(make-variable-buffer-local 'my-eshell-command-count)

;; This section is to allow customization, thus, defcustom.
(defgroup weldon nil
  "Variable group for Scott Weldon's init file.")

(defcustom my-twitter-timelines
  '("(:home)")
  "List of timelines to open when launching twittering-mode.

To modify this variable, you can use the customize interface, or do e.g.:
(customize-set-variable 'my-twitter-timelines
                        '(\"(:home)\" \"(#emacs)\" \"(#gnu)\"))
"
  :type '(repeat string)
  :tag "My Twitter Timelines"
  :group 'weldon
  :set 'my-set-twitter-timelines
  )

;;----------------------------------------------------------------------------;;
;;                          Keyboard Shortcuts                                ;;
;;----------------------------------------------------------------------------;;
;; Tried to keep from conflicting with defaults, but no guarantees (especially
;; where functionality is replacement for default binding).

;; Prefix of C-x
(global-set-key (kbd "C-x k") 'my-kill-buffer)
(global-set-key (kbd "C-x K") 'my-kill-buffer-and-jump)
(global-set-key (kbd "C-x O") 'my-prev-window)
(global-set-key (kbd "C-x g") 'magit-status)

;; Prefix of C-c
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'bury-buffer)
(global-set-key (kbd "C-c e") 'eval-region)
(global-set-key (kbd "C-c r t m") 'simple-rtm-mode)
(global-set-key (kbd "C-c i r c") 'my-irc-actual)
(global-set-key (kbd "C-c s") 'hs-show-block)
(global-set-key (kbd "C-c h") 'hs-hide-block)
(global-set-key (kbd "C-c S") 'hs-show-all)
(global-set-key (kbd "C-c H") 'hs-hide-all)

;; Scrolling
(global-set-key (kbd "<prior>") 'my-smooth-scroll-up)
(global-set-key (kbd "<next>") 'my-smooth-scroll-down)

;; Ido
;;(define-key ido-common-completion-map (kbd "RET") #'my-ido-complete-and-exit)

;; XKCD
(define-key xkcd-mode-map (kbd "g") 'xkcd-get)
(define-key xkcd-mode-map (kbd "n") 'xkcd-next)
(define-key xkcd-mode-map (kbd "p") 'xkcd-prev)

;;----------------------------------------------------------------------------;;
;;                             Global Config                                  ;;
;;----------------------------------------------------------------------------;;
;; Various global settings, including mode line config.

;; Color theme
(set-face-attribute 'default nil
                    :foreground "white"
                    :background "black"
                    :height 90)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'auto-dim-other-buffers-face nil :background "grey7")

;;(global-whitespace-mode t)
(setq whitespace-style '(face tabs trailing lines space-before-tab
                              newline indentation empty space-after-tab
                              tab-mark))
(global-visual-line-mode t)
(setq display-time-24hr-format t)
(column-number-mode 1)
(display-time-mode 1)
(setq battery-mode-line-format " [%b%p%%] ")
(global-aggressive-indent-mode)
(auto-dim-other-buffers-mode)
(setq ring-bell-function #'ignore)
(global-git-gutter-mode)

(add-to-list 'safe-local-variable-values '(auto-revert-mode . 1))

;; 2048
(defface 2048-2-face '((t (:foreground "red")))
  "Face used for 2" :group '2048-game)
(defface 2048-4-face '((t (:foreground "orange")))
  "Face used for 4" :group '2048-game)
(defface 2048-8-face '((t (:foreground "yellow")))
  "Face used for 8" :group '2048-game)
(defface 2048-16-face '((t (:foreground "green")))
  "Face used for 16" :group '2048-game)
(defface 2048-32-face '((t (:foreground "lightblue" :bold t)))
  "Face used for 32" :group '2048-game)
(defface 2048-64-face '((t (:foreground "lavender" :bold t)))
  "Face used for 64" :group '2048-game)
(defface 2048-128-face '((t (:foreground "SlateBlue" :bold t)))
  "Face used for 128" :group '2048-game)
(defface 2048-256-face '((t (:foreground "MediumVioletRed" :bold t)))
  "Face used for 256" :group '2048-game)
(defface 2048-512-face '((t (:foreground "tomato" :bold t)))
  "Face used for 512" :group '2048-game)
(defface 2048-1024-face '((t (:foreground "SkyBlue1" :bold t)))
  "Face used for 1024" :group '2048-game)
(defface 2048-2048-face '((t (:foreground "lightgreen" :bold t)))
  "Face used for 2048" :group '2048-game)
(defvar 2048-font-lock-keywords
  '(("\\<2\\>" 0 '2048-2-face)
    ("\\<4\\>" 0 '2048-4-face)
    ("\\<8\\>" 0 '2048-8-face)
    ("\\<16\\>" 0 '2048-16-face)
    ("\\<32\\>" 0 '2048-32-face)
    ("\\<64\\>" 0 '2048-64-face)
    ("\\<128\\>" 0 '2048-128-face)
    ("\\<256\\>" 0 '2048-256-face)
    ("\\<512\\>" 0 '2048-512-face)
    ("\\<1024\\>" 0 '2048-1024-face)
    ("\\<2048\\>" 0 '2048-2048-face)))
(defun my-2048-hook()
  (font-lock-add-keywords nil 2048-font-lock-keywords)
  (text-scale-set 2))

;; Battery?
(when (and battery-status-function
           (not (string-match-p "N/A"
                                (battery-format "%B"
                                                (funcall battery-status-function)))))
  (display-battery-mode 1))

;; Auto Backup Files
(setq backup-directory-alist '(("." . "~/.saves"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Auto-complete
(add-to-list 'ac-dictionary-directories
             (concat my-external-library-location "/auto-complete/dict"))
(ac-config-default)
(setq ac-ignore-case nil)

;; Code Formatting
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 2)
(setq c-basic-indent 2)
(setq sh-basic-offset 2)
(setq sh-indentation 2)
(setq lisp-basic-indent 2)
(setq lisp-basic-offset 2)
(setq-default fill-column 70)
(setq ruby-indent-size 2)
(setq python-indent-offset 2)

;; Gnus
(setq gnus-save-newsrc-file nil
      gnus-summary-make-false-root nil
      gnus-use-cache t)

;; IRC
(setq erc-kill-server-buffer-on-quit t
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477")
      erc-log-channels-directory "~/.erc/logs/"
      erc-log-insert-log-on-open nil
      erc-log-channels t
      erc-server-reconnect-timeout 30
      erc-join-buffer 'bury
      erc-server-reconnect-attempts 10)
(erc-autojoin-mode 1)

;; Navigation
(ido-mode)
(setq ido-separator "\n"
      ido-auto-merge-work-directories-length -1
      ido-default-buffer-method 'selected-window
      ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
                           "*Messages*" "Async Shell Command"))
(flx-ido-mode 1)
(setq magit-completing-read-function 'magit-ido-completing-read)

;; Org Mode
(setq org-use-fast-todo-selection t
      org-deadline-warning-days 7
      org-agenda-start-on-weekday nil
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-log-into-drawer t
      org-expiry-inactive-timestamps t
      org-expiry-created-property-name "CREATED"
      org-todo-keyword-faces '(("WAITING" . "yellow"))
      org-stuck-projects '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))
(add-hook 'org-after-todo-statistics-hook 'my-org-summary-todo)

;; Projectil
(setq projectile-enable-caching t)

;; Web mode
(set-face-attribute 'web-mode-whitespace-face nil :background "red")
(set-face-attribute 'web-mode-html-tag-face nil :foreground "Blue")
(set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "Blue")
(setq web-mode-extra-auto-pairs
      '(("erb" . (("open" "close")))
        ("php" . (("open" "close")
                  ("open" "close")))))
(setq web-mode-engines-alist
      '(("php"   . "\\.php\\'")
        ("blade" . "\\.blade\\.")
        ("erb"   . "\\.erb\\'")))
(setq web-mode-script-padding 2
      web-mode-style-padding 2
      web-mode-enable-auto-closing t)

;; Twitter
(setq twittering-use-master-password t
      twittering-icon-mode t
      twittering-status-format "%i %s,%FACE[font-lock-preprocessor-face]{%p} %FACE[font-lock-comment-face]{%@}: %FACE[font-lock-keyword-face]{%e} %FACE[font-lock-function-name-face]{%F}
%FOLD[  ]{%T
%FACE[font-lock-comment-face]{// from %f%L%r%R}}
"
      ;; Format retweets as "<my comments> RT <name>: <tweet>"
      twittering-retweet-format '(nil _ " RT %s: %t"))

;; Eshell Prompt
(setq eshell-prompt-function
      (lambda ()
        (let ((nl "\n"))
          (concat
           (my-group (make-string 78 ?-))
           nl (my-group (eshell/pwd)) nl
           ;; Modifications usually go here
           (my-group (format-time-string "%Y-%m-%d" (current-time)))
           (my-group (format-time-string "%H:%M:%S" (current-time)))
           (my-group (concat user-login-name "@" system-name))
           (my-group (concat (int-to-string my-eshell-command-count)
                             ":"
                             (format "%s" eshell-last-command-status)))
           ;; And end here
           nl (if (= (user-uid) 0) "# " "$ ")))))

;; Uniquify
;;(setq uniquify-buffer-name-style 'post-forward-angle-brackets
;;      uniquify-min-dir-content 999)

;;----------------------------------------------------------------------------;;
;;                           Default-Frame-Alist                              ;;
;;----------------------------------------------------------------------------;;

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(cursor-color . "white"))

;;----------------------------------------------------------------------------;;
;;                             Auto-Mode-Alist                                ;;
;;----------------------------------------------------------------------------;;

;; EditorConfig
(add-to-list 'auto-mode-alist '("\\.editorconfig$" . conf-unix-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Org Trello
(add-to-list 'auto-mode-alist '("\\.trello\\.org\\'" . (lambda ()
                                                         (org-mode)
                                                         (org-trello-mode))))

;; Web
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;----------------------------------------------------------------------------;;
;;                              Mode Hooks                                    ;;
;;----------------------------------------------------------------------------;;
;; Various mode hooks

;; 2048
(add-hook '2048-mode-hook 'my-2048-hook)

;; More indentation
(add-hook 'c-mode-common-hook
          (lambda()
            (c-set-offset 'case-label '2)
            (hs-minor-mode)))

;; Syntax highlighting for diffs
(add-hook 'diff-mode-hook
          (lambda()
            (set-face-foreground 'diff-removed "red")
            (set-face-foreground 'diff-added "green")))

;; Hook to keep a shell in sync with current dired buffer.
;; FIXME:
;;   Doesn't work for switching to existing dired buffer. (Workaround by
;;     refreshing buffer with `g`.)
;;   Shell prompt gets smashed together. (Workaround by adding \n to front of
;;     shell prompt.)
(add-hook 'dired-after-readin-hook (lambda()
                                     (unless (get-buffer "*sync-shell*")
                                       (shell "*sync-shell*"))
                                     (process-send-string
                                      (get-buffer "*sync-shell*")
                                      (format "cd %s\n" default-directory))
                                     (message "Switched to new directory")))

;; Lisp mode
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

;; Custom *scratch*
(add-hook 'emacs-startup-hook
          (lambda ()
            (interactive)
            (kill-buffer "*scratch*")
            (find-file (concat my-external-library-location "/.scratch"))
            (rename-buffer "*scratch*")
            (lisp-interaction-mode)
            (bury-buffer "*scratch*")
            (cd "~")))

;; Eshell
(add-hook 'eshell-after-prompt-hook 'my-increment-eshell-command-count)

;; Git
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;; Markdown
(add-hook 'markdown-mode-hook (lambda ()
                                (flyspell-mode-on)))

;; Ruby
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))

;; Twitter
(add-hook 'twittering-edit-mode-hook
          (lambda ()
            (ispell-minor-mode)
            (flyspell-mode)))

;; Web mode
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-code-indent-offset 2
                  web-mode-enable-auto-pairing t
                  ;;web-mode-enable-whitespaces t
                  ;;web-mode-whitespaces-regexp "(^[\t]+)|([\t ]+$)"
                  web-mode-display-table nil)
            (local-set-key (kbd "RET") 'newline-and-indent)))

;;----------------------------------------------------------------------------;;
;;                                   Advice                                   ;;
;;----------------------------------------------------------------------------;;
;; Any `defadvice` stuff goes here.

(defadvice mouse-set-point (around mouse-set-point (event) activate)
  (let ((event-name (car event))
        (event-target-window (caadr event)))
    (if (and (eql 'down-mouse-1 event-name)
             (eql event-target-window (frame-selected-window)))
        ad-do-it
      (set-frame-selected-window nil event-target-window))))

(defadvice mouse-drag-region (around mouse-drag-region (event) activate)
  (let ((event-name (car event))
        (event-target-window (caadr event)))
    (if (eql event-target-window (frame-selected-window))
        ad-do-it
      (set-frame-selected-window nil event-target-window))))

;;----------------------------------------------------------------------------;;
;;                              Eval-After-Load                               ;;
;;----------------------------------------------------------------------------;;

;; Allow interaction with zip files in Dired
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(eval-after-load 'parse-time
  '(progn
     (setq parse-time-weekdays (nconc parse-time-weekdays
                                      `(("tom" . ,(my-tomorrow-day)))))
     (defvar parse-time-tomorrow-timer
       (run-at-time "12am"
                    (* 24 60 60)
                    (lambda ()
                      (setf (cdr (assoc "tom" parse-time-weekdays))
                            (my-tomorrow-day))))
       "Timer to reset the day to which \"tom\" refers in timestamps.")))

(eval-after-load 'tramp
  '(vagrant-tramp-enable))

(message "End of Scott Weldon's custom init-file.")
;; End init.el
