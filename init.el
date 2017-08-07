;; Personal init file for Emacs
;; Copyright (C) 2013-2016 Scott Weldon

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
;; (load-file (concat my-external-library-location "/emacs-init/init.el"))    ;;
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
;;(require 'auto-complete-config)
(require 'auto-dim-other-buffers)
(require 'auto-formatter)
(require 'battery)
(require 'chess)
(load-library "clang-format")
(require 'company)
(require 'csharp-mode)
(require 'csv-mode)
(require 'doc-present)
(require 'dockerfile-mode)
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
(require 'feature-mode)
(require 'fic-mode)
(require 'flx)
(require 'flx-ido)
(require 'flymake)
(require 'git-gutter)
(require 'gnus)
(require 'go-mode)
(require 'hideshow)
(require 'ido-ubiquitous)
(require 'inf-ruby)
(require 'ledger-mode)
(require 'less-css-mode)
(require 'lorem-ipsum)
(require 'magit)
(require 'magit-annex)
(require 'magit-blame)
(require 'magit-gitflow)
(require 'markdown-mode)
(require 'mew)
(require 'notify) ;; TODO find git repo
(require 'omnisharp)
(require 'org)
(require 'org-checklist) ;; TODO find git repo
(require 'org-expiry)
(require 'org-feed)
(require 'org-habit)
(require 'orgit)
(require 'os) ;; org-sync
(load-library "os-github")
(require 'org-super-agenda)
(require 'org-trello)
(require 'perspective)
(require 'persp-projectile)
(require 'pocket-api)
(require 'pocket-mode)
(require 'projectile)
(require 'python)
(require 'ruby-electric)
(require 'soundcloud)
(require 'ssh-agency)
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

(defun my-compose-words()
  "Count the number of words in the closest :COMPOSE: org-mode drawer."
  (interactive)
  (save-excursion
    (search-backward ":COMPOSE:")
    (beginning-of-line)
    (next-line)
    (let ((beg (point)))
      (search-forward ":END:")
      (beginning-of-line)
      (message "Your message has %s words." (count-words beg (point))))))

(defun my-dev-layout(path folder)
  (let ((fullpath (concat path "/" folder)))
    (persp-switch folder)
    (split-window-right)
    (split-window-right)
    (balance-windows)
    (dired fullpath)
    (other-window 1)
    (dired fullpath)
    (magit-status fullpath)
    (split-window-below)
    (other-window 1)
    (shell (concat "*" folder "-shell*"))))

(defun my-dev-full-layout(directory)
  (interactive "DEnter a directory: ")
  (let* ((parsed-dir (split-string directory "/"))
         (folder (car (last parsed-dir)))
         (path (string-join (nbutlast parsed-dir) "/")))
    (my-dev-layout path folder)
    (my-full-magit-log directory (concat folder "-log"))))

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

(defun my-full-magit-log(directory short-name)
  (persp-switch short-name)
  (magit-status directory)
  (magit-log-all (append magit-log-arguments '("--date-order") nil))
  (rename-buffer (concat "*" short-name "-git-log*"))
  (delete-other-windows))

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

(defun my-ledger-layout(fullpath)
  (let* ((parsed-dir (split-string fullpath "/"))
         (file (car (last parsed-dir)))
         (folder (car (last parsed-dir 2)))
         (path (string-join (nbutlast parsed-dir) "/")))
    (persp-switch folder)
    (find-file fullpath)
    (ledger-mode)
    (split-window-horizontally)
    ;; TODO: Resize windows?
    (other-window 1)
    (shell (concat "*" folder "-shell*"))))

(defun my-org-agenda-skip-multi(list)
  "Call multiple functions for org-agenda-skip

If a function in LIST returns nil, then continue on to the next function. Otherwise, return that value. Per the usage of org-agenda-skip-function, this means that any function in LIST should return nil if the current headline should not be skipped, and otherwise should return a point where the search should continue (usually `next-headline`).

Since this aborts early, try to place fast-running functions near the beginning!

Example:

(org-agenda-skip-function
 '(my-org-agenda-skip-multi
   '((my-org-agenda-skip-tags '(\"HOME\"))
     (org-agenda-skip-entry-if 'todo \"WAITING\"))))

This will skip all tasks with the tag HOME, and also all tasks with the status of WAITING."
  (catch 'skip-please
    (dolist (element list)
      (let* ((unquoted (car (cdr element)))
             (function (car unquoted))
             (arguments (cdr (car (cdr unquoted)))))
        (let ((ret (apply function arguments)))
        (when ret
          (throw 'skip-please ret)))))))

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

;; FIXME: This doesn't seem to work for tags on a sub-task when parent task is
;; folded. Expanding the parent task fixes the issue.
(defun my-org-agenda-skip-tags(tags &optional others)
  "Skip all entries that correspond to any elements of TAGS.

If OTHERS is true, skip all entries that do not correspond to any elements of TAGS."
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
        (current-headline (or (and (org-at-heading-p)
                                   (point))
                              (save-excursion (org-back-to-heading)))))
    (catch 'skip-please
      (dolist (tag tags)
        (if others
            (if (not (member tag (org-get-tags-at current-headline)))
                (throw 'skip-please next-headline)
              nil)
          (if (member tag (org-get-tags-at current-headline))
              (throw 'skip-please next-headline)
            nil))))))

(defun my-org-any-subheading-has-state(state)
  "Check if any subentries of the current heading have the given state."
  (save-excursion
    (org-down-element)
    (let ((ret nil)
          (continue t))
      (while continue
        (when (string= (nth 2 (org-heading-components)) state)
          (setq ret t)
          (setq continue nil))
        (unless (org-get-next-sibling)
          (setq continue nil)))
      ret)))

(defun my-org-any-subheading-has-any-state(list)
  " Check if any subentries of the current heading have any of the states in
the given list. Pass `org-not-done-keywords` to see if task is open, or pass
`org-done-keywords` to see if task is closed."
  (let (value)
    (catch 'break
      (dolist (element list value)
        (setq value (my-org-any-subheading-has-state element))
        (when value
          (throw 'break t))))))

;; https://www.joelonsoftware.com/2007/10/26/evidence-based-scheduling/
(defun my-org-calc-velocity()
  "Compare the estimated Effort for the current task to the time clocked, calculate the Velocity (effort / actual), and save that value to `Velocity` property."
  (let* ((current-headline (or (and (org-at-heading-p)
                                    (point))
                               (save-excursion (org-back-to-heading))))
         (effort-prop (org-entry-get current-headline "Effort"))
         (effort (org-duration-string-to-minutes
                  (if effort-prop effort-prop 0)))
         (actual (org-clock-sum-current-item))
         (velocity (/ effort actual)))
    (cond ((= actual 0)
           (message "No time clocked, skipping velocity calculation."))
          ((= effort 0)
           (message "No time estimate, skipping velocity calculation."))
          (t
           (org-entry-put current-headline "Velocity"
                          (number-to-string velocity))))))

;; TODO: make this more customizable
(defun my-org-summary-todo ()
  "Switch entry to DONE when all subentries are done."
  (save-excursion
    (org-up-element)
    (when (nth 2 (org-heading-components))
      (let (org-log-done org-log-states)
        (org-todo (if (not (my-org-any-subheading-has-any-state
                            org-not-done-keywords))
                      "DONE"
                    (if (not (my-org-any-subheading-has-any-state
                              org-done-keywords))
                        (if (my-org-any-subheading-has-state "STARTED")
                            "STARTED"
                          "TODO")
                      "STARTED")))))))

(defun my-prev-window()
  (interactive)
  (other-window -1))

(defun my-print-elements-of-list(list)
  (while list
    (print (car list))
    (setq list (cdr list))))

(defun my-set-org-todo(symbol value)
  (set-default symbol value)
  (add-to-list 'org-agenda-files value))

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
\(customize-set-variable 'my-twitter-timelines
                        '(\"(:home)\" \"(#emacs)\" \"(#gnu)\"))
"
  :type '(repeat string)
  :tag "My Twitter Timelines"
  :group 'weldon
  :set 'my-set-twitter-timelines
  )

(defcustom my-org-todo-file
  'nil
  "Full path to file used as todo-list in org-mode.

To modify this variable, you can use the customize interface, or do e.g.:
\(customize-set-variable 'my-org-todo-file \"/path/to/org/todo/file\")
"
  :type '(string)
  :tag "My Org Todo File"
  :group 'weldon
  :set 'my-set-org-todo
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

;; C-x p: Pocket
(global-set-key (kbd "C-x p a") 'pocket-api-add)

;; Prefix of C-c
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'bury-buffer)
(global-set-key (kbd "C-c e") 'eval-region)
(global-set-key (kbd "C-c f") 'auto-formatter-format-buffer)
(global-set-key (kbd "C-c i r c") 'my-irc-actual)
(global-set-key (kbd "C-c o") 'ace-window)
(global-set-key (kbd "C-c s") 'hs-show-block)
(global-set-key (kbd "C-c h") 'hs-hide-block)
(global-set-key (kbd "C-c S") 'hs-show-all)
(global-set-key (kbd "C-c H") 'hs-hide-all)
(global-set-key (kbd "C-c r") 'replace-string)

;; C-c v: OmniSharp
(global-set-key (kbd "C-c v a") 'omnisharp-add-to-solution-current-file)
(global-set-key (kbd "C-c v u") 'omnisharp-fix-usings)
(global-set-key (kbd "C-c v d") 'omnisharp-go-to-definition)
(global-set-key (kbd "C-c v r") 'omnisharp-run-code-action-refactoring)

;; Scrolling
(global-set-key (kbd "<prior>") 'my-smooth-scroll-up)
(global-set-key (kbd "<next>") 'my-smooth-scroll-down)

;; Ido
;;(define-key ido-common-completion-map (kbd "RET") #'my-ido-complete-and-exit)

;; Perspective prefix key
(persp-mode-set-prefix-key (kbd "C-z"))

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
(set-face-attribute 'highlight nil :background "grey10")

(setq whitespace-style '(face tabs trailing lines-tail space-before-tab
                              newline indentation empty space-after-tab
                              tab-mark))
(global-visual-line-mode t)
(setq display-time-24hr-format t)
(column-number-mode 1)
(display-time-mode 1)
(setq battery-mode-line-format " [%b%p%%] ")
;;(global-aggressive-indent-mode)
(auto-dim-other-buffers-mode)
(setq ring-bell-function #'ignore)
(global-git-gutter-mode)
(projectile-global-mode)
(persp-mode 1)

(setq projectile-mode-line '(:eval
                             (if (projectile-project-root)
                                 (format " Projectile[%s]"
                                         (projectile-project-name))
                               "")))

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

;; Ace Window
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(set-face-attribute 'aw-leading-char-face nil :foreground "red" :weight 'normal :height 3.0)

;; Auto-complete
(global-company-mode)
(add-to-list 'company-backends 'company-omnisharp)
(setq company-idle-delay 0
      company-selection-wrap-around t
      company-minimum-prefix-length 1)
;;(add-to-list 'ac-dictionary-directories
;;             (concat my-external-library-location "/auto-complete/dict"))
;;(ac-config-default)
;;(setq ac-ignore-case nil)

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
(editorconfig-mode 1)

;; Doc View
(setq doc-view-resolution 300)

;; Gnus
(setq gnus-save-newsrc-file nil
      gnus-summary-make-false-root nil
      gnus-use-cache t)

;; IRC
(setq erc-kill-server-buffer-on-quit t
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477")
      erc-log-insert-log-on-open nil
      erc-log-channels t
      erc-server-reconnect-timeout 30
      erc-join-buffer 'bury
      erc-server-reconnect-attempts 10)
(erc-autojoin-mode 1)

;; Magit
(add-to-list 'magit-log-arguments "--color")
(setq magit-revert-buffers 'silent
      magit-push-always-verify nil)

;; Navigation
(ido-mode)
(setq ido-separator "\n"
      ido-auto-merge-work-directories-length -1
      ido-default-buffer-method 'selected-window
      ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
                           "*Messages*" "Async Shell Command"))
(flx-ido-mode 1)
(setq magit-completing-read-function 'magit-ido-completing-read)
(setq persp-interactive-completion-function 'ido-completing-read)

;; Org Mode
(setq org-use-fast-todo-selection t
      org-deadline-warning-days 5
      org-agenda-log-mode-items '(closed)
      org-agenda-sticky t
      org-agenda-start-on-weekday nil
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-sorting-strategy '((agenda time-up priority-down tag-up))
      org-log-into-drawer t
      org-log-done 'time
      org-agenda-span 'day
      org-extend-today-until 4
      org-expiry-inactive-timestamps t
      org-clock-persist t
      org-expiry-created-property-name "CREATED"
      org-todo-keyword-faces '(("WAITING" . "yellow")
                               ("DEFERRED" . "purple"))
      org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)"
                                    "APPT(a)" "DEFERRED(f)" "|" "DONE(d)"
                                    "CANCELLED(c)" "UNREQUIRED(u)"))
      org-stuck-projects '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))
(add-hook 'org-after-todo-state-change-hook 'my-org-summary-todo)
(org-clock-persistence-insinuate)
(org-super-agenda-mode 1)

;; Pocket
(pocket-api-load-auth)

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
(add-to-list 'web-mode-indentation-params '("lineup-quotes" . nil))
(setq web-mode-script-padding 2
      web-mode-style-padding 2
      web-mode-block-padding 2
      web-mode-enable-auto-closing t)

;; Scrolling
(setq scroll-preserve-screen-position 'always)

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

;; Docker
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; EditorConfig
(add-to-list 'auto-mode-alist '("\\.editorconfig$" . conf-unix-mode))

;; Ledger
(add-to-list 'auto-mode-alist '("\\.dat\\'" . ledger-mode))
(add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Org Trello
(add-to-list 'auto-mode-alist '("\\.trello\\.org\\'" . (lambda ()
                                                         (org-mode)
                                                         (org-trello-mode))))

;; Ruby
(add-to-list 'auto-mode-alist '("\\Capfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Cheffile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Gemfile\\'" . ruby-mode))

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
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

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

;; C-Sharp
(add-hook 'csharp-mode-hook
          (lambda()
            ;;(auto-complete-mode 1)
            (omnisharp-mode 1)
            (hs-minor-mode 1)))

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
;;   Doesn't automatically sync dirs. (Workaround by pressing `M-RET`.)
;;     WIP fix below
;;
;; For testing:
;; (setq dired-after-readin-hook nil)
(add-hook 'dired-after-readin-hook (lambda()
                                     (unless (get-buffer "*sync-shell*")
                                       (shell "*sync-shell*"))
                                     (process-send-string
                                      (get-buffer "*sync-shell*")
                                      (format "cd %s\n" default-directory))
                                     ;; (with-current-buffer "*sync-shell*"
                                     ;;   (goto-char (point-max))
                                     ;;   (shell-resync-dirs))
                                     (message "Switched to new directory")))

(setq dired-dwim-target t)

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

;; JavaScript
(add-hook 'js-mode-hook 'hs-minor-mode)

;; Markdown
(add-hook 'markdown-mode-hook (lambda ()
                                (flyspell-mode 1)))

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
            (whitespace-mode t)
            (hs-minor-mode t)
            (show-paren-mode t)
            (local-set-key (kbd "RET") 'newline-and-indent)))

;; XML
(setq nxml-slash-auto-complete-flag t)

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

(defadvice shell-command (after shell-in-new-buffer
                                (command &optional output-buffer error-buffer))
  (when (get-buffer "*Async Shell Command*")
    (with-current-buffer "*Async Shell Command*"
      (rename-uniquely))))

(ad-activate 'shell-command)

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
