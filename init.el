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
;; This ends the introduction. The example code follows (in correct order):   ;;
;;----------------------------------------------------------------------------;;
;; Windows specific:                                                          ;;
;; (setq notify-method 'notify-via-growl)                                     ;;
;;----------------------------------------------------------------------------;;
;; Cross platform:                                                            ;;
;; (setq my-external-library-location "/path/to/git/folders")                 ;;
;; (setq my-twitter-timelines '(":home" "#emacs" "#ResetTheNet"))             ;;
;;                                                                            ;;
;; (load-file "/path/to/this/file")                                           ;;
;;                                                                            ;;
;; Or, more simply:                                                           ;;
;; (load-file (concat my-external-library-location "/emacs/init.el"))         ;;
;;----------------------------------------------------------------------------;;


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
(require 'alist) ;; Required by elscreen-buffer-list...
(require 'auto-complete-config)
(require 'battery)
(require 'chess)
(require 'csharp-mode) ;; TODO find git repo
(require 'csv-mode) ;; TODO find git repo
(require 'deft)
(require 'editorconfig)
(require 'elscreen)
(require 'elscreen-buffer-list)
(require 'emms-setup)
(require 'erc-chess) ;; TODO find git repo
(require 'erc-join)
(require 'erc-log)
(require 'erc-match)
(require 'facebook)
(require 'fic-mode)
(require 'flymake)
(require 'hideshow)
(require 'inf-ruby)
(require 'magit-gitflow)
(require 'magit)
(require 'markdown-mode)
(require 'mew)
(require 'notify) ;; TODO find git repo
(require 'org-feed)
(require 'python)
(require 'ruby-electric)
(require 'simple-rtm)
(require 'tls)
(require 'twittering-mode)
(require 'web-mode)
(require 'whitespace)

;;----------------------------------------------------------------------------;;
;;                           Custom Variables                                 ;;
;;----------------------------------------------------------------------------;;

(defvar my-eshell-command-count 0 "Variable to keep track of command count")
(make-variable-buffer-local 'my-eshell-command-count)

;;----------------------------------------------------------------------------;;
;;                             Functions                                      ;;
;;----------------------------------------------------------------------------;;
;; Mostly designed to be called directly (or have a key-combo bound to.
;; All begin with prefix "my".

(defun my-prev-window()
  (interactive)
  (other-window -1))

(defun my-kill-buffer()
  (interactive)
  (kill-buffer)
  (other-window 1))

(defun my-insert-space-or-newline-and-indent()
  (interactive)
  (if (>= (current-column) fill-column)
      (newline-and-indent)
    (insert-char ? )))

;; Open the window all over that screen.
(defun my-all-over-the-screen (&number)
  (interactive)
  (delete-other-windows)
  (let ((splits (or &number 3)))
    (dotimes (i (- splits 1))
      (split-window-horizontally)))
  (balance-windows)
  (follow-mode t))

;; TODO: kill only sub-dirs of given dir?
(defun my-kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

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

(defun my-increment-eshell-command-count ()
  "Increments the eshell command count var."
  (incf my-eshell-command-count))

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

;;----------------------------------------------------------------------------;;
;;                          Keyboard Shortcuts                                ;;
;;----------------------------------------------------------------------------;;
;; Tried to keep from conflicting with defaults, but no guarantees.
;; Did not use standard C-c prefix.

(global-set-key (kbd "C-x K") 'my-kill-buffer)
(global-set-key (kbd "C-x O") 'my-prev-window)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c b") 'bury-buffer)
(global-set-key (kbd "C-c e") 'eval-region)
(global-set-key (kbd "C-c r t m") 'simple-rtm-mode)
(global-set-key (kbd "C-c s") 'hs-show-block)
(global-set-key (kbd "C-c h") 'hs-hide-block)
(global-set-key (kbd "C-c S") 'hs-show-all)
(global-set-key (kbd "C-c H") 'hs-hide-all)
(global-set-key (kbd "<prior>") 'my-smooth-scroll-up)
(global-set-key (kbd "<next>") 'my-smooth-scroll-down)

;;----------------------------------------------------------------------------;;
;;                             Global Config                                  ;;
;;----------------------------------------------------------------------------;;
;; Various global settings, including mode line config.

;(global-whitespace-mode t)
(setq whitespace-style '(face tabs trailing lines space-before-tab
                              newline indentation empty space-after-tab
                              tab-mark))
(global-visual-line-mode t)
(setq display-time-24hr-format t)
(column-number-mode 1)
(display-time-mode 1)
(setq battery-mode-line-format " [%b%p%%] ")

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

;; Code Formatting
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 2)
(setq c-basic-indent 2)
(setq lisp-basic-indent 2)
(setq lisp-basic-offset 2)
(setq-default fill-column 70)
(setq ruby-indent-size 2)
(setq python-indent-offset 2)

;; IRC
(setq erc-kill-server-buffer-on-quit t
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477")
      erc-log-channels-directory "~/.erc/logs/"
      erc-log-insert-log-on-open nil
      erc-log-channels t
      erc-server-reconnect-timeout 30
      erc-server-reconnect-attempts 10)
(erc-autojoin-mode 1)

;; Navigation
(ido-mode)
(setq ido-separator "\n"
      ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
                           "*Messages*" "Async Shell Command"))

;; Web mode
(set-face-attribute 'web-mode-whitespace-face nil :background "red")
(set-face-attribute 'web-mode-html-tag-face nil :foreground "Blue")
(set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "Blue")
(setq web-mode-extra-auto-pairs
      '(("erb" . (("open" "close")))
        ("php" . (("open" "close")
                  ("open" "close")))))
(setq web-mode-engines-alist
      '(("php"   . "\\.phtml\\'")
        ("blade" . "\\.blade\\.")
        ("erb"   . "\\.erb\\'")))

;; Twitter
(setq twittering-use-master-password t
      twittering-icon-mode t
      twittering-initial-timeline-spec-string my-twitter-timelines)

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

;; Elscreen
(setq elscreen-display-tab nil
      elscreen-buffer-list-enabled t)

;;----------------------------------------------------------------------------;;
;;                             Auto-Mode-Alist                                ;;
;;----------------------------------------------------------------------------;;

;; EditorConfig
(add-to-list 'auto-mode-alist '("\\.editorconfig$" . conf-unix-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

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

;; Elscreen
(add-hook 'emacs-startup-hook 'elscreen-start)

;; Eshell
(add-hook 'eshell-after-prompt-hook 'my-increment-eshell-command-count)

;; Git
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

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
                  ;web-mode-enable-whitespaces t
                  ;web-mode-whitespaces-regexp "(^[\t]+)|([\t ]+$)"
                  web-mode-display-table nil)
            (local-set-key (kbd "RET") 'newline-and-indent)))
