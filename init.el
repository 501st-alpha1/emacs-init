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
;;                          Custom Load Path                                  ;;
;;----------------------------------------------------------------------------;;
;; Load path for various external libraries, managed with git.

;TODO: throw error if path not defined

(add-to-list 'load-path external-library-location)

(let* ((my-lisp-dir external-library-location)
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
(require 'auto-complete-config)
(require 'battery)
(require 'chess)
(require 'editorconfig)
(require 'emms-setup)
(require 'erc-chess) ;; TODO find git repo
(require 'erc-join)
(require 'erc-log)
(require 'erc-match)
(require 'facebook)
(require 'fic-mode) ;; TODO find git repo
(require 'inf-ruby)
(require 'magit-gitflow)
(require 'magit)
(require 'markdown-mode)
(require 'notify) ;; TODO find git repo
(require 'org-feed)
;(require 'php-mode)
(require 'python)
(require 'ruby-electric)
(require 'simple-rtm)
(require 'tls)
(require 'twittering-mode)
(require 'web-mode)
(require 'whitespace)

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

(defun my-kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

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
;(global-set-key (kbd "SPC") 'my-insert-space-or-newline-and-indent)

;;----------------------------------------------------------------------------;;
;;                             Global Config                                  ;;
;;----------------------------------------------------------------------------;;
;; Various global settings, including mode line config.

(global-whitespace-mode t)
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
             (concat external-library-location "/auto-complete/dict"))
(ac-config-default)

;; Code Formatting
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 2)
(setq c-basic-indent 2)
(setq-default fill-column 70)
(setq ruby-indent-size 2)

;; Navigation
(ido-mode)
(setq ido-separator "\n"
      ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
                           "*Messages*" "Async Shell Command"))

;; Web mode
(set-face-attribute 'web-mode-html-tag-face nil :foreground "Blue")
(set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "Blue")

;; Twitter
(setq twittering-use-master-password t
      twittering-icon-mode t)

;;----------------------------------------------------------------------------;;
;;                             Auto-Mode-Alist                                ;;
;;----------------------------------------------------------------------------;;

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Web
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
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

;; Syntax highlighting for diffs
(add-hook 'diff-mode-hook
          (lambda()
            (set-face-foreground 'diff-removed "red")
            (set-face-foreground 'diff-added "green")))

;; More indentation
(add-hook 'c-mode-common-hook
          (lambda()
            (c-set-offset 'case-label '2)))

;; Git
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;; Custom *scratch*
(add-hook 'emacs-startup-hook
          (lambda ()
            (interactive)
            (kill-buffer "*scratch*")
            (find-file (concat external-library-location "/.scratch"))
            (rename-buffer "*scratch*")
            (lisp-interaction-mode)
            (bury-buffer "*scratch*")
            (cd "~")))

;; Ruby
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))

;; Web mode
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-code-indent-offset 2)
            (local-set-key (kbd "RET") 'newline-and-indent)))
