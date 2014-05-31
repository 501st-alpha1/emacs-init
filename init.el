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

(require 'chess)
(require 'editorconfig)
(require 'emms-setup)
(require 'erc-chess) ;; TODO find git repo
(require 'erc-join)
(require 'erc-log)
(require 'erc-match)
(require 'facebook)
(require 'fic-mode) ;; TODO find git repo
(require 'magit-gitflow)
(require 'magit)
(require 'markdown-mode)
(require 'notify) ;; TODO find git repo
(require 'org-feed)
;(require 'php-mode)
(require 'python)
(require 'simple-rtm)
(require 'tls)
(require 'twittering-mode)
(require 'web-mode)

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
;(global-set-key (kbd "SPC") 'my-insert-space-or-newline-and-indent)

;;----------------------------------------------------------------------------;;
;;                             Global Config                                  ;;
;;----------------------------------------------------------------------------;;
;; Various global settings, including mode line config.

(global-visual-line-mode t)
(setq display-time-24hr-format t)
(column-number-mode 1)
(display-time-mode 1)
(display-battery-mode 1)
(setq battery-mode-line-format " [%b%p%%] ")

;; Auto Backup Files
(setq backup-directory-alist '(("." . "~/.saves"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Code Formatting
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 2)
(setq c-basic-indent 2)
(setq-default fill-column 70)

;; Navigation
(ido-mode)

;;----------------------------------------------------------------------------;;
;;                             Auto-Mode-Alist                                ;;
;;----------------------------------------------------------------------------;;

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

;;----------------------------------------------------------------------------;;
;;                              Mode Hooks                                    ;;
;;----------------------------------------------------------------------------;;
;; Various mode hooks

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
