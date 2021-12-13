;;; org-present.el --- Minimalist presentation minor-mode for Emacs org-mode.
;;
;; Copyright (C) 2012 by Ric Lister
;;
;; Author: Ric Lister
;; Package-Requires: ((org "7"))
;; URL: https://github.com/rlister/org-present
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.
;;
;;; Commentary:
;;
;; This is meant to be an extremely minimalist presentation tool for
;; Emacs org-mode.
;;
;; Usage:
;;
;; Add the following to your emacs config:
;;
;;   (add-to-list 'load-path "~/path/to/org-present")
;;   (autoload 'org-present "org-present" nil t)
;;
;;   (add-hook 'org-present-mode-hook
;;             (lambda ()
;;               (org-present-big)
;;               (org-display-inline-images)))
;;
;;   (add-hook 'org-present-mode-quit-hook
;;             (lambda ()
;;               (org-present-small)
;;               (org-remove-inline-images)))
;;
;; Open an org-mode file with each slide under a top-level heading.
;; Start org-present with org-present-mode, left and right keys will move forward
;; and backward through slides. C-c C-q will quit org-present.
;;
;; This works well with hide-mode-line (http://webonastick.com/emacs-lisp/hide-mode-line.el),
;; which hides the mode-line when only one frame and buffer are open.
;;
;; If you're on a Mac you might also want to look at the fullscreen patch here:
;; http://cloud.github.com/downloads/typester/emacs/feature-fullscreen.patch

(defvar org-present-mode-keymap (make-keymap) "org-present-mode keymap.")

;; left and right page keys
(define-key org-present-mode-keymap [right]         'org-present-next)
(define-key org-present-mode-keymap [left]          'org-present-prev)
(define-key org-present-mode-keymap (kbd "C-c C-=") 'org-present-big)
(define-key org-present-mode-keymap (kbd "C-c C--") 'org-present-small)
(define-key org-present-mode-keymap (kbd "C-c C-q") 'org-present-quit)
(define-key org-present-mode-keymap (kbd "C-c C-r") 'org-present-read-only)
(define-key org-present-mode-keymap (kbd "C-c C-w") 'org-present-read-write)
(define-key org-present-mode-keymap (kbd "C-c <")   'org-present-beginning)
(define-key org-present-mode-keymap (kbd "C-c >")   'org-present-end)

;; how much to scale up font size
(defvar org-present-text-scale 5)
(defvar org-present-cursor-cache (or cursor-type nil)
  "Holds the user set value of cursor for `org-present-read-only'")
(defvar org-present-overlays-list nil)

(define-minor-mode org-present-mode
  "Minimalist presentation minor mode for org-mode."
  :init-value nil
  :lighter " OP"
  :keymap org-present-mode-keymap)

(make-variable-buffer-local 'org-present-mode)

(defun org-present-top ()
  "Jump to current top-level heading, should be safe outside a heading."
  (unless (org-at-heading-p) (outline-previous-heading))
  (let ((level (org-current-level)))
    (when (and level (> level 1))
      (outline-up-heading (- level 1) t))))

(defun org-present-next ()
  "Jump to next top-level heading."
  (interactive)
  (widen)
  (if (org-current-level) ;inside any heading
      (progn
        (org-present-top)
        (or
         (org-get-next-sibling) ;next top-level heading
         (org-present-top)))    ;if that was last, go back to top before narrow
    ;; else handle title page before first heading
    (outline-next-heading))
  (org-present-narrow)
  (org-present-run-after-navigate-functions))

(defun org-present-prev ()
  "Jump to previous top-level heading."
  (interactive)
  (if (org-current-level)
      (progn
        (widen)
        (org-present-top)
        (org-get-last-sibling)))
  (org-present-narrow)
  (org-present-run-after-navigate-functions))

(defun org-present-narrow ()
  "Show just current page; in a heading we narrow, else show title page (before first heading)."
  (if (org-current-level)
      (progn
        (org-narrow-to-subtree)
        (show-all))
    ;; else narrow to area before first heading
    (outline-next-heading)
    (narrow-to-region (point-min) (point))
    (goto-char (point-min))))

(defun org-present-beginning ()
  "Jump to first slide of presentation."
  (interactive)
  (widen)
  (beginning-of-buffer)
  (org-present-narrow)
  (org-present-run-after-navigate-functions))

(defun org-present-end ()
  "Jump to last slide of presentation."
  (interactive)
  (widen)
  (end-of-buffer)
  (org-present-top)
  (org-present-narrow)
  (org-present-run-after-navigate-functions))

(defun org-present-big ()
  "Make font size larger."
  (interactive)
  (text-scale-increase 0)
  (text-scale-increase org-present-text-scale)) ;MAKE THIS BUFFER-LOCAL

(defun org-present-small ()
  "Change font size back to original."
  (interactive)
  (text-scale-increase 0))

(defun org-present-add-overlay (beginning end)
  "Create a single overlay over given region and remember it."
  (let ((overlay (make-overlay beginning end)))
    (push overlay org-present-overlays-list)
    (overlay-put overlay 'invisible 'org-present)))

(defun org-present-show-option (string)
  "Returns non-nil if string is an org-mode exporter option whose value we want to show."
  (save-match-data
    (string-match
     (regexp-opt '("title:" "author:" "date:" "email:"))
     string)))

(defun org-present-add-overlays ()
  "Add overlays for this mode."
  (add-to-invisibility-spec '(org-present))
  (save-excursion
    ;; hide org-mode options starting with #+
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*\\(#\\+\\)\\([^[:space:]]+\\).*" nil t)
      (let ((end (if (org-present-show-option (match-string 2)) 2 0)))
        (org-present-add-overlay (match-beginning 1) (match-end end))))
    ;; hide stars in headings
    (goto-char (point-min))
    (while (re-search-forward "^\\(*+\\)" nil t)
      (org-present-add-overlay (match-beginning 1) (match-end 1)))
    ;; hide emphasis/verbatim markers if not already hidden by org
    (if org-hide-emphasis-markers nil
      ;; TODO https://github.com/rlister/org-present/issues/12
      ;; It would be better to reuse org's own facility for this, if possible.
      ;; However it is not obvious how to do this.
      (progn
        ;; hide emphasis markers
        (goto-char (point-min))
        (while (re-search-forward org-emph-re nil t)
          (org-present-add-overlay (match-beginning 2) (1+ (match-beginning 2)))
          (org-present-add-overlay (1- (match-end 2)) (match-end 2)))
        ;; hide verbatim markers
        (goto-char (point-min))
        (while (re-search-forward org-verbatim-re nil t)
          (org-present-add-overlay (match-beginning 2) (1+ (match-beginning 2)))
          (org-present-add-overlay (1- (match-end 2)) (match-end 2)))))))

(defun org-present-rm-overlays ()
  "Remove overlays for this mode."
  (mapc 'delete-overlay org-present-overlays-list)
  (remove-from-invisibility-spec '(org-present)))

(defun org-present-read-only ()
  "Make buffer read-only."
  (interactive)
  (setq buffer-read-only t)
  (setq org-present-cursor-cache cursor-type
        cursor-type nil)
  (define-key org-present-mode-keymap (kbd "SPC") 'org-present-next))

(defun org-present-read-write ()
  "Make buffer read-only."
  (interactive)
  (setq buffer-read-only nil)
  (setq cursor-type org-present-cursor-cache)
  (define-key org-present-mode-keymap (kbd "SPC") 'self-insert-command))

(defun org-present-hide-cursor ()
  "Hide the cursor for current window."
  (interactive)
  (blink-cursor-suspend)  
  (internal-show-cursor (selected-window) nil))

(defun org-present-show-cursor ()
  "Show the cursor for current window."
  (interactive)
  (blink-cursor--start-timer)
  (internal-show-cursor (selected-window) t))


;;;###autoload
(defun org-present ()
  "init."
  (interactive)
  (setq org-present-mode t)
  (org-present-navigation-modeline)
  (org-present-add-overlays)
  (run-hooks 'org-present-mode-hook)
  (org-present-narrow)
  (org-present-run-after-navigate-functions)
  (org-present-hide-cursor)
  (org-present-hide-bars))


(defun org-present-quit ()
  "Quit the minor-mode."
  (interactive)
  (org-present-small)
  (org-present-rm-overlays)
  (widen)
  ;; Exit from read-only mode before exiting the minor mode
  (when buffer-read-only
    (org-present-read-write))
  (run-hooks 'org-present-mode-quit-hook)
  (org-present-show-cursor)
  (org-present-show-bars)
  (org-present-restore-modeline)
  (setq org-present-mode nil))

(defvar org-present-after-navigate-functions nil
  "Abnormal hook run after org-present navigates to a new heading.")

;; courtesy Xah Lee ( http://ergoemacs.org/emacs/modernization_elisp_lib_problem.html )
(defun org-present-trim-string (string)
  "Remove whitespace (space, tab, emacs newline (LF, ASCII 10)) in beginning and ending of STRING."
  (replace-regexp-in-string
   "\\`[ \t\n]*" ""
   (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun org-present-run-after-navigate-functions ()
  "Run org-present-after-navigate hook, passing the name of the presentation buffer and the current heading."
  (let* ((title-text (thing-at-point 'line))
         (safe-title-text (replace-regexp-in-string "^[ \*]" "" title-text))
         (current-heading (org-present-trim-string safe-title-text)))
    (run-hook-with-args 'org-present-after-navigate-functions (buffer-name) current-heading)))

(defun org-present-headlines ()
  (mapcar (lambda (x) (plist-get (cadr x) :raw-value)) (cddr (org-element-parse-buffer 'headline))))


(defun org-present-navigation-modeline ()
  "Create a navigation headline with org headings of level 1."
  ;; (setq org-present-nav-header (org-present-headlines))
  (setq org-present-mode-line mode-line-format)
  (setq mode-line-format (string-join (append '("  ") (org-present-headlines) '("  ")) "  #|#  ")))

(defun org-present-restore-modeline ()
  "Restore the original modeline format"
  (interactive)
  (setq mode-line-format org-present-mode-line))

(defun org-present-hide-bars ()
  (setq-local org-present-scroll-bar (symbol-value 'scroll-bar-mode))
  (setq-local org-present-tool-bar (symbol-value 'tool-bar-mode))
  (setq-local org-present-menu-bar (symbol-value 'menu-bar-mode))
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1))

(defun org-present-show-bars ()
  (if org-present-scroll-bar
	  (scroll-bar-mode org-present-scroll-bar))
  (if org-present-tool-bar
	  (tool-bar-mode org-present-tool-bar))
  (if org-present-menu-bar
	  (menu-bar-mode org-present-menu-bar)))

;; (defun org-present-fullscreen ()
;; 	(setq-local org-present-fullscreen-restore
;; 	  (frame-parameter (window-frame) 'fullscreen))
;; 	(if (not (symbol-value org-present-fullscreen-restore))
;; 		(toggle-frame-fullscreen)))

;; (defun org-present-fullscreen-restore ()
;;   (if (not (symbol-value org-present-fullscreen-restore))
;; 	  (toggle-frame-fullscreen)))

;; (defun org-present-on ()
;;   (interactive)
;;   (org-present-fullscreen)
;;   (org-present-hide-bars))

;; (defun org-present-off ()
;;   (interactive)
;;   (org-present-fullscreen-restore)
;;   (org-present-restore-bars))

(provide 'org-present)
;;; org-present.el ends here

;;
;; USE HEADER-LINE AS NAVIGATION OVERVIEW
;; ALSO YOU CAN SET THE TEXT PROPERTIES,
;; SEE BELOW

(let ((s "Topic 1 #||# Topic 2 #||# Topic"))
  (put-text-property 0 7 'face 'bold s)
  (add-face-text-property 0 7 '(:foreground "white") nil s)
  (add-face-text-property 0 (length s) '(:height 170) nil s)
  (setq header-line-format s))
