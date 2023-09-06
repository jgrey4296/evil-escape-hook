 ;;; evil-escape.el --- Escape from anything with a customizable key sequence -*- lexical-binding: t; -*-

;; Forked and Refactored 2023 by John Grey

;; Original:
;; Copyright (C) 2014-2015 syl20bnr
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: convenience editing evil
;; Created: 22 Oct 2014
;; Version: 3.15
;; Package-Requires: ((emacs "24") (evil "1.0.9") (cl-lib "0.5"))
;; URL: https://github.com/syl20bnr/evil-escape

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Press `fd` quickly to:
;; ----------------------

;;   - escape from all stock evil states to normal state
;;   - escape from evil-lisp-state to normal state
;;   - escape from evil-iedit-state to normal state
;;   - abort evil ex command
;;   - quit minibuffer
;;   - quit compilation buffers
;;   - abort isearch
;;   - quit ibuffer
;;   - quit image buffer
;;   - quit magit buffers
;;   - quit help buffers
;;   - quit apropos buffers
;;   - quit ert buffers
;;   - quit undo-tree buffer
;;   - quit paradox
;;   - quit gist-list menu
;;   - quit helm-ag-edit
;;   - hide neotree buffer
;;   - quit evil-multiedit
;; And more to come !

;; Configuration:
;; --------------

;; The key sequence can be customized with the variable
;; `evil-escape-key-sequence'.

;; The delay between the two key presses can be customized with
;; the variable `evil-escape-delay'. Default is `0.1'.

;; The key sequence can be entered in any order by setting
;; the variable `evil-escape-unordered-key-sequence' to non nil.

;; A major mode can be excluded by adding it to the list
;; `evil-escape-excluded-major-modes'.

;; An inclusive list of major modes can defined with the variable
;; `evil-escape-enable-only-for-major-modes'. When this list is
;; non-nil then evil-escape is enabled only for the major-modes
;; in the list.

;; A list of zero arity functions can be defined with the variable
;; `evil-escape-inhibit-functions', if any of these functions return
;; non nil then evil-escape is inhibited.
;; It is also possible to inhibit evil-escape in a let binding by
;; setting the `evil-escape-inhibit' variable to non nil.

;; It is possible to bind `evil-escape' function directly, for
;; instance to execute evil-escape with `C-c C-g':

;; (global-set-key (kbd "C-c C-g") 'evil-escape)

;; More information in the readme of the repository:
;; https://github.com/syl20bnr/evil-escape

;;; Code:

(require 'evil)
(require 'cl-lib)
(require 'ring)

(eval-when-compile
  (declare-function evil-iedit-state/quit-iedit-mode "evil-iedit-state.el"))

(defgroup evil-escape nil
  "Key sequence to escape insert state and everything else."
  :prefix "evil-escape-"
  :group 'evil)

(defcustom evil-escape-key-sequence (kbd "fd")
  "Two keys sequence to escape from insert state."
  :type 'key-sequence
  :group 'evil-escape)

(defcustom evil-escape-lighter '(concat " " evil-escape-key-sequence)
  "The lighter for the evil escape mode."
  :type 'sexp
  :group 'evil-escape)

(defcustom evil-escape-delay 0.1
  "Max time delay between two key presses."
  :type 'number
  :group 'evil-escape)

(defcustom evil-escape-excluded-major-modes nil
  "Excluded major modes where escape sequences have no effect."
  :type 'sexp
  :group 'evil-escape)

(defcustom evil-escape-excluded-states nil
  "Excluded states where escape sequences have no effect."
  :type 'sexp
  :group 'evil-escape)

(defcustom evil-escape-enable-only-for-major-modes nil
  "List of major modes where evil-escape is enabled."
  :type 'sexp
  :group 'evil-escape)

(defcustom evil-escape-inhibit-functions nil
  "List of zero argument predicate functions disabling evil-escape.
 If any of these functions return non nil, evil escape will be inhibited."
  :type 'sexp
  :group 'evil-escape)

(defcustom evil-escape-hook nil "functions to run on escaping")

(defcustom evil-escape-delete-char-on-fns '(self-insert-command org-self-insert-command) "Function symbols to delete the previous inserted char. usually self-insert-command and org-self-insert-command")

(defvar evil-escape-inhibit nil
  "When non nil evil-escape is inhibited.")

(defvar evil-escape-ring (make-ring 2))

(defvar evil-escape-last-time (float-time))

(defvar evil-escape-trigger-passed nil)

(defvar evil-escape--key1 nil)

(defvar evil-escape--key2 nil)

;;;###autoload
(define-minor-mode evil-escape-mode
  "Buffer-local minor mode to escape insert state and everything else
with a key sequence."
  :lighter (:eval evil-escape-lighter)
  :group 'evil
  :global t
  (if evil-escape-mode
      (progn
        (setq evil-escape--key1 (elt evil-escape-key-sequence 0)
              evil-escape--key2 (elt evil-escape-key-sequence 1)
              )
        (add-hook 'pre-command-hook #'evil-escape-pre-command-hook)
        (add-hook 'post-command-hook #'evil-escape-post-command-hook-plus))
    (remove-hook 'pre-command-hook #'evil-escape-pre-command-hook)
    (remove-hook 'post-command-hook #'evil-escape-post-command-hook-plus)))

(defun evil-escape ()
  "Escape from everything... well almost everything."
  (interactive)
  (call-interactively (evil-escape--get-appropriate-func)))

(defun evil-escape--get-appropriate-func ()
  "Return the function to escape from everything."
  (pcase evil-state
    (`normal (evil-escape--escape-normal-state))
    (`motion (evil-escape--escape-motion-state))
    (`insert 'evil-normal-state)
    (`emacs (evil-escape--escape-emacs-state))
    (`hybrid (evil-escape--escape-emacs-state))
    (`evilified (evil-escape--escape-emacs-state))
    (`visual 'evil-exit-visual-state)
    (`replace 'evil-normal-state)
    (`lisp 'evil-lisp-state/quit)
    (`iedit 'evil-iedit-state/quit-iedit-mode)
    (`iedit-insert 'evil-iedit-state/quit-iedit-mode)
    (`multiedit 'evil-multiedit-abort)
    (`multiedit-insert 'evil-multiedit-state)
    (_ (evil-escape--escape-normal-state))))

(defun evil-escape-pre-command-hook ()
  " add to pre-command-hook to listen for keyboard events,
and intercept them if they match the evil-escape-key-sequence "
  (with-demoted-errors "evil-escape: Error %S"
    (evil-escape-update-state)
    (when (and evil-escape-trigger-passed (evil-escape-p))
      (let ((inhibit-redisplay nil)
            (fontification-functions nil)
            (esc-fun (evil-escape--get-appropriate-func)))
        (evil-repeat-stop)
        (when esc-fun ;; override the command
          (when (and (memq this-command evil-escape-delete-char-on-fns)
                     (not buffer-read-only))
            (delete-char -1))
          (setq this-command esc-fun
                this-original-command esc-fun))))
        ))

(defun evil-escape-update-state ()
  " add events to the escape ring, record the time the last matching event happened,
then set the flag for whether the condition has been met"
  (ring-insert evil-escape-ring last-input-event)
  (when (eq (ring-ref evil-escape-ring 0) evil-escape--key1)
    (setq evil-escape-last-time (float-time)))
  (setq evil-escape-trigger-passed (and (eq (ring-ref evil-escape-ring 0) evil-escape--key2)
                                        (eq (ring-ref evil-escape-ring 1) evil-escape--key1)
                                        (<= (- (float-time) evil-escape-last-time) evil-escape-delay)
                                        )
        )
  )

(defun evil-escape-post-command-hook-plus ()
  " add to post-command-hook as necessary "
  (when evil-escape-trigger-passed
    (run-hooks 'evil-escape-hook)
    )
  )

(defadvice evil-repeat (around evil-escape-repeat-info activate)
  (let ((evil-escape-inhibit t))
    ad-do-it))

(defun evil-escape-p ()
  "Return non-nil if evil-escape can run."
  (and evil-escape-key-sequence
       (not evil-escape-inhibit)
       (or (window-minibuffer-p)
           (bound-and-true-p isearch-mode)
           (memq major-mode '(ibuffer-mode
                              image-mode))
           (evil-escape--is-magit-buffer)
           (and (fboundp 'helm-alive-p) (helm-alive-p))
           (or (not (eq 'motion evil-state))
               (not (eq 'evil-force-normal-state
                        (lookup-key evil-normal-state-map [escape])))))
       (not (memq major-mode evil-escape-excluded-major-modes))
       (not (memq evil-state evil-escape-excluded-states))
       (or (not evil-escape-enable-only-for-major-modes)
           (memq major-mode evil-escape-enable-only-for-major-modes))
       (not (cl-reduce (lambda (x y) (or x y))
                       (mapcar 'funcall evil-escape-inhibit-functions)
                       :initial-value nil))))

;; TODO : refactor these to just be in the hook

(defun evil-escape--escape-normal-state ()
  "Return the function to escape from normal state."
  (cond
   ((and (fboundp 'helm-alive-p) (helm-alive-p)) 'helm-keyboard-quit)
   ((eq 'ibuffer-mode major-mode) 'ibuffer-quit)
   ((eq 'image-mode major-mode) 'quit-window)
   ((evil-escape--is-magit-buffer) 'evil-escape--escape-with-q)
   ((bound-and-true-p isearch-mode) 'isearch-abort)
   ((window-minibuffer-p) 'abort-recursive-edit)
   (t (lookup-key evil-normal-state-map [escape]))))

(defun evil-escape--escape-motion-state ()
  "Return the function to escape from motion state."
  (cond
   ((or (memq major-mode '(apropos-mode
                           help-mode
                           ert-results-mode
                           ert-simple-view-mode
                           compilation-mode
                           image-mode))) 'quit-window)
   ((eq 'undo-tree-visualizer-mode major-mode) 'undo-tree-visualizer-quit)
   ((and (fboundp 'helm-ag--edit-abort)
         (string-equal "*helm-ag-edit*" (buffer-name))) 'helm-ag--edit-abort)
   ((eq 'neotree-mode major-mode) 'neotree-hide)
   (t 'evil-normal-state)))

(defun evil-escape--escape-emacs-state ()
  "Return the function to escape from emacs state."
  (cond
   ((bound-and-true-p isearch-mode) 'isearch-abort)
   ((window-minibuffer-p) 'abort-recursive-edit)
   ((evil-escape--is-magit-buffer) 'evil-escape--escape-with-q)
   ((eq 'ibuffer-mode major-mode) 'ibuffer-quit)
   ((eq 'emoji-cheat-sheet-plus-buffer-mode major-mode) 'kill-this-buffer)
   ((eq 'paradox-menu-mode major-mode) 'evil-escape--escape-with-q)
   ((memq major-mode '(gist-list-menu-mode
                       image-mode)) 'quit-window)
   (t 'evil-normal-state)))

(defun evil-escape--escape-with-q ()
  "Send `q' key press event to exit from a buffer."
  (interactive)
  (setq unread-command-events (listify-key-sequence "q")))

(defun evil-escape--is-magit-buffer ()
  "Return non nil if the current buffer is a Magit buffer."
  (string-match-p "magit" (symbol-name major-mode)))

(defun evil-escape-check-ring ()
  "for debugging"
  (interactive)
  (message "evil escape ring: |%s| %s : %s : %s"
           (string-join (mapcar (lambda (x) (format "%c" x)) (ring-elements evil-escape-ring)) "")
           (this-command-keys-vector)
           this-command
           last-command
           )
  )

(provide 'evil-escape)

;;; evil-escape.el ends here
