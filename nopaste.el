;;; nopaste.el --- interface to nopaste.pl

;; Copyright (C) 2007, 2010 Ævar Arnfjörð Bjarmason

;; Author: Ævar Arnfjörð Bjarmason <avar@cpan.org>
;; Created: 2007-11-22
;; Keywords: comm

;; This file is not a part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Interfaces with paste bots via the `nopaste' which is part of the
;; `App::Nopaste' CPAN distribution found at
;; http://github.com/sartak/app-nopaste

;; Example usage with a custom path and nick:

;; (require 'nopaste)
;; (setq nopaste-nick "avar")

;;; Code:


;; Public variables
(defvar nopaste-nickname ""
  "The nick given to nopaste")
(defvar nopaste-description ""
  "The description given to nopaste")
(defvar nopaste-channel ""
  "The channel given to nopaste")
(defvar nopaste-language ""
  "The the language given to nopaste")
(defvar nopaste-service ""
  "The nopaste service to use. This can also be set through the
  NOPASTE_SERVICES environmental variable to be read by nopaste
  itself.")

(defvar nopaste-command "nopaste"
  "The nopaste command name. Will use `nopaste' in your system's
  $PATH by default")

;; Internal variables
(defvar nopaste-prev-description ""
  "The last description provided. For internal use")
(defvar nopaste-prev-channel nil
  "The last channel provided or `nil' if none. For internal use")

(defvar nopaste-last-url nil "The last URL from the paste server")

(defun nopaste-buffer (start end)
  "`mark-whole-buffer' and nopaste it." 
  (interactive "r")
  (mark-whole-buffer)
  (nopaste-region (point-min) (point-max)))

(defun nopaste-region (start end &optional nickname description channel)
  "nopaste the currently selected region."
  (interactive "r")
  (let* ((nickname (or nickname nopaste-nickname  (read-from-minibuffer "Nick: " nopaste-nickname)))
        (description (and nopaste-description (read-from-minibuffer "Description: " nopaste-prev-description)))
        (channel (and nopaste-channel (or channel (read-from-minibuffer "Channel: " (or nopaste-prev-channel nopaste-channel)))))
        (service nil)
        (language nil)
        (args
         (append
          (and nickname (list "--name" nickname))
          (and channel (list "--channel" channel))
          (and description (list "--description" description))
          (and service (list "--service" service))
          (and language (list "--language language")))))

    (setq nopaste-prev-description description)
    (setq nopaste-prev-channel channel)

    (let ((current-buffer-name (buffer-name)))
      (with-temp-buffer
        (let ((temp-buffer-name (buffer-name)))
          (set-buffer current-buffer-name)

          ;; Call nopaste
          (let ((exit-value (apply 'call-process-region start end "nopaste" nil temp-buffer-name t args)))
            (if (numberp exit-value)
              (cond
               ((eq 0 exit-value))
               (t (error "nopaste failed with exit value %d" exit-value)))
              (error "nopaste failed failed: %s" exit-value)))

          (set-buffer temp-buffer-name)
          (let ((url (chomp (buffer-string))))
            (message "Got URL %s from nopaste, use `nopaste-yank-url' to grab it" url)
            (setq nopaste-last-url url)))))))


(defun nopaste-yank-url ()
  "Insert the URL of the last nopaste at point"
  (interactive)
  (insert nopaste-last-url))

;; From http://github.com/al3x/emacs/blob/cdbd57f589f967efa5e9d4c83e88497db0fd71f9/utilities/chomp.el
(defun chomp (str)
     "..."
     (let ((s (if (symbolp str)(symbol-name str) str)))
        (save-excursion
          (while (and
                  (not (null (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
                  (> (length s) (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
            (setq s (replace-match "" t nil s)))
          (while (and
                  (not (null (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
                  (> (length s) (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
            (setq s (replace-match "" t nil s))))
        s))

(provide 'nopaste)

;;; nopaste.el ends here
