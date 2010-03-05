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

(defun nopaste-region (start end &optional nickname description channel)
  ""
  (interactive "r")
  (let ((nickname (or nickname (read-from-minibuffer "Nick: " nopaste-nickname)))
        (description (read-from-minibuffer "Description: " nopaste-prev-description))
        (channel (or channel (read-from-minibuffer "Channel: " (or nopaste-prev-channel nopaste-channel)))))
    (setq nopaste-prev-description description)
    (setq nopaste-prev-channel channel)
    (let* ((out (make-temp-file "nopaste"))
           (command (concat
                     nopaste-command
                     " --channel '" channel
                     "' --name '" nickname
                     "' --description '" description
                     "' '" out "'")))
      (unwind-protect
           (progn
             (write-region start end out)
             (let* ((str (shell-command-to-string command))
                    (str-len (length str))
                    (url (substring str 0 (- str-len 1))))
               (kill-new url))
             (delete-file out))))))

(provide 'nopaste)

;;; nopaste.el ends here
