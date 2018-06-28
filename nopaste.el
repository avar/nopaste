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

;; The recommended keybindings are:
;;
;; (global-set-key (kbd "C-c n p") 'nopaste)
;; (global-set-key (kbd "C-c n y") 'nopaste-yank-url)


;;; History:
;; 

;;; Code:


;; Public variables
(defvar nopaste-nickname ""
  "The nick given to nopaste.")
(defvar nopaste-description ""
  "The description given to nopaste.")
(defvar nopaste-channel ""
  "The channel given to nopaste.")
(defvar nopaste-language ""
  "The the language given to nopaste.")
(defvar nopaste-service nil
  "The nopaste service to use.
This can also be set through the
NOPASTE_SERVICES environmental variable to be read by nopaste
  itself.")

(defvar nopaste-command "nopaste"
  "The nopaste command name.
Will use `nopaste' in your system's $PATH by default")

;; Internal variables
(defvar nopaste--prev-description ""
  "The last description provided.  For internal use.")
(defvar nopaste--prev-channel nil
  "The last channel provided or nil if none.  For internal use.")

(defvar nopaste--last-url nil "The last URL from the paste server.")
(defvar nopaste-kill-last-url t
  "Whether to make the URL we get available in the kill ring.")

(defvar nopaste-proc-buffer nil
  "Log interactions with the subprocess in the given buffer
  name. For example, can be set to `*nopaste*' to see the output
  of the command interactively.")

(defvar nopaste-proc-buffer-errors "*nopaste-errors*"
  "Buffer name where to send errors from the subprocess. Can be
  set to `nil' to use the same buffer as `nopaste-proc-buffer`,
  but that also has the side-effect of sending errors to the
  `nopaste--filter' which might be confusing because errors will
  be mixed with URLs.")

(defcustom nopaste-type-assoc
  '((actionscript-mode . " actionscript")
    (ada-mode . "ada")
    (asm-mode . "asm")
    (autoconf-mode . "bash")
    (bibtex-mode . "bibtex")
    (cmake-mode . "cmake")
    (c-mode . "c")
    (c++-mode . "cpp")
    (cobol-mode . "cobol")
    (conf-colon-mode . "properties")
    (conf-javaprop-mode . "properties")
    (conf-mode . "ini")
    (conf-space-mode . "properties")
    (conf-unix-mode . "ini")
    (conf-windows-mode . "ini")
    (cperl-mode . "perl")
    (csharp-mode . "csharp")
    (css-mode . "css")
    (delphi-mode . "delphi")
    (diff-mode . "diff")
    (ebuild-mode . "bash")
    (eiffel-mode . "eiffel")
    (emacs-lisp-mode . "lisp")
    (erlang-mode . "erlang")
    (erlang-shell-mode . "erlang")
    (espresso-mode . "javascript")
    (fortran-mode . "fortran")
    (glsl-mode . "glsl")
    (gnuplot-mode . "gnuplot")
    (graphviz-dot-mode . "dot")
    (haskell-mode . "haskell")
    (html-mode . "html4strict")
    (idl-mode . "idl")
    (inferior-haskell-mode . "haskell")
    (inferior-octave-mode . "octave")
    (inferior-python-mode . "python")
    (inferior-ruby-mode . "ruby")
    (java-mode . "java")
    (js2-mode . "javascript")
    (jython-mode . "python")
    (latex-mode . "latex")
    (lisp-mode . "lisp")
    (lua-mode . "lua")
    (makefile-mode . "make")
    (makefile-automake-mode . "make")
    (makefile-gmake-mode . "make")
    (makefile-makepp-mode . "make")
    (makefile-bsdmake-mode . "make")
    (makefile-imake-mode . "make")
    (matlab-mode . "matlab")
    (nxml-mode . "xml")
    (oberon-mode . "oberon2")
    (objc-mode . "objc")
    (ocaml-mode . "ocaml")
    (octave-mode . "matlab")
    (pascal-mode . "pascal")
    (perl-mode . "perl")
    (php-mode . "php")
    (plsql-mode . "plsql")
    (po-mode . "gettext")
    (prolog-mode . "prolog")
    (python-2-mode . "python")
    (python-3-mode . "python")
    (python-basic-mode . "python")
    (python-mode . "python")
    (ruby-mode . "ruby")
    (scheme-mode . "lisp")
    (shell-mode . "bash")
    (sh-mode . "bash")
    (smalltalk-mode . "smalltalk")
    (sql-mode . "sql")
    (tcl-mode . "tcl")
    (visual-basic-mode . "vb")
    (xml-mode . "xml")
    (yaml-mode . "properties"))
  "Alist composed of major-mode names and corresponding pastebin
highlight formats.

This'll be used as the default `LANGUAGE' parameter to
`nopaste'. Of course it's only a default, some nopaste(1)
services take e.g. \"Perl\" instead of \"perl\", or maybe
\"yaml\" instead of \"properties\".

The definition was stolen as-is from pastebin.el at
http://www.emacswiki.org/emacs/pastebin.el"
  :type '(alist :key-type symbol :value-tupe string)
  :group 'nopaste)

(defun nopaste (&optional start end nickname description channel language)
   "Shell out to the nopaste(1) program with the current region or buffer.

Nopastes either the currently active region from `START' to
`END', or the entire buffer.  If `region-active-p' is true we'll
paste the region, otherwise we'll paste the buffer from
`point-min' to `point-max'.

Other optional arguments are `NICKNAME' `DESCRIPTION', `CHANNEL'
and `LANGUAGE'."
  (interactive "r")
  (when (not (region-active-p))
    (setq start (point-min) end (point-max)))
  (nopaste-region start end nickname description channel language))

(defun nopaste-region (start end &optional nickname description channel language)
  "Nopaste a given region, nopaste will be called with `call-process-region'.

The arguments are like the arguments to `nopaste', except `START'
and `END' aren't optional, i.e it also takes `NICKNAME'
`DESCRIPTION', `CHANNEL' and `LANGUAGE'."
  (interactive "r")
  (let* ((nickname (or nickname nopaste-nickname  (read-from-minibuffer "Nick: " nopaste-nickname)))
        (description (and nopaste-description (read-from-minibuffer "Description: " nopaste--prev-description)))
        (channel (and nopaste-channel (or channel (read-from-minibuffer "Channel: " (or nopaste--prev-channel nopaste-channel)))))
        (service (or nopaste-service nil))
        (language (or (assoc-default major-mode nopaste-type-assoc) nil))
        (args
         (append
          (and nickname (list "--name" nickname))
          (and channel (list "--channel" channel))
          (and description (list "--description" description))
          (and service (list "--service" service))
          (and language (list "--language" language)))))

    (setq nopaste--prev-description description)
    (setq nopaste--prev-channel channel)

    (let ((proc (make-process :filter #'nopaste--filter
                              :sentinel #'nopaste--sentinel
                              :command (cons "nopaste" args)
                              :connection-type 'pipe
                              :buffer nopaste-proc-buffer
                              :stderr nopaste-proc-buffer-errors
                              :name "nopaste")))
      (process-send-region proc start end)
      (process-send-eof proc)
      (message "paste started"))))

(defun nopaste--ordinary-insertion-filter (proc string)
  "Append output of command to the assigned process buffer.

   Cargo-culted from the `Filter functions' topic in the Elisp
   info manual"
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun nopaste--filter (proc string)
  "handle async output from `nopaste-region'"
  (when nopaste-proc-buffer
    (nopaste--ordinary-insertion-filter proc string))
  ;; TODO: the chomp might want to check for errors if the
  ;; sentinel doesn't suffice
  (let ((url (nopaste--chomp string)))
    (message "Pasted content to URL %s" url)
    (when nopaste-kill-last-url
      (kill-new url))
    (setq nopaste--last-url url)))

(defun nopaste--sentinel (proc event)
  "handle errors from async process"
  (let ((err-fmt "Paste command %s failed with exit value %d")
        (status (process-exit-status proc)))
    (unless (= 0 status)
      (error (or (and (not nopaste-proc-buffer-errors) err-fmt)
               (concat err-fmt
                       (format ", see %s for details"
                               nopaste-proc-buffer-errors)))
             (process-command proc) status))))

(defun nopaste-yank-url ()
  "Insert the URL of the last nopaste at point."
  (interactive)
  (insert nopaste--last-url))

(defun nopaste--chomp (str)
  "Private: Takes a `STR' and chomps its trailing \n$."
  (if (equal (elt str (- (length str) 1)) ?\n)
      (substring str 0 (- (length str) 1))
    str))

(provide 'nopaste)

;;; nopaste.el ends here
