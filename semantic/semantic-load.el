;;; semantic-load.el --- Autoload definitions for Semantic

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-load.el,v 1.36 2003-07-18 05:26:53 zappo Exp $

;; Semantic is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Initialize semantic for all supported conditions.

;;; Code:
;;

(require 'semantic-fw)

;;; Add parser directories
;;
(let ((dir (file-name-directory (locate-library "semantic"))))
  (add-to-list 'load-path (concat dir "bovine"))
  (add-to-list 'load-path (concat dir "wisent"))
  )

;;; Some speedbar major modes
(eval-after-load "speedbar"
  '(progn
     (require 'semantic-cb)
     (require 'semantic-ia-sb)))

;;; Useful predefined setup
;;
(defvar semantic-load-turn-everything-on nil
  "Non-nil means turn on all features in the semantic package.")

(defvar semantic-load-turn-useful-things-on nil
  "Non-nil means turn on all `useful' features.
Sadly `useful' here means things Eric wants on as opposed to some
other criteria.")

(when (or semantic-load-turn-everything-on
	  semantic-load-turn-useful-things-on)

  (if (and semantic-load-turn-everything-on
	   (fboundp #'which-func-mode))
      (add-hook 'semantic-init-hooks (lambda ()
				       (which-func-mode 1))))

  (when (and (eq window-system 'x)
	     (locate-library "imenu"))
    (add-hook 'semantic-init-hooks (lambda ()
				     (condition-case nil
					 (imenu-add-to-menubar "TOKENS")
				       (error nil)))))

  (when semantic-load-turn-everything-on
    (global-semantic-highlight-edits-mode 1)
    )

  (global-senator-minor-mode 1)
  (global-semantic-show-unmatched-syntax-mode 1)
  (global-semantic-summary-mode 1)
  (global-semantic-auto-parse-mode 1)

  (global-semantic-show-parser-state-mode 1)

  (global-semanticdb-minor-mode 1)

  (when (boundp 'header-line-format)
    (global-semantic-stickyfunc-mode 1))

  (when (and (fboundp 'display-graphic-p)
	     (display-graphic-p)
	     ;; The above is also asking for Emacs 21... I think.
	     )
    (global-semantic-show-tag-boundaries-mode 1))

  (global-semantic-highlight-by-attribute-mode 1)

  ;; This loads any created system databases which get linked into
  ;; any searches performed.
  (if (and (boundp 'semanticdb-default-system-save-directory)
	   (stringp semanticdb-default-system-save-directory)
	   (file-exists-p semanticdb-default-system-save-directory))
      (semanticdb-load-system-caches))

 )

(provide 'semantic-load)

;;; semantic-load.el ends here
