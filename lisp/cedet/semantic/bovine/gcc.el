;;; semantic/bovine/gcc.el --- gcc querying special code for the C parser

;; Copyright (C) 2008-2015 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; GCC stores things in special places.  These functions will query
;; GCC, and set up the preprocessor and include paths.

(require 'semantic/dep)
(require 'semantic/lex-spp)

(defvar semantic-lex-c-preprocessor-symbol-file)
(defvar semantic-lex-c-preprocessor-symbol-map)
(declare-function semantic-c-reset-preprocessor-symbol-map "semantic/bovine/c")

;; TODO: Make this a defcustom.
(defvar semantic-gcc-get-preprocessor-macros nil)

;;; Code:

(defun semantic-gcc-query (gcc-cmd &rest gcc-options)
  "Return program output or error code in case error happens.
GCC-CMD is the program to execute and GCC-OPTIONS are the options
to give to the program."
  ;; $ gcc -v
  ;;
  (let* ((buff (get-buffer-create " *gcc-query*"))
         (old-lc-messages (getenv "LC_ALL"))
         (options `(,nil ,(cons buff t) ,nil ,@gcc-options))
         (err 0))
    (with-current-buffer buff
      (erase-buffer)
      (setenv "LC_ALL" "C")
      (condition-case nil
          (setq err (apply 'call-process gcc-cmd options))
        (error ;; Some bogus directory for the first time perhaps?
         (let ((default-directory (expand-file-name "~/")))
           (condition-case nil
               (setq err (apply 'call-process gcc-cmd options))
             (error ;; gcc doesn't exist???
              nil)))))
      (setenv "LC_ALL" old-lc-messages)
      (prog1
          (if (zerop err)
              (buffer-string)
            err)
        (kill-buffer buff)))))

;;(semantic-gcc-get-include-paths "c")
;;(semantic-gcc-get-include-paths "c++")
(defun semantic-gcc-get-include-paths (lang)
  "Return include paths as gcc uses them for language LANG."
  (let* ((gcc-cmd (cond
                   ((string= lang "c") "gcc")
                   ((string= lang "c++") "c++")
                   (t (if (stringp lang)
                          (error "Unknown lang: %s" lang)
                        (error "LANG=%S, should be a string" lang)))))
         (gcc-output (semantic-gcc-query gcc-cmd "-v" "-E" "-x" lang null-device))
         (lines (split-string gcc-output "\n"))
         (include-marks 0)
         (inc-mark "#include ")
         (inc-mark-len (length "#include "))
         inc-path)
    ;;(message "gcc-output=%s" gcc-output)
    (dolist (line lines)
      (when (> (length line) 1)
        (if (= 0 include-marks)
            (when (and (> (length line) inc-mark-len)
                       (string= inc-mark (substring line 0 inc-mark-len)))
              (setq include-marks (1+ include-marks)))
          (let ((chars (append line nil)))
            (when (= 32 (nth 0 chars))
              (let ((path (substring line 1)))
                (when (file-accessible-directory-p path)
                  (when (if (memq system-type '(windows-nt))
                            (/= ?/ (nth 1 chars))
                          (= ?/ (nth 1 chars)))
                    (add-to-list 'inc-path
                                 (expand-file-name (substring line 1))
                                 t)))))))))
    inc-path))


(defun semantic-cpp-defs (str)
  "Convert CPP output STR into a list of cons cells with defines for C++."
  (let ((lines (split-string str "\n"))
        (lst nil))
    (dolist (L lines)
      (let ((dat (split-string L)))
        (when (= (length dat) 3)
          (add-to-list 'lst (cons (nth 1 dat) (nth 2 dat))))))
    lst))

(defun semantic-gcc-fields (str)
  "Convert GCC output STR into an alist of fields."
  (let ((fields nil)
        (lines (split-string str "\n"))
        )
    (dolist (L lines)
      ;; For any line, what do we do with it?
      (cond ((or (string-match "Configured with\\(:\\)" L)
                 (string-match "\\(:\\)\\s-*[^ ]*configure " L))
             (let* ((parts (substring L (match-end 1)))
                    (opts (split-string parts " " t))
                    )
               (dolist (O (cdr opts))
                 (let* ((data (split-string O "="))
                        (sym (intern (car data)))
                        (val (car (cdr data))))
                   (push (cons sym val) fields)
                   ))
               ))
            ((string-match "gcc[ -][vV]ersion" L)
             (let* ((vline (substring L (match-end 0)))
                    (parts (split-string vline " ")))
               (push (cons 'version (nth 1 parts)) fields)))
            ((string-match "Target: " L)
             (let ((parts (split-string L " ")))
               (push (cons 'target (nth 1 parts)) fields)))
            ))
    fields))

(defvar semantic-gcc-setup-data nil
  "The GCC setup data.
This is setup by `semantic-gcc-setup'.
This is an alist, and should include keys of:
  'version  - the version of gcc
  '--host   - the host symbol (used in include directories)
  '--prefix - where GCC was installed.
It should also include other symbols GCC was compiled with.")

;;;###autoload
(defun semantic-gcc-setup ()
  "Setup Semantic C/C++ parsing based on GCC output."
  (interactive)
  (let* ((fields (or semantic-gcc-setup-data
                     (semantic-gcc-fields (semantic-gcc-query "gcc" "-v"))))
         (cpp-options `("-E" "-dM" "-x" "c++" ,null-device))
         (query (let ((q (apply 'semantic-gcc-query "cpp" cpp-options)))
                  (if (stringp q)
                      q
                    ;; `cpp' command in `semantic-gcc-setup' doesn't work on
                    ;; Mac, try `gcc'.
                    (apply 'semantic-gcc-query "gcc" cpp-options))))
         (defines (if (stringp query)
		      (semantic-cpp-defs query)
		    (message (concat "Could not query gcc for defines. "
				     "Maybe g++ is not installed."))
		    nil))
         (ver (cdr (assoc 'version fields)))
         (host (or (cdr (assoc 'target fields))
                   (cdr (assoc '--target fields))
                   (cdr (assoc '--host fields))))
         (prefix (cdr (assoc '--prefix fields)))
         ;; gcc output supplied paths
         (c-include-path (semantic-gcc-get-include-paths "c"))
         (c++-include-path (semantic-gcc-get-include-paths "c++"))
	 (gcc-exe (locate-file "gcc" exec-path exec-suffixes 'executable))
	 )
    ;; Remember so we don't have to call GCC twice.
    (setq semantic-gcc-setup-data fields)
    (when (and (not c-include-path) gcc-exe)
      ;; Fallback to guesses
      (let* ( ;; gcc include dirs
             (gcc-root (expand-file-name ".." (file-name-directory gcc-exe)))
             (gcc-include (expand-file-name "include" gcc-root))
             (gcc-include-c++ (expand-file-name "c++" gcc-include))
             (gcc-include-c++-ver (expand-file-name ver gcc-include-c++))
             (gcc-include-c++-ver-host (expand-file-name host gcc-include-c++-ver)))
        (setq c-include-path
              ;; Replace cl-function remove-if-not.
              (delq nil (mapcar (lambda (d)
                                  (if (file-accessible-directory-p d) d))
                                (list "/usr/include" gcc-include))))
        (setq c++-include-path
              (delq nil (mapcar (lambda (d)
                                  (if (file-accessible-directory-p d) d))
                                (list "/usr/include"
                                      gcc-include
                                      gcc-include-c++
                                      gcc-include-c++-ver
                                      gcc-include-c++-ver-host))))))

    ;;; Fix-me: I think this part might have been a misunderstanding, but I am not sure.
    ;; If this option is specified, try it both with and without prefix, and with and without host
    ;; (if (assoc '--with-gxx-include-dir fields)
    ;;     (let ((gxx-include-dir (cdr (assoc '--with-gxx-include-dir fields))))
    ;;       (nconc try-paths (list gxx-include-dir
    ;;                              (concat prefix gxx-include-dir)
    ;;                              (concat gxx-include-dir "/" host)
    ;;                              (concat prefix gxx-include-dir "/" host)))))

    ;; Now setup include paths etc
    (dolist (D (semantic-gcc-get-include-paths "c"))
      (semantic-add-system-include D 'c-mode))
    (dolist (D (semantic-gcc-get-include-paths "c++"))
      (semantic-add-system-include D 'c++-mode)
      (let ((cppconfig (list (concat D "/bits/c++config.h") (concat D "/sys/cdefs.h")
			     (concat D "/features.h"))))
	(dolist (cur cppconfig)
	  ;; Presumably there will be only one of these files in the try-paths list...
	  (when (file-readable-p cur)
          ;; Add it to the symbol file
          (if (boundp 'semantic-lex-c-preprocessor-symbol-file)
              ;; Add to the core macro header list
              (add-to-list 'semantic-lex-c-preprocessor-symbol-file cur)
            ;; Setup the core macro header
            (setq semantic-lex-c-preprocessor-symbol-file (list cur)))
          ))))
    (if (not (boundp 'semantic-lex-c-preprocessor-symbol-map))
        (setq semantic-lex-c-preprocessor-symbol-map nil))
    (dolist (D defines)
      (add-to-list 'semantic-lex-c-preprocessor-symbol-map D))
    ;; Needed for parsing OS X libc
    (when (eq system-type 'darwin)
      (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("__i386__" . "")))
    (when (featurep 'semantic/bovine/c)
      (semantic-c-reset-preprocessor-symbol-map))
    nil))

;; Obtaining preprocessor macros.

(defvar semantic-gcc-got-macros nil)
(make-variable-buffer-local 'semantic-gcc-got-macros)

;; Function for hook
(defun semantic-gcc-get-macros-for-lexer (start end)
  "Use GCC to obtain macro definitions from includes."
  (when (and semantic-gcc-get-preprocessor-macros
	     (not semantic-gcc-got-macros)
	     (not (string-match ".*preprocessed.*" (buffer-name))))
    (semantic-gcc-get-macros)))

(defun semantic-gcc-buffer-name ()
  "Preprocessor buffer name for current file."
  (concat " *" (buffer-name (current-buffer)) " preprocessed*"))

(defun semantic-gcc-get-macros ()
  "Get preprocessor definitions from includes."
  (interactive)
  (let* ((lang (if (eq major-mode 'c++-mode) "c++" "c"))
	 (buffer
	  (with-current-buffer (get-buffer-create
				(semantic-gcc-buffer-name))
	    (erase-buffer)
	    (current-buffer)))
	 (args (semantic-gcc-args-from-project)))
    ;; Add system include paths
    (setq args
	  (append args
		  (mapcar
		   (lambda (path)
		     (concat "-I" path))
		   semantic-dependency-system-include-path)))
    ;; Get all #include's for current buffer.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*#\\s-*include \\(.+\\)" nil t)
	(let ((include (match-string-no-properties 1)))
	  (with-current-buffer buffer
	    (insert "#include " include "\n")))))
    ;; Call gcc to get macros.
    (with-current-buffer buffer
      (let ((end (point)))
	(apply 'call-process-region (point-min) (point-max)
	       "gcc" nil buffer nil "-E" "-dM" "-x" lang
	       (append  args (list "-")))
	(delete-region (point-min) end)))
    ;; Parse macros and put them in the lex-spp obarray.
    (semantic-gcc-parse-macros (current-buffer) buffer)
    (setq semantic-gcc-got-macros t))
  t)

(defun semantic-gcc-parse-macros (origbuf macrosbuf)
  (with-current-buffer macrosbuf
    (goto-char (point-min))
    ;; Delete all macros which are already defined in the original
    ;; buffer
    (while (re-search-forward "^#define \\(.+\\) " nil t)
      (let ((name (match-string 1)))
	(when (with-current-buffer origbuf
		(semantic-lex-spp-symbol-p name))
	  (delete-region (point-at-bol) (point-at-eol)))))
    (let ((c-mode-common-hook nil)
	  (c-mode-hook nil)
	  (c++-mode-hook nil)
	  (prog-mode-hook nil))
      (condition-case nil
	  (c++-mode)
	(error nil)))
    (activate-mode-local-bindings)
    (semantic-default-c-setup)
    (setq semantic-new-buffer-fcn-was-run t)
    (semantic-lex-init)
    (semantic-clear-toplevel-cache)
    (remove-hook 'semantic-lex-reset-functions
    		 'semantic-lex-spp-reset-hook t)
    (let ((tags (semantic-parse-region (point-min) (point-max))))
      (dolist (cur tags)
    	(let* ((name (semantic-tag-name cur))
    	       (symbol (semantic-lex-spp-symbol name))
    	       (value (symbol-value symbol)))
    	  (with-current-buffer origbuf
    	    (semantic-lex-spp-symbol-set name value)))))))

(defun semantic-gcc-args-from-project ()
  "Return list of additional arguments for the compiler from the project.
If the current buffer is part of an EDE project, return a list of
additional arguments for the compiler; currently, this deals with
include directories (-I) and preprocessor symbols (-D)."
  (let ((proj ede-object-root-project)
	(tarproj ede-object))
    (when proj
      (cond
       ;; For ede-cpp-root-project it's easy
       ((ede-cpp-root-project-child-p proj)
	(append
	 (mapcar (lambda (inc) (concat "-I" inc))
		 (append (mapcar (lambda (x) (concat (ede-project-root-directory proj) x))
				 (oref proj include-path))
			 (oref proj system-include-path)))
	 (mapcar (lambda (spp) (concat "-D" (car spp)
				       (when (cdr spp)
					 (concat "=" (cadr spp)))))
		 (oref proj spp-table))
	 (list (concat "-I" (ede-project-root-directory proj)))))
       ;; Similarly for ede-linux-project
       ((ede-linux-project-child-p proj)
	(let* ((root (ede-project-root-directory proj))
	       (dir (file-name-directory (buffer-file-name)))
	       (rel-dir (substring dir (length root))))
	  (append
	   (list (format "-include%s/include/linux/kconfig.h" root))
	   (mapcar (lambda (inc) (concat "-I" inc))
		   (oref proj include-path))
	   (list (concat "-I" root rel-dir)
		 (concat "-I" (oref proj build-directory) rel-dir)
		 "-D__KERNEL__"))))
       ;; For more general project types it's a bit more difficult.
       ((ede-proj-project-p proj)
	;; Get the local and configuration variables.
	(let ((vars (mapcar 'cdr (oref proj variables))))
	  (when (slot-boundp tarproj 'configuration-variables)
	    (setq vars (append vars
			       (mapcar 'cdr
				       (cdr (oref tarproj configuration-variables))))))
	  ;; Get includes and preprocessor symbols.
	  (setq vars (apply 'append (mapcar 'split-string vars)))
	  (append (list (concat "-I" (ede-project-root-directory proj)))
		  (delq nil
			(mapcar (lambda (var)
				  (when (string-match "^\\(-I\\|-D\\)" var)
				    var))
				vars)))))))))

(provide 'semantic/bovine/gcc)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/bovine/gcc"
;; End:

;;; semantic/bovine/gcc.el ends here
