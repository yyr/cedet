;;; ede/auto.el --- Autoload features for EDE

;; Copyright (C) 2010-2014 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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
;; EDE Autoloads are a way to refer to different project types without
;; loading those projects into Emacs.
;;
;; These routines are used to detect a project in a filesystem before
;; handing over control to the usual EDE project system.

;;; Code:

(require 'eieio)

(declare-function ede-directory-safe-p "ede")
(declare-function ede-add-project-to-global-list "ede")

(defclass ede-project-autoload-dirmatch ()
  ((fromconfig :initarg :fromconfig
	       :initform nil
	       :documentation
	       "A config file within which the match pattern lives.")
   (configregex :initarg :configregex
		:initform nil
		:documentation
		"A regexp to identify the dirmatch pattern.")
   (configregexidx :initarg :configregexidx
		   :initform nil
		   :documentation
		   "An index into the match-data of `configregex'.")
   (configdatastash :initform nil
		    :documentation
		    "Save discovered match string.")
   )
  "Support complex matches for projects that live in named directories.
For most cases, a simple string is sufficient.  If, however, a project
location is varied dependent on other complex criteria, this class
can be used to define that match without loading the specific project
into memory.")

(defmethod ede-dirmatch-installed ((dirmatch ede-project-autoload-dirmatch))
  "Return non-nil if the tool DIRMATCH might match is installed on the system."
  (let ((fc (oref dirmatch fromconfig)))

    (cond
     ;; If the thing to match is stored in a config file.
     ((stringp fc)
      (file-exists-p fc))

     ;; Add new types of dirmatches here.

     ;; Error for weird stuff
     (t (error "Unknown dirmatch type.")))))


(defmethod ede-do-dirmatch ((dirmatch ede-project-autoload-dirmatch) file)
  "Does DIRMATCH match the filename FILE."
  (let ((fc (oref dirmatch fromconfig)))

    (cond
     ;; If the thing to match is stored in a config file.
     ((stringp fc)
      (when (file-exists-p fc)
	(let ((matchstring (oref dirmatch configdatastash)))
	  (unless matchstring
	    (save-current-buffer
	      (let* ((buff (get-file-buffer fc))
		     (readbuff
		      (let ((find-file-hook nil)) ;; Disable ede from recursing
			(find-file-noselect fc))))
		(set-buffer readbuff)
		(save-excursion
		  (goto-char (point-min))
		  (when (re-search-forward (oref dirmatch configregex) nil t)
		    (setq matchstring
			  (match-string (or (oref dirmatch configregexidx) 0)))))
		(if (not buff) (kill-buffer readbuff))))
	    ;; Save what we find in our cache.
	    (oset dirmatch configdatastash matchstring))
	  ;; Match against our discovered string
	  (and matchstring (string-match (regexp-quote matchstring) file))
	  )))

     ;; Add new matches here
     ;; ((stringp somenewslot ...)
     ;;   )

     ;; Error if none others known
     (t
      (error "Unknown dirmatch object match style.")))
    ))

(declare-function ede-directory-safe-p "ede")
(declare-function ede-add-project-to-global-list "ede")

(defclass ede-project-autoload ()
  ((name :initarg :name
	 :documentation "Name of this project type")
   (file :initarg :file
	 :documentation "The lisp file belonging to this class.")
   (proj-file :initarg :proj-file
	      :documentation "Name of a project file of this type.")
   (root-only :initarg :root-only
	      :initform t ;; Default - majority case.
	      :documentation
	      "Non-nil if project detection only finds proj-file @ project root.")
   (proj-root-dirmatch :initarg :proj-root-dirmatch
		       :initform ""
		       :type (or string ede-project-autoload-dirmatch)
		       :documentation
		       "To avoid loading a project, check if the directory matches this.
For projects that use directory name matches, a function would load that project.
Specifying this matcher will allow EDE to check without loading the project.")
   (proj-root :initarg :proj-root
	      :type function
	      :documentation "A function symbol to call for the project root.
This function takes no arguments, and returns the current directories
root, if available.  Leave blank to use the EDE directory walking
routine instead.")
   (initializers :initarg :initializers
		 :initform nil
		 :documentation
		 "Initializers passed to the project object.
These are used so there can be multiple types of projects
associated with a single object class, based on the initializers used.")
   (load-type :initarg :load-type
	      :documentation "Fn symbol used to load this project file.")
   (class-sym :initarg :class-sym
	      :documentation "Symbol representing the project class to use.")
   (generic-p :initform nil
	      :documentation
	      "Generic projects are added to the project list at the end.
The add routine will set this to non-nil so that future non-generic placement will
be successful.")
   (new-p :initarg :new-p
	  :initform t
	  :documentation
	  "Non-nil if this is an option when a user creates a project.")
   (safe-p :initarg :safe-p
	   :initform t
	   :documentation
	   "Non-nil if the project load files are \"safe\".
An unsafe project is one that loads project variables via Emacs
Lisp code.  A safe project is one that loads project variables by
scanning files without loading Lisp code from them.")
   )
  "Class representing minimal knowledge set to run preliminary EDE functions.
When more advanced functionality is needed from a project type, that projects
type is required and the load function used.")

(defvar ede-project-class-files
  (list
   (ede-project-autoload "edeproject-makefile"
			 :name "Make" :file 'ede/proj
			 :proj-file "Project.ede"
			 :root-only nil
			 :load-type 'ede-proj-load
			 :class-sym 'ede-proj-project
			 :safe-p nil)
   (ede-project-autoload "edeproject-automake"
			 :name "Automake" :file 'ede/proj
			 :proj-file "Project.ede"
			 :root-only nil
			 :initializers '(:makefile-type Makefile.am)
			 :load-type 'ede-proj-load
			 :class-sym 'ede-proj-project
			 :safe-p nil)
   (ede-project-autoload "automake"
			 :name "automake" :file 'ede/project-am
			 :proj-file "Makefile.am"
			 :root-only nil
			 :load-type 'project-am-load
			 :class-sym 'project-am-makefile
			 :new-p nil
			 :safe-p t)
   )
  "List of vectors defining how to determine what type of projects exist.")

(put 'ede-project-class-files 'risky-local-variable t)

(defun ede-add-project-autoload (projauto &optional flag)
  "Add PROJAUTO, an EDE autoload definition to `ede-project-class-files'.
Optional argument FLAG indicates how this autoload should be
added.  Possible values are:
  'generic - A generic project type.  Keep this at the very end.
  'unique - A unique project type for a specific project.  Keep at the very
            front of the list so more generic projects don't get priority."
  ;; First, can we identify PROJAUTO as already in the list?  If so, replace.
  (let ((projlist ede-project-class-files)
	(projname (eieio-object-name-string projauto)))
    (while (and projlist (not (string= (eieio-object-name-string (car projlist)) projname)))
      (setq projlist (cdr projlist)))

    (if projlist
	;; Stick the new one into the old slot.
	(setcar projlist projauto)

      ;; Else, see where to insert it.
      (cond ((and flag (eq flag 'unique))
	     ;; Unique items get stuck right onto the front.
	     (setq ede-project-class-files
		   (cons projauto ede-project-class-files)))

	    ;; Generic Projects go at the very end of the list.
	    ((and flag (eq flag 'generic))
	     (oset projauto generic-p t)
	     (setq ede-project-class-files
		   (append ede-project-class-files
			   (list projauto))))

	    ;; Normal projects go at the end of the list, but
	    ;; before the generic projects.
	    (t
	     (let ((prev nil)
		   (next ede-project-class-files))
	       (while (and next (not (oref (car next) generic-p)))
		 (setq prev next
		       next (cdr next)))
	       (when (not prev)
		 (error "ede-project-class-files not initialized"))
	       ;; Splice into the list.
	       (setcdr prev (cons projauto next))))))))

;;; Project Autoload Methods
;;

;; New method using detect.el
(defmethod ede-auto-detect-in-dir ((this ede-project-autoload) dir)
  "Return non-nil if THIS project autoload is found in DIR."
  (let* ((d (file-name-as-directory dir))
	 (pf (oref this proj-file))
	 (f (when (stringp pf) (expand-file-name pf d))))
    (and f (file-exists-p f))))

(defmethod ede-auto-load-project ((this ede-project-autoload) dir)
  "Load in the project associated with THIS project autoload description.
THIS project description should be valid for DIR, where the project will
be loaded."
  ;; Last line of defense: don't load unsafe projects.
  (when (not (or (oref this :safe-p)
		 (ede-directory-safe-p dir)))
    (error "Attempt to load an unsafe project (bug elsewhere in EDE)"))
  ;; Things are good - so load the project.
  (let ((o (funcall (oref this load-type) dir)))
    (when (not o)
      (error "Project type error: :load-type failed to create a project"))
    (ede-add-project-to-global-list o)
    ;; @TODO - Add to hash over at `ede-inode-directory-hash'.
    ))















;;; -------- Old Methods 
;; See if we can do without them.

;;; EDE project-autoload methods
;;

;; @FIXME - delete from loaddefs to remove this.
(defmethod ede-project-root ((this ede-project-autoload))
  "If a project knows its root, return it here.
Allows for one-project-object-for-a-tree type systems."
  nil)

;; @FIXME - delete from loaddefs to remove this.
(defmethod ede-project-root-directory ((this ede-project-autoload) &optional file)
  "" nil)
					   
;; @FIXME - can we obsolete this?
(defmethod ede-dir-to-projectfile ((this ede-project-autoload) dir)
  "Return a full file name of project THIS found in DIR.
The file is the file that marks this project.
Return nil if the project file does not exist."
  (let* ((d (file-name-as-directory dir))
	 (root (ede-project-root-directory this d))
	 (pf (oref this proj-file))
	 (dm (oref this proj-root-dirmatch))
	 (f (cond ((stringp pf)
		   (expand-file-name pf (or root d)))
		  ((and (symbolp pf) (fboundp pf))
		   ;; If there is a symbol to call, lets make extra
		   ;; sure we really can call it without loading in
		   ;; other EDE projects.  This happens if the file is
		   ;; already loaded, or if there is a dirmatch, but
		   ;; root is empty.
		   (when (and (featurep (oref this file))
			      (or (not (stringp dm))
				  (not (string= dm "")))
			      root)
		     (funcall pf (or root d))))))
	 )
    (when (and f (file-exists-p f))
      f)))

(provide 'ede/auto)

;;; ede/auto.el ends here
