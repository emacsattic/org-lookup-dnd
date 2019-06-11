;;; org-lookup-dnd.el --- Reference the index of a D&D handbook pdf -*- lexical-binding: t; -*-

;; This program was meant a) To help me run my Dungeons and Dragons sessions
;; better, and b) as a exercise for me to learn a bit of lisp.

;; Copyright (C) 2019 Malte Lau Petersen

;; Author: Malte Lau Petersen <maltelau@protonmail.com>
;; Created: May 2019
;; Version: 0.1
;; URL: https://gitlab.com/maltelau/org-lookup-dnd
;; Package-Requires: ((emacs "24"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ## INSTALLATION
;; 1. Clone this repository into somewhere in your load-path
;; 2. Load the program somehow, I have this in my ~/.emacs

;; (use-package org-lookup-dnd
;;     :bind ("C-c d" . org-lookup-dnd-at-point))

;; 3. Customize the variable org-lookup-dnd-sources to point to
;; one or more pdf files you'd like to run this on.  For example
;; to index the table of contents on page 4 of your players handbook,
;; and subtract 6 from all the page numbers in that index:

;; Path to pdf        : ~/Documents/DnD5ePlayersHandbook.pdf
;; Page offset        : -6
;; First page of index: 4
;; Last page of index : 4

;; ## HOW TO USE IT
;; Run `org-lookup-dnd-at-point`.  If there is a word under the pointer, it will
;; search for that term.  Otherwise, write a search term in the minibuffer.
;; If there are more than one matches, you get to pick which one to link to.

;; ## DEPENDENCIES
;; - pdftotext (from poppler-utils on ubuntu)
;; - org-pdfview (from melpa)

;;; Code:

(require 'org-table)


;; Variables

(defvar org-lookup-dnd-db nil
  "The db, loaded into memory.
A list of entries, where each entry is a list:
\('searchterm' '/path/to/pdf' 'pagenr')")


(defvar org-lookup-dnd-choice nil
  "Last dnd entry inserted.")


(defvar org-lookup-dnd-history nil
  "History for searches in 'org-lookup-dnd-db'.")


;; Utility functions

(defun org-lookup-dnd-dump-vars-to-file (varlist filename)
  "Simplistic dumping of variables in VARLIST to a file FILENAME."
  (unless (file-exists-p filename) (make-directory (file-name-directory filename) t))
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (org-lookup-dnd-dump varlist buf)
      (save-buffer)
      (kill-buffer))))


(defun org-lookup-dnd-dump (varlist buffer)
  "For all the variables in VARLIST, insert into BUFFER the setq statement to recreate them."
  (dolist (var varlist)
    (print (list 'setq var (list 'quote (symbol-value var)))
	   buffer)))


(defun org-lookup-dnd-delete-region-curried (pos)
  "Delete a region, accepting POS as a (START . END) list."
  (when pos (delete-region (car pos) (cdr pos))))


(defun org-lookup-dnd-extract-from-index (regexp string &optional start)
  "Match a REGEXP in STRING with exactly 2 capture groups.
Returns a list of the matches (group1 group2).
Optional: Specify the START position.
Adapted from ‘replace-regexp-in-string’."
  ;; (setq matches nil)
  (let ((l (length string))
	(start (or start 0))
	matches mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
	(setq mb (match-beginning 0)
	      me (match-end 0))
	;; If we matched the empty string, make sure we advance by one char
	(when (= me mb) (setq me (min l (1+ mb))))
	(setq start me)
	(setq matches
	      (cons (list (match-string 1 string) (match-string 2 string))
		    matches))))
    (reverse matches)))


;; Here comes the meat of this little library

(defun org-lookup-dnd-new-config (symbol value)
  "Set the custom varable (assign to SYMBOL the VALUE) and index the pdfs to be ready to search."
  (set symbol value)
  (when (and (bound-and-true-p org-lookup-dnd-sources)
	     (bound-and-true-p org-lookup-dnd-db-file))
    (org-lookup-dnd-parse)))


(defun org-lookup-dnd-check-if-setup ()
  "Check if the custom variables are setup, and the db index loaded from file."
  (unless (and (bound-and-true-p org-lookup-dnd-sources)
	       (bound-and-true-p org-lookup-dnd-db-file))
    (error "Please ensure you've customized org-lookup-dnd to point to your pdf"))
  (unless (boundp 'org-lookup-dnd-db) (load-file org-lookup-dnd-db-file)))


(defun org-lookup-dnd-parse ()
  "Parse pdfs, (opt) the extra org table, and store the db on disk."
  (org-lookup-dnd-parse-pdfs)
  (setq org-lookup-dnd-db (append
			   org-lookup-dnd-db
			   (org-lookup-dnd-parse-extras)))
  (org-lookup-dnd-dump-vars-to-file '(org-lookup-dnd-db) org-lookup-dnd-db-file))


(defun org-lookup-dnd-parse-pdfs ()
  "Read in all the pdfs, and extract and index the table of contents.
Stores what it finds in ‘org-lookup-dnd-db’."
  (setq org-lookup-dnd-db
	(apply #'append (mapcar (lambda (source)
				  (let (txt lst)
				    ;; For each pdf source, run pdftottext on the index,
				    (setq txt (shell-command-to-string (format "pdftotext -layout -f %d -l %d %s -"
									       (nth 2 source)
									       (nth 3 source)
									       (shell-quote-argument (expand-file-name (car source))))))
				    ;; remove most punctuation,
				    (setq txt (replace-regexp-in-string "[-':�;~^\"\`\'•|,·./() ]" "" txt))
				    ;; And extract the (term)......(page).
				    (setq lst (org-lookup-dnd-extract-from-index
					       "\\([^\n[:digit:]]+\\)\n*\\([[:digit:]]+\\)" txt))
				    ;; Then add the page offset for this source.
				    (mapcar (lambda (entry)
					      (list (car entry)
						    (car source)
						    (+ (string-to-number (nth 1 entry)) (nth 1 source))))
					    lst)))
				org-lookup-dnd-sources))))


(defun org-lookup-dnd-parse-extras ()
  "Read in the extra index from ‘org-lookup-dnd-extra-index’."
  (when (file-exists-p org-lookup-dnd-extra-index)
    (save-excursion
      (let (extras
	    (buf (find-file-noselect org-lookup-dnd-extra-index)))
	(set-buffer buf)
	(goto-char (point-min))
	;; Read in the table, interpreting the page nr (3rd col) as numeric
	(setq extras (mapcar (lambda (entry)
			       (list (car entry)
				     (nth 1 entry)
				     (string-to-number (nth 2 entry))))
			     (cdr (cdr (org-table-to-lisp))))) ;; skip header
	(kill-buffer buf)
	extras)))) ;; return


(defun org-lookup-dnd-search (regexp)
  "Filter the db down according to the search REGEXP."
  (interactive (list (read-regexp "Search dnd reference: " nil 'org-lookup-dnd-history)))
  (delete nil
	  (mapcar
	   (lambda (entry)
	     (when (string-match regexp (car entry)) entry))
	   org-lookup-dnd-db)))


;;;###autoload
(defun org-lookup-dnd-at-point ()
  "Search for a (dnd) term from the index, clarify which one is meant, and then output an ‘org-mode’ link to the pdf at the right page."
  (interactive)
  (org-lookup-dnd-check-if-setup)
  (let ((orig-word (thing-at-point 'word))
	entries)
    (if (not orig-word)
	;; ask for a search term
	(setq orig-word (read-regexp "Search dnd reference: "
				     nil 'org-lookup-dnd-history))
      ;; add the word under point to history
      (setq org-lookup-dnd-history (cons orig-word
					 (when (boundp 'org-lookup-dnd-history)
					   org-lookup-dnd-history))))
    (setq entries (org-lookup-dnd-search orig-word))
    (setq org-lookup-dnd-choice
	  ;; select between multiple matches
	  (if (= (length entries) 1)
	      (car (car entries))
	    (ido-completing-read "Which one? "  (mapcar #'car entries))))
    ;; TODO: better way of indexing the db than by term.
    (dolist (entry entries)
      (when (string= org-lookup-dnd-choice (car entry))
	(org-lookup-dnd-delete-region-curried (bounds-of-thing-at-point 'word))
	(insert (format org-lookup-dnd-link-format
			(nth 1 entry)
			(nth 2 entry)
			orig-word))))))


;; Customization

(defgroup org-lookup-dnd nil
  "This package indexes some pdfs and lets you insert links
from the table of contents into your org-mode document.

You need to tell it which pdfs to index, and which pages to look at."
  :group 'org)


(defcustom org-lookup-dnd-db-file "~/.local/share/org-lookup-dnd-db.el"
  "Location to store the index on disk."
  :type '(string)
  :set #'org-lookup-dnd-new-config
  :group 'org-lookup-dnd)


(defcustom org-lookup-dnd-extra-index "~/.local/share/org-lookup-dnd-extra.org"
  "Location of (org)file with extra search references.  Optional.
The format is an org table with the columns: | searchterm | path/to/pdffile | page |"
  :type '(string)
  :set #'org-lookup-dnd-new-config
  :group 'org-lookup-dnd)


(defcustom org-lookup-dnd-sources nil
  "A list of source(book)s. Each entry should be a list of four elements:
1. The pdf's filename, 2. How many pages in the pdf to add to the page nr,
3. The first page of the index in the pdf, 4. the last page of the index.

Needs to be customized before org-lookup-dnd will work at all."
  :type '(repeat (list :tag ""
		       (string :tag  "Path to pdf        ")
		       (integer :tag "Page offset        ")
		       (integer :tag "First page of index")
		       (integer :tag "Last page of index ")))
  :set #'org-lookup-dnd-new-config
  :group 'org-lookup-dnd)


(defcustom org-lookup-dnd-link-format "[[pdfview:%s::%d][%s]]"
  "Format string to be inserted at point with ‘org-lookup-dnd-at-point’.
The first replacement is the path to the pdf.
The second is the page number in the pdf,
and the third is the link title."
  :type '(string)
  :group 'org-lookup-dnd)


(provide 'org-lookup-dnd)

;;; org-lookup-dnd.el ends here
