;;; -*- Mode:Emacs-Lisp -*-
;;; 
;;; Copyright (c) 2002 Jim Hourihan
;;;
;;; bbdb-vcard-export.el is free software you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at
;;; your option) any later version.
;;;
;;; This software is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Author: Jim Hourihan <jimh@panix.com>
;;; Created: 2002-08-08
;;; Version: 1.0
;;; Keywords: vcard ipod
;;;
;;; I use this code to sync my ipod with bbdb under OS X. To do so:
;;;
;;;	M-x bbdb-vcard-export-update-all
;;;
;;; and enter `/Volumes/IPOD_NAME/Contacts/' at the prompt
;;;

(require 'bbdb)

(defun bbdb-vcard-export-address-string (address)
  "Return the address string"
  (let ((streets (bbdb-address-streets address))
	(city (bbdb-address-city address))
	(state (bbdb-address-state address))
	(country (bbdb-address-country address))
	(zip (bbdb-address-zip address)))
    (concat 
     "adr;type=" (bbdb-address-location address) ":"
     (cond 
      ((eq (length streets) 3)
       (concat (nth 0 streets) ";" (nth 1 streets) ";" (nth 2 streets)))
      ((eq (length streets) 2)
       (concat ";" (nth 0 streets) ";" (nth 1 streets)))
      ((eq (length streets) 1)
       (concat ";;" (nth 0 streets)))
      (t ";;;"))
     ";" city ";" state ";" zip ";" country)))
	

(defun bbdb-vcard-export-record-insert-vcard (record)
  "Insert a vcard formatted version of RECORD into the current buffer"
  (let ((name (bbdb-record-name record))
	(first-name (elt record 0))
	(last-name (elt record 1))
	(company (elt record 3))
	(notes (bbdb-record-notes record))
	(phones (bbdb-record-phones record))
	(addresses (bbdb-record-addresses record))
	(net (bbdb-record-net record)))
    (insert "begin:vcard\n"
	    "version:3.0\n"
	    (concat "fn:" name "\n")
	    (concat "n:" last-name ";" first-name ";;;\n"
		    (if company (concat "org:" company "\n"))
		    (if notes (concat "note:" notes "\n"))))
    (if phones
	(while phones
	  (insert "tel;type=" (bbdb-phone-location (car phones)) ":"
		  (bbdb-phone-string (car phones)) "\n")
	  (setq phones (cdr phones))))
    (if addresses
	(while addresses
	  (insert (bbdb-vcard-export-address-string (car addresses)) "\n")
	  (setq addresses (cdr addresses))))
    (if net
	(while net
	  (insert "email;type=internet:" (car net) "\n")
	  (setq net (cdr net))))
    (insert "end:vcard\n")))
		    
(defun bbdb-vcard-export-vcard-name-from-record (record)
  "Come up with a vcard name given a record"
  (let ((name (bbdb-record-name record))
	(first-name (elt record 0))
	(last-name (elt record 1)))
    (concat first-name "_" last-name ".vcf")))

(defun bbdb-vcard-export-make-vcard (record vcard-name)
  "Make a record buffer and write it"
  (let ((buffer (get-buffer-create "*bbdb-vcard-export*")))
    (save-excursion
      (set-buffer buffer)
      (kill-region (point-min) (point-max))
      (bbdb-vcard-export-record-insert-vcard record)
      (write-region (point-min) (point-max) vcard-name))
    (kill-buffer buffer)))

(defun bbdb-vcard-export-update-all (output-dir)
  "Update the vcard Contacts directory from the bbdb database"
  (interactive "DDirectory to update: ")
  (bbdb ".*" nil)
  (let ((records (bbdb-records)))
    (while records
      (let ((record (car records)))
	(message "Updating %s" (bbdb-record-name record))
	(bbdb-vcard-export-make-vcard 
	 record
	 (concat output-dir
		 (bbdb-vcard-export-vcard-name-from-record record)))
	(setq records (cdr records))))))
