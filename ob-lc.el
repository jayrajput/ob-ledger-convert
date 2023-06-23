;;; ob-lc.el --- Babel functions for converting anything to ledger   -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Jay Rajput <jayrajput@gmail.com>

;; Author: Jay Rajput <jayrajput@gmail.com>
;; Homepage: https://github.com/jayrajput/ob-ledger-convert
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-babel support for converting anything to ledger.  Provides
;; parses for some Indian bank/card.  New parsers can be added easily.

(require 'ob)
(require 'cl-lib)

(defvar org-babel-default-header-args:lc
  '((:wrap . "example hledger"))
  "Default arguments to use when evaluating a lc source block.")

(defvar ob-lc-account-misc
  "expenses:misc"
  "Miscellaneous account used when no account can be deduced from description.")

(defvar ob-lc-regex-account-map
  '(("GROCERY" "expenses:grocery")
    ("MEDICAL"  "expenses:med")
    ("UTILITIES"  "expenses:utility")
    ("EDUCATION"  "expenses:education")
    ("CASHBACK"  "income:cashback")))

(defvar-local ob-lc-opening-bal nil
  "Holds the opening-bal used by parsers to find if the entry is credited/debited and closing balance.")

(defvar-local ob-lc-my-account nil
  "Read only variable in the code which is set to the argument passed to the org-babel-execute:lc.")

;; Main 

(defun org-babel-execute:lc (body params)
  (if-let ((verbatim (assoc :verbatim params)))
      body
    (let* ((parser (cdr (or (assq :parser params)
			    (error "You need to specify a :parser parameter"))))
	   (my-account (cdr (or (assq :account params)
				(error "You need to specufy a :account parameter"))))
	   (opening-bal (cdr (or (assq :opening-bal params)
				 (error "You need to specify a :opening-bal parameter")))))
      (setq ob-lc-opening-bal opening-bal)
      (setq ob-lc-my-account my-account)
      (thread-last
	(split-string (string-trim body) "\n" t)
	(mapcar (intern parser))
	(mapcar #'ob-lc-format-to-ledger)
	(mapconcat #'identity)))))

;; Parsers

(defun ob-lc-parser-date-desc-amount-total (items)
  "Convert ITEMS into ledger format. 

ITEMS is a list of date, description, amount, total.  Description can
be one or more elements.  It is used by other parsers."
  (let* ((date (ob-lc-ledger-date (nth 0 items)))
	 (description (string-join (cl-subseq items 1 -2) " "))
	 (amount (ob-lc-amount-to-number (ob-lc-nth-neg -2 items)))
	 (closing-bal (ob-lc-amount-to-number (ob-lc-nth-neg -1 items)))
	 (credited (> closing-bal ob-lc-opening-bal))
	 (other-account (ob-lc-get-account-from-desc description))
	 (from-account (if credited other-account ob-lc-my-account))
	 (to-account (if credited ob-lc-my-account other-account)))
    (setq ob-lc-opening-bal closing-bal)
    (list date description amount from-account to-account)))

;; all the bank/card lines are converted to a list containing date, description, amount, closing-bal, ob-lc-my-account
;; ob-lc-line

;; Transaction
;; (cl-defstruct ob-lc-txn
;;   date description amount opening-bal closing-bal from-account to-account)

;; (defun ob-lc--get-accounts (ob-lc-my-account other-account opening-bal closing-bal)
;;   (if (> closing-bal opening-bal)
;;       (list other-account ob-lc-my-account)
;;     (list ob-lc-my-account other-account)))

(defun ob-lc-parser-icici-amazonpay (line)
  "Convert LINE into ledger for ICICI Amazonpay."
  (let* ((items (split-string line))
	 (debit (string-equal (ob-lc-nth-neg -1 items) "CR"))
	 (items (if debit (cl-subseq items 0 -1) items))
	 (amount (ob-lc-amount-to-number (ob-lc-nth-neg -1 items)))
	 (closing-bal (if debit (+ ob-lc-opening-bal amount) (- ob-lc-opening-bal amount))))
    (ob-lc-parser-date-desc-amount-total
     (append items (list closing-bal)))))

(defun ob-lc-parser-axis-ace (line)
  "Convert LINE into ledger for AXIS ACE."
  (let* ((items (split-string line))
	 (amount1 (ob-lc-amount-to-number (ob-lc-nth-neg -4 items)))
	 (unit1 (ob-lc-nth-neg -3 items))
	 (amount2 (ob-lc-amount-to-number (ob-lc-nth-neg -2 items)))
	 (unit2 (ob-lc-nth-neg -1 items))
	 (cr-amount (if (string-equal unit1 "Cr") amount1 amount2))
	 (dr-amount (if (string-equal unit1 "Dr") amount1 amount2))
	 (amount (if (and (> cr-amount dr-amount) (zerop dr-amount)) cr-amount dr-amount))
	 (closing-bal (if (equal amount cr-amount) (+ ob-lc-opening-bal amount) (- ob-lc-opening-bal amount))))
    (ob-lc-parser-date-desc-amount-total
     (append (cl-subseq items 0 -4) (list amount closing-bal)))))

(defun ob-lc-parser-axis (line)
  "Convert LINE into ledger for AXIS."
  (ob-lc-parser-date-desc-amount-total
   (split-string line)))

(defun ob-lc-parser-hdfc (line)
  "Convert LINE into ledger for HDFC."
  (let* ((items (split-string line))
	 (closing-bal (ob-lc-amount-to-number (ob-lc-nth-neg -1 items)))
	 (debit-amount (ob-lc-amount-to-number (ob-lc-nth-neg -2 items)))
	 (credit-amount (ob-lc-amount-to-number (ob-lc-nth-neg -3 items)))
	 (amount (if (zerop debit-amount) credit-amount debit-amount)))
    (ob-lc-parser-date-desc-amount-total
     (append (cl-subseq items 0 -3) (list amount closing-bal)))))

(defun ob-lc-parser-idfcfirstb (line)
  "Convert LINE into ledger for IDFC First Bank."
  (let* ((items (split-string line))
	 (date-str (format-time-string "%d-%m-%Y" (date-to-time (mapconcat #'prin1-to-string (cl-subseq items 0 3))))))
    (ob-lc-parser-date-desc-amount-total
     (append (list date-str) (cl-subseq items 7 -1)))))

;; Utility functions

(defun ob-lc-get-account-from-desc (description)
  "Categorize the given description based on partial matches using the category mapping list."
  (let ((matched-category (cl-loop for (category-key  . category-value) in ob-lc-regex-account-map
				   when (string-match-p (if (numberp category-key) (number-to-string category-key) category-key) description)
				   return (car  category-value))))
    (or matched-category ob-lc-account-misc)))

(defun ob-lc-ledger-date (date-string)
  "Converts a DATE-STRING in the format DD/MM/YYYY or DD-MM-YYYY to YYYY/MM/DD format"
  (let* ((date-components (mapcar #'string-to-number (split-string date-string "[/-]")))
	 (day (nth 0 date-components))
	 (month (nth 1 date-components))
	 (year (nth 2 date-components)))
    (format-time-string "%Y-%m-%d" (encode-time 0 0 0 day month year))))

(defun ob-lc-amount-to-number (amount)
  "Convert AMOUNT to number after removing commas."
  (if (numberp amount) amount (string-to-number (replace-regexp-in-string "," "" amount))))

(defmacro ob-lc-nth-neg (index list)
  "nth supporting negative INDEX for LIST."
  `(nth (if (>= ,index 0) ,index (+ (length ,list) ,index)) ,list))

(defun ob-lc-format-to-ledger (expense)
  "Format the EXPENSE as per ledger."
  (let* ((date (car expense))
	 (description (nth 1 expense))
	 (expense-amount (nth 2 expense))
	 (from-account (nth 3 expense))
	 (to-account (nth 4 expense)))
    (format "%s  %s\n    %s     %.2f INR\n    %s\n\n"
	    date
	    description
	    to-account
	    expense-amount
	    from-account)))

(provide 'ob-lc)
