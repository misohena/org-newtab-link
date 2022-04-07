;;; org-newtab-link.el --- Open Org Link in New Tab  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
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

;; 

;;; Code:

(eval-when-compile
  (require 'ox))

;;;; Filter Function

(defun org-newtab-link-filter (s backend _info)
  (if (and
       ;;@todo Support <a data-ex=">" href=...>
       (with-no-warnings ;; lazy load ox.el
         (org-export-derived-backend-p backend 'html)) ;; html only
       (not (string-match "\\`[^>]* target=\"" s)) ;; has no target=
       (not (string-match "\\`[^>]* rel=\"" s)) ;; has no rel=
       (string-match "\\`[^>]* href=\"[^#]" s) ;;not internal link
       (string-match "\\`<a " s)) ;; a tag
      (replace-match "<a target=\"_blank\" rel=\"noopener\" " t t s)
    s))

;;;; Export Options

(defvar org-newtab-link-enabled nil)
(put 'org-newtab-link-enabled 'safe-local-variable #'booleanp)

(defvar org-newtab-link-options-alist
  '((:newtab-link-enabled "HTML_LINK_NEWTAB" nil org-newtab-link-enabled)))

(defun org-newtab-link-filter-opt (s backend info)
  (if (not (member (plist-get info :newtab-link-enabled) '(nil "" "nil" "no")))
      (org-newtab-link-filter s backend info)
    s))

;; Modify HTML Backend

(defun org-newtab-link-modify-html-backend ()
  (require 'ox-html)
  (let ((backend (with-no-warnings ;;lazy load ox.el
                   (org-export-get-backend 'html))))
    ;; Add options defined by org-newtab-link-options-alist to backend
    (let ((backend-options (org-export-backend-options backend))
          (new-option-names (mapcar #'car org-newtab-link-options-alist)))
      (setf (org-export-backend-options backend)
            (nconc
             (seq-remove (lambda (elem) (memq (car elem) new-option-names))
                         backend-options)
             org-newtab-link-options-alist)))

    ;; Add org-newtab-link-filter-opt to backend
    (let ((filter-link (assq :filter-link
                                   (org-export-backend-filters backend))))
      ;; null => (:filter-link . ())
      (when (null filter-link)
        (push (setq filter-link (list :filter-link))
              (org-export-backend-filters backend)))
      ;; (:filter-link . function) => (:filter-link . (function))
      (when (functionp (cdr filter-link))
        (setcdr filter-link (list (cdr filter-link))))
      ;; Add my filter function
      (when (not (memq 'org-newtab-link-filter-opt (cdr filter-link)))
        (push 'org-newtab-link-filter-opt (cdr filter-link))))))

(with-eval-after-load "ox-html"
  (org-newtab-link-modify-html-backend))


(provide 'org-newtab-link)
;;; org-newtab-link.el ends here
