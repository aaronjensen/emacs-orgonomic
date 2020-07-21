;;; orgonomic.el --- More ergonomic org commands -*- lexical-binding:t -*-

;; Copyright (C) 2020 Aaron Jensen

;; Author: Aaron Jensen <aaronjensen@gmail.com>
;; Keywords: org-mode
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; More ergonomic org commands.

;;; Code:

(defvar orgonomic-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "-") #'orgonomic-minus)
    (define-key map (kbd "RET") #'orgonomic-return)
    (define-key map (kbd "S-RET") #'orgonomic-shift-return)
    (define-key map (kbd "<S-return>") #'orgonomic-shift-return)
    (define-key map (kbd "<backspace>") #'orgonomic-delete-backward-char)
    map))

(defun orgonomic--in-empty-item ()
  "Return t if in an empty list item, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (looking-at (concat org-list-full-item-re " ?$"))))

;;;###autoload
(defun orgonomic-return (&optional ignore)
  "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
  (interactive "P")
  (if ignore
      (org-return)
    (cond
     ;; Break lines like normal
     ((eq 'line-break (car (org-element-context)))
      (org-return-indent))
     ;; Open links like usual
     ((and (eq 'link (car (org-element-context))) (not (eolp)))
      (org-open-at-point-global))
     ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
     ;; Johansson!
     ((and (fboundp 'org-inlinetask-in-task-p)
           (org-inlinetask-in-task-p))
      (org-return))
     ;; If we are in an item and not at the beginning of the line...
     ((and (org-in-item-p) (not (bolp)))
      (cond
       ;; Delete the bullet if on an empty item
       ((orgonomic--in-empty-item)
        (delete-region (line-beginning-position) (line-end-position)))
       ;; add checkboxes
       ((and (eolp) (org-at-item-checkbox-p))
        (org-insert-todo-heading nil))
       ;; Insert a new bullet if at the end of the line
       ((eolp)
        (org-meta-return))
       ;; Otherwise, just return normally
       (t
        (org-return))))
     ((org-at-heading-p)
      (cond
       ;; If there's no title, delete the *'s
       ((string= "" (org-element-property :title (org-element-context)))
        (delete-region (line-beginning-position) (line-end-position)))
       ((eolp)
        (org-meta-return))
       (t
        (org-return))))
     ((and (org-at-table-p)
           (org-table-check-inside-data-field t))
      (if (-any?
           (lambda (x) (not (string= "" x)))
           (nth
            (- (org-table-current-dline) 1)
            (-reject (lambda (row) (eq row 'hline)) (org-table-to-lisp))))
          (org-return)
        ;; empty row
        (delete-region (line-beginning-position) (line-end-position))
        (org-return)))
     (t
      (org-return)))))

;;;###autoload
(defun orgonomic-shift-return (n)
  "Copy down if in table or insert newline and indent."
  (interactive "p")
  (if (org-at-table-p)
      (org-table-copy-down n)
    (org-return-indent)))

;;;###autoload
(defun orgonomic-delete-backward-char (&optional n)
  "Delete checkboxes, list item bullets and demote headlines."
  (interactive "P")
  (cond
   (n
    (org-delete-backward-char n))
   ((org-at-item-p)
    (let ((point (point))
          (bullet-beginning (match-beginning 1))
          (bullet-end (match-end 1)))
      (cond
       ;; If we are near the end of the bullet...
       ((or (eql bullet-end point)
            (eql bullet-end (- point 1)))
        ;; See if we are in a nested list...
        (if (save-excursion
              (goto-char bullet-end)
              (eql
               'item
               (car
                (org-element-property
                 :parent
                 (org-element-property :parent (org-element-context))))))
            ;; And if so, outdent
            (org-metaleft)
          ;; Otherwise, delete the bullet
          (delete-region bullet-beginning point)))
       ;; If we are near the end of a checkbox...
       ((and (org-at-item-checkbox-p)
             (or (eql (match-end 1) point)
                 (eql (match-end 1) (- point 1))))
        ;; Delete the entire checkbox
        (delete-region (match-beginning 1) point))
       (t
        (org-delete-backward-char 1)))))
   ((org-at-heading-p)
    (cond
     ;; Delete top level headline *, including trailing space
     ((looking-back "^\\* " 0)
      (org-ctrl-c-star))
     ;; Promote headline
     ((looking-back "^\\*\\*+ " 0)
      (org-metaleft))
     ;; Otherwise, delete normally
     (t
      (org-delete-backward-char 1))))
   ;; Normal delete
   (t
    (org-delete-backward-char 1))))

;;;###autoload
(defun orgonomic-minus (N)
  "Convert a headline to a list item if at the beginning of an empty headline, otherwise insert -."
  (interactive "p")
  (if (and (org-at-heading-p)
           (looking-back "\\* " 0)
           (string= "" (org-element-property :title (org-element-context))))
      (progn
        (org-toggle-item 0)
        (end-of-line))
    (org-self-insert-command N)))

;;;###autoload
(define-minor-mode orgonomic-mode
  "When active, RET and backspace will behave more like they would in a word
processor."
  :keymap orgonomic-mode-map)

(provide 'orgonomic)
;;; orgonomic.el ends here
