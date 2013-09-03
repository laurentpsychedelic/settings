;;; javaimport.el --- This module allows inserting imports in Java and Groovy source code files, based on the current word. The classes to be  mported are scanned in the current project file tree, the standard Java (and Groovy) SDK APIs, and in the libs (JAR files) used in the project.

;; Copyright (C) 2013  Laurent FABRE

;; Author: Laurent FABRE <laurentdev@laurentdev-CF-S10CYBDR>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(setq javaimport-valid-object-name "[a-zA-Z_][a-zA-Z_0-9]*")
(setq javaimport-package-regexp (concat "package[[:space:]]+\\(" javaimport-valid-object-name "\\([.]" javaimport-valid-object-name "\\)*\\)"))
(setq javaimport-class-regexp (concat "\\(\\(public\\)[[:space:]]+\\)?\\(\\(class\\)\\|\\(interface\\)\\)[[:space:]]+\\(" javaimport-valid-object-name "\\)"))
(setq javaimport-class-regexp-class-name-index 6)
(setq javaimport-class-regexp-class-access-modifier-index 2)

(defun javaimport-compute-brace-differential (text)
  "Compute the differential of right - left curved braces in given text"
  (let (n-left n-right)
    (setq n-left (javaimport-count-occurences-of "{" text))
    (setq n-right (javaimport-count-occurences-of "}" text))
    (- n-left n-right)))

(defun javaimport-count-occurences-of (pattern text)
  "Count occurence of given pattern (pure text) in given text"
  (with-temp-buffer
    (let ((n 0))
      (setq case-fold-search nil)
      (insert text)
      (beginning-of-buffer)
      (while (re-search-forward (regexp-quote pattern) nil t)
        (setq n (1+ n)))
      n)))

(defun javaimport-combine-sub-class-with-parent-class (parent-class sub-class offset)
  "Combine class names depending on their inclusion relation defined by offset.
 If sub-class is an internal class directly defined within parent-class offset is equal to 1
 (parent class name +1) and 'parent-class.sub-class' is returned (ex: from 'InternalClass'
 defined within 'ParentClass' class, 'ParentClass.InternalClass'). 
If the offset is 0, their is no parent/internal class relation (they are at the same level), 
thus sub-class is returned as it. For negative offsets, sub-class is actually at a higher
 level than parent-class. The according number of level is deleted from parent-class and 
sub-class is added at the end (ex: with 'ParentClass.InternalClass' and 'AnOtherClass' with 
offset=-1, 'AnOtherClass' is returned"
  (if parent-class
      (let (parent-eles npop)
        (setq parent-eles (split-string parent-class "[.]")) ; elements of the parent class name
        (setq npop (1+ (- offset))) ; number of elements to pop from parent class before appending sub-class elements
        (mapc (lambda (ele) (pop parent-eles)) (make-list npop 1))
        (setq sub-class (mapconcat 'identity (append parent-eles (cons sub-class '())) "."))))
  sub-class)

(defun javaimport-scan-package-in-source (source-code)
  "Scan source and return the package declared inside"
  (string-match javaimport-package-regexp source-code)
  (match-string 1 source-code))

(defun javaimport-remove-all-comments-in-buffer ()
  "Remove all comments in current buffer"
  (interactive)
  (beginning-of-buffer)
  (while ; // comments
      (re-search-forward (regexp-quote "//") nil t)
    (goto-char (match-beginning 0))
    (kill-line))
  (beginning-of-buffer)
  (while ; /* ... */ comments
      (re-search-forward (regexp-quote "/*") nil t)
    (goto-char (match-beginning 0))
    (push-mark-command nil)
    (re-search-forward (regexp-quote "*/"))
    (kill-region (point) (mark))))

(defun javaimport-remove-all-string-litterals-in-buffer ()
  "Remove all comments in current buffer"
  (interactive)
  (beginning-of-buffer)
  (while (re-search-forward (regexp-quote "[^\\][\"]") nil t)
    (goto-char (match-beginning 0))
    (push-mark-command nil)
    (forward-char)
    (re-search-forward (regexp-quote "\""))
    (kill-region (point) (mark))))

(defun javaimport-get-all-files-with-matching-extension (extension dir)
  "Get all files with matching extension in specified directory"
  (split-string (shell-command-to-string (concat "find " dir " -iname \\*." extension))))
; (javaimport-get-all-files-with-matching-extension "groovy" "/home/laurentdev/dev/SE-View_101.git/")

; (message (format "All detected classes: %s" (javaimport-get-all-classes-defined-in-dir "/home/laurentdev/dev/SE-View_101.git/")))
(defun javaimport-get-all-classes-defined-in-dir (dir)
  "Get the list of all files defined in the source files in the current directory"
  (let ((class-list ()) (file-list ()))
    (mapc (lambda (extension) (setq file-list (append (javaimport-get-all-files-with-matching-extension extension dir) file-list)))
          (list "java" "groovy"))
    (mapc (lambda (filepath) (setq class-list (append (javaimport-scan-defined-classes-in-source (javaimport-get-file-contents filepath)) class-list)))
          file-list)
    class-list))

(defun javaimport-get-file-contents (filepath)
  "Get file contents as a string"
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(defun javaimport-test-scan-defined-classes-in-source ()
  "Test..."
  (interactive)
  (message (format "Classes: %s" (javaimport-scan-defined-classes-in-source (buffer-string)))))
(defun javaimport-scan-defined-classes-in-source (source-code)
  "Scan source code and return a list of the classes defined within it"
  (with-temp-buffer
    (let ((package "") (class-list ()) (access-modifier) (curr-class nil) (last-class nil) (class-offset 0) (curr-point 1) (last-point 1) (interval-text ""))
      (insert source-code)
      (javaimport-remove-all-comments-in-buffer)
      (javaimport-remove-all-string-litterals-in-buffer)
      (beginning-of-buffer)
      (setq case-fold-search nil)
      (setq package (javaimport-scan-package-in-source source-code))
      (while (setq curr-point (re-search-forward javaimport-class-regexp nil t))
        (setq curr-class (match-string javaimport-class-regexp-class-name-index))
        (setq access-modifier (match-string javaimport-class-regexp-class-access-modifier-index))
        (setq interval-text (substring (buffer-string) last-point curr-point))
        (setq class-offset (javaimport-compute-brace-differential interval-text))
        (setq curr-class (javaimport-combine-sub-class-with-parent-class last-class curr-class class-offset))
        (add-to-list 'class-list (list curr-class (if access-modifier access-modifier "package-private")))
        (setq last-class curr-class)
        (setq last-point curr-point))
      (setq class-list (mapcar (lambda (ele) (if package (list (concat package "." (car ele)) (car (nreverse ele))) ele)) class-list))
      class-list)))

(provide 'javaimport)
;;; javaimport.el ends here
