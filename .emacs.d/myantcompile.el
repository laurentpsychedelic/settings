;;; myantcompile.el --- This a custom function collection to invoke ant commands (NetBeans project) from a Java source file

;; Copyright (C) 2013  

;; Author:  <LaurentDev@LAURENT-DEV>
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

(defun get-java-package-info (text)
  "Return package info (Fully-Qualified-Name and path (with \"/\")"
  (with-temp-buffer
    (let (package package-path)
      (insert text)
      (goto-char 1)
      (re-search-forward "package[[:space:]]+\\([a-zA-Z]+\\([.][a-zA-Z]+\\)*\\)" nil t)
      (setq package (match-string 1))
      (setq package-path
            (mapconcat
             'identity
             (split-string package "\\.")
             "/"))
      (list package package-path))))

(defun get-class-name (text &optional package-fqn)
  "Return package info (Fully-Qualified-Name and path (with \"/\")"
  (with-temp-buffer
    (let (class-name)
      (insert text)
      (goto-char 1)
      (re-search-forward "class[[:space:]]+\\([a-zA-Z]+\\)[[:space:]]*[{]?" nil t)
      (setq class-name (match-string 1))
      (if (boundp 'package-fqn)
          (list class-name (mapconcat 'identity (list package-fqn class-name) "."))
        (list class-name nil)))))

(defun get-build-file-relative-location ()
  "Find relative path to build file by scanning upstream folders"
  (let (relative-path index package-path)
    (setq index 0)
    (while (< index 10)
      ; (setq package-path (directory-file-name (concat (mapconcat 'identity (make-list index "..") "/"))))
      (setq relative-path (mapconcat
              'identity
              (make-list index "..")
              "/"))
      (if (or (file-exists-p (concat relative-path "/build.xml"))
              (file-exists-p (concat relative-path "/Makefile"))
              (file-exists-p (concat relative-path "/src")))
                 (progn ; (message "build file (or src folder) found!") 
                        (setq level index)
                        (setq index 10))
        (setq index (1+ index))))
    (setq build-file-path (mapconcat 'identity (make-list level "..") "/"))))

(defun get-package-fqn-from-dir-tree ()
  "Get Java package fully-qualified name from directory tree (guess)"
  (let ((package-path "") (package-fqn "") (index 0) (relative-path ""))
    (while (< index 10)
      (setq relative-path (mapconcat
              'identity
              (make-list index "..")
              "/"))
      (if (or (file-exists-p (concat relative-path "/build.xml")) 
              (file-exists-p (concat relative-path "/Makefile")) 
              (file-exists-p (concat relative-path "/src")))
          (progn ; (message "build file (or src folder) found!") 
                        (message "package-path: %s" package-path) ; !!
                        (setq level index)
                        (setq index 10))
        (progn
          (if (> index 0) (setq package-path (concat (nth 1 (reverse (split-string (file-name-directory (file-truename (concat buffer-file-name (mapconcat
                                                                                                                                                          'identity
                                                                                                                                                          (make-list index "..")
                                                                                                                                                          "/")))) "/"))) (if (> index 1) "/" "") package-path)))
                   (setq index (1+ index)))))
    (setq package-fqn (mapconcat 'identity (split-string package-path "/") "."))
    ; (message "FQN: %s" package-fqn)
    (setq package-path package-fqn)))

(defun get-ant-basic-command-element (text)
  "Get ant command basic elements"
  (let (basic-command-elements package-fqn package-path class-name class-fqn build-file-relative-location)
    (let (package-info)
      (setq package-info (get-java-package-info text))
      (setq package-fqn (nth 0 package-info))
      (setq package-path (nth 1 package-info)))
    (let (class-info)
      (setq class-info (get-class-name text package-fqn))
      (setq class-name (nth 0 class-info))
      (setq class-fqn (nth 1 class-info)))
    (setq build-file-relative-location (get-build-file-relative-location))
    ;; (message "Package FQN : %s" package-fqn)
    ;; (message "Package path: %s" package-path)
    ;; (message "Class FQN : %s" class-fqn)
    ;; (message "Class name: %s" class-name)
    ;; (message "Build file relative location: %s" build-file-relative-location)
    (setq basic-command-elements (list package-fqn package-path class-fqn class-name build-file-relative-location))))

(defun get-ant-test-single-command (text filename-sans-extension)
  "Get command for ant test-single target"
  (let (basic-command-elements package-fqn package-path class-name class-fqn build-file-relative-location command)
    (setq basic-command-elements (get-ant-basic-command-element text))
    (setq package-fqn (nth 0 basic-command-elements))
    (setq package-path (nth 1 basic-command-elements))
    (setq class-fqn (nth 2 basic-command-elements))
    (setq class-name (nth 3 basic-command-elements))
    (setq build-file-relative-location (nth 4 basic-command-elements))
    (setq command (concat "pwd && cd " build-file-relative-location " && "
                          "ant -emacs test-single "
                          "-Djavac.includes=" (replace-regexp-in-string "[.][.]" "*" build-file-relative-location) ".java "
                          "-Dtest.includes=" package-path "/" filename-sans-extension ".java"))))

(defun get-ant-run-single-command (text filename-sans-extension)
  "Get command for ant run-single target"
  (let (basic-command-elements package-fqn package-path class-name class-fqn build-file-relative-location command)
    (setq basic-command-elements (get-ant-basic-command-element text))
    (setq package-fqn (nth 0 basic-command-elements))
    (setq package-path (nth 1 basic-command-elements))
    (setq class-fqn (nth 2 basic-command-elements))
    (setq class-name (nth 3 basic-command-elements))
    (setq build-file-relative-location (nth 4 basic-command-elements))
    (setq command (concat "pwd && cd " build-file-relative-location " && "
                          "ant -emacs run-single "
                          "-Djavac.includes=" (replace-regexp-in-string "[.][.]" "*" build-file-relative-location) ".java "
                          "-Drun.class=" class-fqn))))

(defun get-ant-compile-command (text filename-sans-extension)
  "Get command for ant compile target"
  (let (basic-command-elements package-fqn package-path class-name class-fqn build-file-relative-location command)
    (setq basic-command-elements (get-ant-basic-command-element text))
    (setq package-fqn (nth 0 basic-command-elements))
    (setq package-path (nth 1 basic-command-elements))
    (setq class-fqn (nth 2 basic-command-elements))
    (setq class-name (nth 3 basic-command-elements))
    (setq build-file-relative-location (nth 4 basic-command-elements))
    (setq command (concat "pwd && cd " build-file-relative-location " && "
                          "ant -emacs compile"))))

(defun get-ant-clean-command (text filename-sans-extension)
  "Get command for ant clean target"
  (let (basic-command-elements package-fqn package-path class-name class-fqn build-file-relative-location command)
    (setq basic-command-elements (get-ant-basic-command-element text))
    (setq package-fqn (nth 0 basic-command-elements))
    (setq package-path (nth 1 basic-command-elements))
    (setq class-fqn (nth 2 basic-command-elements))
    (setq class-name (nth 3 basic-command-elements))
    (setq build-file-relative-location (nth 4 basic-command-elements))
    (setq command (concat "pwd && cd " build-file-relative-location " && "
                          "ant -emacs clean"))))

(defun get-ant-jar-command (text filename-sans-extension)
  "Get command for ant jar target"
  (let (basic-command-elements package-fqn package-path class-name class-fqn build-file-relative-location command)
    (setq basic-command-elements (get-ant-basic-command-element text))
    (setq package-fqn (nth 0 basic-command-elements))
    (setq package-path (nth 1 basic-command-elements))
    (setq class-fqn (nth 2 basic-command-elements))
    (setq class-name (nth 3 basic-command-elements))
    (setq build-file-relative-location (nth 4 basic-command-elements))
    (setq command (concat "pwd && cd " build-file-relative-location " && "
                          "ant -emacs jar"))))

(defun get-ant-run-command (text filename-sans-extension)
  "Get command for ant jar target"
  (let (basic-command-elements package-fqn package-path class-name class-fqn build-file-relative-location command)
    (setq basic-command-elements (get-ant-basic-command-element text))
    (setq package-fqn (nth 0 basic-command-elements))
    (setq package-path (nth 1 basic-command-elements))
    (setq class-fqn (nth 2 basic-command-elements))
    (setq class-name (nth 3 basic-command-elements))
    (setq build-file-relative-location (nth 4 basic-command-elements))
    (setq command (concat "pwd && cd " build-file-relative-location " && "
                          "ant -emacs run"))))

(defun get-jdb-command (text filename-sans-extension)
  "Get command for ant jar target"
  (let (basic-command-elements package-fqn package-path class-name class-fqn build-file-relative-location command)
    (setq basic-command-elements (get-ant-basic-command-element text))
    (setq package-fqn (nth 0 basic-command-elements))
    (setq package-path (nth 1 basic-command-elements))
    (setq class-fqn (nth 2 basic-command-elements))
    (setq class-name (nth 3 basic-command-elements))
    (setq build-file-relative-location (nth 4 basic-command-elements))
    (setq command (concat "jdb -sourcepathsrc -classpathbuild/classes;lib/*;lib/other_libs/* " class-fqn))))

(defun ant-test-single ()
  "Set ant test-single compilation target command into compilation buffer"
  (interactive)
  (setq compile-command (get-ant-test-single-command (buffer-string) (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
  (call-interactively 'compile compile-command))

(defun ant-run-single ()
  "Set ant run-single compilation target command into compilation buffer"
  (interactive)
  (setq compile-command (get-ant-run-single-command (buffer-string) (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
  (call-interactively 'compile compile-command))

(defun ant-compile ()
  "Set ant compile compilation target command into compilation buffer"
  (interactive)
  (setq compile-command (get-ant-compile-command (buffer-string) (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
  (call-interactively 'compile compile-command))

(defun ant-clean ()
  "Set ant clean compilation target command into compilation buffer"
  (interactive)
  (setq compile-command (get-ant-clean-command (buffer-string) (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
  (call-interactively 'compile compile-command))

(defun ant-jar ()
  "Set ant jar compilation target command into compilation buffer"
  (interactive)
  (setq compile-command (get-ant-jar-command (buffer-string) (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
  (call-interactively 'compile compile-command))

(defun ant-run ()
  "Set ant run compilation target command into compilation buffer"
  (interactive)
  (setq compile-command (get-ant-run-command (buffer-string) (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
  (call-interactively 'compile compile-command))

(defun jdb-run ()
  "Set jdb compilation target command into compilation buffer"
  (interactive)
  (let (directory filename command buff)
    (setq filename (buffer-file-name))
    (setq command (get-jdb-command (buffer-string) (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    (setq directory (concat (file-name-directory filename) (get-build-file-relative-location)))
    (setq buff (find-file-other-window directory))
     (message "Command: %s" command)
     (message "Dir: %s" directory)
    (with-current-buffer buff
        (jdb command))))

(provide 'myantcompile)
;;; myantcompile.el ends here
