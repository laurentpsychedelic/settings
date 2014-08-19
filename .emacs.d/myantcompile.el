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

(require 'dropdown-list)

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
      (re-search-forward ".*class[[:space:]]+\\([a-zA-Z]+\\)[[:space:]]*[{]?" nil t)
      (while (string-match ".*[*].*" (match-string 0))
        (re-search-forward ".*class[[:space:]]+\\([a-zA-Z]+\\)[[:space:]]*[{]?" nil t))
      (setq class-name (match-string 1))
      (if (boundp 'package-fqn)
          (list class-name (mapconcat 'identity (list package-fqn class-name) "."))
        (list class-name nil)))))

(defun get-build-file-relative-location ()
  "Find relative path to build file by scanning upstream folders"
  (interactive)
  (let ((relative-path nil) (build-file-name-regexps (list "build[.]\\(\\(xml\\)\\|\\(gradle\\)\\)" "Makefile" "src" ".*[.]vcproj")))
    (setq relative-path (get-build-file-relative-location-impl build-file-name-regexps))))

(defun get-build-file-relative-location-impl (build-file-name-regexps)
  "Find relative path to build file, whose name is match a regexp in the list provided"
  (interactive)
  (let ((path nil) (local-path nil))
    (setq path (catch 'loop 
                 (mapc (lambda (regexp)
                         (setq local-path (get-build-file-relative-location-impl-single regexp))
                         (when local-path (throw 'loop local-path)))
                       build-file-name-regexps)))
    (setq path (if (stringp path) path nil))))

(defun get-build-file-relative-location-impl-single (build-file-name-regex &optional with-filename)
  "Find relative path to the build file whose name matches the regexp provided"
  (let ((path "") (dir "") (files ""))
    (setq path (catch 'loop
      (mapc (lambda (ele) 
              (setq dir (mapconcat (lambda (subele) (if (> subele 0) ".." ".")) (number-sequence 0 ele) "/"))
              (setq files (directory-files dir))
              ;(message (format "Files in %s: %s" dir files))
              (mapc (lambda (file)
                      (when (string-match-p build-file-name-regex file) (throw 'loop (if with-filename (concat dir "/" file) dir))))
                    files))
            (number-sequence 0 10))))
    (setq path (if (stringp path) (replace-regexp-in-string "^[.]/[.][.]" ".." path) nil))))

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
            ; (message "package-path: %s" package-path)
            ;(setq level index)
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
    (setq command (concat "CDPATH=. && cd " build-file-relative-location " && "
                          "ant -emacs test-single "
                          "-Djavac.includes=" (replace-regexp-in-string "[.][.]" "*" build-file-relative-location) ".java "
                          "-Dtest.includes=" package-path "/" filename-sans-extension ".java"))))

(defun get-ant-run-single-command (text filename-sans-extension &optional extension)
  "Get command for ant run-single target"
  (let (basic-command-elements package-fqn package-path class-name class-fqn build-file-relative-location command)
    (setq basic-command-elements (get-ant-basic-command-element text))
    (setq package-fqn (nth 0 basic-command-elements))
    (setq package-path (nth 1 basic-command-elements))
    (setq class-fqn (nth 2 basic-command-elements))
    (setq class-name (nth 3 basic-command-elements))
    (setq build-file-relative-location (nth 4 basic-command-elements))
    (setq command (concat "CDPATH=. && cd " build-file-relative-location " && "
                          "ant -emacs run-single "
                          "-Djavac.includes=" (replace-regexp-in-string "[.][.]" "*" build-file-relative-location) "." (if extension extension "java") " "
                          "-Drun.class=" class-fqn))))

(defun get-file-contents (filepath)
  "Get file contents as a string"
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(defun get-basic-compile-command (commandname subcommand text filename-sans-extension &optional additional-options &rest rest-var)
  "Get command for ant-like compile target"
  (let (basic-command-elements package-fqn package-path class-name class-fqn build-file-relative-location command jvm-options)
    (setq basic-command-elements (get-ant-basic-command-element text))
    (setq package-fqn (nth 0 basic-command-elements))
    (setq package-path (nth 1 basic-command-elements))
    (setq class-fqn (nth 2 basic-command-elements))
    (setq class-name (nth 3 basic-command-elements))
    (setq build-file-relative-location (nth 4 basic-command-elements))
    (setq jvm-options (if additional-options (concat " " additional-options) ""))
    (if (file-exists-p (concat build-file-relative-location "/.antoptions")) 
        (setq jvm-options (concat jvm-options " " (get-file-contents (concat build-file-relative-location "/.antoptions")))))
    (setq command (concat "CDPATH=. && " (if rest-var (concat (mapconcat 'identity rest-var " && ") " && ") "") "cd " build-file-relative-location " && "
                          commandname jvm-options " " subcommand))))

(defun get-ant-compile-command (text filename-sans-extension &optional additional-options)
  "Get command for ant compile target"
  (get-basic-compile-command "ant" "compile" text filename-sans-extension (concat (if additional-options (concat additional-options " ") "") "-emacs")))

(defun get-ant-clean-command (text filename-sans-extension &optional additional-options)
  "Get command for ant clean target"
  (get-basic-compile-command "ant" "clean" text filename-sans-extension (concat (if additional-options (concat additional-options " ") "") "-emacs")))

(defun get-ant-jar-command (text filename-sans-extension &optional additional-options)
  "Get command for ant jar target"
  (get-basic-compile-command "ant" "jar" text filename-sans-extension (concat (if additional-options (concat additional-options " ") "") "-emacs")))

(defun get-ant-run-command (text filename-sans-extension &optional additional-options)
  "Get command for ant jar target" 
  (get-basic-compile-command "ant" "run" text filename-sans-extension (concat (if additional-options (concat additional-options " ") "") "-emacs")))

(defun get-gradle-compile-command (text filename-sans-extension)
  "Get command for gradle compile target"
  (get-basic-compile-command "gradle" "build" text filename-sans-extension "--info" "export JAVA_TOOL_OPTIONS=\"-Dfile.encoding=UTF-8 -Duser.language=en\"" ))

(defun get-gradle-clean-command (text filename-sans-extension)
  "Get command for gradle clean target"
  (get-basic-compile-command "gradle" "clean" text filename-sans-extension "--info" "export JAVA_TOOL_OPTIONS=\"-Dfile.encoding=UTF-8 -Duser.language=en\""))

(defun get-gradle-jar-command (text filename-sans-extension)
  "Get command for gradle jar target"
  (get-basic-compile-command "gradle" "jar" text filename-sans-extension "--info" "export JAVA_TOOL_OPTIONS=\"-Dfile.encoding=UTF-8 -Duser.language=en\""))

(defun get-gradle-run-command (text filename-sans-extension &optional additional-options)
  "Get command for gradle jar target"
  (get-basic-compile-command "gradle" "runJar" text filename-sans-extension (concat additional-options (if additional-options (concat additional-options " ") "") "--info") "export JAVA_TOOL_OPTIONS=\"-Dfile.encoding=UTF-8 -Duser.language=en\""))

(defun get-gradle-run-single-command (text filename-sans-extension &optional additional-options)
  "Get command for gradle jar target"
  (get-basic-compile-command "gradle" (concat "runSingle+" (nth 2 (get-ant-basic-command-element text))) text filename-sans-extension (concat additional-options (if additional-options (concat additional-options " ") "") "--info") "export JAVA_TOOL_OPTIONS=\"-Dfile.encoding=UTF-8 -Duser.language=en\""))

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
  (setq compile-command (get-ant-run-single-command (buffer-string) (file-name-sans-extension (file-name-nondirectory (buffer-file-name))) (file-name-extension (file-name-nondirectory (buffer-file-name)))))
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

(defun gradle-compile ()
  "Set gradle compile compilation target command into compilation buffer"
  (interactive)
  (setq compile-command (get-gradle-compile-command (buffer-string) (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
  (call-interactively 'compile compile-command))

(defun gradle-clean ()
  "Set gradle clean compilation target command into compilation buffer"
  (interactive)
  (setq compile-command (get-gradle-clean-command (buffer-string) (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
  (call-interactively 'compile compile-command))

(defun gradle-jar ()
  "Set gradle jar compilation target command into compilation buffer"
  (interactive)
  (setq compile-command (get-gradle-jar-command (buffer-string) (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
  (call-interactively 'compile compile-command))

(defun gradle-run ()
  "Set gradle run compilation target command into compilation buffer"
  (interactive)
  (setq compile-command (get-gradle-run-command (buffer-string) (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
  (call-interactively 'compile compile-command))

(defun gradle-run-single ()
  "Set gradle run compilation target command into compilation buffer"
  (interactive)
  (setq compile-command (get-gradle-run-single-command (buffer-string) (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
  (call-interactively 'compile compile-command))

(defun ant-run-jdb ()
  "Set ant run target into compilation buffer with options to set a transport socket for debugging application with jdb"
  (interactive)
  (let ((jvm-args (concat "-Drun.jvmargs=-agentlib:jdwp=transport=dt_socket,address=" (format "%d" (read-number "Port number: ")) ",server=y,suspend=n")))
    (setq compile-command (get-ant-run-command (buffer-string) (file-name-sans-extension (file-name-nondirectory (buffer-file-name))) jvm-args))
    (call-interactively 'compile compile-command)))

(defun jdb-run ()
  "Set jdb compilation target command into compilation buffer"
  (interactive)
  (let (directory filename command buff)
    (setq filename (buffer-file-name))
    (setq command (get-jdb-command (buffer-string) (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    (setq directory (concat (file-name-directory filename) (get-build-file-relative-location)))
    (setq buff (find-file-other-window directory))
     ; (message "Command: %s" command)
     ; (message "Dir: %s" directory)
    (with-current-buffer buff
        (jdb command))))

(defun show-pid-of-given-process ()
  "Show a menu with the different PID's of the processes associated with the name provided as input"
  (interactive)
  (let (process-name pid command)
    (setq process-name (read-string "Process name: ")) 
    (setq command (concat "ps -e | sed 's/^[A-Z]//g' | awk '/" process-name "/{print $1}'"))
    ; (message (format "Command: %s" command))
    (setq pid (show-menu-and-get-selected-element (split-string (replace-regexp-in-string "\n$" "" (replace-regexp-in-string "\\(\\(\r\\)?\n\\)+" "\n" (shell-command-to-string command))) "\n")))))

(defun jdb-connect ()
  "Connect with jdb to existing instance of Java"
  (interactive)
  (let ((port (read-number "Port number: ")) (command "") (filename (buffer-file-name)) (directory nil) (buff nil))
    (setq command (concat "jdb -sourcepathsrc -connect com.sun.jdi.SocketAttach:port=" (format "%d" port)))
    ;(message (format "Command: %s" command))
    (setq directory (concat (file-name-directory filename) (get-build-file-relative-location)))
    (setq buff (find-file-other-window directory))
    (with-current-buffer buff
        (jdb command))))

(defun javac-this ()
  "Compile current file with javac"
  (interactive)
  (let (class-name relative-path class-fqn class-path compile-command)
    (setq class-name (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
    (setq relative-path (get-build-file-relative-location))
    (setq class-fqn (concat (get-package-fqn-from-dir-tree) "." class-name))
    (setq class-path (mapconcat 'identity (split-string class-fqn "\\.") "/"))
    ; (message "class-name: %s" class-name)
    ; (message "class-fqn: %s" class-fqn)
    ; (message "class-path: %s" class-path)
    ; (message "relative-path: %s" relative-path)
    (setq compile-command (concat "cd " relative-path " && javac -classpath src src/" class-path ".java"))
    (call-interactively 'compile compile-command)))

(defun java-this ()
  "Run current file class file"
  (interactive)
  (let (class-name relative-path class-fqn compile-command)
    (setq class-name (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
    (setq relative-path (get-build-file-relative-location))
    (setq class-fqn (concat (get-package-fqn-from-dir-tree) "." class-name))
    ; (message "class-name: %s" class-name)
    ; (message "class-fqn: %s" class-fqn)
    ; (message "relative-path: %s" relative-path)
    (setq compile-command (concat "cd " relative-path " && java -classpath src " class-fqn))
    (call-interactively 'compile compile-command)))

(defun insert-current-package-fqn-import-at-point ()
  "Insert current file package FQN import at point"
  (interactive)
  (insert-current-package-fqn-at-point "import"))

(defun insert-current-package-fqn-package-at-point ()
  "Insert current file package FQN package at point"
  (interactive)
  (insert-current-package-fqn-at-point "package"))

(defun insert-current-package-fqn-at-point (&optional prefix)
  "Insert current file package FQN at point"
  (insert (concat (if prefix (concat prefix " ") "") (get-package-fqn-from-dir-tree))))

(defun insert-import-if-not-present ()
  "Insert import if not already present"
  (interactive)
  (let (import new-text npt cpt inc)
    (setq import (read-string "Import name: "))
    (insert-import-if-not-present-impl import)))

(defun insert-import-of-class-at-point-if-not-present ()
  "Insert an import for the class at point if not already present"
  (interactive)
  (let ((import (thing-at-point 'word)))
    (insert-import-if-not-present-impl import)
    (reorder-imports)))


(defun get-all-classes-matching-word (word)
  "Get all classes matching the name specified by word"
  (let ((files ()) (buildfileloc (get-build-file-relative-location)) (basedir "") (classes ()))
    (setq basedir (expand-file-name buildfileloc))
    (setq files (find-all-files-in-dir buildfileloc word "java"))
    (setq classes (mapcar (lambda (ele) (replace-regexp-in-string "[.]java" "" (replace-regexp-in-string "/" "." (replace-regexp-in-string (concat (regexp-quote basedir) "\\(/?src/\\)?") "" ele)))) files))
    (setq classes (flatten (cons (get-standard-class-fqn word) classes)))
    ;(message (format "classes>> %s" classes))
    classes))

(defun flatten (mylist)
  (cond
   ((null mylist) nil)
   ((atom mylist) (list mylist))
   (t
    (append (flatten (car mylist)) (flatten (cdr mylist))))))

(defun find-all-files-in-dir (directory filename extension)
  "Find all files with given name and extension in given directory"
  (let ((files ()) (tmp ()))
    (setq tmp (directory-files-recursive directory (concat filename "." extension) 10 "nothing"))
    (mapc (lambda (ele) (if (string-match (regexp-quote (concat filename "." extension)) ele) (setq files (cons ele files)))) tmp)
    files))

;;;
;;; Recursively list files in a given directory
;;;
;;; Author:    daniel m german dmg at uvic dot ca
;;; Copyright: daniel m german
;;; License:   Same as Emacs
;;;
;;; !! From http://turingmachine.org/bl/2013-05-29-recursively-listing-directories-in-elisp.html
(defun directory-files-recursive (directory match maxdepth ignore)
  "List files in DIRECTORY and in its sub-directories. 
   Return files that match the regular expression MATCH but ignore     
   files and directories that match IGNORE (IGNORE is tested before MATCH. Recurse only 
   to depth MAXDEPTH. If zero or negative, then do not recurse"
  (let* ((files-list '())
         (current-directory-list
          (directory-files directory t)))
    ;; while we are in the current directory
     (while current-directory-list
       (let ((f (car current-directory-list)))
         (cond 
          ((and
           ignore ;; make sure it is not nil
           (string-match ignore f))
           ; ignore
            nil
           )
          ((and
            (file-regular-p f)
            (file-readable-p f)
            (string-match match f))
          (setq files-list (cons f files-list))
           )
          ((and
           (file-directory-p f)
           (file-readable-p f)
           (not (string-equal ".." (substring f -2)))
           (not (string-equal "." (substring f -1)))
           (> maxdepth 0))     
           ;; recurse only if necessary
           (setq files-list (append files-list (directory-files-recursive f match (- maxdepth -1) ignore)))
           (setq files-list (cons f files-list))
           )
          (t)
          )
         )
       (setq current-directory-list (cdr current-directory-list))
       )
       files-list
     )
    )

;; (defun test-show-menu-and-get-selected-element ()
;;   "Test function for show-menu-and-get-selected-element"
;;   (interactive)
;;   (message (format "Selected element: [ %s ]" (show-menu-and-get-selected-element '("First" "Second" "Last")))))

(defun show-menu-and-get-selected-element (list)
  "Show a menu of items and get the element choosen by the user"
  (nth (dropdown-list list) list))

(defun insert-import-if-not-present-impl (import)
  "Insert import if not already present"
  (interactive)
  (let (new-text npt cpt inc endl)
    (setq endl (if (string= "groovy" (file-name-extension (file-name-nondirectory (buffer-file-name)))) "" ";"))
    ;(message (format "import: %s" import))
    (setq new-text (concat "\nimport " (get-package-fqn-from-dir-tree) ".?" endl)) ; default
    (if (string-match "[a-zA-Z]+\\([.][a-zA-Z]+\\)+" import)
        (setq new-text (concat "import " import endl)) ; import is a fully qualified name
      (progn
        (setq new-text (show-menu-and-get-selected-element (get-all-classes-matching-word import)))
        (if new-text
            (setq new-text (format (concat "import %s" endl) new-text))
          (setq new-text ""))
        ))
    ;(message (format "New text: %s" new-text))
    (if (not (string-match (regexp-quote new-text) (buffer-string)))
        (progn
          (setq new-text (replace-regexp-in-string "\\(\r?\n\\)*" "" new-text))
          (setq cpt (point))
          (setq inc (length new-text))
          (if (not (string-match "import" (buffer-string))); <- not yet any import
              (progn
                (setq npt (string-match "\\(package[[:space:]]*[^\r\n]*\\)" (buffer-string)))
                (goto-char npt)
                (forward-word)
                (end-of-line)
                (insert "\n"))
            (progn
              (setq npt (string-match "import" (buffer-string)))
              (goto-char npt)))
          (insert (concat "\n" new-text))
          (goto-char (+ cpt inc))))))

(defun reorder-imports ()
  "Reorder imports in alphabetical order"
  (interactive)
  (let (string imports-regex import-head-regex imports new-contents current-point endl)
    (setq endl (if (string= "groovy" (file-name-extension (file-name-nondirectory (buffer-file-name)))) "\n" ";\n"))
    (setq string (buffer-string))
    (setq imports-regex "\\(package[[:space:]]*[^\r\n]*\\)[;]?\\([[:space:]]*\r?\n[[:space:]]*\\)*\\(\\(import \\(static \\)?[a-zA-Z][a-zA-Z0-9]*\\([.][a-zA-Z][a-zA-Z0-9]*\\)*\\([.][*]\\)?[[:space:]]*\\(as[[:space:]]+[a-zA-Z][a-zA-Z0-9]*\\)?[[:space:]]*;?[[:space:]]*\\(\r?\n\\)+\\)+\\)")
    (setq import-head-regex "import \\(static \\)?")
    (string-match imports-regex string)
    (setq imports (match-string 3 string))
    ;(message (format "Imports: %s" imports))
    (setq imports (replace-regexp-in-string "\\(\r?\n\\)+" "\n" imports))
    (setq imports (split-string imports "[;]?\r?\n"))
    ;(message (format "Imports: %s" imports))
    (setq imports (sort imports (lambda (ele1 ele2)
                                  (string-lessp (nth 1 (split-string ele1 import-head-regex)) (nth 1 (split-string ele2 import-head-regex))))))
    ;(message (format "Imports: %s" imports))
    (setq imports (concat (mapconcat
                           'identity
                           imports
                           endl)
                          endl))
    (setq imports (replace-regexp-in-string ";\\([[:space:]\r\n]\\)*;" ";" imports))
    (setq imports (replace-regexp-in-string "\\(\r?\n\\)+" "\n" imports))
    ;(message (format "Imports: %s" imports))
    (setq imports (replace-regexp-in-string "^[\r\n]?[;]\\([[:space:]\r\n]\\)*import" "import" imports))
    (setq new-contents (replace-regexp-in-string imports-regex (concat imports "\n") string))
    ;(message (format "Imports: %s" imports))
    (setq current-point (point))
    (erase-buffer)
    (insert-current-package-fqn-package-at-point)
    (insert endl)
    (insert "\n")
    (insert new-contents)    
    (goto-char current-point)))

(defun make-basic (&optional subcommand)
  "Set make basic compilation command into compilation buffer"
  (interactive "sSubcommand: ")
  (setq compile-command (concat "CDPATH=. && cd " (get-build-file-relative-location) " && make -k " subcommand))
  (call-interactively 'compile compile-command))

(defun vcbuild-basic (&optional subcommand)
  "Set vcbuild compilation command into compilation buffer"
  (interactive "sSubcommand: ")
  (setq compile-command (concat "CDPATH=. && cd " (get-build-file-relative-location-impl-single ".*[.]vcproj") " && vcbuild /rebuild " (replace-regexp-in-string "[.][.]/" "" (get-build-file-relative-location-impl-single ".*[.]vcproj" t)) " \"" subcommand "\""))
  (call-interactively 'compile compile-command))

(setq myantcompile-msbuild "MSBuild.exe")
(defun msbuild-basic (&optional subcommand)
  "Set msbuild compilation command into compilation buffer"
  (interactive "sSubcommand: ")
  (setq compile-command (concat "CDPATH=. && cd " (get-build-file-relative-location-impl-single ".*[.]sln") " && " myantcompile-msbuild " " (replace-regexp-in-string "[.][.]/" "" (get-build-file-relative-location-impl-single ".*[.]sln" t)) " /t:rebuild " subcommand))
  (call-interactively 'compile compile-command))


(defun insert-standard-class-import-at-point ()
  "Insert import for a standard library class at point"
  (interactive)
  (insert (concat
           "import "
           (mapconcat
             'identity
             (split-string (car (split-string (get-class-fqn) "[.]")) "/")
             ".")
           ";")))

(defun get-standard-class-fqn (word)
  "Return a list of standard classes fqn's matching word"
  (let ((class "") (classes nil))
    (setq class (get-class-fqn-impl word))
    (if (and class (not (string= "nil" class)))
        (setq classes (cons class '()))
      (setq class nil))
    ;(message (format "class: %s; classes: %s" class classes))
    classes))

;; Key bindings
(define-prefix-command 'myantcompile-specific-map)
(global-set-key (kbd "C-b") 'myantcompile-specific-map)
;; gradle
(define-key myantcompile-specific-map (kbd "g c") 'gradle-clean)
(define-key myantcompile-specific-map (kbd "g b") 'gradle-compile)
(define-key myantcompile-specific-map (kbd "g j") 'gradle-jar)
(define-key myantcompile-specific-map (kbd "g r") 'gradle-run)
(define-key myantcompile-specific-map (kbd "g u r s") 'gradle-run-single)
;; ant
(define-key myantcompile-specific-map (kbd "a c") 'ant-clean)
(define-key myantcompile-specific-map (kbd "a b") 'ant-compile)
(define-key myantcompile-specific-map (kbd "a j") 'ant-jar)
(define-key myantcompile-specific-map (kbd "a r") 'ant-run)
(define-key myantcompile-specific-map (kbd "a u r s") 'ant-run-single)
(define-key myantcompile-specific-map (kbd "a u r d") 'ant-run-jdb)
(define-key myantcompile-specific-map (kbd "a t") 'ant-test)
(define-key myantcompile-specific-map (kbd "a d") 'jdb-run)
(define-key myantcompile-specific-map (kbd "a u d") 'jdb-connect)
;; java(c)
(define-key myantcompile-specific-map (kbd "j b") 'javac-this)
(define-key myantcompile-specific-map (kbd "j c") 'javac-this)
(define-key myantcompile-specific-map (kbd "j j") 'java-this)
(define-key myantcompile-specific-map (kbd "j r") 'java-this)
;; related to imports
(define-key myantcompile-specific-map (kbd "j i") 'insert-current-package-fqn-import-at-point)
(define-key myantcompile-specific-map (kbd "p i") 'insert-current-package-fqn-package-at-point)
(define-key myantcompile-specific-map (kbd "j p") 'insert-current-package-fqn-package-at-point)
(define-key myantcompile-specific-map (kbd "j s i") 'insert-standard-class-import-at-point)
(define-key myantcompile-specific-map (kbd "i") 'insert-import-of-class-at-point-if-not-present)
(define-key myantcompile-specific-map (kbd "a i") 'insert-import-if-not-present)
(define-key myantcompile-specific-map (kbd "r i") 'reorder-imports)
;; make
(define-key myantcompile-specific-map (kbd "m r") (lambda () (interactive) (make-basic "run")))
(define-key myantcompile-specific-map (kbd "m b") (lambda () (interactive) (make-basic "rebuild")))
(define-key myantcompile-specific-map (kbd "m c") (lambda () (interactive) (make-basic "clean")))
(define-key myantcompile-specific-map (kbd "m t") (lambda () (interactive) (make-basic "test")))
;; vcbuild
(define-key myantcompile-specific-map (kbd "v r") (lambda () (interactive) (vcbuild-basic "Release|Win32")))
(define-key myantcompile-specific-map (kbd "v d") (lambda () (interactive) (vcbuild-basic "Debug|Win32")))
;; msbuild
(define-key myantcompile-specific-map (kbd "m r") (lambda () (interactive) (msbuild-basic "/p:Configuration=Release /p:Platform=Win32")))
(define-key myantcompile-specific-map (kbd "m d") (lambda () (interactive) (msbuild-basic "/p:Configuration=Debug /p:Platform=Win32")))

(provide 'myantcompile)
;;; myantcompile.el ends here
