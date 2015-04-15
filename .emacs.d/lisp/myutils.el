;;; myutils.el --- Various utils for programming, essentially in Java, Groovy, C, C++

;; Copyright (C) 2013  

;; Author:  <Laurent_dev@LAURENT_DEV-PC>
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

(defun convert-api-header-to-jna-binding ()
  "Create a JNA binding interface bound to the API definitions in current buffer"
  (interactive)
  (let ((yas-good-grace nil)
        (text (buffer-string))
        (native-apis "") 
        (bindings-text "") 
        (regexp-apis "\\(extern \"C\"{[[:space:]\r\n]*\\(#endif\\)?[[:space:]\r\n]\\)\\([^}]*\\)[[:space:]\r\n]*\\(#ifdef[^}]*\\)}")
        (regexp-types "\\(\\([(,]\\)[[:space:]]*\\([a-zA-Z_][a-zA-Z_0-9]*[[:space:]]+\\)+[*]?\\([a-zA-Z_][a-zA-Z_0-9]*[[:space:]]*\\)\\(\\[[[:space:]]*\\]\\)?\\)"))
    (string-match regexp-apis text)
    (setq native-apis (match-string 3 text))
    (setq native-apis (replace-regexp-in-string regexp-types "\\2 def \\4" native-apis t))
    (setq native-apis (replace-regexp-in-string "[[:space:]]*([[:space:]]+" "(" native-apis))
    (setq native-apis (replace-regexp-in-string "[[:space:]]*,[[:space:]]*" ", " native-apis))
    (setq native-apis (replace-regexp-in-string "[[:space:]]*[a-zA-Z_][a-zA-Z_0-9]*API[[:space:]]*" " " native-apis))
    (setq native-apis (replace-regexp-in-string ";" "" native-apis))
   (setq native-apis (replace-regexp-in-string "[\r\n]+" "\n" native-apis))
    (get-buffer-create "*JNA bindings*")
    (switch-to-buffer "*JNA bindings*")
    (groovy-mode)
    (yas-expand-snippet (concat "import com.sun.jna.*

def binding = Native.loadLibrary(\"${1:$(downcase (replace-regexp-in-string \"[-/[:space:]]+\" \"_\" text))}\", ${1:$(mapconcat 'capitalize (split-string text \"[-/[:space:]]+\") \"\")}JNABinding)

/**
 * JNA interface to ${1:API name} native library
 */
interface ${1:$(mapconcat 'capitalize (split-string text \"[-/[:space:]]+\") \"\")}JNABindings extends Library {
" native-apis "}
"))))




(defun expand-native-java-api-to-header-file ()
  "Create C header file contents corresponding to the implementation of all
methods declared as \"native\" in current Java source file."
  (interactive)
  (let ((text (buffer-string)) (yas-good-grace nil) (start 0) (header-text "") (api-regexp "\\([^[:space:];()]* native[^;()]*([^;()]*);\\)"))
    (while start
      (if (string-match api-regexp text start)
          (progn
            (setq start (nth 1 (match-data)))
            (setq header-text (concat header-text "\n" (match-string 1 text))))
        (setq start nil)))
    (setq header-text (replace-regexp-in-string "\\(\\(public\\)\\|\\(final\\)\\|\\(private\\)\\|\\(native\\)\\|\\(protected\\)\\|\\(synchronized\\)\\|\\(static\\)\\) " "" header-text))
    (setq header-text (replace-regexp-in-string "\\[\\]" "*" header-text))
    (split-window)
    (get-buffer-create "*native api*")
    (switch-to-buffer "*native api*")
    (c-mode)
    (yas-expand-snippet (concat
"#ifndef ${1:$(upcase (replace-regexp-in-string \"[-/[:space:]]+\" \"_\" text))}_H
#define ${1:$(upcase (replace-regexp-in-string \"[-/[:space:]]+\" \"_\" text))}_H

/* ${1:$(upcase (replace-regexp-in-string \"[-/[:space:]]+\" \"_\" text))}.h
   Declares ${1:API-NAME} API functions
*/

#ifdef _WIN32

  #ifdef ${1:$(upcase (replace-regexp-in-string \"[-/[:space:]]+\" \"_\" text))}_EXPORTS
    #define ${1:$(upcase (replace-regexp-in-string \"[-/[:space:]]+\" \"_\" text))}_API __declspec(dllexport)
  #else
    #define ${1:$(upcase (replace-regexp-in-string \"[-/[:space:]]+\" \"_\" text))}_API __declspec(dllimport)
  #endif

  /* Define calling convention. */
  #define ${1:$(upcase (replace-regexp-in-string \"[-/[:space:]]+\" \"_\" text))}_CALL __cdecl

#else /* _WIN32 not defined. */

  /* Define with no value on non-Windows OSes. */
  #define ${1:$(upcase (replace-regexp-in-string \"[-/[:space:]]+\" \"_\" text))}_API
  #define ${1:$(upcase (replace-regexp-in-string \"[-/[:space:]]+\" \"_\" text))}_CALL

#endif

/* Make sure functions are exported with C linkage under C++ compilers. */
#ifdef __cplusplus
extern \"C\"
{
#endif

    /* API type definitions */

    /* API funtions declarations */
"
header-text "$0

#ifdef __cplusplus
} // __cplusplus defined.
#endif

#endif"))))


(defun expand-settable-interface-from-selection (&optional interface-name)
     "Expand a settable interface (Java) given a list of variables
    ex:
        int radius;
        Map<String, Object> map;
    Expanded to:
        public interface InterfaceName {
            public abstract int getRadius();
            public abstract void setRadius(int radius);
            public abstract Map<String> getProps();
            public abstract void setProps(Map<String> props);
        }"
     (interactive)
     (let ((region nil) (text "") (varlist ()) (yas-good-grace nil))
       (if (or (not interface-name) (string= "" interface-name)) (setq interface-name (concat "I" (read-string "Interface name: "))))
       (if (or (not interface-name) (string= "" interface-name)) (setq interface-name (concat "I" (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))
       (setq region (get-selection-or-unit 'block))
       (setq text (buffer-substring (elt region 1) (elt region 2)))
       (delete-region (elt region 1) (elt region 2))
       (setq varlist (split-string text "\\([;]?\n\\)\\|\\(;\\)"))
       (setq varlist (delete-if (lambda (var) (string= "" var)) varlist))
       (message (format "VARLIST: %s" region))
       (insert
        (concat "/**\n"
                " * Interface to define a set of setter/accessors to a set of properties.\n"
                " * @author Laurent FABRE ; (c) 2013 Photonic Lattice, Inc.\n"
                " * @version 1.0\n"
                " */\n"
                "public interface " interface-name " {\n"
                (mapconcat (lambda (var)
                             (let ((vartype (car (split-string var "[[:space:]]"))) (varname (nth 1 (split-string var "[[:space:]]"))))
                               (concat "    /* Property << " varname ": " vartype "  >> */\n"
                                       "    /**\n"
                                       "     * Get property << " varname " >>\n"
                                       "     * @return Property << " varname " >> value\n"
                                       "     */\n"
                                       "    public abstract " vartype " get" (capitalize varname) "();\n"
                                       "    /**\n"
                                       "     * Set property << " varname " >>\n"
                                       "     * @param " varname " New value for property << " varname " >>\n"
                                       "     */\n"
                                       "    public abstract void set" (capitalize varname) "(" vartype " " varname ");")))
                           varlist "\n")
                "\n}"))
       (setq region (get-selection-or-unit 'block))
       (indent-region (elt region 1) (elt region 2))))


(provide 'myutils)
;;; myutils.el ends here
