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
