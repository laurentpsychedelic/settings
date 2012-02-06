;;-*-coding: emacs-mule;-*-
(define-abbrev-table 'Buffer-menu-mode-abbrev-table '())

(define-abbrev-table 'Custom-mode-abbrev-table '())

(define-abbrev-table 'awk-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ("#e" "#endif /* */" my-abbrev-hook 0)
    ("#i" "#include \"\"" my-abbrev-hook 0)
    ("#ifd" "#ifdef" nil 0)
    ("#ifn" "#ifndef" nil 0)
    ("#s" "#include <>" my-abbrev-hook 0)
    ("else" "else {
}
" my-abbrev-hook 0)
    ("for" "for (;;) {
}
" my-abbrev-hook 0)
    ("if" "if () {
}
" my-abbrev-hook 0)
    ("imain" "int main(int ac, char **av[]) {

}" my-abbrev-hook 0)
    ("printf" "printf(\"\")" my-abbrev-hook 0)
    ("while" "while () {
}
" my-abbrev-hook 0)
   ))

(define-abbrev-table 'c-mode-abbrev-table
  '(
    ("#e" "#endif /* */" my-abbrev-hook 0)
    ("#i" "#include \"\"" my-abbrev-hook 0)
    ("#ifd" "#ifdef" nil 0)
    ("#ifn" "#ifndef" nil 0)
    ("#s" "#include <>" my-abbrev-hook 0)
    ("else" "else {
}
" my-abbrev-hook 0)
    ("for" "for (;;) {
}
" my-abbrev-hook 0)
    ("if" "if () {
}
" my-abbrev-hook 0)
    ("imain" "int main(int ac, char **av[]) {

}" my-abbrev-hook 0)
    ("printf" "printf(\"\")" my-abbrev-hook 0)
    ("while" "while () {
}
" my-abbrev-hook 0)
   ))

(define-abbrev-table 'change-log-mode-abbrev-table '())

(define-abbrev-table 'comint-mode-abbrev-table '())

(define-abbrev-table 'completion-list-mode-abbrev-table '())

(define-abbrev-table 'conf-colon-mode-abbrev-table '())

(define-abbrev-table 'conf-javaprop-mode-abbrev-table '())

(define-abbrev-table 'conf-ppd-mode-abbrev-table '())

(define-abbrev-table 'conf-space-mode-abbrev-table '())

(define-abbrev-table 'conf-unix-mode-abbrev-table '())

(define-abbrev-table 'conf-windows-mode-abbrev-table '())

(define-abbrev-table 'conf-xdefaults-mode-abbrev-table '())

(define-abbrev-table 'diff-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-mode-abbrev-table '())

(define-abbrev-table 'fundamental-mode-abbrev-table '())

(define-abbrev-table 'global-abbrev-table
  '(
    ("antr" "and" nil 1)
    ("apped" "append" nil 1)
    ("atuo" "auto" nil 1)
    ("auot" "auto" nil 1)
    ("beenn" "been" nil 1)
    ("bufer" "buffer" nil 1)
    ("cakculation" "Calculation" nil 1)
    ("casncelled" "cancelled" nil 1)
    ("cehckout" "checkout" nil 1)
    ("chich" "which" nil 1)
    ("chweck" "check" nil 1)
    ("ciontents" "contents" nil 1)
    ("classpaht" "classpth" nil 1)
    ("classpth" "classpath" nil 1)
    ("colroing" "coloring" nil 1)
    ("comlpete" "complete" nil 1)
    ("complte" "complete" nil 1)
    ("correctr" "correct" nil 1)
    ("defore" "before" nil 1)
    ("dimensuion" "Dimension" nil 1)
    ("dound" "found" nil 4)
    ("enablle" "enable" nil 1)
    ("exti" "exit" nil 1)
    ("falot" "float" nil 1)
    ("flase" "false" nil 1)
    ("flloat" "float" nil 1)
    ("found" "dound" nil 2)
    ("frmae" "frame" nil 1)
    ("hava" "java" nil 1)
    ("histroy" "history" nil 1)
    ("idel" "idle" nil 1)
    ("inffo" "info" nil 1)
    ("instane" "instance" nil 1)
    ("isswitch" "iswitch" nil 1)
    ("langauge" "LANGUAGE" nil 1)
    ("linekd" "Linked" nil 1)
    ("linkedlsit" "LinkedList" nil 3)
    ("memoru" "memory" nil 1)
    ("mesages" "messages" nil 1)
    ("mide" "mode" nil 1)
    ("nasme" "name" nil 1)
    ("nbame" "name" nil 1)
    ("nll" "null" nil 1)
    ("nrew" "new" nil 1)
    ("over6ride" "Override" nil 1)
    ("pasth" "path" nil 1)
    ("poijnts" "POINTS" nil 1)
    ("privaet" "private" nil 1)
    ("publuic" "public" nil 1)
    ("pulic" "public" nil 2)
    ("putr" "put" nil 1)
    ("recangle" "Rectangle" nil 1)
    ("redct" "rect" nil 1)
    ("redfine" "redefine" nil 1)
    ("rednerbars" "RenderBars" nil 1)
    ("rednerplot3d" "RenderPlot3D" nil 1)
    ("resolutoin" "resolution" nil 1)
    ("retrun" "return" nil 1)
    ("retunr" "return" nil 1)
    ("returnt" "return" nil 1)
    ("reutrn" "returnt" nil 1)
    ("rpeaint" "repaint" nil 1)
    ("sdtokes" "strokes" nil 1)
    ("servere" "server" nil 1)
    ("sfn" "svn" nil 1)
    ("sotred" "stored" nil 1)
    ("statis" "static" nil 1)
    ("stats" "status" nil 1)
    ("statua" "status" nil 1)
    ("string" "String" nil 3)
    ("stringbuolder" "StringBuilder" nil 1)
    ("temmpbuf" "tempbuf" nil 1)
    ("thjen" "then" nil 1)
    ("thois" "this" nil 1)
    ("unti" "unit" nil 2)
    ("vreak" "break" nil 1)
    ("wundow" "window" nil 1)
   ))

(define-abbrev-table 'groovy-mode-abbrev-table
  '(
    ("else" "else {
}
" my-abbrev-hook 0)
    ("for" "for (;;) {
}
" my-abbrev-hook 0)
    ("if" "if () {
}
" my-abbrev-hook 0)
    ("imain" "public static void main(String[] args) {

}" my-abbrev-hook 0)
    ("print" "System.out.print()" my-abbrev-hook 0)
    ("println" "System.out.println()" my-abbrev-hook 0)
    ("while" "while () {
}
" my-abbrev-hook 0)
   ))

(define-abbrev-table 'html-mode-abbrev-table '())

(define-abbrev-table 'idl-mode-abbrev-table '())

(define-abbrev-table 'java-mode-abbrev-table
  '(
    ("else" "else {
}
" my-abbrev-hook 0)
    ("for" "for (;;) {
}
" my-abbrev-hook 0)
    ("if" "if () {
}
" my-abbrev-hook 0)
    ("imain" "public static void main(String[] args) {

}" my-abbrev-hook 0)
    ("print" "System.out.print()" my-abbrev-hook 0)
    ("println" "System.out.println()" my-abbrev-hook 0)
    ("while" "while () {
}
" my-abbrev-hook 0)
   ))

(define-abbrev-table 'lisp-mode-abbrev-table '())

(define-abbrev-table 'log-edit-mode-abbrev-table '())

(define-abbrev-table 'log-view-mode-abbrev-table '())

(define-abbrev-table 'nroff-mode-abbrev-table '())

(define-abbrev-table 'objc-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'outline-mode-abbrev-table '())

(define-abbrev-table 'pike-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'select-tags-table-mode-abbrev-table '())

(define-abbrev-table 'sgml-mode-abbrev-table '())

(define-abbrev-table 'sh-mode-abbrev-table '())

(define-abbrev-table 'shell-mode-abbrev-table '())

(define-abbrev-table 'special-mode-abbrev-table '())

(define-abbrev-table 'text-mode-abbrev-table '())

(define-abbrev-table 'twittering-edit-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-view-mode-abbrev-table '())

(define-abbrev-table 'vc-svn-log-view-mode-abbrev-table '())

