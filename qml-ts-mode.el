;;; qml-ts-mode.el --- Major mode for edit qml file   -*- lexical-binding: t; -*-

;; Filename: qml-ts-mode.el
;; Description: Major mode for edit qml file
;; Author: xhcoding <xhcoding@foxmail.com>
;; Maintainer: xhcoding <xhcoding@foxmail.com>
;; Copyright (C) 2023, xhcoding, all rights reserved.
;; Created: 2023-01-24 20:40:09
;; Version: 0.1
;; Last-Updated: 2023-01-24 20:40:09
;;           By: xhcoding
;; URL: https://www.github.org/xhcoding/qml-ts-mode
;; Keywords: qml treesit
;; Compatibility: emacs-version >= 29
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Major mode for edit qml file
;;

;;; Installation:
;;
;; Put qml-ts-mode.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'qml-ts-mode)
;;
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET qml-ts-mode RET
;;

;;; Change log:
;;
;; 2023/01/24
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require

(require 'treesit)
(require 'c-ts-common)  ; For comment indent and filling.
(require 'js)           ; Base js-ts-mode

(eval-when-compile
  (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-search-subtree "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")


;;; Code:

(defgroup qml-ts nil
  "Major mode for the qml programming language using tree-sitter."
  :group 'languages
  :prefix "qml-ts-")

(defcustom qml-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `qml-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'qml-ts)


(defvar qml--treesit-indent-rules
  (let ((offset qml-ts-mode-indent-offset)
        (switch-case (rx "switch_" (or "case" "default"))))
    `((qmljs
       ;; from js-ts-mode
       ((parent-is "program") parent-bol 0)
       ((node-is "}") parent-bol 0)
       ((node-is ")") parent-bol 0)
       ((node-is "]") parent-bol 0)
       ((node-is ">") parent-bol 0)
       ((and (parent-is "comment") c-ts-common-looking-at-star)
        c-ts-common-comment-start-after-first-star -1)
       ((parent-is "comment") prev-adaptive-prefix 0)
       ((parent-is "ternary_expression") parent-bol ,offset)
       ((parent-is "member_expression") parent-bol ,offset)
       ((node-is ,switch-case) parent-bol 0)
       ;; "{" on the newline.
       ((node-is "statement_block") parent-bol ,offset)
       ((parent-is "named_imports") parent-bol ,offset)
       ((parent-is "statement_block") parent-bol ,offset)
       ((parent-is "variable_declarator") parent-bol ,offset)
       ((parent-is "arguments") parent-bol ,offset)
       ((parent-is "array") parent-bol ,offset)
       ((parent-is "formal_parameters") parent-bol ,offset)
       ((parent-is "template_substitution") parent-bol ,offset)
       ((parent-is "object_pattern") parent-bol ,offset)
       ((parent-is "object") parent-bol ,offset)
       ((parent-is "pair") parent-bol ,offset)
       ((parent-is "arrow_function") parent-bol ,offset)
       ((parent-is "parenthesized_expression") parent-bol ,offset)
       ((parent-is "binary_expression") parent-bol ,offset)
       ((parent-is "class_body") parent-bol ,offset)
       ((parent-is ,switch-case) parent-bol ,offset)
       ((parent-is "statement_block") parent-bol ,offset)

       ))))

(defvar qml--treesit-keywords
  '( "on" "property" "signal" "declare" "enum" "export" "implements"
     "interface" "keyof" "namespace" "type" "override" "abstract"
     "private" "protected" "public" "default" "readonly" "required")
  "QML keywords for tree-sitter font-locking.")


(defvar qml--treesit-font-lock-settings
  (treesit-font-lock-rules

   :language 'qmljs
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'qmljs
   :feature 'constant
   '(((identifier) @font-lock-constant-face
      (:match "^[A-Z_][A-Z_\\d]*$" @font-lock-constant-face))
     [(true) (false) (null)] @font-lock-constant-face)

   :language 'qmljs
   :feature 'keyword
   `([,@js--treesit-keywords] @font-lock-keyword-face
     [(this) (super)] @font-lock-keyword-face
     [,@qml--treesit-keywords] @font-lock-keyword-face)

   :language 'qmljs
   :feature 'string
   '((regex pattern: (regex_pattern)) @font-lock-regexp-face
     (string) @font-lock-string-face)

   :language 'qmljs
   :feature 'string-interpolation
   :override t
   '((template_string) @js--fontify-template-string
     (template_substitution ["${" "}"] @font-lock-misc-punctuation-face))

   :language 'qmljs
   :feature 'type
   '((type_identifier) @font-lock-type-face

     (predefined_type) @font-lock-type-face

     (ui_object_definition
      type_name: (identifier) @font-lock-type-face)

     (ui_object_definition
      type_name: (nested_identifier) @font-lock-type-face))

   :language 'qmljs
   :feature 'definition
   '((function_declaration
      name: (identifier) @font-lock-function-name-face)

     (variable_declarator
      name: (identifier) @font-lock-variable-name-face)

     (variable_declarator
      name: (identifier) @font-lock-function-name-face
      value: [(function) (arrow_function)])

     (variable_declarator
      name: (array_pattern
             (identifier)
             (identifier)
             @font-lock-function-name-face)
      value: (array (number) (function)))

     (import_clause (identifier) @font-lock-variable-name-face)
     (import_clause (named_imports (import_specifier (identifier))
                                   @font-lock-variable-name-face))

     (required_parameter (identifier) @font-lock-variable-name-face)

     (optional_parameter (identifier) @font-lock-variable-name-face)

     (ui_signal
      name: (identifier) @font-lock-function-name-face)

     (ui_binding
      name: [
             (identifier) @font-lock-variable-name-face
             (nested_identifier) @font-lock-variable-name-face
             ])

     )

   :language 'qmljs
   :feature 'property
   '(((property_identifier) @font-lock-property-face
      (:pred js--treesit-property-not-function-p
             @font-lock-property-face))

     (pair value: (identifier) @font-lock-variable-name-face)

     ((shorthand_property_identifier) @font-lock-property-face)

     ((shorthand_property_identifier_pattern) @font-lock-property-face))

   :language 'qmljs
   :feature 'assignment
   '((assignment_expression
      left: (_) @js--treesit-fontify-assignment-lhs))

   :language 'qmljs
   :feature 'function
   '((call_expression
      function: (identifier) @font-lock-variable-name-face)

     (call_expression
      function: (member_expression
                 object: (identifier) @font-lock-variable-name-face
                 property: (property_identifier) @font-lock-function-name-face))
     )


   :language 'qmljs
   :feature 'number
   '((number) @font-lock-number-face
     ((identifier) @font-lock-number-face
      (:match "^\\(:?NaN\\|Infinity\\)$" @font-lock-number-face)))

   :language 'qmljs
   :feature 'operator
   `([,@js--treesit-operators] @font-lock-operator-face
     (ternary_expression ["?" ":"] @font-lock-operator-face))

   :language 'qmljs
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'qmljs
   :feature 'delimiter
   '((["," "." ";" ":"]) @font-lock-delimiter-face)

   :language 'qmljs
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)
   )
  "Tree-sitter font-lock settings.")


(defun qml--treesit-defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (treesit-node-text
   (pcase (treesit-node-type node)
     ("lexical_declaration"
      (treesit-node-child-by-field-name
       (treesit-search-subtree node "variable_declarator" nil nil 1) "name"))
     ((or "function_declaration" "method_definition" "class_declaration" "ui_signal")
      (treesit-node-child-by-field-name node "name"))
     ("ui_object_definition"
      (treesit-node-child-by-field-name node "type_name")))
   t))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-ts-mode))

;;;###autoload
(define-derived-mode qml-ts-mode prog-mode "QML"
  "Major mode for editing QML."
  :group 'qml-ts
  :syntax-table js-mode-syntax-table
  ;; Comment
  (c-ts-common-comment-setup)
  (setq-local comment-multi-line t)
  (setq-local treesit-text-type-regexp
              (regexp-opt '("comment"
                            "template_string")))

  ;; Treesit setup
  (treesit-parser-create 'qmljs)

  ;; Indent
  (setq-local treesit-simple-indent-rules qml--treesit-indent-rules)

  ;; Fontification.
  ;; (setq-local treesit--font-lock-verbose t)
  ;; (setq-local treesit-font-lock-level 4)
  (setq-local treesit-font-lock-settings qml--treesit-font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '(( comment definition)
                ( keyword string type)
                ( assignment constant escape-sequence number
                  pattern string-interpolation)
                ( bracket delimiter function operator property)))

  ;; Navigation.
  (setq-local treesit-defun-prefer-top-level t)
  (setq-local treesit-defun-type-regexp
              (rx (or "class_declaration"
                      "method_definition"
                      "function_declaration"
                      "lexical_declaration"
                      "ui_object_definition")))

  (setq-local treesit-defun-name-function #'qml--treesit-defun-name)

  (setq-local treesit-sentence-type-regexp
              (regexp-opt js--treesit-sentence-nodes))


  ;; Imenu
  (setq treesit-simple-imenu-settings
        `(("Function" "\\`function_declaration\\'" nil nil)
          ("Signal" "\\`ui_signal\\'" nil nil)
          ("Component" "\\`ui_object_definition\\'" nil nil)))

  (treesit-major-mode-setup)
  )


(provide 'qml-ts-mode)

;;; qml-ts-mode.el ends here

