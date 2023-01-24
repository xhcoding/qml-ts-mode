# QML major mode using treesit

---
qml-ts-mode is major-mode for editing Qt Declarative (QML) code.

# Installation

## Dependencies

This package requires:

1. Emacs 29 or newer built with treesit support;
2. install [QML tree-sitter grammar](https://github.com/yuja/tree-sitter-qmljs)

   steps:

   ```
   git clone https://github.com/yuja/tree-sitter-qmljs.git
   cd tree-sitter-qmljs
   gcc ./src/parser.c ./src/scanner.c -fPIC -I./ --shared -o libtree-sitter-qmljs.so
   cp libtree-sitter-qmljs.so ~/.emacs.d/tree-sitter/
   ```


## Install from source
You can install this package from source by cloning this directory and adding
the following lines to your Emacs configuration:

``` emacs-lisp
(add-to-list 'load-path "<path to the source-code tree>")
(require 'qml-ts-mode)
```


