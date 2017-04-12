vim.cpp - additional vim d syntax highlighting
------------------------------------------------

This file contains additional syntax highlighting that used for D
development in Vim. Compared to the standard syntax highlighting for D it
adds highlighting of (user defined) functions and the containers and types in
the standard library (phobos).

Development is done at: http://github.com/Sirsireesh/vim-dlang-extra-highlight

Optional features
-----------------

Highlighting of class scope is disabled by default. To enable set
```vim
let g:d_class_scope_highlight = 1
```

Installation instructions
-------------------------
Follow one of the sets of directions below and reload vim afterwards.

#### Vundle
Install using [vundle](https://github.com/gmarik/Vundle.vim) by adding
```vim
Plugin 'Sirsireesh/vim-dlang-extra-highlight'
```
to .vimrc and run `:PluginInstall`.


#### Git submodule + Pathogen
If you have [pathogen](https://github.com/tpope/vim-pathogen) installed,
and you prefer to use git submodules, run
```sh
cd ~/.vim
git submodule add https://github.com/Sirsireesh/vim-dlang-extra-highlight.git bundle/syntax/
```

#### Manual installation
If you don't have either Vundle or Pathogen installed, copy the cpp.vim file
(optionally also c.vim) to .vim/after/syntax.
```sh
git clone https://github.com/Sirsireesh/vim-dlang-extra-highlight.git /tmp/vim-dlang-extra-highlight
mkdir -p ~/.vim/after/syntax/
mv /tmp/vim-dlang-extra-highlight/after/syntax/d.vim ~/.vim/after/syntax/d.vim
rm -rf /tmp/vim-dlang-extra-highlight
```

Issues
------

Vim tend to a have issues with flagging braces as errors, see for example
https://github.com/vim-jp/vim-cpp/issues/16. A workaround is to set
```vim
let c_no_curly_error=1
```

Background information
----------------------

Based heavily on [vim-cpp-enhanced-highlight by octol](https://github.com/octol/vim-cpp-enhanced-highlight), mostly copied from there
- http://stackoverflow.com/questions/736701/class-function-names-highlighting-in-vim
- http://www.vim.org/scripts/script.php?script_id=4293
- http://www.vim.org/scripts/script.php?script_id=2224
- http://www.vim.org/scripts/script.php?script_id=1640
- http://www.vim.org/scripts/script.php?script_id=3064

Sireesh Kodali

Last update: 12 April 2017
