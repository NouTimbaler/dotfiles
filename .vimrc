" Vim configuration file

" leader for extra maps
let maplocalleader = ","

" jump to next <++> mark
map ,, :keepp /<++><CR>ca<
imap ,, <ESC>:keepp /<++><CR>ca<

" current millenium lol
set nocompatible

" enable mouse support
set mouse=a

" scroll at 4
set scrolloff=4

" enable line numbers
set number
set relativenumber

" status bar
set laststatus=2
set showcmd " display commands
set showmode " display mode
set ruler   "always show cursor position

" highlight current line
set cursorline
:highlight Cursorline cterm=bold ctermbg=black

" enable highlight search pattern
set hlsearch
" search while typing
set incsearch
" enable smartcase search sensitivity
set ignorecase
set smartcase

" indentation using spaces
set tabstop=4
set softtabstop=4
set shiftwidth=4
set textwidth=79
set expandtab
set smartindent

" show the matching part of pairs [] {} ()
set showmatch

" enable syntax highlight and indentation for file detection
filetype plugin indent on
syntax on

" enable color themes
 if !has('gui_running')
    set t_Co=256 "
 endif "

" enable true colors support
set termguicolors
" Vim colorscheme
colorscheme desert

" copy and paste
vnoremap <C-c> "+y
map <C-p> "+P

" autocomplete
set complete+=kspell,t " add spelling and tags to C-n
set completeopt=menuone,longest,preview " some options, testing longest

" good split
set splitbelow splitright

"split navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <C-Left> <C-w>h
nnoremap <C-Down> <C-w>j
nnoremap <C-Up> <C-w>k
nnoremap <C-Right> <C-w>l

" FINDING FILES:
"
" search into subfolders
" tab-completion for file tasks
set path+=**

" display all matching files
set wildmenu
set wildmode=longest,list,full

" TAG JUMPING:
"
" create `tags` file
command! MakeTags !ctags -R .
" how to use tags
" ^]    to jump to tag on cursor
" g ^]  to list of tag uses
" ^t    jump back

" BUILDING: (compiling) TODO:
" look at makeprg 

" extra idk
set backspace=indent,eol,start  " allow backspacing over everything
set display=truncate " Show @@@ when truncate

" jump to last known cursor position
autocmd BufReadPost *
  \ if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
  \ |   exe "normal! g`\""
  \ | endif

" automatically close stuff
" inoremap " ""<left>
" inoremap ' ''<left>
" inoremap ( ()<left>
" inoremap [ []<left>
" inoremap { {}<left>
" inoremap /* /**/<left><left>

" disable automatic commenting on new line
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

