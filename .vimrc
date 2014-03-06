runtime bundle/vim-pathogen/autoload/pathogen.vim

execute pathogen#infect()
Helptags

set nocompatible

set tabstop=8
set expandtab
set shiftwidth=2
set softtabstop=2

set smartindent

set showmatch
set incsearch

set number
set ruler
set showcmd

set history=100

set backspace=indent,eol,start

set wildmenu
"set wildmode=list:longest,full
set wildmode=longest:full,full

set foldmethod=syntax
set foldnestmax=2
set foldlevelstart=99
set foldcolumn=1

if has("multi_byte")
  if &termencoding == ""
    let &termencoding = &encoding
  endif
  set encoding=utf-8
  setglobal fileencoding=utf-8
  set fileencodings=ucs-bom,utf-8,latin1
endif

set fileformats=unix,dos

"set textwidth=78

set spelllang=en_gb
"set spell

set directory=/tmp,/dev/shm,$TMP,$TEMP

if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
  set cursorline
  "set cursorcolumn
  set background=dark
  colorscheme desert
  if &t_Co >= 256 || has("gui_running")
    colorscheme Tomorrow-Night
  endif
endif

if has("gui_running")
  set guifont=Terminus,Inconsolata-g,Inconsolata,Consolas
  set lines=50 columns=100
endif

if has('mouse')
  set mouse=a
endif

if version >= 703
  set colorcolumn=81,101
  set undofile
  set undodir=/tmp,/dev/shm,$TMP,$TEMP
endif

map <ESC>[H <Home>
map <ESC>[F <End>
imap <ESC>[H <C-O><Home>
imap <ESC>[F <C-O><End>
cmap <ESC>[H <Home>
cmap <ESC>[F <End>

" inoremap {      {}<Left>
" inoremap (      ()<Left>
" inoremap [      []<Left>
" inoremap "      ""<Left>
inoremap {<CR>  {<CR>}<Esc>O
inoremap (<CR>  (<CR>)<Esc>O
inoremap [<CR>  [<CR>]<Esc>O
" inoremap {{     {
" inoremap ((     (
" inoremap [[     [
" inoremap ""     "
" inoremap {}     {}
" inoremap []     []
" inoremap ()     ()

" Don't use Ex mode, use Q for formatting
" map Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" Only do this part when compiled with support for autocommands.
if has("autocmd")
    " Enable file type detection.
    " Use the default filetype settings, so that mail gets 'tw' set to 72,
    " 'cindent' is on in C files, etc.
    " Also load indent files, to automatically do language-dependent indenting.
    filetype plugin indent on
    " Put these in an autocmd group, so that we can delete them easily.
    augroup vimrcEx
    au!
    " For all text files set 'textwidth' to 78 characters.
    autocmd FileType text setlocal textwidth=78
    " When editing a file, always jump to the last known cursor position.
    " Don't do it when the position is invalid or when inside an event handler
    " (happens when dropping a file on gvim).
    " Also don't do it when the mark is in the first line, that is the default
    " position when opening a file.
    autocmd BufReadPost *
      \ if line("'\"") > 1 && line("'\"") <= line("$") |
      \   exe "normal! g`\"" |
      \ endif
    augroup END

    if &t_Co > 2 || has("gui_running")
        " Trailing whitespace highlighting
        highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen
        match ExtraWhitespace /\s\+$/
        autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
        autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
        autocmd InsertLeave * match ExtraWhitespace /\s\+$/
        autocmd BufWinLeave * call clearmatches()
    endif
endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif

