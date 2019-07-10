" ==========================================================
" Dependencies
" ==========================================================
"
" - pylint (python linter)
" - flake8 (on-the-fly linter)
" - jedi (autocomplete)
" - yapf (autoformat)
"
" - exuberant-ctags

" ==========================================================
" Shortcuts
" ==========================================================
set nocompatible              " Don't be compatible with vi
let mapleader=","             " change the leader to be a comma vs slash


fu! SplitScroll()
    :wincmd v
    :wincmd w
    execute "normal! \<C-d>"
    :set scrollbind
    :wincmd w
    :set scrollbind
endfu

nmap <leader>sb :call SplitScroll()<CR>


"<CR><C-w>l<C-f>:set scrollbind<CR>

" ctrl-jklm  changes to that split
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h

" Open NerdTree
map <leader>n :NERDTreeToggle<CR>

map <leader>f :CtrlP<CR>
map <leader>b :CtrlPBuffer<CR>

" ==========================================================
" Vim-Plug - Allows us to organize our vim plugins
" ==========================================================

" Specify a directory for plugins
call plug#begin('~/.vim/plugged')

" Navigate tmux + vim
Plug 'christoomey/vim-tmux-navigator'

" VIM ---(text)---> screen / tmux
Plug 'jpalardy/vim-slime'
let g:slime_target = "tmux"
" expects 2 tmux plan with repl in 2.
let g:slime_python_ipython = 1
"let g:slime_default_config = {"socket_name": split($TMUX, ",")[0], "target_pane": ":.1"}

" create tags
Plug 'ludovicchabant/vim-gutentags'

" cross file / buffer search
Plug 'ctrlpvim/ctrlp.vim'

" Folder outliner
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

" faster folding
Plug 'Konfekt/FastFold'

" universal outcommanding of lines
Plug 'scrooloose/nerdcommenter'

" Git
Plug 'tpope/vim-fugitive', { 'tag': 'v2.4'}
Plug 'airblade/vim-gitgutter'

Plug 'plasticboy/vim-markdown'

" linter
Plug 'w0rp/ale'
" 18.04 repo version has cursor bug
" https://github.com/w0rp/ale/issues/1334
" bug tracker https://bugs.launchpad.net/ubuntu/+source/vim/+bug/1768026
" this is a workarount
"let g:ale_echo_cursor = 0

" Color theme
Plug 'liuchengxu/space-vim-dark'

" Status bar
Plug 'vim-airline/vim-airline'

" Quoting/parenthesizing
Plug 'tpope/vim-surround'

" auto-complete for quotes, parens, brackets
Plug 'Raimondi/delimitMate'

" autocomplete
Plug 'maralla/completor.vim'

" correct python folding
Plug 'tmhedberg/SimpylFold'

" Use TAB to complete when typing words, else inserts TABs as usual.  Uses
" dictionary, source files, and completor to find matching words to complete.

" Note: usual completion is on <C-n> but more trouble to press all the time.
" Never type the same word twice and maybe learn a new spellings!
" Use the Linux dictionary when spelling is in doubt.
function! Tab_Or_Complete() abort
  " If completor is already open the `tab` cycles through suggested completions.
  if pumvisible()
    return "\<C-N>"
  " If completor is not open and we are in the middle of typing a word then
  " `tab` opens completor menu.
  elseif col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~ '^\w'
    return "\<C-R>=completor#do('complete')\<CR>"
  else
    " If we aren't typing a word and we press `tab` simply do the normal `tab`
    " action.
    return "\<Tab>"
  endif
endfunction

" Use `tab` key to select completions.  Default is arrow keys.
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Use tab to trigger auto completion.  Default suggests completions as you type.
let g:completor_auto_trigger = 0
inoremap <expr> <Tab> Tab_Or_Complete()

" Initialize plugin system
call plug#end()

" ==========================================================
" Basic Settings
" ==========================================================
syntax on                     " syntax highlighing
filetype on                   " try to detect filetypes
filetype plugin indent on     " enable loading indent file for filetype
set number                    " Display line numbers
set numberwidth=1             " using only 1 column (and 1 space) while possible
set background=dark           " We are using dark background in vim
set title                     " show title in console title bar
set wildmenu                  " Menu completion in command mode on <Tab>
set wildmode=full             " <Tab> cycles between all matching choices.
set dir=/home/ibayer/tmp,/var/tmp,/tmp

" don't bell or blink
set noerrorbells
set vb t_vb=

" Ignore these files when completing
set wildignore+=*.o,*.obj,.git,*.pyc
set wildignore+=eggs/**
set wildignore+=*.egg-info/**

" Load Project specific .vimrc files "
set exrc
set secure  " Disable unsave commands "


" Disable the colorcolumn when switching modes.  Make sure this is the
" first autocmd for the filetype here
"autocmd FileType * setlocal colorcolumn=0


""" Moving Around/Editing
set cursorline              " have a line indicate the cursor location
set ruler                   " show the cursor position all the time
set nostartofline           " Avoid moving cursor to BOL when jumping around
set virtualedit=block       " Let cursor move past the last char in <C-v> mode
set scrolloff=3             " Keep 3 context lines above and below the cursor
set backspace=2             " Allow backspacing over autoindent, EOL, and BOL
set showmatch               " Briefly jump to a paren once it's balanced
set nowrap                  " don't wrap text
set linebreak               " don't wrap textin the middle of a word
set autoindent              " always set autoindenting on
set smartindent             " use smart indent if there is no indent file
set tabstop=4               " <tab> inserts 4 spaces 
set shiftwidth=4            " but an indent level is 2 spaces wide.
set softtabstop=4           " <BS> over an autoindent deletes both spaces.
set expandtab               " Use spaces, not tabs, for autoindent/tab key.
set shiftround              " rounds indent to a multiple of shiftwidth
set matchpairs+=<:>         " show matching <> (html mainly) as well
set foldmethod=indent       " allow us to fold on indents
set foldlevel=99            " don't fold by default

" don't outdent hashes
inoremap # #

" close preview window automatically when we move around
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

"""" Reading/Writing
set noautowrite             " Never write a file unless I request it.
set noautowriteall          " NEVER.
set noautoread              " Don't automatically re-read changed files.
set modeline                " Allow vim options to be embedded in files;
set modelines=5             " they must be within the first or last 5 lines.
set ffs=unix,dos,mac        " Try recognizing dos, unix, and mac line endings.

"""" Messages, Info, Status
set ls=2                    " allways show status line
set vb t_vb=                " Disable all bells.  I hate ringing/flashing.
set confirm                 " Y-N-C prompt if closing with unsaved changes.
set showcmd                 " Show incomplete normal mode commands as I type.
set report=0                " : commands always print changed line count.
set shortmess+=a            " Use [+]/[RO]/[w] for modified/readonly/written.
set ruler                   " Show some info, even without statuslines.
set laststatus=2            " Always show statusline, even if only 1 window.
set statusline=[%l,%v\ %P%M]\ %f\ %r%h%w\ (%{&ff})\ %{fugitive#statusline()}

" displays tabs with :set list & displays when a line runs off-screen
set listchars=tab:>-,eol:$,trail:-,precedes:<,extends:>
set list

""" Searching and Patterns
set ignorecase              " Default to using case insensitive searches,
set smartcase               " unless uppercase letters are used in the regex.
set smarttab                " Handle tabs more intelligently 
set hlsearch                " Highlight searches by default.
set incsearch               " Incrementally search while typing a /regex

"""" Display
if has("gui_running")
    " Remove menu bar
    set guioptions-=m

    " Remove toolbar
    set guioptions-=T
endif


set termguicolors
" set Vim-specific sequences for RGB colors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

colorscheme space-vim-dark
hi Comment cterm=italic

" Paste from clipboard
map <leader>p "+p

" Exit insert mode
inoremap jj <Esc>   """ jj key is <Esc> setting

" Paste multiple time the same
xnoremap p pgvy

" Quit window on <leader>q
nnoremap <leader>q :q<CR>

" Save buffer to file
nnoremap <leader>s :w<CR>

" Autoformat python src with yapf
nnoremap <leader>f :ALEFix yapf<CR>

" hide matches on <leader>space
nnoremap <leader><space> :nohlsearch<cr>

" Remove trailing whitespace on <leader>S
nnoremap <leader>S :%s/\s\+$//<cr>:let @/=''<CR>

" Select the item in the list with enter
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"


if exists("&colorcolumn")
   set colorcolumn=79
endif
