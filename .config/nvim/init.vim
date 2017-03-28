" Adapted in part from ccorbi@github
"
" ----------------------------------------------------------------------
" | General Settings                                                   |
" ----------------------------------------------------------------------
"
"  UI settings
" ______________

set nocompatible               " Don't make Vim vi-compatibile.

set backspace=indent           " ┐
set backspace+=eol             " │ Allow `backspace`
set backspace+=start           " ┘ in insert mode.

set cursorline                 " Highlight the current line.
hi CursorLine cterm=bold gui=bold

"set cpoptions+=$               " When making a change, don't
                               " redisplay the line, and instead,
                               " put a `$` sign at the end of
                               " the changed text.

set laststatus=2               " Always show the status line.

set lazyredraw                 " Do not redraw the screen while
                               " executing macros, registers
                               " and other commands that have
                               " not been typed.

set listchars=tab:▸\           " ┐
set listchars+=trail:·         " │ Use custom symbols to
set listchars+=eol:↴           " │ represent invisible characters.
set listchars+=nbsp:_          " ┘
set list

set nojoinspaces               " When using the join command,
                               " only insert a single space
                               " after a `.`, `?`, or `!`.

set number                     " Show line number.
set numberwidth=5              " Increase the minimal number of
                               " columns used for the `line number`.
set report=0                   " Report the number of lines changed.
"set relativenumber             " Use relative line numbers. Current line is still in status bar.

set ruler                      " Show cursor position.

set noshowmode                 " Don't show the current mode (airline.vim takes care of us)

set nostartofline              " Keep the cursor on the same column.

set tabstop=2                  " ┐
set smarttab                   " |
set softtabstop=2              " │ Set global <TAB> settings.
set shiftwidth=4               " │
set expandtab                  " ┘

set scrolloff=5                " When scrolling, keep the cursor
                               " 5 lines below the top and 5 lines
                               " above the bottom of the screen.

set showcmd                    " Show the command being typed.
set showmatch                  " Highlight matching brackets
set showtabline=2              " Always show tab bar.

set spelllang=en_us            " Set the spellchecking language.

set synmaxcol=2500             " Limit syntax highlighting (this
                               " avoids the very slow redrawing
                               " when files contain long lines).

set title                      " Show the filename in the window titlebar.

"set virtualedit=all            " Allow cursor to be anywhere.

set wildmenu                   " Enable enhanced command-line
                               " completion (by hitting <TAB> in
                               " command mode, Vim will show the
                               " possible matches just above the
                               " command line with the first
                               " match highlighted).

set winminheight=0             " Allow windows to be squashed.

"
" Search & Replace
" __________________

set gdefault                   " By default add g flag to search/replace. Add g to toggle.
set hlsearch                   " Enable search highlighting.
set ignorecase                 " Ignore case in search patterns.
set incsearch                  " Highlight search pattern
                               " as it is being typed
set smartcase                  " Override `ignorecase` option
                               " if the search pattern contains
                               " uppercase characters.
set magic                      " Enable extended regexp.

"
" Switching windows
" ------------------0
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
noremap <C-h> <C-w>h

" Search mappings: These will make it so that going to the next one in a
" search will center on the line it's found in.
nnoremap n nzzzv
nnoremap N Nzzzv



"
" File Administration
" ____________________

set encoding=utf-8							 " Use UTF-8 without BOM.
set fileencoding=utf-8
set binary

"" Fix backspace indent
set backspace=indent,eol,start


set wildignore+=*.jpeg          " ┐
set wildignore+=*.jpeg          " |
set wildignore+=*.gif           " |
set wildignore+=*.png           " |
set wildignore+=*.gif           " |
set wildignore+=*.psd           " |
set wildignore+=*.o             " | Igoore these filetypes
set wildignore+=*.obj           " |
set wildignore+=*.min.js        " |
set wildignore+=*/bower_c*/*    " |
set wildignore+=*/node_m*/*     " |
set wildignore+=*/vendor/*      " |
set wildignore+=*/.git/*        " |
set wildignore+=*/.hg/*         " |
set wildignore+=*/.svn/*        " |
set wildignore+=*/log/*         " |
set wildignore+=*/tmp/*         " ┘

" In case we forgot to sudo nvim protected files`
cmap w!! w !sudo tee > /dev/null %

set backup                      " keep a backup file

if !strlen($SUDO_USER) && has('unix') && has('persistent_undo')

  " Keep per-file undo history in ~/.local/share/nvim/undo; the double-slash at the end
  " of the directory prods Vim into keeping the full path to the file in its
  " undo filename to avoid collisions; the same thing works for swap files

  set undofile
  set undodir^=~/.local/share/nvim/undo//

  " Create the ~/.local/nvim/undo directory if necessary and possible

  if !isdirectory($HOME . '/.local/share/nvim/undo') && exists('*mkdir')
    call mkdir($HOME . '/.local/share/nvim/undo', 'p', 0700)
  endif

  " Don't track changes to sensitive files

  "if has('autocmd')
   "augroup undoskip
      "autocmd!
      "silent! autocmd BufWritePre
          "\ /tmp/*,$TMPDIR/*,$TMP/*,$TEMP/*,*/shm/*
          "\ setlocal noundofile
    "augroup END
  "endif
endif

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set buftype=nofile | read ++edit # | 0d_ | diffthis
                 \ | wincmd p | diffthis
endif

function! Preserve(command)
		"save last search
		let _s=0

		" Save cursor position
		let l = line('.')
		let c = col('.')

		execute a:command
		let @/=_s
		call cursor(l, c)
endfunction

function! StripTrailingWhitespaces()
    call Preserve("%s/\\s\\+$//e")
endfunction



" ----------------------------------------------------------------------
" | Plugins                                                            |
" ----------------------------------------------------------------------

" - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
" - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

" Initialize Plugin and specify the path
" where the plugins should be installed.
call plug#begin('~/.local/share/nvim/plugged')
" 24b Colorschemes
Plug 'rakr/vim-one'
" Status line
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'jacoborus/tender'
" Sync airline with tmux
Plug 'edkolev/tmuxline.vim'
Plug 'osyo-manga/unite-airline_themes'
" Vim-Signify
Plug 'mhinz/vim-signify'
" Improved linter system
Plug 'w0rp/ale'
"" Plugins for typescript
" semantic autocomplete
function! BuildYCM(info)
    if a:info.status != 'installed' || a:info.force
        !python2 ./install.py --tern-completer
    endif
endfunction
Plug 'Valloric/YouCompleteMe', {'do': function('BuildYCM')}
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" Prerequisite for Tsuquyomi
Plug 'quramy/typescript-vim'
Plug 'shougo/denite.nvim'
Plug 'shougo/vimproc.vim', {'do' : 'make' }
Plug 'quramy/tsuquyomi'
" Indentation for typescript
Plug 'Quramy/vim-js-pretty-template'
" Pretty icons
Plug 'ryanoasis/vim-devicons'
" NERD File navigator
Plug 'scrooloose/nerdtree'
" NERD Commenter
Plug 'scrooloose/nerdcommenter'
" Git plugins
Plug 'tpope/vim-fugitive'
" git plugin for NERDTree
"Plug 'xuyuanp/nerdtree-git-plugin'
Plug 'airblade/vim-gitgutter'
" Ensure closing of brackets
Plug 'Raimondi/delimitMate'
" Start Menu
Plug 'mhinz/vim-startify'
" Plugin for use with Elixir
Plug 'slashmili/alchemist.vim'
" Plugin to handle focus events
Plug 'tmux-plugins/vim-tmux-focus-events'
" Plugin to include 24bit theme
Plug 'morhetz/gruvbox'
" Elixir and Phoenix Plugin
Plug 'elixir-lang/vim-elixir'
" Indent Guide
Plug 'nathanaelkane/vim-indent-guides'
" Truecolor theme
Plug 'lifepillar/vim-solarized8'
" Colorize hex code and color names
Plug 'chrisbra/Colorizer'
" Syntax Highlighting for tmux.conf files
Plug 'keith/tmux.vim'
" Colorschmes
Plug 'flazz/vim-colorschemes'
" Integration with tmux and iTerm2
Plug 'wincent/terminus'
call plug#end()

if has("autocmd")
    filetype plugin indent on
    "           │     │    └──── Enable file type detection.
    "           │     └───────── Enable loading of indent file.
    "           └─────────────── Enable loading of plugin files.
    augroup vimrc
        autocmd!
				autocmd vimrc ColorScheme * :hi NonText ctermfg=236
				autocmd vimrc ColorScheme * :hi SpecialKey ctermfg=236

				" Show trailing whitespace.
				autocmd vimrc ColorScheme * :hi ExtraWhitespace ctermbg=red guibg=red

				" Make selection more visible.
				autocmd vimrc ColorScheme * :hi Visual guibg=#00588Av
				autocmd vimrc ColorScheme * :hi link multiple_cursors_cursor Search
				autocmd vimrc ColorScheme * :hi link multiple_cursors_visual Visual
    augroup END
endif

set autoindent                 " Copy indent to the new line.


" Restore curent cursor when opening file.
if has("autocmd")
	au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") |     exe "normal! g'\"" | endif
endif
" ----------------------------------------------------------------------
" | Plugins - Airline                                                  |
" ----------------------------------------------------------------------

let g:airline_powerline_fonts = 1

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_nr_format = '%s '
let g:airline#extensions#tabline#buffer_nr_show = 1

"let g:airline#extensions#tabline#fnamecollapse = 0

let g:airline#extensions#tabline#fnamemod = ':t'

" Don't trigger writing to .tmux.conf everytime Airline applies a new theme
let g:airline#extensions#tmuxline#enabled = 0
" Specify which file the snapshot should be written to:
let g:airline#extensions#tmuxline#snapshot_file = "~/.tmux-statusline-colors.conf"
" Enable smarter tab handling
let g:airline#extensions#tabline#enabled = 1

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

if !exists('g:airline_powerline_fonts')
  let g:airline#extensions#tabline#left_sep = ' '
  let g:airline#extensions#tabline#left_alt_sep = '|'
  let g:airline_left_sep          = '▶'
  let g:airline_left_alt_sep      = '»'
  let g:airline_right_sep         = '◀'
  let g:airline_right_alt_sep     = '«'
  let g:airline#extensions#branch#prefix     = '⤴' "➔, ➥, ⎇
  let g:airline#extensions#readonly#symbol   = '⊘'
  let g:airline#extensions#linecolumn#prefix = '¶'
  let g:airline#extensions#paste#symbol      = 'ρ'
  let g:airline_symbols.linenr    = '␊'
  let g:airline_symbols.branch    = '⎇'
  let g:airline_symbols.paste     = 'ρ'
  let g:airline_symbols.paste     = 'Þ'
  let g:airline_symbols.paste     = '∥'
  let g:airline_symbols.whitespace = 'Ξ'
else
  let g:airline#extensions#tabline#left_sep = ''
  let g:airline#extensions#tabline#left_alt_sep = ''
  " powerline symbols
  let g:airline_left_sep = ''
  let g:airline_left_alt_sep = ''
  let g:airline_right_sep = ''
  let g:airline_right_alt_sep = ''
  let g:airline_symbols.branch = ''
  let g:airline_symbols.readonly = ''
  let g:airline_symbols.linenr = ''
endif

" ----------------------------------------------------------------------
" | Plugins - NERDTree                                                 |
" ----------------------------------------------------------------------

let NERDTreeShowHidden = 1
let NERDTreeMouseMode = 2
let NERDTreeMinimalUI = 1

augroup nerd_tree
		autocmd!
		autocmd vimrc StdinReadPre * let s:std_in=1
		" If nvim is started with an input directory, start up NERDTree
		autocmd vimrc VimEnter *
			\ if argc() == 1 && isdirectory(argv(0)) |
			\   bd |
			\   exec 'cd' fnameescape(argv(0)) |
			\   NERDTree |
			\ end
augroup end

" ----------------------------------------------------------------------
" | Plugins - Indent Guides                                            |
" ----------------------------------------------------------------------

let g:indent_guides_auto_colors = 0
augroup indentguides
		autocmd!
		autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd
						\ guibg=#00323D
						\ ctermbg=Magenta

		autocmd VimEnter,Colorscheme * :hi IndentGuidesEven
						\ guibg=#073642
						\ ctermbg=DarkMagenta
augroup end

" ----------------------------------------------------------------------
" | Plugins - tsuquyomi                                                |
" ----------------------------------------------------------------------

let g:typescript_compiler_binary = 'tsuquyomi'
let g:typescript_compiler_options = ''

" ----------------------------------------------------------------------
" | Misc. Automatic Commands                                           |
" ----------------------------------------------------------------------

if has("autocmd")

    " Autocommand Groups.
    " http://learnvimscriptthehardway.stevelosh.com/chapters/14.html

    " - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    " Automatically reload the configurations from
    " the `~/.config/nvim/init.vim` file whenever they are changed.

    augroup auto_reload_vim_configs

        autocmd!
        autocmd BufWritePost vimrc source $MYVIMRC

    augroup END

    " - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    " Use relative line numbers.
    " http://jeffkreeftmeijer.com/2012/relative-line-numbers-in-vim-for-super-fast-movement/

    augroup relative_line_numbers

        autocmd!

        " - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        " Show absolute numbers in insert mode, otherwise relative line numbers

        autocmd vimrc InsertEnter * :set norelativenumber
        autocmd vimrc InsertLeave * :set relativenumber


        " - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        " Automatically switch to absolute
        " line numbers when Vim loses focus.

        autocmd FocusLost * :set number

        " - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        " Automatically switch to relative
        "( line numbers when Vim gains focus.

        autocmd FocusGained * :set relativenumber

    augroup END

    " - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    " Automatically strip the trailing
    " whitespaces when files are saved.

    augroup strip_trailing_whitespaces

        " List of file types that use the trailing whitespaces:
        "
        "  * Markdown
        "    https://daringfireball.net/projects/markdown/syntax#block

        let excludedFileTypes = [
            \ "markdown",
            \ "mkd.markdown"
						\]

        " - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        " Only strip the trailing whitespaces if
        " the file type is not in the excluded list.

        autocmd!

        autocmd BufWritePre * if index(excludedFileTypes, &ft) < 0 | :call StripTrailingWhitespaces()

    augroup END

    " - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    " Automatically set column width to 78 for
    " text files
    augroup vimrcEx

      autocmd!

        autocmd FileType text setlocal textwidth=78
        autocmd FileType typescript JsPreTmpl html

    augroup END

endif
" ----------------------------------------------------------------------
" | Color Scheme                                                       |
" ----------------------------------------------------------------------

set t_Co=256                   " Enable full-color support.

set background=dark            " Use colors that look good
                               " on a dark background.
let g:airline_theme='ravenpower' " Set the airline theme
syntax on                      " Enable syntax highlighting.
colorscheme gruvbox

if has("termguicolors")
	set termguicolors
	colorscheme gruvbox
	let g:airline_theme='gruvbox'
endif

"if !has("gui_running")
"    colorscheme solarized8_dark
"endif


" ----------------------------------------------------------------------
" | Key Mappings                                                       |
" ----------------------------------------------------------------------

if has('user_commands')                                    " ┐
 command! -bang -complete=file -nargs=? E e<bang> <args>   " |
 command! -bang -complete=file -nargs=? W w<bang> <args>   " | I fat finger :wq a lot
 command! -bang -complete=file -nargs=? WQ wq<bang> <args> " | it.
 command! -bang -complete=file -nargs=? Wq wq<bang> <args> " |
 command! -bang Q q<bang>                                  " |
 command! -bang Qa qa<bang>                                " |
 command! -bang QA qa<bang>                                " |
 command! -bang Wa wa<bang>                                " |
 command! -bang WA wa<bang>                                " |
endif                                                      " ┘

inoremap <C-U> <C-G>u<C-U>       " ^-U in inset mode deletes a lot, so use
                                " ^-G-u to first break undo, then ^-U after inserting
                                " line break

" [\* ] Search and replace the word under the cursor.
nmap <leader>* :%s/\<<C-r><C-w>\>//<Left>

" [\cs] Clear search.
map <leader>cs <Esc>:noh<CR>

" [\n ] Toggle NERDTree.
map <leader>n :NERDTreeToggle<CR>

" [\v ] Make the opening of the `.vimrc` file easier.
nmap <leader>v :vsp $MYVIMRC<CR>
