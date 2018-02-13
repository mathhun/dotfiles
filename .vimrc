call plug#begin('~/.vim/plugged')

Plug 'w0rp/ale'
Plug 'vim-airline/vim-airline'
Plug 'eagletmt/ghcmod-vim'
Plug 'Shougo/vimproc'
Plug 'derekwyatt/vim-scala'

call plug#end()

syntax on
filetype plugin indent on
set guicursor+=a:blinkon0

set ts=2 sw=2 et
set autochdir

" ctags
nnoremap <C-]> g<C-]>

" scala
au BufNewFile,BufRead *.scala set tags+=$HOME/scala.tags

" netrw
let g:netrw_silent = 1

" font
set guifont=Osaka-Mono:h14
