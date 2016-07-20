syntax on

"dein Scripts-----------------------------
if &compatible
  set nocompatible
endif

" Required:
set runtimepath^=~/.vim/dein/repos/github.com/Shougo/dein.vim

" Required:
call dein#begin(expand('~/.vim/dein'))

" Let dein manage dein
" Required:
call dein#add('Shougo/dein.vim')

" Add or remove your plugins here:
call dein#add('Shougo/neosnippet.vim')
call dein#add('Shougo/neosnippet-snippets')
call dein#add('Shougo/vimshell', { 'rev': '3787e5' })
call dein#add('Shougo/vimproc.vim', {'build': 'make'})
call dein#add('Shougo/unite.vim')
call dein#add('Shougo/neomru.vim')

" programming
call dein#add('scrooloose/nerdtree')
call dein#add('scrooloose/syntastic')
call dein#add('tpope/vim-surround' )
call dein#add('bronson/vim-trailing-whitespace')
call dein#add('lambdalisue/vim-gista', {'on_cmd': 'Gista'})
call dein#add('tomasr/molokai')
call dein#add('ciaranm/inkpot')
call dein#add('nathanaelkane/vim-indent-guides')
" scala
call dein#add('derekwyatt/vim-scala')
call dein#add('ensime/ensime-vim')

" Required:
call dein#end()

" Required:
filetype plugin indent on

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif
"End dein Scripts-------------------------

" Unite
let g:unite_enable_start_insert=1
noremap <C-P> :Unite buffer<CR>
noremap <C-N> :Unite -buffer-name=file file<CR>
au FileType unite nnoremap <silent> <buffer> <ESC><ESC> :q<CR>
au FileType unite inoremap <silent> <buffer> <ESC><ESC> <ESC>:q<CR>

" programming
set ts=2 sw=2 et
set background=dark
colorscheme delek

" scala
autocmd BufWritePost *.scala :EnTypeCheck
nnoremap <localleader>t :EnTypeCheck<CR>
au FileType scala nnoremap <localleader>df :EnDeclaration<CR>
