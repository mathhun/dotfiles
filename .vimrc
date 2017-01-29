execute pathogen#infect()
syntax on
filetype plugin indent on

" Unite
" let g:unite_enable_start_insert=1
" noremap <C-P> :Unite buffer<CR>
" noremap <C-N> :Unite -buffer-name=file file<CR>
" au FileType unite nnoremap <silent> <buffer> <ESC><ESC> :q<CR>
" au FileType unite inoremap <silent> <buffer> <ESC><ESC> <ESC>:q<CR>

" programming
set ts=2 sw=2 et
set background=dark
colorscheme delek

" scala
autocmd BufWritePost *.scala :EnTypeCheck
nnoremap <localleader>t :EnTypeCheck<CR>
au FileType scala nnoremap <localleader>df :EnDeclaration<CR>
