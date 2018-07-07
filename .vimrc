call plug#begin('~/.vim/plugged')

Plug 'w0rp/ale'
Plug 'vim-airline/vim-airline'
Plug 'derekwyatt/vim-scala'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'dracula/vim', { 'as': 'dracula' }

call plug#end()

syntax on
filetype plugin indent on
set guicursor+=a:blinkon0
set noeb vb t_vb=

set ts=2 sw=2 et
set autochdir
set autowrite
let mapleader = ","
colorscheme dracula

" quickfix
map <C-n> :cnext<CR>
map <C-m> :cprevious<CR>
nnoremap <leader>a :cclose<CR>

" ctags
nnoremap <C-]> g<C-]>

" netrw
let g:netrw_silent = 1

"
" scala
"
au BufNewFile,BufRead *.scala set tags+=$HOME/scala.tags

"
" go
"
let $GOPATH = $HOME.'/dev'
let $PATH = $HOME.'/dev/bin:'.$PATH
let g:go_fmt_command = "goimports"

autocmd FileType go nmap <leader>b :<C-u>call <SID>build_go_files()<CR>
autocmd FileType go nmap <leader>r  <Plug>(go-run)
autocmd FileType go nmap <leader>t  <Plug>(go-test)
autocmd FileType go nmap <Leader>c <Plug>(go-coverage-toggle)

" run :GoBuild or :GoTestCompile based on the go file
function! s:build_go_files()
  let l:file = expand('%')
  if l:file =~# '^\f\+_test\.go$'
    call go#test#Test(0, 1)
  elseif l:file =~# '^\f\+\.go$'
    call go#cmd#Build(0)
  endif
endfunction
