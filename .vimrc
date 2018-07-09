call plug#begin('~/.vim/plugged')

Plug 'w0rp/ale'
Plug 'vim-airline/vim-airline'
Plug 'crusoexia/vim-dracula'
Plug 'tpope/vim-fugitive'
Plug 'vim-syntastic/syntastic'

Plug 'derekwyatt/vim-scala'
Plug 'rust-lang/rust.vim'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

call plug#end()

syntax on
filetype plugin indent on
set guicursor+=a:blinkon0
set noeb vb t_vb=

set ts=2 sw=2 et
set autochdir
set autowrite
let mapleader = ","
if has("gui_macvim")
  color dracula
endif
set lines=999 columns=130

" kaoriya
set cmdheight=1

" quickfix
map <C-n> :cnext<CR>
map <C-m> :cprevious<CR>
nnoremap <leader>a :cclose<CR>

" ctags
nnoremap <C-]> g<C-]>

" netrw
let g:netrw_silent = 1

" airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 0
set guifont=Migu\ 1M:h12

" syntastic
"let g:syntastic_always_populate_loc_list = 1
"let g:syntastic_auto_loc_list = 1
"let g:syntastic_check_on_open = 1
"let g:syntastic_check_on_wq = 0

"
" scala
"
au BufNewFile,BufRead *.scala set tags+=$HOME/scala.tags

"
" rust
"
let $PATH = $HOME.'/.cargo/bin:'.$PATH
let g:rustfmt_autosave = 1

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
