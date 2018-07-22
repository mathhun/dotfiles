call plug#begin('~/.local/share/nvim/plugged')

Plug 'ctrlpvim/ctrlp.vim'
Plug 'itchyny/lightline.vim'
" color
Plug 'cocopon/iceberg.vim'
Plug 'rakr/vim-one'
Plug 'whatyouhide/vim-gotham'
Plug 'jacoborus/tender.vim'
" lang
Plug 'rust-lang/rust.vim'
Plug 'neovimhaskell/haskell-vim'
Plug 'alx741/vim-hindent'
Plug 'fatih/vim-go', { 'frozen': 1, 'tag': '*', 'do': ':GoUpdateBinaries' }

call plug#end()

let mapleader = ","
nnoremap <leader>, :e $HOME/.config/nvim/init.vim<CR>
set ts=4 sw=4 et

" color
set t_Co=256
set background=dark
colorscheme one
let g:lightline = { 'colorscheme': 'one' }

" quickfix
map <C-n> :cnext<CR>
map <C-m> :cprevious<CR>
nnoremap <leader>a :cclose<CR>

" ctags
nnoremap <C-]> g<C-]>

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
