call plug#begin('~/.local/share/nvim/plugged')

Plug 'ctrlpvim/ctrlp.vim'
Plug 'itchyny/lightline.vim'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'vimlab/split-term.vim'
Plug 'jiangmiao/auto-pairs'
" color
Plug 'cocopon/iceberg.vim'
Plug 'rakr/vim-one'
Plug 'whatyouhide/vim-gotham'
Plug 'jacoborus/tender.vim'
Plug 'dracula/vim'
" rust
Plug 'rust-lang/rust.vim'
" haskell
Plug 'neovimhaskell/haskell-vim'
Plug 'sbdchd/neoformat'
" go
Plug 'fatih/vim-go', {
    \ 'frozen': 1,
    \ 'tag': '*',
    \ 'do': ':GoUpdateBinaries'
    \ }
" elm
Plug 'ElmCast/elm-vim'
" RLS
Plug 'autozimu/LanguageClient-neovim', {
    \ 'frozen': 1,
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
" misc
Plug 'junegunn/fzf'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'w0rp/ale'

call plug#end()

let mapleader = ","
nnoremap <leader>, :e $HOME/.config/nvim/init.vim<CR>
set ts=4 sw=4 et
set noswapfile

" color
set t_Co=256
set background=dark
let g:dracula_italic = 0
colorscheme dracula
"let g:lightline = { 'colorscheme': 'one' }

" quickfix
map <C-n> :cnext<CR>
map <C-m> :cprevious<CR>
nnoremap <leader>a :cclose<CR>

" ctags
nnoremap <C-]> g<C-]>

" ctrlp
let g:ctrlp_custom_ignore = {
  \ 'dir': 'target$',
  \ }

" nerdtree
map <C-n> :NERDTreeToggle<CR>

" neoformat
augroup fmt
  autocmd!
  autocmd BufWritePre * undojoin | Neoformat
augroup END

"
" haskell
"
let g:neoformat_enabled_haskell = ['brittany', 'hindent', 'stylish-haskell']
let g:ale_linters_explicit = 1
let g:ale_linters = {'haskell': ['stack-ghc']}

"
" rust
"
let $PATH = $HOME.'/.cargo/bin:'.$PATH
let g:rustfmt_autosave = 1

"
" LSP
"
" Required for operations modifying multiple buffers like rename.
set hidden
let g:LanguageClient_serverCommands = {
    \ 'rust': [$HOME.'/.cargo/bin/rustup', 'run', 'stable', 'rls'],
    \ }
nnoremap <F5> :call LanguageClient_contextMenu()<CR>
" Or map each action separately
nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>

"
" ALE
"
let g:ale_linters = {'rust': ['rle']}
let g:ale_rust_rls_toolchain = 'stable'

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
