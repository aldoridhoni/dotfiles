" VIM PLUG
" Vim Plugin Manager
" https://github.com/junegunn/vim-plug
call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-commentary'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'racer-rust/vim-racer'
call plug#end()

" PLUGIN OPTIONS
" Vim-airline
let g:airline_theme='minimalist'
let g:airline_powerline_fonts = 0
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
