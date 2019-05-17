let mapleader = ' '
let g:mapleader = ' '

nmap <leader>w :w!<cr>
nmap <leader>q :q<cr>
nmap <leader>x :x<cr>

cmap w!! w !sudo tee > /dev/null %

" OPEN FILES IN DIRECTORY OF CURRENT FILE
cnoremap <expr> %% expand('%:h').'/'
map <leader>e :edit %%
map <leader>v :view %%

" KEYMAP WINDOW NAVIGATION
map <leader>h <C-w>h
map <leader>j <C-w>j
map <leader>k <C-w>k
map <leader>l <C-w>l

" Navigation Buffer
nmap <leader>n :bn<cr>
nmap <leader>p :bp<cr>
map <leader>] :bn<cr>
map <leader>[ :bp<cr>
map <leader><right> :bn<cr>
map <leader><left> :bp<cr>
