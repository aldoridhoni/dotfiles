""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MULTIPURPOSE TAB KEY
" Indent if we're at the beginning of a line. Else, do completion.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <expr> <tab> InsertTabWrapper()
inoremap <s-tab> <c-n>

" CUSTOM AUTOCMDS
augroup vimrcEx
    autocmd!
    autocmd FileType text setlocal textwidth=78
    autocmd FileType ruby,haml,yaml,html,javascript,sass,rust set ai sw=2 sts=2 et
    autocmd FileType python set sw=4 sts=4 et
    autocmd BufNewFile,BufRead *.md set filetype=markdown
augroup END

autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif
  \| if !argc()
  \|   setlocal hidden noswapfile nospell
  \|   execute "normal! Gi \<ESC>"
  \|   echomsg 'Welcome to Vim'
  \| endif
