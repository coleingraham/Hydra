
" set .hydra files to lua filetype
au BufNewFile,BufRead,BufEnter *.hydra set filetype=lua

" format the selected text for use with oscsend
function! s:hydra_get_visual_selection()
    " Why is this not a built-in Vim script function?!
    let [lnum1, col1] = getpos("'<")[1:2]
    let [lnum2, col2] = getpos("'>")[1:2]
    let lines = getline(lnum1, lnum2)
    let lines[-1] = lines[-1][: col2 - (&selection == 'inclusive' ? 1 : 2)]
    let lines[0] = lines[0][col1 - 1:]
    let i = 0
    for line in lines 
        let block_comment_start = matchstr(line,"--[[")
        let block_comment_end = matchstr(line,"--]]")
        let inline_comment = matchstr(line,"--")
        if(empty(block_comment_start) && empty(block_comment_end) && !empty(inline_comment))
             let lines[i] = substitute(line,"--","--[[ ","") . " --]]"
         endif
         let i += 1
    endfor
    return join(lines, " ")
endfunction

" send the selected code to Hydra
function! HydraCode() range
    let code = s:hydra_get_visual_selection()
    let escaped_code = escape(code,'"%#\')
    silent! exe "!oscsend localhost " . g:hydra_port . " /code s \"" . escaped_code . "\""
    redraw!
endfunction

