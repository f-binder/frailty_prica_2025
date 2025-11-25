# cw function to paste to clipboard (e.g. table of results to export to sheets)
cw <- function(x){clipr::write_clip(x, return_new = T)}
