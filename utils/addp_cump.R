# adds p and cumulative sums to a count tibble

add_pcump <- function(a_tibble){
  if(sum(names(a_tibble) == "n") != 1){
    stop("tibble need to have one 'n' column")
  } else {
    a_tibble %>%
      mutate(p = n/sum(n)) %>%
      mutate(cump = cumsum(p),
             cumn = cumsum(n))
  }
}

message("add_pcum() function adds proportion and cumulative sums to a tibble \n")


