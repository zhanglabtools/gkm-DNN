LmerName <- function(l = 2, alphabet = c("A", "C", "G", "T")) {
  # Generate the name of raw l-mers.
  #
  # Args:
  #   l: The word length of gapped l-mer. 
  #   alphabet: the alphabet of the desired l-mer.
  #
  # Returns:
  #   The names of l-mers. Results are in dictionary order.
  
  if (l == 1) return(alphabet)
  expand <- rep(LmerName(l-1, alphabet), each = length(alphabet))
  paste0(expand, alphabet)
}

GkmName <- function(l = 7,
                    k = 5, 
                    alphabet = c("A", "C", "G", "T"),
                    nonInfo = "N", 
                    gapEnd = FALSE,
                    degenerate = TRUE) {
  require(magrittr)
  # 1) check some special cases 
  stopifnot(k <= l)
  if (k == l) return(LmerName(l, alphabet = alphabet))
  if (k == 1 && !gapEnd) return(LmerName(1, alphabet = alphabet))
  
  # 2) generate gapped k-mer
  gap <- if (gapEnd) combn(l, l - k) else combn(l - 2, l - k)+1
  gap <- gap %>% as.data.frame() %>% lapply(setdiff, x = 1:l)
  nucMat <- LmerName(k, alphabet = alphabet) %>% 
    strsplit(split = NULL) %>% do.call(what = rbind) 
  gapMat <- matrix(nonInfo, nrow = 4^k, ncol = l)
  gkmName <- gap %>% lapply(function(x) gapMat %>% '[<-'(, x, nucMat) %>%
                      apply(1, paste, collapse = "")) %>% unlist()
  names(gkmName) <- NULL
  
  # 3) return the result
  if (!degenerate | gapEnd) return(gkmName)
  gkmName1 <- GkmName(l = l - 1, k = k, alphabet = alphabet, nonInfo = nonInfo, 
                      gapEnd = gapEnd, degenerate = degenerate)
  return(c(gkmName1, gkmName))
}
