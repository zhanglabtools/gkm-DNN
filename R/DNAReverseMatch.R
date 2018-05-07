DNAReverseMatch <- function(s, 
                            alphabet = c("A", "C", "G", "T"),
                            complement = c("T", "G", "C", "A"),
                            nonInfo = "N") {
  # For each sequence in s, find the position of its reverse complement in s. 
  #
  # Args:
  #   s: A vector of DNA strings.Typically, s is the output of 
  #   LmerName or GkmName function.
  #   dict: Character vector. The dictionary of your alphabat.
  #   complement: Character vector. The complement of dict.
  #
  # Returns:
  #   A list of three components. The first and second components are positions 
  #   of matched ones, the last element is the position of unique ones in s 
  #   (same as their reverse complements).
  
  # generate the reverse complement of s.
  s <- toupper(s)
  s1 <- strsplit(s, NULL)
  s1 <- lapply(s1, rev)
  s1 <- lapply(s1, factor, levels = c(alphabet, nonInfo))
  s1 <- lapply(s1, 'levels<-', c(complement, nonInfo))
  s1 <- lapply(s1, as.character)
  s1 <- sapply(s1, paste, collapse = "", sep = "")
  
  # find the matches.
  rawMatch <- match(s, s1)
  uniMatch <- which(seq_along(s) == rawMatch)
  douMatch <- which(seq_along(s) < rawMatch)
  result <- list(seq_along(s1)[douMatch], rawMatch[douMatch])
  result[["unique"]] <- uniMatch
  return(result)
}
