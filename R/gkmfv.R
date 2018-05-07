# last time modified: 2018/5/5
# Contain many function to count the lmerfv, gkmfv and gkmcfv
require(stringr)
require(parallel)
require(magrittr)

Gkmcfv <- function(sequence,
                   l = 4,
                   k = 3,
                   alphabet = c("A", "C", "G", "T"),
                   nonInfo = "N",
                   RCmode = TRUE,
                   thread = 1) {
  stopifnot(k <= l)
  lmerfv <- Lmerfv(sequence = sequence, l = k, alphabet = alphabet,
                   RCmode = RCmode, thread = thread)
  result <- list(lmerfv)
  if(k < l) {
    for (i in (k+1):l) {
      gkmfv <- Gkmfv(sequence = sequence, l = i, k = k, alphabet = alphabet,
                     nonInfo = nonInfo, gapEnd = FALSE,
                     RCmode = RCmode, thread = thread)
      result[[i - k + 1]] <- gkmfv
    }
  }
  result <- do.call(cbind, result)
  result
}

Gkmfv <- function(sequence,
                  l = 4,
                  k = 3,
                  alphabet = c("A", "C", "G", "T"),
                  nonInfo = "N",
                  gapEnd = TRUE,
                  RCmode = TRUE,
                  thread = 1) {

  stopifnot(l > k)
  # 0) calculation, a help function
  lmer2gkm <- function(lmer, tk, tgap, tl) {
    lmer <- strsplit(lmer, NULL) 
    lmer <- do.call(rbind, lmer)
    gkm <- lapply(1:ncol(tgap), function(x) {
      temp <- lmer
      temp[, tgap[[x]]] <- ""
      temp <- apply(temp, 1, paste, collapse = "")
      temp <- strtoi(temp, base = 4) + (x-1)*4^k+1
    })
    gkm <- table(unlist(gkm))
    index <- as.integer(names(gkm))
    value <- as.integer(gkm)
    isNa <- !is.na(index)
    index <- index[isNa]
    value <- value[isNa]
    result <- integer(length = tl)
    result[index] <- value
    matrix(result, nrow = 1)
  }
  
  #special cases
  if (k == 1 && !gapEnd) return(Lmerfv(sequence = sequence, l = 1,
                                       alphabet = alphabet, RCmode = RCmode))
  
  # 1) transformation
  rname <- sequence
  sequence <- str_to_upper(sequence)
  replace <- as.character(1:length(alphabet) - 1)
  names(replace) <- alphabet
  sequence <- str_replace_all(sequence, replace)
  
  # 2) split into l-mer list
  slength <- nchar(sequence)
  stopifnot(all(slength >= l))
  sta <- lapply(slength, function(x, l) 1:(x-l+1), l = l)
  end <- lapply(sta, function(x) x + l - 1)
  lmerList <- Map(stringr::str_sub, sequence, sta, end)
  
  # 3) from l-mer to gapper k-mer
  gap <- if (gapEnd) combn(l, l - k) else combn(l - 2, l - k) + 1
  gap <- as.data.frame(gap)
  cname <- GkmName(l = l, k = k, alphabet = alphabet, nonInfo = nonInfo,
                   gapEnd = gapEnd, degenerate = F)
  if (thread > 1) {
    cl <- makeCluster(thread)
    gkmList <- parLapplyLB(cl, lmerList, lmer2gkm, 
                           tk = k, tgap = gap, tl = length(cname))
    stopCluster(cl)
  } else {
    gkmList <- lapply(lmerList, lmer2gkm, tk = k, tgap = gap, tl = length(cname))
  }
  names(gkmList) <- NULL
  gkmfv <- do.call(rbind, gkmList)
  rownames(gkmfv) <- rname
  colnames(gkmfv) <- cname
  
  # 4) deal with double strand information
  if (is.list(RCmode)) {
    rc <- RCmode
  } else if (RCmode) {
    rc <- DNAReverseMatch(cname, nonInfo = nonInfo)
  } else {
    rc <- NULL
  }
  if (!is.null(rc)) {
    gkmfv <- cbind(gkmfv[, rc[[1]]] + gkmfv[, rc[[2]]],
                   gkmfv[, rc[[3]]] + gkmfv[, rc[[3]]])
    colnames(gkmfv) <- c(paste(cname[rc[[1]]], cname[rc[[2]]], sep = "|"),
                         cname[rc[[3]]])
  }
  gkmfv
}

Lmerfv <- function(sequence,
                   l = 4,
                   alphabet = c("A", "C", "G", "T"),
                   RCmode = TRUE,
                   thread = 1) {
  
  # 0) core calculation
  lmer2vec <- function(lmer, tl) {
    temp <- strtoi(lmer, base = 4) + 1
    temp <- table(temp)
    index <- as.integer(names(temp))
    value <- as.integer(temp)
    isNa <- !is.na(index)
    index <- index[isNa]
    value <- value[isNa]
    result <- integer(length = tl)
    result[index] <- value
    matrix(result, nrow = 1)
  }
  
  # 1) transformation
  rname <- sequence
  replace <- as.character(1:length(alphabet) - 1)
  names(replace) <- alphabet
  sequence <- sequence %>% str_to_upper() %>% str_replace_all(replace)
  
  # 2) split into l-mer list
  slength <- nchar(sequence)
  stopifnot(all(slength >= l))
  sta <- lapply(slength, function(x) 1:(x-l+1))
  end <- lapply(sta, '+', l - 1)
  lmerList <- Map(stringr::str_sub, sequence, sta, end)
  
  # 3) from l-mer to count table
  cname <- LmerName(l = l, alphabet = alphabet)
  if (thread > 1) {
    cl <- makeCluster(thread)
    lmerList <- parLapplyLB(cl, lmerList, lmer2vec, tl = length(cname))
    stopCluster(cl)
  } else {
    lmerList <- lapply(lmerList, lmer2vec, tl = length(cname))
  }
  names(lmerList) <- NULL
  lmerfv <- do.call(rbind, lmerList)
  rownames(lmerfv) <- rname
  colnames(lmerfv) <- cname
  
  # 4) deal with double strand information
  if (is.list(RCmode)) {
    rc <- RCmode
  } else if (RCmode) {
    rc <- DNAReverseMatch(cname)
  } else {
    rc <- NULL
  }
  if (!is.null(rc)) {
    lmerfv <- cbind(lmerfv[, rc[[1]]] + lmerfv[, rc[[2]]],
                    lmerfv[, rc[[3]]] + lmerfv[, rc[[3]]])
    colnames(lmerfv) <- c(paste(cname[rc[[1]]], cname[rc[[2]]], sep = "|"),
                          cname[rc[[3]]])
  }
  lmerfv
}
