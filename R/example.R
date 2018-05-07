# examples
source('GkmName.R')
source('DNAReverseMatch.R')
source('gkmfv.R')

#########################################
# 1) get the name of L-mers
print(LmerName(2))

# 2) get the name of gapped k-mers
print(GkmName(l = 4, k = 2, gapEnd = T))

# 3) get the name of concise gapped k-mers
# this is the default option
print(GkmName(l = 4, k = 2, gapEnd = F, degenerate = T))
#########################################
sequence <- c("ACTGACTG", "CGCTTGGTAA")

# 4) Counting lmer-fv
# default: counting the double strand DNA sequence
lmerfv <- Lmerfv(sequence, 2)
print(lmerfv)

# 5) Counting the raw gkm-fv
gkmfv <- Gkmfv(sequence, 4, 2)
print(gkmfv[, 1:10])

# 6) Counting the concise gkm-fv
gkmcfv <- Gkmcfv(sequence, 4, 2)
print(gkmcfv)

# 7) Counting the concise gkm-fv (l = 7, k = 5, default in the paper)
gkmcfv <- Gkmcfv(sequence, 7, 5)
print(gkmcfv)