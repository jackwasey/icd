
#now some development code to see what permutations there are of ICD-10 codes
#based on the 2016 CM set.

i10 <- icd10cm2016$code

alpha_in_tail <- grep("[[:alpha:]]", i10tail, value = TRUE)
alpha_in_tail_bool <- grepl("[[:alpha:]].*[[:alpha:]].*", x = i10)
alpha_in_tail <- i10[alpha_in_tail_bool]
unique(gsub("[[:digit:]]", replacement = "", x = alpha_in_tail))

# verify, e.g. J in middle?
grep("[[:alpha::]].*J.*", i10)

# find unique characters at each position from 4 to 7
for (i in 1:7)
  message(i)
  substring(alpha_in_tail, i, i) %>% unique %>% sort %>% message
}
