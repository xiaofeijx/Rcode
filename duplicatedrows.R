#查找重复行
#A sample data frame:
  df <- read.table(header=T, text='
                   label value
                   A     4
                   B     3
                   C     6
                   B     3
                   B     1
                   A     2
                   A     4
                   A     4
                   ')


# Is each row a repeat?
duplicated(df)
# FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE

# Show the repeat entries
df[duplicated(df),]
# label value
#     B     3
#     A     4
#     A     4

# Show unique repeat entries 
nn <- unique(df[duplicated(df),])
# label value
#     B     3
#     A     4
  

nn$dup <- 1
  
total <- merge(df,nn,all.x=T)

total[!is.na(total$dup),]

library(dplyr)
filter(total,dup == 1)
  
  
# Original data with repeats removed. These do the same:
unique(df)
df[!duplicated(df),]
# label value
#     A     4
#     B     3
#     C     6
#     B     1
#     A     2
cname <- read.table("cname.txt",header =F,stringsAsFactors = F)
  cnamelist <- strsplit(cname$V1,split="")
  str(cname)
  cnamedf <- as.data.frame(cnamelist)


  
  
  