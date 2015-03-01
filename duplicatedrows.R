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
unique(df[duplicated(df),])
# label value
#     B     3
#     A     4

# Original data with repeats removed. These do the same:
unique(df)
df[!duplicated(df),]
# label value
#     A     4
#     B     3
#     C     6
#     B     1
#     A     2