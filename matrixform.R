#linear model in matrix form

df <- data.frame(selfscore=c(1,9,1,5,6,8,2,4,2,8,7,7),
                 classrank=c(3,8,2,8,5,9,4,5,2,4,2,6))
df$incept <- rep(1,12)

#假设没有截距
df.beta <- solve(t(df$selfscore) %*% df$selfscore) %*% t(df$selfscore) %*% df$classrank
df.fitted <- df$selfscore %*% df.beta
df.res <- df$classrank - df.fitted
mean(df.res)
sum(df.res^2)

#假设有截距
pre <- as.matrix(df[,-2])
df.beta <- solve(t(pre) %*% pre) %*% t(pre) %*% df$classrank
df.fitted <- pre %*% df.beta
df.res <- df$classrank - df.fitted
mean(df.res)
sum(df.res^2)

df.lm <- lm(classrank ~ selfscore,data=df)
df.lm
