
df = data.frame(oil = as.ts(dados$Oil.close),
                gpr = as.ts(dados$gpr.geopolitical),
                sap = as.ts(dados$SaP.close),
                bond = as.ts(dados$bond.rate))
train_df = df[1:4500,]
test_df = df[4501:length(df$oil),]

plot.ts(df)

var.1 <- VAR(train_df, 2, type = "none")
var.aic <- VAR(train_df, type = "none", lag.max = 5, ic = "AIC")

model_summary = summary(var.aic)

est_coefs_se <- coef(var.aic)
est_coefs <- rbind(est_coefs_se[[1]][, 1],
                   est_coefs_se[[2]][, 1],
                   est_coefs_se[[3]][, 1],
                   est_coefs_se[[4]][, 1])

irfunc <- irf(var.aic, impulse = c('bond', 'gpr', 'sap'), response = 'oil', n.ahead = 5, ortho = FALSE)
plot(irfunc)

pred <- predict(var.aic, n.ahead=5)
result = data.frame(pred = round(pred$fcst$oil[,1],2),
           real = round(test_df$oil[1:5],2))
plot.ts(result)

