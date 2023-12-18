dataset <- read_sheet("docs.google.com/spreadsheets/d/1Ba_-KPGfN3VS7zXzO5cKYnWXQ3fb3A07kbCQU4Cvc-8/edit?resourcekey#gid=1943360347")

summary(df)

dataset$`Distance in kilometer (500m is 0.500):` <- as.numeric(as.character(dataset$`Distance in kilometer (500m is 0.500):`))

df <- subset(dataset, select = c("Gender", "Age (In numbers)", "I make decisions logically and systematically."
                                           , "When making decisions, I trust my intuition.", "My decision was made in a logical and systematic way.", 
                                           "While making my decision, I trusted my intuition.", "Time awareness", "Distance in kilometer (500m is 0.500):"))

dataset_geen <- df[df$"Time awareness" == "Geen", ]
dataset_wel <- df[df$"Time awareness" == "Wel", ]

boxplot(df$`Distance in kilometer (500m is 0.500):` ~ df$`Time awareness`, data = df, xlab = "Tijdsbesef", ylab = "Afstand (km)")

sd(dataset$`Distance in kilometer (500m is 0.500):`)

list(dataset_wel$`Distance in kilometer (500m is 0.500):`)

summary(df$`I make decisions logically and systematically.`)
summary(df$`When making decisions, I trust my intuition.`)
summary(df$`My decision was made in a logical and systematic way.`)
summary(df$`While making my decision, I trusted my intuition.`)

corr.test(df$`Distance in kilometer (500m is 0.500):`, total_diff)

shapiro.test(df$`Distance in kilometer (500m is 0.500):`)
shapiro.test(total_diff)

diff_rational <- df$`My decision was made in a logical and systematic way.` - df$`I make decisions logically and systematically.`
diff_intuitive <- df$`While making my decision, I trusted my intuition.` - df$`When making decisions, I trust my intuition.`
total_diff <- diff_rational - diff_intuitive
summary(diff_intuitive)

manova_res <- manova(cbind(df$`Distance in kilometer (500m is 0.500):`, total_diff) ~ df$`Time awareness`, data = df)
manova_res1 <- kruskal.test(total_diff ~ df$`Time awareness`, data = df)
manova_res2 <- kruskal.test(df$`Distance in kilometer (500m is 0.500):` ~ df$`Time awareness`, data = df)
summary.aov(manova_res)
print(manova_res)
print(manova_res1)
print(manova_res2)

dependent <- cbind(df$`Distance in kilometer (500m is 0.500):`, total_diff)
independent <- df$`Time awareness`
man <- manova(dependent ~ independent, data = df)
summary.aov(man)

man_lda <- lda(independent ~ dependent, CV = F)
man_lda

summary_model <- summary(manova_res)
summary_model$stats
TukeyHSD(summary_model$stats)


bartlett.test(df$`Distance in kilometer (500m is 0.500):` ~ df$`Time awareness`, data = df)
bartlett.test(total_diff ~ df$`Time awareness`, data = df)

df$total_diff_g <- as.factor(total_diff)
df$`Time awareness g` <- as.factor(df$`Time awareness`)
levene_test_result <- car::leveneTest(total_diff ~ df$`Time awareness`, data = df)
print(levene_test_result)

dataset_geen2 <- dataset[dataset$"Time awareness" == "Geen", ]
dataset_wel2 <- dataset[dataset$"Time awareness" == "Wel", ]

corr.test(dataset_wel2$`Seconds left`, dataset_wel2$`Distance in kilometer (500m is 0.500):`)
corr.test(dataset_geen2$`Distance in kilometer (500m is 0.500):`, dataset_geen2$`Seconds left`)

diw <- dataset_wel2$`While making my decision, I trusted my intuition.` - dataset_wel2$`When making decisions, I trust my intuition.`
dig <- dataset_geen2$`While making my decision, I trusted my intuition.` - dataset_geen2$`When making decisions, I trust my intuition.`
drw <- dataset_wel2$`My decision was made in a logical and systematic way.` - dataset_wel2$`I make decisions logically and systematically.`
drg <- dataset_geen2$`My decision was made in a logical and systematic way.` - dataset_geen2$`I make decisions logically and systematically.`
totw <- drw - diw
totg <- drg - dig
corr.test(dataset_wel2$`Seconds left`, totw)
corr.test(dataset_geen2$`Seconds left`, totg)

