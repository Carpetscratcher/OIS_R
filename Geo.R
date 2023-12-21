dataset <- read_sheet("docs.google.com/spreadsheets/d/1Ba_-KPGfN3VS7zXzO5cKYnWXQ3fb3A07kbCQU4Cvc-8/edit?resourcekey#gid=1943360347")

dataset$`Distance in kilometer (500m is 0.500):` <- as.numeric(as.character(dataset$`Distance in kilometer (500m is 0.500):`))

df <- subset(dataset, select = c("Gender", "Age (In numbers)", "I make decisions logically and systematically."
                                           , "When making decisions, I trust my intuition.", "My decision was made in a logical and systematic way.", 
                                           "While making my decision, I trusted my intuition.", "Time awareness", "Distance in kilometer (500m is 0.500):"))

dataset_geen <- df[df$"Time awareness" == "Geen", ]
dataset_wel <- df[df$"Time awareness" == "Wel", ]

boxplot(df$`Distance in kilometer (500m is 0.500):` ~ df$`Time awareness`, data = df, xlab = "Tijdsbesef", ylab = "Afstand (km)")

"Samenvatting van beide groepen"
summary(dataset_geen)
summary(dataset_wel)

"Standaard deviatie van totale dataset"
sd(dataset$`Distance in kilometer (500m is 0.500):`)

"Gemiddelde van de vragenlijstanwtoorden"
summary(df$`I make decisions logically and systematically.`)
summary(df$`When making decisions, I trust my intuition.`)
summary(df$`My decision was made in a logical and systematic way.`)
summary(df$`While making my decision, I trusted my intuition.`)

"Maken van balans rationliteit en intuïtie"
diff_rational <- df$`My decision was made in a logical and systematic way.` - df$`I make decisions logically and systematically.`
diff_intuitive <- df$`While making my decision, I trusted my intuition.` - df$`When making decisions, I trust my intuition.`
total_diff <- diff_rational - diff_intuitive
summary(diff_intuitive)

"Correlatie tussen afstand en balans rationaliteit en intuïtie"
corr.test(df$`Distance in kilometer (500m is 0.500):`, total_diff)

"Homogeneity test"
bartlett.test(df$`Distance in kilometer (500m is 0.500):` ~ df$`Time awareness`, data = df)
bartlett.test(total_diff ~ df$`Time awareness`, data = df)

"Normality test"
shapiro.test(df$`Distance in kilometer (500m is 0.500):`)
shapiro.test(total_diff)

"Manova test"
dependent <- cbind(df$`Distance in kilometer (500m is 0.500):`, total_diff)
independent <- df$`Time awareness`
man <- manova(dependent ~ independent, data = df)
summary.aov(man)

"Correlatie tussen tijd over en afstand"
dataset_geen2 <- dataset[dataset$"Time awareness" == "Geen", ]
dataset_wel2 <- dataset[dataset$"Time awareness" == "Wel", ]
corr.test(dataset_wel2$`Seconds left`, dataset_wel2$`Distance in kilometer (500m is 0.500):`)
corr.test(dataset_geen2$`Distance in kilometer (500m is 0.500):`, dataset_geen2$`Seconds left`)

"Correlatie tussen tijd over en de balans tussen rationaliteit en intuïtie"
diw <- dataset_wel2$`While making my decision, I trusted my intuition.` - dataset_wel2$`When making decisions, I trust my intuition.`
dig <- dataset_geen2$`While making my decision, I trusted my intuition.` - dataset_geen2$`When making decisions, I trust my intuition.`
drw <- dataset_wel2$`My decision was made in a logical and systematic way.` - dataset_wel2$`I make decisions logically and systematically.`
drg <- dataset_geen2$`My decision was made in a logical and systematic way.` - dataset_geen2$`I make decisions logically and systematically.`
totw <- drw - diw
totg <- drg - dig
corr.test(dataset_wel2$`Seconds left`, totw)
corr.test(dataset_geen2$`Seconds left`, totg)

