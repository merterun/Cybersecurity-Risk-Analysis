df1209 <- read_csv("2022-12-09-enriched.csv")
df0704 <- read_csv("2022-07-04-enriched.csv")
df0627 <- read_csv("2022-06-27-enriched.csv")
df0609 <- read_csv("2022-06-09-enriched.csv")
df0608 <- read_csv("2022-06-08-enriched.csv")

all_df <- rbind(df0608, df0609, df0627, df0704, df1209)

head(all_df)
dim(all_df)
str(all_df)
summary(all_df)

sapply(all_df, function(x) sum(is.na(x)))

all_df <- subset(all_df, select = -c(notes))

sapply(all_df, function(x) sum(is.na(x)))

str(all_df)


write.csv(all_df, file="all_enriched.csv")
