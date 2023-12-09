install.packages("dplyr")
install.packages("ggplot2")
install.packages("patchwork")

library(patchwork)
library(dplyr)
library(ggplot2)

all_gpus <- read.csv("ALL_GPUs.csv")
head(all_gpus,n=10)

all_gpus_news <- all_gpus[, c("Max_Power","Boost_Clock", "Core_Speed", "Memory", "Memory_Bandwidth", "Memory_Bus", "Memory_Speed", "Release_Price", "Texture_Rate")]
head(all_gpus_news, n=10)

all_gpus_news <- all_gpus_news %>%
  mutate(
    Max_Power = as.numeric(gsub("[^0-9.]", "", Max_Power)),
    Boost_Clock = as.numeric(gsub("[^0-9.]", "", Boost_Clock)),
    Core_Speed = as.numeric(gsub("[^0-9.]", "", Core_Speed)),
    Memory = as.numeric(gsub("[^0-9.]", "", Memory)),
    Memory_Bandwidth = as.numeric(gsub("[^0-9.]", "", Memory_Bandwidth)),
    Memory_Bus = as.numeric(gsub("[^0-9.]", "", Memory_Bus)),
    Memory_Speed = as.numeric(gsub("[^0-9.]", "", Memory_Speed)),
    Release_Price = as.numeric(gsub("[^0-9.]", "", Release_Price)),
    Texture_Rate = as.numeric(gsub("[^0-9.]", "", Texture_Rate))
  )
head(all_gpus_news,n = 10)

missing_counts <- colSums(is.na(all_gpus_news))
missing_counts

all_gpus_news <- all_gpus_news[, -2]

mean_values <- sapply(all_gpus_news[, -7], function(x) mean(x, na.rm = TRUE))
mean_values


all_gpus_news <- all_gpus_news %>%
  mutate(
    Max_Power = ifelse(is.na(Max_Power), mean_values["Max_Power"], Max_Power),
    Core_Speed = ifelse(is.na(Core_Speed), mean_values["Core_Speed"], Core_Speed),
    Memory = ifelse(is.na(Memory), mean_values["Memory"], Memory),
    Memory_Bandwidth = ifelse(is.na(Memory_Bandwidth), mean_values["Memory_Bandwidth"], Memory_Bandwidth),
    Memory_Bus = ifelse(is.na(Memory_Bus), mean_values["Memory_Bus"], Memory_Bus),
    Memory_Speed = ifelse(is.na(Memory_Speed), mean_values["Memory_Speed"], Memory_Speed),
    Texture_Rate = ifelse(is.na(Texture_Rate), mean_values["Texture_Rate"], Texture_Rate)
  )

head(all_gpus_news, n = 10)

all_gpus_news_clean <- na.omit(all_gpus_news)
head(all_gpus_news_clean, n = 10)

rownames(all_gpus_news_clean) <- NULL
head(all_gpus_news_clean, n = 10)

ggplot(all_gpus_news_clean, aes(x = Max_Power, y = Release_Price)) +
  geom_point() +
  labs(x = "Max_Power", y = "Release_Price") +
  ggtitle("Max_Power vs. Release_Price")
ggplot(all_gpus_news_clean, aes(x = Core_Speed, y = Release_Price)) +
  geom_point() +
  labs(x = "Core_Speed", y = "Release_Price") +
  ggtitle("Core_Speed vs. Release_Price")

ggplot(all_gpus_news_clean, aes(x = Memory, y = Release_Price)) +
  geom_point() +
  labs(x = "Memory", y = "Release_Price") +
  ggtitle("Memory vs. Release_Price")

ggplot(all_gpus_news_clean, aes(x = Memory_Bandwidth, y = Release_Price)) +
  geom_point() +
  labs(x = "Memory_Bandwidth", y = "Release_Price") +
  ggtitle("Memory_Bandwidth vs. Release_Price")

ggplot(all_gpus_news_clean, aes(x = Memory_Bus, y = Release_Price)) +
  geom_point() +
  labs(x = "Memory_Bus", y = "Release_Price") +
  ggtitle("Memory_Bus vs. Release_Price")

ggplot(all_gpus_news_clean, aes(x = Memory_Speed, y = Release_Price)) +
  geom_point() +
  labs(x = "Memory_Speed", y = "Release_Price") +
  ggtitle("Memory_Speed vs. Release_Price")

ggplot(all_gpus_news_clean, aes(x = Texture_Rate, y = Release_Price)) +
  geom_point() +
  labs(x = "Texture_Rate", y = "Release_Price") +
  ggtitle("Texture_Rate vs. Release_Price")

his0 <- ggplot(all_gpus_news_clean, aes(x = Max_Power)) +
  geom_histogram(binwidth = 100,color = "white") +
  labs(x = "value", y = "count") +
  ggtitle("Max_Power")

his1 <- ggplot(all_gpus_news_clean, aes(x = Core_Speed)) +
  geom_histogram(binwidth = 100,color = "white") +
  labs(x = "value", y = "count") +
  ggtitle("Core_Speed")

his2 <- ggplot(all_gpus_news_clean, aes(x = Memory )) +
  geom_histogram(binwidth = 1000, color = "white") +
  labs(x = "value", y = "count") +
  ggtitle("Memory")

his3 <- ggplot(all_gpus_news_clean, aes(x = Memory_Bandwidth )) +
  geom_histogram(binwidth = 50, color = "white") +
  labs(x = "value", y = "count") +
  ggtitle("Memory_Bandwidth")


his4 <- ggplot(all_gpus_news_clean, aes(x = Memory_Speed )) +
  geom_histogram(binwidth = 100, color = "white") +
  labs(x = "value", y = "count") +
  ggtitle("Memory_Speed ")

his5 <- ggplot(all_gpus_news_clean, aes(x = Memory_Bus )) +
  geom_histogram(binwidth = 400,color = "white") +
  labs(x = "value", y = "count") +
  ggtitle("Memory_Bus")

his6 <- ggplot(all_gpus_news_clean, aes(x = Release_Price )) +
  geom_histogram(binwidth = 1000,color = "white") +
  labs(x = "value", y = "count") +
  ggtitle("Release_Price")

his7 <- ggplot(all_gpus_news_clean, aes(x = Texture_Rate )) +
  geom_histogram(binwidth = 50,color = "white") +
  labs(x = "value", y = "count") +
  ggtitle("Texture_Rate")
his0 + his1 + his2 + his3 + his4 + his5 + his6 + his7 

boxplot(all_gpus_news_clean$Max_Power, horizontal=TRUE, main="Max_Power")
boxplot(all_gpus_news_clean$Core_Speed, horizontal=TRUE, main="Core_Speed")
boxplot(all_gpus_news_clean$Memory, horizontal=TRUE, main="Memory")
boxplot(all_gpus_news_clean$Memory_Bandwidth, horizontal=TRUE, main="Memory_Bandwidth")
boxplot(all_gpus_news_clean$Memory_Bus, horizontal=TRUE, main="Memory_Bus")
boxplot(all_gpus_news_clean$Memory_Speed, horizontal=TRUE, main="Memory_Speed")
boxplot(all_gpus_news_clean$Release_Price, horizontal=TRUE, main="Release_Price")
boxplot(all_gpus_news_clean$Texture_Rate, horizontal=TRUE, main="Texture_Rate")

find_boxplot_boundaries <- function(col, whisker_coeff = 1.5) {
  Q1 <- quantile(col, 0.25)
  Q3 <- quantile(col, 0.75)
  IQR <- Q3 - Q1
  lower <- Q1 - whisker_coeff * IQR
  upper <- Q3 + whisker_coeff * IQR
  return(list(lower = lower, upper = upper))
}

BoxplotOutlierClipper <- function(whisker_coeff = 1.5, X) {
  boundaries <- find_boxplot_boundaries(X, whisker_coeff)
  clipped_X <- pmax(pmin(X, boundaries$upper), boundaries$lower)
  return(clipped_X)
}

# Max_Power
all_gpus_news_clean$Max_Power <- BoxplotOutlierClipper(whisker_coeff = 1.5, X = all_gpus_news_clean$Max_Power)
par(mfrow=c(1,2))
hist(all_gpus_news_clean$Max_Power , breaks = 50, main = "Histogram", xlab = "Max_Power", ylab = "Frequency")
boxplot(all_gpus_news_clean$Max_Power, horizontal = TRUE, main = "Boxplot")

# Core_Speed
all_gpus_news_clean$Core_Speed <- BoxplotOutlierClipper(whisker_coeff = 1.5, X = all_gpus_news_clean$Core_Speed)
par(mfrow=c(1,2))
hist(all_gpus_news_clean$Core_Speed , breaks = 50, main = "Histogram", xlab = "Core_Speed", ylab = "Frequency")
boxplot(all_gpus_news_clean$Core_Speed, horizontal = TRUE, main = "Boxplot")

# memory
all_gpus_news_clean$Memory <- BoxplotOutlierClipper(whisker_coeff = 1.5, X = all_gpus_news_clean$Memory)
par(mfrow=c(1,2))
hist(all_gpus_news_clean$Memory , breaks = 50, main = "Histogram", xlab = "Memory", ylab = "Frequency")
boxplot(all_gpus_news_clean$Memory, horizontal = TRUE, main = "Boxplot")

# Memory_Bandwidth
all_gpus_news_clean$Memory_Bandwidth <- BoxplotOutlierClipper(whisker_coeff = 1.5, X = all_gpus_news_clean$Memory_Bandwidth)
par(mfrow=c(1,2))
hist(all_gpus_news_clean$Memory_Bandwidth , breaks = 50, main = "Histogram", xlab = "Memory_Bandwidth", ylab = "Frequency")
boxplot(all_gpus_news_clean$Memory_Bandwidth, horizontal = TRUE, main = "Boxplot")

# Memory_Bus
all_gpus_news_clean$Memory_Bus <- BoxplotOutlierClipper(whisker_coeff = 1.5, X = all_gpus_news_clean$Memory_Bus)
par(mfrow=c(1,2))
hist(all_gpus_news_clean$Memory_Bus , breaks = 50, main = "Histogram", xlab = "Memory_Bus", ylab = "Frequency")
boxplot(all_gpus_news_clean$Memory_Bus, horizontal = TRUE, main = "Boxplot")

#Memory_Speed
all_gpus_news_clean$Memory_Speed <- BoxplotOutlierClipper(whisker_coeff = 1.5, X = all_gpus_news_clean$Memory_Speed)
par(mfrow=c(1,2))
hist(all_gpus_news_clean$Memory_Speed , breaks = 50, main = "Histogram", xlab = "Memory_Speed", ylab = "Frequency")
boxplot(all_gpus_news_clean$Memory_Speed, horizontal = TRUE, main = "Boxplot")

#Release_Price
all_gpus_news_clean$Release_Price <- BoxplotOutlierClipper(whisker_coeff = 1.5, X = all_gpus_news_clean$Release_Price)
par(mfrow=c(1,2))
hist(all_gpus_news_clean$Release_Price , breaks = 50, main = "Histogram", xlab = "Release_Price", ylab = "Frequency")
boxplot(all_gpus_news_clean$Release_Price, horizontal = TRUE, main = "Boxplot")

#Texture_Rate
all_gpus_news_clean$Texture_Rate <- BoxplotOutlierClipper(whisker_coeff = 1.5, X = all_gpus_news_clean$Texture_Rate)
par(mfrow=c(1,2))
hist(all_gpus_news_clean$Texture_Rate , breaks = 50, main = "Histogram", xlab = "Texture_Rate", ylab = "Frequency")
boxplot(all_gpus_news_clean$Texture_Rate, horizontal = TRUE, main = "Boxplot")

thongkemota <- data.frame(
  cbind(
    apply(all_gpus_news_clean, MARGIN=2, min),
    apply(all_gpus_news_clean, MARGIN=2, sd),
    apply(all_gpus_news_clean, MARGIN=2, mean),
    apply(all_gpus_news_clean, MARGIN=2, median),
    apply(all_gpus_news_clean, MARGIN=2, max)
  )
)

colnames(thongkemota) <- c("GTNN(min)", "Do lech chuan (sd)", "Trung binh (mean)", "Trung vi (median)", "GTLN (max)")
head(thongkemota)

model1 = lm(Release_Price~Max_Power+Core_Speed+Memory+Memory_Bandwidth+Memory_Bus+Memory_Speed+Texture_Rate, data = all_gpus_news_clean)
summary(model1)

model2 = lm(Release_Price~Max_Power+Core_Speed+Memory_Bandwidth+Memory_Bus+Memory_Speed+Texture_Rate, data = all_gpus_news_clean)
summary(model2)
anova(model1, model2)

plot(model2)

X <- data.frame("Max_Power" = 141, "Core_Speed" = 738, "Memory_Bandwidth" = 64, "Memory_Bus" = 256, "Memory_Speed" = 1000, "Texture_Rate" = 47)
head(X)
predictX <- predict(model2, X, interval = "confidence")
head(predictX)