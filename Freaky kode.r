#Variabler
5 + 3
10 / 2
2^4

x <- 10
y <- 20
z <- x + y
print(z)

#Vektorer
tall <- c(2, 4, 6, 8, 10)
frukt <- c("eple", "banan", "appelsin")
rekke <- 1:10
partall <- seq(2, 20, by = 2)
doble_tall <- tall * 2

#Datarammer
df <- data.frame(
  Navn = c("Hassan", "Ridwan", "Mohammed"),
  Alder = c(30, 22, 17),
  Høyde = c(184, 185, 195)
)

head(df)
summary(df)
str(df)

df$Navn
df[1, ]

#Plottefunksjoner
plot(x = df$Alder, y = df$Høyde, 
     main = "Alder vs Høyde", 
     xlab = "Alder", 
     ylab = "Høyde (cm)")

hist(df$Alder, col = "skyblue", main = "Aldersfordeling")


install.packages("ggplot2")
library(ggplot2)

ggplot(df, aes(x = Navn, y = Alder)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Alder etter Navn")

#Funksjoner
beregn_areal <- function(radius) {
  pi * radius^2
}

beregn_areal(5)


temperatur <- 28
if (temperatur > 30) {
  print("PLS NO MORE")
} else {
  print("Relaxation")
}

for (i in 1:5) {
  print(paste("Iterasjon:", i))
}

#Lesing av Data
data <- read.csv("Fraud.csv")
data(mtcars)
head(mtcars)





#Plottefunksjoner for Svindeldatasett

library(ggplot2)
library(dplyr)

#eksempeldata
set.seed(123)
svindel_data <- data.frame(
  TransaksjonsID = 1:500,
  Beløp = c(rgamma(450, shape = 2, scale = 50), 
            rgamma(50, shape = 10, scale = 500)),
  Dato = seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 500),
  Land = sample(c("Norge", "Sverige", "Danmark", "Annet"), 500, replace = TRUE),
  Svindel = c(rep(0, 450), rep(1, 50))
)

#Histogram
ggplot(svindel_data, aes(x = Beløp, fill = factor(Svindel))) +
  geom_histogram(bins = 30, alpha = 0.7) +
  scale_fill_manual(values = c("blue", "red"), 
                    labels = c("Normal", "Svindel")) +
  labs(title = "Fordeling av Transaksjonsbeløp",
       x = "Beløp (NOK)",
       y = "Antall Transaksjoner",
       fill = "Transaksjonstype") +
  theme_minimal()

#Boxplot
ggplot(svindel_data, aes(x = Land, y = Beløp, fill = factor(Svindel))) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "red"), 
                    labels = c("Normal", "Svindel")) +
  labs(title = "Transaksjonsbeløp etter Land",
       x = "Land",
       y = "Beløp (NOK)",
       fill = "Transaksjonstype") +
  theme_minimal()

#Tidslinje for svindelhendelser
svindel_data %>%
  filter(Svindel == 1) %>%
  group_by(Dato) %>%
  summarise(Antall = n()) %>%
  ggplot(aes(x = Dato, y = Antall)) +
  geom_line(color = "red") +
  geom_point(color = "darkred") +
  labs(title = "Svindelhendelser over Tid",
       x = "Dato",
       y = "Antall Svindelforsøk") +
  theme_minimal()





set.seed(123)
coffee_data <- data.frame(
  shop = rep(c("Java", "Brew", "Aroma"), each = 30),
  day = rep(1:30, 3),
  temp = round(rnorm(90, mean = 22, sd = 5), 1),
  sales = c(rpois(30, 120), rpois(30, 95), rpois(30, 110))
)

library(ggplot2)
ggplot(coffee_data, aes(x = temp, y = sales, color = shop)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("#6F4E37", "#C0A080", "#3E2723")) +
  theme_minimal()

library(dplyr)
coffee_data %>%
  group_by(shop) %>%
  summarise(
    avg_sales = mean(sales),
    max_temp = max(temp),
    min_temp = min(temp)
  ) %>%
  arrange(-avg_sales) %>%
  mutate(profit = avg_sales * 4.5 - 300)

latte_art <- function(attempts) {
  results <- sample(c("❤️", "🌷", "☕", "❌"), attempts, 
                   prob = c(0.2, 0.3, 0.4, 0.1), replace = TRUE)
  table(results)
}

latte_art(100)

coffee_palette <- colorRampPalette(c("#3E2723", "#6F4E37", "#C0A080"))
barplot(rep(1,10), col = coffee_palette(10), 
        border = NA, axes = FALSE, space = 0)
