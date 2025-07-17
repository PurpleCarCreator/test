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