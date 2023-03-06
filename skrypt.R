library(ggplot2)
library(readxl)
library(cluster)
library(factoextra)
library(gridExtra)

# wczytanie bazy danych
woda <- read_excel("woda.xlsx")

# wypełnienie pustych miejsc przez 0
woda[is.na(woda)] <- 0

###Grupowanie k-najbliższych sąsiadów

# podział na 4 grupy wybranie zmiennych numerycznych

model1=kmeans(woda[8:16],4, nstart = 5)
plot1<- fviz_cluster(model1,woda[8:16]) + ggtitle("k = 4")

# podział na 3 grupy wybranie zmiennych numerycznych
model2=kmeans(woda[8:16],3, nstart = 5)
plot2<- fviz_cluster(model2,woda[8:16]) + ggtitle("k = 3")

# wizualizacja dwóch modelów
grid.arrange(plot1, plot2, nrow = 2)

# inna wizualizacja modelu nr 2
clusplot(woda[8:16],model2$cluster)

#dodanie wyniku do ramki głównej
woda$model <- model2$cluster

# sumowanie wszystkich minerałów
woda$suma <- rowSums(woda[8:16])

## Algorytm podzielił zbiór wód na 3 grupy, które najbardziej oddają charakterystykę produktu: 
# 1) cechująca się wysoką liczbą minerałów powyżej 1000 mg/1l. Na tę grupę składają się wody lekkogazowane.
# 2) cechująca się od 500 do 1000 mg/1l, Na tę grupę składają się w większości wody mineralne niegazowane.
# 3) cechująca się niższą niż 500 mg/1l. Na tę grupę składają się wody źródlane.


### Regresja, Szacowanie

# sprawdzenie czy zachodzi korelacja pomiędzy ceną, a liczbą minerałów
cor(woda$cena, woda$suma)

# czy dane posiadają rozkład normalny
hist(woda$cena)

# po selekcji zmiennych wybrano 5, które są najistotniejsze z perspektywy ceny
lm <- lm(cena ~Wapń+Wodorowęglany + Magnez+ Potas+ Chlorki, data = woda)

## można również użyć regresji krokowej, po kolei usuwającej zmienne nie istotne
#step(lm, direction="backward")

#podsumowanie ostatecznego modelu
summary(lm)

# szacunkowa cena dla każdej wody biorąc pod uwagę składniki mineralne
woda$cena2 <-lm$fitted.values
woda$reszta <-lm$residuals

# wizualizacja pokazująca rozpiętość
ggplot(woda, aes(y=cena, x=cena2, label=nazwa))+
  geom_point()+geom_smooth(method="lm", col="black")+ geom_label(vjust = 0, hjust = 0)

# Kluczową rolę w cenie odgrywają Wapń, Wodorowęglany, Magnez, Potas, Chlorki
# Najbardziej optymalną wodą pod względem ceny jest Cisowianka Magnesia i Staropolanka, z kolei najbardziej 
# nie doszacowaną wodą jest Święcicki, Kinga Pienińska, a przezpłacamy 
# w przypadku Nałęczowianki, Kropli Beskidu i Ustronianki

# Poniżej przykładowa predykcja wymyślonej wody.
a<-c(23,45,10,12,1,0.4,0.6,700,5)
wod<-as.data.frame(t(a))
colnames(wod) = names(woda[7:15])
predict(lm, newdata=wod, interval = "prediction")

## wersja ze normalizowanymi danymi

normalization <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
woda_norm <- lapply(c(woda[8:16],woda[18]), normalization)

lm_norm <- lm(cena ~., data = woda_norm)

summary(lm_norm)

step(lm_norm, direction="backward")

lm_norm2 <- lm(cena ~Wapń+Wodorowęglany + Magnez+ Potas+ Chlorki, data = woda_norm)
summary(lm_norm2)
summary(lm)