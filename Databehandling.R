
## indlæse databehandlingspakker
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)

## importere den samlede dataliste 
mergedlists <- read_csv2('Data/mergedlists.csv')

## tjek af data
head(mergedlists)
view(mergedlists)
## tjek ok

## gøre alle forekomster til små bogstaver
mergedlists$lemma <- tolower(mergedlists$lemma)
view(mergedlists)

## rense listen for doppelte/tredoppelte forekomster
mergedlists_cleaned <- mergedlists %>%
  distinct(lemma, .keep_all = TRUE)
view(mergedlists_cleaned)
## tjek ok

## eksport af mergedlists_cleaned
write_csv2(mergedlists_cleaned, file = "Data/Data_output/mergedlists_cleaned.csv")

## import af den manuelt annoterede fil
mergedlists_annotated <- read_csv2("Data/mergedlists_annotated.csv")

## tjek af data
view(mergedlists_annotated)
## tjek ok

# fjernelse af falske positiver
mergedlists_nofp <- mergedlists_annotated %>% 
  filter(!falsk_positiv %in% 1)
view(mergedlists_nofp)
## tjek ok

## fjerne søjlen "falsk_positiv"
mergedlists_nofpcolumn <- mergedlists_nofp %>% 
  select(-falsk_positiv)
view(mergedlists_nofpcolumn)

## eksport af mergedlists_nofpcolumn
write_csv2(mergedlists_nofpcolumn, file = "Data/Data_output/mergedlists_nofpcolumn.csv")

## lad os lave et plot over fordelingen af ordklasser
wordclasses <- mergedlists_nofpcolumn %>% 
  count(ordklasse)
view(wordclasses)
## hovsa - der var et par slåfejl i datasættet - f.eks. var et adjektiv fejlagtigt angivet som "ADJ" i stedet for "A". 
## Derudover var et substantiv et par gange fejlagtigt tagget med "N" i stedet for "S", samt en interjektion "I" angivet med "U".
## finde ordet med den manglende ordklasseangivelse
mergedlists_nofpcolumn %>%
  filter(is.na(ordklasse) | trimws(ordklasse) == "")

## dette rettes altsammen til i excelarket, og gemmes som "table_all"-csv i /Data.

table_all <- read_csv2("Data/table_all.csv")
view(table_all)

## vi tjekker igen ordklasserne
wordclasses <- table_all %>% 
  count(ordklasse)
view(wordclasses)
## det passer nu

## diagram over ordklasserne
table_all %>% 
  ggplot(aes(x = ordklasse)) +
  labs(title = "Ordklassernes fordeling",
       x = "Ordklasse",
       y = "Antal ord") +
  geom_bar()

## procenter
percentages <- wordclasses %>% 
  mutate(percent = round(100 * n / sum(n), 2))
view(percentages)
write_csv2(percentages, file = "Data/Data_output/percentages.csv")

## danne tabel med de respektive, relevante ordklasser
nouns <- table_all %>% 
  filter(ordklasse == "S")
view(nouns)
write.csv2(nouns, file = "Data/Data_output/nouns.csv")

verbs <- table_all %>% 
  filter(ordklasse == "V")
view(verbs)
write_csv2(verbs, file = "Data/Data_output/verbs.csv")

adjectives <- table_all %>% 
  filter(ordklasse == "A")
view(adjectives)
write_csv2(adjectives, file = "Data/Data_output/adjectives.csv")

prefixes <- table_all %>% 
  filter(ordklasse == "PRÆ")
view(prefixes)
write_csv2(prefixes, file = "Data/Data_output/prefixes.csv")

## lad os kigge lidt på præfixverberne
## vi starter med at lave en liste med præfixerne
prefixlist <- c("be", "af", "er", "over", "under", "an", "rust", "for", "til", "ud", "und", "veder", "fore", "fyr", "ge", "isen", "middel", "platten", "rive", "rosen", "sam", "sennes", "skand", "spege", "stif", "straten", "sæms", "sølver", "tamper", "tanke", "und", "van", "vankel", "vrag", "vædde", "vær", "vån", "yder")

## tabel med præfixverber, idet verbtabellen i en funktion klaskes sammen med vores præfixliste
verbs_with_prefix <- verbs %>% 
  mutate(prefix = sapply(lemma, function(w) {
    matched <- prefixlist[str_starts(w, prefixlist)]
    if (length(matched) > 0) {
      matched[which.max(nchar(matched))]
    } else {
      NA
    }
  }))

## diagram over fordelingen af præfixverber
verbs_with_prefix %>% 
  filter(!is.na(prefix)) %>% 
  count(prefix, sort = TRUE) %>% 
  ggplot(aes(x = reorder(prefix, -n), y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Fordeling af præfixverber",
    x = "Præfix",
    y = "Antal"
  ) +
  theme_minimal()

## procenter internt i præfixverberne
percentages_verbs_with_prefix <- verbs_with_prefix %>% 
  filter(!is.na(prefix)) %>% 
  count(prefix) %>% 
  mutate(percent = round(100 * n / sum(n), 2))
view(percentages_verbs_with_prefix)
write_csv2(percentages_verbs_with_prefix, file = "Data/Data_output/percentages_verbs_with_prefix.csv")


## procentforhold præfixverber kontra ikkepræfixverber
percentages_verbs_total <- verbs_with_prefix %>% 
  count(prefix) %>% 
  mutate(percent = round(100 * n / sum(n), 2))
view(percentages_verbs_total)
write_csv2(percentages_verbs_total, file = "Data/Data_output/percentages_verbs_total.csv")

## lad os kigge på substantiverne indgående nu
nouns_tagged <- read.csv2("Data/nouns_tagged.csv")
view(nouns_tagged)

tagset_nouns <- nouns_tagged %>% 
  separate_rows(tags, sep = ";")
view(tagset_nouns)

tag_counts <- tagset_nouns %>% 
  count(tags) %>% 
  arrange(desc(n))
view(tag_counts)

tag_percentages <- tag_counts %>% 
  mutate(Percentage = n / nrow(tagset_nouns) * 100) %>% 
  arrange(desc(Percentage))
write_csv2(tag_percentages, file = "Data/Data_output/tag_percentages.csv")

view(tag_percentages)
## hm, hvad har jeg misset
nouns_tagged %>%
  filter(is.na(tags) | trimws(tags) == "")

filter(nouns_tagged, str_detect(tags, "SOD"))
## gotcha!

## diagram over tags
tag_counts %>% 
  ggplot(aes(x = reorder(tags, -n), y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Fordeling af semantiske områder",
    x = "Semantiske område",
    y = "Antal"
  ) +
  theme_minimal()

## udtræk af de respektive semantiske områder
håndværk <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "HÅN")))
view(håndværk)
write_csv2(håndværk, file = "Data/Data_output/semantic_areas/håndværk.csv")

samfund <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "SAM")))
view(samfund)
write.csv2(samfund, file = "Data/Data_output/semantic_areas/samfund.csv")

handling <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "HAN")))
view(handling)
write.csv2(handling, file = "Data/Data_output/semantic_areas/handling.csv")

fauna <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "FAU")))
view(dyreliv)
write.csv2(fauna, file = "Data/Data_output/semantic_areas/fauna.csv")

søfart <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "SØF")))
view(søfart)
write.csv2(søfart, file = "Data/Data_output/semantic_areas/søfart.csv")

flora <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "FLO")))
view(flora)
write.csv2(flora, file = "Data/Data_output/semantic_areas/flora.csv")

militært <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "MIL")))
view(militært)
write.csv2(militært, file = "Data/Data_output/semantic_areas/militært.csv")

husholdning <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "HUS")))
view(husholdning)
write.csv2(husholdning, file = "Data/Data_output/semantic_areas/husholdning.csv")

verden <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "VER")))
view(verden)
write.csv2(verden, file = "Data/Data_output/semantic_areas/verden.csv")

gastronomi <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "GAS")))
view(gastronomi)
write.csv2(gastronomi, file = "Data/Data_output/semantic_areas/gastronomi.csv")

kropsligt <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "KRO")))
view(kropsligt)
write.csv2(kropsligt, file = "Data/Data_output/semantic_areas/kropsligt.csv")

følelsesliv <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "FØL")))
view(følelsesliv)
write.csv2(følelsesliv, file = "Data/Data_output/semantic_areas/følelsesliv.csv")

besiddelse <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "BES")))
view(besiddelse)
write.csv2(besiddelse, file = "Data/Data_output/semantic_areas/besiddelse.csv")

beklædning <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "BEK")))
view(beklædning)
write.csv2(beklædning, file = "Data/Data_output/semantic_areas/beklædning.csv")

juridisk <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "JUR")))
view(juridisk)
write.csv2(juridisk, file = "Data/Data_output/semantic_areas/juridisk.csv")

religiøsitet <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "REL")))
view(religiøsitet)
write.csv2(religiøsitet, file = "Data/Data_output/semantic_areas/religiøsitet.csv")

kognitivitet <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "KOG")))
view(kognitivitet)
write.csv2(kognitivitet, file = "Data/Data_output/semantic_areas/kognitivitet.csv")

kvantitet <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "KVA")))
view(kvantitet)
write.csv2(kvantitet, file = "Data/Data_output/semantic_areas/kvantitet.csv")

bevægelse <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "BEV")))
view(bevægelse)
write.csv2(bevægelse, file = "Data/Data_output/semantic_areas/bevægelse.csv")

rumlighed <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "RUM")))
view(rumlighed)
write.csv2(rumlighed, file = "Data/Data_output/semantic_areas/rumlighed.csv")

musik_kunst <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "MUS")))
view(musik_kunst)
write.csv2(musik_kunst, file = "Data/Data_output/semantic_areas/musik_kunst.csv")

sproglighed <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "SPR")))
view(sproglighed)
write.csv2(sproglighed, file = "Data/Data_output/semantic_areas/sproglighed.csv")

slægtskab <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "SLÆ")))
view(slægtskab)
write.csv2(slægtskab, file = "Data/Data_output/semantic_areas/slægtskab.csv")

tidslighed <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "TID")))
view(tidslighed)
write.csv2(tidslighed, file = "Data/Data_output/semantic_areas/tidslighed.csv")

sansning <- nouns_tagged %>% 
  filter(if_any(tags, ~ str_detect(.x, "SAN")))
view(sansning)
write.csv2(sansning, file = "Data/Data_output/semantic_areas/sansning.csv")


## fjernelse af suffiks- og præfiks-"ordklasser"
nopseudowordclasses <- table_all %>% 
  filter(ordklasse != "PRÆ") %>% 
  filter(ordklasse != "SUF") 

view(nopseudowordclasses)

count_nopseudowordclasses <- nopseudowordclasses %>% 
  count(ordklasse) %>% 
  arrange(desc(n))
view(count_nopseudowordclasses)

percentages_nopseudowordclasses <- count_nopseudowordclasses %>% 
  mutate(percent = round(100 * n / sum(n), 2))
view(percentages_nopseudowordclasses)

ggplot(count_nopseudowordclasses, aes(x = ordklasse, y = n)) +
  labs(title = "Ordklassernes fordeling",
       x = "Ordklasse",
       y = "Antal ord") +
  geom_col()
