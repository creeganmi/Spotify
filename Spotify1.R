library (readr)
urlfile="https://raw.githubusercontent.com/creeganmi/Spotify/main/top10s.csv"
data <- read_csv(url(urlfile))
head(data)
