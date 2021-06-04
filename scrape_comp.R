##
## scrape_players: players in squads
## scrape_flag: national team flags
## scrape_colours: national team kits
## scrape_comp: competition teams and logos
##

library(tidyvese)
library(rvest)
library(countrycode)
library(janitor)

d <- tibble(
  year = c(1930, 1934, 1938, seq(1950, 2018, 4)),
  teams = c(13, 16, 15, 13, rep(16, 7), rep(24, 4), rep(32, 6)),
  url = paste0("https://en.wikipedia.org/wiki/", year,"_FIFA_World_Cup")
) 

get_logo <- function(u){
  # u = d$url[i]; u
  h <- read_html(u)
  h %>%
    html_nodes(".infobox-image img") %>%
    html_attr("src")
}

get_teams <- function(u){
  # u = d$url[i]; u
  h <- read_html(u)

  x <- 
    tibble(
    team_fa = h %>%
      html_nodes(".wikitable span a") %>%
      html_attr("title"),
    team = h %>%
      html_nodes(".wikitable span a") %>%
      html_text(),
    url_team = h %>%
      html_nodes(".wikitable span a") %>%
      html_attr("href")
  ) %>%
    distinct()
  return(x)
}

d <- d %>%
  mutate(url_comp_logo = map(.x = url, .f = ~get_logo(u = .x)),
         team = map(.x = url, .f = ~get_teams(u = .x)))

# for(i in 1:nrow(d))
#   print(get_teams(u = d$url[i]))


cm <- c("CSSR" = "CSK",
        "Czechoslovakia" = "CSK",
        "England" = "GB-ENG",
        "Northern Ireland" = "GB-NIR",
        "Scotland" = "GB-SCT",
        "Wales" = "GB-WLS",
        "Serbia and Montenegro" = "SCG",
        "FR Yugoslavia" = "SCG",
        "Yugoslavia" = "YUG",
        "Kingdom of Yugoslavia" = "YUG",
        "Federal Republic of Yugoslavia" = "YUG",
        "USSR" = "SUN",
        "Soviet Union" = "SUN",
        "East Germany" = "DDR",
        "Dutch East Indies" = "DEI")

d0 <- d %>%
  unnest(url_comp_logo) %>%
  unnest(team) %>%
  clean_names() %>%
  filter(str_detect(string = team_fa, pattern = "team")) %>%
  distinct() %>%
  filter(!(str_detect(string = team, pattern = "India") & year == 1950),
         !(str_detect(string = team, pattern = "France") & year == 1950),
         !(str_detect(string = team, pattern = "Spain") & year == 1974)) %>%
  rename(nat_team = team) %>%
  mutate(nat_team = ifelse(nat_team == "", 
                           str_remove(team_fa, pattern = " national football team"),
                           nat_team),
         nat_team_alpha3 = countrycode(
           sourcevar = nat_team, origin = "country.name",
           destination = "iso3c", custom_match = cm)
         )


# d0 <- read_csv("./data/wiki_comp.csv")
write_excel_csv(d0,  "./data/wiki_comp.csv")