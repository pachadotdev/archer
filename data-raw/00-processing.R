# 1: load packages ----

if (!require("pacman")) install.packages("pacman")
p_load(data.table, dplyr, stringr, rvest, janitor, purrr)
p_load_gh("fkeck/subtools")

# 2: scrap Archer subtitles ----

file_all_seasons <- "data-raw/links-all-seasons.csv"

if (!file.exists(file_all_seasons)) {
  url_subtitles <- read_html("https://www.opensubtitles.org/en/ssearch/sublanguageid-eng/idmovie-54780")

  # subtitles_tables <- url_subtitles %>%
  #   html_nodes(xpath = '//*[(@id = "search_results")]') %>%
  #   html_table(fill = TRUE)

  links <- url_subtitles %>%
    html_nodes("a") %>%
    html_attr("href")

  titles <- url_subtitles %>%
    html_nodes("a") %>%
    html_attr("title")

  links_all_seasons <- tibble(link = links, title = titles) %>%
    filter(!is.na(title)) %>%
    distinct(link, .keep_all = T) %>%
    # mutate(link = str_replace_all(link, "^/en/search/", "https://www.opensubtitles.org/download/s/")) %>%
    mutate(link = str_replace_all(link, "^/en/search/", "https://www.opensubtitles.org/en/search/")) %>%
    filter(grepl("imdb", link)) %>%
    mutate(
      title = str_replace_all(title, "\"Archer\" ", ""),
      title = iconv(title, to = "ASCII//TRANSLIT", sub = ""),
      season = c(rep("S01", 10), rep("S02", 13), rep("S03", 13), rep("S04", 13), rep("S05", 13), rep("S06", 13), rep("S07", 10), rep("S08", 8), rep("S09", 8), rep("S10", 9)),
      episode = c(1:10, 1:13, 1:13, 1:13, 1:13, 1:13, 1:10, 1:8, 1:8, 1:9),
      episode = if_else(nchar(episode) == 1, paste0("E0", episode), paste0("E", episode)),
      file_name = paste(season, episode, str_replace_all(title, "[[:punct:]]", ""), sep = "_"),
      file_name = str_replace_all(file_name, " ", "_"),
      file_name = iconv(file_name, to = "ASCII//TRANSLIT", sub = "")
    ) %>%
    select(episode, season, title, file_name, link)

  links2 <- links_all_seasons$link

  links2 <- map_chr(
    seq_along(links2),
    function(x) {
      y <- read_html(links2[x])

      y2 <- y %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        grep("en/subtitleserve", ., value = T)

      paste0("https://dl.opensubtitles.org/en/download/sub/", gsub(".*/", "", y2[1]))
    }
  )

  links_all_seasons <- links_all_seasons %>%
    mutate(link = links2)

  try(dir.create("data-raw/subtitles"))
  try(dir.create("data-raw/subtitles-extraction"))
  fwrite(links_all_seasons, file_all_seasons)
} else {
  links_all_seasons <- fread(file_all_seasons)
}


# this part needs reworking bc of download limit
# re-running manually + reboot router is not ok
for (j in 1:nrow(links_all_seasons)) {
  if (!file.exists(paste0("data-raw/subtitles/", links_all_seasons$file_name[j], ".zip"))) {
    Sys.sleep(sample(seq(1, 3, by = 0.5), 1))
    message(links_all_seasons$file_name[j])
    download.file(links_all_seasons$link[j], paste0("data-raw/subtitles/", links_all_seasons$file_name[j], ".zip"), method = "wget")
  }
}

subtiles_zip <- list.files("data-raw/subtitles/", recursive = T) %>%
  paste0("data-raw/subtitles/", .)

for (j in 1:length(subtiles_zip)) {
  try(dir.create(paste0("data-raw/subtitles-extraction/", links_all_seasons$file_name[j])))
  unzip(subtiles_zip[[j]], exdir = paste0("data-raw/subtitles-extraction/", links_all_seasons$file_name[j]), overwrite = F)
}

subtitles_srt <- tibble(sub_location = list.files("data-raw/subtitles-extraction/", pattern = "srt", recursive = T)) %>%
  mutate(
    sub_location = paste0("data-raw/subtitles-extraction/", sub_location),
    folder = as.character(str_extract_all(sub_location, ".*/")),
    file_size = file.size(sub_location)
  ) %>%
  mutate(
    episode_name = str_replace_all(sub_location, "data-raw/subtitles-extraction/", ""),
    episode_name = str_replace_all(episode_name, "/.*", ""),
    season = str_replace_all(substr(episode_name, 1, 7), "_.*", ""),
    episode = str_replace_all(substr(episode_name, 1, 7), ".*_", ""),
    episode_name = str_replace_all(episode_name, "S[0-9][0-9]_E[0-9][0-9]_", ""),
    episode_name = str_replace_all(episode_name, "_", " ")
  )

# for (j in 1:nrow(subtitles_srt)) {
#   system(paste0("recode UTF-8 -f \"", subtitles_srt$sub_location[j], "\""))
# }

for (j in 1:nrow(subtitles_srt)) {
  assign(
    paste(subtitles_srt$season[j], subtitles_srt$episode[j], sep = "_"),
    read_subtitles(subtitles_srt$sub_location[j]) %>%
      clean_names() %>%
      rename(linenumber = id, text = text_content) %>%
      filter(nchar(text) > 0) %>%
      mutate(
        linenumber = as.integer(linenumber),
        season = subtitles_srt$season[j],
        episode = subtitles_srt$episode[j],
        episode_name = subtitles_srt$episode_name[j]
      ) %>%
      filter(!grepl("OpenSubtitles|Open Subtitles|Addic7ed|Subs fixed|English - US|made in georgia|00:", text, ignore.case = T))
  )
}

archer_subs <- mget(ls(pattern = "S[0-9][0-9]_E[0-9][0-9]")) %>%
  bind_rows()

rm(list = ls(pattern = "S[0-9][0-9]_E[0-9][0-9]"))

# inspect final lines
# foo <- archer_subs %>%
#   group_by(episode_name) %>%
#   filter(linenumber == max(linenumber))

archer_subs <- as_tibble(archer_subs)

save(archer_subs, file = "data/archer_subs.rda", compress = "xz")
