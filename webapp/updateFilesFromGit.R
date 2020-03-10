library(httr)

## LIST FILE PRESENTI
curFileList <- list.files("www/pcm_data/", full.names=F)

## CHECK FILE SU GITHUB
my_url <- modify_url("https://api.github.com", path = "/repos/pcm-dpc/COVID-19/contents/dati-province")
my_resp <- GET(my_url)
my_json <- jsonlite::fromJSON(content(my_resp, "text"), simplifyVector = FALSE)
git_files <- vapply(my_json, function(x) x$path, "a")
git_files <- gsub("dati-province/","", git_files)
git_files <- git_files[grepl(".*-\\d{,8}.csv$",git_files)]

## DOWNLOAD NEW FILES
new_files <- setdiff(git_files, curFileList)
if (length(new_files)) {
	for (ff in new_files)
		download.file(paste0("https://github.com/pcm-dpc/COVID-19/raw/master/dati-province/", ff), paste0("www/pcm_data/", ff))
}
