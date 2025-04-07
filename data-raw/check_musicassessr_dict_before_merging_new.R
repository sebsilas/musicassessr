

# Check current musicassessr_dict against a new one (e.g., where someone has added new translations)

# And make sure they haven't gone out of sync (e.g., new keys added to the main file, whilst someone else is translating an older file without the new keys)


load_all()

musicassessr_dict <- musicassessr_dict %>%
  as.data.frame()

new_translation_dict <- readxl::read_excel("~/Downloads/musicassessr_dict_YanbingHU_Revision_Hsin-Rui Lin20250308.xlsx")


length(setdiff(musicassessr_dict$key, new_translation_dict$key)) == 0L
