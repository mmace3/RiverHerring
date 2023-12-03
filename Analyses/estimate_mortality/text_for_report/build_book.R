

bookdown::render_book("index.Rmd", "officedown::rdocx_document")


# bookdown::render_book("index.Rmd", "bookdown::word_document2")


# get copy of .bib file from main folder where I keep it
file.copy(from = "C:/Users/MMace/Documents/projects/LitCited/LitCited.bib", 
          to = paste0(getwd(), "/LitCited.bib"),
          overwrite = TRUE)


# get copy of .csl file from main folder where I keep it
file.copy(from = "C:/Users/MMace/Documents/projects/LitCited/Styles/fisheries-research_mod.csl", 
          to = paste0(getwd(), "/LitCited.bib"),
          overwrite = TRUE)