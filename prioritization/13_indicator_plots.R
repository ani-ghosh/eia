library(readxl)
library(tidyverse)
library(viridis)
library(RColorBrewer)
library(officer)

datadir <- "G:/My Drive/work/ciat/eia/analysis"

xl <- "G:/My Drive/work/ciat/eia/analysis/output/Level123_cgregion_country_farming_system_2021-07-11 15-08 - BV.xlsx"

dd <- lapply(2:7, function(x){
  d <- read_excel(xl, sheet = x)
  d <- d[3:nrow(d), ]
  return(d)
})
dd <- bind_rows(dd)

dd <- dd %>%
  mutate_at(vars(starts_with("level")), funs(round(as.numeric(.), 2)))

colourCount <- length(unique(dd$farming_system))
getPalette <- colorRampPalette(brewer.pal(11, "Paired"))

cols <- grep("^level", names(dd), value = TRUE)

for(col in cols){
  ggplot(dd, aes(y=!!sym(col), fill=farming_system, x=ISO_A3)) + 
    geom_bar(position="stack", stat="identity") + facet_wrap(~cgregion, scales = "free_x") +
    scale_fill_manual(values = getPalette(colourCount)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust=0, vjust = 0.5, size = 8))
  cvar <- str_split(col, "\\(", simplify = TRUE)[1]
  ofile <- file.path(datadir, "output\\indicator_plots", paste0(cvar, ".png"))
  ggsave(ofile, width = 12, height = 5, dpi = 300, units = "in")
}

#################################################################################################
# Pick whichever blank template you want
file <- "16-9.pptx"
# Get the path to the template file:
template <- system.file("extdata", file, package = "xaringanBuilder", mustWork = TRUE)
out <- read_pptx(template)

create_pptx <- function(img, path, w, h){
  
  nm <- gsub(".png","",basename(img))
  out %>%
    add_slide(layout = "Blank", master = "Office Theme") %>%
    ph_with(block_list(fpar(ftext(nm, prop = fp_text(font.size = 12)))),
            location = ph_location(left = 0.2, top = 0, width = 10, height = 0.5)) %>%
    ph_with(external_img(img, width = w, height = h),
            location = ph_location(left = 0.2, top = 0.6),
            use_loc_size = FALSE) %>%
    print(target = path)
}

ff <- list.files(file.path(datadir, "output", "indicator_plots"), pattern = ".png", full.names = TRUE)
pptpath <- file.path(file.path(datadir, "output", "indicator_plots", "eia_indicators.pptx"))

for(img in ff){
  create_pptx(img, pptpath, 12, 5)
}



