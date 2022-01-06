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

#######################################################################################################
# averaged over selected country*farming systems
rfc <- readxl::read_excel(xl, sheet = 1)
dds <- dd[dd$`Population, poverty, density` == 1, ]

cols <- grep("^level2_", names(dd), value = TRUE)

dds <- dd %>%
  filter(`Population, poverty, density` == 1) %>%
  group_by(cgregion, farming_system) %>%
  select(cols) %>%
  summarise_all(funs(round(mean(.,na.rm = T), 2))) %>%
  pivot_longer(cols, names_to = "KPI", values_to = "values") %>%
  mutate(cg_fs = paste0(cgregion, "_", farming_system)) %>%
  mutate(KPI = str_remove(KPI, "level2_"))

ggplot(dds, aes(y=values, x=cg_fs, fill = cgregion)) + 
  geom_bar(stat="identity", width = 0.5) + facet_grid(KPI ~ ., scales = "free_y") +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank()
  ) +
  geom_text(aes(label = KPI), x = 1, y = Inf, vjust = 1, hjust = 0) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  xlab("CGIAR region * Farming System") +
  ylab("Average value of Agronomic KPIs") 

ofile <- file.path(datadir, "output/indicator_plots/combined_plots.png")
ggsave(ofile, width = 8, height = 11, dpi = 300, units = "in")

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