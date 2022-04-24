# Calculating concentrations from UV-VIS outputs
# Inputs - UV-VIS samples and standards, readouts
# Outputs - readings in concentration units

### set up and file read in ###
library(ggplot2)
library(tidyverse)
library(readxl)

# input and output file path
infile <- "~/1 School and Research/1 Northwestern/1 Research/2 Method Development/Nutrients/211014_uvvis/211014_uvvis.xlsx"
outpath <- "~/1 School and Research/1 Northwestern/1 Research/2 Method Development/Nutrients/211014_uvvis/"

# read in raw absorbances
abs_raw_no2 <- read_xlsx(infile, sheet="results_no2")
abs_raw_po4 <- read_xlsx(infile, sheet="results_po4")

# read in standards and plate layout
## standards sheet consists of sample id and concentration
standards <- read_xlsx(infile, sheet="standards")
plate <- read_xlsx(infile, sheet="plate_layout")
samples <- read_xlsx(infile, sheet="samples")

# separate by nutrient
samples_no2 <- samples %>%
  filter(stringr::str_detect(label, "no2")) %>%
  mutate(rowID = as.character(seq(1:nrow(.))))

samples_po4 <- samples %>%
  filter(stringr::str_detect(label, "po4")) %>%
  mutate(rowID = as.character(seq(1:nrow(.))))

standards_no2 <- standards %>%
  filter(stringr::str_detect(label, "no2"))

standards_po4 <- standards %>%
  filter(stringr::str_detect(label, "po4"))

# join absorbances to samples and standards
# join absorbance reading to well id
abs_vals_no2 <- samples_no2 %>%
  left_join(.,abs_raw_no2)

abs_vals_po4 <- samples_po4 %>%
  left_join(.,abs_raw_po4)

# join absorbance reading to standard
standards_no2 <- standards_no2 %>%
  left_join(.,abs_raw_no2)

standards_po4 <- standards_po4 %>%
  left_join(.,abs_raw_po4)

# calibration curve
calib_no2 <- lm(conc~abs, data=standards_no2)
predict_no2 <- predict(calib_no2, newdata=abs_vals_no2)
calib_no2_summary <- tidy(calib_no2)

conc_no2 <- predict_no2 %>%
  as.data.frame() %>%
  rename("uM NO2" = ".") %>%
  rownames_to_column("rowID") %>%
  left_join(samples_no2, ., "rowID") %>%
  select(-rowID)

conc_no2$`mgL_N` <- conc_no2$`uM NO2`/1e6*46*14/46*1000*conc_no2$dil


calib_po4 <- lm(conc~abs, data=standards_po4)
predict_po4 <- predict(calib_po4, newdata=abs_vals_po4)
calib_po4_summary <- tidy(calib_po4)

conc_po4 <- predict_po4 %>%
  as.data.frame() %>%
  rename("uM PO4" = ".") %>%
  rownames_to_column("rowID") %>%
  left_join(samples_po4, ., "rowID") %>%
  select(-rowID)

conc_po4$`mgL_P` <- conc_po4$`uM PO4`/1e6*95*31/95*1000*conc_po4$dil

# clean data
conc_no2 <- conc_no2 %>%
  select(-dil)
conc_po4 <- conc_po4 %>%
  select(-dil)

### save output ###

write.csv(conc_no2, paste(outpath,"no2_result.csv",sep=""))
write.csv(conc_po4, paste(outpath,"po4_result.csv",sep=""))
write.csv(calib_no2_summary, paste(outpath,"no2_calib.csv",sep=""))
write.csv(calib_po4_summary, paste(outpath,"po4_calib.csv",sep=""))


### calibration curve visualization ###
ggplot(standards_no2,aes(abs,conc)) +
  geom_point() + 
  stat_smooth(method = "lm", col = "red") +
  theme_bw() +
  ylab("concentration uM NO2") +
  xlab("absorbance") + 
  ggtitle("no2 calibration curve")

ggsave(paste(outpath,"no2_curve.png",sep=""))

ggplot(standards_po4,aes(abs,conc)) +
  geom_point() + 
  stat_smooth(method = "lm", col = "red") +
  theme_bw() +
  ylab("concentration uM PO4") +
  xlab("absorbance") + 
  ggtitle("po4 calibration curve")

ggsave(paste(outpath,"po4_curve.png",sep=""))

