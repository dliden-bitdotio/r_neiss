library(tidyverse)
library(httr)
library(magrittr)

# Downloads data from the specified years
# Tested for 2016 to 2020
download_neiss = function(years = seq(2016, 2020, 1)) {
  data_list = list()
  i =  1
  for (year in years) {
    address = str_c(
      "https://www.cpsc.gov/cgibin/NEISSQuery/Data/Archived%20Data/",
      year,
      "/neiss",
      year,
      ".xlsx"
    )
    GET(address, write_disk(tf <-
                              tempfile(
                                fileext = ".xlsx", tmpdir = "."
                              )))
    data <- readxl::read_excel(path.expand(tf))
    unlink(tf)
    data_list[[i]] = tibble(data)
    i = i + 1
  }
  return(bind_rows(data_list))
}

# Downloads neiss codebook
download_codebook <- function() {
    codebook <- read_delim("https://www.cpsc.gov/cgibin/NEISSQuery/Data/Info%20Docs/neiss_fmt.txt", delim="\t")
    return(codebook)
}

# splits neiss codebook by category to make a list of
# reference codebooks for e.g. body part codes, product codes,
# location codes, etc.
process_codebook <- function(codebook) {
    ref <- codebook %>%
        filter(`Format name` %in% c("BDYPT", "DIAG", "DISP", "LOC",
                                    "PROD", "RACE", "HISP", "FIRE", "ALC_DRUG")) %>%
        rename(value=`Starting value for format`, name=`Format name`,
               label = `Format value label`) %>%
        select(-`Ending value for format`) %>%
        mutate(label_clean = str_remove(label, "^\\d* -? ?"),
               value = suppressWarnings(as.numeric(value)))

    prod <- filter(ref, name=="PROD")
    diag <- filter(ref, name=="DIAG")
    alc_drug <- filter(ref, name=="ALC_DRUG")
    bodypt <- filter(ref, name=="BDYPT")
    race <- filter(ref, name=="RACE")
    disp <- filter(ref, name=="DISP")
    fire <- filter(ref, name=="FIRE")
    loc <- filter(ref, name=="LOC")
    eth <- filter(ref, name=="HISP")
    out = list(prod, diag, alc_drug, bodypt, race, disp, fire, loc, eth)
    names(out) = c("prod", "diag", "alc_drug", "bodypt",
                   "race", "disp", "fire", "loc", "eth")
    return(out)
}

process_neiss_data <- function(data, codelists) {
    list2env(codelists, env = environment())
    labeled_data <- data %>% 
        left_join(prod, by = c("Product_1" = "value")) %>%
        mutate(Product_1 = label_clean) %>%
        select(-label, -label_clean) %>%
        left_join(prod, by = c("Product_2" = "value")) %>%
        mutate(Product_2 = label_clean) %>%
        select(-label, -label_clean) %>%
        left_join(prod, by = c("Product_3" = "value")) %>%
        mutate(Product_3 = label_clean) %>%
        select(-label, -label_clean) %>%
        left_join(bodypt, by = c("Body_Part" = "value")) %>%
        mutate(Body_Part = label_clean) %>%
        select(-label, -label_clean) %>%
        left_join(bodypt, by = c("Body_Part_2" = "value")) %>%
        mutate(Body_Part_2 = label_clean) %>%
        select(-label, -label_clean) %>%
        left_join(diag, by = c("Diagnosis" = "value")) %>%
        mutate(Diagnosis = label_clean) %>%
        select(-label, -label_clean) %>%
        left_join(diag, by = c("Diagnosis_2" = "value")) %>%
        mutate(Diagnosis_2 = label_clean) %>%
        select(-label, -label_clean)%>%
        left_join(alc_drug, by = c("Alcohol" = "value")) %>%
        mutate(Alcohol = label_clean) %>%
        select(-label, -label_clean) %>%
        left_join(alc_drug, by = c("Drug" = "value")) %>%
        mutate(Drug = label_clean) %>%
        select(-label, -label_clean) %>%
        left_join(race, by = c("Race" = "value")) %>%
        mutate(Race = label_clean) %>%
        select(-label, -label_clean) %>%
        left_join(disp, by = c("Disposition" = "value")) %>%
        mutate(Disposition = label_clean) %>%
        select(-label, -label_clean) %>%
        left_join(fire, by = c("Fire_Involvement" = "value")) %>%
        mutate(Fire_Involvement = label_clean) %>%
        select(-label, -label_clean) %>%
        left_join(loc, by = c("Location" = "value")) %>%
        mutate(Location = label_clean) %>%
        select(-label, -label_clean) %>%
        left_join(eth, by = c("Hispanic" = "value")) %>%
        mutate(Hispanic = label_clean) %>%
        select(-label, -label_clean, -starts_with("name"))

    labeled_data$Alcohol[str_detect(labeled_data$Alcohol, "NA before")] = NA
    labeled_data$Drug[str_detect(labeled_data$Drug, "NA before")] = NA
    labeled_data$Hispanic[str_detect(labeled_data$Hispanic, "NA before")] = NA

    labeled_data$Sex = case_when(labeled_data$Sex == 0 ~ "Not Recorded",
                                 labeled_data$Sex == 1 ~ "Male",
                                 labeled_data$Sex == 2 ~ "Female")
    labeled_data$Age[labeled_data$Age > 200] <-
        (labeled_data$Age[labeled_data$Age > 200] - 200) / 12

    labeled_data$Narrative[is.na(labeled_data$Narrative)] =
        labeled_data$Narrative_1[!(is.na(labeled_data$Narrative_1))]
    labeled_data %<>% select(-Narrative_1)



    # labeled_data$Age
    return(labeled_data)
}


main <- function(years = seq(2016, 2020, 1),
                 to_csv = TRUE,
                 return = FALSE){
    print("Downloading Neiss Data...")
    data <- download_neiss(years)
    print("Downloading and Processing Codebook...")
    codebook <- download_codebook()
    codelists <- process_codebook(codebook)
    print("Cleaning and Processing Neiss Data...")
    clean_data <- process_neiss_data(data = data, codelists = codelists)
    if(to_csv) write_csv(clean_data, "neiss_data.csv")
    if(return) return(clean_data)
}

main()

