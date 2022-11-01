
# Create test data for old annotations that can be especially inconsistent with regard to denoting NA
# `Comments` is included because the typical view data includes non-clinical data that needs to be ignored
n <- 20
test_data_ok <- data.frame(individualID = sample(1:6, n, replace = TRUE),
                        comments = sample(c("foo", "bar", "baz", ""), n, replace = TRUE)) %>%
  dplyr::mutate(sex = ifelse(individualID %% 2, "Male", "Female"),
         nf1Genotype = c("+/-", "-/-", "+/+", "", "NA", "unknown")[individualID],
         nf2Genotype = c("-/-", "+/-", "+/+", "", "NA", "unknown")[individualID],
         age = as.character(c(12, NA, 23, 11, 46, 17)[individualID]),
         specimenID = sample(1:50, n, replace = FALSE),
         tumorType = sample(c("Plexiform Neurofibroma", "", "NA"), n, replace = TRUE),
         tissue = sample(c("Blood", "Skin", "", "NA"), n, replace = TRUE),
         individualID = paste0("p", individualID))

test_data_missing_an_attribute <- test_data_ok %>% dplyr::select(-tissue)
test_data_missing_sample_attributes <- test_data_ok %>% dplyr::select(-c(specimenID, tumorType, age, tissue))
         
  
  
  
                        
