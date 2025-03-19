NCO <- readxl::read_xlsx(here("Data","NCO_pregnancy.xlsx"))

nco_codelists <- lapply(as.list(NCO$OutcomeName),
                        function(x) {
                          cl <- CodelistGenerator::getCandidateCodes(cdm, x, domains = c("Condition", "Observation", "Procedure", "Measurement"))
                          return(cl$concept_id)
                        })

names(nco_codelists) <- NCO$OutcomeName

save(nco_codelists, file = here("Data", "nco.RData"))

nco_summary <- lapply(as.list(names(nco_codelists)),
                      function(x) {
                        return(tibble(NCO_name = x,
                                      concept_id = nco_codelists[[x]]))
                      }) %>%
  bind_rows() %>%
  inner_join(cdm$concept %>%
               select(concept_id, concept_name),
             copy = TRUE) %>%
  collect()

write_csv(nco_summary, file = here("Data", "pregnant_full_NCO_list.csv"))
