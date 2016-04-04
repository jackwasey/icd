#!/bin/bash

R -d gdb -e 'devtools::load_all(); icd10_comorbid_parent_search_use_cpp(uranium_pathology, icd10_map_ahrq, visit_name = "case", icd_name = "icd10", short_code = F, short_map = T, return_df = F)'
