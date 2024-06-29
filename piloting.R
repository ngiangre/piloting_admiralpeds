# install.packages("devtools")
devtools::install_github("pharmaverse/admiralpeds")

library(admiralpeds)

# get list of datasets from pkg namespace ---------------------------------

data(list="adsl_peds",package="admiralpeds")
datanames <- d$results[,"Item"]
names(datanames) <- datanames
datanames |> 
    lapply(
        function(x){
            try(
                eval(
                    parse(text = 
                              paste0('admiralpeds::',x)
                    )
                )
            )

        }
    )


# Trying out some function examples ----------------------------------------

library(dplyr)
library(lubridate)
library(rlang)
library(admiral)

advs <- dm_peds %>%
    select(USUBJID, BRTHDTC, SEX) %>%
    right_join(., vs_peds, by = "USUBJID") %>%
    mutate(
        VSDT = ymd(VSDTC),
        BRTHDT = ymd(BRTHDTC)
    ) %>%
    derive_vars_duration(
        new_var = AGECUR_D,
        new_var_unit = CURU_D,
        start_date = BRTHDT,
        end_date = VSDT,
        out_unit = "days",
        trunc_out = FALSE
    ) %>%
    derive_vars_duration(
        new_var = AGECUR_M,
        new_var_unit = CURU_M,
        start_date = BRTHDT,
        end_date = VSDT,
        out_unit = "months",
        trunc_out = FALSE
    ) %>%
    mutate(
        AGECUR = ifelse(AGECUR_D >= 365.25 * 2, AGECUR_M, AGECUR_D),
        AGECURU = ifelse(AGECUR_D >= 365.25 * 2, CURU_M, CURU_D)
    )

#' metadata is in months
cdc_meta_criteria <- admiralpeds::cdc_wtage %>%
    mutate(
        age_unit = "months",
        SEX = ifelse(SEX == 1, "M", "F")
    )

#' metadata is in days
who_meta_criteria <- bind_rows(
    (admiralpeds::who_wt_for_age_boys %>%
         mutate(
             SEX = "M",
             age_unit = "days"
         )
    ),
    (admiralpeds::who_wt_for_age_girls %>%
         mutate(
             SEX = "F",
             age_unit = "days"
         )
    )
) %>%
    rename(AGE = Day)

criteria <- bind_rows(
    cdc_meta_criteria,
    who_meta_criteria
) %>%
    rename(AGEU = age_unit)

derive_params_growth_age(
    advs,
    sex = SEX,
    age = AGECUR,
    age_unit = AGECURU,
    meta_criteria = criteria,
    parameter = VSTESTCD == "WEIGHT",
    analysis_var = VSSTRESN,
    set_values_to_sds = exprs(
        PARAMCD = "WGASDS",
        PARAM = "Weight-for-age z-score"
    ),
    set_values_to_pctl = exprs(
        PARAMCD = "WGAPCTL",
        PARAM = "Weight-for-age percentile"
    )
)
