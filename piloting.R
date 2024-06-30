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


# derive_params_growth_height examples ----------------------------------------

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
        new_var = AAGECUR,
        new_var_unit = AAGECURU,
        start_date = BRTHDT,
        end_date = VSDT,
        out_unit = "days"
    )

heights <- vs_peds %>%
    filter(VSTESTCD == "HEIGHT") %>%
    select(USUBJID, VSSTRESN, VSSTRESU, VSDTC) %>%
    rename(
        HGTTMP = VSSTRESN,
        HGTTMPU = VSSTRESU
    )

advs <- advs %>%
    right_join(., heights, by = c("USUBJID", "VSDTC"))

advs_under2 <- advs %>%
    filter(AAGECUR < 730)

advs_over2 <- advs %>%
    filter(AAGECUR >= 730.5)

who_under2 <- bind_rows(
    (admiralpeds::who_wt_for_lgth_boys %>%
         mutate(
             SEX = "M",
             height_unit = "cm"
         )
    ),
    (admiralpeds::who_wt_for_lgth_girls %>%
         mutate(
             SEX = "F",
             height_unit = "cm"
         )
    )
) %>%
    rename(
        HEIGHT_LENGTH = Length,
        HEIGHT_LENGTHU = height_unit
    )

who_over2 <- bind_rows(
    (admiralpeds::who_wt_for_ht_boys %>%
         mutate(
             SEX = "M",
             height_unit = "cm"
         )
    ),
    (admiralpeds::who_wt_for_ht_girls %>%
         mutate(
             SEX = "F",
             height_unit = "cm"
         )
    )
) %>%
    rename(
        HEIGHT_LENGTH = Height,
        HEIGHT_LENGTHU = height_unit
    )


advs_under2 <- derive_params_growth_height(
    advs_under2,
    sex = SEX,
    height = HGTTMP,
    height_unit = HGTTMPU,
    meta_criteria = who_under2,
    parameter = VSTESTCD == "WEIGHT",
    analysis_var = VSSTRESN,
    set_values_to_sds = exprs(
        PARAMCD = "WGHSDS",
        PARAM = "Weight-for-height z-score"
    ),
    set_values_to_pctl = exprs(
        PARAMCD = "WGHPCTL",
        PARAM = "Weight-for-height percentile"
    )
)

advs_over2 <- derive_params_growth_height(
    advs_over2,
    sex = SEX,
    height = HGTTMP,
    height_unit = HGTTMPU,
    meta_criteria = who_over2,
    parameter = VSTESTCD == "WEIGHT",
    analysis_var = VSSTRESN,
    set_values_to_sds = exprs(
        PARAMCD = "WGHSDS",
        PARAM = "Weight-for-height z-score"
    ),
    set_values_to_pctl = exprs(
        PARAMCD = "WGHPCTL",
        PARAM = "Weight-for-height percentile"
    )
)

bind_rows(advs_under2, advs_over2)
