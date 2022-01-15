# sYNGEO_Func_Sync_V2

# Code index

1 - 01a_trait_ordination_interpolated_abundances.R
2 - 01b_single_traits_interpolated_abundances.R
3 - 02a_sync_trait_ord_groups_eff_resp_interpolated.R
4 - 02b_sync_single_traits_groups_eff_resp_interpolated.R
5 - 03a_dummy_connectivity_variable_interpolated.R
6 - 04_differences_in_synchrony.R
7 - 05a_figures_single_traits.R
8 - 05b_figures_ordination.R


# 01a_trait_ordination_interpolated_abundances.R

Takes raw abundance data and traits for each species
cleans and interpolates abundances. 
produces ordinations for each trait group

#01b_single_traits_interpolated_abundances.R

same as above but with single traits
Produces CWMs (interpolated) for all traits and groups


### output_data from scripts 02a & 02b & 3a & 3b are saved into output_data/sync under .gitignore
# 02a_sync_trait_ord_groups_eff_resp_interpolated.R

calculates synchrony with ordination scores
within site, between site, jackknife (leaving one year out)


# 02b_sync_single_traits_groups_eff_resp_interpolated.R

calculates synchrony for single traits
within site, between site, jackknife (leaving one year out)
also includes synchrony calculation for temperature and flow

## 03a_dummy_connectivity_variable_interpolated.R

calculates distances and connectivity variable
joins to all synchrony datasets

## 04_differences_in_synchrony.R

calculates the differences in synchrony from the calculation of sync using all years and the jack knife leave one out (LOO)
some NAs in df, from where correaltion couldn't be calculated in scrip 02a/b

## 05a_figures_single_traits.R
## 05b_figures_ordination.R





