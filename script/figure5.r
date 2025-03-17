#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~             Plot Case3 Result            ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~    Author: Wenbo Lv; Date: 2025-03-17    ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

source('./script/case_plot_funs.r')

gcmc_case3 = readxl::read_xlsx('./result/case/case3.xlsx',sheet = "gcmc")
gccm_case3 = readxl::read_xlsx('./result/case/case3.xlsx',sheet = "gccm")
pcc_case3 = readxl::read_xlsx('./result/case/case3.xlsx',sheet = "pcc")
gd_case3 = readxl::read_xlsx('./result/case/case3.xlsx',sheet = "gd")

plot_cs_matrix(gcmc_case3)
plot_cs_matrix(gccm_case3)
plot_cs_matrix(pcc_case3,legend_title = "Correlation")
plot_cs_matrix(gd_case3,legend_title = "Association")