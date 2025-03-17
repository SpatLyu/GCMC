#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~             Plot Case2 Result            ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~    Author: Wenbo Lv; Date: 2025-03-17    ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

source('./script/case_plot_funs.r')

gcmc_case2 = readxl::read_xlsx('./result/case/case2.xlsx',sheet = "gcmc")
gccm_case2 = readxl::read_xlsx('./result/case/case2.xlsx',sheet = "gccm")
pcc_case2 = readxl::read_xlsx('./result/case/case2.xlsx',sheet = "pcc")
gd_case2 = readxl::read_xlsx('./result/case/case2.xlsx',sheet = "gd")

plot_cs_matrix(gcmc_case2)
plot_cs_matrix(gccm_case2)
plot_cs_matrix(pcc_case2,legend_title = "Correlation")
plot_cs_matrix(gd_case2,legend_title = "Association")