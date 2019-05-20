# Rcheck

This folder contains the main outputs of a `R CDM check` call, as well as the tar.gz used for the corresponding check.

## Configuration

This is the configuration of the testing plateform:

```{r}
> devtools::load_all(".")
> sessionInfo()
R version 3.6.0 (2019-04-26)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Mojave 10.14.4

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods  
[7] base     

other attached packages:
[1] adRes_1.0.0

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.1        pillar_1.4.0      compiler_3.6.0   
 [4] prettyunits_1.0.2 remotes_2.0.4     iterators_1.0.10 
 [7] tools_3.6.0       digest_0.6.18     pkgbuild_1.0.3   
[10] pkgload_1.0.2     tibble_2.1.1      memoise_1.1.0    
[13] nlme_3.1-140      lattice_0.20-38   pkgconfig_2.0.2  
[16] doSNOW_1.0.16     rlang_0.3.4       Matrix_1.2-17    
[19] foreach_1.4.4     cli_1.1.0         rstudioapi_0.10  
[22] yaml_2.2.0        parallel_3.6.0    spaMM_2.7.1      
[25] metafor_2.1-0     dplyr_0.8.1       withr_2.1.2      
[28] desc_1.2.0        fs_1.3.1          devtools_2.0.2   
[31] tidyselect_0.2.5  rprojroot_1.3-2   grid_3.6.0       
[34] glue_1.3.1        R6_2.4.0          snow_0.4-3       
[37] processx_3.3.1    pbapply_1.4-0     binom_1.1-1      
[40] sessioninfo_1.1.1 purrr_0.3.2       callr_3.2.0      
[43] magrittr_1.5      backports_1.1.4   ps_1.3.0         
[46] codetools_0.2-16  usethis_1.5.0     MASS_7.3-51.4    
[49] assertthat_0.2.1  proxy_0.4-23      crayon_1.3.4   
```
