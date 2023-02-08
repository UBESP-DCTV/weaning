# import_patient works

    Code
      res
    Output
      # A tibble: 1,929 x 45
         type  id_re~1 filte~2 id_un~3 id_me~4 data_let~5 ega_ph ega_p~6 ega_p~7  sofa
         <fct>   <dbl>   <dbl> <chr>     <dbl> <date>      <dbl>   <dbl>   <dbl> <dbl>
       1 nava      356       0 AN001        81 2013-10-22   7400  106000   41000    10
       2 nava      361       0 AN001        81 2013-10-23   7430  153000   38000     9
       3 nava      365       0 AN001        81 2013-10-24   7450   94000   42000     9
       4 nava      374       0 AN001        81 2013-10-25   7430   46000  109000     7
       5 nava      380       0 AN001        81 2013-10-26   7460  121000   43000     5
       6 psv       384       0 AN002        81 2013-10-27   7410   89000   41000     7
       7 psv       396       0 AN002        81 2013-10-28   7430   95000   39000     6
       8 psv       397       0 AN002        81 2013-10-29   7370  109000   53000     4
       9 psv       400       0 AN002        81 2013-10-30   7510   98000   40000     5
      10 psv       401       0 AN002        81 2013-10-31   7480   80000   41000     5
      # ... with 1,919 more rows, 35 more variables: cpis <dbl>, estubato <lgl>,
      #   reintubato <lgl>, morto <lgl>, susp_aspir <lgl>, susp_tosse <lgl>,
      #   susp_gcs <lgl>, susp_fcpas <lgl>, susp_drugs <lgl>, susp_pafi <lgl>,
      #   susp_peep <lgl>, susp_vt <lgl>, susp_rr <lgl>, susp_distress <lgl>,
      #   susp_ph <lgl>, susp_rass <lgl>, stop_fr <lgl>, stop_distress <lgl>,
      #   stop_spo2 <lgl>, stop_pas <lgl>, stop_fc <lgl>, stop_rass <lgl>,
      #   fail_agit <lgl>, fail_coma <lgl>, fail_muscoli <lgl>, ...

