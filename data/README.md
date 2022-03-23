# How to read the data

> WARNING: `weaning` is
>
>    - ~150 MB `{qs}`-stored on disk (default "high" compression)
>    - ~650 MB RAM loaded in R.

``` r
weaning_path <- here::here("data/weaning.qs")
weaning <- qs::qread(weaning_path)
```
