# stoner

<!-- badges: start -->
[![R build status](https://github.com/vimc/stoner/workflows/R-CMD-check/badge.svg)](https://github.com/vimc/stoner/actions)
<!-- badges: end -->

<pre>
     .-'''-. ,---------.    ,-----.    ,---.   .--.    .-''-.  .-------.
    / _     \\          \ .'  .-,  '.  |    \  |  |  .'_ _   \ |  _ _   \
   (`' )/`--' `--.  ,---'/ ,-.|  \ _ \ |  ,  \ |  | / ( ` )   '| ( ' )  |
  (_ o _).       |   \  ;  \  '_ /  | :|  |\_ \|  |. (_ o _)  ||(_ o _) /
   (_,_). '.     :_ _:  |  _`,/ \ _/  ||  _( )_\  ||  (_,_)___|| (_,_).' __
  .---.  \  :    (_I_)  : (  '\_/ \   ;| (_ o _)  |'  \   .---.|  |\ \  |  |
  \    `-'  |   (_(=)_)  \ `"/  \  ) / |  (_,_)\  | \  `-'    /|  | \ `'   /
   \       /     (_I_)    '. \_/``".'  |  |    |  |  \       / |  |  \    /
    `-...-'      '---'      '-----'    '--'    '--'   `'-..-'  ''-'   `'-'
</pre>

Stoner is a helper package for writing montagu-imports concerning touchstones and associated responsibilities and 
expectations. It allows creation of or changes to a touchstone to be specified by CSV files.

# Using stoner

* `install.packages("stoner")`
* Create a [montagu-import](https://github.com/vimc/montagu-imports).
* Write CSV files in a folder called `meta` inside that import folder, or use `stoner::stone_dump(con, touchstone, path)` 
to dump a set of CSVs for an existing touchstone, which you could (carefully) modify.
* For your *extract* call `stoner::stone_extract(path, con)`
* For your *test-extract* call `stoner::stone_test_extract(extracted_data)`
* For your *transform* call `stoner::stone_transform(extracted_data)`
* For your *test-transform* call `stoner::stone_test_transform(transform_data)`
* For your *load* call `stoner::stone_load(transformed_data, con)`

The above is the simplest case. You should also write some load tests as usual,
but these can now concentrate entirely on what rows the particular import is
expected to add. See the vignette for more detail.
