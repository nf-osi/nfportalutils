# Create reference file for new cancer type

Helper for creating reference for new cancer subtype which does not
already exist. https://docs.cbioportal.org/file-formats/#cancer-type

## Usage

``` r
cbp_new_cancer_type(type_of_cancer, name, color, parent_type_of_cancer)
```

## Arguments

- type_of_cancer:

  Id for new cancer type, e.g. "cnf".

- name:

  Full name for new cancer type, e.g. "Cutaneous Neurofibroma"

- color:

  Color name for new cancer;
  https://en.wikipedia.org/wiki/Web_colors#X11_color_names.

- parent_type_of_cancer:

  Id of existing parent, e.g. "nfib" for Neurofibroma.
