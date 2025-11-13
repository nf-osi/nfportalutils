# Adjust view

When a view schema and data are mismatched, the view cannot be built and
therefore cannot be queried. The most common causes (likely accounting
for 98%+ of instances combined) will be the max size/length issues. This
will iteratively update the schema based exactly on whatever the server
is saying to do\* for the sizing and list length issues, until the view
is functional and querying works. However, if the issue is not one of
these, this will fail because handlers for other problems are currently
not implemented.

## Usage

``` r
adjust_view(view, max_tries = 5L, check_byte_budget = TRUE)
```

## Arguments

- view:

  Synapse view id.

- max_tries:

  Max number of tries. Vast majority of views should only have
  accumulated 1-2 bad data mutations, so a default of 5 should be
  reasonable.

- check_byte_budget:

  Check if this will lead to exceeding table budget.

## Details

\*Note: Fixes are applied iteratively because that's how server
currently surfaces repair recommendations.
