# Remove button from a project wiki

This provides a way to remove buttons that should no longer be present,
possibly as decided by newer and wiser design decisions. See also
[`button_widget`](button_widget.md). The target button is selected based
on the specified text label. If for some reason there are multiple
buttons with the same label, all of them will be removed.

## Usage

``` r
remove_button(wiki, label, dry_run = TRUE)
```

## Arguments

- wiki:

  The wiki object to operate on.

- label:

  Button label text.

- dry_run:

  Whether to return a wiki object only without actually performing
  update.
