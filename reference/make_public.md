# Make public

Sets READ/DOWNLOAD permissions for web and registered users equivalently
to the "Make Public" button in Synapse UI. TODO: For regular users this
can be a one-and-done action, but for the DCC admin this likely entails
some other actions, such as updating a project tracking table, so a
wrapper or "callback" functionality might be needed.

## Usage

``` r
make_public(id)
```

## Arguments

- id:

  Synapse entity id.
