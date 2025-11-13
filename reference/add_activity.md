# Add activity to entity

Util for adding activity info to a file entity. See also
https://help.synapse.org/docs/Provenance.1972470373.html

## Usage

``` r
add_activity(entity, act_name, act_executed, used_inputs)
```

## Arguments

- entity:

  Synapse entity id.

- act_name:

  Name of activity.

- act_executed:

  Reference to the the activity executed (URL preferred).

- used_inputs:

  Vector of inputs for this act, e.g. syn ids, links to other data
  sources, etc.
