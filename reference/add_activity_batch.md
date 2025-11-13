# Add activity to multiple entities

Wrapper provenance function that does a little more work to expand
many-to-many mappings to create records of entity, activity, and input.

## Usage

``` r
add_activity_batch(entities, act_name, act_executed, used_inputs)
```

## Arguments

- entities:

  Vector or list of entities.

- act_name:

  Vector or list of activity name.

- act_executed:

  Vector or list of reference activity executed.

- used_inputs:

  Vector or list of inputs for each entity.
