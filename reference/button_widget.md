# Generate button widget for a Synapse wiki

Generate markup for a button widget for a Synapse project wiki. Refer to
widget docs at
<https://help.synapse.org/docs/Wikis.1975746682.html#Wikis-WikiWidgets>.
Buttons should be created sparingly and strategically. See
[`remove_button`](remove_button.md) in case of future regret.

## Usage

``` r
button_widget(label, url, align = c("None", "Left", "Right", "Center"))
```

## Arguments

- label:

  Button label text.

- url:

  URL that the button will link to.

- align:

  Button alignment, can be one of "None", "Left", "Right", or "Center"
  (defaults to "None").
