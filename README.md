# manual gating shiny operator

#### Description

`manual gating` shiny operator is a graphic, interactive operator that allow manually drawing polygonal gates
in 1d and 2d FCS data.

##### Usage

Input projection |.
---|---
`row`   | represents the variables (e.g. genes, channels, markers)
`col`   | represents the observations (e.g. cells, samples, individuals) 
`y-axis`| measurement value
`color` | Value used to color the data-points (e.g. previous classification flag)


Output relations|.
---|---
`FLAGID_NegNeg, FLAGID_NegPos, ..., `| Boolean [0 or 1] indicating gate membership.

`FLAGID` is a string defined by the used in the operator UI.

##### See Also

[Automated gating operator](https://github.com/tercen/automated_gating_operator)



