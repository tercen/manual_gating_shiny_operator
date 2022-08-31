# Manual Gating Shiny Operator

### Roadmap

#### To 0.1.0
* ~~ Add biexponential scaling ~~
* ~~ Show saved gate when opening the operator again ~~
* ~~ Selection Accuracy Improvement ~~

#### To 0.2.0
* ~~ 1d Gating ~~
* ~~ Quadrant gating ~~
* ~~ Add log scaling ~~
* ~~ Change input mapping to gather ~~

#### To 0.2.5
* ~~ Overall plot consistency ~~
* ~~ Ellipsoid gating ~~
* ~~ Placement of cell selection percentage ~~
* ~~ Spider gating ~~

#### Unscheduled
* Create an update mode (read polygon info and update it)
* Fix biexponential ticks in 1d


#### Known Bugs / Limitations
* Loading modal dialogue does not start with page load
* Logicle transform is unstable, crashing on certain data. 
* Plot parameters are currently fixed.
* ~~ The operator currently outputs the input mapping. The JoinOperator needs to be fixed, otherwise it tries to add all flag values to each input entry ~~
* Trying to save the gate before drawing is currently allowed and causes an error
* ~~ Axis appearance is different between linear and biexponential plots ~~

