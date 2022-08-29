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
* Fix biexponential ticks in 1d
* Overall plot consistency
* ~~ Placement of cell selection percentage ~~

#### Unscheduled
* Create an update mode (read polygon info and update it)
* Spider gating
* Ellipsoid gating



#### Known Bugs / Limitations

* Logicle transform is unstable, crashing on certain data. 
* Plot parameters are currently fixed.
* ~~ The operator currently outputs the input mapping. The JoinOperator needs to be fixed, otherwise it tries to add all flag values to each input entry ~~
* Trying to save the gate before drawing is currently allowed and causes an error
* Axis appearance is different between linear and biexponential plots

