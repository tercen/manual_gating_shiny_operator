# Manual Gating Shiny Operator

### Roadmap

#### To 0.1.0
* ~~ Add biexponential scaling ~~
* ~~ Show saved gate when opening the operator again ~~
* ~~ Selection Accuracy Improvement ~~

#### To 0.2.0
* 1d Gating
* UI improvements (tick length in biexponential plot)
* ~~ Quadrant gating ~~
* ~~ Add log scaling ~~
* Change input mapping to gather


#### Unscheduled
* Create an update mode (read polygon info and update it)
* Semi-automatic gating
* Spider gating
* Ellipsoid gating
* Density-based gating



#### Known Bugs 

* The operator currently outputs the input mapping. The JoinOperator needs to be fixed, otherwise it tries to add all flag values to each input entry
* Trying to save the gate before drawing is currently allowed and causes an error
* Axis appearance is different between linear and biexponential plots

