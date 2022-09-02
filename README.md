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

#### To 0.3.0
* ~~ Overall plot consistency ~~
* ~~ Ellipsoid gating ~~
* ~~ Placement of cell selection percentage ~~
* ~~ Spider gating ~~

#### To 0.4.0
* ~~ Move transforms out of the operator  ~~

#### Unscheduled
* Create an update mode (read polygon info and update it)



#### Known Bugs / Limitations
* Logicle transform is unstable, crashing on certain data. 
* Plot parameters are currently hard-coded and fixed.
* Plot ticks in the 1d are not always appearing (depends on data range)
* ~~ Loading modal dialogue does not start with page load ~~ Now just show a GIF and hide the buttons while it waits for the connection to the Shiny server
* ~~ The operator currently outputs the input mapping. The JoinOperator needs to be fixed, otherwise it tries to add all flag values to each input entry ~~
* ~~ Trying to save the gate before drawing is currently allowed and causes an error ~~
* ~~ Axis appearance is different between linear and biexponential plots ~~

