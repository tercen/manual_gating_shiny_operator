# Manual Gating Shiny Operator

### Roadmap

#### To 0.1.0
* <del>Add biexponential scaling</del>
* <del>Show saved gate when opening the operator again</del>
* <del>Selection Accuracy Improvement</del>

#### To 0.2.0
* <del>1d Gating</del>
* <del>Quadrant gating</del>
* <del>Add log scaling</del>
* <del>Change input mapping to gather</del>

#### To 0.3.0
* <del>Overall plot consistency</del>
* <del>Ellipsoid gating</del>
* <del>Placement of cell selection percentage</del>
* <del>Spider gating</del>

#### To 0.4.0
* Move transforms out of the operator
* Change how flags are saved [+,-], single column for each - 1d and quadrant
* Flag column names should come from a parameter set by the user
* Add margins to the axes
* Color dots by an optional 'color' column [flag or continuous]


#### Unscheduled 
* Create an update mode (read polygon info and update it)
* Add support for multiple polygonal gates on same plot



#### Known Bugs / Limitations
* Points lying on the edge of the axis are invisible to the user and might no be selected
* Plot ticks in the 1d are not always appearing (depends on data range)
* <del>Loading modal dialogue does not start with page load <del>Now just show a GIF and hide the buttons while it waits for the connection to the Shiny server</del>
* <del>The operator currently outputs the input mapping. The JoinOperator needs to be fixed, otherwise it tries to add all flag values to each input entry</del>
* <del>Trying to save the gate before drawing is currently allowed and causes an error</del>
* <del>Axis appearance is different between linear and biexponential plots</del>

#### Deprecated requirements
* Logicle transform is unstable, crashing on certain data. [No more transforms]
* Plot parameters are currently hard-coded and fixed. [No more transforms]