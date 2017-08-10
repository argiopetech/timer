![Image of a basic split file with one personal-best split](https://github.com/argiopetech/timer/raw/master/screenshot/test.png)

# Timer

A statistics-enabled splits-based timer application for NCurses.

## Rationale

Split-based timing is encountered in a variety of fields. This specific application is designed for and tested with speedrunning.

Where most split timers are primarily focused on personal-best times and their associated metrics (sum of bests, possible time saves, etc), this timer focuses on consistency. It saves all splits (including invalid splits, see below) and outputs them to an R (programming language) data file which is read by [STAN](http://mc-stan.org/). STAN fits a (admittedly simple) empirical Bayesian model to the data and updates the splits.yaml file.

## Status
Timer is alpha software. It does not have a stable API, input or output format, or even statistical model. It probably won't `rm -rf /`, but I won't guarantee anything.

Having said this, I use timer almost daily for speedrunning. My splits.yaml file will be available for reference as soon as I finalize it.

## Use

### `timer`

The core of the system. Reads a `splits.yaml` file in the current directory and saves to `splits.dat`.

#### Keys that change use when the timer is running

| Key | Timer Stopped | Timer Running |
|-----|---------------|---------------|
|`SPACE` | Starts the timer   | Moves to the next split|
|`RETURN`| Starts the timer   | Pauses the timer on the current split |
| `r`    | Resets the current run | Nothing |
| `s`    | Saves the current splits to a file and resets | Nothing |

##### Other keys

| Key  | Use |
|------|-----|
| `q`    | Quits the application without saving
| `i`    | Toggles between valid/invalid for the current split
| `PgDn` | Skips the current split, invalidating both it and the split following
| `PgUp` | Moves the current split to the previous split, adding the current time to the previous split's time (see also "Invalid splits") |

#### Saving Splits
In a complete run (all splits have a time), all splits (valid and invalid) are saved.

If not all splits are complete, the last split is not recorded. The rationale for this is that if the run was not completed, the last split was probably stopped prior to its completion (making it invalid). 

#### Invalid splits
Splits marked invalid are saved in the data file for the purposes of calculating personal best "full run" times. Invalid splits are not exported to STAN.

When an invalid split (the "current" split) is combined with the previous split using `PgUp` it validates the current split. Additionally, if the split previous to the previous split (the "grandsplit") is valid, it validates the previous split. This behavior is desirable to account for accidental uses of the skip functionality. If the previous split is actually invalid, it can be marked as such with the `i` key. 

### `level-bests`
Outputs the name and best time for each split.

### `total-time`
Outputs the total time for all splits and all runs.

### `output-split-timing`
Outputs all valid split data for each split in a format compatible with `tools/unisplits.stan`.

### `update-split-timing`
Reads a `stansummary` CSV file and updates a `splits.yaml` file based on the contents.
