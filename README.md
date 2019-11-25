# mh-injury


Builds the injury module, using data from Stats19 and distances from mh-execute/inputs/distances (processed in mh-distance/process_distances_for_execute.R)

`add_distance_to_injury.R` builds the injury dataset, if not build already, by calling `build_injury_data_from_stats19.R` and combines it with distances data

It then builds the injury glm models, which it saves to mh-execute/inputs/injury.

Model building is computationally intensive, and can be done on a cluster using `slurm_script.injury`.

