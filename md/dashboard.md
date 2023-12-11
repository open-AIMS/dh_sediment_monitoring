The analysis pipeline comprises numerous **Stages**, each of which is
made up of several more specific **Tasks**. The individual Tasks
represent an action performed in furtherance of the analysis and of
which there are reportable diagnostics. For example, once the
application loads, the first Stage of the pipeline is to prepare the
environment. The first Task in this Stage is to load the necessary R
packages used by the codebase. Whilst this technically, this action
consists of numerous R calls (one for each package that needs to be
loaded), the block of actions are evaluated as a set.

Initially, all tasks are reported as "pending" (<span class="fas
fa-clock"></span>). As the pipeline progresses, each Task is evaluated
and a status is returned as either "success" (<span class="fas
fa-circle-check"></span>) or "failure" (<span class="fas
fa-circle-xmark"></span>). 

The Stage that is currently (or most recently) being run will be
expanded, whereas all other Stages will be collapsed (unless they
contain errors). It is also possible to expand/collapse a Stage by
double clicking on its title (or the small arrow symbol at the left
side of the tree).

As the pipeline progresses, Task logs are written to a log_file and
echoed to the **Logs** panel. Each row represents the returned status
of a specific Task and are formatted as:

- the time/date that the Task was evaluated
- the Task status, which can be one of:
  - `SUCCESS` the task succeeded
  - `FAILURE` the task failed and should be investigated
  - `WARNING` the task contained a warning - typically these can be
    ignored as they are usually passed on from underlying routines and
    are more targetted to developers than users.
- the Stage followed by the Task name
- in the case of errors and warnings, there will also be the error or
  warning message passed on from the underlying routines. These can be
  useful for helping to diagnose the source and cause of issues.
  
The Logs in the Log panel are presented in chronological order and
will autoscroll such that the most recent log is at the bottom of th e
display. If the number of Log lines exceeds 10, a scroll bar will
appear on the right side of the panel to help reviewing earlier Logs.

<div class="callout call-info"><h4>Note</h4> 
The Status and Logs are
completely refreshed each time the application is restarted. 
</div>

The Progress panel also has a tab (called **Terminal-like**) also has
an alternative representation of the status and progress of the
pipeline.
