Darwin Harbour Sediment Monitoring analysis tool
==================================================


To build

`docker build . --tag dh_sediment_monitoring`

To test run on local machine

`docker run -it --rm dh_sediment_monitoring`

To compile locally

`make -i`

Continuous integration

Before running the build and deploy workflow, make sure

- goto the organization page
- select `Packages` from the top menu
- select the package from the list
- select `Package settings` from the lower right menu
- In the `Manage Actions access` section, make sure the repository has
  a package role of `write`
- then run the actions
