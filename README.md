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
- In the `Inherit access` section, make sure the `Inherit access from source repository (recommended)` checkbox is checked
- In the `Danger Zone` section, change the visibility of the package
  to `public` - if this is not enabled, then you will need to make
  sure public package visibility is enabled for the organization.
- then run the actions
