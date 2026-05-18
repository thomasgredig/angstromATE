# Resubmission / Update

This is an update to angstromATE version 0.1.3.

In this version:

* added support for reading Angstrom Engineering `.rcp` recipe files;
* added `ATE.readRecipe()` to extract recipe source name, pre-vacuum pressure,
  substrate temperature, deposition rate, deposition time, and target thickness;
* updated documentation and examples;
* fixed minor issues identified during package checks.

## Test environments

* local R installation, 4.5.3, Mac OS
* win-builder, R Under development (unstable) (2026-05-15 r90061 ucrt)


## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE

  Maintainer: 'Thomas Gredig <tgredig@csulb.edu>'

## Reverse dependencies

There are no reverse dependencies.


## Additional comments

This update does not change the existing `ATE.import()` interface.


---

# Original Submission

## Test environments

* local R installation, R 4.4.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.


