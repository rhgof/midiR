
<!-- README.md is generated from README.Rmd. Please edit that file -->

# midiR

<!-- badges: start -->
<!-- badges: end -->

The goal of midiR is to allow midi tracks to be generated from notes
data frames. The aim is allow the sonification of interesting data sets.

For example, turning the sound of global temperature anomaly data into
music.

From treat this as a score to be played ….

![](man/figures/Latest-Stripes-GBR-sst-month.png)

… resulting in the notes to generate this …

<iframe width="100%" height="166" scrolling="no" frameborder="no" allow="autoplay" src="https://w.soundcloud.com/player/?url=https%3A//api.soundcloud.com/tracks/1332712327&amp;color=%234c9784&amp;auto_play=false&amp;hide_related=false&amp;show_comments=true&amp;show_user=true&amp;show_reposts=false&amp;show_teaser=true">
</iframe>

<div
style="font-size: 10px; color: #cccccc;line-break: anywhere;word-break: normal;overflow: hidden;white-space: nowrap;text-overflow: ellipsis; font-family: Interstate,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Garuda,Verdana,Tahoma,sans-serif;font-weight: 100;">

<a href="https://soundcloud.com/citizen-science-2799538" title="Citizen Science" target="_blank" style="color: #cccccc; text-decoration: none;">Citizen
Science</a> ·
<a href="https://soundcloud.com/citizen-science-2799538/reef-anomaly" title="Sea Surface Anomaly" target="_blank" style="color: #cccccc; text-decoration: none;">Sea
Surface Anomaly</a>

</div>

[Fugal
Anomaly](https://soundcloud.com/citizen-science-2799538/reef-anomaly)

## Installation

You can install the development version of midiR like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(midiR)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
