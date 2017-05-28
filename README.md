Formulus
========

> “If debugging is the process of removing software bugs,
>  then programming must be the process of putting them
>  in.”  
> –- Edsger Dijkstra

The application is available at:
[https://mpetruska.github.io/formulus/](https://mpetruska.github.io/formulus/)  
(by the courtesy of [GitHub Pages](https://pages.github.com/))

This project is created for the purpose of helping you creating
simple shareable calculations (and not only because the humble
author enjoys working with [PureScript](http://www.purescript.org/)).

Creating calculations
---------------------

<img src="https://raw.githubusercontent.com/mpetruska/formulus/master/screenshots/tutorial_001.jpg" width="500" />

<img src="https://raw.githubusercontent.com/mpetruska/formulus/master/screenshots/tutorial_002.jpg" width="500" />

<img src="https://raw.githubusercontent.com/mpetruska/formulus/master/screenshots/tutorial_003.png" width="500" />

<img src="https://raw.githubusercontent.com/mpetruska/formulus/master/screenshots/tutorial_004.png" width="500" />

<img src="https://raw.githubusercontent.com/mpetruska/formulus/master/screenshots/tutorial_005.jpg" width="500" />

<img src="https://raw.githubusercontent.com/mpetruska/formulus/master/screenshots/tutorial_006.jpg" width="500" />

Only the four basic mathematical operations (and parentheses) are
recognized by the formula parser at the moment.

How to share your calculations
------------------------------

Once your worksheet is ready all the components that make up the
calculation (inputs and calculations) are encoded in the fragment
identifier of the url
(e.g. `#!i:km:km:2500.0,i:litres%20per%20100%20km:c:7.5,c:total%20consumption:total:km*c%2F100.0:1`).
All you need to do is to copy or bookmark the whole url
(e.g. [https://mpetruska.github.io/formulus/#!i:km:km:2500.0,i:litres%20per%20100%20km:c:7.5,c:total%20consumption:total:km*c%2F100.0:1](https://mpetruska.github.io/formulus/#!i:km:km:2500.0,i:litres%20per%20100%20km:c:7.5,c:total%20consumption:total:km*c%2F100.0:1)),
opening the link in a browser
makes the application re-load the worksheet. Be aware that the
current values of the input fields are also encoded.

Feature requests and bugs
-------------------------

Please report the here:
[https://github.com/mpetruska/formulus/issues](https://github.com/mpetruska/formulus/issues)
