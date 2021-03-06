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

Operations:
- negation (`-` prefix)
- mathematical base operators (`+`, `-`, `*`, `/`)
- "min" and "max" as binary functions (form: `min(x, y)` and `max(x, y)`)

How to share your calculations
------------------------------

Once your worksheet is ready all the components that make up the
calculation (inputs and calculations) are encoded in the fragment
identifier of the url (example 1 below).
All you need to do is to copy or bookmark the whole url (example 2 below),
opening the link in a browser makes the application re-load the worksheet.
Be aware that the current values of the input fields are also encoded.

^ example 1:
`#!i:km:km:2500.0,i:litres%20per%20100%20km:c:7.5,c:total%20consumption:total:km*c%2F100.0:1`
^ example 2:
[https://mpetruska.github.io/formulus/#!i:km:km:2500.0,i:litres%20per%20100%20km:c:7.5,c:total%20consumption:total:km*c%2F100.0:1](https://mpetruska.github.io/formulus/#!i:km:km:2500.0,i:litres%20per%20100%20km:c:7.5,c:total%20consumption:total:km*c%2F100.0:1)

_Bonus_: if you want to share a non-editable version of the calculation
(e.g. to allow only input values to be modified) add an "l" (lowercase L)
character between the hash mark (#) and exclamation mark (!) in the
fragment identifier.

example:
[https://mpetruska.github.io/formulus/#l!i:km:km:2500.0,i:litres%20per%20100%20km:c:7.5,c:total%20consumption:total:km*c%2F100.0:1](https://mpetruska.github.io/formulus/#l!i:km:km:2500.0,i:litres%20per%20100%20km:c:7.5,c:total%20consumption:total:km*c%2F100.0:1)

Feature requests and bugs
-------------------------

Please report the here:
[https://github.com/mpetruska/formulus/issues](https://github.com/mpetruska/formulus/issues)
