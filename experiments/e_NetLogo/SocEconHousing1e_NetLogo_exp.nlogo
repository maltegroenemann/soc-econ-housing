globals [
  reporter ; to extract data from all patches in the simulations
]

turtles-own [ ; turtles are renters
  income ; fixed, in the article: w_i
  status ; fixed, in the article: s_i
]

patches-own [ ; patches are landlords
  rent ; variable: rent of the location, in the article: p(x)
  quality; variable: housing characteristics, in the article: q(x)

  ; variables as in-between steps for the utility calculation of turtles
  status-nb; variable: mean status of the renters in moore nb, in the article: sum of S_j / n_j
  utility-here; variable: utility of the given location

  ; variables for reporting
  turtle-here?  ; variable: is there currently a turtle here or is the plot empty?
  income-turtle ; variable: income of the current turtle here
  status-turtle ; variable: status of the current turtle here
]

to setup
  clear-all
  ; set up patches.
  ask patches [
    let X random-gamma distribution 4
    let Y random-gamma (2.5 * distribution) 4
    set quality X / (X + Y) ; Beta distributed
    set utility-here quality
    set rent quality
    set pcolor quality * 10 + 22

    ; set up turtles on random patches
    if random 100 <= 85 [   ; set the occupancy density, here ~85%
      sprout 1 [
        let X1 random-gamma distribution 4
        let Y1 random-gamma (2.5 * distribution) 4
        set income X1 / (X1 + Y1) ; Beta distributed

        let X2 random-gamma distribution 4
        let Y2 random-gamma (2.5 * distribution) 4
        let partial-status X2 / (X2 + Y2) ; Beta distributed
        set status partial-status * (1 - r_correlation) + income * r_correlation

        set shape "circle"
        set size income + 0.4 ; size is proportional to income
        set color status * 10 + 2 ; status is represented by a greyscale. the brighter, the higher the status.
      ]
    ]
  ]
  supply
  reporting
  reset-ticks
end

; run the model for one tick
to go
  if ticks = 500 [ stop ]
  popdyn
  demand
  supply
  reporting
  tick
end

to supply
  ask patches [
    let invest-threshold mean [rent] of neighbors
    if quality <= invest-threshold [ ; quality reflects mean nb rent at t-1, quality rises if mean nb rent rises
        set quality invest-threshold
      ]
      if quality > invest-threshold [
        set quality d_decay * quality ; arbitrary choice
      ]
      set pcolor quality * 10 + 22

    if any? turtles-on neighbors [
      set status-nb mean [status] of turtles-on neighbors
      set utility-here status-nb ^ a_utility * quality ^ (1 - a_utility)
    ]
    if not any? turtles-on neighbors [set utility-here 0]

    let own-utility utility-here
    let competition [income-turtle] of patches with [utility-here <= own-utility]
    let med median competition
    let upper filter [ x -> x >= med ] competition
    set rent median upper ; rent is third quartile of incomes of turtles living on patches with lower utility

    if any? turtles-here [
      set turtle-here? TRUE
      set income-turtle mean [income] of turtles-here
      set status-turtle mean [status] of turtles-here
    ]

    if not any? turtles-here [
      set turtle-here? FALSE
    ]
  ]
end

to demand
  ask turtles [
    ; the current patch is available to the turtle as well, staying is an option: patch-here is included
    let empty (patch-set patches with [not any? turtles-on self] patch-here)
    let affordable empty with [rent <= [income] of myself]
    ; if free affordable locations, move to best if utility is higher or current rent is too high
    if any? affordable [
      move-to one-of affordable with-max [utility-here]
    ]
    ; if no free affordable locations but current rent too high, move to cheapest free location.
    if not any? affordable and [rent] of patch-here > income [
      move-to one-of empty with-min [rent]
    ]
  ]
end

to popdyn
  ask n-of (turnover * count patches) turtles [
    die
  ]

  ask n-of (turnover * count patches) patches with [not any? turtles-on self] [
    sprout 1 [
        let X1 random-gamma distribution 4
        let Y1 random-gamma (2.5 * distribution) 4
        set income X1 / (X1 + Y1) ; Beta distributed

        let X2 random-gamma distribution 4
        let Y2 random-gamma (2.5 * distribution) 4
        let partial-status X2 / (X2 + Y2) ; Beta distributed
        set status partial-status * (1 - r_correlation) + income * r_correlation

        set shape "circle"
        set size income + 0.4 ; size is proportional to income
        set color status * 10 + 2 ; status is represented by a greyscale. the brighter, the higher the status.
      ]
  ]

end

to reporting
  set reporter [(list pxcor pycor quality rent ([who] of turtles-here) income-turtle status-turtle)] of patches
end
@#$#@#$#@
GRAPHICS-WINDOW
300
25
758
484
-1
-1
15.0
1
10
1
1
1
0
1
1
1
0
29
0
29
1
1
1
ticks
30.0

SLIDER
10
95
285
128
r_correlation
r_correlation
0
1
0.5
0.1
1
NIL
HORIZONTAL

BUTTON
10
55
90
88
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
200
55
285
88
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
100
55
190
88
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
10
135
285
168
a_utility
a_utility
0
1
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
15
270
290
303
turnover
turnover
0
0.1
0.0
0.01
1
NIL
HORIZONTAL

CHOOSER
15
315
290
360
distribution
distribution
1 2 3 4 5
1

SLIDER
10
175
285
208
d_decay
d_decay
0.5
1
0.7
0.05
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?



## CREDITS AND REFERENCES

Benard, Stephen and Rob Willer (2007): A Wealth and Status-based Model of Residential Segregation. Journal of Mathematical Sociology 31(2) pp. 149-174 

Schelling, T. (1978). Micromotives and Macrobehavior. New York: Norton.

Wilensky, U. (1997).  NetLogo Segregation model.  http://ccl.northwestern.edu/netlogo/models/Segregation.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## HOW TO CITE

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 2021

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

face-happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

person2
false
0
Circle -7500403 true true 105 0 90
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 285 180 255 210 165 105
Polygon -7500403 true true 105 90 15 180 60 195 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square - happy
false
0
Rectangle -7500403 true true 30 30 270 270
Polygon -16777216 false false 75 195 105 240 180 240 210 195 75 195

square - unhappy
false
0
Rectangle -7500403 true true 30 30 270 270
Polygon -16777216 false false 60 225 105 180 195 180 240 225 75 225

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

square-small
false
0
Rectangle -7500403 true true 45 45 255 255

square-x
false
0
Rectangle -7500403 true true 30 30 270 270
Line -16777216 false 75 90 210 210
Line -16777216 false 210 90 75 210

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 0 0 0 300 300 300 30 30

triangle2
false
0
Polygon -7500403 true true 150 0 0 300 300 300

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 300 60 225 0 0 225 60 300
Polygon -7500403 true true 0 60 75 0 300 240 225 300
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>reporter</metric>
    <enumeratedValueSet variable="r_correlation">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="a_utility">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="d_decay">
      <value value="0.95"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turnover">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
