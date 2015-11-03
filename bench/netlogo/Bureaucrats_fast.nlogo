globals [total]
patches-own [n]

to setup
  clear-all
  reset-timer
  ask patches [
    set n 2
    colorize
  ]
  set total 2 * count patches
  reset-ticks
end

to go
  if ticks > 5000 [print timer stop]
  let active-patches patch-set one-of patches
  ask active-patches [
    set n n + 1
    set total total + 1
    colorize
  ]
  while [any? active-patches] [
    let overloaded-patches active-patches with [n > 3]
    ask overloaded-patches [
      set n n - 4
      set total total - 4
      colorize
      ask neighbors4 [
        set n n + 1
        set total total + 1
        colorize
      ]
    ]
    set active-patches patch-set [neighbors4] of overloaded-patches
  ]
  tick
end

to colorize  ;; patch procedure
  ifelse n <= 3
    [ set pcolor item n [83 54 45 25] ]
    [ set pcolor red ]
end
@#$#@#$#@
GRAPHICS-WINDOW
415
10
725
341
-1
-1
3.0
1
10
1
1
1
0
0
0
1
0
99
0
99
1
1
1
ticks
100000.0

BUTTON
10
25
89
58
NIL
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
95
25
173
58
NIL
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

PLOT
10
165
400
340
Average
NIL
NIL
0.0
1.0
2.0
2.1
true
true
"" "if not plot? [ stop ]"
PENS
"average" 1.0 0 -16777216 true "" "plotxy ticks (total / count patches)"

SWITCH
40
65
143
98
plot?
plot?
1
1
-1000

OUTPUT
175
84
400
164
12

@#$#@#$#@
## WHAT IS IT?

This is like Bob's model, but stripped down and speeded up.

Seth Tisue, October 2011
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

@#$#@#$#@
NetLogo 5.0.3
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment1" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="plot?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
1
@#$#@#$#@
