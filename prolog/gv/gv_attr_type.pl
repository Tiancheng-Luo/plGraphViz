:- module(
  gv_attr_type,
  [
    gv_attr_type//1, % ?Type:atom
    addDouble//1, % +Double:float
    addPoint//1, % +Point:compound
    arrowType//1, % +ArrowType:atom
    bool//1, % +Boolean:boolean
    clusterMode//1, % +ClusterMode:atom
    dirType//1, % +DirectionType:oneof([back,both,forward,none])
    double//1, % +Double:float
    doubleList//1, % +Doubles:list(float)
    escString//1,
    %layerList//1,
    %layerRange//1,
    lblString//1,
    int//1, % +Integer:integer
    outputMode//1, % +OutputMode:atom
    %packMode//1,
    pagedir//1, % +Pagedir:atom
    point//1, % +Point:compound
    pointList//1, % +Points:list(compound)
    %portPos//1,
    quadType//1, % +QuadType:atom
    rankType//1, % +RankType:atom
    rankdir//1, % +RankDirection:atom
    rect//1, % +Rectangle:compound
    shape//1,
    smoothType//1, % +SmoothType:atom
    %splineType//1,
    %startType//1,
    string//1, % ?Content:atom
    style//2 % +Context:oneof([cluster,edge,node])
             % +Style:atom
    %viewPort//1
  ]
).
:- reexport(
  library(gv/gv_color),
  [
    color//1, % +Color:compound
    colorList//1 % +ColorList:list(compound)
  ]
).

/** <module> GraphViz attribute types

@author Wouter Beek
@version 2015/07, 2015/11
*/

:- use_module(library(dcg/basics), except([string//1])).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(gv/gv_html)).





%! gv_attr_type(?Type:atom) is nondet.

gv_attr_type(addDouble)   --> "addDouble".
gv_attr_type(addPoint)    --> "addPoint".
gv_attr_type(arrowType)   --> "arrowType".
gv_attr_type(bool)        --> "bool".
gv_attr_type(color)       --> "color".
gv_attr_type(colorList)   --> "colorList".
gv_attr_type(clusterMode) --> "clusterMode".
gv_attr_type(dirType)     --> "dirType".
gv_attr_type(double)      --> "double".
gv_attr_type(doubleList)  --> "doubleList".
gv_attr_type(escString)   --> "escString".
gv_attr_type(layerList)   --> "layerList".
gv_attr_type(layerRange)  --> "layerRange".
gv_attr_type(lblString)   --> "lblString".
gv_attr_type(int)         --> "int".
gv_attr_type(outputMode)  --> "outputMode".
gv_attr_type(packMode)    --> "packMode".
gv_attr_type(pagedir)     --> "pagedir".
gv_attr_type(point)       --> "point".
gv_attr_type(pointList)   --> "pointList".
gv_attr_type(portPos)     --> "portPos".
gv_attr_type(quadType)    --> "quadType".
gv_attr_type(rankType)    --> "rankType".
gv_attr_type(rankdir)     --> "rankdir".
gv_attr_type(rect)        --> "rect".
gv_attr_type(shape)       --> "shape".
gv_attr_type(smoothType)  --> "smoothType".
gv_attr_type(splineType)  --> "splineType".
gv_attr_type(startType)   --> "startType".
gv_attr_type(string)      --> "string".
gv_attr_type(style)       --> "style".
gv_attr_type(viewPort)    --> "viewPort".



%! addDouble(+Float:float)// .
% An *addDouble* is represented by a Prolog float.

addDouble(N) --> ("+", ! ; ""), float(N).



%! addPoint(+Point:compound)// .
% An *addPoint* is represented by a compound of the following form:
% `point(X:float,Y:float,InputOnly:boolean)`.

addPoint(Point) --> ("+", ! ; ""), point(Point).



%! arrowType(+ArrowType:atom)// .

arrowType(V) --> primitive_shape(V).
arrowType(V) --> derived(V).
arrowType(V) --> backwards_compatible(V).

primitive_shape(box)     --> "box".
primitive_shape(crow)    --> "crow".
primitive_shape(circle)  --> "circle".
primitive_shape(diamond) --> "diamond".
primitive_shape(dot)     --> "dot".
primitive_shape(inv)     --> "inv".
primitive_shape(none)    --> "none".
primitive_shape(normal)  --> "normal".
primitive_shape(tee)     --> "tee".
primitive_shape(vee)     --> "vee".

derived(odot)     --> "odot".
derived(invdot)   --> "invdot".
derived(invodot)  --> "invodot".
derived(obox)     --> "obox".
derived(odiamond) --> "odiamond".

backwards_compatible(ediamond) --> "ediamond".
backwards_compatible(empty)    --> "empty".
backwards_compatible(halfopen) --> "halfopen".
backwards_compatible(invempty) --> "invempty".
backwards_compatible(open)     --> "open".



%! bool(+Value:boolean)// .

bool(false) --> "false".
bool(false) --> "no".
bool(true)  --> "true".
bool(true)  --> "yes".



%! clusterMode(+ClusterMode:atom)// .

clusterMode(global) --> "global".
clusterMode(local) --> "local".
clusterMode(none) --> "none".



%! dirType(+DirectionType:oneof([back,both,forward,none]))// .

dirType(back) --> "back".
dirType(both) --> "both".
dirType(forward) --> "forward".
dirType(none) --> "none".



%! double(+Double:float)// .

double(N) --> float(N).



%! doubleList(+Doubles:list(float))// .

doubleList([H|T]) --> double(H), (":", !, doubleList(T) ; {T = []}).



%! escString(+String:or([atom,string]))// .
% @tbd Support for context-dependent replacements.

escString(S1) -->
  {(  string(S1)
  ->  string_phrase(escape_double_quotes, S1, S2)
  ;   atom_phrase(escape_double_quotes, S1, S2)
  )},
  "\"", atom(S2), "\"".

escape_double_quotes, [0'\\,0'"] --> [0'"], !, escape_double_quotes.
escape_double_quotes, [X]        --> [X],   !, escape_double_quotes.
escape_double_quotes             --> "".



% @tbd layerList



% @tbd layerRange



%! lblString(+String:compound)// .

lblString(html_like_label(V)) --> gv_html_like_label(V).
lblString(V) --> escString(V).



%! int(+Integer:integer)// .

int(V) --> integer(V).



%! outputMode(+OutputMode:atom)// .

outputMode(breadthfirst) --> "breadthfirst".
outputMode(edgesfirst)   --> "edgesfirst".
outputMode(nodesfirst)   --> "nodesfirst".



% @tbd packMode



%! pagedir(+PageDirection:atom)// .

pagedir('BL') --> "BL".
pagedir('BR') --> "BR".
pagedir('LB') --> "LB".
pagedir('LT') --> "LT".
pagedir('RB') --> "RB".
pagedir('RT') --> "RT".
pagedir('TL') --> "TL".
pagedir('TR') --> "TR".



%! point(+Point:compound)// .
% A *point* is represented by a compound of the following form:
% `point(X:float,Y:float,Changeable:boolean)`.

point(point(X,Y,Changeable)) -->
  float(X), ",", float(Y),
  input_changeable(Changeable).

input_changeable(false) --> "".
input_changeable(true) --> "!".



%! pointList(+Points:list(compound))// .

pointList(Points) -->
  *(point, Points, []).



% @tbd portPos



%! quadType(+QuadType:atom)// .

quadType(fast) --> "fast".
quadType(none) --> "none".
quadType(normal) --> "normal".



%! rankType(+RankType:atom)// .

rankType(max) --> "max".
rankType(min) --> "min".
rankType(same) --> "same".
rankType(sink) --> "sink".
rankType(source) --> "source".



%! rankdir(+RankDirection:oneof(['BT','LR','RL','TB']))// .

rankdir('BT') --> "BT".
rankdir('LR') --> "LR".
rankdir('RL') --> "RL".
rankdir('TB') --> "TB".



%! rect(+Rectangle:compound)// .

rect(rect(LowerLeftX,LowerLeftY,UpperRightX,UpperRightY)) -->
  float(LowerLeftX), ",",
  float(LowerLeftY), ",",
  float(UpperRightX), ",",
  float(UpperRightY).



%! shape(+Shape:atom)// .

shape(V) --> {polygon_based_shape(V)}, atom(V).

polygon_based_shape(assembly).
polygon_based_shape(box).
polygon_based_shape(box3d).
polygon_based_shape(cds).
polygon_based_shape(circle).
polygon_based_shape(component).
polygon_based_shape(diamond).
polygon_based_shape(doublecircle).
polygon_based_shape(doubleoctagon).
polygon_based_shape(egg).
polygon_based_shape(ellipse).
polygon_based_shape(fivepoverhang).
polygon_based_shape(folder).
polygon_based_shape(hexagon).
polygon_based_shape(house).
polygon_based_shape(insulator).
polygon_based_shape(invhouse).
polygon_based_shape(invtrapezium).
polygon_based_shape(invtriangle).
polygon_based_shape(larrow).
polygon_based_shape(lpromoter).
polygon_based_shape('Mcircle').
polygon_based_shape('Mdiamond').
polygon_based_shape('Msquare').
polygon_based_shape(none).
polygon_based_shape(note).
polygon_based_shape(noverhang).
polygon_based_shape(octagon).
polygon_based_shape(oval).
polygon_based_shape(parallelogram).
polygon_based_shape(pentagon).
polygon_based_shape(plaintext).
polygon_based_shape(point).
polygon_based_shape(polygon).
polygon_based_shape(primersite).
polygon_based_shape(promoter).
polygon_based_shape(proteasesite).
polygon_based_shape(proteinstab).
polygon_based_shape(rarrow).
polygon_based_shape(rect).
polygon_based_shape(rectangle).
polygon_based_shape(restrictionsite).
polygon_based_shape(ribosite).
polygon_based_shape(rnastab).
polygon_based_shape(rpromoter).
polygon_based_shape(septagon).
polygon_based_shape(signature).
polygon_based_shape(square).
polygon_based_shape(tab).
polygon_based_shape(terminator).
polygon_based_shape(threepoverhang).
polygon_based_shape(trapezium).
polygon_based_shape(triangle).
polygon_based_shape(tripleoctagon).
polygon_based_shape(utr).



%! smoothType(+SmoothType:atom)// .

smoothType(V) --> {smoothType(V)}, atom(V).

smoothType(avg_dist).
smoothType(graph_dist).
smoothType(none).
smoothType(power_dist).
smoothType(rng).
smoothType(spring).
smoothType(triangle).



% @tbd splineType



% @tbd startType



%! string(?String:atom)// .
% A GraphViz string.

string(Content) --> "\"", atom(Content), "\"".



%! style(?Context:oneof([cluster,edge,node]), ?Style:atom) is nondet.

style(Context, Style) --> {style(Context, Style)}, atom(Style).

style(cluster, bold).
style(cluster, dashed).
style(cluster, dotted).
style(cluster, filled).
style(cluster, rounded).
style(cluster, solid).
style(cluster, striped).
style(edge, bold).
style(edge, dashed).
style(edge, dotted).
style(edge, solid).
style(node, bold).
style(node, dashed).
style(node, diagonals).
style(node, dotted).
style(node, filled).
style(node, rounded).
style(node, solid).
style(node, striped).
style(node, wedged).



% @tbd viewPort
