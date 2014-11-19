:- module(
  gv_html,
  [
    gv_html_like_label//1 % +Content:compound
  ]
).

/** <module> GraphViz: HTML-like labels

Grammar taken from the GraphViz Web site:

```
label :   text
        | table
text :   textitem
       | text textitem
textitem :   string
           | <BR/>
           | <FONT> text </FONT>
           | <I> text </I>
           | <B> text </B>
           | <U> text </U>
           | <O> text </O>
           | <SUB> text </SUB>
           | <SUP> text </SUP>
           | <S> text </S>
table : [ <FONT> ] <TABLE> rows </TABLE> [ </FONT> ]
rows :   row
       | rows row
       | rows <HR/> row
row: <TR> cells </TR>
cells :   cell
        | cells cell
        | cells <VR/> cell
cell:   <TD> label </TD>
      | <TD> <IMG/> </TD>
```

@author Wouter Beek
@see http://www.graphviz.org/content/node-shapes#html
@version 2013/07, 2013/09, 2014/03-2014/06, 2014/11
*/

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_bracket)).
:- use_module(plDcg(dcg_content)).

:- use_module(plHtml(html_dcg)).



%! gv_html_like_label(?Content:compound)// .

gv_html_like_label(Content) -->
  bracketed(angular, label(Content)).


%! cell(?Contents:compound)// .
% Supported attributes for TD:
% ```
% ALIGN="CENTER|LEFT|RIGHT|TEXT"
% BALIGN="CENTER|LEFT|RIGHT"
% BGCOLOR="color"
% BORDER="value"
% CELLPADDING="value"
% CELLSPACING="value"
% COLOR="color"
% COLSPAN="value"
% FIXEDSIZE="FALSE|TRUE"
% GRADIENTANGLE="value"
% HEIGHT="value"
% HREF="value"
% ID="value"
% PORT="portName"
% ROWSPAN="value"
% SIDES="value"
% STYLE="value"
% TARGET="value"
% TITLE="value"
% TOOLTIP="value"
% VALIGN="MIDDLE|BOTTOM|TOP"
% WIDTH="value"
% ```
%
% Supported attributes for IMG:
% ```
% SCALE="FALSE|TRUE|WIDTH|HEIGHT|BOTH"
% SRC="value"
% ```

cell(td(Contents)) -->
  html_element(td, [], label(Contents)).
cell(td(img)) -->
  html_element(td, [], html_element(img)).


%! cells(?Contents:list(compound))// .

cells([H|T]) -->
  cell(H),
  cells(T).
cells([H,vr|T]) -->
  cell(H),
  html_element(vr),
  cells(T).
cells([H]) -->
  cell(H).


%! label(?Content:compound)// .
% GraphViz HTML-like label.

label(Content) -->
  text(Content).
label(Content) -->
  table(Content).


%! row(?Contents:compound)// .

row(tr(Contents)) -->
  html_element(tr, [], cell(Contents)).


%! rows(?Contents:list)// .

rows([H|T]) -->
  row(H),
  rows(T).
rows([H,hr|T]) -->
  row(H),
  html_element(hr),
  rows(T).
rows([H]) -->
  row(H).


%! table(?Contents:compound)// .
% Supported attributes for TABLE:
% ```
% ALIGN="CENTER|LEFT|RIGHT"
% BGCOLOR="color"
% BORDER="value"
% CELLBORDER="value"
% CELLPADDING="value"
% CELLSPACING="value"
% COLOR="color"
% COLUMNS="value"
% FIXEDSIZE="FALSE|TRUE"
% GRADIENTANGLE="value"
% HEIGHT="value"
% HREF="value"
% ID="value"
% PORT="portName"
% ROWS="value"
% SIDES="value"
% STYLE="value"
% TARGET="value"
% TITLE="value"
% TOOLTIP="value"
% VALIGN="MIDDLE|BOTTOM|TOP"
% WIDTH="value"
% ```
%
% Supported attributes for FONT:
% ```
% COLOR="color"
% FACE="fontname"
% POINT-SIZE="value"
% ```

table(table(Attrs,Contents)) -->
  html_element(table, Attrs, rows(Contents)).
table(table(font(Contents))) -->
  html_element(font, [], html_element(table, [], rows(Contents))).


%! text(?Contents:list)// .

text(Contents) -->
  '+'(textitem, Contents, []).


%! textitem(?Content)// .
% Supported attributes for BR:
% ```
% ALIGN="CENTER|LEFT|RIGHT"
% ```

textitem(string(String)) -->
  html_string(String).
textitem(entity(Name)) -->
  html_entity(Name).
textitem(br(Attrs)) -->
  html_element(br, Attrs).
textitem(Compound) -->
  {var(Compound)}, !,
  html_element(Name, _, text(Content)),
  {
    supported_html_element(Name),
    Compound =.. [Name,Content]
  }.
textitem(Compound) -->
  {
    Compound =.. [Name,Content],
    supported_html_element(Name)
  },
  html_element(Name, _, text(Content)).



% HELPERS

%! supported_html_element(+Name:atom) is semidet.
%! supported_html_element(-Name:atom) is multi.

supported_html_element(b).
supported_html_element(font).
supported_html_element(i).
supported_html_element(o).
supported_html_element(s).
supported_html_element(sub).
supported_html_element(sup).
supported_html_element(u).
