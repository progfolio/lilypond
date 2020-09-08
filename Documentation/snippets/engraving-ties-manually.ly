%% DO NOT EDIT this file manually; it is automatically
%% generated from LSR http://lsr.di.unimi.it
%% Make any changes in LSR itself, or in Documentation/snippets/new/ ,
%% and then run scripts/auxiliar/makelsr.py
%%
%% This file is in the public domain.
\version "2.21.2"

\header {
  lsrtags = "rhythms"

  texidoc = "
Ties may be engraved manually by changing the @code{tie-configuration}
property of the @code{TieColumn} object. The first number indicates the
distance from the center of the staff in half staff-spaces, and the
second number indicates the direction (1 = up, -1 = down).

Note that LilyPond makes a distinction between exact and inexact values
for the first number. If using an exact value (i.e., either an integer
or a fraction like @code{(/ 4 5)}), the value serves as a rough
vertical position that gets further tuned by LilyPond to make the tie
avoid staff lines. If using an inexact value like a floating point
number, it is taken as the vertical position without further
adjustments.

"
  doctitle = "Engraving ties manually"
} % begin verbatim

\relative c' {
  <c e g>2~ <c e g>
  \override TieColumn.tie-configuration =
    #'((0.0 . 1) (-2.0 . 1) (-4.0 . 1))
  <c e g>2~ <c e g>
  \override TieColumn.tie-configuration =
    #'((0 . 1) (-2 . 1) (-4 . 1))
  <c e g>2~ <c e g>
}
