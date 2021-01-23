\version "2.19.21"

\header{
  texidoc="
Volta (Semi folded) behavior.  Voltas can start on non-barline moments.
If they don't barlines should still be shown.
"
}

#(ly:set-option 'warning-as-error #t)
%% One warning for the \repeat and one for the \alternative.
#(ly:expect-warning (_ "More alternatives than repeats.  Junking excess alternatives"))
#(ly:expect-warning (_ "More alternatives than repeats.  Junking excess alternatives"))

\context Voice \relative {
  %% repeat non aligning with barlines.
  \repeat volta 3 { c''^"3x 0alt" d e }
  %% less alts than body
  \repeat volta 4 { c^"4x 2alt" d } \alternative { e f }
  %% more alts than body
  \repeat volta 2 { c^"2x 3alt" d } \alternative { e f g }
}
