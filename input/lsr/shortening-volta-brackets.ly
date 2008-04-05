%% Do not edit this file; it is auto-generated from LSR http://lsr.dsi.unimi.it
%% This file is in the public domain.
\version "2.11.38"

\header {
  lsrtags = "repeats"
 texidoc = "
By default, the volta brackets will be drawn over all of the
alternative music, but it is possible to shorten them by setting
@code{voltaSpannerDuration}.  In the next example, the bracket only
lasts one measure, which is a duration of 3/4. 
" }
% begin verbatim
\relative c'' {
  \time 3/4
  c c c
  \set Score.voltaSpannerDuration = #(ly:make-moment 3 4)
  \repeat volta 5 { d d d }
  \alternative {
    { e e e f f f }
    { g g g }
  }
}
