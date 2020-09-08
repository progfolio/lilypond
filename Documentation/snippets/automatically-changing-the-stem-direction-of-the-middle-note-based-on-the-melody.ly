%% DO NOT EDIT this file manually; it is automatically
%% generated from LSR http://lsr.di.unimi.it
%% Make any changes in LSR itself, or in Documentation/snippets/new/ ,
%% and then run scripts/auxiliar/makelsr.py
%%
%% This file is in the public domain.
\version "2.21.2"

\header {
  lsrtags = "contexts-and-engravers, pitches"

  texidoc = "
LilyPond can alter the stem direction of the middle note on a staff so
that it follows the melody, by adding the @code{Melody_engraver} to the
Voice context and overriding the @code{neutral-direction} of Stem.

"
  doctitle = "Automatically changing the stem direction of the middle note based on the melody"
} % begin verbatim

\relative c'' {
  \time 3/4
  a8 b g f b g |
  c  b d c b c |
}

\layout {
  \context {
    \Voice
    \consists "Melody_engraver"
    \autoBeamOff
    \override Stem.neutral-direction = #'()
  }
}
