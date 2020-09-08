%% DO NOT EDIT this file manually; it is automatically
%% generated from LSR http://lsr.di.unimi.it
%% Make any changes in LSR itself, or in Documentation/snippets/new/ ,
%% and then run scripts/auxiliar/makelsr.py
%%
%% This file is in the public domain.
\version "2.21.2"

\header {
  lsrtags = "keyboards, tweaks-and-overrides"

  texidoc = "
@code{Staff.pedalSustainStrings} can be used to set the text used for
pedal down and up.  Note that the only valid strings are those found in
the list of pedal glyphs - the values used this snippet constitute an
exhaustive list.

"
  doctitle = "Changing the text for sustain markings"
} % begin verbatim

sustainNotes = { c4\sustainOn d e\sustainOff\sustainOn f\sustainOff }

\relative c' {
  \sustainNotes
  \set Staff.pedalSustainStrings = #'("P" "P-" "-")
  \sustainNotes
  \set Staff.pedalSustainStrings = #'("d" "de" "e")
  \sustainNotes
  \set Staff.pedalSustainStrings = #'("M" "M-" "-")
  \sustainNotes
  \set Staff.pedalSustainStrings = #'("Ped" "*Ped" "*")
  \sustainNotes
}
