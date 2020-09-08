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
When notes are placed on ledger lines, their beams are usually centred
on the stave.  Grace notes beams are shorter and grace notes on ledger
lines may well have beams outside the stave.  You can override this
beaming for grace notes.

"
  doctitle = "Positioning grace note beams at the height of normal note beams"
} % begin verbatim

\relative c {
  f8[ e]
  \grace {
    f8[ e]
    \override Stem.no-stem-extend = ##f
    f8[ e]
    \revert Stem.no-stem-extend
  }
  f8[ e]
}
