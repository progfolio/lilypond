%% Do not edit this file; it is auto-generated from LSR http://lsr.dsi.unimi.it
%% This file is in the public domain.
\version "2.11.38"

\header {
  lsrtags = "expressive-marks"
 texidoc = "
To make the \\flageolet circle smaller you can use the following scheme
code (found in the Lilypond-user-archive)
" }
% begin verbatim
F = #(let ((m (make-music 'ArticulationEvent
                          'articulation-type "flageolet")))
       (set! (ly:music-property m 'tweaks)
             (acons 'font-size -3
                    (ly:music-property m 'tweaks)))
       m)

\relative c'' { d4^\flageolet_\markup {"orginal \flageolet "} d4_\flageolet
  c4^\F_\markup {smaller } c4_\F 
}
