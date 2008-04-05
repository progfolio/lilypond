%% Do not edit this file; it is auto-generated from LSR http://lsr.dsi.unimi.it
%% This file is in the public domain.
\version "2.11.38"

\header {
  lsrtags = "text, vocal-music"
 texidoc = "
You can specify different horizontal alignment for your lyrics, by
overriding the @code{self-alignment-X} property of the @code{LyricText}
object. @code{#-1} is left, @code{#0} is center and @code{#1} is right;
however, you can use @code{#LEFT}, @code{#CENTER} and @code{#RIGHT} as
well. 
" }
% begin verbatim
\relative c'' {
  c1 c c
}
\addlyrics {
  \once \override LyricText #'self-alignment-X = #LEFT "This is left-aligned" 
  \once \override LyricText #'self-alignment-X = #CENTER "This is centered" 
  \once \override LyricText #'self-alignment-X = #1 "This is right-aligned"  
}
