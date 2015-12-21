\version "2.19.22"

\header {
  lsrtags = "contemporary-notation, winds"

  texidoc = "
It is possible to indicate special articulation techniques such as a
flute @qq{tongue slap} by replacing the note head with the appropriate
glyph.

"
  doctitle = "Flute slap notation"
}
slap =
#(define-music-function (music) (ly:music?)
#{
  \temporary \override NoteHead.stencil =
  #(lambda (grob)
     (grob-interpret-markup grob
      (markup #:musicglyph "scripts.sforzato")))
  \temporary \override NoteHead.stem-attachment =
  #(lambda (grob)
     (let* ((thickness (ly:staff-symbol-line-thickness grob))
            (stem (ly:grob-object grob 'stem))
            (dir (ly:grob-property stem 'direction UP)))
       (cons 1 (+ (if (= dir DOWN)
                      0.5
                      0)
                  (/ thickness 2)))))
  #music
  \revert NoteHead.stencil
  \revert NoteHead.stem-attachment
#})

\relative c' {
  c4 \slap c d r
  \slap { g4 a } b r
}
