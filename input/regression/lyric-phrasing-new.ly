\version "2.1.9"
\header {

texidoc = "Lyric phrasing:

  Normally, the lyric is centered on the note head. However, on
  melismata, the text is left aligned on the left-side of the note head.

"
}


\score{
<<	\notes \relative c' \context Voice = "bla" {
	    \autoBeamOff
	    c4( c16 d c b)  c4
	    d16[ e f g]
	    
	}
	\lyrics  \newaddlyrics  "bla" \context LyricsVoice {
	    al tijd
	    izzz
	} >>
	
    \paper { raggedright = ##t }
}
