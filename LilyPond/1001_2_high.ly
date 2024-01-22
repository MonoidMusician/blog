% https://lilypond.org/doc/v2.24/Documentation/notation/inside-the-staff#fingering-instructions
% https://lilypond.org/doc/v2.24/Documentation/notation/inside-the-staff#gliding-fingers

lineTop = \relative {
  \override Fingering.add-stem-support = ##t
  \override FingerGlideSpanner.style = #'stub-right
  \stemDown r8 d''-3[ d d] d[ c16-2 bes-1] c8-2[ a-\thumb] | % 14
  \stemUp bes8-1 r8 es8-4 r8 a,8-\thumb r8 d4-3~ | % 15
  d8\glide-3 c-3[ c c-3] c[ bes16-1 a-\thumb] bes8[ g] | % 16
  a8-\thumb r8 d8-3 r8 g,8-\thumb r8 c4-3~ | % 17
  c8\glide-3 bes-3[ bes bes-3] bes[ a16-1 g-\thumb] a8[ bes16 a] | % 18
  g8-\thumb r8 c8 r8 c8[ bes16-2 a\glide-1] bes8-1[ c16 bes] | % 19
  a8-\thumb r8 d8 r8 d8 r8 | % 20
}

lineBottom = \relative {
  \override Fingering.add-stem-support = ##t
  \override FingerGlideSpanner.style = #'stub-right
  s8 s8 s2. | % 14
  \stemDown r8 g'8-3[ g g] g[ f16-2 es-1] f8-2[ d-\thumb] | % 15
  es8\glide-1[ es16-1 d-\thumb] es8[ f16-2 es-1] d8-\thumb r8 g8 r8 | % 16
  g8-3 f[ f f] f[ es16 d] es8[ c] | % 17
  d8\glide-1[ d16-1 c] d8[ es16-2 d] c8-\thumb r8 f8 r8 | % 18
  f8-3[ es16 d] es8[ f16 es] d8 r8 g8 r8 | % 19
  g8-3[ f16 e] f8[ g16 f]
    \override Fingering.add-stem-support = ##f
    \override Fingering.staff-padding = #'()
    \set fingeringOrientations = #'(right) e8-1[
    \set fingeringOrientations = #'(left) a,8-1] | % 20
}

staff = {
  \clef "treble_8"
  \key f \major
  \time 4/4
  \set Score.currentBarNumber = #14
  << \lineTop \\ \lineBottom >>
}

\version "2.23.81"

\header { tagline = \markup {} }

\paper {
  #(set-paper-size "a5")
  top-margin = 0.0
  bottom-margin = 0.0
  left-margin = 0.0
  right-margin = 0.0
  indent = 0.0
  page-breaking = #ly:one-page-breaking
  system-system-spacing.basic-distance = 15
}

\score {
  \context Staff \staff
  \layout {
    ragged-right = ##f
    \context {
      \Score
      \override BarNumber.font-size = #-4
      \override BarNumber.break-visibility = ##(#t #f #t)
      \override BarNumber.Y-offset = -0.5
      % \override BarNumber.side-axis = X
      % \override BarNumber.direction = CENTER
      \override BarNumber.stencil = #(make-stencil-circler 0.1 0.25 ly:text-interface::print)
      \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/16)
    }
  }
}
