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
