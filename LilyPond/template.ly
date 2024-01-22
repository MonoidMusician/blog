% LilyPond/template.ly
\version "2.23.81"

% to use variables from `--evaluate=(define-public narrow #f)`
#(use-modules (guile-user))
% idk there is some scoping issue, so smuggle it as an option
% #(ly:add-option 'narrow narrow #f "Narrow")

\header { tagline = \markup {} }

\paper {
  % #(set-paper-size (if (ly:get-option 'narrow) "a7" "a5"))
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
      \override BarNumber.break-visibility = ##(#f #f #t)
      \override BarNumber.Y-offset = -0.5
      \override BarNumber.stencil = #(make-stencil-circler 0.1 0.25 ly:text-interface::print)
      \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/16)
    }
  }
}

