---
title: "Ruboto: Robotic Rubato"
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

In the course of transcribing [Le Vertigo] for the electronic music program Max/MSP, I decided that I didnʼt want it to sound robotic.
So I taught the robot to play with _rubato_.

## The difference it makes

## My design for rubato input

## Why I chose the design I did
The main question is: what is the Y axis?
Is it “position”, or is it “velocity”?
I opted to go with position, for three reasons:

(1) The main reason: by representing time, itʼs obvious how to “stutter” in the music, just insert a stairstep discontinuity of the amount you want to delay or advance by.
  If it was representing velocity, you would have to insert some area that corresponds to what you want, itʼs just a mess.
  It could be snuck in between events.
(3) Conversely, although it might be nice to have different levels correspond to different speeds in the “velocity” view, the “position” view just encodes that as slope, which is pretty recognizable still.
(2) It required less math, didnʼt have to compute sums.
  (I wouldnʼt need to compute integrals anyways, since the events were finite and discrete.)

Another design consideration is do I feed-forward or pull-back time?
In this case, since it is a percussion instrument synthesizer, itʼs super easy to just map music time to realtime and never worry about converting in the reverse direction.
(You can even reverse time, play notes out of sequence, without issue.)

## Future work

As I mentioned above, I could have done a better job of actually using it.

I didnʼt really find a satisfactory way of encoding grace notes (other than as like 64th notes).
And I would love to incorporate rolled chords, but that did not happen.

Part of it is that I need to step outside of the limitations of Max/MSP and use a real programming language.
Like, ahem, PureScript.
