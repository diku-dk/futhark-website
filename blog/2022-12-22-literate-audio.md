---
title: Generating audio with literate Futhark
author: Philip Munksgaard
description: This post shows how to generate music using literate Futhark.
---

Hello, it's me, [Philip Munksgaard](https://munksgaard.me)
[again](./2022-11-03-short-circuiting.html)!  Today, I'm here to talk about a
new feature in literate Futhark: [Audio
support](https://github.com/diku-dk/futhark/pull/1810).  In short, we have
added a new `:audio` directive, which let's you turn an array of signed 8-bit
integers into audio.  For details, check out the documentation
[here](https://futhark.readthedocs.io/en/latest/man/futhark-literate.html#directives).
This post is a demonstration of how you can use this functionality to compose
songs in Futhark.  By the way, everything you're reading here is itself a
literate Futhark script, so you can check [the
source](2022-12-22-literate-audio.fut) if you want to see how it works behind
the scenes. Let's get started!

Sound is vibrations in the air that are picked up by our ears[^1].  Vibrations
can be described using sine-curves. Computers are ultimately discrete in
nature, so in order to produce sound we have to approximate sine-curves using
sampling.  Let's see how that works in practice.

[^1]: Citation needed.

We start by defining some global variables that we're going to use.  First,
the volume which we wish to output at.  This should be a value between 0.0
and 1.0.

```futhark
def volume = 0.5f32
```

Then, the output frequency, meaning the number of samples we need to generate
per second.

```futhark
def output_hz = 44100i64
```

And the _standard pitch_.  This is an agreed-upon frequency that forms the
base of a particular school of music.  For instance, most western music is
based on a 12-tone chromatic scale, centered around the A₄-note, which is
defined to have the frequency 440 Hz. This means that the A₄ can be described
as a sine-curve with 440 periods every second.

```futhark
def standard_pitch = 440.0f32
```

All other notes in the 12-tone chromatic scale can be defined in terms of the
A₄ note. Notes are divided into octaves, which is why the standard pitch has
that four in it: It is the A-note of the fourth octave.  Each octave has 12
tones or notes in it, named A, A#, B, C, C#, D, D#, E, F, F#, G, G#, and the
each note of an octave is exactly double or half that of the corresponding note
in the previous or next octave, so the A₅-note has the frequency 880 Hz.  The
12-notes in each octave are distributed in such a way that this invariant is
always true.

This function helps us to compute the frequency of different notes based on
the standard pitch.  So, for instance, the pitch of the next note up from A₄,
the A#₄-note, is computed by calling `pitch 1`.

```futhark
def pitch (i: i64): f32 =
  standard_pitch * 2 ** (f32.i64 i/12)
```

To produce notes we need two things: a pitch and a duration.
If the duration is given in seconds, for instance 0.5 seconds, we need to
turn that into the number of samples resulting in a sound of that length,
given the output frequency.

```futhark
def num_samples (duration: f32): i64 =
  i64.f32 (f32.i64 output_hz * duration)
```

Next, we define the `sample` function, which takes a pitch and a sample index
in order to generate the frequency of the pitch at that particular point in
time.  The result is a number between -1.0 and 1.0 corresponding to the value
of the sine function corresponding to the given pitch at that particular
point in time.

```futhark
def sample (p: f32) (i: i64): f32 =
  volume * f32.sin (2 * f32.pi * f32.i64 i * p / f32.i64 output_hz)
```

Now we can define the `note` function, which samples the frequency for a
particular note for the given duration. This function ties together the pitch
and duration by taking samples of the sine function corresponding to a
particular pitch.

```futhark
def note (i: i64) (duration: f32): []f32 =
  let p = pitch i
  let n = num_samples duration
  in tabulate n (sample p)
```

Let's also make it possible to insert breaks in our compositions.

```futhark
def break (duration: f32): []f32 =
  replicate (num_samples duration) 0.0
```

Finally, we need a function to turn the samples into signed 8-bit integers,
such that `futhark literate` can turn that into music.

```futhark
def play [n] (samples: [n]f32): [n]i8 =
  samples
  |> map ((*) (f32.i8 i8.highest))
  |> map i8.f32
```

In the spirit of season, let's use what we have defined so far to compose a
song, inserting breaks and adjusting the length of notes as necessary.  Let's
see if you can recognize it.

```futhark
def seasonal_song =
  let c = note 3
  let d = note 5
  let e = note 7
  let g = note 10
  in e 0.3
       ++ break 0.1
       ++ e 0.3
       ++ break 0.1
       ++ e 0.6
       ++ break 0.2
       ++ e 0.3
       ++ break 0.1
       ++ e 0.3
       ++ break 0.1
       ++ e 0.6
       ++ break 0.2
       ++ e 0.3
       ++ break 0.1
       ++ g 0.3
       ++ break 0.1
       ++ c 0.5
       ++ break 0.05
       ++ d 0.15
       ++ break 0.1
       ++ e 0.6
       |> play
```

```
> :audio seasonal_song
```


![](2022-12-22-literate-audio-img/fd1d071397a89bfa6860eb8286014944-audio.wav)


There are plenty of things to improve in our song in particular and the music
framework in general.  For instance, it would be nice to get rid of the
popping sound between notes, and at some point we'd like to be able to write
chords (multiple notes played at the same time) and so on, but I think this
is a nice start.  There are also plenty of things we can do to improve the
`:audio` directive in literate Futhark, like specifying a different
frequency, a different output format (currently we generate wave-files) and
support for 32-bit sound.  I should also note that I have no real knowledge
about music except for the bits and pieces I've read on Wikipedia, so if
anyone more knowledge about music comes along, please let me know about my
mistakes.

Nevertheless, I hope you've enjoyed this blog post; I look forward to seeing
what kind of things people come up with using the new audio support.  Happy
holidays to everyone!
