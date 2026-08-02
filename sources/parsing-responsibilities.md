---
title: "Separation of Responsibilities in (Binary) (Media) Formats"
subtitle: "Or: a bunch of examples of why media formats are weird"
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2025/08/20
---

Media standards are annoyingly complex, error-prone, and annoyingly not as simple as following a specification: just because it is in the specification does not mean anyone actually supports it, or supports it the way it was intended.

In light of this, one would hope that there is a kind of separation of concerns that we could rely on: I handle this layer of the media for myself, commit myself to parsing, understanding, and/or producing these bits of it, and the rest is handled by other layers.

But letʼs understand whatʼs going on in the media world and why that is not the case.

## Background

For starters, actual codecs are the base layer of media formats: almost always handled by a library, a library that someone(s) else has written that you interface with.
AAC, H.264, Opus, AV1, and so on, usually with a few canonical de/encoders ([e.g.]{t=} “whatever ffmpeg uses by default”), a few [“platform”](https://trac.ffmpeg.org/wiki/Encode/AAC#aac_at) ones, a couple more custom decoders for specific applications, a scattering of GPU/hardware implementations, and so on.
Theyʼre all encoded as bitstreams of some kind, because the difference between bits and bytes is very important when dealing with data compression.

On top of codecs, thereʼs sometimes bitstream encapsulation, wrapping the “raw” compressed frames with metadata, which can include synchronization/byte alignment^[“Synchronization” comes from broadcast media, where it literally was used to synchronize receivers to radio signals, but byte alignment is a similar thing.], error correction (if that isnʼt included in the codec itself), metadata required for codec playback, and other such things.
Examples include ADTS for AAC, which includes some necessary codec metadata along with the AAC frames, and Annex B encapsulation for video codecs like H.264, which separates Network Abstraction Layer (NAL) units.
(These bitstream encapsulations can be used for raw formats, but sometimes are placed in other container formats.)

Then thereʼs container formats: MP4, WAV, MKV (Matroška), TS (Transport Stream)^[(Fun fact: you can concatenate several Transport Streams to get another valid Transport Stream, unlike most other media file formats.)] ^[(Not so fun fact: TypeScript gets very annoyed when it encounters very large binary `.ts` files.)], and so on, which are used for “muxing” (combining streams), and which not only organize the raw bytes of the frames of each stream for structured access, but also provide lots of metadata:

- metadata necessary for codec playback (generic things like audio sample rate, video colorspace information, and details specific to each codec),
- metadata necessary for media playback (timestamps/durations for each video and audio frame, edit lists),
- and end-user metadata (like track names, artist information, creation time).

These container formats are used as file formats: they usually exist standalone on disk, or could be served over HTTP.
But they can also be packaged into playlists, which I would call another layer of the cake.
The two main “playlist” formats are HLS and DASH: HTTP Live Streaming (HLS) grew out of simple playlist files into a streaming media format (… you can imagine how smoothly thatʼs evolved^[not]), and Dynamic Adaptive Streaming over HTTP (MPEG-DASH) is a similar thing but now with XML&trade;.
Both use MP4 containers and sometimes Transport Stream containers: Transport Streams are inherently a streaming format, with metadata periodically embedded in the stream, while MP4 containers werenʼt even meant for streaming at all: they arenʼt particularly appendable, although fragmented MP4 (fMP4) managed to address some of that (… in a weird way).

In addition, there are more transport protocols for streaming media, that donʼt qualify as container–file formats: SRT, RTMP, RTP, and of course WebRTC now.
These have various expectations of how the data is encapsulated for transport, in addition to specifying negotiation, connection and reconnection, timing, and other details.

## What I Canʼt Fix

### Compatibility (Codec Complexity vs Codec Profiles)

One of the sad facts is that you can have a perfectly well-formed file that contains valid codec data for a “common” codec like AAC or H.264, and you still might not be able to play that file on *any* player you have.
Codecs contain too much surface area, and it is not always visible on the surface that a codec wonʼt be able to be decoded without knowing a lot about those codecs.

Browser support varies a lot across platforms, and even across API: native file support, Media Source Extensions (MSE), and WebCodecs are three different ways to play/process media within web browser, which each may support different codecs or parts of codecs.
As [MDN warns about `MediaSource.isTypeSupported(...)`](https://developer.mozilla.org/en-US/docs/Web/API/MediaSource/isTypeSupported_static#return_value),

> A value of `true` is returned if the browser can *probably* play media of the specified type. This is *not* a guarantee, and your code must be prepared for the possibility that the media will not play correctly if at all.
>
> All web APIs that work with media files use a “no/maybe/probably” approach (or, in this case, “no or probably”) when determining if a media type can be used. This is because media files are complex, intricate constructs with far too many subtle variations to be absolutely certain of anything until you actually use the contents of the media.

However, there are ways to stick to a known subset of codec support: these are codec *profiles*.
Codecs often contain a “baseline” profile that is expected to work with any decoder, and then other profiles that add features: better quality, better compression, HDR color spaces or surround sound or even volumetric sound.

This is the frosting on top of the cake: codec profiles frosting over codec types, placed on encapsulations, muxed over containers, packaged in playlists.

But even what a specification author thinks of as a codec profile that implementers should support, isnʼt always the case, because [someone somewhere along the line didnʼt implement a general case in the binary data](https://issues.chromium.org/issues/40317953).^[when the `channel_configuration` enum is `0`, it uses a `program_config_element()` struct specifying the exact channel elements – basically an intended speaker configuration.]
That is, itʼs not even the profile or the capability that is missing, but just a particular feature or edge case that wasnʼt implemented for whatever reason.

### Naming

Unfortunately, even just knowing what to call things is a huge mess:

- [“Advanced Video Coding”](https://en.wikipedia.org/wiki/Advanced_Video_Coding#Naming) is more commonly known as H.264.
  One could use its exact specification: ISO/IEC 14496-10, or MPEG-4 Part 10, but you can imagine that those numbers mean nothing to anyone who hasnʼt spent hours staring at those particular standards.
- AVC is sometimes referred to as its fourcc `avc1`, but it is not to be confused with the much newer and more complicated/efficient AV1 whose fourcc is `av01`.
- HEVC (High Efficiency Video Coding) is generally preferred over calling it H.265, something about flashy names for marketing.
- There is even an H.263, but it is very obsolete.
- `vp8` is the [only registered WebCodecs name](https://www.w3.org/TR/webcodecs-vp8-codec-registration/#fully-qualified-codec-strings) for the VP8 codec, but VP9 is qualified as [`vp09.*`](https://www.w3.org/TR/webcodecs-vp9-codec-registration/#fully-qualified-codec-strings)
- AAC (Advanced Audio Coding) is also known as MPEG-4 Audio, which is why it is registered in WebCodecs as variants of [`mp4a.*`](https://www.w3.org/TR/webcodecs-aac-codec-registration/#fully-qualified-codec-strings), which I quote since it kind of explains its own ridiculousness:
  - `mp4a.40.2` — MPEG-4 AAC LC
  - `mp4a.40.02` — MPEG-4 AAC LC, leading 0 for Aud-OTI compatibility
  - `mp4a.40.5` — MPEG-4 HE-AAC v1 (AAC LC + SBR)
  - `mp4a.40.05` — MPEG-4 HE-AAC v1 (AAC LC + SBR), leading 0 for Aud-OTI compatibility
  - `mp4a.40.29` — MPEG-4 HE-AAC v2 (AAC LC + SBR + PS)
  - `mp4a.67` — MPEG-2 AAC LC

  Yeah, thatʼs right: I lied and it also has a version called MPEG-2 AAC.
  To be fair, HE-AAC v1 and v2 have significant differences with compatibilty for AAC.

- *sometimes* [MIME types include additional codec info](https://developer.mozilla.org/en-US/docs/Web/Media/Guides/Formats/codecs_parameter), which can give better indications of compatibility for playback, but most of the time the MIME types you see will not have it, or not correctly ... and MIME types are solely based off of file extensions way too often

Labels called “fourcc” codes are commonly used, but they are victim of a patchwork of attempts at standardization vs usage in practice. (And sometimes they are only two characters?)

## What *Is* Fixable

I think it would help a lot of specification authors thought of specific roles to look after: what the responsibilities of that role would be with regards to the various formats, and what would make their use-cases easy to accomplish.

### Case Study: WebCodec AAC and H.264 from MP4

I hate to say it, but WebCodecs are kind of alright.
They get rid of a lot of complexity, and expose de/encoders as rather simple unit of: configuration + frames in = frames out.
And the configuration is mostly simple and mostly easy to understand (parameters like `sampleRate`: easy to understand, easy to access, and you probably had to know it anyways), except for the `description` parameter:

The `description` parameter of WebCodecs is an opaque blob of binary data that you need to get from *somewhere* that is specified in some document *somewhere*, which *may* be needed to decode the subformat/encapsulation of *your* data.
The Web Codec registration for each codec lists what it is supposed to be, by linking to another standard (which may be paywalled by ISO/IEC, ugh)^[although I happen to have access to many of those for my work] and expanding no further.

Specifically the way it works with AAC^[Other formats like Opus and AVC=H.264 work similarly] is that if `description` is absent, the frames are expected in [ADTS](https://wiki.multimedia.cx/index.php/ADTS) encapsulation (a bytestream format for AAC which includes some of the metadata that `description` would normally have).
Otherwise it specifies a format for `description` that gives encoders enough metadata to decode raw AAC frames.

Specifically [the AAC `description` is specified to be `AudioSpecificConfig`](https://www.w3.org/TR/webcodecs-aac-codec-registration/#audiodecoderconfig-description), a bitstring structure defined in the AAC standard (ISO/IEC 14496-3).

Where does one get their paws on this?

Well, letʼs start with the `description` for video formats like H.264.

There are some examples floating around: they all amount to finding the specific codec box inside the MP4 track structure, and getting the bytes it contains, and passing those to to the WebCodec decoder. Looking up MP4 boxes is easy, since they are the bread and butter of the MP4 container format (specifically, its underlying ISOBMFF “ISO Base Media File Format” specification). The common MP4 parser is even named `mp4box`, and it was used for a [demuxer demo for WebCodecs](https://github.com/w3c/webcodecs/blob/1e0cab73393f7f9ac4c0d780dda2acef57010061/samples/video-decode-display/demuxer_mp4.js#L62-L75):

```javascript
class MP4Demuxer {
  ...
  // Get the appropriate `description` for a specific track. Assumes that the
  // track is H.264, H.265, VP8, VP9, or AV1.
  #description(track) {
    const trak = this.#file.getTrackById(track.id);
    for (const entry of trak.mdia.minf.stbl.stsd.entries) {
      const box = entry.avcC || entry.hvcC || entry.vpcC || entry.av1C;
      if (box) {
        const stream = new DataStream(undefined, 0, DataStream.BIG_ENDIAN);
        box.write(stream);
        return new Uint8Array(stream.buffer, 8);  // Remove the box header.
      }
    }
    throw new Error("avcC, hvcC, vpcC, or av1C box not found");
  }
  ...
}
```

Looks up a box type (which `mp4box` has already recognized and pulled out), writes it to binary, and chops off its header to be left with its binary contents: pretty simple.

After some more digging, I eventually found W3Cʼs corresponding [example for an AAC description](https://github.com/w3c/webcodecs/blob/1e0cab73393f7f9ac4c0d780dda2acef57010061/samples/audio-video-player/mp4_pull_demuxer.js#L181-L192):

```javascript
class Mp4Source {
  ...
  getAudioSpecificConfig() {
    // TODO: make sure this is coming from the right track.

    // 0x04 is the DecoderConfigDescrTag. Assuming MP4Box always puts this at position 0.
    console.assert(this.file.moov.traks[0].mdia.minf.stbl.stsd.entries[0].esds.esd.descs[0].tag == 0x04);
    // 0x40 is the Audio OTI, per table 5 of ISO 14496-1
    console.assert(this.file.moov.traks[0].mdia.minf.stbl.stsd.entries[0].esds.esd.descs[0].oti == 0x40);
    // 0x05 is the DecSpecificInfoTag
    console.assert(this.file.moov.traks[0].mdia.minf.stbl.stsd.entries[0].esds.esd.descs[0].descs[0].tag == 0x05);

    return this.file.moov.traks[0].mdia.minf.stbl.stsd.entries[0].esds.esd.descs[0].descs[0].data;
  }
  ...
}
```

Ouch. That hurts.

It turns out that AACʼs `description` does not live in such a sunny place: it lives in the murkier corners of MP4ʼs *descriptors*, which I had honestly not paid any attention to until running into this.^[I think descriptors are part of MP4 proper, not the ISO Base Media File Format?]
Not only are they buried under the same trail of `trak.mdia.minf.stbl.stsd`{.javascript} (Track › MediaInformation › SampleTable › SampleDescription), but several extra layers of descriptor lists for who knows what reason.

For my own engineering pride, I replaced some of those `[0]`{.javascript}s with loops, looking for the right descriptors at any location.
But it is just magic numbers galore, some of which `mp4box` probably wouldnʼt help us out with even if it did have better support for descriptors.

### Case Study: MP4 AAC to LOAS/LATM

Anyways that is not where my saga with `AudioSpecificConfig` ends.

So I was trying to “polyfill” WebCodecs, transporting frames via a WebSocket (through Python) to ffmpeg, since Firefox on macOS was not handling the HE-AAC I had in one of the files I wanted to play.^[Yes I know the correct solution was just to transcode it, but but but…]
I was trying to find a lightweight way to wrap the AAC frames for ffmpeg, since I knew I could then get raw Float32 data out and send that back over the WebSocket, and, well, lots of container formats arenʼt good for streaming into a pipe, and most were too complex anyways.

So I looked into ADTS, since that was mentioned in the WebCodec spec and it seemed plausible that I could turn some knowledge of `description` into working ADTS.

Well, it turns out that the [ADTS headers are too simplistic](https://wiki.multimedia.cx/index.php/ADTS): I donʼt think they even really support HE-AAC in their two-bit profile^[[audio object type](https://wiki.multimedia.cx/index.php/MPEG-4_Audio#Audio_Object_Types)] field?
Well, actually there are several ways of signaling HE-AAC (specifically the [SBR Spectral Band Replication](https://en.wikipedia.org/wiki/Spectral_band_replication) feature), depending on backwards compatibility requirements, and I think ffmpeg was even smart enough to re-encapsulate the HE-AAC in one of those backward-compatible ways to fit inside ADTS without fully re-encoding it when I asked^[`ffmpeg -i input.m4a -c:a copy -f adts output.adts`] (not 100% sure).
But I did not want to parse the AAC bitstream itself to try and do that myself, sheesh.

Anyways, it turns out that LATM is the encapsulation I want: it includes `AudioSpecificConfig` verbatim, which should be perfect for the decoder!

(As far as I understand: LATM is a bitstream format that specifies lightweight muxing of multiple AAC streams. This was too difficult to understand in full, but ffmpeg writes and understands a single-stream format which actually looked reasonable. LOAS is a synchronization layer on top of it, which for our purposes accomplishes length-prefixing the data and aligning it to byte boundaries with a magic constant for good meaure (“synchronization”).)

Exceeeept… it includes `AudioSpecificConfig` verbatim, as a bitstring.
You have to know its exact length *in bits*.

WebCodecs takes `AudioSpecificConfig` on its own, so it doesnʼt care if there are trailing bits, and it is byte-padded anyways.
But MP4 also byte-pads it, so we donʼt *know* its bitlength!
(And it usually ends in `0`, so even knowing that the padding is `0` bits would not be enough to determine its length.)

So now again, I could just hardcode the length of the bitstring (my experiment with ffmpeg helped me confirm it), but that would be no fun.
So I found myself implementing an `AudioSpecificConfig` parser.
But *still*, Iʼm just shuffling around the gaps: now the limitation isnʼt HE-AAC decoding, but whatever I missed in my config parser (or messed up).
It turns out that there are a ton of specific case to handle in `AudioSpecificConfig` and I canʼt be bothered, especially since most decoders for it I saw could not be bothered either.

:::Key_Idea
Itʼs no oneʼs fault really, but it is not a happy state of affairs that I have to implement a parser for a specific bitstring structure for AAC metadata in order to re-mux AAC from MP4 to LATM.

I view it as a failure of separation of concerns: the spec was authored as if the LATM muxer had a parser for `AudioSpecificConfig` (which is rather unreasonable) or was handed it as a bitstring not a bytestring (which is sadly not the case).

Just because I am re-muxing AAC frames from a generic container format (MP4) to an AAC-specific format (LATM), does not mean that I actually have a parser at hand for every aspect of AAC!
:::

If the spec could be amended, one way to fix it would just be to add a sentinel `1` bit after the inclusion of `AudioSpecificConfig` and specify that it can be zero-padded, possibly specifying to {a/the next} byte alignment, up until that `1` bit.
That way, anyone who is handed a zero-padded `AudioSpecificConfig` would be able to insert it into LATM headers verbatim without having to re-parse it to determine its length.

Now, it *would* be cool if decoders exposed all of their bitstring parsers, or if specifications used a machine-readable format that everyone could generate parsers from …
(To be fair, the binary formats of the AAC spec kind of look machine-readable, but Iʼm not sure anyone has actually implemented parsers for them that way.)
(And thereʼs also the question of parsing the bits of the structure, versus interpreting them in a sensible way. I just happent to not need semantics for what I am doing.)

### Case Study: MP4/ISOBMFF

Via my job, I have learned many things about MP4s.
(Unfortunately all media topics are essentially bottomless pits of esoteric knowledge, so I canʼt say that I truly understand what all is going on in MP4s.)

Some of their shortcomings are:

- You canʼt “just” manipulate a MP4 file: if you want to edit one, you have two options:
  1. Fully understand the format and *every* box in the file, so that you know exactly how you can touch and rearrange bytes: in particular, what bytes are pointers to other locations in the file
      a. (Find a clever scheme to edit without doing that, like changing the `moov` atom to `free`^[Fun fact: `free` is not technically an ISOBMFF box, it is apparently part of QuickTime MOV.] and appending your edited `moov`)
  2. Demux the file, perform your edits on whatever representation you work with, and write out a totally new file that should at least capture what *you* thought of the media – but then you have to hope that you understood the media correctly in the first place and didnʼt omit any details, like HDR color spaces, or timeline edits, or really anything else

  I really would like a typed binary format that indicates at least which numbers refer to offsets in the binary file and what they are relative to.
  (Length prefixes at least are mostly taken care of by the box structure itself.)
  Most offsets point into `mdat` boxes, which are there for that purpose, but it is allowed and in fact not uncommon for some offsets to point into the `moov` box too ([e.g.]{t=} I have seen it for sample encryption: but our encoder did not have that offset into the `moov`, so I opted to duplicate it in the `mdat` and use that offset instead).

  Without basic information like that, editing a MP4 is just not feasible.
  Even a task as simple as “extract one track into its own MP4” should be as simple as dropping tracks from the `moov` box, finding all the offsets and lengths of samples, repacking `mdat` to include just those, and putting in the new offsets and lengths.
  But unless you understood every box that is in the `trak` that you copied, you donʼt know if any of them contains offsets into the `mdat` you just changed.
  And you canʼt just discard boxes that you donʼt know, because those might be important information like HDR color spaces that change the interpretation of data in important but not technically essential ways.

- They have a mechanism for edit lists, which sounds nice, but they are in a weird form (they mix two timelines in different timebases lol), but there are some “common” edit lists which are awkward to specify and understood mostly by convention: [an edit to discard audio decoder priming frame(s)](https://developer.apple.com/documentation/quicktime-file-format/using_track_structures_to_represent_encode_delay_explictly), an edit to offset timestamps for predictive coding video (B-frames), edit to just offset the track for other reasons (start timestamps, or post-hoc edits ... I donʼt even know how that stacks with the other edit list formats tbh), and their “rate” parameter is mostly not supported.

  This all means that edit lists are *at best* something that end-user media players *might* be able to understand and handle: they just arenʼt suitable for other uses of media (for example, streaming), outside from a couple limited formats recognized by convention.

- In general, you just donʼt know what you need to pay attention to in a media file, so the answer seems to be that you are responsible for *all* of it, regardless of what your role is: player, demuxer, editor, transcoder, and so on.
  This is despite the fact that the format is supposedly extensible – or maybe because of it.
  You canʼt just read one specification and know it all, either: MP4 is split across many specifications, most of which are ISO/IEC 14496 parts, but not all are, as they are extended by other standards bodies ([e.g.]{t=} ITU-T).
  At least there are open source implementations that one can look at for finding important box types.

<!--
What ISOBMFF did well:

- Boxes always follow a specific length-prefixed format, so they can always be skipped over while parsing.
  Either `size: 32; fourcc: 32` or `0x00000001; fourcc: 32; largesize: 64`.
- Boxes are extensible in the since that new ones can be added without breaking existing parsers.
- Writing an MP4 is probably the easiest part about it: you can write the box types and structure you know and get a working file fairly easily, without needing to worry about things that you donʼt care about.

Whatʼs not so great:

- Boxes all have their own
- Boxes are coupled in weird ways
 -->
