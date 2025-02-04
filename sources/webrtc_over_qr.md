---
title: WebRTC over QR Connection Protocol
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2025/01/26
---

Details on the connection protocol for [WebRTC over QR](https://webrtc-over-qr.veritates.love/).
How is it even possible, and how could you use it too?

:::Warning
See there for caveats and compatibility and so on.

Most notably: automatic connection only works on Chromium-based browsers and only for [LAN]{t=} connections.
:::

## Baseline connection initiation (happy path \^\^)

This relies on a particular mechanic that only seems to be available in Chromium: the peer reflexive candidate^[Apparently just over [LAN]{t=} (link-local connections?), though I have not really confirmed this.] is available via `RTCPeerConnection.prototype.getStats`{.js}, which exposes the [ICE]{t=} username fragment before the host JavaScript sets any answer [SDP]{t=}.

We use this as a side-channel to exfiltrate the fingerprint of the guest, which the host must set before the connection will be trusted. This allows for one-way connection establishment, whereas normal signaling processes need to be bidirectional.

Outside of this happy path, some data will need to be returned from the guest back to the host: always the fingerprint, and then candidate information for [WAN]{t=} connections (public [IP]{t=} and public port, as discovered via a [STUN]{t=} server).

Here are the steps for basic connection establishment:

#. Host generates a password that the guest will set as its password for [ICE]{t=} (`a=ice-pwd:$TEMPLATE_PASSWORD`{.sdp} in the [SDP]{t=})

    ```javascript
    const AGREED_UPON_PASSWORD = 'this-is-not-a-password+' + String(Math.random()).substring(2);
    ```

    <div class="Note">
    Using [`crypto.getRandomValues`{.js}](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues) is recommended, but less cute.

    Something like

    ```javascript
    function randomHex() {
      return Array.from(crypto.getRandomValues(new Uint8Array(16)), x=>x.toString(16).padStart(2,'0')).join("").toUpperCase();
    }
    ```
    </div>

#. Host starts a peer connection and creates a data channel for it

    ```javascript
    // Start creating a peer connection
    var rtc = new RTCPeerConnection(options);

    // As the host, we create a data channel immediately, so it is in the offer
    var dc = rtc.createDataChannel('init', { reliable: true });
    ```

#. Host creates an offer with all available candidates

    ```javascript
    // Create an offer
    var offer = undefined;
    rtc.createOffer().then(async desc => {
      // We need to set it locally
      rtc.setLocalDescription(desc);
      // Bypass trickle ICE
      await new Promise(resolve => {
        rtc.onicegatheringstatechange = _ =>
          rtc.iceGatheringState === 'complete' && resolve();
      });
      // The SDP is now updated with ICE candidates
      offer = rtc.localDescription.sdp;
    });
    ```

#. Host generates the JavaScript for the guest, substituting in `$TEMPLATE_*`{.js} variables (the content of the template is given in the following code snippets for the guest)

    ```javascript
    const templateJS = `...`; // Obtain the guest template somehow
    const guestJS = templateJS
      .replace('$TEMPLATE_OPTIONS', options ? `(${options})` : ``)
      .replace('$TEMPLATE_OFFER', "`"+offer+"`")
      .replace('$TEMPLATE_PASSWORD', "`"+AGREED_UPON_PASSWORD+"`");
    ```

#. Next you have to transport the JavaScript to the guest, [e.g.]{t=} encode it as an [HTML]{t=} `data:` [URI]{t=} in a [QR]{t=} code, scan it with a [QR]{t=} reader that will give you that raw [URI]{t=}, and paste it into a compatible browser (**not** Firefox on iOS)

#. Guest starts setting up its own side of the peer connection

    ```javascript{data-lang="Guest JS"}
    var rtc = new RTCPeerConnection($TEMPLATE_OPTIONS);
    ```

#. Guest sets the offer using the [SDP]{t=} from the host (can be taken verbatim, maybe preprocessed a little bit by the host to trim candidates, useless information, or whatever)

    ```javascript{data-lang="Guest JS"}
    rtc.setRemoteDescription({type:'offer',sdp:$TEMPLATE_OFFER})
    ```

#. Guest creates its own answer [SDP]{t=}, and modifies it to exfiltrate the fingerprint and override the password

    ```javascript{data-lang="Guest JS"}
    rtc.createAnswer().then({ sdp } => {
      // Extract the fingerprint from the SDP
      var fingerprint = /fingerprint:sha-256 (.+)/.exec(sdp)[1];
      // Exfiltrate it via `ice-ufrag`
      sdp = sdp.replace(/(a=ice-ufrag):.+/, '$1:'+fingerprint);
      // And replace `ice-pwd` with the password the host already knows about
      sdp = sdp.replace(/(a=ice-pwd):.+/, '$1:'+$TEMPLATE_PASSWORD);
      // Use this updated answer SDP
      rtc.setLocalDescription({
        type:'answer',
        sdp: sdp,
      });
    });
    ```

#. Guest starts waiting for a data channel, which it will `eval`{.js} messages from

    ```javascript{data-lang="Guest JS"}
    var dc;
    rtc.ondatachannel = ev => {
      dc = ev.channel;
      dc.onmessage = e => eval(e.data);
    };
    ```

#. Host listens for the guest to try to connect, which allows us to finish the connection

    <div class="Warning">
    This only works on Chromium-based browsers! And only over [LAN]{t=}! (not behind [NAT]{t=})
    </div>

    - The fingerprint comes from the `a=ice-ufrag:`{.sdp} line
    - The password is known, since we asked the guest to set it to the one we chose
    - The [SDP]{t=} does not require many other details (like candidates) since the peer already initiated the connection

    ```javascript
    // On Chromium we can see the remote connection that is trying our
    // Offer SDP before we have the right Answer SDP and, most importantly,
    // before we have the fingerprint for its certificate to trust it
    rtc.oniceconnectionstatechange = async (e) => {
      if (rtc.iceConnectionState === "checking") {
        // This `getStats` API is wonky and not fully standardized
        const stats = [...(await rtc.getStats()).values()];
        // But we can get the remote candidate and thus the username fragment from it
        const { usernameFragment: fingerprint } = stats.find(s => s.type === 'remote-candidate');
        // Which the guest kindly set to be its fingerprint

        // Most of this Answer SDP is just formality
        // e.g. the `c=` line is literally ignored but must be present
        // I believe the ice-{ufrag,pwd} is enough to identify which remote
        // we are talking about, given that it has attempted to connect
        // already (that is how we got here after all)
        const sdp = `
          v=0
          o=- 1 2 IN IP4 127.0.0.1
          s=-
          t=0 0
          a=group:BUNDLE 0
          a=extmap-allow-mixed
          a=msid-semantic: WMS
          m=application 9 UDP/DTLS/SCTP webrtc-datachannel
          c=IN IP4 0.0.0.0
          a=ice-ufrag:${fingerprint.replaceAll(':','')}
          a=ice-pwd:${AGREED_UPON_PASSWORD}
          a=ice-options:trickle
          a=fingerprint:sha-256 ${fingerprint}
          a=setup:active
          a=mid:0
          a=sctp-port:5000
          a=max-message-size:262144
        `.replaceAll(/\n\s+/g,'\n').slice(1); // keep the trailing newline!

        rtc.setRemoteDescription({ type: 'answer', sdp });
      }
    };
    ```

    <div class="Note">
    Note that the host and guest have somewhat/extremely different [SDP]{t=}s for each other, and this is okay!
    </div>

#. Host waits for the connection to fully open and then sends some JavaScript over the `DataChannel`{.js} to finish bootstrapping the guest

    ```javascript
    dc.onopen = () => {
      dc.send(`
        // Your JavaScript code to start the application
        alert("hi");

        // Remember to clean up from the bootstrapping:
        // Delete global variables (especially from code golfing)
        // Remove event listeners to prevent further \`eval()\`
        rtc.ondatachannel = null;
        dc.onmessage = null;
        // Etc.

        // You also should add error handling, since
        // there was no room for it in the QR code
      `);
    };
    ```

## Tweaks and so on

First of all: to get the JavaScript-in-HTML-in-`data:` [URI]{t=} to fit into a QR code with a full [SDP]{t=}, the JavaScript template needs to be minified a bunch, but it is possible.

As I mentioned above, outside of this happy path you need to return a fingerprint, and this is what the [QR in QR](qrinqr.html) code is intended for: with aggressive minification and code golfing, the guest can print a QR code which the host can scan.
This is an alright solution for that in a local situation.

And for [WAN]{t=} connections, it needs the public [IP]{t=} address and public port from the [STUN]{t=} candidate(s).
I donʼt really have a great way to return this information yet: itʼs just a bit ugly to write the guest code, it really should be a dedicated mode with a separate template.
([TURN]{t=} servers^[which act as relays, especially when one peer is behind symmetric NAT/CGNAT, which prevents P2P connections from being established with the help of STUN] are basically out of scope for this, though it would be possible to add support … but at that point you should almost always set up your own signaling server and use standard solutions, especially since [TURN]{t=} servers require passwords and are not publicly available because of the bandwidth considerations.)

Smaller details include:

- I set the username fragment to be the fingerprint without colons, but putting them back in requires code that is just ugly to present here. (Thanks JavaScript.)
- The first `eval`{.js} kicks off a multi-stage bootstrapping, first loading the stage 1 script, which runs itself on the guest, then it calls stage 2 (which is one of the scripts copied by stage 1), and finally it is ready to call the main app now that host and guest are basically identically loaded
- Various debug helpers and error handling
- Support for remote candidate (from STUN)

<!-- ## Security/privacy considerations

This is primarily meant for local communication with someone you already trust, especially initiating it in person.

Obviously the guest is running arbitrary JavaScript from the host.^[If your guest is already running code you control, this `eval`{.js} is not necessary: it is just intended here for the case of bootstrapping purely over QR+WebRTC.]

Conversely, the host is accepting the first guest that tries to connect.
You should confirm the guestʼs identity before sharing any private information, etc. etc.

The communication is only as trustworthy as the method that is used to bootstrap it.

IP addresses are exposed in the offers to establish the [P2P]{t=} WebRTC connection.
And the internet traffic will reveal that a P2P connection is taking place between those peers.
(In fact, the characteristics of this particular application may be leaked to eavesdroppers who can view the packets, [e.g.]{t=} by examining the unique format of the `ufrag`: I am not quite sure how the [ICE]{t=} connectivity is transmitted but I think it may be unencrypted.)

The [STUN]{t=} servers are a relatively limited risk: they do not mediate the connection, only facilitate [NAT]{t=} traversal for the peers to establish a connection directly.

However, once the WebRTC connection is established, it is encrypted and thus the contents of each message should be safe from eavesdropping.
(This relies on the browser and OS being secure. And eavesdroppers can still see the metadata about how much data is being exchanged over time.) -->
