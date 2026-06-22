import * as Resource from "../Resource.js";
import { Stream } from "../Riverdragon.js";
import { DOM, repollable, Timer } from "../APIs.js";
////////////////////////////////////////////////////////////////////////////////
// WebRTC (and WebTransport?) (communication with peers or servers)
////////////////////////////////////////////////////////////////////////////////
export var RTC;
(function (RTC) {
    function negotiable(methods) {
        let { renegotiate, requested, negotiated, check, compare } = methods;
        if (!compare)
            compare = (x, y) => x === y;
        const store = Stream.createStore(requested());
        const negotiating = Stream.createStore(!compare(requested(), negotiated()));
        const crosscheck = Stream.createRiver();
        // Update:
        // - immediately on subscription
        //   (TODO: double check and broadcast to others?)
        // - when requested globally
        // - when an event occurs on the PeerConnection that
        //   suggests negotiation status changed
        // - when renegotiation is requested
        const checking = repollable("rtc", check, crosscheck.stream);
        checking.subscribe(() => {
            const got = requested();
            store.send(got);
            negotiating.send(!compare(got, negotiated()));
        });
        return {
            requested: {
                current: requested,
                // TODO: dedup normally but not from renegotiate?
                stream: store.stream,
            },
            // `to` is not used directly, it is always taken from the getter
            renegotiate: to => crosscheck.send(renegotiate(to)),
            negotiated: {
                current: negotiated,
                stream: checking.map(negotiated)
                    .mapArray(v => v === undefined || v === null ? [] : [v])
                    .dedup().unsafeRiver(),
            },
            negotiating: {
                current: () => {
                    const n = !compare(requested(), negotiated());
                    if (n !== negotiating.current())
                        negotiating.send(n);
                    return n;
                },
                stream: negotiating.stream.dedup().unsafeRiver(),
            },
        };
    }
    RTC.negotiable = negotiable;
    ;
    // This is the main entrypoint interface to WebRTC, it is complicated mostly
    // by the many layers to get between establishing secure communication and
    // actually transferring media and data over it, and also by the negotiation
    // processes that happen at many levels of that as well.
    function Connection(connection, configuration) {
        return Resource.inSubScope(() => {
            const scope = Resource.getScope();
            let provided = connection;
            const rtc = provided ?? new RTCPeerConnection(configuration);
            if (!provided)
                rtc.setConfiguration(configuration);
            provided = undefined;
            scope.addDestructor(() => rtc.close());
            const events = DOM.listenTo(rtc);
            const check = Stream.createRiver();
            const recheck = check.send;
            const negotiations = Stream.oneStream([
                events.mintListener("negotiationneeded").stream,
                events.mintListener("signalingstatechange").stream,
                events.mintListener("track").stream,
                events.mintListener("connectionstatechange").stream,
                check.stream,
            ]);
            const iceGatheringState = repollable("rtc", events.listener("icegatheringstatechange").map(() => { }))
                .map(() => rtc.iceGatheringState);
            const iceGatheringComplete = iceGatheringState.filter(s => s === "complete").limitTo(1);
            const createDataChannel = (label, config) => {
                return Resource.withScope(scope, (DataChannel), rtc.createDataChannel(label, config), config?.binaryType);
            };
            return {
                connection: rtc,
                sdp: {
                    local: negotiable({
                        requested: () => rtc.localDescription,
                        renegotiate: desc => rtc.setLocalDescription(desc).then(recheck, recheck),
                        negotiated: () => rtc.currentLocalDescription,
                        check: negotiations,
                        compare: (x, y) => x === y || x?.type === y?.type || x?.sdp === y?.sdp,
                    }),
                    remote: negotiable({
                        requested: () => rtc.remoteDescription,
                        renegotiate: desc => rtc.setRemoteDescription(desc).then(recheck, recheck),
                        negotiated: () => rtc.currentRemoteDescription,
                        check: negotiations,
                        compare: (x, y) => x === y || x?.type === y?.type || x?.sdp === y?.sdp,
                    }),
                },
                iceGatheringState,
                iceGatheringComplete,
                createDataChannel,
                onDataChannel: events.listener("datachannel"),
            };
        });
    }
    RTC.Connection = Connection;
    ;
    function DataChannel(channel, binaryType) {
        return Resource.inSubScope(() => {
            const scope = Resource.getScope();
            scope.addDestructor(() => channel.close());
            const events = DOM.listenTo(channel);
            if (binaryType)
                channel.binaryType = binaryType;
            const getStatus = () => channel.readyState;
            const onOpen = events.mintListener1("open").promise;
            const onError = events.mintListener1("error").promise;
            const onClosing = Resource.impervious(() => events.mintListener1("closing").promise);
            const onClose = Resource.impervious(() => events.mintListener1("close").promise);
            const status = Stream.makeLake((cb, runDry) => {
                let subbed = true;
                const poke = () => {
                    if (!subbed)
                        return;
                    const s = getStatus();
                    cb(s);
                    if (s === "closed")
                        runDry();
                    return s;
                };
                const s0 = poke();
                if (s0 !== "closed") {
                    if (s0 === "connecting")
                        onOpen.then(poke);
                    onError.then(poke);
                    onClose.then(poke);
                    onClosing.then(poke);
                }
                return () => subbed = false;
            }).instantiate().stream;
            return {
                channel,
                status: { stream: status, current: getStatus },
                onOpen, onError, onClosing, onClose,
                bufferedAmountLow: events.listener("bufferedamountlow"),
                send: channel.send.bind(channel),
                receive: DOM.listener(channel, "message"),
            };
        });
    }
    RTC.DataChannel = DataChannel;
    ;
    let RTP;
    (function (RTP) {
        function Transceiver(gadget, check) {
            return Resource.inSubScope(() => {
                return {
                    direction: negotiable({
                        renegotiate: value => gadget.direction = value,
                        requested: () => gadget.direction,
                        negotiated: () => gadget.currentDirection,
                        check,
                    }),
                };
            });
        }
        RTP.Transceiver = Transceiver;
    })(RTP = RTC.RTP || (RTC.RTP = {})); // namespace RTC.RTP
    // SDP stands for Session Description Protocol. It is an ancient crusty format
    // for negotiating connection information. It contains connection information
    // (IP addresses, ICE connectivity establishment), media streams information,
    // boilerplate that predates WebRTC, and other stuff.
    let SDP;
    (function (SDP) {
        SDP.fingerprintLine = /^a=fingerprint:sha-256 ([0-9a-fA-F]{2}(?:[:][0-9a-fA-F]{2}){31})$/m;
        function extractFingerprint(sdp) {
            return SDP.fingerprintLine.exec(sdp)?.[1];
        }
        SDP.extractFingerprint = extractFingerprint;
        ;
        // Generate a basic answer SDP, assuming one data channel.
        //
        // This is the basic data needed to target and trust a connection:
        // the ICE credentials (username fragment and password) for the
        // connection, plus the peerʼs certificate finterprint.
        // (This is why the signaling channel needs to be trusted.)
        //
        // The RTCPeerConnection (really the RTC subsystem) remembers the ICE
        // details from the ICE credentials, so that is why there are no substantive
        // IP addresses required in this SDP.
        function basicAnswer(details) {
            return `
        v=0
        o=- 1 2 IN IP4 127.0.0.1
        s=-
        t=0 0
        a=group:BUNDLE 0
        a=extmap-allow-mixed
        a=msid-semantic: WMS
        m=application 9 UDP/DTLS/SCTP webrtc-datachannel
        c=IN IP4 0.0.0.0
        a=ice-ufrag:${details.ufrag}
        a=ice-pwd:${details.pwd}
        a=ice-options:trickle
        a=fingerprint:sha-256 ${details.fingerprint}
        a=setup:active
        a=mid:0
        a=sctp-port:5000
        a=max-message-size:262144
      `.replaceAll(/\n\s+/g, '\n').slice(1); // keep the trailing newline!
        }
        SDP.basicAnswer = basicAnswer;
        ;
    })(SDP = RTC.SDP || (RTC.SDP = {})); // namespace RTC.SDP
    // Start a basic connection over a signaling channel, starting with just
    // a data channel, so you can negotiate media transfer later.
    function startBasic(common) {
        return Resource.inSubScope(() => {
            const scope = Resource.getScope();
            // Start creating a peer connection, if we werenʼt handed one
            const rtc = common.connection ?? Connection(undefined, common.configuration);
            return {
                // Use a signaling channel to bootstrap a basic connection
                host: Resource.runner(function* (as_host) {
                    const plz = Resource.plz; // helper so Promise<T> begets <T>
                    // As the host, we create a data channel immediately, so it is in the offer
                    const channel = Resource.withScope(scope, DataChannel, rtc.connection.createDataChannel(common.dataChannelLabel ?? 'bootstrap', { ordered: true }));
                    // Generate an initial offer, with local candidate addresses,
                    // the data channel, and other details, and set it locally
                    // `rtc.connection.localDescription = ...`
                    rtc.sdp.local.renegotiate(yield* plz(rtc.connection.createOffer()));
                    // Bypass trickle ICE
                    yield rtc.iceGatheringComplete.nextValue();
                    // The SDP is now updated with ICE candidates
                    // But let the config modify it first
                    if (as_host.processSDP) {
                        const generatedOffer = rtc.sdp.local.requested.current();
                        const modifiedOffer = yield* plz(as_host.processSDP(generatedOffer));
                        if (modifiedOffer)
                            rtc.sdp.local.renegotiate(modifiedOffer);
                    }
                    // `rtc.connection.localDescription`
                    const offerToSend = rtc.sdp.local.requested.current();
                    // Send the offer through the signaling channel to the peer
                    // (Subscribe first though)
                    Timer.microtask.mint(as_host.sendOffer, offerToSend);
                    // Wait for signaling to do its thing
                    // TODO: retry until successful?
                    // `rtc.connection.remoteDescription = ...`
                    rtc.sdp.remote.renegotiate(yield* plz(as_host.receiveAnswer.nextValue()));
                    return { rtc, channel };
                }),
                guest: Resource.runner(function* (as_guest) {
                    const plz = Resource.plz; // helper so Promise<T> begets <T>
                    // Wait for signaling to give us an offer
                    // TODO: retry until successful?
                    // `rtc.connection.remoteDescription = ...`
                    rtc.sdp.remote.renegotiate(yield* plz(as_guest.receiveOffer.nextValue()));
                    // Now we have enough information to create an answer
                    // `rtc.connection.localDescription = ...`
                    rtc.sdp.local.renegotiate(yield* plz(rtc.connection.createAnswer()));
                    // Let the config modify it first
                    if (as_guest.processSDP) {
                        const generatedAnswer = rtc.sdp.local.requested.current();
                        const modifiedAnswer = yield* plz(as_guest.processSDP(generatedAnswer));
                        if (modifiedAnswer)
                            rtc.sdp.local.renegotiate(modifiedAnswer);
                    }
                    // `rtc.connection.localDescription`
                    const answerToSend = rtc.sdp.local.requested.current();
                    // Send the answer back through the signaling channel
                    as_guest.sendAnswer(answerToSend);
                    // Wait for the negotiated channel to come through
                    const channel = yield* plz(rtc.onDataChannel
                        .map(({ channel }) => channel)
                        .filter(channel => channel.label === (common.dataChannelLabel ?? 'boostrap'))
                        .nextValue());
                    return { rtc, channel };
                }),
            };
        });
    }
    RTC.startBasic = startBasic;
})(RTC || (RTC = {})); // namespace RTC
