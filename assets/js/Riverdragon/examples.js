import * as Resource from "./Resource.js";
import { Stream } from "./Riverdragon.js";
import { DOM, Input, Network } from "./APIs.js";
import { Gesture } from "./APIs/Gesture.js";
const nice_colors = [
    "#d350fe",
    "#2372ff",
    "#1dca24",
    "#fab30e",
    "#ff004d",
];
export function touch() {
    Resource.withScope(Resource.mkSubscope(Resource.noScope), () => {
        const scope = Resource.getScope();
        const el = (id) => document.getElementById(id);
        const select1 = (q) => document.querySelector(q);
        const controls = el("controls");
        const outputs = el("outputs");
        const outputList = outputs.appendChild(document.createElement("ol"));
        const allevents = el("allEvents");
        const eventList = allevents.appendChild(document.createElement("ol"));
        Network.eventSource(import.meta.url + '?watch').receive.drop(1).limitTo(1).subscribe(() => {
            scope.destroy();
            location.reload();
        });
        const touchPoints = (() => {
            const template = select1("svg .touchPoint");
            const size = template.parentElement.getBoundingClientRect();
            const { width, height } = size;
            return nice_colors.map(color => {
                const point = template.parentElement.appendChild(template.cloneNode());
                point.setAttribute("fill", color + "77");
                point.setAttribute("cx", String(Math.random() * width));
                point.setAttribute("cy", String(Math.random() * height));
                return point;
            });
        })();
        let gesture = Gesture.defaultGesture();
        let gestures = Stream.createRiver();
        const { $text, $HTML } = DOM.VDOM;
        const dl = (items) => $HTML("dl", {}, Object.entries(items).map(([k, vs]) => {
            return [
                $HTML("dt", {}, k),
                vs.map(v => $HTML("dd", {}, v)),
            ];
        }));
        const $info = (f) => $text(gestures.stream.map(f));
        const inner = dl({
            distanceTraveled: [
                $info(g => g.distanceTraveled),
            ],
            rotation: [
                $info(g => g.metrics.rotation.stable.degrees),
                $info(g => g.metrics.rotation.current.degrees),
            ],
            radius: [
                $info(g => g.metrics.radius.stable.screen.r),
                $info(g => g.metrics.radius.current.screen.r),
            ],
            radiusX: [
                $info(g => g.metrics.radius.stable.screen.x),
                $info(g => g.metrics.radius.current.screen.x),
            ],
            radiusY: [
                $info(g => g.metrics.radius.stable.screen.y),
                $info(g => g.metrics.radius.current.screen.y),
            ],
            screenX: [
                $info(g => g.metrics.center.stable.screen.x),
                $info(g => g.metrics.center.current.screen.x),
            ],
            screenY: [
                $info(g => g.metrics.center.stable.screen.y),
                $info(g => g.metrics.center.current.screen.y),
            ],
        });
        outputs.appendChild($HTML("div", {}, inner));
        const globals = Input.globals();
        globals.touch.stream.subscribe(event => {
            // console.log(event);
            event.preventDefault();
            if (event.type !== "touchmove") {
                const item = document.createElement("li");
                item.appendChild(document.createTextNode(event.type));
                eventList.appendChild(item);
            }
            const touches = Array.from(event.touches);
            // console.log(touches[0]?.identifier, touches);
            for (const [i, point] of touchPoints.entries()) {
                if (!touches[i]) {
                    point.style.display = 'none';
                }
                else {
                    point.style.display = '';
                    point.setAttribute("cx", String(touches[i].clientX));
                    point.setAttribute("cy", String(touches[i].clientY));
                }
            }
            let gest = gesture(touches, event);
            if (touches.length) {
                // console.log(gest);
                gestures.send(gest);
            }
        });
        // globals.pointer.stream.subscribe(event => {
        //     if (event.pointerType !== "touch") return;
        //     if (["pointerenter", "pointerleave", "pointerover", "pointerout"].includes(event.pointerType))
        //         return;
        //     console.log(event);
        //     event.preventDefault();
        //     if (event.type !== "pointermove") {
        //         const item = document.createElement("li");
        //         item.appendChild(document.createTextNode(event.type));
        //         eventList.appendChild(item);
        //     }
        //     touchPoints[0].setAttribute("cx", String(event.clientX));
        //     touchPoints[0].setAttribute("cy", String(event.clientY));
        //     // const touches = Array.from(event.touches);
        //     // console.log(touches[0]?.identifier, touches);
        //     // for (const [i, point] of touchPoints.entries()) {
        //     //     if (!touches[i]) {
        //     //         point.style.display = 'none';
        //     //     } else {
        //     //         point.style.display = '';
        //     //         point.setAttribute("cx", String(touches[i].clientX));
        //     //         point.setAttribute("cy", String(touches[i].clientY));
        //     //     }
        //     // }
        // });
        globals.keyboard.stream.subscribe(ev => {
            console.log([...globals.keyboard.knownKeys.entries()].join("; "), ev);
        });
    });
}
;
