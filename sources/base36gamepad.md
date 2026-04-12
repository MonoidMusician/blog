<style>
td { text-align: center; }
#viz svg text {
  font-family: 'Fira Code', monospace;
}
</style>

+:-:+:-:+:-:+:-:+:-:+:-:+
| a | b | c | d | e | f |
+---+---+---+---+---+---+
| g | h | i | j | k | l |
+---+---+---+---+---+---+
| m | n | o | p | q | r |
+---+---+---+---+---+---+
| s | t | u | v | w | x |
+---+---+---+---+---+---+
| y | z | 0 | 1 | 2 | 3 |
+---+---+---+---+---+---+
| 4 | 5 | 6 | 7 | 8 | 9 |
+---+---+---+---+---+---+

<br/>

<div id="error" style="color: red;"></div>

<input id="write" type="text" style="width: 100%; font: inherit; color: inherit; background: none; border: 1px dotted currentColor; " />

<div id="viz" style="display: flex">
<div style="display: flex; flex-direction: column">
  <label>
  <input type="radio" name="inputMethod" value="36" />
  6 times 6
  </label>
  <div style="display: flex">
  <div id="vizL"></div>
  <div id="vizR"></div>
  </div>
</div>
<div style="display: flex; flex-direction: column">
  <label>
  <input type="radio" name="inputMethod" value="26" checked />
  26 plus 10
  </label>
  <div id="vizA"></div>
</div>
</div>

<p>Buttons: [<span id="buttonList"></span>].</p>

B (1): Backspace\
Left stick (10): Space\
ZL (6): Shift\
ZR (7): Insert selected character\

<script defer>
window.addEventListener("DOMContentLoaded", () => {
  try {
    navigator.getGamepads();
    const { ById, SVG, HTML } = Ve;

    const squareAxes = true;
    const angle = (gamepad, i, j, snap=6, sensitivity=0.5) => {
      if (gamepad === undefined) return;
      let x = gamepad.axes[i];
      let y = gamepad.axes[j];
      let mag = Math.hypot(x, y);
      if (squareAxes) {
        const angle = Math.atan2(y, x);
        const maxValue = Math.min(Math.abs(1/Math.sin(angle)), Math.abs(1/Math.cos(angle)));
        mag /= maxValue;
      }
      if (mag < sensitivity) return null;
      let rawAngle = Math.atan2(y, x) * 180 / Math.PI;
      // -90 left, 0 up, 90 right
      rawAngle = (630 + rawAngle) % 360 - 180;
      let snapped = rawAngle;
      if (snap) {
        let snapAngle = 360 / snap;
        snapped = Math.round(rawAngle / snapAngle) * snapAngle;
        if (snapped === -180) snapped = 180;
        if (snapped === -0) snapped = 0;
      }
      return snapped;
    };
    const snapto = (gamepad, i, j, snap=6, offset=0, sensitivity=0.5) => {
      let snappedAngle = angle(gamepad, i, j, snap, sensitivity);
      if (snappedAngle == null || !snap) return snappedAngle;
      snappedAngle = (snappedAngle + 360) % 360;
      let value = Math.round(snappedAngle / (360 / snap));
      if (offset) value = (value + (offset + snap) % snap) % snap;
      return value;
    };
    const pct = (gamepad, i, j) => {
      if (gamepad === undefined) return;
      let x = gamepad.axes[i];
      let y = gamepad.axes[j];
      let mag = Math.hypot(x, y);
      if (squareAxes) {
        const angle = Math.atan2(y, x);
        const maxValue = Math.min(Math.abs(1/Math.sin(angle)), Math.abs(1/Math.cos(angle)));
        mag /= maxValue;
      }
      return mag;
    };


    const dedup = (cb, change=undefined) => {
      let oldValue;
      return newValue => {
        const event = newValue;
        if (change) newValue = change(newValue);
        if (newValue === oldValue) return;
        const saved = oldValue;
        cb(oldValue = newValue, saved, event);
      };
    };
    const poll = (fn, ...cbs) => {
      let running = true;
      let pollNow = () => {
        if (!running) return;
        let result = fn();
        for (const cb of cbs) cb(result);
        requestAnimationFrame(pollNow);
      };
      requestAnimationFrame(pollNow);
      return () => { running = false; };
    };
    const pushpair = (cb) => {
      let l = undefined;
      let r = undefined;
      return [x => cb(l=x,r), y => cb(l,r=y)];
    };
    const pullpair = (cbl, cbr) => {
      // ??
      return (...arg) => {
        cbl(...arg);
        cbr(...arg);
      };
    };
    const pushmany = (cb, init) => {
      let values = Object.assign([], init || {});
      if (init) cb(values, ...values);
      return Verity.sugar(idx => {
        return (value) => {
          values[idx] = value;
          cb(values, ...values);
        };
      });
    };
    const broadcast = (...cbs) => {
      return (...arg) => {
        for (const cb of cbs) cb(...arg);
      };
    };

    const alphabet36 = "abcdefghijklmnopqrstuvwxyz0123456789";
    const alphabet72 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ)!@#$%^&*(";

    const size = 50;
    const placement = 0.65;
    const coplacement = 1.3;
    const pad = 30;
    const angles = [-60, 0, 60, 120, 180, 240].map(angle => 120 + (angle - 120)*0.96 - 10);
    const apices = angles.map(angle => [size*Math.sin(angle * Math.PI/180), -size*Math.cos(angle * Math.PI/180)]);
    const coangles = angles.flatMap(angle => [angle - 10, angle + 10]);
    const coapices = coangles.map((angle, i) => [[coplacement*size*Math.sin(angle * Math.PI/180), -coplacement*size*Math.cos(angle * Math.PI/180)], (i - i%2)/2, i%2 ? 6+1 : 6-1]);
    const font = (size=18) => ({
      'dominant-baseline': 'middle',
      'text-anchor': 'middle',
      'font-size': size,
    });

    let selected = '26'; // '36' | '26'
    Ve.forQuery('input[name=inputMethod]', e => {
      Ve.on.change(e, () => { selected = e.value });
    });

    const alphabeta = pushmany((_, alpha, beta, shift, enter) => {
      let characters = shift ? alphabet72 : alphabet36;
      let chosen = alpha != null && beta != null ? alpha + 6 * beta : null;
      if (selected === '36' && enter && chosen != null) {
        console.log(characters[chosen]);
        ById.write.value += characters[chosen];
      }

      ById.vizL.clearChildren().appendChild(SVG.svg({ attrs: {
        'width': `${2*(size+pad)}`, 'height': `${2*(size+pad)}`,
        'viewBox': `${-(size+pad)} ${-(size+pad)} ${2*(size+pad)} ${2*(size+pad)}`,
      } }, [
        SVG.circle({ attrs: { 'cx': 0, 'cy': 0, 'r': size, 'stroke': 'black', 'stroke-width': '2', 'fill': 'none' } }),
        ...apices.map((pos, i) => SVG.circle({ attrs: { 'cx': pos[0], 'cy': pos[1], 'r': 4, 'fill': alpha == null || alpha == i ? 'mediumpurple' : 'gray' } })),
        ...apices.map((pos, i) => SVG.text({ attrs: { 'x': placement * pos[0], 'y': placement * pos[1], ...font(), 'fill': beta == null ? 'gray' : 'mediumpurple' } }, [characters[i + 6*beta]])),
        ...coapices.map(([pos, i, j]) => SVG.text({ attrs: { 'x': pos[0], 'y': pos[1], ...font(16), 'fill': '#888'+(alpha!=i?'a':'') } }, [characters[i + 6*((beta+j)%6)]])),
      ]));
      ById.vizR.clearChildren().appendChild(SVG.svg({ attrs: {
        'width': `${2*(size+pad)}`, 'height': `${2*(size+pad)}`,
        'viewBox': `${-(size+pad)} ${-(size+pad)} ${2*(size+pad)} ${2*(size+pad)}`,
      } }, [
        SVG.circle({ attrs: { 'cx': 0, 'cy': 0, 'r': size, 'stroke': 'black', 'stroke-width': '2', 'fill': 'none' } }),
        ...apices.map((pos, j) => SVG.circle({ attrs: { 'cx': pos[0], 'cy': pos[1], 'r': 4, 'fill': beta == null || beta == j ? 'mediumpurple' : 'gray' } })),
        ...apices.map((pos, j) => SVG.text({ attrs: { 'x': placement * pos[0], 'y': placement * pos[1], ...font(), 'fill': alpha == null ? 'gray' : 'mediumpurple' } }, [characters[alpha + 6*j]])),
        ...coapices.map(([pos, j, i]) => SVG.text({ attrs: { 'x': pos[0], 'y': pos[1], ...font(16), 'fill': '#888'+(beta!=j?'a':'') } }, [characters[(alpha+i)%6 + 6*j]])),
      ]));
    }, [null, null, null, null]);

    const radially = pushmany(({ alpha, beta, shift, enter }) => {
      let characters = shift ? alphabet72 : alphabet36;
      if (selected === '26' && enter) {
        if (alpha != null) {
          console.log(characters[alpha]);
          ById.write.value += characters[alpha];
        } else if (beta != null) {
          console.log(characters[26 + beta]);
          ById.write.value += characters[26 + beta];
        }
      }
      const size = 60;
      const placement = 1.2;
      const pad = 30;
      const angles = [...alphabet36.substring(0, 26)].map((_, n) => 360 / 26 * n);
      const apices = angles.map(angle => [size*Math.sin(angle * Math.PI/180), -size*Math.cos(angle * Math.PI/180)]);
      const dialup = [...alphabet36.substring(26, 36)].map((_, n) => 360 / 10 * n);
      const mini = 0.4;
      const inner = dialup.map(angle => [mini*size*Math.sin(angle * Math.PI/180), -mini*size*Math.cos(angle * Math.PI/180)]);
      ById.vizA.clearChildren().appendChild(SVG.svg({ attrs: {
        'width': `${2*(size+pad)}`, 'height': `${2*(size+pad)}`,
        'viewBox': `${-(size+pad)} ${-(size+pad)} ${2*(size+pad)} ${2*(size+pad)}`,
      } }, [
        SVG.circle({ attrs: { 'cx': 0, 'cy': 0, 'r': size, 'stroke': 'black', 'stroke-width': '2', 'fill': 'none' } }),
        ...apices.map((pos, i) => SVG.circle({ attrs: { 'cx': pos[0], 'cy': pos[1], 'r': 4, 'fill': alpha == i ? 'mediumpurple' : 'gray' } })),
        ...apices.map((pos, i) => SVG.text({ attrs: { 'x': placement * pos[0], 'y': placement * pos[1], ...font(), 'fill': alpha == i ? 'mediumpurple' : 'gray' } }, [characters[i]])),

        SVG.circle({ attrs: { 'cx': 0, 'cy': 0, 'r': mini * size, 'stroke': 'black', 'stroke-width': '2', 'fill': 'none' } }),
        ...inner.map((pos, i) => SVG.circle({ attrs: { 'cx': pos[0], 'cy': pos[1], 'r': 4, 'fill': beta == i ? 'mediumpurple' : 'gray' } })),
        ...inner.map((pos, i) => SVG.text({ attrs: { 'x': 1.5 * pos[0], 'y': 1.5 * pos[1], ...font(), 'fill': beta == i ? 'mediumpurple' : 'gray' } }, [characters[26 + i]])),
      ]));
    }, {
      alpha: null,
      beta: null,
      shift: null,
      enter: null,
    });

    const button = i => gamepad => !!gamepad?.buttons[i]?.value;
    const rising = (f, v) => dedup(v => { if (v) f(v) }, v);

    const threshold = 0.93;
    poll(() => navigator.getGamepads()[0],
      dedup(alphabeta[0], gamepad => {
        return snapto(gamepad,0,1, 6, 1);
      }),
      dedup(alphabeta[1], gamepad => {
        return snapto(gamepad,2,3, 6, 1);
      }),
      dedup(broadcast(alphabeta[2], radially.shift), button(6, "ZL", "Shift")),
      dedup(broadcast(alphabeta[3], radially.enter), button(7, "ZR", "Insert")),
      dedup(v => {
        if (v) console.log(v);
        ById.buttonList.textContent = v;
      }, gamepad =>
        gamepad?.buttons.flatMap(({ value }, i) => value ? [i] : []).join(', ')
      ),
      rising(() => { ById.write.value += " " }, button(10, "Left stick", "Space")),
      rising(() => { const e = ById.write;
        e.value = e.value.substring(0, e.value.length-1)
      }, button(1, "B", "Backspace")),
      dedup(radially.alpha, gamepad => {
        const mag = pct(gamepad,0,1);
        return mag > threshold ? snapto(gamepad,0,1, 26) : null;
      }),
      dedup(radially.beta, gamepad => {
        const mag = pct(gamepad,0,1);
        console.log(mag > 0.1, mag <= threshold, mag);
        return mag <= threshold ? snapto(gamepad,0,1, 10, 0, 0.1) : null;
      }),
    );

    window.addEventListener("gamepadconnected", ({ gamepad }) => {
      console.log(
        "Gamepad connected at index %d: %s. %d buttons, %d axes.",
        gamepad.index,
        gamepad.id,
        gamepad.buttons.length,
        gamepad.axes.length,
      );
    });
  } catch(e) {
    document.getElementById("error").textContent = `${e.name}: ${e.message}`;
    throw e;
  }
});
</script>
