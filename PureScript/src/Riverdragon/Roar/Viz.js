export const _oscilloscope =
  (/** @type AnalyserNode */ node) =>
  (/** @type HTMLCanvasElement */ canvas) => {
    /** @type CanvasRenderingContext2D */
    const ctx = canvas.getContext('2d');
    canvas.style.imageRendering = 'crisp-edges';
    const latestBin = new Float32Array(node.fftSize);
    if (window.devicePixelRatio && window.devicePixelRatio !== 1) {
      canvas.style.width = `${canvas.width / window.devicePixelRatio}px`;
      canvas.style.height = `${canvas.height / window.devicePixelRatio}px`;
    }
    return () => {
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      node.getFloatTimeDomainData(latestBin);
      const samples = latestBin.slice(-Math.ceil(canvas.width));
      let max = 0;
      for (const s of samples) max = Math.max(Math.abs(s), max);
      if (!max) max = 1;
      const h2 = canvas.height/2;
      const points = Array.from(samples, offset =>
        h2 - offset * h2/max
      ).reverse();
      ctx.strokeStyle = 'currentColor';
      ctx.lineWidth = 2;
      ctx.fillStyle = 'none';
      ctx.beginPath();
      ctx.moveTo(canvas.width, points[0]);
      for (const i in points) {
        if (!i) continue;
        ctx.lineTo(canvas.width - i, points[i]);
      }
      ctx.stroke();
    };
  };

export const _spectrogram =
  (/** @type AnalyserNode */ node) =>
  (/** @type HTMLCanvasElement */ canvas) => {
    /** @type CanvasRenderingContext2D */
    const ctx = canvas.getContext('2d', { willReadFrequently: true });
    const latestBin = new Uint8Array(node.frequencyBinCount);
    if (window.devicePixelRatio && window.devicePixelRatio !== 1) {
      canvas.style.width = `${canvas.width / window.devicePixelRatio}px`;
      canvas.style.height = `${canvas.height / window.devicePixelRatio}px`;
    }
    return () => {
      // Shift everything left by 1 pixel
      tmpImgData = ctx.getImageData(1, 0, canvas.width - 1, canvas.height);
      ctx.putImageData(tmpImgData, 0, 0);
      ctx.clearRect(canvas.width - 1, 0, 1, canvas.height);

      node.getByteFrequencyData(latestBin);
      const samples = latestBin;
      const dy = canvas.height/samples.length;
      const points = Array.from(samples, mag => mag / 0x100);
      ctx.strokeStyle = 'currentColor';
      ctx.fillStyle = 'none';
      for (const i in points) {
        ctx.globalAlpha = points[i];
        ctx.beginPath();
        ctx.moveTo(canvas.width, canvas.height - i*dy);
        ctx.lineTo(canvas.width, canvas.height - i*dy - dy);
        ctx.stroke();
      }
    };
  };
