export const getBoundsB32Impl = (x0,x1,x2,x3,y0,y1,y2,y3,V2) => {
    const min4 = (t0, t1, t2, t3) => Math.min(Math.min(t0,t1), Math.min(t2,t3));
    const max4 = (t0, t1, t2, t3) => Math.max(Math.max(t0,t1), Math.max(t2,t3));
    return V2({ min: (min4(x0,x1,x2,x3)), max: (max4(x0,x1,x2,x3)) })({ min: (min4(y0,y1,y2,y3)), max: (max4(y0,y1,y2,y3)) });
};
