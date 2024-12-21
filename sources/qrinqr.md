---
title: Code golf for computing a 32-byte QR code!
subtitle: "*Or*: QR in QR"
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2024/12/20
---

If you run `.replaceAll(/ *(\/\/[^\n]*\n)?/g, '').replace('\\x20',' ').length`{.js} on [this fileʼs contents](assets/js/qr.ann.js), you will see that this JavaScript comes in at 512 ASCII characters when minified!
(Newlines without comments are still used as substitutes for semicolons, to keep the minified file somewhat readable. Other spaces are not important except for the one in the string.)

Why would I do such a thing? Well, it helps me bootstrap a WebRTC connection over QR codes!
The host generates a HTML stub that contains this JavaScript, allowing it to display the fingerprint of the guestʼs generated certificate.

There is a hard limit of 2,953 bytes of UTF-8 for QR codes, and I have even less to work with considering that I have to encode it into a `data:` URI, so having a compact algorithm for this is essential.

::: {#demo}
<p style="display: flex; align-items: baseline; gap: 2em">
  <label class="input-wrapper" style="flex: 1">
    <span>32-bytes of hexadecimal</span>
    <input id="demo-input" class="code" style="width: 100%; text-overflow: ellipsis" />
  </label>
  <button id="demo-random" class="add" style="flex: 0 0 auto">Random!</button>
</p>
<pre id="demo-output" class="no-visible-space"></pre>
<script>
  "use strict";
  function randomHex() {
    return Array.from(crypto.getRandomValues(new Uint8Array(32)), x=>x.toString(16).padStart(2,'0')).join(":").toUpperCase();
  }
  function display(f) {
    let s,d,u,a,z,i,r,x,k,Z,V,t,j,o;
    s=420+f.replaceAll(':','')+0
    d=[]
    a=[u=1];z=[1/0]
    for(i=0;d[i/2]="0x"+s[i]+s[++i],i<512;r=[...d])z[a[i]=(x=2*a[i-1])^(x>255)*285]??=i
    for(k=35;Z=+r[i=0],--k;V={})for(var{abs:B,max:X,min:N}=Math;i<34;t=(x,y=x)=>X(B(x-i),B(y-j)))d[34+i]=r[i]=a['\xFBC.=vF@^ -'.charCodeAt(i)+z[Z]]^r[++i]
    for(i=j=24;~j;j-=j%2^j>6?1:i<u|i-u>24?(j-6?u=-u:i=0,1):j-6?(i-=u,-1):i++&0)V[[i,j]]=i-8&&j-8||16<i+j&i+j<25?"101"[t(18)]??"11010"[N(t(3),t(3,21),t(21,3))]??!(i-6&&j-6&&d[k/8|0]>>7-k++%8&1)^(i+j)%2:4588023&1<<(i-8?24-i:j)
    o='';
    for(i=0;i<25;i++)for(o+=i?'\n':'',j=0;j<25;)o+=+V[[i,j++]]?'\u2588':' '
    document.getElementById("demo-output").textContent=o;
  }
  var input = document.getElementById("demo-input");
  display(input.value = randomHex());
  input.oninput = () => { if (/^(:?[a-fA-F0-9]{2}){32}$/.test(input.value)) display(input.value) };
  document.getElementById("demo-random").onclick = () => display(input.value = randomHex());
</script>
:::

:::full-width
```javascript
s=420+(U=f[F='replace'](/:/g,''))+0
d=[]
a=[u=1];z=[1/0]
for(i=0;d[i/2]="0x"+s[i]+s[++i],i<512;r=[...d])z[a[i]=(x=2*a[i-1])^(x>255)*285]??=i
for(k=35;Z=+r[i=0],--k;V={})for(var{abs:B,max:X,min:N}=Math;i<34;t=(x,y=x)=>X(B(x-i),B(y-j)))d[34+i]=r[i]=a['\xFBC.=vF@^ -'.charCodeAt(i)+z[Z]]^r[++i]
for(i=j=24;~j;j-=j%2^j>6?1:i<u|i-u>24?(j-6?u=-u:i=0,1):j-6?(i-=u,-1):i++&0)V[[i,j]]=i-8&&j-8||16<i+j&i+j<25?"101"[t(18)]??"11010"[N(t(3),t(3,21),t(21,3))]??!(i-6&&j-6&&d[k/8|0]>>7-k++%8&1)^(i+j)%2:4588023&1<<(i-8?24-i:j)
```
:::

Hereʼs a walkthrough of the code:

Letʼs start with the input `f: string`{.ts}, a 32-byte fingerprint encoded as hexadecimal, with colon separators between each byte.
We sandwich it between the strings `"420"`{.js} and `"0"`{.js} to form the raw QR data `s: string`{.ts} in plain hexadecimal:
The nibbles `"420"`{.js} represent the encoding mode (`0b0100`{.js} for encoding bytes) and the length of the encoded data (`0x20 = 32`{.js} bytes), and it needs to be padded with the trailing nibble `"0"`{.js} to be a whole number of octets.
```javascript
s = 420 + (U=f[F='replace'](/:/g,'')) + 0
```
(`U`{.js} and `F`{.js} are useful for golfing the WebRTC connection establishment – not pictured.)

We are going to grab the octets out of it to form 34 bytes in `d`{.js}, but we also pad `d`{.js} with 10 extra bytes for computing the error correction bytes (they end up as `NaN`{.js}s, which mercifully does not matter, and later we overwrite them).
```javascript
d = []
```

These are two tables for Galois fields: `a`{.js} for antilogarithm (exponentiation) and `z`{.js} for logarithm, such that `a[z[i]] = i`{.js} (modulo 256).
They are used to transform the problem of multiplication in the Galois field (which is pretty complicated) into a problem of addition (which is addition modulo 256, very easy).
```javascript
a = [u=1]; z = [1/0]
```
To fill it in, we loop `i`{.js} in the range `0<=i<512`{.js}: the table repeats from 256 onwards, but doing it this way saves computing the modulo 256 later.

We are computing powers `a[i] = 2**i`{.js} in the Galois field \(\operatorname{GF}(256)\) with prime modulus `285 = 0b100011101`{.js}, subtracted using bitwise “or” (`^`{.js}):
```javascript
for (i=0; d[i/2] = "0x"+s[i]+s[++i], i < 512; r=[...d]) //
  z[ a[i] = (x=2*a[i-1]) ^ (x > 255)*285 ] ??= i
```
As you can see, we snuck in the nibbles->bytes conversion on `d`{.js} in the loop, just to save some space. This ends up filling `d`{.js} with 34 bytes of actual data, 10 bytes of `NaN`{.js} padding, more irrelevant bytes of `NaN`{.js}s, and lots and lots of non-integer keys, all the way up to `255.5`{.js}. Note that they are hexadecimal strings and so they need to be pulled out as numbers, using `+d[k]`{.js}!

Now we compute the error correction code, which will be 10 bytes. This is the most mathematically complicated part!

The error correction code operates on polynomials over \(\operatorname{GF}(256)\): each “byte” is a coefficient, with index `r[0]`{.js} being the leading term, and the constant term being in the last index.

We have already copied the data from `d`{.js} into `r`{.js}, so we will loop 34 times to compute the remainder, which will end up being 10 bytes after starting with 34 bytes + 10 zeros (the bytes here are roughly the degree of the polynomial, although leading zeros still count).
We first save the leading coefficient `Z=+r[0]`{.js} and then loop over the remaining coefficients, propagating them leftwards as we transform them.
Although `d`{.js} has a lot of extra junk `NaN`{.js}s that we also copy over, they quickly become zeros from the bitwise arithmetic and do not impact the algorithm.
And because `z[0] = 1/0 = Infinity`{.js}, on the loop iterations when `Z=+r[0]=0`{.js}, it just shifts all relevant items leftwards by one index, removing leading zeros one at a time.

The array `[251,67,46,61,118,70,64,94,32,45][i]`{.js} is encoded as `'\xFBC.=vF@^\x20-'.charCodeAt(i)`{.js}, and it represents the logarithm of the coefficients of the error correction polynomial, minus its leading term (the polynomial is monic, so its leading coefficient is `1`{.js}, whose logarithm is `0`{.js}).
(The polynomial is the multiplication of `(x-0)...(x-9)`{.js}, which we *could* compute … but it is shorter to just embed the coefficients directly, since we do not use polynomial multiplication anywhere else.)
By taking the antilogarithm of the addition of the logarithms, we compute multiplication in \(\operatorname{GF}(256)\). This is where the doubled length of `a`{.js} saves us one modulo operation, and awkward parenthesization to boot.
Altogether, this computes the remainder of dividing the data (represented as a polynomial in \(\operatorname{GF}(256)\)) by the error correction polynomial.
```javascript
for (k = 35; Z = +r[i=0], --k; V={}) //
  for ( var { abs: B, max: X, min: N } = Math; // grab some helpers
        i<34; // loop from i=0 through i=33
        t=(x,y=x)=>X(B(x-i),B(y-j)) // define a function for QR patterns for later
      ) //
    d[34+i] = r[i] = a['\xFBC.=vF@^\x20-'.charCodeAt(i) + z[Z]] ^ r[++i]
```
Since we need to splice the result of error correction, namely `r[0..10]`{.js}, into the bytes after our padded data, namely `d[34..44]`{.js}, we just continuously perform this copying during the loop: only the last time we do this matters. (And of course we continue the tradition of adding junk to `d`{.js} after the data we care about.)

Now we are ready to generate the graphical content of the QR code!

We will write each “module” (pixel) into `V={}`{.js}, where `V["0,0"]`{.js} is the top left, `V["24,0"]`{.js} is the bottom left, `V["0,24"]`{.js} is the top right, and so on.
We use a cute array trick to build these coordinates: `V[[0,0]]`{.js} evaluates to `V["0,0"]`{.js}, since the object index `[0,0]`{.js} gets coerced to a string, joining the stringified components with a comma.
This is shorter than trying to initialize an array with 25 empty arrays.

After a lot of thinking and experimentation, I was able to reduce the QR code generation to one single loop, which is the best way to save characters, since `for`{.js} loops require a lot of characters to set up (and `while`{.js} loops are no better).

The first note is that we have to walk the QR code in a strange way to pull out the data in the correct order:

- We start by walking upwards: `u=1`{.js}, and this corresponds to bit `k=0`{.js} of the data. (These variables were initialized in other places, to save characters.)
- The small-scale zigzag pattern is handled by `j -= 1`{.js} or `j += 1, i -= u`{.js}
- The condition to decide between these is a little annoying: for `j > 6`{.js}, it is odd values `j`{.js}, and for `j < 6`{.js}, it is even values of `j`{.js}. (Column 7 is skipped over, as it has timing patterns.) Thus if `j%2 ^ j>6`{.js} is truthy, we choose `j -= 1`{.js}, else `j += 1, i -= u`{.js}.
- To make the larger-scale zigzag, when we reach the top or bottom edges of the QR code (`i-u < 0 || i-u == 25`{.js}), we decrement `j`{.js}, and swap `u = -u`{.js} to wander the other way.
- There is one special case: along the column `j=6`{.js}, we walk strictly from top to bottom (`i++`{.js}), to pick up the fixed patterns and the timing pattern that fills the rest of the column. When that reaches the bottom, we resume at the top with `i=0`{.js} and `u=-1`{.js} along column `j=5`{.js}.

First we add some static information about the QR codeʼs error correction level (Low) and Mask Pattern (0).
This would be a nightmare to compute dynamically, because they also contain error correction and masking themselves, so instead we embed magic bitstrings encoded as decimal (shorter than hexadecimal).
The magic bitstring is `4588023 = 0b0010001100000000111110111`{.js}, read from right to left and top to bottom.
This information ends up along row 9 and column 9, with gaps in the middle: `16 < i+j && i+j < 25`{.js} is a cute way to encode the gaps without checking which of `i`{.js} or `j`{.js} equals `8`{.js}: if `j=8`{.js}, then it translates to `8 < i < 17`{.js}, and vice-versa.

Next we use a helper `t = (x,y=x) => Math.max(Math.abs(x-y), Math.abs(y-i))`{.js} to compute square patterns around certain locations.
It returns the largest deviation of the current coordinate `[i,j]`{.js} from the center `[x,y]`{.js}, and if it is too high, the pattern will not be applied and the next part of the algorithm will run instead.

- The three finder patterns around `[3,3]`{.js}, `[3,21]`{.js} and `[21,3]`{.js} have a radius of `5`{.js} and the bit pattern `"11010"`{.js}. (These are fixed for all QR codes.)
- The lone alignment pattern at `[18,18]`{.js} has a radius of `3`{.js} and the bit pattern `"101"`{.js}. (Larger versions of QR codes have more alignment patterns.)

Next there are two timing patterns, vertical and horizontal, that run along row 7 and column 7 in the space not taken up by the other patterns.
Again we can combine both lines into one check: `(i+j)%2`{.js}.

Finally, if none of the static patterns apply, we have to pull out the binary data and increment the bit index `k`{.js}:

- The *byte* we are interested in is `d[(k/8) | 0]`{.js} which computes the integer division of `k`{.js} by `8`{.js},
- and the *bit index* is `7 - (k%8)`{.js}, which is the amount we shift the byte by before masking with `& 1`{.js} (while incrementing `k`{.js} inline, which saves some characters).
- (Remember that `d`{.js} now contains the error correcting code from bytes `34`{.js} to `44`{.js}!)
- Finally we negate and XOR it with `!(...) ^ (i+j)%2`{.js} to implement masking pattern 0, which consists of inverting the odd pixels.

Notice that the masking pattern `(i+j)%2`{.js} coincides nicely with the timing pattern: we only compute it once for the both of them!
This is the reason for choosing that masking pattern, and we also happen to save a character with the magic constant `4588023 = 0b0010001100000000111110111`{.js} because of its two leading zeros (which are displayed as white pixels on the right side and the top of the QR code, along row/column 9).
```javascript
for ( // Zigzag from bottom right, up and around to bottom left
      i = j = 24; ~j;                            //
      j -= j%2 ^ j>6 ? 1 :                       //
           i<u|i-u>24 ? (j-6 ? u=-u : i=0,  1) : //
           j-6 ? (i-=u,-1) : i++ & 0             //
    ) //
  V[[i,j]] = //
    i-8 && j-8  ||  16<i+j & i+j<25 // detect metadata or not
    ? "101"[t(18)] ?? // alignment pattern
      "11010"[N(t(3),t(3,21),t(21,3))] ?? // finder patterns
      !(i-6 && j-6 && // i==6 or j==6 for timing patterns
        d[k/8 | 0] >> 7 - k++%8  &  1 // data + error correction, indexed by bit
      ) ^ (i+j)%2 // timing patterns *and* mask pattern
    : 4588023 & 1<<(i-8 ? 24-i : j) // metadata
```
Tada! Now we are left with `V`{.js}, an object whose *numeric* value at index `i+','+j`{.js} is nonzero if the pixel at `i,j`{.js} is black and zeroish if it is white. (You have to pull it out with `+V[[i,j]]`{.js} to cast the `"0"`{.js} strings to a zero value.)


Tricks we used along the way include

- Booleans doubling as numbers, `|`{.js} instead of `||`{.js}, and other such coercions.
- Very careful analysis of [operator precedence](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_precedence#table) to eliminate parentheses.
   It is honestly so ridiculous that `d[k/8|0]>>7-k++%8&1`{.js} works correctly!
- `j-6`{.js} as a subsitute for `j!=6`{.js} when only the truthiness mattered (mostly in ternary conditionals!): e.g. `j-6 ? u=-u : i=0`{.js} means “if `j`{.js} is `6`{.js}, then `i=0`{.js}, else flip the sign of `u`{.js}”.
- `V[[i,j]]`{.js} to encode indices as `i+','+j`{.js}.
- Using static tables like `"101"[t(18)]`{.js} and `[251,67,46,61,118,70,64,94,32,45][i]`{.js} (actually `'\xFBC.=vF@^ -'.charCodeAt(i)`{.js} now) and the bitstring `4588023 & 1<<j`{.js} to cheaply encode choices that change over time.
- Squashing lots of loops together and re-using loop indices, to save on characters.
- One single helper function. Seriously, it wasnʼt worth it for anything else!

In general, choosing as many parts of the QR code standard ahead of time as is possible (fixed size, fixed mask pattern, fixed length, and so on) is the only way this was possible.
And choosing a fixed mask pattern, while technically discouraged, is not even that big of a deal: the fingerprint bytes are essentially random, so odds are that this pattern will be good enough anyways.

All in all, this feels like a really successful project.
It is incredible that it is even possible, and I am even happier that the code crystallized into a somewhat tidy form, as opposed to some of the monsters I created during development.
All the characters I shaved off here are characters I can put towards things like WebRTC connection information in the SDP, or CSS, or error handling, or proper HTML structure.

Of course, most of the journey of how this code evolved and came out, what I was thinking of and what I *wasnʼt* thinking of, is in the less successful attempts at golfing the algorithm:

<details class="Details">

  <summary>Previous revisions</summary>

  These are just the copies I still have lying around, there arenʼt particularly evenly distributed or anything. Some of them have bugs!

  #. 672 characters, my first real proof of concept after a day of hyperfocus. You can see that I used some different methods and initialized the layers of the QR code one at a time, costing four whole loops (but no special cases!):

      ```javascript
      let{abs:B,max:X,min:N}=Math
      a=[1]
      z=[0,0]
      for(i=0;++i<512;)z[a[i]=((x=2*a[i-1])>255)*285^x]??=i
      s='420'+f.replace(/:/g,'')+'0'
      d=[];for(i=0;i<68;)d[i/2]=+("0x"+s[i++]+s[i++])
      r=[...d,..."0000000000"]
      while(X(...r.slice(0,-10))>0)r=r.slice(1).map((x,i)=>x^a[[251,67,46,61,118,70,64,94,32,45][i]+z[r[0]]])
      V=[]
      W=25
      for(i=0;i<W;i+=i-8?1:9)V[V[[8,i]]=27132263&1<<i,[i,8]]=30015987&1<<i
      for(i=W;i-->0;)for(j=W;j-->0;)V[t=(x,y=x)=>X(B(x-i),B(y-j)),[i,j]]??="11010"[N(t(3),t(3,21),t(21,3))]??"101"[t(18)]??(i-6?j-6?V.V:1-i%2:1-j%2)
      k=0;u=1
      i=j=W-1
      while(j>=0)j-=(V[[i,j]]??=[...d,...r.slice(-10)][k/8|0]>>(7-k++%8)&1^(1-i%2),j-(j>6))%2?1:(i-u<0||i-u==W)?(u=-u,j-7?1:2):(i-=u,-1)
      ```

  #. 650 characters:

      ```javascript
      let{abs:B,max:X,min:N}=Math
      a=[1]
      z=[0,0]
      for(i=0;++i<512;z[a[i]=((x=2*a[i-1])>255)*285^x]??=i)d=[]
      s=420+(U=f[F='replace'](/:/g,''))+0
      for(i=0;i<88;)d[i/2]=+("0x"+s[i++]+s[i++])
      for(r=[...d];X(...r.slice(0,-10))>0;r=r.slice(1).map((x,i)=>x^a[[251,67,46,61,118,70,64,94,32,45][i]+z[r[0]]]))W=25
      V=[]
      for(i=0;i<W;i+=i-8?1:9)V[V[[8,i]]=27132263&1<<i,[i,8]]=30015987&1<<i
      for(i=W;i--;d[34+i]=r.at(-10+i))for(j=W;j--;)V[t=(x,y=x)=>X(B(x-i),B(y-j)),[i,j]]??="11010"[N(t(3),t(3,21),t(21,3))]??"101"[t(18)]??(i-6?j-6?V.V:1-i%2:1-j%2)
      k=0;u=1
      for(i=j=W-1;j>=0;j-=(j-(j>6))%2?1:(i-u<0||i-u==W)?(u=-u,j-7?1:2):(i-=u,-1))V[[i,j]]??=d[k/8|0]>>(7-k++%8)&1^(1-i%2)
      ```

  #. 636 characters:

      ```javascript
      let{abs:B,max:X,min:N}=Math
      a=[1]
      z=[0,0]
      for(i=0;++i<512;z[a[i]=((x=2*a[i-1])>255)*285^x]??=i)d=[]
      s=420+(U=f[F='replace'](/:/g,''))+0
      for(i=0;i<88;)d[i/2]=+("0x"+s[i++]+s[i++])
      for(r=[...d];X(...r.slice(0,-10))>0;r=r.slice(1).map((x,i)=>x^a[[251,67,46,61,118,70,64,94,32,45][i]+z[r[0]]]))W=25
      V=[]
      for(i=0;i<W;i+=i-8?1:9)d[34+i]=r.at(-10+i),V[V[[8,i]]=27132263&1<<i,[i,8]]=30015987&1<<i
      k=0;u=1
      t=(x,y=x)=>X(B(x-i),B(y-j))
      for(i=j=W-1;j>=0;j-=(j-(j>6))%2?1:i-u<0||i-u==W?(j-6?u=-u:i=1,1):j-6?(i-=u,-1):(i+=1,0))V[[i,j]]??="11010"[N(t(3),t(3,21),t(21,3))]??"101"[t(18)]??(i-6&&j-6?V.V:(1-i+j)%2),V[[i,j]]??=d[k/8|0]>>(7-k++%8)&1^!(i%2)
      ```

  #. 604 characters. `31327172`{.js} was my favorite number, but it was not canonical and it did not stay long.

      ```javascript
      let{abs:B,max:X,min:N}=Math
      a=[1];z=[0,0]
      for(i=0;++i<512;d=[])z[a[i]=((x=2*a[i-1])>255)*285^x]??=i
      s=420+(U=f[F='replace'](/:/g,''))+0
      for(i=0;i<88;)d[i/2]=+("0x"+s[i++]+s[i++])
      for(r=[...d];X(...r.slice(0,-10))>0;)r=r.slice(1).map((x,i)=>x^a[[251,67,46,61,118,70,64,94,32,45][i]+z[r[0]]])
      V={}
      for(i=0;i<25;i+=i-8?1:9)d[34+i]=r.at(i-10),V[V[[8,i]]=4588023&1<<i,[i,8]]=31327172&1<<i
      t=(x,y=x)=>X(B(x-i),B(y-j))
      k=0;u=1
      for(i=j=24;j+1;j-=j%2^j>6?1:i-u<0||i-u==25?(j-6?u=-u:i=0,1):j-6?(i-=u,-1):(i++,0))V[[i,j]]??="101"[t(18)]??"11010"[N(t(3),t(3,21),t(21,3))]??(i-6&&j-6&&d[k/8|0]>>(7-k++%8)&1)^(1+i+j)%2
      ```

  #. 594 characters:

      ```javascript
      s=420+(U=f[F='replace'](/:/g,''))+0
      d=[]
      a=[1];z=[0,0]
      for(i=0;d[i/2]=+("0x"+s[i]+s[++i]),i<512;)z[a[i]=((x=2*a[i-1])>255)*285^x]??=i
      let{abs:B,max:X,min:N}=Math
      for(r=d.slice(0,44);X(...r.slice(0,-10))>0;)r=r.slice(1).map((x,i)=>x^a[[251,67,46,61,118,70,64,94,32,45][i]+z[r[0]]])
      V={}
      for(i=0;i<25;i+=i-8?1:9)d[34+i]=r.at(i-10),V[[8,i]]=4588023&1<<i,V[[i,8]]=31327172&1<<i
      t=(x,y=x)=>X(B(x-i),B(y-j))
      k=0;u=1
      for(i=j=24;j+1;j-=j%2^j>6?1:i-u<0||i-u==25?(j-6?u=-u:i=0,1):j-6?(i-=u,-1):(i++,0))V[[i,j]]??="101"[t(18)]??"11010"[N(t(3),t(3,21),t(21,3))]??(i-6&&j-6&&d[k/8|0]>>(7-k++%8)&1)^(1+i+j)%2
      ```

  #. 561 characters:

      ```javascript
      s=420+(U=f[F='replace'](/:/g,''))+0
      d=[]
      a=[u=1];z=[1/0,0]
      for(i=0;d[i/2]="0x"+s[i]+s[++i],i<512;)z[a[i]=((x=2*a[i-1])>255)*285^x]??=i
      let{abs:B,max:X,min:N}=Math
      for(r=d.slice(0,44);r[10]+1;r=r.slice(1).map((x,i)=>x^a[[251,67,46,61,118,70,64,94,32,45][i]+z[+r[0]]]))V={}
      for(k=i=0;i<25;i+=i-8?1:9)V[[8,i]]=4588023&1<<i,V[[i,8]]=31326660&1<<i
      t=(x,y=x)=>X(B(x-i),B(y-j))
      for(i=j=24;j+1;j-=j%2^j>6?1:i<u|i-u>24?(j-6?u=-u:i=0,1):j-6?(i-=u,-1):i++&0)V[[i,j]]??="101"[t(18)]??"11010"[N(t(3),t(3,21),t(21,3))]??!(d[34+k]=r[k],i-6&&j-6&&d[k/8|0]>>(7-k++%8)&1)^(i+j)%2
      ```

  #. 553 characters, when I switched to manual iteration for the Galois remainder:

      ```javascript
      s=420+(U=f[F='replace'](/:/g,''))+0
      d=[]
      a=[u=1];z=[1/0]
      for(i=0;d[i/2]="0x"+s[i]+s[++i],i<512;)z[a[i]=((x=2*a[i-1])>255)*285^x]??=i
      r=[...d]
      for(k=35;Z=+r[i=0],--k;V={})for(;i<34;)r[i]=a[[251,67,46,61,118,70,64,94,32,45][i]+z[Z]]^r[++i]
      for(i=0;i<25;i+=i-8?1:9)V[[i,8]]=31326404&1<<i,V[[8,i]]=4588023&1<<i
      let{abs:B,max:X,min:N}=Math
      t=(x,y=x)=>X(B(x-i),B(y-j))
      for(i=j=24;j+1;j-=j%2^j>6?1:i<u|i-u>24?(j-6?u=-u:i=0,1):j-6?(i-=u,-1):i++&0)V[[i,j]]??="101"[t(18)]??"11010"[N(t(3),t(3,21),t(21,3))]??!(d[34+k]=r[k],i-6&&j-6&&d[k/8|0]>>(7-k++%8)&1)^(i+j)%2
      ```

  #. 552 characters, when I consolidated the magic constants:

      ```javascript
      s=420+(U=f[F='replace'](/:/g,''))+0
      d=[]
      a=[u=1];z=[1/0]
      for(i=0;d[i/2]="0x"+s[i]+s[++i],i<512;)z[a[i]=((x=2*a[i-1])>255)*285^x]??=i
      r=[...d]
      for(k=35;Z=+r[i=0],--k;V={})for(;i<34;)r[i]=a[[251,67,46,61,118,70,64,94,32,45][i]+z[Z]]^r[++i]
      for(i=0;i<25;i+=i-7?1:10)V[[8,i]]=V[[24-i,8]]=4588023&1<<i
      let{abs:B,max:X,min:N}=Math
      t=(x,y=x)=>X(B(x-i),B(y-j))
      for(V[[8,8]]=i=j=24;j+1;j-=j%2^j>6?1:i<u|i-u>24?(j-6?u=-u:i=0,1):j-6?(i-=u,-1):i++&0)V[[i,j]]??="101"[t(18)]??"11010"[N(t(3),t(3,21),t(21,3))]??!(d[34+k]=r[k],i-6&&j-6&&d[k/8|0]>>(7-k++%8)&1)^(i+j)%2
      ```

  #. 548 characters:

      ```javascript
      s=420+(U=f[F='replace'](/:/g,''))+0
      d=[]
      a=[u=1];z=[1/0]
      for(i=0;d[i/2]="0x"+s[i]+s[++i],i<512;)z[a[i]=((x=2*a[i-1])>255)*285^x]??=i
      r=[...d]
      for(k=35;Z=+r[i=0],--k;V={})for(;i<34;)r[i]=a[[251,67,46,61,118,70,64,94,32,45][i]+z[Z]]^r[++i]
      for(;i<25;i+=i-7?1:10)V[[8,i]]=V[[24-i,8]]=4588023&1<<i
      let{abs:B,max:X,min:N}=Math
      t=(x,y=x)=>X(B(x-i),B(y-j))
      for(V[[8,8]]=j=--i;j+1;j-=j%2^j>6?1:i<u|i-u>24?(j-6?u=-u:i=0,1):j-6?(i-=u,-1):i++&0)V[[i,j]]??="101"[t(18)]??"11010"[N(t(3),t(3,21),t(21,3))]??!(d[34+k]=r[k],i-6&&j-6&&d[k/8|0]>>(7-k++%8)&1)^(i+j)%2
      ```

  #. 526 characters, when I finally got the QR code down to one loop:

      ```javascript
      s=420+(U=f[F='replace'](/:/g,''))+0
      d=[]
      a=[u=1];z=[1/0]
      for(i=0;d[i/2]="0x"+s[i]+s[++i],i<512;r=[...d])z[a[i]=((x=2*a[i-1])>255)*285^x]??=i
      for(k=35;Z=+r[i=0],--k;V={})for(var{abs:B,max:X,min:N}=Math;i<34;t=(x,y=x)=>X(B(x-i),B(y-j)))r[i]=a[[251,67,46,61,118,70,64,94,32,45][i]+z[Z]]^r[++i]
      for(i=j=24;d[34+k]=r[k],~j;j-=j%2^j>6?1:i<u|i-u>24?(j-6?u=-u:i=0,1):j-6?(i-=u,-1):i++&0)V[[i,j]]=i-8&&j-8||16<i+j&i+j<25?"101"[t(18)]??"11010"[N(t(3),t(3,21),t(21,3))]??!(i-6&&j-6&&d[k/8|0]>>(7-k++%8)&1)^(i+j)%2:4588023&1<<(i-8?24-i:j)
      ```

  #. 518 characters (final? polished? edition):

      ```javascript
      s=420+(U=f[F='replace'](/:/g,''))+0
      d=[]
      a=[u=1];z=[1/0]
      for(i=0;d[i/2]="0x"+s[i]+s[++i],i<516;r=[...d])z[a[i]=(x=2*a[i-1])^(x>255)*285]??=i
      for(k=35;Z=+r[i=0],--k;V={})for(var{abs:B,max:X,min:N}=Math;i<34;t=(x,y=x)=>X(B(x-i),B(y-j)))d[34+i]=r[i]=a[[251,67,46,61,118,70,64,94,x,45][i]+z[Z]]^r[++i]
      for(i=j=24;~j;j-=j%2^j>6?1:i<u|i-u>24?(j-6?u=-u:i=0,1):j-6?(i-=u,-1):i++&0)V[[i,j]]=i-8&&j-8||16<i+j&i+j<25?"101"[t(18)]??"11010"[N(t(3),t(3,21),t(21,3))]??!(i-6&&j-6&&d[k/8|0]>>7-k++%8&1)^(i+j)%2:4588023&1<<(i-8?24-i:j)
      ```

  And as an addendum since you were nice enough to scroll through here, the silly code that renders it to HTML!

  ```html
  <style>pre{line-height:1;padding:3em;transform:scaleY(0.6)}</style>
  ```

  ```javascript
  o=f+'<pre>'
  for(i=0;i<25;i++)for(o+='\n',j=0;j<25;)o+=+V[[i,j++]]?'\u2588':' '
  document.body.innerHTML=o
  ```

</details>

And some links to helpful resources about QR codes:

- https://qr.blinry.org/
- https://www.nayuki.io/page/creating-a-qr-code-step-by-step
- https://www.thonky.com/qr-code-tutorial/

<style>
  #demo > pre {
    font-size: 8px;
    line-height: 1;
    padding: 3em;
    user-select: none;
    transform: scaleY(0.6);
    transform-origin: top center;
    color: black;
    background: white;
    width: fit-content;
    margin: auto;
    margin-bottom: -10em;
  }
  pre.sourceCode:not([data-lang]).javascript::before {
    content: '';
    margin-bottom: 0;
  }
  pre.sourceCode:not([data-lang]).javascript {
    white-space: pre-wrap;
    overflow-wrap: anywhere;
    max-width: 100%;
  }
</style>
