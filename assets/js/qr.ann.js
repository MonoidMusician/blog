// # Code golf for computing a 32-byte QR code!
//
// If you run `.replaceAll(/ *(\/\/[^\n]*\n)?/g, '').replace('\\x20',' ').length` on this fileʼs contents, you will see that this JavaScript comes in at 512 ASCII characters when minified!
// (Newlines without comments are still used as substitutes for semicolons, to keep the minified file somewhat readable. Other spaces are not important, except for the one in the string.)
//
// Why would I do such a thing? Well, it helps me bootstrap a WebRTC connection over QR codes!
// The host generates a HTML stub that contains this JavaScript, allowing it to display the fingerprint of the guestʼs generated certificate.
//
// There is a hard limit of 2,953 bytes of UTF-8 for QR codes, and I have even less to work with considering that I have to encode it into a `data:` URI, so having a compact algorithm for this is essential.
//
// Hereʼs a walkthrough of the code:
//
// Letʼs start with the input `f: string`, a 32-byte fingerprint encoded as hexadecimal, with colon separators between each byte.
// We sandwich it between the strings `"420"` and `"0"` to form the raw QR data `s: string` in plain hexadecimal:
// The nibbles `"420"` represent the encoding mode (`0b0100` for encoding bytes) and the length of the encoded data (`0x20 = 32` bytes), and it needs to be padded with the trailing nibble `"0"` to be a whole number of octets.
s = 420 + (U=f[F='replace'](/:/g,'')) + 0
// (`U` and `F` are useful for golfing the WebRTC connection establishment – not pictured.)
//
// We are going to grab the octets out of it to form 34 bytes in `d`, but we also pad `d` with 10 extra bytes for computing the error correction bytes (they end up as `NaN`s, which mercifully does not matter, and later we overwrite them).
d = []
//
// These are two tables for Galois fields: `a` for antilogarithm (exponentiation) and `z` for logarithm, such that `a[z[i]] = i` (modulo 256).
// They are used to transform the problem of multiplication in the Galois field (which is pretty complicated) into a problem of addition (which is addition modulo 256, very easy).
a = [u=1]; z = [1/0]
// To fill it in, we loop `i` in the range `0<=i<512`: the table repeats from 256 onwards, but doing it this way saves computing the modulo 256 later. We actually extend it to `i=515` so that the loop ends with `x=32`: this saves one character when we happen to need that number later. After all, we are computing powers of two already!
//
// We are computing powers `a[i] = 2**i` in the Galois field GF(256) with prime modulus `285 = 0b100011101`, subtracted using bitwise “or” (`^`):
for (i=0; d[i/2] = "0x"+s[i]+s[++i], i < 512; r=[...d]) //
  z[ a[i] = (x=2*a[i-1]) ^ (x > 255)*285 ] ??= i
// As you can see, we snuck in the nibbles->bytes conversion on `d` in the loop, just to save some space. This ends up filling `d` with 34 bytes of actual data, 10 bytes of `NaN` padding, more irrelevant bytes of `NaN`s, and lots and lots of non-integer keys, all the way up to `255.5`. Note that they are hexadecimal strings and so they need to be pulled out as numbers, using `+d[k]`!
//
// Now we compute the error correction code, which will be 10 bytes. This is the most mathematically complicated part!
//
// The error correction code operates on polynomials over GF(256): each “byte” is a coefficient, with index `r[0]` being the leading term, and the constant term being in the last index.
//
// We have already copied the data from `d` into `r`, so we will loop 34 times to compute the remainder, which will end up being 10 bytes after starting with 34 bytes + 10 zeros (the bytes here are roughly the degree of the polynomial, although leading zeros still count).
// We first save the leading coefficient `Z=+r[0]` and then loop over the remaining coefficients, propagating them leftwards as we transform them.
// Although `d` has a lot of extra junk `NaN`s that we also copy over, they quickly become zeros from the bitwise arithmetic and do not impact the algorithm.
// And because `z[0] = 1/0 = Infinity`, on the loop iterations when `Z=+r[0]=0`, it just shifts all relevant items leftwards by one index, removing leading zeros one at a time.
//
// The array `[251,67,46,61,118,70,64,94,32,45][i]` is encoded as `'\xFBC.=vF@^\x20-'.charCodeAt(i)`, and it represents the logarithm of the coefficients of the error correction polynomial, minus its leading term (the polynomial is monic, so its leading coefficient is `1`, whose logarithm is `0`).
// (The polynomial is the multiplication of `(x-0)...(x-9)`, which we *could* compute … but it is shorter to just embed the coefficients directly, since we do not use polynomial multiplication anywhere else.)
// By taking the antilogarithm of the addition of the logarithms, we compute multiplication in GF(256). This is where the doubled length of `a` saves us one modulo operation, and awkward parenthesization to boot.
// Altogether, this computes the remainder of dividing the data (represented as a polynomial in GF(256)) by the error correction polynomial.
for (k = 35; Z = +r[i=0], --k; V={}) //
  for ( var { abs: B, max: X, min: N } = Math; // grab some helpers
        i<34; // loop from i=0 through i=33
        t=(x,y=x)=>X(B(x-i),B(y-j)) // define a function for QR patterns for later
      ) //
    d[34+i] = r[i] = a['\xFBC.=vF@^\x20-'.charCodeAt(i) + z[Z]] ^ r[++i]
// Since we need to splice the result of error correction, namely `r[0..10]`, into the bytes after our padded data, namely `d[34..44]`, we just continuously perform this copying during the loop: only the last time we do this matters. (And of course we continue the tradition of adding junk to `d` after the data we care about.)
//
// Now we are ready to generate the graphical content of the QR code!
//
// We will write each “module” (pixel) into `V={}`, where `V["0,0"]` is the top left, `V["24,0"]` is the bottom left, `V["0,24"]` is the top right, and so on.
// We use a cute array trick to build these coordinates: `V[[0,0]]` evaluates to `V["0,0"]`, since the object index `[0,0]` gets coerced to a string, joining the stringified components with a comma.
// This is shorter than trying to initialize an array with 25 empty arrays.
//
// After a lot of thinking and experimentation, I was able to reduce the QR code generation to one single loop, which is the best way to save characters, since `for` loops require a lot of characters to set up (and `while` loops are no better).
//
// The first note is that we have to walk the QR code in a strange way to pull out the data in the correct order:
// - We start by walking upwards: `u=1`, and this corresponds to bit `k=0` of the data. (These variables were initialized in other places, to save characters.)
// - The small-scale zigzag pattern is handled by `j -= 1` or `j += 1, i -= u`
// - The condition to decide between these is a little annoying: for `j > 6`, it is odd values `j`, and for `j < 6`, it is even values of `j`. (Column 7 is skipped over, as it has timing patterns.) Thus if `j%2 ^ j>6` is truthy, we choose `j -= 1`, else `j += 1, i -= u`.
// - To make the larger-scale zigzag, when we reach the top or bottom edges of the QR code (`i-u < 0 || i-u == 25`), we decrement `j`, and swap `u = -u` to wander the other way.
// - There is one special case: along the column `j=6`, we walk strictly from top to bottom (`i++`), to pick up the fixed patterns and the timing pattern that fills the rest of the column. When that reaches the bottom, we resume at the top with `i=0` and `u=-1` along column `j=5`.
//
// First we add some static information about the QR codeʼs error correction level (Low) and Mask Pattern (0).
// This would be a nightmare to compute dynamically, because they also contain error correction and masking themselves, so instead we embed magic bitstrings encoded as decimal (shorter than hexadecimal).
// The magic bitstring is `4588023 = 0b0010001100000000111110111`, read from right to left and top to bottom.
// This information ends up along row 9 and column 9, with gaps in the middle: `16 < i+j && i+j < 25` is a cute way to encode the gaps without checking which of `i` or `j` equals `8`: if `j=8`, then it translates to `8 < i < 17`, and vice-versa.
//
// Next we use a helper `t = (x,y=x) => Math.max(Math.abs(x-y), Math.abs(y-i))` to compute square patterns around certain locations.
// It returns the largest deviation of the current coordinate `[i,j]` from the center `[x,y]`, and if it is too high, the pattern will not be applied and the next part of the algorithm will run instead.
//
// - The three finder patterns around `[3,3]`, `[3,21]` and `[21,3]` have a radius of `5` and the bit pattern `"11010"`. (These are fixed for all QR codes.)
// - The lone alignment pattern at `[18,18]` has a radius of `3` and the bit pattern `"101"`. (Larger versions of QR codes have more alignment patterns.)
//
// Next there are two timing patterns, vertical and horizontal, that run along row 7 and column 7 in the space not taken up by the other patterns.
// Again we can combine both lines into one check: `(i+j)%2`.
//
// Finally, if none of the static patterns apply, we have to pull out the binary data and increment the bit index `k`:
// - The *byte* we are interested in is `d[(k/8) | 0]` which computes the integer division of `k` by `8`,
// - and the *bit index* is `7 - (k%8)`, which is the amount we shift the byte by before masking with `& 1` (while incrementing `k` inline, which saves some characters).
// - (Remember that `d` now contains the error correcting code from bytes `34` to `44`!)
// - Finally we negate and XOR it with `!(...) ^ (i+j)%2` to implement masking pattern 0, which consists of inverting the odd pixels.
//
// Notice that the masking pattern `(i+j)%2` coincides nicely with the timing pattern: we only compute it once for the both of them!
// This is the reason for choosing that masking pattern, and we also happen to save a character with the magic constant `4588023 = 0b0010001100000000111110111` because of its two leading zeros (which are displayed as white pixels on the right side and the top of the QR code, along row/column 9).
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
//
// Tada! Now we are left with `V`, an object whose *numeric* value at index `i+','+j` is nonzero if the pixel at `i,j` is black and zeroish if it is white. (You have to pull it out with `+V[[i,j]]` to cast the `"0"` strings to a zero value.)
//
//
// Tricks we used along the way include
// - Booleans doubling as numbers, `|` instead of `||`, and other such coercions.
// - Very careful analysis of [operator precedence](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_precedence#table) to eliminate parentheses.
//   It is honestly so ridiculous that `d[k/8|0]>>7-k++%8&1` works correctly!
// - `j-6` as a subsitute for `j!=6` when only the truthiness mattered (mostly in ternary conditionals!): e.g. `j-6 ? u=-u : i=0` means “if `j` is `6`, then `i=0`, else flip the sign of `u`”.
// - `V[[i,j]]` to encode indices as `i+','+j`.
// - Using static tables like `"101"[t(18)]` and `[251,67,46,61,118,70,64,94,32,45][i]` (actually `'\xFBC.=vF@^ -'.charCodeAt(i)` now) and the bitstring `4588023 & 1<<j` to cheaply encode choices that change over time.
// - Squashing lots of loops together and re-using loop indices, to save on characters.
// - One single helper function. Seriously, it wasnʼt worth it for anything else!
//
// In general, choosing as many parts of the QR code standard ahead of time as is possible (fixed size, fixed mask pattern, fixed length, and so on) is the only way this was possible.
// And choosing a fixed mask pattern, while technically discouraged, is not even that big of a deal: the fingerprint bytes are essentially random, so odds are that this pattern will be good enough anyways.
//
// All in all, this feels like a really successful project.
// It is incredible that it is even possible, and I am even happier that the code crystallized into a somewhat tidy form, as opposed to some of the monsters I created during development.
// All the characters I shaved off here are characters I can put towards things like WebRTC connection information in the SDP, or CSS, or error handling, or proper HTML structure.
