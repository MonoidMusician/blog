---
title: Information theory and merge sort
---

Fact:
There are \(n!\) permutations of a list with \(n\) elements.
This means that if you have an optimal representation, it would take \(\log_2 n!\) bits to describe a particular permutation.

Fact:
Optimal sorting algorithms operate in \(Θ(n \log_2 n)\) time.

What can we do with these two bits of knowledge?
Calculate the average information consumed by each step of a sorting algorithm!

---

Letʼs take merge sort as our algorithm of choice.
It does some array manipulation and copying, that takes linear time, so we will neglect it.
Itʼs the comparisons that take \(Θ(n \log_2 n)\) time (best-, worst-case, and average time complexity).

I had to verify that \(n \log_2 n\) is the actual number of comparisons for the worst case, as opposed to just being proportional, based on [these lecture notes](https://www.cs.auckland.ac.nz/compsci220s1c/lectures/2016S1C/CS220-Lecture09.pdf#Navigation32).

(More precisely, if \(n = 2^u\) is a power of \(2\), then it takes \(n \log_2 n = u 2^u\) comparisons.)

But if we put this together, it means that:

:::{.Key_Idea .centered .fit-content-width}
In the worst-case behavior of <br> an optimal sorting algorithm <br> (like merge sort),

Each comparison consumes
\[\frac{\log_2 n!}{n \log_2 n}\]
bits of information.
:::

It turns out this is asymptotically 1 bit of information.
This makes sense!
Each comparison is a binary decision, thus producing 1 bit of information, so we would hope that it is extracting almost 1 bit of information from the permutation.

---

We can break this formula down in a few ways.

The factorial \(n!\) is a bunch of multiplications, so we can express its logarithm as a bunch of additions:

\[
\frac
  {\log_2 n + \cdots + \log_2 2 + \log_2 1}
  {\log_2 n + \cdots + \log_2 n + \log_2 n}
\]

This explains that it is always less than 1 bit.

\[
\begin{align}
  \frac {\ln(n!)}{n \ln(n)} &\approx \frac {\ln(\sqrt n \left (\frac ne\right )^n)}{n \ln(n)} \\
  &= \frac {\ln(\sqrt n) + \ln(\left (\frac ne\right )^n)}{n \ln(n)} \\
  &= \frac {\frac 12 \ln(n) + n \ln(\frac ne)}{n \ln(n)} \\
  &= \frac {\frac 12 \ln(n) + n \ln(n) - n \ln(e)}{n \ln(n)} \\
  &= \frac {\frac 12 \ln(n)}{n \ln(n)} + \frac {n \ln(n)}{n \ln(n)} - \frac {n \ln(e)}{n \ln(n)} \\
  &= \frac 1{2 n} + 1 - \frac {\ln(e)}{\ln(n)} \\
  &= \frac 1{2 n} + 1 - \frac 1{\ln(n)} \\
  \\ \\
  \frac {\ln(n!)}{n \ln(n)} &= \frac {\ln(n (n - 1)!)}{n \ln(n)} \\
  &= \frac {\ln(n) + \ln((n - 1)!)}{n \ln(n)}
\end{align}
\]

---

It would be interesting to have more detailed breakdowns of this.
What about average case? How is it distributed? And so on.
