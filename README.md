# How to use

Just compile whatever Theorem to check its proof importing these ProofChecker and Terms and verify it.

Example
```
import ProofChecker
import Terms

verify = let theorem = (p <==> q) <==> q === p in
         proof theorem
         >>=
         statement 3.1 with (q =: r) using lambda z (z)
         >>=
         statement 3.3 with (q =: p) using lambda z (p <==> z)
         >>=
         statement 3.4 with (p =: p) using lambda z (z)
         >>=
         done theorem
```
At the end it will show if the proof was successful or not, and point out the first inference error that might have occurred.
