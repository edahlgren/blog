title: Hash functions for the weary
author: E. Dahlgren

"**Damnit**", you say.  "Building this hash ring or routing this data means that I need to find the fastest hash function that isn't horrible."  The reason you've said "Damnit!" is because what makes a hash function horrible is knowledge you've popped off your memory stack long ago.  Simply put, a non-horrible hash function:

>Distributes your keys randomly (evenly) across your key space

>Avoids collisions with other keys

>Maybe is irreversible

>Maybe compresses your key into a checksum

>Is quick!

What do you care about?  Maybe you care that all your machines have the same load, so you care that your keys hash to a uniform distribution.  Collisions don't matter to you because you're not storing anything about keys that hash to the same place.  Maybe you want to avoid iterating through a data structure when multiple values are stored at the same place, so you care about minimizing key collisions.  Maybe you care about garaunteeing that two messages aren't the same very quickly.  Maybe you care about how easy it would be to brute force find a collision for your hash.  Irreversable output (160 bit large) matters most to you.  But always, you care about "Moar speed!"

It's easy to evaulate a hash function for speed (clock those cpu seconds), or output size (derp de derp, how many bytes is this thing?).  Less obvious, a hash function produces a uniform distribution if:

>Each pattern of output bits is equally likely

A hash function avoids collisions when:

>Changing a single input bit drastically affects output bits

When you hear people talking about *avalanche*, this is what they mean.  Formally, "drastically" means that if an input bit changes, each output bit should change with a 50% probability.  When an input bit has a 0% or 100% affect on an output bit, we say that the output bit isn't *mixed*.  If this happens to a lot of your bits, then your hashing function *mixes poorly*.  Your hash function will also avoid collisions if:

>Output bits are independent of one another

Those are the three properities you're looking for if you want your hash function to produce uniformly distributed and collision resistant output.  Now that we know what we're looking for and what we care about, let's look at some options:

**CRC32.**  Standing for cyclic redundancy check, this simply produces a 32-bit "tl;dr" of your message (a [checksum](http://en.wikipedia.org/wiki/Checksum)), useful to checking whether your message was transmitted with errors.  It's computed by continuously dividing a smaller and smaller slices of the input by a polynomial, eventually resulting in a 32-bit remainder [1].  *Pros.*  Since your polynomial is usually fixed, one can write very fast CRC implementations involving pre-computed lookup tables.  *Cons.*  A chi-squared test [2] shows that only half of the CRC32's output bits were uniformly distributed; each input bit affects each output bit at less than 33% probability or greater than 66% probability (we want 50%), meaning that it has abysmal collision resistance [2].

**FNV.** Standing for its creators: (Glenn) Fowler/(Landon Cur) Noll/(Phong) Vo, FNV is a very simple hash function.  The idea is to take a starting seed, and continually multiply it by a specific prime and XOR it with the next byte of your input.  *Pros.*  The simplicity of the algorithm makes it fast enough to be used as a checksum.  *Cons.*  Its simplicity makes it trivial to brute force find collisions.  A chi-sqaured test shows that output bits are uniformly distributed up to only 2^14 [3].  It also appears that the last byte of the input does not cause any mixing at all [3], meaning that this function has poor collision resistance.

**SIPHASH.** The hipster on the block, SipHash is a relatively new hash function (2012) which boasts to have better collision resistance than FNV (not unimaginable) and to be just as fast or faster. There are few benchmarks available, some of them only comparing Cycles/Byte (and only between SipHash and several [hmac](https://en.wikipedia.org/wiki/Hash-based_message_authentication_code) implementations) [4], or language internal improvements over FNV [5].  I don't have enough information to recommend this function in terms of uniform distribution or collision resistance, but its speed looks comparable to FNV.

**JENKINS.** Named after its creator Bob Jenkins, this hash function mixes keys 12 bytes at a time [6].  The mixing is a complicated sequence of shifts, adds, and XORs, making it non-trivial to implement.  *Pros.*  It produces uniformly distributed output, and has excellent collision resistance due to good mixing (every input bit causes a change in every output bit at 33-66% probability) [7].  Since the mixing can be done on parallel processors, it is still relatively fast.  *Cons.* It is not cryptographically secure and it is easy to implement incorrectly.

**SHA-1.** Standing for Secure Hash Algorithm, this hash function produces cryptographic (hopefully irreversable) 160-bit output.  We can compare SHA-1 to our other algorithms (which can produce 32-bit output) by XOR-ing the five 32-bit sections of the 160-bit output together.  *Pros.*  Cryptographic hashes need to be uniformly distributed and they need to mix well, or else finding collisions by brute force would be possible.  SHA-1 has good mixing like Jenkins and produces output that is equally uniformly distributed [8].  It is also mostly cryptographically secure [9].  *Cons.*  Computing a 160-bit hash takes cpu seconds any way you slice it, so it's totally unnecessary if security isn't required.

Know what you want your hash function to do, and what you can compromise to get there!

>[1] [Cylic redundancy check: wikipedia](http://en.wikipedia.org/wiki/Cyclic_redundancy_check)<br>
>[2] [CRC32 test: Bret Mulvey](http://home.comcast.net/~bretm/hash/8.html)<br>
>[3] [FNV test: Bret Mulvey](http://home.comcast.net/~bretm/hash/6.html)<br>
>[4] [SipHash Bytes/Cycle benchmarks](http://bench.cr.yp.to/results-auth.html)<br>
>[5] [SipHash Haskell benchmarks](http://www.serpentine.com/blog/2012/10/02/a-fast-new-siphash-implementation-in-haskell/)<br>
>[6] [Jenkins hash function: wikipedia](http://en.wikipedia.org/wiki/Jenkins_hash_function)<br>
>[7] [Jenkins test: Bret Mulvey](http://home.comcast.net/~bretm/hash/7.html)<br>
>[8] [SHA-1 test: Bret Mulvey](http://home.comcast.net/~bretm/hash/9.html)<br>
>[9] [Bruce Schneier on SHA-1](http://www.schneier.com/blog/archives/2005/02/cryptanalysis_o.html)
