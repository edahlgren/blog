title: Hash collisions for the skeptical
author: E. Dahlgren

"**Gah**" you say.  "I should probably be using something other than the dumbest way ever to resolve hash collisions in my super fast blah blah blah."  Or should you?  A good hash collision resolution strategy:

>*Minimizes probing through your hash structure*

>*Minimizes data cache misses*

Why do these matter?

Speed!  The more steps you take --- the more indices you check --- the farther your inserts and lookups get from O(1).  The farther apart your key's first hash location is from its new home in memory, the more cache lines you'll need to load into your data cache.  This takes time!  You're writing a super fast blah blah blah, remember?

"Well, I just want to choose a strategy."  Alright, then let's have a method for doing so.  For each strategy:

* Insert and check the membership of *n* keys, where *n* is the size of the hash structure.  Output probe counts for both ```insert``` and ```member```.  The lowest mean, median, and variance wins. *n*=4KB

* Use ```valgrind --tool=cachegrind``` to profile cache miss rate with a virtual 32KB data cache.  The lowest cache miss rate wins. *n*=64KB, so the structure doesn't fit in the data cache.

The strategies implemented:

*Chaining.*  Each index in your hash structure holds a linked structure (list or tree).  Simply add your key to the end of the linked structure.  Dead simple. [code](https://github.com/edahlgren/collisiontest/blob/master/chaining.c)

*Linear probing.*  Check the next index until you find an open place.  Iterate through each index (starting at the one your key hashes to) to find it again, iterating through all keys if it's not there.  Also really simple. [code](https://github.com/edahlgren/collisiontest/blob/master/linear.c)

*Quadratic probing.*  Check the next *n*th index until you find a place.  *N* is determined by some quadratic function, so you'll hop around the hash structure in wider strides. [code](https://github.com/edahlgren/collisiontest/blob/master/quadratic.c)

*Double hashing.*  Check the next *n*th index until you find a place.  *N* is determined by another hash function, so you'll hop around the hash structure in different strides per key. [code](https://github.com/edahlgren/collisiontest/blob/master/double.c)

*Robin hood hashing.*  The idea is to evict keys from their place, and take their place, if you've traveled farther away from your original hash index than they have from theirs.  Steal from the rich (the stupid bastards in your search path), and give to the poor (you who have traveled far). [code](https://github.com/edahlgren/collisiontest/blob/master/robinhood.c)

**Probe Count.**

<div style="display:block; margin-top: 25px; margin-left:auto; margin-right:auto;">
<div style="display:block; float:left;">
Chaining
<table>
<tr><td></td><td style="color:#0000CC;">insert</td><td style="color:#0000CC;">member</td></tr>
<tr><td>mean</td><td>0.493</td><td>0.493</td></tr>
<tr><td>median</td><td>0.000</td><td>0.000</td></tr>
<tr><td>variance</td><td>0.565</td><td>0.565</td></tr>
<tr><td>deviation</td><td>0.751</td><td>0.751</td></tr>
<tr><td>max</td><td>4.000</td><td>4.000</td></tr>
</table>

Quadratic probing
<table>
<tr><td></td><td style="color:#0000CC;">insert</td><td style="color:#0000CC;">member</td></tr>
<tr><td>mean</td><td>6.891</td><td>6.891</td></tr>
<tr><td>median</td><td>1.000</td><td>1.000</td></tr>
<tr><td>variance</td><td>3191.905</td><td>3191.905</td></tr>
<tr><td>deviation</td><td>56.496</td><td>56.496</td></tr>
<tr><td>max</td><td>2982.000</td><td>2982.000</td></tr>
</table>

Robin hood hashing
<table>
<tr><td></td><td style="color:#0000CC;">insert</td><td style="color:#0000CC;">member</td></tr>
<tr><td>mean</td><td>1.554</td><td>30.752</td></tr>
<tr><td>median</td><td>1.000</td><td>27.000</td></tr>
<tr><td>variance</td><td>2.062</td><td>357.251</td></tr>
<tr><td>deviation</td><td>1.436</td><td>18.901</td></tr>
<tr><td>max</td><td>76.000</td><td>87.000</td></tr>
</table>
</div>

<div style="display:block;">
Linear probing
<table>
<tr><td></td><td style="color:#0000CC;">insert</td><td style="color:#0000CC;">member</td></tr>
<tr><td>mean</td><td>30.752</td><td>30.752</td></tr>
<tr><td>median</td><td>1.000</td><td>1.000</td></tr>
<tr><td>variance</td><td>27576.575</td><td>27576.575</td></tr>
<tr><td>deviation</td><td>166.061</td><td>166.061</td></tr>
<tr><td>max</td><td>3153.000</td><td>3153.000</td></tr>
</table>

Double hashing
<table>
<tr><td></td><td style="color:#0000CC;">insert</td><td style="color:#0000CC;">member</td></tr>
<tr><td>mean</td><td>6.663</td><td>6.663</td></tr>
<tr><td>median</td><td>1.000</td><td>1.000</td></tr>
<tr><td>variance</td><td>3450.241</td><td>3450.241</td></tr>
<tr><td>deviation</td><td>58.738</td><td>58.738</td></tr>
<tr><td>max</td><td>2014.000</td><td>2014.000</td></tr>
</table>
</div>
</div>

<div style="width:500px; display:inline-block;"></div>
First notice that all strategies except for robin hood hashing have the same statistics for both ```insert``` and ```member```.  Why?  In robing hood hashing, once you insert a key it will probably be evicted from that index one or times.  That means when you go to find it, you'll probably have to probe farther than you did to insert it.  *Any insert can induce many evictions.*

In robin hood hashing this shows up as ```member``` performing considerably worse than ```insert``` (the median is 27 times greater, and the variance skyrockets!).  If your super fast blah blah blah is anything like a cache or ring, you'll be doing more reads (lookups) than writes (inserts).

But the sky turns grey when we look at linear probing, quadratic probing, and double hashing.  All three of these strategies are know to perform badly at >70% load [1]. We filled the hash structures up to 100%. Linear probing performs particularly ingloriously because keys with similar probe sequences clump: creating an uneven number of dangerious patches that cause lots of probing.  Quadratic probing and double hashing perform better because they take wider strides over the hash structure, effectively "stepping over" these key clumps.  While their mean probe counts are low (< 7), the variance is still much higher than robin hood hashing.

How about chaining?  That dead simple strategy, remember?  Chaining gets a lot of heat for wasting space: After you ```insert``` *n* keys into your size *n* hash structure, you could very well have many indices totally empty.  There's no mechanism for distributing the tails of the linked list into the space you've preallocated for the hash structure.  But do we really care?  The mean probe count is well under 1 and the variance is incredibly low.  Clearly the winner so far.

**Cache Misses.**

<div style="margin-top:25px;">
Data cache
<table>
<tr><td></td><td>data refs</td><td>miss rate</td></tr>
<tr><td>chaining</td><td>109,316,426</td><td style="color:#0000CC;">0.3%</td</tr>
<tr><td>linear probing</td><td>265,519,800</td><td style="color:#0000CC;">3.8%</td></tr>
<tr><td>quadratic probing</td><td>127,420,471</td><td style="color:#0000CC;">1.9%</td></tr>
<tr><td>double hashing</td><td>128,662,471</td><td style="color:#0000CC;">2.1%</td></tr>
<tr><td>robin hood hashing</td><td>1,712,635,748</td><td style="color:#0000CC;">0.5%</td></tr>
</table>
</div>

At first glance the cache miss rates look incorrect.  Linear probing, with its inorder search of our preallocated structure, should have the most cache line hits, right?  And if linear probing has the best cache line hit rate, then total cache misses should be lowest, right?

Well, if we weren't loading tons of cache lines into memory anyway, essentially if both our mean probe count and our probe count *variance* were low, yep you'd be right.  But right now it seems that the cache benefits of the linear probing strategy are totally overwhelmed by high probe counts.  This could be made better if we were to fill our hash structure half full.  By why give chaining so much heat if you're now wasting 50% of your space?

Quadratic probing has a cache miss rate twice as small as linear probing.  This means that despite making wider jumps around the hash structure, meaning that hitting the same cache line twice in a row was less probable, the decreased probe count seems to have made a huge difference.  The same story can be said about double hashing, but less so because the wider jumps were less regular (the jump size varied by key).

Robin hood hashing and chaining performed marvelously, at less than 1% cache miss rate.  Theoretically robin hood hashing has better locality than chaining: If it takes very little probing to insert or lookup each element *stored in our preallocated hash structure*, then we're going to load few cache lines into our data cache.

In chaining, the only elements that are guaranteed good locality are the heads of the lists (stored in the preallocated hash structure).  And these aren't the elements we're probing!  We're probing through the elements of the linked lists, which are allocated individually.  How did chaining still beat robin hood hashing?

The answer: Remeber that with robin hood hashing, every insertion can induce many evictions?  Even though the mean probe count and variance are low, robin hood hashing still causes us to load more cache lines than chaining does simply because each insert often triggers a flood of other inserts, sometimes leading us far away from our starting index.

>*Do you care about speed?*<br>
>Keep it simple, just use chaining.

>*Do you care about using your entire hash structure?*<br>
>Use chaining with a good hash function, but if you must, use robin hood hashing.

>[1] [Linear Probing with Constant Independence](http://www.it-c.dk/people/pagh/papers/linear.pdf)<br>
>[Hash table: Collision resolution (wikipedia)](http://en.wikipedia.org/wiki/Hash_table#Collision_resolution)<br>
>[Choosing a quadratic probing function](http://stackoverflow.com/a/2349774/1672086)<br>
>[Choosing a double hashing function](http://en.wikipedia.org/wiki/Double_hashing)<br>
>[Robin hood hashing](http://sebastiansylvan.com/2013/05/08/robin-hood-hashing-should-be-your-default-hash-table-implementation)<br>
>[Gallery of processor cache effects](http://igoro.com/archive/gallery-of-processor-cache-effects/)<br>
>[Measuring cache misses](http://csqlcache.wordpress.com/2008/08/25/measuring-cache-misses/)
