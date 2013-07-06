title: Ring balancing for weary
author: E. Dahlgren

"**Ugh**, Statistics!" you say, "I became a programmer in order to avoid this shit."  Me too, my friend, me too.  But there comes a time when you want to balance load between servers in a [hash ring](http://www.tom-e-white.com/2007/11/consistent-hashing.html) and you end up wondering "How many replicas of my servers do I need to be confident that my uniformly distributing hash function disperses those replicas evenly enough?"  What you'd like is simply a function that takes the size of your key space, the number of servers you want to store in the ring, and returns the number of replications per server to maintain a nearly uniform distribution.  Then you could move on with your life.

What does perfect look like?  If your servers hash to 32-bit integers, then your hash ring has 2^32 potential hashes.  Three servers uniformly distributed in a 32-bit space would be (2^32)/3 32-bit integers apart.

"When we add a new server, why don't we just move all of the existing servers along the ring a little to make room for the new one?"  Sorry.  The point of using a consistent hash ring is to be able to remap as few data keys to new servers as possible when a server is added or deleted.  If we changed the hash position of every server in the ring, we'd likely be remapping a lot of data keys to new servers.

>*Constraint.* Move as few servers on the hash ring as possible.

**Lazy.**  What's the easiest way to be certain you have a uniform distribution over a space?  Fill ALL the spaces!  Imagine you're using a 16-bit hash space.  This isn't crazy, since you're only storing your servers in your hash ring usually.  A 16-bit hash space means 2^16 or 65536 potential hashes.  If you have no more than 100 servers, you could "replicate" --- hash unique keys with your server name as the prefix --- each server 650 times so that you would store 65500 keys in your ring.

>*Collisions?*  Just rehash with an incrementing postfix until you find a hash that isn't taken.

>*Hash function?*  Use a fast one that produces uniformly distributed hashes and has good avalanche (collision resistance) behavior like the [Jenkin's hash](http://home.comcast.net/~bretm/hash/7.html).

How uniform is this?  In the worst case, all 65500 are clumped together with a 65536-65500=36 gap.  That means 36/65536 of your ring is not uniformly distributed, only 0.05%.  Not bad.

What happens if you lose a machine?  You go from 100 machines to 99 machines, so you mark the 650 hashes in your ring corresponding to the dead sucker for removal.  Now you've gone from 65500 hashes to 65500-650=64850.  Fewer machines means that you can have more replicas per machine.  If we round down 65536/99 we get 661 replications.  So we add 661-650=6 extra replicas per machine into the ring.  We're closer to 65536 now, with 65439 hashes in the ring.

What did that do to the likelihood that the ring is still just as uniformly distributed?  Worst case we have gap of size 65536-65439=97, so 97/65536 of the ring is not uniformly distributed, still very low at 0.14%.  As we approach zero machines, the likelihood that we'll be able to evenly divide our machine count into 65536 increases (perfect uniformity!), so the ring will stay nonuniform in the 0-1% range.  Also not bad.

"Wait, I have to keep track of up to 65536 ordered hashes?"  Yep.  You have to store all of those hashes and their metadata in memory somewhere.  For 65536 16-bit hashes, this isn't a big deal (around a megabyte with a string of IP metadata).  What if you're using 32-bit hashes, because you've got significantly more than 100 machines?  That's 4294967296 32-bit hashes with metadata (around 50 gigabytes), which most modern laptops cannot fit in RAM.  If you're using a sorted data structure (useful for finding the "next" machine in the ring for a given data key), that means O(log n) for lookups or insertions.  That could also be quite annoying.

**Hopeful.**  What if we were to find the largest gap in our ring and put our next hash right in the middle of this gap?  Throw the hash function out the window!  This is what would happen:

<img src="./static/images/LargestGap.jpg" alt="Uniform" style="width: 300px;display:block; margin-left:auto; margin-right:auto;"/>

As we add servers, the ring is only balanced when we have 2^n servers, at 2,4,8,16,etc.  We'll never be able to have a perfectly balanced ring with three servers, for instance, or any odd number for that matter.  Removing servers is also a pain, because it's not intuitive how we would rebalance.  This is a hopeful approach, but don't take it.

**Empirical.**  We can still hash servers into the ring, and fewer of them than the entire ring, if we know what degree of nonuniformity we can handle.  Empirically,

1. Hash r replications of n servers into a s-bit space
2. Vary r by 1,10,100,500,1000,5000,10000 replications, so long as n*r is less than the number of hashes the space can fit
3. Vary s by 3,10,100,500,1000 servers, so long as n*r is also less than the size of the space
4. For each replica in the ring, find the number of data keys that would get routed to it
5. Sum up the number of data keys that would go to each server by reducing the replicas back to single servers
6. Get the difference between the number of data keys that would go to each server in a uniform world (2^s)/n and the actual number of data keys per server
7. The sum of the differences in 6 gives us the nonuniformity of the entire ring

Here are the results for a 16-bit space and 32-bit space ([code](https://gist.github.com/edahlgren/5938401)):

<div style="margin-left:auto; margin-right:auto;">
<div style="display:block; overflow:hidden; float:left; margin-left:25px; margin-right:50px;">
<div style="text-align:center;">16-bit</div>
<table>
<tr>
<td>servers</td>
<td>replicas</td>
<td>% nonuniform</td>
</tr>
<tr>
<td>3</td>
<td>1</td>
<td>30.415</td>
</tr>
<tr>
<td>3</td>
<td>10</td>
<td>19.422</td>
</tr>
<tr>
<td>3</td>
<td>100</td>
<td>6.370</td>
</tr>
<tr>
<td>3</td>
<td>500</td>
<td style="color:#0000CC;">0.724</td>
</tr>
<tr>
<td>3</td>
<td>1000</td>
<td>1.295</td>
</tr>
<tr>
<td>3</td>
<td>5000</td>
<td style="color:#0000CC;">0.593</td>
</tr>
<tr>
<td>3</td>
<td>10000</td>
<td style="color:#0000CC;">0.331</td>
</tr>
<tr></tr>
<tr>
<td>10</td>
<td>1</td>
<td>82.101</td>
</tr>
<tr>
<td>10</td>
<td>10</td>
<td>35.485</td>
</tr>
<tr>
<td>10</td>
<td>100</td>
<td>5.508</td>
</tr>
<tr>
<td>10</td>
<td>500</td>
<td>2.316</td>
</tr>
<tr>
<td>10</td>
<td>1000</td>
<td>2.249</td>
</tr>
<tr>
<td>10</td>
<td>5000</td>
<td style="color:#0000CC;">0.531</td>
</tr>
<tr></tr>
<tr>
<td>100</td>
<td>1</td>
<td>74.716</td>
</tr>
<tr>
<td>100</td>
<td>10</td>
<td>26.733</td>
</tr>
<tr>
<td>100</td>
<td>100</td>
<td>6.497</td>
</tr>
<tr>
<td>100</td>
<td>500</td>
<td>1.815</td>
</tr>
<tr></tr>
<tr>
<td>500</td>
<td>1</td>
<td>75.839</td>
</tr>
<tr>
<td>500</td>
<td>10</td>
<td>22.915</td>
</tr>
<tr>
<td>500</td>
<td>100</td>
<td>3.948</td>
</tr>
<tr></tr>
<tr>
<td>1000</td>
<td>1</td>
<td>70.220</td>
</tr>
<tr>
<td>1000</td>
<td>10</td>
<td>22.842</td>
</tr>
</table>
</div>

<div style="display:block; overflow:hidden; margin-left:50px; margin-right:auto;">
<div style="text-align:center;">32-bit</div>
<table>
<tr>
<td>servers</td>
<td>replicas</td>
<td>% nonuniform</td>
</tr>
<tr>
<td>3</td>
<td>1</td>
<td>30.415</td>
</tr>
<tr>
<td>3</td>
<td>10</td>
<td>19.418</td>
</tr>
<tr>
<td>3</td>
<td>100</td>
<td>6.379</td>
</tr>
<tr>
<td>3</td>
<td>500</td>
<td style="color:#0000CC;">0.660</td>
</tr>
<tr>
<td>3</td>
<td>1000</td>
<td>1.731</td>
</tr>
<tr>
<td>3</td>
<td>5000</td>
<td style="color:#0000CC;">0.647</td>
</tr>
<tr>
<td>3</td>
<td>10000</td>
<td style="color:#0000CC;">0.333</td>
</tr>
<tr></tr>
<tr>
<td>10</td>
<td>1</td>
<td>82.105</td>
</tr>
<tr>
<td>10</td>
<td>10</td>
<td>35.483</td>
</tr>
<tr>
<td>10</td>
<td>100</td>
<td>5.733</td>
</tr>
<tr>
<td>10</td>
<td>500</td>
<td>2.186</td>
</tr>
<tr>
<td>10</td>
<td>1000</td>
<td>2.476</td>
</tr>
<tr>
<td>10</td>
<td>5000</td>
<td>1.624</td>
</tr>
<tr>
<td>10</td>
<td>10000</td>
<td style="color:#0000CC;">0.694</td>
</tr>
<tr></tr>
<tr>
<td>100</td>
<td>1</td>
<td>74.740</td>
</tr>
<tr>
<td>100</td>
<td>10</td>
<td>27.578</td>
</tr>
<tr>
<td>100</td>
<td>100</td>
<td>7.369</td>
</tr>
<tr>
<td>100</td>
<td>500</td>
<td>2.938</td>
</tr>
<tr>
<td>100</td>
<td>1000</td>
<td>2.314</td>
</tr>
<tr>
<td>100</td>
<td>5000</td>
<td>1.233</td>
</tr>
<tr>
<td>100</td>
<td>10000</td>
<td style="color:#0000CC;">0.781</td>
</tr>
<tr></tr>
<tr>
<td>500</td>
<td>1</td>
<td>75.939</td>
</tr>
<tr>
<td>500</td>
<td>10</td>
<td>23.786</td>
</tr>
<tr>
<td>500</td>
<td>100</td>
<td>7.856</td>
</tr>
<tr>
<td>500</td>
<td>500</td>
<td>3.711</td>
</tr>
<tr>
<td>500</td>
<td>1000</td>
<td>2.598</td>
</tr>
</table>
</div>
</div>

It was surprising to find that even 500 replicas of 3 servers in a 16-bit and 32-bit space could so readily result in a nearly (99%) uniform ring.  This test was only run once and there needs to be averaging over several runs, but the take-home is that you need far fewer keys than the full hash space to produce uniform rings at 99% uniformity.  Also more replicas with few servers is much better than more servers and few replicas.

> *Ok, so how many replicas of my servers do I need?*

>Simple: Choose the smallest bit-space that supports at least 500 replications per server.<br>

>Better: Run the tests!  (and make the tests better!)
