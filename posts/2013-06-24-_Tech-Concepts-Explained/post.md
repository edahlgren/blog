title: Tech Concepts Explained
author: edahlgren

I was recently asked to put together a basic tech concepts page, with a bias on haskell.  I will cover:

* Complexity
* Sorting
* Lists and unboxed arrays
* Hashmaps, writing an LRU, and sets in a copy-on-write language
* MVars, STM, and concurrency mechanisms
* Trees and Graphs, and why I don't use them that much
* Solving space leaks from laziness
* Bloom filters (because they're cool)
* Some stupid discrete math
* The haskell scheduler: context switching
* Capabilities, spawning processes, threads, (and how that maps to ffi pthreads)
* locks, mutexes, semaphores

Things I still need to review:
a. various puzzle questions from the googlers
b. mergesort quicksort and friends
c. processes/threads
d. locks, mutexes, semaphores
e. traveling salesman-esque problems

Have ready -> things I've found interesting or challenging

# O(n)
A haskell function in linear time might involve a single map or fold (foldl :: (base -> list item -> base) -> base -> list -> base):

    reverse          :: [a] -> [a]
    reverse          =  foldl (flip (:)) []

Reverse works by flipping the arguments of cons (:) -> flip (:) $ a b == (:) b a, so the list item can be consed into the base.  Since the fold pops items off the head, and cons pushes new items in front of the head, you get a reversed list just by consing.  The flip only corrects the types, because you can't cons a list into an item.

# O(n2)
An quadratic complexity function might involve two folds, where the goal is to compare every item with every other item.

Bubble sort is another example of a quadratic complexity function.  The idea is to make multiple passes over a list, swapping adjacent elements in place if they're in the wrong order, until the list is sorted.  If the list is in reverse order, all of the elements need to be flipped, meaning that it would take n passes (over n things) to get them in the right order.  Yikes.

# O(log n)
Think binary search, and this is only possible to do on ordered data.

# O(n log n)
These functions essentially do a binary search for all of the things in your data structure.

    Data.Set.map :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
    Data.Set.map f = fromList . List.map f . toList

Data.Set.map works as if you applied a function to all of the members of a list, and constructed a binary tree out of them.  Unless map preserves the monotonic ordering of the values, then you're stuck building a binary tree out of unsorted values.  So for each element in your result list, you need to binary search for the place to put it in your new list.

# O(n + m)
Set union is a function that need to iterate over all of the values of two collections.

# O(!n)
The traveling salesman is an example of factorial complexity.

Another example is the permutations of a list:

permutations            :: [a] -> [[a]]
permutations xs0        =  xs0 : perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)

An O (n - !n) function would randomly order n things, and then repeat if they weren't in sorted order.  This is bogo sort.

# Sorting
Sorting is really interesting in haskell because it's a copy-on-write language: no destructive updates means that the in-place sorting isn't possible on a Haskell list, unless you change the haskell datatype (mutable array, state monad are options).  The famous space inefficient haskell quicksort:

## Quicksort

quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs

Given a first item p, filter the items for all those less than p and quicksort, and with greater than and quicksort.  Easy to understand, but O(n2) at sort and not space efficient.  Better to use mergesort because you won't get the space benefits, because mergesort is O(n log n) at worst.

## Unstable mergesort

merge []         ys                     = ys
merge xs         []                     = xs
merge xs@(x:xs') ys@(y:ys') | x <= y    = x : merge xs' ys
                            | otherwise = y : merge xs  ys'

The idea is simple: take two sorted lists, and interleave the heads as you walk down them.  You can build up these small sorted lists from singletons, or you can build them up from runs in your original list.  You can do something really simple like this:

singletons = map (\x -> [x])

mergePairs (xs:ys:zs) = merge xs (ys:mergePairs zs)

split (x:y:zs) = let (xs,ys) = split zs in (x:xs,y:ys)
split [x]      = ([x],[])
split []       = ([],[])

mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = let (as,bs) = split xs
                in merge (mergeSort as) (mergeSort bs)

## Stable mergesort (Data.List)

The idea is to look for runs in the original list, and to use those as the tiny lists to repeatedly merge.

sort = sortBy compare
sortBy cmp = mergeAll . sequences
  where
    sequences (a:b:xs)
      | a `cmp` b == GT = descending b [a]  xs
      | otherwise       = ascending  b (a:) xs
    sequences xs = [xs]

    Sequences compares the fir

    descending a as (b:bs)
      | a `cmp` b == GT = descending b (a:as) bs
    descending a as bs  = (a:as): sequences bs

    ascending a as (b:bs)
      | a `cmp` b /= GT = ascending b (\ys -> as (a:ys)) bs
    ascending a as bs   = as [a]: sequences bs

    mergeAll :: [[a]] -> [a]
    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs :: [[a]] -> [[a]]
    mergePairs (a:b:xs) = merge a b: mergePairs xs
    mergePairs xs       = xs

    merge :: [a] -> [a] -> [a]
    merge as@(a:as') bs@(b:bs')
      | a `cmp` b == GT = b:merge as  bs'
      | otherwise       = a:merge as' bs
    merge [] bs         = bs
    merge as []         = as

The problem with Data.List.sort is that it uses merge sort, which creates new lists during each pass. So a lot of time is spent on allocating and freeing memory.

And on ByteStrings, a "counting" sort:

sort :: ByteString -> ByteString
sort (PS input s l) = unsafeCreate l $ \p -> allocaArray 256 $ \arr -> do

    _ <- memset (castPtr arr) 0 (256 * fromIntegral (sizeOf (undefined :: CSize)))
    withForeignPtr input (\x -> countOccurrences arr (x `plusPtr` s) l)

    let STRICT2(go)
        go 256 _   = return ()
        go i   ptr = do n <- peekElemOff arr i
                        when (n /= 0) $ memset ptr (fromIntegral i) n >> return ()
                        go (i + 1) (ptr `plusPtr` (fromIntegral n))
    go 0 p
  where
    -- | Count the number of occurrences of each byte.
    -- Used by 'sort'
    --
    countOccurrences :: Ptr CSize -> Ptr Word8 -> Int -> IO ()
    STRICT3(countOccurrences)
    countOccurrences counts str len = go 0
     where
        STRICT1(go)
        go i | i == len    = return ()
             | otherwise = do k <- fromIntegral `fmap` peekElemOff str i
                              x <- peekElemOff counts k
                              pokeElemOff counts k (x + 1)
                              go (i + 1)

In constrast, here's quicksort that allows for recursion and destructive updates:

def quicksort(xs):
    if xs == []:
       return xs
    pivot = xs[0]
    lesser =

# Sets and Maps

Balanced binary trees
data Set a    = Tip
              | Bin {-# UNPACK #-} !Size !a !(Set a) !(Set a)

singleton x = Bin 1 x Tip Tip

## Insert
insert :: Ord a => a -> Set a -> Set a
insert = go
  where
    go x Tip = singleton x
    go x (Bin sz y l r) = case compare x y of
        LT -> balanceL y (go x l) r
        GT -> balanceR y l (go x r)
        EQ -> Bin sz x l r

If the set is empty, make a singleton.  If the set is non empty, go to the root and compare the new value to the root value.  If it's less than, recurse into the left, if greater than, recurse into the right, if equal, replace the root value with the new value (no duplicates in a set).  What's balance?  That's where all of the real work is happening.

delta = 3 -- max diff between the sizes of the left and right trees
-- A lower delta leads to a more perfectly balanced tree, a higher delta less rebalancing
ratio = 2 -- ratio of the outer and inner sibling (count?) in an unbalanced tree -> tells you how to rebalance

balance :: a -> Set a -> Set a -> Set a
balance x l r
     | sizeL + sizeR <= 1   = Bin sizeX x l r
     | sizeR > delta*sizeL  = rotateL x l r
     | sizeL > delta*sizeR  = rotateR x l r
     | otherwise            = Bin sizeX x l r
   where
     sizeL = size l
     sizeR = size r
     sizeX = sizeL + sizeR + 1

This function says if your sibling count (sizeL + sizeR) is <= 1, meaning that either you have only 1 left sibling, or only 1 right sibling, or both are tips, then stick your new value as the parent of that tree.  If the sibling count on the right side is greater than delta (3) times the left, then rotate the tree left; same with the left side, in reverse.  If the tree is not that unbalanced, then just throw up your hands and say whatever and stick your value into the tree as the new parent.  Ok, pandora's box, what does rotate do?

rotateL :: a -> Set a -> Set a -> Set a
rotateL x l r@(Bin _ _ ly ry) | size ly < ratio*size ry = singleL x l r
                              | otherwise               = doubleL x l r
rotateR :: a -> Set a -> Set a -> Set a
rotateR x l@(Bin _ _ ly ry) r | size ry < ratio*size ly = singleR x l r
                              | otherwise               = doubleR x l r

To rebalance, if your right subtree is significantly bigger than your left subtree, you rotate the right subtree left: meaning you take some shit off it and put it on the left side.  If the left subtree is bigger, you take some shit off it and put it on the right side.  Pretty intuitive.  When your rotate your right subtree left, you need to know whether it's best to do a single (things are unbalanced enough to rotate, but not a ton) or a double (things are bad) rotation.

You do a double rotation if the bloated branch itself has a bloated branch on the same side (bloat right, with bloated right subtree).  Bloated means twice as big here (ration=2).  Remember we have a less strict constraint on whether we should rebalance, if of the branches is more than 3 times the size of the other.  So we don't do a lot of preemptive balancing, which is probably good.

Ok, what you've been waiting for ... how do these rotations actually work?

singleL, singleR :: a -> Set a -> Set a -> Set a
singleL x1 t1 (Bin _ x2 t2 t3)  = bin x2 (bin x1 t1 t2) t3
singleR x1 (Bin _ x2 t1 t2) t3  = bin x2 t1 (bin x1 t2 t3)

    x1              R (x2)
   / \             / \
  L   R (x2)  ->  x1 Rr
     / \         / \
    Lr Rr       L  Lr

  preserve strict left to right ordering:
  L -> x1 -> Lr -> R (x2) -> Rr

bin :: a -> Set a -> Set a -> Set a
bin x l r = Bin (size l + size r + 1) x l r

For a single rotation take the branch to be rotated and literally just stick a part of it onto the other side.  So a rotation just means "take the heavy branch, lift it up and append the lighter branch to the other side".  The bin helper function simply sums the subtree sizes and sticks the value as the parent of the branches.

doubleL, doubleR :: a -> Set a -> Set a -> Set a
doubleL x1 t1 (Bin _ x2 (Bin _ x3 t2 t3) t4) = bin x3 (bin x1 t1 t2) (bin x2 t3 t4)
doubleR x1 (Bin _ x2 t1 (Bin _ x3 t2 t3)) t4 = bin x3 (bin x2 t1 t2) (bin x1 t3 t4)

For a double rotation it's pretty simple too.  To double rotate, you go down two levels of children, and bring the lowest one up, attaching the parents to it in reverse order.

Ok, that's a lot of detail to remember.  Remember that a non-balanced binary tree is in fact really simple.  You just recurse down the branches, comparing your new value to the old ones until you reach the same value (replace) or a place to insert the value as a Tip.

Balanced binary trees involve two constant parameters that help you decide when you need to balance, and how many balancing acts you need to perform when you do need to balance.  If one branch is much bigger than the other, you need to rebalance (that's the first parameter, the delta size).  If the side that's too big has a child on the same side that's too big, then you need to do more rebalancing that usual.  You rebalance by taking the heavy subtree and lifting it up, attaching the lighter side.  So when you're implementing this you need:

* a function that does the recursion (these are the high level api functions)
* a function to decide whether the left or right side is unbalanced, and to execute ...
* a rotation function, which decides *how* unbalanced that side is -> how many rotations to do ...
* a workhorse rotation function which does single and double rotations.

(I don't actually know what type of tree this is, it's just a size balancing binary tree)

## Delete
delete :: Ord a => a -> Set a -> Set a
delete = go
  where
    go _ Tip = Tip
    go x (Bin _ y l r) = case compare x y of
        LT -> balanceR y (go x l) r
        GT -> balanceL y l (go x r)
        EQ -> glue l r

If you find a value equal to the one you want to delete, then do the work with glue, otherwise be a nice citizen and balance the tree for kicks.

glue Tip r = r
glue l Tip = l
glue l r
  | size l > size r = let (m,l') = deleteFindMax l in balanceR m l' r
  | otherwise       = let (m,r') = deleteFindMin r in balanceL m l r'

The glue function

## Member
This is just basic binary search, no balancing is needed.
member :: Ord a => a -> Set a -> Bool
member = go
  where
    go _ Tip = False
    go x (Bin _ y l r) = case compare x y of
          LT -> go x l
          GT -> go x r
          EQ -> True

# HashSets and HashMaps

Prefix, Mask, Key :: Int

data IntMap a = Bin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask !(IntMap a) !(IntMap a)
              | Tip {-# UNPACK #-} !Key a
              | Nil

The idea is the Bin nodes have a prefix mask, a children mask, and branches, while Tip nodes have full keys and values.  An IntMap is a glorified, confusing binary tree.

lookup :: Key -> IntMap a -> Maybe a
lookup k = k `seq` go
  where
    go (Bin _ m l r)
      | zero k m  = go l
      | otherwise = go r
    go (Tip kx x)
      | k == kx   = Just x
      | otherwise = Nothing
    go Nil      = Nothing

To lookup a value in an IntMap, you check a key against a children mask; sharing or not sharing bits with a children mask propells you in different directions down the tree ... until you get to a Tip.  Once you get to a Tip, you check that the masks that led you there are correct by checking your key against the key stored there, and you return appropriately "I found something" or "fuck you, you fuck up".

zero :: Key -> Mask -> Bool
zero i m
  = (fromIntegral i) .&. (fromIntegral m) == 0

To understand these prefixes and children masks, let's take the simplest case of insertion, starting from a singleton IntMap and adding a parent to it.

singleton :: Key -> a -> IntMap a
singleton k x
  = Tip k x

To create a singleton, you make a Tip node, with the key and value.  Simple enough.  To add a parent to that Tip node, you match on Tip in the insert function below, and if it's a new (key,value) (it is), then your job is to join/become a sibling of the existing Tip, and the child of a new root Bin node.

insert :: Key -> a -> IntMap a -> IntMap a
insert k x t = k `seq`
  case t of
    Bin p m l r
      | nomatch k p m -> join k (Tip k x) p t
      | zero k m      -> Bin p m (insert k x l) r
      | otherwise     -> Bin p m l (insert k x r)
    Tip ky _
      | k==ky         -> Tip k x
      | otherwise     -> join k (Tip k x) ky t
    Nil -> Tip k x

join :: Prefix -> IntMap a -> Prefix -> IntMap a -> IntMap a
join p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m

Here's the high level idea:

If your new key and the node you're on can together mask to match the prefix stored on that node, that means you should be a child of that node.  If you can't match, then you should be a sibling of that node.  If you get all the way to a Tip, then you should be a sibling of that Tip only if you aren't a copy of it.  If you get to Nil, it means you had the empty Map, and you should actually be the first Tip.

The way you mask your new key with a node is a bit complicated.  The node stores two numbers, a number that represents the key prefix shared by all of its children, and a number that represents the two keys of its direct children.  If you mask the direct children mask with a new key, and check to see if it matches the prefix shared by those children ... and all of its other children, you can determine whether that's the correct subtree for you (you, the unplaced key and value).  Essentially you're checking to see if you combine your new keys with the existing ones, will the shared prefix be the same.

The way this is efficient is that your keys are all Ints, and hence just bit arrays.  You can pattern match very quickly on your key to see if you should be traversing left or right.  The complexity of this is O(min(n,W)), where is W is the number of bits in your Int (32, 64, whatever).

Haskell's HashMap and HashSet are implemented this way, where the keys are hashed to Ints.

# Stupid tree things

Postorder, preorder, inorder, bfs, dfs (throw up now, so dumb).

# LRUs

-- timestamped value, for ordering
LRUValue k v = LRUValue Double k v

tell haskell you want to order the values in time:
instance (Eq k, Eq v) => Ord (LRUValue k v) where
    compare = compare `on` (\(LRUValue t _ _) -> t)

values :: HashMap k (LRUValue k v)
order :: Set (LRUValue k v)
numValues :: Int
maxItems :: Int

lookup pseudo-commands
*
*
*

insert pseudo-commands
*
*
*

delete pseudo-commands
*
*
*

# Bloom Filters (yay)

Bloom filters are essentially a set that allows for fuck ups: X either "may be" in the set, or is definitely not.  If what you care about is whether something is a member of a set, you can use the bloom filter as a first pass filter, so that when you check "fo realz" whether you thing is a member, you're checking fewer potential members.  I've found it useful for limiting expensive operations by limiting the input count.

You have a bit array of size N, and you have M unique hash functions of type (Hashable a) -> a -> Int.  Initialize your bit array with all zeroes. To insert into the bloom filter, you hash your element with each hash function, producing three unique Ints (i1,i2,i3).  These unique Ints are positions in your array!  Flip the bit positions i1, i2, i3 to 1.  Yay, now your key is in the bit array.  Obviously the same key could hash to one of more overlapping Int positions, so if you check for your element and all of the bits are flipped to 1, it could be other elements filling in those bits for you.  But if less than three of your bits are flipped, your element definitely didn't set them, so it's definitely not there.  Given a big enough array and more/better hash functions (usually worse than just making the array bigger, because it's expensive to compute hashes) you can make your false positive rate pretty low.

Application: I wanted to filter pairs of people that had a certain level of address book overlap.  Maybe pairs needed at least 3 shared contacts to pass this filter.  For people with big address books (upwards of 100+ people), it was not a delicious idea to compute the intersect for every pair, just to know whether they passed the filter.  To make matters worse, I couldn't just do a bounded intersect, where if up to 3 matches were found I could exit the intersect computation early, because the data guys wanted the raw intersects to decide if 3 was really the best lower bound.  At least there was an upper bound, an overlap of 20+ that didn't really make a difference.  So what did I do.  I made a bloom filter out of each address book (unordered collection of strings) -> 64 kilobits each, with a precision of 3 hash functions (seemed like a good compromise).  To get the number of shared elements between two bloom filters you AND the bits together and divide the pop count by the number of hash functions.

sizeB = 128*64 :: Int
rangeB = (sizeB `quot` 32) - 1 :: Int
precision = 3 :: Int

intersect :: BitArray -> BitArray -> Int
intersect b1 b2 = intersect `quot` precision
  where
    intersect' = foldl' (\pop i -> pop + match i) 0 [0..rangeB]
    match i = popCount $ (b1 ! i) .&. (b2 ! i)

Where you AND the bit positions together (if they share something in common there, only then will the bit be one).  The reason we do the popCount is because the implementation of the BitArray isn't just 128*64 Word8s, it's 512 Word32s.  So popCount does the trick of adding up all of the 1s that survive the AND.  We divide by the precision at the end, because that's how many bits were duplicated per element (by the hash functions).  This is nice, because if this fast intersect is less than our requirement (3), we can ignore the pair entirely ... because there's no false negatives.

Since we don't get any benefit out of knowing what the exact bloom filter intersect is if it's greater than (3), because of false positives, we can actually exit early from the intersect once we reach that bound.  We could do something like:

Well, in our situation since the bloom filter is *a filter*, so we could do better.   If we find out that

intersect :: Int -> BitArray -> BitArray -> Bool
intersect bound b1 b2 = intersect' 0 rangeB
  where
    match i = popCount $ (b1 ! i) .&. (b2 ! i)

    weightedBound = bound*precisionB

    intersect' !pop !k = case k of
        -1 -> False
	_ -> if pop >= weightedBound then True else intersect' (pop + match k) (k-1)

Which is on average even faster.  Nice.

How are these BitArrays implemented? Unboxed Arrays: UArray Int Word32.  UArrays are effectively C arrays, they're strict in their elements, and they are preallocated (you cannot dynamically resize the elements as you use it).

# Graphs (ugh, pure language, ugh)

Let's say that your vertices are keyed by Int32's or less, and that the relationship between any vertices A,B is any data type you want (representing the edge).  This is what I would do: create a bijective NxN -> N mapping so that A,B -> unique key, and store value in a hashmap.  Easy way to do this, stuff A into the bottom 32 bits, and B into the top 32 bits.  Sort them and Use bit masks to shift them in and out like so:

-- ordered least to greatest
pair :: Int -> Int -> Word64
pair a b | a64 > b64 = (b64 `shiftL` 32) + a64
         | otherwise   = (a64 `shiftL` 32) + b64
  where
    a64 = fromIntegral a :: Word64
    b64 = fromIntegral a :: Word64

-- returned least to greatest
unpair :: Word64 -> (Int, Int)
unpair pairing = (fromIntegral a, fromIntegral b)
  where
    a = pairing .&. getHexadecimal "ffffffff"
    b = pairing `shiftR` 32

Let's say that your vertices are something less wholesome, like Int64 (where the pairing above can't fit into a nice machine word), or floats or strings.  If they're not already strings, I would cast them to strings and do this:

Compute a message digest of both strings and XOR the values
MD5(x) ^ MD5(Y)
The message digest gives you unique value for each string and the XOR makes it possible for f(x, y) to be equal to f(y, x).

Then use a hashmap of vertice to Set (connected buddies) so you can walk the graph.

## Fucking graph algorithms, I never use these.

If we stored our data as graphs, it would not be straightforward to merge siblings, or not efficient at least.  We would store someone's vertices, and then we would store the values using the pairing trick.  And then we would operate on that, but since database fetches are expensive fuck that.

------
Best review for Apple
(1) High level questions
(2) Describe systems that you've built
(3) review some C stuff
(4) Basic Design mind-bending problem

(1)
What do you do for bump?
I write and scale backend pipelines, I've done a lot of work on our database proxy, do most of the day to day functional programming.

What motivates you?
Making things simpler.  Making things faster.

(2)
What are some things you've worked on?
* building a fast reminders pipeline (bloom filter is related to this)

    * intersect filter -> address book bloom filters
    * reactivation pushes -> seek & tell
    * fast filters: routing to threads and local thread caches
      	   not always ideal -> if the thread that your routed data is going to is too busy to
	       	      	       pop your routed data off it's queue, then your serving thread
			       blocks, and stops the world
           I simplified the filters to a such a degree (offloading a lot of the validity
	   checking/grouping of filtered packets to processes consuming the filtered output)
	   that this wasn't a "this filter can't keep up".

	   But the intersect process had problems keeping up, due to the huge cpu cost of
	   deserializing the large address books and computing intersects.

	   One solution, which I never implemented and also doesn't work: don't block the serving
	   thread by forking off threads for each incoming request, where each of these threads
	   tries to put the data onto its routed thread's queue.  If things get really backed up,
	   because the number of forks isn't bounded you could run out of memory or most likely gc
	   becomes horrendous and you're spending most of your cpu time doing gc, getting slower, shit's bad.

	   You can you bounded queues of a size greater than 1 and you still run into the problem of
	   at some point backing up the serving thread until they flush lower than the bound,
	   but this is less likely to happen if your worker thread count is high.  Hopefully your scheduler
	   can handle that many hundreds of threads though.

	   So what do you do?  The solution I took which actually worked pretty well was to have a set of
	   fetching threads and working threads.  You route your data to *any* worker thread, which frees
	   up the serving thread until all threads are blocked.  Worker threads send an empty package to
	   fetching threads, and the fetching threads fill it with data from their local caches or the database.
	   Data is routed deterministically to fetching threads, to ensure overlap between requests and local caches
	   but because you would create more fetching threads than worker threads (I used twice as many), then
	   the likelyhood that any two workers are blocked on the same request is lower but still possible.

	   Anyway it scaled well.

    * I wrote/improved an LRU cache
           Simple design -> hashmap + ordered set (binary under the hood) + numValues + limit
	   Insert would give you back the value it evicted, if that was necessary.
	   So this was actually a really clean way of also doing grouping of fragmented data:
	     let's say our data came in packets that could only contain 100 points
	     and the phones couldn't garauntee ordering of packets, and for god's sake with
	     all of the sharding going on, it's likely that there'd be 0-some number under 100
	     packets not from the same source (totally unrelated) interspersed with the packets
	     we wanted to do something with.

	     So this is what you do:

* building our database resolution proxy
    * we use riak as a backend, which has a lazy consistency principle
    * it's great for writes, and also acceptable for reads
    * I worked to design the thread management and FFI bindings for the resolution functions
* cpu time statistics for a greenlet thread hub

(3)
C bits

# Basics

## Pointers

& is used to "reference" the memory address, and * is used to deference the pointer so you can get at the data.

int x = 2;
int* p_x = &x;  // put the address of the x variable into the pointer p_x
*p_x = 4;       // change the memory at the address in p_x to be 4
assert(x == 4); // check x is now 4

Here are ways you could deference a pointer:

assert(*p == 'a');  // the first character at address p will be 'a'
assert(p[1] == 'b'); // p[1] actually dereferences a pointer created by adding
                     // p and 1 times the size of the things to which p points:
                     // in this case they're char which are 1 byte in C...
assert(*(p + 1) == 'b');  // another notation for p[1]

And the structure referencing syntax:

typedef struct X { int i_; double d_; } X;
X x;
X* p = &x;
p->d_ = 3.14159;  // dereference and access data member x.d_
(*p).d_ *= -1;    // another equivalent notation for accessing x.d_

## Basic utils

malloc :: size_t -> void *
--------
Allocates size bytes, the memory is not initialized.

int* p = malloc(sizeof(int));   // get some memory somewhere...
*p = 10;            // dereference the pointer to the memory, then write a value in
fn(*p);             // call a function, passing it the value at address p
(*p) += 3;          // change the value, adding 3 to it
free(p);            // release the memory back to the heap allocation library

realloc :: void * -> size_t -> void *
--------
Resizes a memory block by copying all of the data into a larger memory block.

strncpy :: char * -> char * -> char *
---------
char *
strncpy(char *dest, const char *src, size_t n)
{
    size_t i;

    for (i = 0; i < n && src[i] != '\0'; i++)
        dest[i] = src[i];
    for ( ; i < n; i++)
        dest[i] = '\0';

    return dest;
}

free :: void * -> void
---------

memset :: void * -> int c -> size_t -> void *
---------
Fills the first n (size_t) bytes of the memory area pointed to by s with the constant byte (int)

## Reversing a linked list

#include <stdio.h>

typedef struct Node {
  char data;
  struct Node* next;
} Node;

Linked lists are really simple in C.  You usually define a datatype with room for the data in the list, and room for a pointer to the next node.  Typedefing is great because you can write pretty type signatures.

void print_list(Node* root) {
  while (root) {
    printf("%c ", root->data);
    root = root->next;
  }
  printf("\n");
}

Here's a good demonstration of how to walk a linked list.  Given the head of a linked list, print the data and point the root pointer to the next element.

Node* reverse(Node* root) {
  Node* new_root = NULL;
  while (root) {
    Node* next = root->next;
    root->next = new_root;
    new_root = root;
    root = next;
  }
  return new_root;
}

If you want to reverse a linked list, it's equally straightforward.  You're effectively popping the head off of the list and consing it onto something new.  In C, consing is pointing your next pointer at the head of a list.  So if we take the head off our old list and point it continuously at an empty list (NULL), then we'll get the reverse.  So save the tail of the list at a variable and point each head at the new_root.  Voila.

## How hashmaps work in C

Hashmaps are really cool in C.  You choose your favorite hash function: djb2, jenkins, whatever.  Allocate an array of pointers of a pretty big length.  You can use Hashmaps as sets, which is not a horrible idea (hash set).

Insertion:
Then hash your favorite datatype using your hash function to a position in your array.  There's either something already linked to that position, or there's not.
* If you care about a balanced hash table (minimizing more than one value at an index if there's still indices without data), because you want your reads to be super fast:
Choose one or more other hash functions and try to find another indice to put your data.

* If you honestly don't care that much about a more balanced hash table (your hash function creates "good enough" spread, as most do). because your writes need to be fast (please don't hash things lots of times): walk down the linked list of things stored there and add your value to the end.

Lookup:
Hash your key again and go to the indice.  Walk down the linked list until you find your entry.  If you can't find it, then you rehash with hash function blah, and search again.  If you have no other hash functions, then you return Nothing.

## How sets work -> binary tree

You really need a balanced binary tree, or else you're wasting your time.  To make this work, you need to keep track of how many siblings each node has.  That way you can easily rotate your branches if one side is too heavy.  You'll want parameters for (1) the difference between the left and right required to rebalance at all, and (2) the difference between the left and right side of the unbalanced subtree required to rotate it once or multiple times (to get it balanced).  If it's > 2*size, you only need single and double rotation functions.

## How queues and stacks work

Basic Queue

I think you can just keep a pointer to the head and the tail; I don't think you need doubly linked lists, if you want to keep variables hanging around.  If you want your data structure to have all it needs to know, then make a doubly linked circular list, where the head points both to the next item and also to the tail.

Priority Queue

Simple priority queue: binary tree implementation of a set, each element is ordered in time, along with the associated data.  Inserting is O(log n), deleting is O(log n); you would delete the max/min (depending on whether your priority queue was ordered in the min or max direction) and return that value.  Not horrible.

## C gotchas that are interesting
http://www.thegeekstuff.com/2012/08/c-interview-questions/

(3.1) Basic Unix things

## Processes versus threads

## Epoll and Select

## Mutexes, semaphores, locks

(4)
# Consistent Hashing

(5)
# Nearest Neighbor

How would you design a system where the nodes contain routing information about other nodes, and you need to route data to each of the appropriate databases?  How to do intelligent sharding?  I'd probably try managing that with consistent hashing.  If it's required to node hop, well, that's lame, I wouldn't design the network that way; much simpler to use consistent hashing.

(6)
# How would you make a single sorted stream out of k sorted streams?  You can't fit the data in memory.

You have n sockets, you have n spaces allocated for heads.  You read the first chunk from each stream (if it's a socket, a packet, stdin, until your delimiter into the head buffers.  You take the min and put it onto the new stream, and fill the empty buffer with the new head.  Repeat.  Since you're continually finding the min, then using a binary tree set might be better, where you store the stream+buffer in the binary tree.  But this will work.

## Given a function which produces a random integer in the range 1 to 5, write a function which produces a random integer in the range 1 to 7:

I think you can solve this calling the function twice (call it frand) twice.
(frand `mod` 2) + (frand `mod` 2) + frand
5 `mod` 2 = 1
4 `mod` 2 = 0
3 `mod` 2 = 1
2 `mod` 2 = 0
1 `mod` 2 = 1
(0 + 0) -> 0, (0 + 1) -> (1 + 0) -> (1 + 1) -> 2, not perfect, because there's more odd numbers than even ones, but since you're calling frand twice, it's not *so so* bad.

## Write a function to find the longest substring with two unique characters:

pairs xs = zip xs $ tail xs
groupPairs = groupBy (\pair1 pair2 -> sortPair pair1 == sortPair pair2)
sortPair (a,b) = if a < b then (a,b) else (b,a)
sortGroups = sortBy (\l1 l2 -> compare (length l1) (length l2)) groups
largestRight xs = let (p1,p2) = unzip . head . reverse xs in (head p1):p2

largest2Unique = largestRight . sortGroups . groupPairs . pairs

## Write a function f(a, b) which takes two character string arguments and returns a string containing only the characters found in both strings in the order of a. Write a version which is order N-squared and one which is order N.

Put b into a hashset, which will take care of the duplicates; iterate through a, checking the hashset for matches and throwing everything else out.

## You are given a the source to a application which is crashing when run. After running it 10 times in a debugger, you find it never crashes in the same place. The application is single threaded, and uses only the C standard library. What programming errors could be causing this crash? How would you test each one?

I would run it under valgrind to get the line numbers and see what type of error (invalid read, invalid free, etc.) go to the places it was crashing, looking for

* strcpy overflow
* buffer overflow from stdin
* trying to free a pointer that you've gotten from malloc and incremented
* making changes to a read-only segment

## How does congestion control work in TCP?

## How would you efficiently sort 1 million integers?

Figure out the length of the longest key in digits.  Here it's 3 digits.
get1s n = n `mod` 10
past1s n degree = n `div` (10^degree)

sample = [67,45,802,3]

radixSort input digit limit = if digit == limit then input else radixSort output (digit+1) limit
  where
    output = map (\(k,v) -> v) $ toList hs
    hs = case digit of
      0 -> foldl (mapDigits get1s) empty input
      _ -> foldl (mapDigits (past1s digit)) empty input

mapDigits getDigit m i = let key = getDigit i in
  maybe (insert key [i] hs) (\past -> insert key (i:past) hs) $ lookup key m

---------------------

1. implement quicksort

Quicksort works by picking a pivot, and sorting around the pivot recursively.  In haskell this would look like:

quicksort (p:ps) = (quicksort lesser) ++ [p] ++ (quicksort greater)
  where
    lesser = filter (< p) ps
    greater = filter (>= p) ps

This isn't great because haskell doesn't sort the list in place.  Because we're not intelligently taking the median as the pivot, we could be verging on O(n2) time cost, too.  That's why haskell prefers to use mergesort, which is O(n log n).

Just in case someone asks you to do the inplace version, here's C:

def qsort(xs):
    if xs == []:
       return []
    pivot = xs[0]
    return qsort([x for x in xs[1:] if x < pivot]) + [pivot] + qsort([x for x in xs[1:] if x >= pivot])

// To sort array a[] of size n: qsort(a,0,n-1)
void qsort(int a[], int lo, int hi)
{
  int h, l, p, t;

  if (lo < hi) {
    l = lo;
    h = hi;
    p = a[hi];

    do {
      while ((l < h) && (a[l] <= p))
          l = l+1;
      while ((h > l) && (a[h] >= p))
          h = h-1;
      if (l < h) {
          t = a[l];
          a[l] = a[h];
          a[h] = t;
      }
    } while (l < h);

    a[hi] = a[l];
    a[l] = p;

    qsort( a, lo, l-1 );
    qsort( a, l+1, hi );
  }
}

2. implement mergesort

Mergesort works by partitioning a list into tiny sorted lists (singletons are garaunteed to be sorted, but you could build up from runs in your original list).  The way you should remember it is: cons the smaller list head onto the result list, and recurse with the tail and other list.  Once you reach the empty list on either you're done.

-- no runs
mergeSortSimple = halves
  where
    halves [] = [] -- base case, empty
    halves [xs] = xs -- base case, singleton
    halves xs = merge (halves as) (halves bs)
      where (as,bs) = splitAt (length xs `quot` 2) xs

    merge [] ys = ys
    merge xs [] = xs
    merge xs@(x:xs') ys@(y:ys') =
      if x <= y then (x : merge xs' ys) else (y : merge xs ys')

3. implement a balanced binary tree

data Node a = Bin Size a (Node a) (Node a)
	    | Tip

To implement a balanced binary tree, you need to balance on insert and delete.  You have two parameters to help with the balancing: a left right size delta ("I should balance!") and an imbalanced side left right subtree size delta ("I should balance this much!").  So you need a top level function which will recurse and balance.  And you need to define a balance function, which calls rotate on a subtree, for some number n rotations.

The rotations are the hardest part, so let's define them first.  Rotating once means taking the lighter side of the tree and lifting it up.  If the left side is too heavy, we rotate it right, and vice versa.

(1) lift the opposite side value up to the root, and move the whole same side down into a subtree.

Here's a trick: preserve the ordering you see: (keep l lR and rR in the same order), and make sure to lift the opposite value into the root, and to lift the opposite subtree into a same side, higher position.

singleRotateL :: a -> Node a -> Node a -> Node a
singleRotateL val l r@(Bin _ rVal lR rR) =
  bin rVal (bin val l lR) rR

bin val l@(Bin sizeL _ _ _) r@(Bin sizeR _ _ _) =
  Bin (sizeL + sizeR + 1) val l r

singleRotateR :: a -> Node a -> Node a -> Node a
singleRotateR val l@(Bind _ lVal lL rL) r =
  bin lVal lL (bin rL r)

Ok, let's pretend that single rotation was the only thing we cared about, because our delta between the two sides is low (maybe can only be 1.5 to twice as big).

delta = 2

balanceL val l@(Bin sizeL _ _) r@(Bin sizeR _ _)
  | sizeL + sizeR <= 1 = bin val l r
  | sizeR > delta*sizeL = singleRotateL val l r
  | sizeL > delta*sizeR = singleRotateR val l r
  | otherwise = bin val l r

Then we actually do the recursion:

-- lesser values go on the left
insert val node = case node of
  Tip -> Bin 1 val Tip Tip
  (Bin sz y l r) -> case compare x y of
    LT -> balanceL y (insert val l) r
    GT -> balanceR y l (insert val r)
    EQ -> Bin sz x l r -- replace it

There we go, whew, that should do it.  gah.

---- Review -----

## Longest-Shortest substring problems

## Graphs, differen't keywords

## Breadth first and depth first descriptions (+/- code)

## Describe how to implement an LRU in C with pseudo-code, resizing, collisions



## More problems

Write a binary search function to count all adjacent values of n in a list of size m

-- return the index of *a* n
def bin_search(list, val, length):
    def bin_search_(low,high):
    	mid = low + (high-low)/2.0
	if val < list[mid]: # recurse to the left
	  bin_search_(low,mid-1)
	if val > list[mid]: # recurse to the right
	  bin_search_(mid+1,high)
	if val == list[mid]:
	   return mid
	return -1
    return bin_search(0,length-1)

def count_n(list, val):
    def direct_count(list, i, op):
    	count = 0
    	i_ = i
    	while True:
	      try:
		  next_val = list[i_]
		  if val == next_val:
		     count += 1
	      except:
		  break
	      i_ = op(i_, 1)
       return count

    found = bin_search(list,val)
    if found != -1:
       return direct_count(list, found-1, op.sub) + direct_count(list,found+1,op.add) + 1
    return 0

How do you sort the top 10 incoming things from a stream?

You could make a hashmap to store the requests, and a sorted data structure of n things.  So when you increment something you (1) check if it's in your sorted structure and if so just update it, and (2) check to see if it beat the min in the top ten, remove the min from the sorted structure and put your new thing in.  So in Haskell you'd probably use a HashMap and a Set.  Then you could get toList -> in ascending order.

What would you do to scale that to lots of machines/requests?

You could run this on a bunch of machines that each get a portion of the requests, and every second or something, every machine sends their top ten to the requestor.  The requestor merges their sorted top ten lists to get a definitive top ten.

Why are Red-Black trees so efficient?

What sort would you use if you had a large data set on disk and a small amount of ram to work with?

Merge sort would work because when you're sorting from smaller lists, you just need to read the head in from each list, given that your sublists are already sorted.

What's the difference between a permutation and a combination?

A permutation is the different ways you could *order* a collection.  Each permutation is the same size.  The number of permutations you can generate from size n is factorial (!n).  So for 5 -> 120.

permutations [] = [[]]
permutations xs = [x:ys | x <- xs, ys <- permutations (remove x xs)]

remove a xs = filter (/= a) xs

Permutations are easy to express recursivley.  The idea is to start off your output permutation lists with all of the possible heads, and the to recurse and add onto those all of the possible second items, and then to recurse until you get the empty list.  So the idea: cons each item in your list onto a new list, and attach all of the n members remaining in n different ways.

       	      	     permutations [1,2]
                     [1:ys | 1 <- xs, ys <- permutations (remove 1 xs)]
	     	     [1:ys | 1 <- xs, ys <- permutations [2]]
		                            [2:ys | 2 <- xs, ys <- permutations (remove 2 xs)]
					    	      	     	   [[]]
					    [2:[]]
		     [1:2:[]]
                     [2:ys | 2 <- xs, ys <- permutations (remove 2 xs)]
                     [2:ys | 2 <- xs, ys <- permutations [1]]
		     	       	            [1:ys | 1 <- xs, ys <- permutations (remove 1 xs)]
		     	       	            [1:ys | 1 <- xs, ys <- permutations []]
					    	      	           [[]]
		     	       	            [1:[]]
		     [2:1:[]]
		     = [[1,2],[2,1]]



*Main> permutations a
[[1,2,3,4,5],[1,2,3,5,4],[1,2,4,3,5],[1,2,4,5,3],[1,2,5,3,4],[1,2,5,4,3],[1,3,2,4,5],[1,3,2,5,4],[1,3,4,2,5],[1,3,4,5,2],[1,3,5,2,4],[1,3,5,4,2],[1,4,2,3,5],[1,4,2,5,3],[1,4,3,2,5],[1,4,3,5,2],[1,4,5,2,3],[1,4,5,3,2],[1,5,2,3,4],[1,5,2,4,3],[1,5,3,2,4],[1,5,3,4,2],[1,5,4,2,3],[1,5,4,3,2],[2,1,3,4,5],[2,1,3,5,4],[2,1,4,3,5],[2,1,4,5,3],[2,1,5,3,4],[2,1,5,4,3],[2,3,1,4,5],[2,3,1,5,4],[2,3,4,1,5],[2,3,4,5,1],[2,3,5,1,4],[2,3,5,4,1],[2,4,1,3,5],[2,4,1,5,3],[2,4,3,1,5],[2,4,3,5,1],[2,4,5,1,3],[2,4,5,3,1],[2,5,1,3,4],[2,5,1,4,3],[2,5,3,1,4],[2,5,3,4,1],[2,5,4,1,3],[2,5,4,3,1],[3,1,2,4,5],[3,1,2,5,4],[3,1,4,2,5],[3,1,4,5,2],[3,1,5,2,4],[3,1,5,4,2],[3,2,1,4,5],[3,2,1,5,4],[3,2,4,1,5],[3,2,4,5,1],[3,2,5,1,4],[3,2,5,4,1],[3,4,1,2,5],[3,4,1,5,2],[3,4,2,1,5],[3,4,2,5,1],[3,4,5,1,2],[3,4,5,2,1],[3,5,1,2,4],[3,5,1,4,2],[3,5,2,1,4],[3,5,2,4,1],[3,5,4,1,2],[3,5,4,2,1],[4,1,2,3,5],[4,1,2,5,3],[4,1,3,2,5],[4,1,3,5,2],[4,1,5,2,3],[4,1,5,3,2],[4,2,1,3,5],[4,2,1,5,3],[4,2,3,1,5],[4,2,3,5,1],[4,2,5,1,3],[4,2,5,3,1],[4,3,1,2,5],[4,3,1,5,2],[4,3,2,1,5],[4,3,2,5,1],[4,3,5,1,2],[4,3,5,2,1],[4,5,1,2,3],[4,5,1,3,2],[4,5,2,1,3],[4,5,2,3,1],[4,5,3,1,2],[4,5,3,2,1],[5,1,2,3,4],[5,1,2,4,3],[5,1,3,2,4],[5,1,3,4,2],[5,1,4,2,3],[5,1,4,3,2],[5,2,1,3,4],[5,2,1,4,3],[5,2,3,1,4],[5,2,3,4,1],[5,2,4,1,3],[5,2,4,3,1],[5,3,1,2,4],[5,3,1,4,2],[5,3,2,1,4],[5,3,2,4,1],[5,3,4,1,2],[5,3,4,2,1],[5,4,1,2,3],[5,4,1,3,2],[5,4,2,1,3],[5,4,2,3,1],[5,4,3,1,2],[5,4,3,2,1]]

-- How does this work?
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

tails [1,2,3] -> [[1,2,3],[2,3],[3],[]]

*Main Data.List> combinations 3 a
[[1,2,3],[1,2,4],[1,2,5],[1,3,4],[1,3,5],[1,4,5],[2,3,4],[2,3,5],[2,4,5],[3,4,5]]

*Main Data.List> concatMap (\i -> combinations i a) [0..(length a)]
[[],[1],[2],[3],[4],[5],[1,2],[1,3],[1,4],[1,5],[2,3],[2,4],[2,5],[3,4],[3,5],[4,5],[1,2,3],[1,2,4],[1,2,5],[1,3,4],[1,3,5],[1,4,5],[2,3,4],[2,3,5],[2,4,5],[3,4,5],[1,2,3,4],[1,2,3,5],[1,2,4,5],[1,3,4,5],[2,3,4,5],[1,2,3,4,5]]


If a person dials a sequence of numbers on the telephone, what possible words/strings can be formed from the letters associated with those numbers?

Permutations of 10 digits
2 [ABC], 3 [DEF], 4 [GHI], 5 [JKL], 6 [MNO], 7 [PQRS], 8 [WXYZ],

where any digit can be repeated up to 10 times.

How would you reverse the image on an n by n matrix where each pixel is represented by a bit?

There is a linked list of numbers of length N. N is very large and you don’t know N. You have to write a function that will return k random numbers from the list. Numbers should be completely random. Hint: 1. Use random function rand() (returns a number between 0 and 1) and irand() (return either 0 or 1) 2. It should be done in O(n).

reservoir sampling

Find or determine non existence of a number in a sorted list of N numbers where the numbers range over M, M>> N and N large enough to span multiple disks. Algorithm to beat O(log n) bonus points for constant time algorithm.

How do you find out the fifth maximum element in an Binary Search Tree in efficient manner. Note: You should not use use any extra space. i.e sorting Binary Search Tree and storing the results in an array and listing out the fifth element.

look at picture

Suppose you have given N companies, and we want to eventually merge them into one big company. How many ways are theres to merge?

!N

-------

Things to do before the end of the day + rest ->

(1) read through these notes
(2) more substring problems
(3) permutations and combinations
(4) graphs and trees, memorize

--------

Review:

1. Sketch out binary tree balancing.  What would insert, delete, and lookup look like?

data Node a = Node Int a (Node a) (Node a) | Tip

rotateL val l r@(Bin val' rL rR) = bin val' rR (bin val l rL)
rotateR val l@(Bin val' lL lR) r =  bin val' lL (bin val lR) r

bin val l@(Bin sizeL _ _ _) r@(Bin sizeR _ _ _) = bin (sizeL+sizeR+1) val l r

Insert and delete would involve a rotateL or rotateR, and lookup wouldn't care.

2. Why are Red Black trees so efficient?

3. What's the Big-O of grep?

n = length string to search in
m = length of pattern

O(n*m)

This is effectively because at each character, you're searching ahead m times to look for a match, and then you repeat it for n+1

4. Explain a permutation, and implement a recursive function for it.

A permutation is all of the orders of a collection/set.  You can break down how that would work by looking at all subsets excluding one member:

[1,2,3] -> 1     2       3
          [2,3] [1,3]   [1,2]
	  2  3   1 3    1  2
         [3][2] [3][1] [2][1]
          3  2   1 3    1  2
         [] []  [] []  [] []

Expressed in haskell: perm xs = [x:ys | x < xs, ys <- perm (remove x xs)]

5. Implement mergesort.  What's the Big-O and why?

merge as [] = as
merge [] bs = bs
merge as@(a:as') bs@(b:bs') = if a < b then a:(merge as' bs) else b:(merge as bs')

mergeSort xs = merge (mergeSort as) (mergeSort bs)
  where
    (as, bs) = splitAt (length xs `div` 2) xs

If we were to convert this to python, it'd be pretty much the same thing:

def merge(as,bs):
    if as == []:
       return bs
    if bs == []:
       return as

    if as[0] < bs[0]:
       res = merge(as[1:], bs)
       res.insert(0,as[0])
    else:
       res = merge(as,bs[1:])
       res.insert(0,bs[0])
    return res

blah blah blah

6. Implement quicksort.  What's the Big-O and why?

quicksort [] = []
quicksort (p:ps) = (quicksort lesser) ++ [p] ++ (quicksort greater)
  where
    lesser = filter (<=) p
    greater = filter (>) p

BigO is n2 because if we choose the pivot in this way and the list is complete sorted and reversed

7. Reverse a linked list in C

8. What would you use to resize a LRU that was linked hashtable?

9. How does TCP work?



10. How would you quickly sort lots of ints?

Using radix sort, which is rad.

11. Explain a combination, and implement a recursive function for it.

Oh god.

combinations 0 _  = [[]]
combinations n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

12. Explain how a bloom filter works.
13. What's a trick you can use for largest sum in a substring?

14. What's the difference between a mutex and a semaphore?

15. How do condition variables work?



16. How does consistent hashing work?
17. How would you make a random number from another random number?
18. How would you find the nth maximal element in a binary tree?
19. How would you find the ith digit of a number?

20. Write some code to show how reservoir sampling works.

21. How do you detect cycles in a linked list?

22. How do you find the middle of a linked list in O(n)?

23. Solve the minimum swap problem for a list you want to be ordered greater than another?

24. What is something hard/exciting you've worked on?

25. In one sentence, what do you do for Bump?

26. How does HTTP work?

27. How does TCP work?
