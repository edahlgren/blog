title: Simple Consistent Hashing
author: edahlgren

Consistent hashing is simply hashing strings to a consistent place in your favorite ordered data structure.  How the hell is this useful?

Imagine this: you have two types of strings, strings that represent servers and strings that represent requests going to those servers.  Your goal is to get your requests to your servers.  Simple enough.  Well what if you have caches on those servers, and you win big if the same data goes to the same server?  Now your goal is to map requests consistently to servers.  In other words, if serverA goes down, suddendly all requests meant for serverA go consistently to serverB.

Here's the beautifully simple idea: the strings that represent servers hash to a position in your data structure, and so do the strings that represent requests.  The positions your servers map to represent boundaries around your requests.  So for all requests that hash to a position between serverA and serverB, they go to server B.  For all that hash to a position between serverB and serverC, they go to serverC.  And finally for all that hash to a position between serverC and serverA, they go to serverA.  When treat your data structure circularly, you get a consistent partitioning of your requests.

All you need to make consistent hashing work is a hash function, a data structure that you can loop over in a circular manner (even a tree will work, as we'll see), and some strings representing your servers and your requests.

I'm going to demonstrate this with simple a Haskell API, but you should look at the hashring package and the Data-Hash-Consistent package if you're bent on doing this in Haskell in the wild.

The first thing you'll need is a "circular" data type where you can store the positions of your servers.  Circular is in scare quotes because you can make any ordered data structure circular if you write a function to continue search at the head once the end is reached.  It's popular to call this a "ring".  Here's the basic API:

    #!haskell

    data Ring = -- something we haven't decided yet

    -- Create a Ring
    newRing :: IO Ring

    -- Something we can hash, representing whole or part of a server
    type ServerID = Lazy.ByteString
    -- Address of your server, so you can things route to it
    type Addr = String
    -- Callback to a particular server.  We'll dig into this later.
    type Ping = IO ()

    -- Plot a server on your Ring
    addServer :: Ring -> ServerID -> Addr -> Ping -> IO ()

    -- Something we can hash representing a request
    type Key = Lazy.ByteString

    -- Consistently get a certain server based on a key
    -- If all of the servers are down, then return no server
    getServer :: Ring -> Key -> IO (Maybe Addr)

    -- Remove a server from your Ring
    removeServer :: Ring -> IO ()

    -- Attempt to make contact with a server, to establish whether up or down
    pingServer :: Ring -> Ping -> IO ()

    -- Status of a server is either UP or DOWN
    data Status = UP | DOWN

Let's flesh these out in order.
