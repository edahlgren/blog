title: Invisible networks for the sneaky
author: E. Dahlgren

Eyebrows raise. Invisible networks you say ...  If you type

	 ifconfig

You'll see all of the network devices currently up: probably eth0 (ethernet) or wlan (wireless), and a lo (loopback) device at least (ifconfig -a for all devices up or down).  Little beknownst to you, these devices exist in a single [network namespace](http://lwn.net/Articles/219794/): a place for them all to romp and play happily together.  And by romp and play I mean they can be [bridged together](http://tldp.org/HOWTO/BRIDGE-STP-HOWTO/what-is-a-bridge.html), they can [masquerade](http://www.tldp.org/HOWTO/IP-Masquerade-HOWTO/ipmasq-background2.1.html) as one another (I know, it's awesome), or deliberately forward each other packets.

What if I told you that you could steal one of these network devices and put it in a private, invisible network?  You might think, "Hmm, that means I can probably run X program on P port in both networks and they won't conflict".  Correct.  Then you might say, "Hey!  I could also hide everything I'm doing in that invisible network from the rest of my machine".  Yep.  But after a few minutes you'd probably say: "What! How do I communicate with the outside world?"  I'd be worried too.

The answer: Virtual ethernet!

"Wait, what?"  Virtual ethernet devices are created in pairs, where one "veth" device can talk directly to its peer, no matter what network you move either of them to.  So if veth0 is in network A and veth1 is in network B, but network B is totally invisible to A, these networks can tunnel messages to one another over their veth secret chat line.  A stranger doesn't know anything about your private network topology other than a veth device exists.  Pretty cool.

"Ok fine, how do I make an invisible network?"  By making another network namespace, by making a pair of veth devices that can talk to each other, and by placing one in the new network namespace and the other in your current namespace.  Command line setup involves:

* [ip netns](http://www.dsm.fordham.edu/cgi-bin/man-cgi.pl?topic=ip-netns&ampsect=8), for creating, removing, and running commands in network namespaces
* [ip link](http://www.dsm.fordham.edu/cgi-bin/man-cgi.pl?topic=ip-link&sect=&querytype=Man+page&srch=&case=sensitive), for generally managing network devices
* [ip addr](http://www.dsm.fordham.edu/cgi-bin/man-cgi.pl?topic=ip&sect=addr&querytype=Man+page&srch=&case=sensitive), for assigning inet and subnet addresses to network devices
* [ifconfig](http://www.dsm.fordham.edu/cgi-bin/man-cgi.pl?topic=ifconfig&sect=addr&querytype=Man+page&srch=&case=sensitive), for checking the network configuration
* [netcat](http://www.dsm.fordham.edu/cgi-bin/man-cgi.pl?topic=nc&sect=&querytype=Man+page&srch=&case=sensitive), for testing

Warning: some of these commands will fail with mysterious error messages if you're not root.  ```sudo su``` to save yourself the confusion.

In Terminal 1:

    #!sh
    # name a new network namespace
    # this is not the network namespace you're in right now
    # but we can enter it by name now at any time
    ip netns add ns1

    # check that the new network namespace is present
    ip netns list

    # create a pair of veth devices
    ip link add host type veth peer name guest

    # make sure you can see both host and guest (down)
    ifconfig -a

    # bring the host veth device up, it will exist in the current
    # network namespace
    ip link set host up

    # give up the guest veth device to the new network
    ip link set guest netns ns1

    # assign an ip and subnet to the host veth
    ip addr add 192.168.0.101/24 dev host

    # check that the address shows up
    ifconfig

In Terminal 2:

    #!sh
    # execute a bash shell in your newly named network
    ip netns exec ns1 bash

    # check that you can see the guest veth device (down)
    ifconfig -a

    # bring the guest veth device up
    # assign an ip and subnet to it
    ip link set GUEST up
    ip addr add 192.168.0.102/24

    # check that the guest veth device is up with an address
    ifconfig

Drumroooool .... In Terminal 2:

    #!sh
    # check that you can ping the host veth device
    # in the guest's network
    ping 192.168.0.101

In Terminal 1:

    #!sh
    # set up a netcat listening server
    nc -l 192.168.0.101 5555

In Terminal 2:

    #!sh
    # set up a netcat client
    nc 192.168.0.101 5555

    # say hi to the host veth device!
    hi

You should see hi in Terminal 1. Mission accomplished.

"Ugh, how do I tear this down?" In Terminal 1:

    ip link delete host

If you check Terminal 2, you'll see that guest cannot live without it's buddy, and has already committed suicide.

To come:

* Attaching a veth device to the internet at large
* Doing this all in a compiled language, for better portability

