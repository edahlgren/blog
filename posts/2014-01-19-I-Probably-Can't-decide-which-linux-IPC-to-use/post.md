title: I Probably Can't decide which linux IPC to use
author: Erin Dahlgren

You want to do interprocess communication in linux?  Wait, you mean [named pipes](http://www.tldp.org/LDP/lpg/node15.html) ... no you mean [POSIX message queues](http://www.users.pjwstk.edu.pl/~jms/qnx/help/watcom/clibref/mq_overview.html) ... oh wait no, maybe just plain [signals](http://www.cs.cf.ac.uk/Dave/C/node24.html) ... but probably not so [shared memory](http://beej.us/guide/bgipc/output/html/multipage/shm.html) ... [unix domain sockets](http://man7.org/linux/man-pages/man7/unix.7.html) ... [network sockets](http://en.wikipedia.org/wiki/Network_socket) ... [d-bus](http://dbus.freedesktop.org/doc/dbus-tutorial.html)? You have no idea?  Me neither.

We could ramble on about your options, but it's more fun to experiment, so pull out your command line:

D-Bus what?
-------------------------------------
Try querying the d-bus system:<br/>
(If that doesn't work, and you're on ubuntu, try installing ```qt4-dev-tools```)

    $ qdbus
    :1.0
     org.gnome.Terminal.Display_0
    :1.1
    :1.143
    :1.15
     org.PulseAudio1
     org.pulseaudio.Server
    :1.16

    ...    
    org.freedesktop.DBus

You'll see a list of numeric addresses or ids intermixed with what look like reversed domain names.

The addresses/ids are a bit meaningless right now, but we can probably guess what ```org.gnome.Terminal.Display_0``` is: the [GNOME terminal application](http://en.wikipedia.org/wiki/GNOME_Terminal). But what is it doing in that list?

    $ qdbus org.gnome.Terminal.Display_0
    /
    /org
    /org/gnome
    /org/gnome/Terminal
    /org/gnome/Terminal/Factory
    /com
    /com/canonical
    /com/canonical/menu
    /com/canonical/menu/ACAF07
    /com/canonical/menu/ABFFD2
    /com/canonical/menu/ABA8B5
    /com/canonical/menu/A03742
    /com/canonical/menu/AC8622
    /com/canonical/menu/AC1E8B
    /com/canonical/menu/AE8B1A
    /com/canonical/menu/AC0CA6

Whoa that's a lot of shit.  It looks like paths.  And we still don't totally know why ```qdbus``` cares about the GNOME terminal.  Ok try (or use whatever path you like):

    $ qdbus org.gnome.Terminal.Display_0 /com/canonical/menu/ACAF07
    method QDBusVariant org.freedesktop.DBus.Properties.Get(QString interface_name, \
        QString property_name)
    method QVariantMap org.freedesktop.DBus.Properties.GetAll(QString interface_name)
    signal void org.freedesktop.DBus.Properties.PropertiesChanged(QString interface_name, \
        QVariantMap changed_properties, QStringList invalidated_properties)
    method void org.freedesktop.DBus.Properties.Set(QString interface_name, \
        QString property_name, QDBusVariant value)
    method QString org.freedesktop.DBus.Introspectable.Introspect()
    method QString org.freedesktop.DBus.Peer.GetMachineId()
    method void org.freedesktop.DBus.Peer.Ping()
    property read QStringList com.canonical.dbusmenu.IconThemePath
    property read QString com.canonical.dbusmenu.Status
    property read QString com.canonical.dbusmenu.TextDirection
    property read uint com.canonical.dbusmenu.Version
    ...

Much more informative!  This looks like an interface through which you can communicate with the GNOME terminal.  Cool!  Let's try getting one of the terminal properties (or try this with a property you find in your list):

    $ qdbus org.gnome.Terminal.Display_0 /com/canonical/menu/ACAF07 \
            com.canonical.dbusmenu.TextDirection
    ltr

Boom!  If you tried this and your terminal isn't displaying text from left to right, give up now and go drink a beer.  Otherwise, let's call a method on GNOME terminal in the same way we asked for one of it's properties.  Let's see what ```org.freedesktop.DBus.Properties.GetAll``` gives us (hopefully a list of all of the properties).  This time we'll use [```dbus-send```](http://en.wikipedia.org/wiki/GNOME_Terminal):

    $ dbus-send --print-reply --dest=org.gnome.Terminal.Display_0 \
                /com/canonical/menu/ACAF07 org.freedesktop.DBus.Properties.GetAll \
                string:com.canonical.dbusmenu
    method return sender=:1.0 -> dest=:1.137 reply_serial=2
       array [
          dict entry(
             string "Version"
             variant uint32 3
          )
          dict entry(
             string "TextDirection"
             variant string "ltr"
          )
          dict entry(
             string "Status"
             variant string "normal"
          )
          dict entry(
             string "IconThemePath"
             variant array [
                ]
          )
       ]

>Weird command, right? ```dbus-send``` gives us more control over sending messages than ```qbus```, because of the ```--dest``` flag, which specifies which service (GNOME terminal) to find the implementation of a generic method like ```org.freedesktop.DBus.Properties.GetAll``` (which nearly all services implement).  After we supply the path under the service to use, we provide a string argument.

Notice that TextDirection is there with the value that we just queried.  So we can send rpc commands to any process that tells us what types of rpc calls it supports, nice!

But what are those signals about?  Just as we can execute a method on GNOME terminal through the d-bus system, or query a terminal property, we can listen for signals that GNOME terminal is broadcasting.

    $ dbus-monitor --profile "sender='org.gnome.Terminal.Display_0'"

Now try selecting one of the drop downs from GNOME terminal (File, Edit, View ...), and notice that you get one or more signals that look like:

    signal sender=:1.0 -> dest=(null destination) serial=501 \
           path=/com/canonical/menu/BA18C1; interface=com.canonical.dbusmenu; \
           member=LayoutUpdated

... showing that the GNOME terminal UI has updated.

So what does the d-bus system actually do?  One way to look at it: D-bus manages RPC routing, where the destinations of the RPCs are always on the same machine.  D-bus also simultaneously broadcasts messages to interested parties.  D-bus is acting both as a router and as a pub/sub hub.  Pretty cool.

**When to use d-bus**

You have a bunch of services/applications that need to play well with each other, and may not be written by the same party.  This is useful for keeping services/applications completely independent (they don't even share a common config file), which is arguably more fault tolerant.  It makes sense for a community of linux desktop applications, or a community of android applications.  Or a community of systems programmers who don't trust each other :)

**When to not use d-bus**

Unless you're communicating with systems services that implement d-bus interfaces, a d-bus bus has a lifetime tied to a user session.  That means that non systems d-bus services can only communicate when a user is logged in, and cannot communicate cross-users.  If you give your d-bus services special system's level permissions, then hell, you can do whatever you want.

Is d-bus memory efficient?  D-bus is as memory efficient as the binary protocol that it uses.  If you're sending large serialized messages through d-bus, that's your fault.  Choose another IPC method where memory can be shared between processes.

I (still) Probably Can't decide which linux IPC to use, so more to come.

>[1] [D-Bus specification](http://dbus.freedesktop.org/doc/dbus-specification.html)<br/>
>[2] [D-Bus internal implementation](http://dbus.freedesktop.org/doc/api/html/group__DBusInternals.html)<br/>
>[3] [kdbus being considered in android](http://kroah.com/log/blog/2014/01/15/kdbus-details/)
