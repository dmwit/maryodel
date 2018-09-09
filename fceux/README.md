TODO. Some notes on getting this working:

\[Is luarocks actually needed? I'm not sure.\]
Build luarocks for lua 5.1. Configure with some extra flags:

    ./configure --lua-version=5.1 --versioned-rocks-dir

You need the lunix library.

    git clone https://github.com/wahern/lunix
    cd lunix
    make all5.1
    make install5.1

The last step prints which file it installs to. Try

    fceux --loadlua dr_mario_server.lua <path_to_dr_mario.zip>

to see if it's picked up this file. If it has, you'll just get a blank, black
screen that doesn't respond (it's waiting for a client to connect). If it
hasn't, you'll see an error like this:

     Lua thread bombed out: ...:1: module 'unix' not found:
     	no field package.preload['unix']

Then there will be a bunch more lines detailing its search path.
