*   The "one-state late" hack should be fixed properly. Perhaps games should
    have their own identifiers, announced during game setup or something. This
    game identifier could be included in the control and queue messages, and
    the server could ignore requests with the wrong game identifier.

    This would eliminate a race condition where the server quickly transitions
    out of a game and into the next one, then processes (say) a control message
    from a previous game that didn't get ignored properly by the one-state-late
    trick.

*   Provide a way to negotiate the number of players and their handicap
    settings.

*   Allow passive game observation by non-player clients. This would probably
    make the state transition diagram a bit more clear, too: each player has
    its own independent cleanup and control states, and the client pretty much
    has to track them all anyway. Being honest about that -- and about the fact
    that the client commands are only allowed when `you` are in certain states
    -- would probably be good.

*   Suppose a client sent lots of state-request messages for the same frame or
    mode transition before the server can respond once. What should the
    server's behavior be like? If it rejects them, should it be required to
    send multiple rejection messages? (I guess yes?) If it accepts, should it
    be required to send the state information multiple times? (I guess no?)

*   Is the server allowed to send `state` messages for players that have lost?
