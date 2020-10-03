module Persisted exposing (Persisted, change, changed, init, persist)


type Persisted a
    = Persisted { persisted : a, local : a }


init : a -> Persisted a
init x =
    Persisted { persisted = x, local = x }


change : a -> Persisted a -> Persisted a
change x (Persisted { persisted }) =
    Persisted { persisted = persisted, local = x }


persist : a -> Persisted a -> Persisted a
persist x (Persisted old) =
    if changed (Persisted old) then
        Persisted { old | persisted = x }

    else
        init x


changed : Persisted a -> Bool
changed (Persisted { persisted, local }) =
    local /= persisted
