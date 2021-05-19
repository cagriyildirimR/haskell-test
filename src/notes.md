* Type is a collection of related values
* Class is a collection of types that support certain overloaded operations called methods.

1- Eq - equality types
2- Ord - ordered types
3- Show - showable types
4- Read - readable types : read "False" :: Bool, not ( read "False" ), type is infered from *not*
5- Num - numeric types: (+), (-), (*), negate, abs, signum
6- Integral - integral types: div, mod, div 7 2 = 3, contains types instance of Num in addition ...
7- Fractional - fractional types: (/), recip (reciprocal; recip 2 = 1/2, recip 0.5 = 2) contains type instance of Num in addition
