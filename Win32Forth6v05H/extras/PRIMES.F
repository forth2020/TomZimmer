\ primes.f              Simple Prime number generator

anew prime_generator

: ?prime        ( n1 -- f1 )
                dup 2
                do      dup i mod 0=
                        if      0= leave
                        then
                loop    ;

: .primes       ( -- )
                cr ."    1   3"
                512 4
                do      i ?prime
                        if      i 4 .r 10 ?cr
                        then
                loop    ;

.primes

