These are the tests for the Tries library by Matti Niemenmaa, and should reside
in a subdirectory of the Tries distribution.

To run the tests, run 'Main.hs'.

You'll need the following packages, other versions may work but haven't been
tested:

  base                      == 4.*
, HUnit                     == 1.2.*
, QuickCheck                >= 1.1 && < 2
, test-framework            >= 0.2 && < 1
, test-framework-hunit      >= 0.2 && < 1
, test-framework-quickcheck >= 0.2 && < 1

In addition, unlike the library itself, no attempt has been made to make sure
that the tests would work with anything other than GHC.
