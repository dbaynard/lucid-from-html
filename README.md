**Please use @alogic0’s fork at <https://github.com/alogic0/lucid-from-html>.**

---

This repository hosts the code to the `lucid-from-html` tool, derived from [@jaspervdj](https://github.com/jaspervdj).
It is quite rough, and doubtless many examples do not work.
The test suite is in progress.

Please report any issues (including examples of html files that don’t parse correctly) at the [issue tracker](https://github.com/dbaynard/lucid-from-html/issues).

# Install

1.  Clone the repository.

2.  Change to the base directory of the repository.

3.  Run

        > stack install markdown-unlit

        > stack install

# Run

To convert *filename*.html to lucid text, run

    > lucid-from-html filename.html

This will print to standard output.
To redirect to *haskell-file*.hs, use

    > lucid-from-html filename.html > haskell-file.hs

To see all options, run

    > lucid-from-html --help
